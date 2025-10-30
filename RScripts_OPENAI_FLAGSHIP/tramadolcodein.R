################################################################################
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
#
# This script builds a Strategus analysis specification object based on the
# provided <Analysis Specifications> for the study "tramadolcodein".
#
# It:
# - Downloads the specified target, comparator, and outcome cohorts from ATLAS
# - Resolves the specified negative control outcome concept set
# - Assembles module settings for CohortGenerator, CohortDiagnostics, and CohortMethod
# - Applies the time-at-risk windows and propensity score settings provided
# - Saves a single JSON specification that Strategus can execute against a CDM
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

# Load packages
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
# NOTE: Update baseUrl to your WebAPI endpoint as needed.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# - The cohortIds are taken from <Analysis Specifications>
# - We will remap them to 1 (target), 2 (comparator), 3 (outcome) for internal use
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to internal IDs (1=target; 2=comparator; 3=outcome)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes
# - Resolving the concept set provides the list of negative control outcome conceptIds
# - We assign synthetic cohortIds (101, 102, ...) to these negative control outcomes
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Negative control concept set: negative
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(
    baseUrl = baseUrl
  ) %>%
  ROhdsiWebApi::getConcepts(
    baseUrl = baseUrl
  ) %>%
  dplyr::rename(
    outcomeConceptId = conceptId,
    cohortName = conceptName
  ) %>%
  dplyr::mutate(
    cohortId = dplyr::row_number() + 100 # reserve 1,2,3 for T/C/O; negative controls from 101,102,...
  ) %>%
  dplyr::select(cohortId, cohortName, outcomeConceptId)

# Check to ensure no cohortId overlap between T/C/O (1,2,3) and negative controls (>=101)
if (length(intersect(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)) > 0) {
  stop("*** Error: duplicate cohort IDs found between primary cohorts and negative control outcomes ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Use the single outcome cohort (id=3) and add a clean window of 365 days
# (this is typically used by diagnostics or downstream processing)
oList <- cohortDefinitionSet %>%
  dplyr::filter(.data$cohortId == 3) %>%
  dplyr::mutate(
    outcomeCohortId = .data$cohortId,
    outcomeCohortName = .data$cohortName
  ) %>%
  dplyr::select(.data$outcomeCohortId, .data$outcomeCohortName) %>%
  dplyr::mutate(cleanWindow = 365)

# Target and Comparator pairing for the CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate selection:
# The <Analysis Specifications> provide empty lists for conceptsToInclude and conceptsToExclude;
# therefore, we include all defaults and do not explicitly include or exclude specific concepts.
includedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# CohortGeneratorModule --------------------------------------------------------
# - Shared resources contain cohort definitions (T/C/O) and negative controls
# - Module will generate the cohorts in the target database and optionally cohort stats
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# - Runs a broad set of diagnostics for all cohorts present in cohortDefinitionSet
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE,
  runIncludedSourceConcepts = TRUE,
  runOrphanConcepts = TRUE,
  runTimeSeries = FALSE,
  runVisitContext = TRUE,
  runBreakdownIndexEvents = TRUE,
  runIncidenceRate = TRUE,
  runCohortRelationship = TRUE,
  runTemporalCohortCharacterization = TRUE,
  minCharacterizationMean = 0.01
)

# CohortMethodModule -----------------------------------------------------------
# Map <Analysis Specifications> to implementation-ready settings.

# Study period(s):
# - The specification indicates a single studyPeriod with null start/end; per template,
#   we use empty strings to indicate no restriction on study period.
studyPeriods <- tibble::tibble(
  studyStartDate = c(""), # unrestricted
  studyEndDate   = c("")  # unrestricted
)

# Time-at-risk windows (two TARs from the specification):
# 1) 0d from cohort start to cohort end (minDaysAtRisk=1)
# 2) 0d from cohort start to 9999d anchored at cohort start (minDaysAtRisk=1)
timeAtRisks <- tibble::tibble(
  label = c("TAR1: CS->CE", "TAR2: CS->CS+9999"),
  riskWindowStart = c(0, 0),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd = c(0, 9999),
  endAnchor = c("cohort end", "cohort start"),
  minDaysAtRisk = c(1, 1)
)

# Propensity Score settings from the specification (match on PS; one configuration):
matchOnPsArgsList <- tibble::tibble(
  label = c("Match 1:1 cal0.2 stdlogit"),
  maxRatio = c(1),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# No PS stratification specified (NULL in the specification)
stratifyByPsArgsList <- tibble::tibble(
  label = c(),
  numberOfStrata = c(),
  baseSelection = c()
)

# Build a list of PS adjustment configurations (each element is either 'match' or 'stratify')
psConfigList <- list()

# Convert match-on-PS rows into configuration entries
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "match",
      label  = matchOnPsArgsList$label[i],
      params = list(
        maxRatio     = matchOnPsArgsList$maxRatio[i],
        caliper      = matchOnPsArgsList$caliper[i],
        caliperScale = matchOnPsArgsList$caliperScale[i]
      )
    )
  }
}

# Convert stratify-by-PS rows into configuration entries (none specified)
if (exists("stratifyByPsArgsList") && nrow(stratifyByPsArgsList) > 0) {
  for (i in seq_len(nrow(stratifyByPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "stratify",
      label  = stratifyByPsArgsList$label[i],
      params = list(
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection  = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}

# Assemble CohortMethod analysis specifications for each combination of:
# - Study period
# - Time-at-risk window
# - PS adjustment configuration
cmAnalysisList <- list()
targetComparatorOutcomesList <- list()
analysisId <- 1

# Pre-build covariate settings
# - Using FeatureExtraction defaults per template (include all typical baseline covariates)
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE
)

# Build outcome definitions:
# - The primary outcome(s) flagged as outcomeOfInterest = TRUE
# - Negative control outcomes flagged as outcomeOfInterest = FALSE
outcomeList <- append(
  lapply(seq_len(nrow(oList)), function(i) {
    CohortMethod::createOutcome(
      outcomeId = oList$outcomeCohortId[i],
      outcomeOfInterest = TRUE,
      trueEffectSize = NA,
      priorOutcomeLookback = 365 # from createStudyPopArgs.priorOutcomeLookBack
    )
  }),
  lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
    CohortMethod::createOutcome(
      outcomeId = i,
      outcomeOfInterest = FALSE,
      trueEffectSize = 1
    )
  })
)

# Build Target-Comparator-Outcome set(s)
# - No explicit covariate concept exclusions per specification (empty lists)
for (i in seq_len(nrow(cmTcList))) {
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
  )
}

# Iterate through study periods, TARs, and PS configurations to create analyses
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Convert PS config to CohortMethod args
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      } else {
        stop("Unknown PS configuration method: ", psCfg$method)
      }

      # Data extraction settings (GetDbCohortMethodData)
      # - Following the specification: maxCohortSize=0; unrestricted period (empty strings)
      # - Using restrictToCommonPeriod=FALSE (the restriction is handled in study population as requested)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Propensity score fitting args (from specification)
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # allow Strategus to continue even if PS fitting fails in some strata
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          tolerance = 2e-07,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "silent",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Covariate balance args (not specified; using reasonable defaults)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model args (from specification)
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = FALSE,
        useCovariates = FALSE,
        inversePtWeighting = FALSE,
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          tolerance = 2e-07,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Study population args (from specification and current TAR)
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 365,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t]
      )

      # Append the analysis specification (one per combination of period, TAR, and PS)
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(nchar(studyStartDate) == 0, "noStart", studyStartDate),
          ifelse(nchar(studyEndDate) == 0, "noEnd", studyEndDate),
          timeAtRisks$label[t],
          psCfg$label
        ),
        getDbCohortMethodDataArgs = getDbCohortMethodDataArgs,
        createStudyPopArgs = createStudyPopArgs,
        createPsArgs = createPsArgs,
        matchOnPsArgs = matchOnPsArgs,
        stratifyByPsArgs = stratifyByPsArgs,
        computeSharedCovariateBalanceArgs = computeSharedCovariateBalanceArgs,
        computeCovariateBalanceArgs = computeCovariateBalanceArgs,
        fitOutcomeModelArgs = fitOutcomeModelArgs
      )
      analysisId <- analysisId + 1
    }
  }
}

# Create CohortMethod module specifications
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the final analysis specification JSON using the study "name" from <Analysis Specifications>
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "tramadolcodein", "tramadolcodeinAnalysisSpecification.json")
)