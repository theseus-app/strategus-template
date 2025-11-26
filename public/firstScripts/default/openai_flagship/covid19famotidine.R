################################################################################
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
#
# This script builds a Strategus analysis specification for the study
# named in the Analysis Specifications: "covid19famotidine".
#
# The script follows the Template structure and uses the settings provided
# in the Analysis Specifications. Inline annotations explain how each block
# maps from the specifications into HADES/Strategus module settings.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

# Load packages used in this script
library(dplyr)
library(tibble)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from the WebAPI that correspond to the Target, Comparator,
# and Outcome cohorts. The IDs are taken EXACTLY from the Analysis Specifications.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# - Export the target, comparator, and outcome cohorts by their WebAPI cohort IDs
# - Then, renumber locally so that:
#   target -> 1, comparator -> 2, outcome -> 3
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to local IDs used by modules downstream (do not change cohort names)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1  # target1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2  # comparator1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3  # outcome1

# Negative control outcomes ----------------------------------------------------
# The negative controls come from a concept set in WebAPI. We resolve the concept set to
# a list of standard concepts, and provide them to Strategus so the CohortMethod module
# will treat them as negative control outcome cohorts (not as generated cohorts).
# - Concept set ID: 1888110 (name: "negative") from Analysis Specifications
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(
    baseUrl = baseUrl
  ) %>%
  ROhdsiWebApi::getConcepts(
    baseUrl = baseUrl
  ) %>%
  dplyr::rename(
    outcomeConceptId = "conceptId",
    cohortName = "conceptName"
  ) %>%
  mutate(
    # Offset NC IDs from the analysis cohorts to avoid any overlap
    cohortId = dplyr::row_number() + 100
  ) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Sanity check: ensure no cohortId overlap between analysis cohorts (1,2,3) and NCs (>=101)
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between primary cohorts and negative controls ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes:
# - Pick the outcome of interest (local cohortId == 3) and set outcome-specific metadata
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(
    # Not used by CM directly here, but retained as an example/placeholder
    cleanWindow = 365
  )

# Target and Comparator for the CohortMethod analysis
# - Using EXACT names from Analysis Specifications
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  stringsAsFactors = FALSE
)

# Covariate selection from Analysis Specifications ----------------------------
# The Analysis Specifications provided empty include/exclude concept lists.
# We reflect that exactly here: no included or excluded covariate concepts specified.
includedCovariateConcepts <- tibble(
  conceptId = integer(),
  conceptName = character()
)

excludedCovariateConcepts <- tibble(
  conceptId = integer(),
  conceptName = character()
)

# CohortGeneratorModule --------------------------------------------------------
# Shared resources for:
#  - Cohort definitions (the 3 cohorts we exported and renumbered)
#  - Negative control outcomes (concept-based list)
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  # Generate cohort stats for a richer diagnostics output
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# Run a comprehensive set of diagnostics on all cohorts (1, 2, 3)
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
# Analysis Specifications mapping:
# studyPeriods:
#   - One period: start 20200201, end 20200530
# createStudyPopArgs Time-at-Risk (TAR):
#   - Single TAR: start = 1 day after cohort start; end = 30 days after cohort start; minDaysAtRisk = 1
# propensityScoreAdjustment:
#   - 2 PS settings:
#       (1) Stratify by PS: numberOfStrata = 5, baseSelection = "all"
#       (2) Match on PS: maxRatio = 1, caliper = 0.2, caliperScale = "standardized logit"
# createPsArgs (model fitting for PS):
#   - maxCohortSizeForFitting = 250000
#   - errorOnHighCorrelation = TRUE
#   - prior: laplace with cross-validation
#   - control: tolerance=2e-7, cvType="auto", fold=10, cvRepetitions=10, noiseLevel="silent",
#              resetCoefficients=TRUE, startingVariance=0.01
# fitOutcomeModelArgs:
#   - modelType="cox", stratified=TRUE, useCovariates=FALSE, inversePtWeighting=FALSE
#   - prior: laplace with cross-validation
#   - control: same tolerance/CV settings, noiseLevel="quiet"

# Study period window from Analysis Specifications
studyPeriods <- tibble(
  studyStartDate = c("20200201"), # YYYYMMDD
  studyEndDate   = c("20200530")  # YYYYMMDD
)

# Time-at-risks (TARs): defined from Analysis Specifications
timeAtRisks <- tibble(
  label = c("TAR_1-30_cohortStart"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"), # EXACT from specifications
  riskWindowEnd  = c(30),
  endAnchor = c("cohort start")    # EXACT from specifications
)

# Propensity Score settings - stratify by PS (from Analysis Specifications)
stratifyByPsArgsList <- tibble(
  label = c("PS_stratify_5_all"),
  numberOfStrata = c(5),
  baseSelection  = c("all")
)

# Propensity Score settings - match on PS (from Analysis Specifications)
matchOnPsArgsList <- tibble(
  label = c("PS_match_1_0.2_stdlogit"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # EXACT from specifications
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# If a data frame for "match on PS" exists and has rows, convert each row to a config
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

# If a data frame for "stratify by PS" exists and has rows, convert each row to a config
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

# Iterate through all analysis setting combinations
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # PS adjustment methods based on the current configuration
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
      }

      # Covariate settings
      # - Using default FE covariates; no custom include/exclude concepts from specifications
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcome list: outcome of interest + negative controls
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
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

      # Target-Comparator-Outcomes list
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # No drug-of-interest covariate exclusions provided in specifications
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Data retrieval settings for CohortMethod
      # - Study window and maximum cohort size from specifications
      # - Not restricting to common period here (handled in study population if requested)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # PS creation settings (from specifications)
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # Keep analysis running if PS fit fails for some strata
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

      # Covariate balance computation settings
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model fitting settings (from specifications)
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = TRUE,
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

      # Study population settings (from specifications)
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = FALSE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1
      )

      # Append the settings to the CM analysis list (each PS/TAR/study window becomes an analysis)
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          studyStartDate,
          studyEndDate,
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

# Create the CohortMethod module specifications
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the Strategus analysis specifications ---------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save analysis specifications to JSON -----------------------------------------
# Use the EXACT study name from the Analysis Specifications in the path/filename.
dir.create(file.path("inst", "covid19famotidine"), recursive = TRUE, showWarnings = FALSE)
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "covid19famotidine", "covid19famotidineAnalysisSpecification.json")
)