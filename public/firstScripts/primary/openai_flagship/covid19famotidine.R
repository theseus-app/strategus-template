################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script builds a Strategus analysis specification using the OHDSI HADES
# CohortGenerator, CohortDiagnostics, and CohortMethod modules.
# It follows the provided Template structure and applies settings from the
# supplied Analysis Specifications for the study "covid19famotidine".
#
# Detailed annotations are included to explain how settings are mapped and used.
################################################################################

# Load required packages -------------------------------------------------------
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# WebAPI base URL where cohorts and concept sets are defined
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# - We export the 3 cohorts (target, comparator, outcome) defined in WebAPI.
# - Then we renumber them locally (1, 2, 3) for use within the module settings.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to local IDs for downstream module consistency:
# 1: target, 2: comparator, 3: outcome
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes
# - Pull the concept set by ID and resolve it into concept IDs for NC outcomes.
# - Assign unique local cohortIds that don't collide with 1,2,3.
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
  rename(
    outcomeConceptId = "conceptId",
    cohortName = "conceptName"
  ) %>%
  mutate(
    # Avoid ID clashes with TC/Outcome cohorts by starting NCs at 101+
    cohortId = dplyr::row_number() + 100
  ) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no duplicate cohort IDs across primary and NC cohorts
if (length(intersect(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)) > 0) {
  stop("*** Error: duplicate cohort IDs found between primary/NC cohorts ***")
}

# Create data frames for analysis inputs ---------------------------------------

# Outcomes (primary outcome only; NCs added later in CohortMethod outcomeList)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # not used directly by CM, kept for completeness

# Target and Comparator for CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  stringsAsFactors = FALSE
)

# Covariate selection configuration (from Analysis Specifications)
# - Provided with null/empty lists for include/exclude, so we configure no
#   special include/exclude concept sets (default FE covariates apply).
includedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character(),
  stringsAsFactors = FALSE
)

excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character(),
  stringsAsFactors = FALSE
)

# CohortGeneratorModule --------------------------------------------------------
# - Define shared resources for cohorts and NC outcomes, and configure generation.

cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource: primary cohorts (TC+outcomes)
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Shared resource: negative control outcomes
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Configure the CohortGenerator module to generate stats for the cohorts
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# - Run a standard suite of diagnostics on the generated cohorts.

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
# Map Analysis Specifications to CM settings

# Study time window (studyPeriods)
# - Analysis Specifications define one period: 20200201 to 20200530
studyPeriods <- tibble::tibble(
  studyStartDate = c("20200201"),
  studyEndDate   = c("20200530")
)

# Time-at-risk (TAR) definitions
# - Single TAR: 1 to 30 days from cohort start, minDaysAtRisk = 1
timeAtRisks <- tibble::tibble(
  label = c("TAR1"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),  # exactly as specified
  riskWindowEnd  = c(30),
  endAnchor = c("cohort start")
)
tarMinDaysAtRisk <- 1

# Propensity Score (PS) adjustment configurations
# - Analysis Specifications specify stratifyByPsArgs with 5 strata and baseSelection=="all"
matchOnPsArgsList <- tibble::tibble(
  label = character(),
  maxRatio  = numeric(),
  caliper = numeric(),
  caliperScale  = character()
)

stratifyByPsArgsList <- tibble::tibble(
  label = c("PS-strata-5-all"),
  numberOfStrata  = c(5),
  baseSelection = c("all")
)

# Build a single PS configuration list combining match/stratify options
psConfigList <- list()

# Convert "match on PS" table to configurations (none here)
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

# Convert "stratify by PS" table to configurations (one entry per row)
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

# Prepare outcome definitions for CohortMethod:
# - Primary outcome is flagged as "outcomeOfInterest = TRUE" with priorOutcomeLookback=30,
#   aligning with the StudyPopulation removal of subjects with prior outcomes.
# - Negative controls are added as "outcomeOfInterest = FALSE" and trueEffectSize=1.
outcomeList <- append(
  lapply(seq_len(nrow(oList)), function(i) {
    CohortMethod::createOutcome(
      outcomeId = oList$outcomeCohortId[i],
      outcomeOfInterest = TRUE,
      trueEffectSize = NA,
      priorOutcomeLookback = 30
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

# Target-Comparator-Outcome groupings for CohortMethod
# - No covariate concept exclusions are specified in the Analysis Specifications,
#   so we leave excludedCovariateConceptIds as an empty vector.
targetComparatorOutcomesList <- list()
for (i in seq_len(nrow(cmTcList))) {
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    excludedCovariateConceptIds = c()
  )
}

# Build CohortMethod analysis configurations by iterating studyPeriods, TARs, and PS setups
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # PS method-specific arguments
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
        stop("Unexpected PS config method: ", psCfg$method)
      }

      # Covariate settings (default FE covariates; no special include/exclude lists)
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Data retrieval settings (align with Analysis Specifications)
      # - studyStartDate/studyEndDate from "getDbCohortMethodDataArgs.studyPeriods"
      # - maxCohortSize = 0 (no limit)
      # - restrictToCommonPeriod: set to FALSE (not specified in provided args)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # PS creation settings from Analysis Specifications (createPsArgs)
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # allow analyses to continue even if a PS model cannot be fit
        estimator = "att",
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
          startingVariance = 0.01,
          seed = 1
        )
      )

      # Covariate balance settings (shared and full balance)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model settings from Analysis Specifications (fitOutcomeModelArgs)
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
          startingVariance = 0.01,
          seed = 1
        )
      )

      # Study population settings from Analysis Specifications (createStudyPopArgs)
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 30,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = tarMinDaysAtRisk
      )

      # Build and add this analysis configuration
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

# Create the CohortMethod module specification
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

# Save to JSON (organized under inst/<studyName>/)
outputDir <- file.path("inst", "covid19famotidine")
if (!dir.exists(outputDir)) {
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
}

ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path(outputDir, "covid19famotidineAnalysisSpecification.json")
)