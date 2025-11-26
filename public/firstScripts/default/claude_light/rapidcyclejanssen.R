################################################################################
# CreateStrategusAnalysisSpecification.R
# Rapid Cycle Janssen Study Analysis Specifications
#
# This script creates a complete analysis specification for a cohort method study
# using the OHDSI Strategus package. It includes cohort definitions, negative
# control outcomes, propensity score adjustment, and outcome model fitting.
#
# For more information about Strategus HADES modules, see:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

library(dplyr)
library(Strategus)

# ============================================================================
# SHARED RESOURCES - Cohort and Concept Set Definitions
# ============================================================================

# Define the base URL for WebAPI (update with your Atlas instance)
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Export Cohort Definitions from Atlas
# This includes: target cohort (1794126), comparator cohort (1794132), 
# and outcome cohort (1794131)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for analysis (standardized IDs: 1=target, 2=comparator, 3=outcome)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# ============================================================================
# NEGATIVE CONTROL OUTCOMES - from Concept Set 1888110
# ============================================================================

# Retrieve negative control concept set definition
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
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  # Assign cohort IDs starting at 101 (to avoid conflicts with target/comparator/outcome IDs)
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Verify no duplicate cohort IDs exist
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# ============================================================================
# DATA FRAMES FOR ANALYSIS CONFIGURATION
# ============================================================================

# Outcomes: Extract outcome cohort (cohortId = 3) from cohort definitions
# Set clean window (prior outcome lookback) to 365 days
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, 
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator Configuration for CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# ============================================================================
# COVARIATE CONFIGURATION
# ============================================================================

# For this analysis, using default covariate settings with all covariates
# No specific concepts included or excluded beyond study design
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE
)

# ============================================================================
# COHORT GENERATOR MODULE SPECIFICATIONS
# ============================================================================

cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Create shared resource specifications for negative control outcomes
# Settings: first occurrence only, detect on concept descendants
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create cohort generator module specifications
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ============================================================================
# COHORT DIAGNOSTICS MODULE SPECIFICATIONS
# ============================================================================

cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Configure diagnostics for all cohorts
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

# ============================================================================
# COHORT METHOD MODULE SPECIFICATIONS
# ============================================================================

# Study Periods: From 2021-01-01 with no end date
# (studyEndDate = NULL means continuing to end of available data)
studyPeriods <- tibble(
  studyStartDate = c(20210101),
  studyEndDate = c(NA)
)

# ============================================================================
# TIME-AT-RISK (TAR) DEFINITIONS
# ============================================================================
# Multiple TAR windows defined for sensitivity analysis:
# - 14 days post-exposure
# - 28 days post-exposure
# - 42 days post-exposure
# - 90 days post-exposure
# - 2 days immediate post-exposure (days 1-2, accounting for lag)

timeAtRisks <- tibble(
  label = c("TAR_14d", "TAR_28d", "TAR_42d", "TAR_90d", "TAR_2d"),
  riskWindowStart = c(1, 1, 1, 1, 0),
  startAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd = c(14, 28, 42, 90, 2),
  endAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start"),
  minDaysAtRisk = c(1, 1, 1, 1, 1)
)

# ============================================================================
# PROPENSITY SCORE CONFIGURATION
# ============================================================================
# PS Matching: maxRatio=100, caliper=0.2 (on standardized logit scale)

matchOnPsArgsList <- tibble(
  label = c("PS_Match_Caliper0.2"),
  maxRatio = c(100),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# Build propensity score configuration list
psConfigList <- list()

# Add PS matching configurations
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "match",
      label = matchOnPsArgsList$label[i],
      params = list(
        maxRatio = matchOnPsArgsList$maxRatio[i],
        caliper = matchOnPsArgsList$caliper[i],
        caliperScale = matchOnPsArgsList$caliperScale[i]
      )
    )
  }
}

# ============================================================================
# BUILD COHORT METHOD ANALYSES
# ============================================================================
# Iterate through all combinations of:
# - Study periods
# - Time-at-risk windows
# - Propensity score strategies

cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- if (is.na(studyPeriods$studyEndDate[s])) "" else studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # ====================================================================
      # PROPENSITY SCORE ARGUMENTS
      # ====================================================================

      if (psCfg$method == "match") {
        # Propensity score matching configuration
        # maxRatio: 100 (allow up to 100:1 matching)
        # caliper: 0.2 on standardized logit scale
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      }

      # ====================================================================
      # GET DB COHORT METHOD DATA ARGUMENTS
      # ====================================================================
      # Configuration for extracting data from database:
      # - Study period: 2021-01-01 onwards
      # - No maximum cohort size restriction
      # - Common period restriction: TRUE
      # - First exposure only: TRUE
      # - Washout period: 365 days
      # - Remove duplicate subjects: ALL

      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        restrictToCommonPeriod = TRUE,
        firstExposureOnly = TRUE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "remove all",
        covariateSettings = covariateSettings
      )

      # ====================================================================
      # CREATE STUDY POPULATION ARGUMENTS
      # ====================================================================
      # Configuration for study population:
      # - No common period restriction at population level
      # - First exposure: FALSE (already handled in getDbCohortMethodData)
      # - Washout: 0 (already handled in getDbCohortMethodData)
      # - Remove duplicate subjects: keep all (already handled in getDbCohortMethodData)
      # - Remove subjects with prior outcome: TRUE
      # - Prior outcome lookback: 99999 days (comprehensive)
      # - Variable time-at-risk windows applied

      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t]
      )

      # ====================================================================
      # CREATE PROPENSITY SCORE ARGUMENTS
      # ====================================================================
      # PS model configuration:
      # - Prior: Laplace with cross-validation
      # - Control: auto CV, 10-fold, 10 repetitions
      # - Max cohort size for fitting: 250,000
      # - Error on high correlation: TRUE

      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",
          exclude = c(0),
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",
          cvType = "auto",
          seed = 1,
          resetCoefficients = TRUE,
          tolerance = 2e-07,
          cvRepetitions = 10,
          fold = 10,
          startingVariance = 0.01
        )
      )

      # ====================================================================
      # COMPUTE COVARIATE BALANCE ARGUMENTS
      # ====================================================================

      # Compute balance for all covariates
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      # Compute balance for Table 1 covariates only
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # ====================================================================
      # FIT OUTCOME MODEL ARGUMENTS
      # ====================================================================
      # Outcome model configuration:
      # - Model type: Cox proportional hazards (stratified)
      # - Stratified: TRUE (by propensity score matching strata)
      # - Use covariates: FALSE (PS-matched analysis)
      # - Inverse PT weighting: FALSE
      # - Prior: Laplace with cross-validation
      # - Control: auto CV, 10-fold, 10 repetitions

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
          cvType = "auto",
          seed = 1,
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          tolerance = 2e-07,
          cvRepetitions = 10,
          fold = 10,
          noiseLevel = "quiet"
        )
      )

      # ====================================================================
      # CREATE OUTCOME DEFINITIONS
      # ====================================================================
      # Combine outcomes of interest and negative control outcomes

      outcomeList <- append(
        # True outcomes of interest
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        # Negative control outcomes (assumed null effect)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # ====================================================================
      # CREATE TARGET-COMPARATOR-OUTCOMES COMBINATIONS
      # ====================================================================

      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList
        )
      }

      # ====================================================================
      # CREATE ANALYSIS SPECIFICATION
      # ====================================================================
      # Combine all arguments into a single CohortMethod analysis

      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study Period: %s-%s; TAR: %s; PS: %s",
          studyStartDate,
          if (studyEndDate == "") "No End" else studyEndDate,
          timeAtRisks$label[t],
          psCfg$label
        ),
        getDbCohortMethodDataArgs = getDbCohortMethodDataArgs,
        createStudyPopArgs = createStudyPopArgs,
        createPsArgs = createPsArgs,
        matchOnPsArgs = matchOnPsArgs,
        stratifyByPsArgs = NULL,
        computeSharedCovariateBalanceArgs = computeSharedCovariateBalanceArgs,
        computeCovariateBalanceArgs = computeCovariateBalanceArgs,
        fitOutcomeModelArgs = fitOutcomeModelArgs
      )

      analysisId <- analysisId + 1
    }
  }
}

# ============================================================================
# CREATE COHORT METHOD MODULE SPECIFICATIONS
# ============================================================================

cmModuleSettingsCreator <- CohortMethodModule$new()

cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# ============================================================================
# CREATE FINAL ANALYSIS SPECIFICATIONS
# ============================================================================
# Combine all module specifications into a single analysis specification

analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# ============================================================================
# SAVE ANALYSIS SPECIFICATIONS
# ============================================================================

ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "rapidcyclejanssen", "rapidcycljansenAnalysisSpecification.json")
)

cat("Analysis specifications created successfully!\n")
cat("Total number of CM analyses created:", length(cmAnalysisList), "\n")