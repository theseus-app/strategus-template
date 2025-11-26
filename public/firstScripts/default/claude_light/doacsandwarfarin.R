################################################################################
# CohortMethod Analysis Specification for DOACs and Warfarin Study
# 
# This script creates a Strategus analysis specification for comparing
# Direct Oral Anticoagulants (DOACs) versus Warfarin using CohortMethod
# with propensity score adjustment.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Configuration for retrieving cohort definitions from ATLAS
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions - Export from ATLAS
# These are pre-defined cohorts for:
# - Target: DOAC users (cohort ID 1794126)
# - Comparator: Warfarin users (cohort ID 1794132)
# - Outcome: Clinical outcome of interest (cohort ID 1794131)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: DOAC users
    1794132, # Comparator: Warfarin users
    1794131  # Outcome: Clinical outcome
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal reference (1=target, 2=comparator, 3=outcome)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative Control Outcomes
# Retrieved from the negative control concept set (ID: 1888110)
# These outcomes should have no causal relationship with the exposure
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
  # Assign negative control cohort IDs starting from 101
  # (to avoid conflicts with target/comparator/outcome cohorts 1-3)
  dplyr::mutate(cohortId = row_number() + 100) %>%
  dplyr::select(cohortId, cohortName, outcomeConceptId)

# Validation: Ensure no duplicate cohort IDs across all cohort sets
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames for analysis configuration --------------------------------

# Outcome Cohort Definition
# Clinical outcome of interest with a clean washout window of 365 days
oList <- cohortDefinitionSet %>%
  dplyr::filter(.data$cohortId == 3) %>%
  dplyr::mutate(
    outcomeCohortId = cohortId,
    outcomeCohortName = cohortName
  ) %>%
  dplyr::select(outcomeCohortId, outcomeCohortName) %>%
  dplyr::mutate(cleanWindow = 365)

# Target and Comparator Cohorts for CohortMethod Analysis
# Defines the comparison: DOAC (target) vs Warfarin (comparator)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  stringsAsFactors = FALSE
)

# Covariates to Exclude from the propensity score model
# Excludes the drugs of interest to avoid direct adjustment
# (propensity score should capture drug indication, not the drug itself)
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c(),
  stringsAsFactors = FALSE
)

# CohortGeneratorModule --------------------------------------------------------
# Configuration for generating the cohorts in the CDM

cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Create shared resource specifications for negative control outcomes
# detectOnDescendants = TRUE: Include descendant concepts in the concept set
# occurrenceType = "first": Only use the first occurrence of each outcome
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications for cohort generation
# generateStats = TRUE: Calculate descriptive statistics for generated cohorts
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# Configuration for cohort diagnostics and validation

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
# Main comparative effectiveness analysis configuration

# Study Period Definition
# Analysis period: October 19, 2010 to December 31, 2018
studyPeriods <- tibble::tibble(
  studyStartDate = "20101019", # YYYYMMDD format
  studyEndDate = "20181231"     # YYYYMMDD format
)

# Time-at-Risk (TAR) Definitions
# Three different risk windows are evaluated:
# TAR1: 1 day after cohort start to 5 days after cohort end
# TAR2: 1 day after cohort start to cohort end
# TAR3: 1 day after cohort start to 99999 days after cohort start (full follow-up)
timeAtRisks <- tibble::tibble(
  label = c("TAR1: 1d-5d post-cohort", "TAR2: 1d post - cohort end", "TAR3: 1d - end of follow-up"),
  riskWindowStart = c(1, 1, 1),
  startAnchor = c("cohort start", "cohort start", "cohort start"),
  riskWindowEnd = c(5, 0, 99999),
  endAnchor = c("cohort end", "cohort end", "cohort start"),
  minDaysAtRisk = c(1, 1, 1)
)

# Propensity Score Adjustment Methods
# Configuration 1: Match on PS with 1:1 ratio and 0.2 caliper
# Configuration 2: Match on PS with 1:100 ratio and 0.2 caliper
matchOnPsArgsList <- tibble::tibble(
  label = c("PS Match 1:1, cal=0.2", "PS Match 1:100, cal=0.2"),
  maxRatio = c(1, 100),
  caliper = c(0.2, 0.2),
  caliperScale = c("standardized logit", "standardized logit")
)

# Build PS Configuration List
# Each row in the matchOnPsArgsList is converted to a configuration list entry
psConfigList <- list()

# Process matching configurations
if (nrow(matchOnPsArgsList) > 0) {
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

# Build CohortMethod Analysis List
# Iterate through all combinations of study periods, time-at-risks, and PS methods
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Configure Propensity Score Matching
      # maxRatio: Maximum number of comparators per target (1:N matching)
      # caliper: Maximum allowed distance in the propensity score
      # caliperScale: Scale for caliper ("standardized logit" is recommended)
      matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
        maxRatio = psCfg$params$maxRatio,
        caliper = psCfg$params$caliper,
        caliperScale = psCfg$params$caliperScale,
        allowReverseMatch = FALSE,
        stratificationColumns = c()
      )

      stratifyByPsArgs <- NULL

      # Covariate Settings for Propensity Score Model
      # Uses default covariates (demographics, conditions, drugs, procedures, observations)
      # addDescendantsToExclude = TRUE: Exclude concept descendants
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Combine outcome of interest with negative control outcomes
      outcomeList <- append(
        # Primary outcome of interest
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        # Negative control outcomes (should show no effect)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Define target-comparator-outcome triplets
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude the drugs themselves from covariates
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Get Database Cohort Method Data Arguments
      # Retrieves the required data from the CDM for analysis
      # restrictToCommonPeriod = FALSE: Include all available data
      # firstExposureOnly = TRUE: Only use first exposure for each subject
      # washoutPeriod = 0: No washout period required
      # maxCohortSize = 0: No size limit (0 = unlimited)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        firstExposureOnly = TRUE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "remove all",
        covariateSettings = covariateSettings
      )

      # Propensity Score Model Creation Arguments
      # Uses LASSO (Laplace) regularization with cross-validation
      # maxCohortSizeForFitting = 250000: Maximum subjects for PS model fitting
      # errorOnHighCorrelation = TRUE: Stop if high correlations detected
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
          startingVariance = 0.01
        )
      )

      # Compute Shared Covariate Balance (before PS adjustment)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      # Compute Covariate Balance (after PS adjustment)
      # Uses Table 1 specifications for standardized balance metrics
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome Model Fitting Arguments
      # modelType = "cox": Cox proportional hazards model
      # stratified = TRUE: Stratify by matched set (for matched analyses)
      # useCovariates = FALSE: No covariate adjustment in outcome model
      # inversePtWeighting = FALSE: Not using inverse probability of treatment weighting
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
          noiseLevel = "quiet"
        )
      )

      # Create Study Population Arguments
      # Defines inclusion/exclusion criteria and follow-up time
      # restrictToCommonPeriod = FALSE: No common period restriction
      # firstExposureOnly = FALSE: Include all exposures
      # washoutPeriod = 0: No washout required
      # removeDuplicateSubjects = "keep all": Keep all duplicate subjects
      # removeSubjectsWithPriorOutcome = FALSE: Include subjects with prior outcome
      # priorOutcomeLookBack = 99999: Look back indefinitely for prior outcomes
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
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t]
      )

      # Create Analysis Specification
      # Combines all arguments into a single analysis definition
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s | TAR: %s | PS: %s",
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

# Create CohortMethodModule Specifications
cmModuleSettingsCreator <- CohortMethodModule$new()

cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create Complete Analysis Specifications ------------------------------------
# Combines all modules (CohortGenerator, CohortDiagnostics, CohortMethod)
# and shared resources into a single analysis specification object

analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save Analysis Specifications to JSON
# This file can be executed by Strategus to run the analysis
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "doacsandwarfarin", "doacsandwarfarinAnalysisSpecification.json")
)