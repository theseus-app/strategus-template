################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates analysis specifications for the "corazon" study using the
# OHDSI Strategus package. It defines cohort definitions, negative control outcomes,
# propensity score adjustment methods, and outcome model specifications.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
# ##############################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Import target, comparator, and outcome cohorts from ATLAS
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to sequential IDs for internal use
# Original IDs are preserved in cohortName field
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Import negative control concept set from ATLAS and resolve to actual concepts
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
  # Assign negative control cohort IDs starting at 101 to avoid conflicts with main cohorts (1, 2, 3)
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Validate no duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeChortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to organize cohorts for analysis
# Outcomes: Define outcomes of interest with lookback window
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# CohortGeneratorModule --------------------------------------------------------
# Initialize the CohortGenerator module to handle cohort creation
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for main cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Create shared resource specifications for negative control outcomes
# detectOnDescendants: TRUE ensures we capture all descendant concepts in the concept hierarchy
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeChortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Configure the CohortGenerator module specifications
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# Initialize the CohortDiagnostics module for cohort quality checks
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Configure comprehensive diagnostics for all cohorts
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
# Define study periods from Analysis Specifications
# Two periods: 2010-2019 and 2012-2019
studyPeriods <- tibble(
  studyStartDate = c("20100101", "20120101"), # YYYYMMDD format
  studyEndDate   = c("20191231", "20191231")  # YYYYMMDD format
)

# Define Time-At-Risks (TARs) for outcomes
# TAR 1: Cohort start to cohort end
# TAR 2: Cohort start to 9999 days after cohort start
timeAtRisks <- tibble(
  label = c("tar1_cohort_start_to_end", "tar2_cohort_start_to_9999d"),
  riskWindowStart  = c(0, 0),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd  = c(0, 9999),
  endAnchor = c("cohort end", "cohort start"),
  minDaysAtRisk = c(1, 1)
)

# Propensity Score settings - Stratification configuration
# 5 strata using all subjects as base selection
stratifyByPsArgsList <- tibble(
  label = c("stratify_5strata"),
  numberOfStrata  = c(5),
  baseSelection = c("all")
)

# Propensity Score settings - Matching configuration
# Max ratio 0 (optimal matching), caliper 0.2 on standardized logit scale
matchOnPsArgsList <- tibble(
  label = c("match_caliper_0.2"),
  maxRatio  = c(0),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# Build a single PS configuration list (each entry has: method, label, params)
# This allows multiple PS adjustment strategies to be applied in sequence
psConfigList <- list()

# Process "stratify by PS" configurations
# Each row becomes a separate analysis configuration
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

# Process "match on PS" configurations
# Each row becomes a separate analysis configuration
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

# Iterate through all analysis setting combinations
# Creates analyses for all combinations of study periods, TARs, and PS methods
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {
    riskWindowStart <- timeAtRisks$riskWindowStart[t]
    startAnchor <- timeAtRisks$startAnchor[t]
    riskWindowEnd <- timeAtRisks$riskWindowEnd[t]
    endAnchor <- timeAtRisks$endAnchor[t]
    minDaysAtRisk <- timeAtRisks$minDaysAtRisk[t]

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Configure propensity score adjustment method
      # Initialize both as NULL, then populate based on method type
      if (psCfg$method == "match") {
        # Matching configuration
        # maxRatio = 0 enables optimal matching
        # caliper = 0.2 on standardized logit scale
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # Stratification configuration
        # numberOfStrata = 5 creates 5 propensity score strata
        # baseSelection = "all" uses all subjects for boundary calculations
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Define covariate settings for confounding adjustment
      # Uses default covariates including demographics, conditions, drugs, procedures, etc.
      # addDescendantsToExclude = TRUE ensures excluded concepts include their descendants
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build outcome list combining outcomes of interest and negative controls
      outcomeList <- append(
        # Outcomes of interest: from main cohort definitions
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,  # Unknown true effect for outcomes of interest
            priorOutcomeLookback = 99999  # Look back all available time for prior outcome
          )
        }),
        # Negative control outcomes: not expected to have effect (trueEffectSize = 1 means no effect)
        lapply(negativeControlOutcomeChortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1  # No expected effect for negative controls
          )
        })
      )

      # Create target-comparator-outcome combinations
      # Each combination specifies which exposures and outcomes to analyze together
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = c()  # No specific concepts excluded from covariates
        )
      }

      # Configure data extraction from database
      # maxCohortSize = 0 means no maximum limit
      # restrictToCommonPeriod = FALSE allows different follow-up times for target and comparator
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Configure propensity score model fitting
      # Uses regularized logistic regression with Laplace prior via Cyclops
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # Max sample size for PS model
        errorOnHighCorrelation = TRUE,     # Fail if high multicollinearity detected
        stopOnError = FALSE,               # Continue with analysis even if PS fitting fails
        estimator = "att",                 # Average Treatment Effect on Treated
        # Prior specification: Laplace (L1 regularization) with cross-validation
        prior = Cyclops::createPrior(
          priorType = "laplace",
          exclude = c(0),
          useCrossValidation = TRUE
        ),
        # Cyclops optimization control parameters
        control = Cyclops::createControl(
          noiseLevel = "silent",
          cvType = "auto",
          seed = 1,
          resetCoefficients = TRUE,
          tolerance = 2e-07,      # Convergence tolerance
          cvRepetitions = 10,     # Cross-validation repetitions
          startingVariance = 0.01
        )
      )

      # Compute covariate balance before and after PS adjustment
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        # Use default Table 1 specifications for balance assessment
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Configure Cox proportional hazards outcome model
      # Stratified = TRUE uses separate baseline hazards for each matched pair or stratum
      # useCovariates = FALSE means no additional covariate adjustment after PS matching/stratification
      # inversePtWeighting = FALSE (not applicable to matching/stratification)
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = TRUE,
        useCovariates = FALSE,
        inversePtWeighting = FALSE,
        # Prior specification: Laplace with cross-validation for regularization
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE
        ),
        # Cyclops optimization control parameters
        control = Cyclops::createControl(
          cvType = "auto",
          seed = 1,
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          tolerance = 2e-07,
          cvRepetitions = 10,
          noiseLevel = "quiet"   # Less verbose output than PS fitting
        )
      )

      # Configure study population creation
      # removeSubjectsWithPriorOutcome = TRUE excludes those with outcome in lookback window
      # priorOutcomeLookBack = 99999 looks back entire available time
      # removeDuplicateSubjects = "keep all" preserves all subject instances
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = riskWindowStart,
        startAnchor = startAnchor,
        riskWindowEnd = riskWindowEnd,
        endAnchor = endAnchor,
        minDaysAtRisk = minDaysAtRisk
      )

      # Combine all settings into a single analysis specification
      # Each analysis is uniquely identified by study period, TAR, and PS method combination
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

# Initialize CohortMethod module and create module specifications
cmModuleSettingsCreator <- CohortMethodModule$new()

# Package all CohortMethod analyses with their target-comparator-outcome associations
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the analysis specifications -------------------------------------------
# Combine all module specifications into comprehensive analysis plan
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Add shared cohort definitions used across all modules
  Strategus::addSharedResources(cohortDefinitionShared) |>
  # Add shared negative control outcomes
  Strategus::addSharedResources(negativeControlsShared) |>
  # Add CohortGenerator module (creates cohorts in database)
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  # Add CohortDiagnostics module (performs cohort quality checks)
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  # Add CohortMethod module (performs comparative effectiveness analysis)
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save analysis specifications to JSON file
# This file can be used to execute the analysis on different data sources
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "corazon", "corazonAnalysisSpecification.json")
)