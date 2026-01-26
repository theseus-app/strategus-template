################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates the analysis specifications for the ranitidinecancer study
# using the OHDSI Strategus package.
# 
# Study Overview:
# - Target Cohort: target1 (ID: 1794126)
# - Comparator Cohort: comparator1 (ID: 1794132)
# - Outcome Cohort: outcome1 (ID: 1794131)
# - Negative Control Concept Set: negative (ID: 1888110)
# 
# Analysis Settings:
# - Washout Period: 365 days (for getDbCohortMethodDataArgs)
# - Remove subjects with prior outcome: Yes (lookback: 99999 days)
# - Multiple time-at-risk windows defined
# - Multiple PS adjustment methods: 1:1 matching, 1:10 matching, stratification, and unadjusted
# - Cox proportional hazards model for outcome fitting
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
################################################################################

library(dplyr)
library(Strategus)

# ==============================================================================
# Shared Resources
# ==============================================================================
# Define the base URL for the OHDSI WebAPI to retrieve cohort definitions
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ------------------------------------------------------------------------------
# Cohort Definitions
# ------------------------------------------------------------------------------
# Export cohort definitions from Atlas using the specified cohort IDs
# These cohorts are defined in the Analysis Specifications:
# - Target: target1 (ID: 1794126)
# - Comparator: comparator1 (ID: 1794132)
# - Outcome: outcome1 (ID: 1794131)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to use sequential IDs for easier reference
# This mapping ensures consistent cohort identification throughout the analysis:
# - Target cohort (1794126) -> 1
# - Comparator cohort (1794132) -> 2
# - Outcome cohort (1794131) -> 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# ------------------------------------------------------------------------------
# Negative Control Outcomes
# ------------------------------------------------------------------------------
# Retrieve negative control concepts from the concept set defined in Atlas
# Negative controls are used to detect potential systematic bias in the study
# Concept Set ID: 1888110 (name: negative)
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
  # Assign cohort IDs starting from 101 to avoid conflicts with main cohorts
  # Main cohorts use IDs 1, 2, 3; negative controls use 101, 102, 103, etc.
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Validate that there are no duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# ------------------------------------------------------------------------------
# Define Cohort Lists for Analysis
# ------------------------------------------------------------------------------
# Outcomes list: Contains the outcome cohort(s) for the study
# outcome1 (ID: 1794131, renumbered to 3)
# cleanWindow: 365 days - period to look back for prior outcomes
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator pairs for CohortMethod analysis
# Defines the comparison: target1 vs comparator1
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# ------------------------------------------------------------------------------
# Covariate Exclusions
# ------------------------------------------------------------------------------
# Note: The Analysis Specifications indicate no specific concepts to exclude
# (conceptsToExclude id is null). However, we create an empty data frame
# to maintain the template structure. In practice, you would typically exclude
# the drug concepts for target and comparator to avoid confounding.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# ==============================================================================
# CohortGeneratorModule Settings
# ==============================================================================
# The CohortGenerator module is responsible for creating the cohorts in the CDM
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource specifications for negative control outcome cohorts
# - occurrenceType: "first" - use only the first occurrence of each negative control
# - detectOnDescendants: TRUE - include descendant concepts when identifying outcomes
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications for cohort generation
# generateStats: TRUE - generate inclusion rule statistics for cohort diagnostics
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ==============================================================================
# CohortDiagnosticsModule Settings
# ==============================================================================
# The CohortDiagnostics module provides comprehensive diagnostics for cohort definitions
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Configure which diagnostic analyses to run
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE,           # Statistics on inclusion rule impact
  runIncludedSourceConcepts = TRUE,        # Source concepts included in cohort
  runOrphanConcepts = TRUE,                # Concepts that might be missing from definition
  runTimeSeries = FALSE,                   # Time series of cohort entry
  runVisitContext = TRUE,                  # Visit context at cohort entry
  runBreakdownIndexEvents = TRUE,          # Breakdown of index events
  runIncidenceRate = TRUE,                 # Incidence rate calculations
  runCohortRelationship = TRUE,            # Relationships between cohorts
  runTemporalCohortCharacterization = TRUE, # Temporal characterization
  minCharacterizationMean = 0.01           # Minimum mean for characterization features
)

# ==============================================================================
# CohortMethodModule Settings
# ==============================================================================

# ------------------------------------------------------------------------------
# Study Periods
# ------------------------------------------------------------------------------
# The Analysis Specifications indicate no specific study period restrictions
# (studyStartDate and studyEndDate are empty strings)
# Creating a single row with empty dates to allow unrestricted study period
studyPeriods <- tibble(
  studyStartDate = c(""), # Empty string means no start date restriction
  studyEndDate   = c("")  # Empty string means no end date restriction
)

# ------------------------------------------------------------------------------
# Time-at-Risk Windows
# ------------------------------------------------------------------------------
# Define multiple time-at-risk (TAR) windows as specified in the Analysis Specifications
# Each TAR defines when outcomes are counted relative to cohort entry/exit
# 
# TAR 1: From day 1 after cohort start to day 99999 (essentially unlimited)
# TAR 2: From day 365 after cohort start to day 99999 (1-year lag)
# TAR 3: From day 1 after cohort start to cohort end (on-treatment)
# TAR 4: From day 365 after cohort start to cohort end (1-year lag, on-treatment)
timeAtRisks <- tibble(
  label = c(
    "TAR: 1 to 99999 from cohort start",
    "TAR: 365 to 99999 from cohort start", 
    "TAR: 1 to cohort end (on-treatment)",
    "TAR: 365 to cohort end (1-year lag on-treatment)"
  ),
  riskWindowStart = c(1, 365, 1, 365),
  startAnchor = c("cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd = c(99999, 99999, 0, 0),
  endAnchor = c("cohort start", "cohort start", "cohort end", "cohort end"),
  minDaysAtRisk = c(1, 1, 1, 1)
)

# ------------------------------------------------------------------------------
# Propensity Score Settings - Match on PS
# ------------------------------------------------------------------------------
# Define propensity score matching configurations
# Two matching strategies are specified:
# 1. 1:1 matching with caliper 0.2 on standardized logit scale
# 2. 1:10 variable ratio matching with caliper 0.2 on standardized logit scale
matchOnPsArgsList <- tibble(
  label = c("1:1 PS Matching", "1:10 PS Matching"),
  maxRatio = c(1, 10),
  caliper = c(0.2, 0.2),
  caliperScale = c("standardized logit", "standardized logit")
)

# ------------------------------------------------------------------------------
# Propensity Score Settings - Stratify by PS
# ------------------------------------------------------------------------------
# Define propensity score stratification configuration
# Stratification into 10 strata using all subjects
stratifyByPsArgsList <- tibble(
  label = c("PS Stratification (10 strata)"),
  numberOfStrata = c(10),
  baseSelection = c("all")
)

# ------------------------------------------------------------------------------
# Build PS Configuration List
# ------------------------------------------------------------------------------
# Combine all PS adjustment methods into a single configuration list
# This includes: matching methods, stratification methods, and unadjusted analysis
psConfigList <- list()

# Add matching configurations
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

# Add stratification configurations
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

# Add unadjusted analysis (no PS matching or stratification)
# This corresponds to the fourth PS setting in the specifications where both
# matchOnPsArgs and stratifyByPsArgs are null
psConfigList[[length(psConfigList) + 1]] <- list(
  method = "unadjusted",
  label  = "Unadjusted (No PS Adjustment)",
  params = NULL
)

# ------------------------------------------------------------------------------
# Build CohortMethod Analysis List
# ------------------------------------------------------------------------------
# Iterate through all combinations of:
# - Study periods
# - Time-at-risk windows
# - PS adjustment methods
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]
  
  for (t in seq_len(nrow(timeAtRisks))) {
    
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment based on method type
      if (psCfg$method == "match") {
        # Propensity score matching configuration
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # Propensity score stratification configuration
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      } else {
        # Unadjusted analysis - no PS adjustment
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- NULL
      }
      
      # ----------------------------------------------------------------------
      # Covariate Settings
      # ----------------------------------------------------------------------
      # Use default covariate settings from FeatureExtraction
      # addDescendantsToExclude: TRUE - when excluding concepts, also exclude descendants
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )
      
      # ----------------------------------------------------------------------
      # Outcome List
      # ----------------------------------------------------------------------
      # Create outcome objects for both outcomes of interest and negative controls
      outcomeList <- append(
        # Outcomes of interest (from oList)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,  # Unknown true effect
            priorOutcomeLookback = 99999  # As specified in createStudyPopArgs
          )
        }),
        # Negative control outcomes (true effect size = 1, i.e., no effect)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1  # Negative controls should have no effect
          )
        })
      )
      
      # ----------------------------------------------------------------------
      # Target-Comparator-Outcomes List
      # ----------------------------------------------------------------------
      # Define the target-comparator pairs with their associated outcomes
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }
      
      # ----------------------------------------------------------------------
      # getDbCohortMethodDataArgs
      # ----------------------------------------------------------------------
      # Settings for extracting cohort method data from the database
      # Based on Analysis Specifications:
      # - maxCohortSize: 0 (no limit)
      # - restrictToCommonPeriod: FALSE
      # - firstExposureOnly: FALSE
      # - washoutPeriod: 365 days
      # - removeDuplicateSubjects: "keep first"
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        firstExposureOnly = FALSE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "keep first",
        covariateSettings = covariateSettings
      )
      
      # ----------------------------------------------------------------------
      # createPsArgs
      # ----------------------------------------------------------------------
      # Settings for propensity score model creation
      # Based on Analysis Specifications:
      # - maxCohortSizeForFitting: 250000
      # - errorOnHighCorrelation: TRUE
      # - Prior: Laplace with cross-validation
      # - Control: tolerance 2e-7, auto CV, 10 folds, 10 repetitions
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,  # Allow Strategus to complete all operations
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
      
      # ----------------------------------------------------------------------
      # Covariate Balance Args
      # ----------------------------------------------------------------------
      # Settings for computing covariate balance before and after PS adjustment
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )
      
      # ----------------------------------------------------------------------
      # fitOutcomeModelArgs
      # ----------------------------------------------------------------------
      # Settings for fitting the outcome model
      # Based on Analysis Specifications:
      # - modelType: "cox" (Cox proportional hazards)
      # - stratified: TRUE
      # - useCovariates: FALSE
      # - inversePtWeighting: FALSE
      # - Prior: Laplace with cross-validation
      # - Control: tolerance 2e-7, auto CV, 10 folds, 10 repetitions, quiet noise
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
      
      # ----------------------------------------------------------------------
      # createStudyPopArgs
      # ----------------------------------------------------------------------
      # Settings for creating the study population
      # Based on Analysis Specifications:
      # - restrictToCommonPeriod: FALSE
      # - firstExposureOnly: FALSE
      # - washoutPeriod: 0 (already applied in getDbCohortMethodDataArgs)
      # - removeDuplicateSubjects: "keep all"
      # - censorAtNewRiskWindow: FALSE
      # - removeSubjectsWithPriorOutcome: TRUE
      # - priorOutcomeLookBack: 99999
      # - Time-at-risk settings from current iteration
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
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999
      )
      
      # ----------------------------------------------------------------------
      # Create CohortMethod Analysis
      # ----------------------------------------------------------------------
      # Combine all settings into a single analysis specification
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(studyStartDate == "", "NoStart", studyStartDate),
          ifelse(studyEndDate == "", "NoEnd", studyEndDate),
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

# ------------------------------------------------------------------------------
# Create CohortMethod Module Specifications
# ------------------------------------------------------------------------------
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# ==============================================================================
# Create the Analysis Specifications
# ==============================================================================
# Combine all module specifications into a single analysis specification object
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# ==============================================================================
# Save the Analysis Specifications
# ==============================================================================
# Save the complete analysis specifications to a JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ranitidinecancer", "ranitidinecancerAnalysisSpecification.json")
)