################################################################################
# OHDSI Strategus Analysis Specification for DOACs and Warfarin Study
# 
# This script creates analysis specifications using the Strategus framework
# for comparative effectiveness/safety research.
#
# Study: doacsandwarfarin
# Target: cohort 1794126 (target1)
# Comparator: cohort 1794132 (comparator1)
# Outcome: cohort 1794131 (outcome1)
# Negative Controls: concept set 1888110 (negative)
################################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Define the base URL for the OHDSI WebAPI instance
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions from ATLAS using their original IDs:
# - 1794126: Target cohort (target1)
# - 1794132: Comparator cohort (comparator1)
# - 1794131: Outcome cohort (outcome1)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to sequential IDs (1, 2, 3) for internal use
# This standardization simplifies downstream reference in analysis settings
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve negative control concept set from ATLAS and resolve to individual concepts
# These are used for empirical calibration to detect residual bias
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Negative control concept set (negative)
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
  mutate(cohortId = row_number() + 100) %>% # Negative controls start at ID 101 to avoid collision with primary cohorts
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no duplicate cohort IDs between primary cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts for each analysis ---------------

# Outcomes: Define the outcome of interest (cohort ID 3)
# cleanWindow specifies the time period to remove duplicate outcomes
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Define the treatment comparison: target (cohort 1) vs comparator (cohort 2)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Excluded Covariate Concepts
# According to specifications, no specific concepts are excluded (id: null, name: "")
# This data frame is left empty as per the covariateSelection settings
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# CohortGeneratorModule --------------------------------------------------------
# This module generates the cohorts defined above in the target database
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for the primary cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource specifications for negative control outcome cohorts
# occurrenceType = "first" means we only consider the first occurrence of each outcome
# detectOnDescendants = TRUE includes descendant concepts in the outcome definition
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Configure the cohort generator module to calculate cohort statistics
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# This module runs diagnostic checks on the generated cohorts to assess quality
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
# This is the main comparative effectiveness analysis module

# Study Period Configuration
# Define the calendar time window for the study
# From specifications: studyStartDate = "20101019", studyEndDate = "20181231"
studyPeriods <- tibble(
  studyStartDate = c("20101019"), # Study begins October 19, 2010
  studyEndDate   = c("20181231")  # Study ends December 31, 2018
)

# Time-at-Risk (TAR) Configuration
# Define multiple risk windows for outcome assessment
# Three TAR periods from specifications:
# 1. Start 1 day after cohort start, end 5 days after cohort end
# 2. Start 1 day after cohort start, end 0 days after cohort end (on treatment)
# 3. Start 1 day after cohort start, end 99999 days after cohort start (intent-to-treat)
timeAtRisks <- tibble(
  label = c("TAR1_CohortStart+1_to_CohortEnd+5",
            "TAR2_CohortStart+1_to_CohortEnd+0",
            "TAR3_CohortStart+1_to_CohortStart+99999"),
  riskWindowStart  = c(1, 1, 1),
  startAnchor = c("cohort start", "cohort start", "cohort start"),
  riskWindowEnd  = c(5, 0, 99999),
  endAnchor = c("cohort end", "cohort end", "cohort start")
) 

# Propensity Score settings - match on PS
# Two matching configurations from specifications:
# 1. 1:1 matching with caliper 0.2 on standardized logit scale
# 2. 1:100 variable ratio matching with caliper 0.2 on standardized logit scale
matchOnPsArgsList <- tibble(
  label = c("PS_Match_1to1_cal0.2", "PS_Match_1to100_cal0.2"),
  maxRatio  = c(1, 100),
  caliper = c(0.2, 0.2),
  caliperScale  = c("standardized logit", "standardized logit")
) 

# Propensity Score settings - stratify by PS
# No stratification specified in the analysis specifications (psSettings only contains matchOnPsArgs)
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0)
) 

# Build a single PS configuration list combining match and stratify approaches
# Each entry specifies: method (match/stratify), label (description), params (method-specific settings)
psConfigList <- list()

# Process "match on PS" configurations
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

# Process "stratify by PS" configurations
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
# This creates a comprehensive analysis grid covering:
# - All study periods (1 in this case)
# - All time-at-risk windows (3 in this case)
# - All PS adjustment methods (2 in this case)
# Total analyses: 1 × 3 × 2 = 6
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment method based on the config type
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

      # Covariate Settings
      # Use default covariate settings (demographics, conditions, drugs, procedures, etc.)
      # addDescendantsToExclude = TRUE means excluded concepts also exclude their descendants
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcome List
      # Combine outcomes of interest with negative control outcomes
      # Outcomes of interest (outcomeOfInterest = TRUE) have unknown true effect
      # Negative controls (outcomeOfInterest = FALSE) have assumed trueEffectSize = 1 (null effect)
      outcomeList <- append(
        # Outcomes of interest
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999 # Look back 99999 days for prior outcome (from specifications)
          )
        }),
        # Negative control outcomes for empirical calibration
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # Assume null effect (HR = 1) for negative controls
          )
        })
      )
      
      # Target-Comparator-Outcomes List
      # Define the target-comparator pairs and their associated outcomes
      # excludedCovariateConceptIds excludes the treatment drugs from covariate construction
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId # Empty based on specifications
        )
      }

      # GetDbCohortMethodData Arguments
      # Configure how data is extracted from the database
      # From specifications:
      # - restrictToCommonPeriod: FALSE (use all available data for each cohort)
      # - studyStartDate/studyEndDate: restrict to study period
      # - maxCohortSize: 0 (no limit, use all eligible subjects)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE, # From getDbCohortMethodDataArgs.restrictToCommonPeriod
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From getDbCohortMethodDataArgs.maxCohortSize
        covariateSettings = covariateSettings
      )

      # Create Propensity Score Arguments
      # Configure propensity score model fitting
      # From specifications:
      # - maxCohortSizeForFitting: 250000
      # - errorOnHighCorrelation: TRUE
      # - prior: Laplace with cross-validation
      # - control: tolerance 2e-7, auto CV, 10-fold, 10 repetitions, silent, resetCoefficients, startingVariance 0.01
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE,    # From propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Allow Strategus to continue if PS model fails
        estimator = "att",   # Average treatment effect in the treated
        prior = Cyclops::createPrior(
          priorType = "laplace",        # From propensityScoreAdjustment.createPsArgs.prior.priorType
          exclude = c(0), 
          useCrossValidation = TRUE     # From propensityScoreAdjustment.createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",        # From propensityScoreAdjustment.createPsArgs.control.noiseLevel
          cvType = "auto",              # From propensityScoreAdjustment.createPsArgs.control.cvType
          seed = 1, 
          resetCoefficients = TRUE,     # From propensityScoreAdjustment.createPsArgs.control.resetCoefficients
          tolerance = 2e-07,            # From propensityScoreAdjustment.createPsArgs.control.tolerance
          cvRepetitions = 10,           # From propensityScoreAdjustment.createPsArgs.control.cvRepetitions
          fold = 10,                    # From propensityScoreAdjustment.createPsArgs.control.fold
          startingVariance = 0.01       # From propensityScoreAdjustment.createPsArgs.control.startingVariance
        )
      )

      # Compute Shared Covariate Balance Arguments
      # Calculate covariate balance across all covariates for diagnostics
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL # No filter, compute for all covariates
      )
      
      # Compute Covariate Balance Arguments
      # Calculate covariate balance for Table 1 covariates (key baseline characteristics)
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit Outcome Model Arguments
      # Configure the outcome model (Cox proportional hazards regression)
      # From specifications:
      # - modelType: "cox" (Cox regression)
      # - stratified: TRUE (stratified on matched pairs/strata)
      # - useCovariates: FALSE (don't include covariates in outcome model, PS adjustment only)
      # - inversePtWeighting: FALSE (no IPTW)
      # - prior: Laplace with cross-validation
      # - control: tolerance 2e-7, auto CV, 10-fold, 10 repetitions, quiet, resetCoefficients, startingVariance 0.01
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",             # From fitOutcomeModelArgs.modelType
        stratified = TRUE,             # From fitOutcomeModelArgs.stratified
        useCovariates = FALSE,         # From fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE,    # From fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior(
          priorType = "laplace",       # From fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE    # From fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          cvType = "auto",             # From fitOutcomeModelArgs.control.cvType
          seed = 1, 
          resetCoefficients = TRUE,    # From fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01,     # From fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07,           # From fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10,          # From fitOutcomeModelArgs.control.cvRepetitions
          fold = 10,                   # From fitOutcomeModelArgs.control.fold
          noiseLevel = "quiet"         # From fitOutcomeModelArgs.control.noiseLevel
        )
      )
      
      # Create Study Population Arguments
      # Configure how the study population is defined from the target and comparator cohorts
      # From specifications:
      # - restrictToCommonPeriod: FALSE
      # - firstExposureOnly: FALSE
      # - washoutPeriod: 0
      # - removeDuplicateSubjects: "keep all"
      # - censorAtNewRiskWindow: FALSE
      # - removeSubjectsWithPriorOutcome: FALSE
      # - priorOutcomeLookBack: 99999
      # - timeAtRisks: defined per TAR configuration
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,          # From createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE,               # From createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0,                       # From createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all",    # From createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE,           # From createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = FALSE,  # From createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999,            # From createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1                        # From createStudyPopArgs.timeAtRisks[].minDaysAtRisk
      )

      # Append the complete analysis configuration to the analysis list
      # Each analysis is uniquely identified and includes all necessary settings
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

# Create CohortMethod Module Specifications
# Package all CohortMethod analyses into the module specification
# - refitPsForEveryOutcome: FALSE (reuse PS model across outcomes)
# - refitPsForEveryStudyPopulation: FALSE (reuse PS model across study populations)
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
# Combine all modules and shared resources into a single analysis specification
# This specification can be executed by Strategus across multiple data sources
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to JSON file
# This file can be version controlled and shared with collaborators
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "doacsandwarfarin", "doacsandwarfarinAnalysisSpecification.json")
)