################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates analysis specifications for the "alzheimerdonepezil" study
# using the OHDSI Strategus package.
#
# Study Overview:
# - Target Cohort: target1 (ID: 1794126)
# - Comparator Cohort: comparator1 (ID: 1794132)
# - Outcome Cohort: outcome1 (ID: 1794131)
# - Negative Control Concept Set: negative (ID: 1888110)
#
# Propensity Score Adjustment:
# - Two matching strategies: 1:1 matching and 1:3 matching
# - Both use caliper of 0.2 on standardized logit scale
#
# Time-at-Risk:
# - Risk window: Day 1 to Day 180 from cohort start
# - Minimum 1 day at risk required
#
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# further details.
################################################################################

library(dplyr)
library(Strategus)

# ==============================================================================
# Shared Resources
# ==============================================================================
# Define the base URL for the OHDSI WebAPI to retrieve cohort definitions
# and concept sets from ATLAS
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ------------------------------------------------------------------------------
# Cohort Definitions
# ------------------------------------------------------------------------------
# Export cohort definitions from ATLAS for:
# - Target cohort (target1): ID 1794126
# - Comparator cohort (comparator1): ID 1794132
# - Outcome cohort (outcome1): ID 1794131
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to use sequential IDs starting from 1
# This simplifies referencing cohorts throughout the analysis
# Original ID 1794126 (target1) -> New ID 1
# Original ID 1794132 (comparator1) -> New ID 2
# Original ID 1794131 (outcome1) -> New ID 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# ------------------------------------------------------------------------------
# Negative Control Outcomes
# ------------------------------------------------------------------------------
# Retrieve the negative control concept set from ATLAS
# Concept Set ID: 1888110 (name: negative)
# Negative controls are used to detect residual confounding and systematic bias
# They are outcomes that are not expected to be causally related to the exposure
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

# ==============================================================================
# Create Data Frames for Analysis Configuration
# ==============================================================================

# ------------------------------------------------------------------------------
# Outcomes List
# ------------------------------------------------------------------------------
# Define the outcomes of interest for the study
# cleanWindow: lookback period (in days) for identifying prior outcomes
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%  # Filter to outcome cohort (outcome1)
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)  # 365-day lookback for prior outcome assessment

# ------------------------------------------------------------------------------
# Target and Comparator Cohorts for CohortMethod Analysis
# ------------------------------------------------------------------------------
# Define the target-comparator pairs for the comparative effectiveness analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# ------------------------------------------------------------------------------
# Excluded Covariate Concepts
# ------------------------------------------------------------------------------
# Note: In the analysis specifications, conceptsToExclude has null ID
# Since no specific concepts are provided for exclusion, we create an empty data frame
# If specific drug concepts need to be excluded (e.g., the study drugs themselves),
# they should be added here
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# Note: conceptsToInclude is also null in the specifications
# This means all default covariates will be included (no restriction)
# If you want to restrict to specific concepts, uncomment and populate:
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# ==============================================================================
# CohortGeneratorModule Settings
# ==============================================================================
# The CohortGenerator module is responsible for creating the cohorts in the CDM
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for cohort definitions
# This allows the cohort definitions to be shared across multiple modules
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource specifications for negative control outcome cohorts
# occurrenceType = "first": Use only the first occurrence of each negative control outcome
# detectOnDescendants = TRUE: Include descendant concepts when identifying outcomes
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications for cohort generation
# generateStats = TRUE: Generate inclusion rule statistics for cohort diagnostics
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
  cohortIds = cohortDefinitionSet$cohortId,  # Run diagnostics for all main cohorts
  runInclusionStatistics = TRUE,              # Analyze inclusion rule impact
  runIncludedSourceConcepts = TRUE,           # Identify source concepts in cohort
  runOrphanConcepts = TRUE,                   # Find potentially missing concepts
  runTimeSeries = FALSE,                      # Skip time series analysis
  runVisitContext = TRUE,                     # Analyze visit context of cohort entries
  runBreakdownIndexEvents = TRUE,             # Break down index events by concept
  runIncidenceRate = TRUE,                    # Calculate incidence rates
  runCohortRelationship = TRUE,               # Analyze relationships between cohorts
  runTemporalCohortCharacterization = TRUE,   # Temporal characterization of cohorts
  minCharacterizationMean = 0.01              # Minimum prevalence threshold for reporting
)

# ==============================================================================
# CohortMethodModule Settings
# ==============================================================================
# The CohortMethod module performs the comparative cohort analysis using
# propensity score methods

# ------------------------------------------------------------------------------
# Study Periods Configuration
# ------------------------------------------------------------------------------
# From specifications: studyStartDate and studyEndDate are empty strings
# This means no restriction on study period - use all available data
# restrictToCommonPeriod = TRUE in getDbCohortMethodDataArgs will ensure
# both cohorts have overlapping observation time
studyPeriods <- tibble(
  studyStartDate = c(""),  # Empty string = no start date restriction
  studyEndDate   = c("")   # Empty string = no end date restriction
)

# ------------------------------------------------------------------------------
# Time-at-Risk (TAR) Configuration
# ------------------------------------------------------------------------------
# Define the risk window for outcome assessment
# From specifications:
# - riskWindowStart: 1 (day 1 after cohort start)
# - startAnchor: "cohort start"
# - riskWindowEnd: 180 (day 180 after cohort start)
# - endAnchor: "cohort start"
# - minDaysAtRisk: 1 (require at least 1 day of follow-up)
timeAtRisks <- tibble(
  label = c("TAR 1-180 days"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(180),
  endAnchor = c("cohort start")
)

# ------------------------------------------------------------------------------
# Propensity Score Settings - Match on PS
# ------------------------------------------------------------------------------
# Two matching configurations are specified:
# 1. 1:1 matching with caliper 0.2 on standardized logit scale
# 2. 1:3 matching with caliper 0.2 on standardized logit scale
matchOnPsArgsList <- tibble(
  label = c("1:1 PS Matching", "1:3 PS Matching"),
  maxRatio  = c(1, 3),
  caliper = c(0.2, 0.2),
  caliperScale  = c("standardized logit", "standardized logit")
)

# ------------------------------------------------------------------------------
# Propensity Score Settings - Stratify by PS
# ------------------------------------------------------------------------------
# No stratification settings specified in the analysis specifications
# stratifyByPsArgs is null for both PS configurations
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0)
)

# ------------------------------------------------------------------------------
# Build PS Configuration List
# ------------------------------------------------------------------------------
# Combine matching and stratification configurations into a single list
# Each entry contains: method (match/stratify), label, and parameters
psConfigList <- list()

# Process matching configurations
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

# Process stratification configurations (none in this study)
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

# ------------------------------------------------------------------------------
# Build CohortMethod Analysis List
# ------------------------------------------------------------------------------
# Iterate through all combinations of:
# - Study periods (1 configuration: no date restriction)
# - Time-at-risk windows (1 configuration: days 1-180)
# - PS adjustment methods (2 configurations: 1:1 and 1:3 matching)
# Total: 1 x 1 x 2 = 2 analyses
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment method based on configuration
      if (psCfg$method == "match") {
        # Create matching arguments
        # allowReverseMatch = FALSE: Only match comparators to targets
        # stratificationColumns = c(): No additional stratification variables
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

      # ----------------------------------------------------------------------
      # Covariate Settings
      # ----------------------------------------------------------------------
      # Use default covariate settings from FeatureExtraction
      # addDescendantsToExclude = TRUE: When excluding concepts, also exclude descendants
      # Note: No specific concepts to include or exclude based on specifications
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # ----------------------------------------------------------------------
      # Outcome List
      # ----------------------------------------------------------------------
      # Create outcome objects for both outcomes of interest and negative controls
      outcomeList <- append(
        # Outcomes of interest (outcome1)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,      # This is a primary outcome
            trueEffectSize = NA,           # Unknown true effect (not a negative control)
            priorOutcomeLookback = 99999   # From specifications: priorOutcomeLookBack = 99999
          )
        }),
        # Negative control outcomes
        # trueEffectSize = 1: Expected hazard ratio of 1 (no effect)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,     # Not a primary outcome
            trueEffectSize = 1             # Expected null effect for calibration
          )
        })
      )
      
      # ----------------------------------------------------------------------
      # Target-Comparator-Outcomes List
      # ----------------------------------------------------------------------
      # Link target and comparator cohorts with outcomes
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude covariate concepts to prevent confounding by indication
          excludedCovariateConceptIds = c(
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # ----------------------------------------------------------------------
      # Get Database Cohort Method Data Arguments
      # ----------------------------------------------------------------------
      # Configure how to extract data from the CDM
      # From specifications:
      # - restrictToCommonPeriod: TRUE (ensure overlapping observation periods)
      # - maxCohortSize: 0 (no limit)
      # - firstExposureOnly: FALSE (include all exposures)
      # - washoutPeriod: 0 (no washout required)
      # - removeDuplicateSubjects: "keep all"
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # ----------------------------------------------------------------------
      # Create Propensity Score Arguments
      # ----------------------------------------------------------------------
      # Configure the propensity score model fitting
      # From specifications:
      # - maxCohortSizeForFitting: 250000
      # - errorOnHighCorrelation: TRUE
      # - Prior: Laplace with cross-validation
      # - Control: tolerance 2e-7, fold 10, cvRepetitions 10
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,  # Allow Strategus to complete even if PS model fails
        estimator = "att",    # Average treatment effect on the treated
        prior = Cyclops::createPrior(
          priorType = "laplace",           # From specifications
          exclude = c(0),                   # Exclude intercept from regularization
          useCrossValidation = TRUE         # From specifications
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",            # From specifications
          cvType = "auto",                  # From specifications
          seed = 1,
          resetCoefficients = TRUE,         # From specifications
          tolerance = 2e-07,                # From specifications
          cvRepetitions = 10,               # From specifications
          fold = 10,                        # From specifications
          startingVariance = 0.01           # From specifications
        )
      )

      # ----------------------------------------------------------------------
      # Covariate Balance Arguments
      # ----------------------------------------------------------------------
      # Configure covariate balance computation for diagnostics
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL  # Compute balance for all covariates
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()  # Table 1 covariates
      )

      # ----------------------------------------------------------------------
      # Fit Outcome Model Arguments
      # ----------------------------------------------------------------------
      # Configure the outcome model fitting
      # From specifications:
      # - modelType: "logistic" (note: typically "cox" for time-to-event)
      # - stratified: TRUE (stratify by matched/stratified sets)
      # - useCovariates: FALSE (no outcome model covariates)
      # - inversePtWeighting: FALSE (use matching, not weighting)
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "logistic",             # From specifications
        stratified = TRUE,                  # From specifications
        useCovariates = FALSE,              # From specifications
        inversePtWeighting = FALSE,         # From specifications
        prior = Cyclops::createPrior(
          priorType = "laplace",            # From specifications
          useCrossValidation = TRUE         # From specifications
        ),
        control = Cyclops::createControl(
          cvType = "auto",                  # From specifications
          seed = 1,
          resetCoefficients = TRUE,         # From specifications
          startingVariance = 0.01,          # From specifications
          tolerance = 2e-07,                # From specifications
          cvRepetitions = 10,               # From specifications
          fold = 10,                        # From specifications
          noiseLevel = "quiet"              # From specifications
        )
      )
      
      # ----------------------------------------------------------------------
      # Create Study Population Arguments
      # ----------------------------------------------------------------------
      # Configure how to create the study population
      # From specifications:
      # - restrictToCommonPeriod: FALSE
      # - firstExposureOnly: FALSE
      # - washoutPeriod: 0
      # - removeDuplicateSubjects: "keep all"
      # - censorAtNewRiskWindow: FALSE
      # - removeSubjectsWithPriorOutcome: TRUE
      # - priorOutcomeLookBack: 99999
      # - Time-at-risk: Day 1 to Day 180 from cohort start
      # - minDaysAtRisk: 1
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,           # From specifications
        firstExposureOnly = FALSE,                # From specifications
        washoutPeriod = 0,                        # From specifications
        removeDuplicateSubjects = "keep all",     # From specifications
        censorAtNewRiskWindow = FALSE,            # From specifications
        removeSubjectsWithPriorOutcome = TRUE,    # From specifications
        priorOutcomeLookback = 99999,             # From specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t],   # 1
        startAnchor = timeAtRisks$startAnchor[t],           # "cohort start"
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],       # 180
        endAnchor = timeAtRisks$endAnchor[t],               # "cohort start"
        minDaysAtRisk = 1,                        # From specifications
        maxDaysAtRisk = 99999
      )

      # ----------------------------------------------------------------------
      # Create CohortMethod Analysis Object
      # ----------------------------------------------------------------------
      # Combine all settings into a single analysis configuration
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
  analysesToExclude = NULL,                    # Run all analyses
  refitPsForEveryOutcome = FALSE,              # Use same PS for all outcomes
  refitPsForEveryStudyPopulation = FALSE,      # Use same PS for all study populations
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# ==============================================================================
# Create the Analysis Specifications
# ==============================================================================
# Combine all module specifications into a single analysis specification object
# This object can be saved and used to execute the study across multiple databases
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
# This file can be shared with study sites for execution
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "alzheimerdonepezil", "alzheimerdonepezilAnalysisSpecification.json")
)