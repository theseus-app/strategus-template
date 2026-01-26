################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates analysis specifications for the sglt2imetformin study
# using the OHDSI Strategus package.
# 
# Study Design:
# - Target Cohort: target1 (ID: 1794126)
# - Comparator Cohort: comparator1 (ID: 1794132)
# - Outcome Cohort: outcome1 (ID: 1794131)
# - Negative Control Concept Set: negative (ID: 1888110)
# - Study Period: 2013-04-01 to 2020-03-31
# - Propensity Score Adjustment: Matching with maxRatio=2, caliper=0.2
# - Outcome Model: Cox proportional hazards, stratified
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Define the base URL for the OHDSI WebAPI to retrieve cohort definitions
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export cohort definitions from ATLAS using the specified cohort IDs
# These cohorts define the target, comparator, and outcome populations
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to use sequential IDs for internal processing
# This ensures consistent referencing throughout the analysis
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative Control Outcomes ----------------------------------------------------
# Retrieve negative control concepts from the specified concept set
# Negative controls are used to detect potential systematic bias in the study
# They are outcomes with no expected causal relationship to the exposure
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # negative
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
  # Target/comparator cohort IDs start with 1, 2, 3...
  # Negative controls get IDs 101, 102, 103...
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Validate that there are no duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create Data Frames for Analysis Configuration --------------------------------

# Outcomes of Interest ---------------------------------------------------------
# Define the outcome cohorts for the comparative effectiveness analysis
# cleanWindow: minimum days required between outcome occurrences (365 days)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator Cohorts ------------------------------------------------
# Define the target-comparator pairs for the CohortMethod analysis
# Target: target1 (patients receiving SGLT2 inhibitors)
# Comparator: comparator1 (patients receiving metformin)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Excluded Covariate Concepts --------------------------------------------------
# For the CohortMethod large-scale propensity score (LSPS), we need to exclude
# the drugs of interest to prevent confounding by indication
# Note: No specific concepts were provided in the specifications for exclusion
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# CohortGeneratorModule --------------------------------------------------------
# This module generates the cohorts defined above in the target database
# It creates both the main study cohorts and negative control outcome cohorts
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource specifications for negative control outcomes
# occurrenceType = "first": only the first occurrence of each negative control is used
# detectOnDescendants = TRUE: include descendant concepts in the detection
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications with statistics generation enabled
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# This module runs comprehensive diagnostics on the generated cohorts
# to assess cohort quality and characteristics
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE,      # Statistics on inclusion rule impact
  runIncludedSourceConcepts = TRUE,   # Source concepts included in cohort
  runOrphanConcepts = TRUE,           # Concepts that may be missing from definition
  runTimeSeries = FALSE,              # Time series of cohort entry
  runVisitContext = TRUE,             # Visit context at cohort entry
  runBreakdownIndexEvents = TRUE,     # Breakdown of index events
  runIncidenceRate = TRUE,            # Incidence rate calculations
  runCohortRelationship = TRUE,       # Relationships between cohorts
  runTemporalCohortCharacterization = TRUE,  # Temporal characterization
  minCharacterizationMean = 0.01      # Minimum mean for characterization features
)

# CohortMethodModule -----------------------------------------------------------
# This module performs the comparative cohort analysis using propensity score methods

# Study Period Configuration ---------------------------------------------------
# Define the study observation period
# studyStartDate: 2013-04-01 (YYYYMMDD format)
# studyEndDate: 2020-03-31 (YYYYMMDD format)
studyPeriods <- tibble(
  studyStartDate = c("20130401"),
  studyEndDate = c("20200331")
)

# Time-at-Risk Configuration ---------------------------------------------------
# Define the time-at-risk windows for outcome assessment
# Based on specifications:
# - riskWindowStart: 1 day after cohort start
# - riskWindowEnd: 0 days relative to cohort end (i.e., at cohort end)
# - minDaysAtRisk: 1 day minimum follow-up required
timeAtRisks <- tibble(
  label = c("On Treatment"),
  riskWindowStart = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(0),
  endAnchor = c("cohort end")
)

# Propensity Score Settings - Match on PS --------------------------------------
# Configure propensity score matching parameters
# maxRatio: 2 (up to 2 comparators matched per target)
# caliper: 0.2 (maximum allowed difference in propensity scores)
# caliperScale: "standardized logit" (caliper applied on standardized logit scale)
matchOnPsArgsList <- tibble(
  label = c("PS Matching 1:2"),
  maxRatio = c(2),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# Propensity Score Settings - Stratify by PS -----------------------------------
# No stratification settings specified in the analysis specifications
# stratifyByPsArgs is set to NULL in the specifications
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata = c(),
  baseSelection = c()
)

# Build PS Configuration List --------------------------------------------------
# Combine all propensity score adjustment methods into a single configuration list
# Each entry contains: method type, label, and parameters
psConfigList <- list()

# Process matching configurations if they exist
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    # Append a new element at the end of psConfigList
    psConfigList[[length(psConfigList) + 1]] <- list(
      # Identify the PS adjustment method for this config
      method = "match",
      # Human-readable label to carry through into descriptions
      label = matchOnPsArgsList$label[i],
      # Parameter bundle passed to createMatchOnPsArgs later
      params = list(
        maxRatio = matchOnPsArgsList$maxRatio[i],
        caliper = matchOnPsArgsList$caliper[i],
        caliperScale = matchOnPsArgsList$caliperScale[i]
      )
    )
  }
}

# Process stratification configurations if they exist
if (exists("stratifyByPsArgsList") && nrow(stratifyByPsArgsList) > 0) {
  for (i in seq_len(nrow(stratifyByPsArgsList))) {
    # Append a new element at the end of psConfigList
    psConfigList[[length(psConfigList) + 1]] <- list(
      # Identify the PS adjustment method for this config
      method = "stratify",
      # Human-readable label to carry through into descriptions
      label = stratifyByPsArgsList$label[i],
      # Parameter bundle passed to createStratifyByPsArgs later
      params = list(
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}

# Build Analysis List ----------------------------------------------------------
# Iterate through all combinations of study periods, time-at-risk windows,
# and propensity score configurations to create comprehensive analysis settings
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]
  
  for (t in seq_len(nrow(timeAtRisks))) {
    
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure propensity score adjustment method based on configuration
      if (psCfg$method == "match") {
        # PS Matching Configuration
        # Creates matched cohorts based on propensity scores
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,        # Maximum comparators per target
          caliper = psCfg$params$caliper,          # Maximum PS difference allowed
          caliperScale = psCfg$params$caliperScale, # Scale for caliper calculation
          allowReverseMatch = FALSE,               # Do not allow reverse matching
          stratificationColumns = c()              # No additional stratification columns
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # PS Stratification Configuration
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }
      
      # Covariate Settings -------------------------------------------------------
      # Use default covariate settings from FeatureExtraction
      # addDescendantsToExclude: Include descendant concepts when excluding covariates
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )
      
      # Outcome List Configuration -----------------------------------------------
      # Create outcome objects for both outcomes of interest and negative controls
      outcomeList <- append(
        # Outcomes of interest (from oList)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,              # This is a primary outcome
            trueEffectSize = NA,                   # Unknown true effect (to be estimated)
            priorOutcomeLookback = 99999           # Look back period for prior outcomes
          )
        }),
        # Negative control outcomes
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,             # Not a primary outcome
            trueEffectSize = 1                     # Expected null effect (HR = 1)
          )
        })
      )
      
      # Target-Comparator-Outcomes Configuration ---------------------------------
      # Link target and comparator cohorts with outcomes
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude drug concepts from covariates to prevent confounding
          excludedCovariateConceptIds = c(
            excludedCovariateConcepts$conceptId
          )
        )
      }
      
      # Get Database Cohort Method Data Arguments --------------------------------
      # Configure how to extract data from the database
      # restrictToCommonPeriod: TRUE - restrict to period where both cohorts observed
      # maxCohortSize: 0 - no limit on cohort size
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )
      
      # Create Propensity Score Arguments ----------------------------------------
      # Configure the propensity score model fitting
      # Uses LASSO regularization with cross-validation
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,          # Max subjects for PS model fitting
        errorOnHighCorrelation = TRUE,             # Error if high correlation detected
        stopOnError = FALSE,                       # Continue even if errors occur
        estimator = "att",                         # Average treatment effect on treated
        # Prior specification for regularization (Laplace/LASSO)
        prior = Cyclops::createPrior(
          priorType = "laplace",                   # LASSO regularization
          exclude = c(0),                          # Exclude intercept from regularization
          useCrossValidation = TRUE                # Use CV to select regularization strength
        ),
        # Control parameters for optimization
        control = Cyclops::createControl(
          noiseLevel = "silent",                   # Suppress optimization output
          cvType = "auto",                         # Automatic CV type selection
          seed = 1,                                # Random seed for reproducibility
          resetCoefficients = TRUE,                # Reset coefficients between CV folds
          tolerance = 2e-07,                       # Convergence tolerance
          cvRepetitions = 10,                      # Number of CV repetitions
          startingVariance = 0.01,                 # Starting variance for coefficients
          fold = 10                                # Number of CV folds
        )
      )
      
      # Covariate Balance Arguments ----------------------------------------------
      # Configure covariate balance computation for diagnostics
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL                     # Include all covariates
      )
      
      # Compute balance for Table 1 specifications
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )
      
      # Fit Outcome Model Arguments ----------------------------------------------
      # Configure the outcome model (Cox proportional hazards)
      # modelType: "cox" - Cox proportional hazards model
      # stratified: TRUE - stratify by matched sets
      # useCovariates: FALSE - do not include additional covariates
      # inversePtWeighting: FALSE - do not use inverse probability weighting
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = TRUE,
        useCovariates = FALSE,
        inversePtWeighting = FALSE,
        # Prior for outcome model (if regularization needed)
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE
        ),
        # Control parameters for outcome model optimization
        control = Cyclops::createControl(
          cvType = "auto",
          seed = 1,
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          tolerance = 2e-07,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          fold = 10
        )
      )
      
      # Create Study Population Arguments ----------------------------------------
      # Configure the study population definition
      # Based on specifications:
      # - restrictToCommonPeriod: TRUE
      # - firstExposureOnly: FALSE (include all exposures)
      # - washoutPeriod: 0 (no washout required)
      # - removeDuplicateSubjects: "keep all"
      # - censorAtNewRiskWindow: FALSE
      # - removeSubjectsWithPriorOutcome: TRUE
      # - priorOutcomeLookBack: 99999 days
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE,
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
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )
      
      # Create CohortMethod Analysis ---------------------------------------------
      # Combine all settings into a single analysis specification
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

# Create CohortMethod Module Specifications ------------------------------------
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,                        # Include all analyses
  refitPsForEveryOutcome = FALSE,                  # Use same PS for all outcomes
  refitPsForEveryStudyPopulation = FALSE,          # Use same PS for all populations
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the Analysis Specifications -------------------------------------------
# Combine all module specifications into a single analysis specification object
# This object contains all settings needed to run the complete study
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Add shared resources (cohort definitions and negative controls)
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  # Add module specifications
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save Analysis Specifications to JSON -----------------------------------------
# Export the complete analysis specifications to a JSON file
# This file can be shared and used to execute the study on different databases
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "sglt2imetformin", "sglt2imetforminAnalysisSpecification.json")
)