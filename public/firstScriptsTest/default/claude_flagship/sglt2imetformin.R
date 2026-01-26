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
# 
# Analysis includes:
# - Two study periods: 2013-04-01 to 2020-03-31 and 2013-04-01 to 2018-12-31
# - Two time-at-risk windows
# - Propensity score matching (1:1, caliper 0.2 on standardized logit scale)
# - Cox proportional hazards model
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
# Export cohort definitions from ATLAS using the specified cohort IDs
# - Target cohort (target1): 1794126
# - Comparator cohort (comparator1): 1794132
# - Outcome cohort (outcome1): 1794131
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
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1  # Target
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2  # Comparator
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3  # Outcome

# ------------------------------------------------------------------------------
# Negative Control Outcomes
# ------------------------------------------------------------------------------
# Retrieve negative control concepts from the concept set defined in ATLAS
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

# ==============================================================================
# Analysis Configuration Data Frames
# ==============================================================================

# ------------------------------------------------------------------------------
# Outcomes List
# ------------------------------------------------------------------------------
# Define the outcomes of interest for the study
# cleanWindow: lookback period (in days) for identifying prior outcomes
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%  # Filter to outcome cohort only
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

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
# Note: No specific concepts to exclude were provided in the analysis specifications
# (conceptsToExclude id is null). If needed, add concept IDs here to exclude
# specific covariates from the propensity score model.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# Note: No specific concepts to include were provided in the analysis specifications
# (conceptsToInclude id is null). The analysis will use default covariate settings.

# ==============================================================================
# CohortGeneratorModule Settings
# ==============================================================================
# The CohortGenerator module is responsible for creating cohorts in the CDM
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource specifications for negative control outcome cohorts
# - occurrenceType: "first" means only the first occurrence of each negative control is used
# - detectOnDescendants: TRUE means descendant concepts are also considered
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications for cohort generation
# generateStats: TRUE enables generation of inclusion rule statistics
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ==============================================================================
# CohortDiagnosticsModule Settings
# ==============================================================================
# The CohortDiagnostics module provides comprehensive diagnostics for cohorts
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE,           # Statistics on inclusion rule impact
  runIncludedSourceConcepts = TRUE,        # Source concepts included in cohort
  runOrphanConcepts = TRUE,                # Concepts that may be missing from definitions
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
# Define the study periods for the analysis
# Two study periods are specified:
# 1. 2013-04-01 to 2020-03-31 (full study period)
# 2. 2013-04-01 to 2018-12-31 (restricted study period)
studyPeriods <- tibble(
  studyStartDate = c("20130401", "20130401"),
  studyEndDate   = c("20200331", "20181231")
)

# ------------------------------------------------------------------------------
# Time-at-Risk Windows
# ------------------------------------------------------------------------------
# Define the time-at-risk (TAR) windows for outcome assessment
# Two TAR windows are specified:
# 1. From day 1 after cohort start to cohort end (on-treatment)
# 2. From day 1 after cohort start to day 99999 after cohort start (intent-to-treat)
timeAtRisks <- tibble(
  label = c("On-treatment", "Intent-to-treat"),
  riskWindowStart  = c(1, 1),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd  = c(0, 99999),
  endAnchor = c("cohort end", "cohort start"),
  minDaysAtRisk = c(1, 1)
)

# ------------------------------------------------------------------------------
# Propensity Score Settings - Match on PS
# ------------------------------------------------------------------------------
# Define propensity score matching parameters
# - maxRatio: 1 (1:1 matching)
# - caliper: 0.2 (maximum allowed difference in PS)
# - caliperScale: "standardized logit" (caliper applied on standardized logit scale)
matchOnPsArgsList <- tibble(
  label = c("1:1 matching, caliper 0.2"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# ------------------------------------------------------------------------------
# Propensity Score Settings - Stratify by PS
# ------------------------------------------------------------------------------
# No stratification settings specified in the analysis specifications
# (stratifyByPsArgs is null)
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0)
)

# ------------------------------------------------------------------------------
# Build PS Configuration List
# ------------------------------------------------------------------------------
# Combine matching and stratification settings into a single configuration list
psConfigList <- list()

# Add matching configurations if they exist
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

# Add stratification configurations if they exist
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
# Iterate Through All Analysis Setting Combinations
# ------------------------------------------------------------------------------
# Create analysis specifications for each combination of:
# - Study period
# - Time-at-risk window
# - Propensity score adjustment method
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure propensity score adjustment based on method type
      if (psCfg$method == "match") {
        # Propensity score matching settings
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # Propensity score stratification settings
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
      # Use default covariate settings with descendant exclusion enabled
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # ----------------------------------------------------------------------
      # Outcome List
      # ----------------------------------------------------------------------
      # Create outcome objects for both outcomes of interest and negative controls
      # priorOutcomeLookBack: 99999 days (as specified in createStudyPopArgs)
      outcomeList <- append(
        # Outcomes of interest
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        # Negative control outcomes (true effect size = 1, i.e., no effect)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # ----------------------------------------------------------------------
      # Target-Comparator-Outcomes List
      # ----------------------------------------------------------------------
      # Create target-comparator-outcomes specifications
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
      # - restrictToCommonPeriod: TRUE (restrict to period where both cohorts have data)
      # - maxCohortSize: 0 (no limit on cohort size)
      # - firstExposureOnly: FALSE (include all exposures)
      # - washoutPeriod: 0 (no washout period required)
      # - removeDuplicateSubjects: "keep all" (keep all subjects)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # ----------------------------------------------------------------------
      # createPsArgs
      # ----------------------------------------------------------------------
      # Settings for propensity score model creation
      # - maxCohortSizeForFitting: 250000 (maximum cohort size for PS model fitting)
      # - errorOnHighCorrelation: TRUE (error if high correlation detected)
      # - prior: Laplace prior with cross-validation
      # - control: Cyclops control settings with specified tolerance and CV parameters
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
      # - modelType: "cox" (Cox proportional hazards model)
      # - stratified: TRUE (stratified analysis, used with PS matching)
      # - useCovariates: FALSE (no additional covariates in outcome model)
      # - inversePtWeighting: FALSE (not using inverse probability weighting)
      # - prior: Laplace prior with cross-validation
      # - control: Cyclops control settings
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
      # - restrictToCommonPeriod: FALSE
      # - firstExposureOnly: FALSE (include all exposures)
      # - washoutPeriod: 0 (no washout period)
      # - removeDuplicateSubjects: "keep all"
      # - censorAtNewRiskWindow: FALSE
      # - removeSubjectsWithPriorOutcome: TRUE (exclude subjects with prior outcome)
      # - priorOutcomeLookBack: 99999 days
      # - Time-at-risk settings from the current iteration
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
# Save the analysis specifications to a JSON file for use with Strategus
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "sglt2imetformin", "sglt2imetforminAnalysisSpecification.json")
)