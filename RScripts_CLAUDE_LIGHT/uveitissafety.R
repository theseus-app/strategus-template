################################################################################
# Uveitis Safety Study - CohortMethod Analysis Specification
# 
# This script creates a Strategus analysis specification for comparing safety
# outcomes between target and comparator cohorts using propensity score matching
# and Cox proportional hazards modeling.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from ATLAS
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions for:
# - Target cohort (ID: 1794126)
# - Comparator cohort (ID: 1794132)
# - Outcome cohort (ID: 1794131)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to sequential IDs for internal processing
# Target = 1, Comparator = 2, Outcome = 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve negative control concept set (ID: 1888110) to generate negative control outcome cohorts
# These are used to validate the propensity score model and assess residual confounding
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
  # Assign negative control cohort IDs starting at 101 (to avoid conflicts with main cohorts 1, 2, 3)
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Verify no duplicate cohort IDs between main cohorts and negative controls
if (any(cohortDefinitionSet$cohortId %in% negativeControlOutcomeCohortSet$cohortId)) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to organize cohorts for analysis -------------------------

# Outcomes: Extract the outcome cohort (ID = 3)
# This is the primary outcome of interest in the safety analysis
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Standard lookback window for prior outcome exclusion

# Target and Comparator for the CohortMethod analysis
# Maps cohort IDs and names for comparison
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# CohortGeneratorModule --------------------------------------------------------
# This module generates the cohorts in the database

cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for main cohorts
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Create shared resource specifications for negative control outcome cohorts
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",  # Use first occurrence of each negative control outcome
  detectOnDescendants = TRUE  # Include descendant concepts in concept set
)

# Create module specifications for cohort generation
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE  # Generate inclusion/exclusion statistics
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# This module generates diagnostics to assess cohort quality and completeness

cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE,        # Statistics on inclusion/exclusion criteria
  runIncludedSourceConcepts = TRUE,     # Concepts contributing to cohort inclusion
  runOrphanConcepts = TRUE,             # Concepts matching criteria but not in data
  runTimeSeries = FALSE,                # Temporal distribution of events
  runVisitContext = TRUE,               # Healthcare visit patterns
  runBreakdownIndexEvents = TRUE,       # Breakdown of index events
  runIncidenceRate = TRUE,              # Incidence rates in population
  runCohortRelationship = TRUE,         # Relationships between cohorts
  runTemporalCohortCharacterization = TRUE,  # Baseline characteristics over time
  minCharacterizationMean = 0.01        # Minimum mean prevalence for reporting
)

# CohortMethodModule -----------------------------------------------------------
# This module performs the comparative effectiveness analysis

# Study periods: Empty values indicate no time restrictions on study window
# User should populate studyStartDate and studyEndDate if restricting to specific dates (YYYYMMDD format)
studyPeriods <- tibble(
  studyStartDate = as.character(NA),  # No study start date restriction
  studyEndDate = as.character(NA)     # No study end date restriction
)

# Time-at-risk definitions for the analysis
# Defines the window during which outcomes are monitored relative to exposure
timeAtRisks <- tibble(
  label = "1 to end of cohort",                    # Human-readable description
  riskWindowStart = 1,                             # Start 1 day after cohort start
  startAnchor = "cohort start",                    # Anchor to exposure start date
  riskWindowEnd = 0,                               # End at cohort end date
  endAnchor = "cohort end",                        # Anchor to exposure end date
  minDaysAtRisk = 1                                # Minimum 1 day of follow-up required
)

# Propensity Score settings - match on PS
# Configuration for 1:many matching on propensity score
matchOnPsArgsList <- tibble(
  label = "Match on PS, caliper=0.2 (std logit)",  # Description for analysis
  maxRatio = 10,                                    # Maximum 10 comparators per target
  caliper = 0.2,                                    # Caliper of 0.2 standardized logit units
  caliperScale = "standardized logit"               # Apply caliper on logit scale
)

# Propensity Score settings - stratify by PS
# Initialize empty; not used in this analysis but structure shown for reference
stratifyByPsArgsList <- tibble(
  label = character(),
  numberOfStrata = integer(),
  baseSelection = character()
)

# Build propensity score configuration list
# This list structures all PS adjustment methods to be evaluated
psConfigList <- list()

# Convert match on PS configuration to list format
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

# Convert stratify by PS configuration to list format (if applicable)
if (nrow(stratifyByPsArgsList) > 0) {
  for (i in seq_len(nrow(stratifyByPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "stratify",
      label = stratifyByPsArgsList$label[i],
      params = list(
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}

# Iterate through all analysis setting combinations
# Creates analysis specifications for each combination of study period, time-at-risk, and PS method
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure propensity score adjustment method
      if (psCfg$method == "match") {
        # Matching on propensity score
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # Stratification on propensity score
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings: Use default covariates with concept descendants excluded
      # Based on covariateSelection specification with empty concept lists
      # (all covariates included by default, target/comparator drugs excluded later)
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build outcome list with primary outcome and negative controls
      outcomeList <- append(
        # Primary outcomes of interest (true effect sizes unknown, prior lookback = 99999 days)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        # Negative control outcomes (assumed null effect, trueEffectSize = 1, no prior lookback)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Create target-comparator-outcomes combinations
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = c()  # No additional exclusions; empty covariate lists in specification
        )
      }

      # Settings for retrieving cohort method data
      # Restricts to common period: Both target and comparator must have observations
      # maxCohortSize = 0 means no limit
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,    # From createStudyPopArgs specification
        studyStartDate = studyStartDate,  # Optional time window restriction
        studyEndDate = studyEndDate,      # Optional time window restriction
        maxCohortSize = 0,                # From getDbCohortMethodDataArgs specification
        covariateSettings = covariateSettings
      )

      # Settings for creating propensity score model
      # Uses LASSO regression with cross-validation for regularization
      # Laplace prior specified in propensityScoreAdjustment.createPsArgs
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # Limit to 250k for computational efficiency
        errorOnHighCorrelation = TRUE,     # Stop if correlation between variables is too high
        stopOnError = FALSE,               # Continue if PS model fails
        estimator = "att",                 # Estimate average treatment effect on treated
        prior = Cyclops::createPrior(
          priorType = "laplace",           # From propensityScoreAdjustment.createPsArgs
          exclude = c(0),
          useCrossValidation = TRUE        # From propensityScoreAdjustment.createPsArgs
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",           # From propensityScoreAdjustment.createPsArgs.control
          cvType = "auto",                 # Automatic cross-validation type selection
          seed = 1,
          resetCoefficients = TRUE,        # From propensityScoreAdjustment.createPsArgs.control
          tolerance = 2e-07,               # From propensityScoreAdjustment.createPsArgs.control
          cvRepetitions = 10,              # From propensityScoreAdjustment.createPsArgs.control
          startingVariance = 0.01          # From propensityScoreAdjustment.createPsArgs.control
        )
      )

      # Settings for computing covariate balance on matched/stratified population
      # Used for shared covariate balance assessment (all covariates)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL  # Include all covariates
      )

      # Settings for computing covariate balance using Table 1 specifications
      # Used for publication-ready covariate balance table
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()  # Standard Table 1 covariates
      )

      # Settings for outcome model fitting
      # Cox proportional hazards model stratified by matched pairs
      # No additional covariates in model (balance achieved through PS adjustment)
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",                 # From fitOutcomeModelArgs specification
        stratified = TRUE,                 # Stratified by matched pairs
        useCovariates = FALSE,             # From fitOutcomeModelArgs specification
        inversePtWeighting = FALSE,        # From fitOutcomeModelArgs specification
        prior = Cyclops::createPrior(
          priorType = "laplace",           # From fitOutcomeModelArgs specification
          useCrossValidation = TRUE        # From fitOutcomeModelArgs specification
        ),
        control = Cyclops::createControl(
          cvType = "auto",                 # From fitOutcomeModelArgs.control
          seed = 1,
          resetCoefficients = TRUE,        # From fitOutcomeModelArgs.control
          startingVariance = 0.01,         # From fitOutcomeModelArgs.control
          tolerance = 2e-07,               # From fitOutcomeModelArgs.control
          cvRepetitions = 10,              # From fitOutcomeModelArgs.control
          noiseLevel = "quiet"             # From fitOutcomeModelArgs.control
        )
      )

      # Settings for creating study population from full cohort data
      # Applies inclusion/exclusion criteria and defines time-at-risk
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE,                        # From createStudyPopArgs specification
        firstExposureOnly = TRUE,                             # From createStudyPopArgs specification
        washoutPeriod = 365,                                  # From createStudyPopArgs specification (days)
        removeDuplicateSubjects = "keep all",                 # From createStudyPopArgs specification
        censorAtNewRiskWindow = TRUE,                         # From createStudyPopArgs specification
        removeSubjectsWithPriorOutcome = TRUE,                # From createStudyPopArgs specification
        priorOutcomeLookback = 99999,                         # From createStudyPopArgs specification (days)
        riskWindowStart = timeAtRisks$riskWindowStart[t],     # From timeAtRisks specification
        startAnchor = timeAtRisks$startAnchor[t],             # From timeAtRisks specification
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],         # From timeAtRisks specification
        endAnchor = timeAtRisks$endAnchor[t],                 # From timeAtRisks specification
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t]          # From timeAtRisks specification
      )

      # Append this analysis configuration to the list
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Uveiits Safety Study; Study: %s-%s; TAR: %s; PS: %s",
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

# Create CohortMethod module specifications
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the analysis specifications -------------------------------------------
# Combines all module specifications (cohort generation, diagnostics, and analysis)

analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specification to a JSON file
# This file can be used to execute the analysis via Strategus
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "uveitissafety", "uveitissafetyAnalysisSpecification.json")
)