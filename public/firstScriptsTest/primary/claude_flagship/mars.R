################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates the analysis specifications for the "mars" study using
# the OHDSI Strategus package. It configures:
# - Cohort definitions (target, comparator, outcome)
# - Negative control outcomes
# - Cohort Method analysis settings including propensity score matching
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

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from ATLAS WebAPI
# The baseUrl points to the ATLAS instance where cohort definitions are stored
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions from ATLAS using their IDs
# - Target cohort (id: 1794126): target1
# - Comparator cohort (id: 1794132): comparator1
# - Outcome cohort (id: 1794131): outcome1
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to use simpler sequential IDs for internal processing
# This makes it easier to reference cohorts throughout the analysis
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Negative controls are used to detect residual confounding and systematic bias
# They are outcomes that are not expected to be causally related to the exposure
# Concept set ID 1888110 contains the negative control concepts (name: "negative")
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
  # target/comparator cohort ids start with 1, 2, 3... negativeControl -> 101, 102, 103...
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Validation check: ensure no duplicate cohort IDs exist between main cohorts
# and negative control cohorts
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------

# Outcomes: Define the outcome cohorts for the analysis
# cleanWindow = 365 means subjects with the outcome in the 365 days prior
# to index will be excluded (when removeSubjectsWithPriorOutcome = TRUE)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# This defines the comparison: target1 vs comparator1
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# For the CohortMethod LSPS (Large-Scale Propensity Score) we'll need to exclude
# the drugs of interest in this study from the covariate set
# Note: No specific concepts to exclude were provided in the specifications
# (conceptsToExclude id is null), so we create an empty data frame
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# Note: No specific concepts to include were provided in the specifications
# (conceptsToInclude id is null)
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# This module generates the cohorts in the CDM database
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource for negative control outcome cohorts
# occurrenceType = "first" means only the first occurrence of each outcome is considered
# detectOnDescendants = TRUE means descendant concepts are also included
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
# This module runs various diagnostics on the cohorts to assess their quality
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE,      # Statistics on inclusion rule impact
  runIncludedSourceConcepts = TRUE,   # Source concepts included in cohort
  runOrphanConcepts = TRUE,           # Concepts that might be missing
  runTimeSeries = FALSE,              # Time series of cohort counts
  runVisitContext = TRUE,             # Visit context of cohort entries
  runBreakdownIndexEvents = TRUE,     # Breakdown of index events
  runIncidenceRate = TRUE,            # Incidence rate calculations
  runCohortRelationship = TRUE,       # Relationships between cohorts
  runTemporalCohortCharacterization = TRUE,  # Temporal characterization
  minCharacterizationMean = 0.01      # Minimum mean for characterization features
)

# CohortMethodModule -----------------------------------------------------------
# This module performs the comparative cohort analysis using propensity scores

# Study period restriction
# The analysis is restricted to data between 2011-01-01 and 2013-12-31
studyPeriods <- tibble(
  studyStartDate = c("20110101"), # YYYYMMDD format
  studyEndDate   = c("20131231")  # YYYYMMDD format
)

# Time-at-risks (TARs) for the outcomes of interest in your study
# This defines when outcomes are counted relative to the exposure
# - riskWindowStart = 3: Start counting outcomes 3 days after cohort start
# - riskWindowEnd = 90: Stop counting outcomes 90 days after cohort start
# - minDaysAtRisk = 1: Subjects must have at least 1 day at risk to be included
timeAtRisks <- tibble(
  label = c("TAR 3-90 days"),
  riskWindowStart  = c(3),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(90),
  endAnchor = c("cohort start")
)

# Propensity Score settings - match on PS
# This configures 1:1 matching with a caliper of 0.2 on the standardized logit scale
# - maxRatio = 1: 1:1 matching (each target matched to at most 1 comparator)
# - caliper = 0.2: Maximum allowed difference in propensity scores
# - caliperScale = "standardized logit": Caliper applied on standardized logit of PS
matchOnPsArgsList <- tibble(
  label = c("1:1 matching, caliper 0.2"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# Propensity Score settings - stratify by PS
# Not used in this analysis (stratifyByPsArgs is null in specifications)
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0)
)

# Build a single PS configuration list (each entry has: method, label, params)
# This allows for flexible combination of matching and stratification approaches
psConfigList <- list()

# If a data frame for "match on PS" exists and has rows, convert each row to a config
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    # Append a new element at the end of psConfigList
    psConfigList[[length(psConfigList) + 1]] <- list(
      # Identify the PS adjustment method for this config
      method = "match",
      # Human-readable label to carry through into descriptions
      label  = matchOnPsArgsList$label[i],
      # Parameter bundle passed to createMatchOnPsArgs later
      params = list(
        maxRatio     = matchOnPsArgsList$maxRatio[i],
        caliper      = matchOnPsArgsList$caliper[i],
        caliperScale = matchOnPsArgsList$caliperScale[i]
      )
    )
  }
}

# If a data frame for "stratify by PS" exists and has rows, convert each row to a config
if (exists("stratifyByPsArgsList") && nrow(stratifyByPsArgsList) > 0) {
  for (i in seq_len(nrow(stratifyByPsArgsList))) {
    # Append a new element at the end of psConfigList
    psConfigList[[length(psConfigList) + 1]] <- list(
      # Identify the PS adjustment method for this config
      method = "stratify",
      # Human-readable label to carry through into descriptions
      label  = stratifyByPsArgsList$label[i],
      # Parameter bundle passed to createStratifyByPsArgs later
      params = list(
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection  = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}

# Iterate through all analysis setting combinations
# This creates all combinations of study periods, time-at-risks, and PS methods
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure propensity score adjustment method based on the config
      if (psCfg$method == "match") {
        # PS Matching configuration
        # allowReverseMatch = FALSE: Only match target to comparator, not vice versa
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # PS Stratification configuration
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings for propensity score model
      # Uses default covariates with descendants of excluded concepts also excluded
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome list combining outcomes of interest and negative controls
      outcomeList <- append(
        # Outcomes of interest (outcome1)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,      # This is a primary outcome
            trueEffectSize = NA,           # Unknown true effect (not a negative control)
            priorOutcomeLookback = 99999   # Look back period for prior outcomes
          )
        }),
        # Negative control outcomes (for bias detection)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,     # Not a primary outcome
            trueEffectSize = 1             # True effect is null (HR = 1)
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
          # Exclude treatment-related concepts from covariates to avoid confounding
          excludedCovariateConceptIds = c(
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # Settings for extracting cohort method data from the database
      # restrictToCommonPeriod = TRUE: Restrict to period where both cohorts have data
      # maxCohortSize = 0: No limit on cohort size (0 means unlimited)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Settings for creating the propensity score model
      # Uses LASSO regularization with cross-validation to prevent overfitting
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,    # Max subjects for PS model fitting
        errorOnHighCorrelation = TRUE,        # Error if covariates highly correlated
        stopOnError = FALSE,                  # Continue even if PS model fails
        estimator = "att",                    # Average treatment effect on treated
        # Regularization prior settings (Laplace/LASSO)
        prior = Cyclops::createPrior(
          priorType = "laplace",              # LASSO regularization
          exclude = c(0),                     # Don't regularize intercept
          useCrossValidation = TRUE           # Use CV to select regularization strength
        ),
        # Optimization control settings
        control = Cyclops::createControl(
          noiseLevel = "silent",              # Suppress optimization output
          cvType = "auto",                    # Automatic CV type selection
          seed = 1,                           # Random seed for reproducibility
          resetCoefficients = TRUE,           # Reset coefficients between CV folds
          tolerance = 2e-07,                  # Convergence tolerance
          cvRepetitions = 10,                 # Number of CV repetitions
          fold = 10,                          # Number of CV folds
          startingVariance = 0.01             # Starting variance for coefficients
        )
      )

      # Settings for computing covariate balance (shared across outcomes)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL                # Include all covariates
      )
      
      # Settings for computing covariate balance (per outcome, Table 1 style)
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Settings for fitting the outcome model (Cox proportional hazards)
      # stratified = FALSE: Not stratified (using matched cohorts)
      # useCovariates = FALSE: No additional covariate adjustment
      # inversePtWeighting = FALSE: Not using IPW (using matching instead)
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",                    # Cox proportional hazards model
        stratified = FALSE,                   # Not stratified by PS strata
        useCovariates = FALSE,                # No covariate adjustment in outcome model
        inversePtWeighting = FALSE,           # Not using inverse probability weighting
        # Regularization prior (if useCovariates = TRUE)
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE
        ),
        # Optimization control settings
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
      
      # Settings for creating the study population
      # These settings define inclusion/exclusion criteria and the risk window
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE,        # Restrict to common observation period
        firstExposureOnly = FALSE,            # Include all exposures, not just first
        washoutPeriod = 0,                    # No washout period required
        removeDuplicateSubjects = "keep all", # Keep all subjects (no deduplication)
        censorAtNewRiskWindow = FALSE,        # Don't censor at new exposure
        removeSubjectsWithPriorOutcome = TRUE, # Exclude subjects with prior outcome
        priorOutcomeLookback = 99999,         # Look back period for prior outcomes
        riskWindowStart = timeAtRisks$riskWindowStart[t],  # Start of risk window (3 days)
        startAnchor = timeAtRisks$startAnchor[t],          # Anchor for start (cohort start)
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],      # End of risk window (90 days)
        endAnchor = timeAtRisks$endAnchor[t],              # Anchor for end (cohort start)
        minDaysAtRisk = 1,                    # Minimum days at risk required
        maxDaysAtRisk = 99999                 # Maximum days at risk (no limit)
      )

      # Append the settings to Analysis List
      # Each analysis is a unique combination of study period, TAR, and PS method
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

# Create the CohortMethod module specifications
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,                   # No analyses excluded
  refitPsForEveryOutcome = FALSE,             # Use same PS for all outcomes
  refitPsForEveryStudyPopulation = FALSE,     # Use same PS for all study populations
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the analysis specifications ------------------------------------------
# Combine all modules into a single analysis specification
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file
# This file can be used to execute the study across multiple databases
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "mars", "marsAnalysisSpecification.json")
)