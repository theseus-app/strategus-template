################################################################################
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# further details.
# ##############################################################################
library(dplyr)
library(Strategus)
library(ROhdsiWebApi)
library(CohortGenerator)
library(CohortDiagnostics)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
# Base URL for the WebAPI to retrieve cohort definitions and concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions from Analysis Specifications:
# targetCohort: id 1794126, name "target1"
# comparatorCohort: id 1794132, name "comparator1"
# outcomeCohort: id 1794131, name "outcome1"
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: "target1"
    1794132, # Comparator: "comparator1"
    1794131  # Outcome: "outcome1"
  ),
  generateStats = TRUE
)

# Re-number cohorts to a simpler ID scheme for internal analysis.
# This maps the original WebAPI cohort IDs to a consistent set of small integers.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Renumber target cohort
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Renumber comparator cohort
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Renumber outcome cohort

# Negative control outcomes from Analysis Specifications:
# negativeControlConceptSet: id 1888110, name "negative"
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Concept set ID for negative controls
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
  mutate(cohortId = row_number() + 100) %>% # Assign cohort IDs starting from 101 to avoid collision with T/C/O cohorts
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs between study cohorts and negative controls.
# This ensures unique identifiers for all cohorts used in the study.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Filter for the re-numbered outcome cohort (ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window for outcomes

# Target and Comparator for the CohortMethod analysis
# Uses the re-numbered target (ID 1) and comparator (ID 2) cohorts.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# For the CohortMethod LSPS we'll need to exclude specific concepts.
# From Analysis Specifications: covariateSelection.conceptsToExclude is empty.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all
# From Analysis Specifications: covariateSelection.conceptsToInclude is empty.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# Module to generate cohorts specified in the cohortDefinitionSet.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Detect only the first occurrence of the negative control outcome
  detectOnDescendants = TRUE # Detect outcomes on descendant concepts as well
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Generate cohort statistics during cohort generation
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# Module to run various diagnostics on the generated cohorts.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId), # Run diagnostics on all study and negative control cohorts
  runInclusionStatistics = TRUE, # Run inclusion rule statistics
  runIncludedSourceConcepts = TRUE, # Run included source concept analysis
  runOrphanConcepts = TRUE, # Run orphan concept analysis
  runTimeSeries = FALSE, # Do not run time series analysis (as per template default)
  runVisitContext = TRUE, # Run visit context analysis
  runBreakdownIndexEvents = TRUE, # Run breakdown index events analysis
  runIncidenceRate = TRUE, # Run incidence rate analysis
  runCohortRelationship = TRUE, # Run cohort relationship analysis
  runTemporalCohortCharacterization = TRUE, # Run temporal cohort characterization
  minCharacterizationMean = 0.01 # Minimum mean for characterization covariates
)

# CohortMethodModule -----------------------------------------------------------

# Study Periods for getDbCohortMethodDataArgs
# From Analysis Specifications: getDbCohortMethodDataArgs.studyPeriods -> studyStartDate: null, studyEndDate: null.
# This means no restriction on study start/end dates for data extraction.
# We create a tibble with a single row where dates are NA (interpreted as NULL by CohortMethod).
studyPeriods <- tibble(
  studyStartDate = list(NULL), # YYYYMMDD or NULL for no restriction
  studyEndDate   = list(NULL)  # YYYYMMDD or NULL for no restriction
)

# Time-at-risks (TARs) for the outcomes of interest in your study
# From Analysis Specifications: createStudyPopArgs.timeAtRisks
timeAtRisks <- tibble(
  label = c("Default TAR"), # Label for this time-at-risk window
  riskWindowStart = c(1), # Start of risk window relative to anchor
  startAnchor = c("cohort start"), # Anchor point for start of risk window
  riskWindowEnd = c(0), # End of risk window relative to anchor
  endAnchor = c("cohort end") # Anchor point for end of risk window
)

# Propensity Score settings - match on PS
# From Analysis Specifications: propensityScoreAdjustment.psSettings.matchOnPsArgs
matchOnPsArgsList <- tibble(
  label = c("Match on PS (MaxRatio 10, Caliper 0.2 Std. Logit)"), # Descriptive label
  maxRatio = c(10), # Maximum number of comparators per target
  caliper = c(0.2), # Caliper for propensity score matching
  caliperScale = c("standardized logit") # Scale of the caliper
)

# Propensity Score settings - stratify by PS
# From Analysis Specifications: propensityScoreAdjustment.psSettings.stratifyByPsArgs is null.
# Therefore, this list remains empty.
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata = numeric(0),
  baseSelection = character(0)
)

# Build a single PS configuration list (each entry has: method, label, params)
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


# Iterate through all analysis setting combinations to create CohortMethod analyses
cmAnalysisList <- list()
analysisId <- 1

# Loop over defined study periods (in this case, only one entry for no restriction)
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[[s]] # Extract study start date (can be NULL)
  studyEndDate <- studyPeriods$studyEndDate[[s]]   # Extract study end date (can be NULL)

  # Loop over defined time-at-risks (TARs)
  for (t in seq_len(nrow(timeAtRisks))) {

    # Loop over defined propensity score (PS) adjustment configurations
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment arguments based on the method
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

      # Covariate settings for feature extraction.
      # From Analysis Specifications: covariateSelection.conceptsToInclude/Exclude are empty.
      # Using default settings, addDescendantsToExclude=TRUE is a common practice.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # List of outcomes for the analysis: study outcome and negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # For true effect size, NA when not a simulated outcome
            priorOutcomeLookback = 99999 # Lookback for prior outcomes
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE, # Negative controls are not outcomes of interest
            trueEffectSize = 1 # True effect size for negative controls (assumed 1 for null hypothesis)
          )
        })
      )
      
      # Define target-comparator-outcomes list for the CohortMethod analysis.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude covariate concepts. From specs, 'excludedCovariateConcepts' is empty.
          # The template had placeholder concept IDs which are not available in specs.
          excludedCovariateConceptIds = c(excludedCovariateConcepts$conceptId) 
        )
      }

      # Arguments for getting data from the database.
      # From Analysis Specifications: getDbCohortMethodDataArgs.maxCohortSize=0, studyPeriods.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Restrict analysis to periods common to all cohorts
        studyStartDate = studyStartDate, # Study start date (NULL for no restriction)
        studyEndDate = studyEndDate, # Study end date (NULL for no restriction)
        maxCohortSize = 0, # Maximum size for cohorts (0 means no restriction)
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # From Analysis Specifications: propensityScoreAdjustment.createPsArgs
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # Max cohort size for PS model fitting
        errorOnHighCorrelation = TRUE, # Throw error if covariates are highly correlated
        stopOnError = FALSE, # Set to FALSE to allow Strategus to complete all CM operations even if PS fails
        estimator = "att", # Estimator for propensity score
        prior = Cyclops::createPrior( # Prior distribution for regularization
          priorType = "laplace", 
          exclude = c(0), # Exclude intercept from regularization
          useCrossValidation = TRUE # Use cross-validation to tune regularization
        ),
        control = Cyclops::createControl( # Control parameters for Cyclops optimizer
          noiseLevel = "silent", # From specs: "silent"
          cvType = "auto", # From specs: "auto"
          seed = 1, # Random seed for reproducibility
          resetCoefficients = TRUE, # Reset coefficients at each cross-validation fold
          tolerance = 2e-07, # From specs: 2e-7
          cvRepetitions = 10, # From specs: 10
          startingVariance = 0.01 # From specs: 0.01
        )
      )

      # Arguments for computing shared covariate balance (e.g., for balancing diagnostics).
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL # No specific filter, compute for all covariates
      )
      
      # Arguments for computing covariate balance, typically for a table 1.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Use default Table 1 specifications
      )

      # Arguments for fitting the outcome model.
      # From Analysis Specifications: fitOutcomeModelArgs
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From specs: "cox"
        stratified = TRUE, # From specs: true (stratified by PS or matching strata)
        useCovariates = FALSE, # From specs: false (no covariates in outcome model after PS adjustment)
        inversePtWeighting = FALSE, # From specs: false (no inverse probability of treatment weighting)
        prior = Cyclops::createPrior( # Prior distribution for regularization
          priorType = "laplace", # From specs: "laplace"
          useCrossValidation = TRUE # From specs: true
        ),
        control = Cyclops::createControl( # Control parameters for Cyclops optimizer
          cvType = "auto", # From specs: "auto"
          seed = 1, # Random seed for reproducibility
          resetCoefficients = TRUE, # From specs: true
          startingVariance = 0.01, # From specs: 0.01
          tolerance = 2e-07, # From specs: 2e-7
          cvRepetitions = 10, # From specs: 10
          noiseLevel = "quiet" # From specs: "quiet"
        )
      )
      
      # Arguments for creating the study population.
      # From Analysis Specifications: createStudyPopArgs
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From specs: false
        firstExposureOnly = FALSE, # From specs: false
        washoutPeriod = 365, # From specs: 365
        removeDuplicateSubjects = "remove all", # From specs: "remove all"
        censorAtNewRiskWindow = FALSE, # From specs: false
        removeSubjectsWithPriorOutcome = TRUE, # From specs: true
        priorOutcomeLookback = 365, # From specs: 365
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From specs: 1
        startAnchor = timeAtRisks$startAnchor[t], # From specs: "cohort start"
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From specs: 0
        endAnchor = timeAtRisks$endAnchor[t], # From specs: "cohort end"
        minDaysAtRisk = 1, # From specs: 1
        maxDaysAtRisk = 99999 # Not in specs, using template default
      )


      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          # Convert NULL dates to "No Restriction" for description
          ifelse(is.null(studyStartDate), "No Restriction", studyStartDate),
          ifelse(is.null(studyEndDate), "No Restriction", studyEndDate),
          timeAtRisks$label[t],
          psCfg$label
        ),
        getDbCohortMethodDataArgs = getDbCohortMethodDataArgs,
        createStudyPopArgs = createStudyPopArgs,
        createPsArgs = createPsArgs,
        matchOnPsArgs = matchOnPsArgs, # Will be NULL if stratify method is used
        stratifyByPsArgs = stratifyByPsArgs, # Will be NULL if match method is used
        computeSharedCovariateBalanceArgs = computeSharedCovariateBalanceArgs,
        computeCovariateBalanceArgs = computeCovariateBalanceArgs,
        fitOutcomeModelArgs = fitOutcomeModelArgs
      )
      analysisId <- analysisId + 1
    }
  }
}

# CohortMethodModule Specifications
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # No analyses to exclude
  refitPsForEveryOutcome = FALSE, # Do not refit PS for every outcome
  refitPsForEveryStudyPopulation = FALSE, # Do not refit PS for every study population
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Use default CM diagnostic thresholds
)

# Create the analysis specifications ------------------------------------------
# From Analysis Specifications: name = "ceeamos"
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path uses the study name from Analysis Specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "ceeamos", "ceeamosAnalysisSpecification.json")
)