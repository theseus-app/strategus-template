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
library(ROhdsiWebApi) # Required for ROhdsiWebApi functions
library(CohortMethod) # Required for CohortMethod functions
library(FeatureExtraction) # Required for FeatureExtraction functions
library(Cyclops) # Required for Cyclops functions

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
# Base URL for the WebAPI instance where cohort definitions are stored.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions from WebAPI based on their IDs provided in Analysis Specifications.
# These IDs are then re-numbered to internal study IDs (1, 2, 3) for consistency within the analysis.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1 (from Analysis Specifications: cohortDefinitions.targetCohort.id)
    1794132, # Comparator: comparator1 (from Analysis Specifications: cohortDefinitions.comparatorCohort.id)
    1794131  # Outcome: outcome1 (from Analysis Specifications: cohortDefinitions.outcomeCohort[0].id)
  ),
  generateStats = TRUE
)

# Re-number cohorts to generic IDs (1 for target, 2 for comparator, 3 for outcome).
# This simplifies referencing them within the Strategus analysis.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Target cohort ID
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Comparator cohort ID
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Outcome cohort ID

# Update cohort names to match the re-numbered IDs for clarity.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1,]$cohortName <- "target1"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 2,]$cohortName <- "comparator1"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 3,]$cohortName <- "outcome1"


# Negative control outcomes
# Retrieve the concept set definition for negative controls from WebAPI.
# The conceptSetId is from Analysis Specifications: negativeControlConceptSet.id.
# These concepts will be resolved to individual concepts and then used to generate
# negative control outcome cohorts.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # ID for the negative control concept set (from Analysis Specifications: negativeControlConceptSet.id)
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
  mutate(cohortId = row_number() + 100) %>% # Assign unique cohort IDs starting from 101 for negative controls
  select(cohortId, cohortName, outcomeConceptId)


# Check for duplicate cohort IDs between study cohorts and negative controls.
# This is a critical check to ensure unique identifiers across all cohorts used in the study.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Filter for the primary outcome cohort (re-numbered to ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window for outcomes, not specified in analysis specifications

# Target and Comparator for the CohortMethod analysis.
# Use the re-numbered target (ID 1) and comparator (ID 2) cohorts.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# For the CohortMethod LSPS, we can exclude specific concepts from covariates.
# The Analysis Specifications provided an empty list for conceptsToExclude,
# so this will be an empty data frame.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0), # No concepts to exclude specified in Analysis Specifications: covariateSelection.conceptsToExclude
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all.
# The Analysis Specifications provided an empty list for conceptsToInclude,
# so this block remains commented out.
# includedCovariateConcepts <- data.frame(
#   conceptId = integer(0), # No concepts to include specified in Analysis Specifications: covariateSelection.conceptsToInclude
#   conceptName = character(0)
# )

# CohortGeneratorModule --------------------------------------------------------
# Initializes the CohortGeneratorModule settings creator.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Creates shared resource specifications for the main study cohorts.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Creates shared resource specifications for negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Detect the first occurrence of the negative control outcome (default)
  detectOnDescendants = TRUE # Include descendants of the negative control concepts (default)
)
# Creates module specifications for the CohortGenerator module.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Generate cohort statistics during cohort generation (default)
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# Initializes the CohortDiagnosticsModule settings creator.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Creates module specifications for the CohortDiagnostics module.
# Runs diagnostics for all defined study cohorts.
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # Run diagnostics for all defined study cohorts
  runInclusionStatistics = TRUE, # Default, not specified in analysis specifications
  runIncludedSourceConcepts = TRUE, # Default, not specified in analysis specifications
  runOrphanConcepts = TRUE, # Default, not specified in analysis specifications
  runTimeSeries = FALSE, # Default, not specified in analysis specifications
  runVisitContext = TRUE, # Default, not specified in analysis specifications
  runBreakdownIndexEvents = TRUE, # Default, not specified in analysis specifications
  runIncidenceRate = TRUE, # Default, not specified in analysis specifications
  runCohortRelationship = TRUE, # Default, not specified in analysis specifications
  runTemporalCohortCharacterization = TRUE, # Default, not specified in analysis specifications
  minCharacterizationMean = 0.01 # Default, not specified in analysis specifications
)

# CohortMethodModule -----------------------------------------------------------

# Study periods: Defines the start and end dates for the study.
# From Analysis Specifications: getDbCohortMethodDataArgs.studyPeriods
studyPeriods <- tibble(
  studyStartDate = c("20101019"), # YYYYMMDD (from Analysis Specifications: getDbCohortMethodDataArgs.studyPeriods[0].studyStartDate)
  studyEndDate   = c("20181231")  # YYYYMMDD (from Analysis Specifications: getDbCohortMethodDataArgs.studyPeriods[0].studyEndDate)
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# From Analysis Specifications: createStudyPopArgs.timeAtRisks
timeAtRisks <- tibble(
  label = c("Main TAR"), # A descriptive label for this time-at-risk window
  riskWindowStart  = c(1), # (from Analysis Specifications: createStudyPopArgs.timeAtRisks[0].riskWindowStart)
  startAnchor = c("cohort start"), # "cohort start" | "cohort end" (from Analysis Specifications: createStudyPopArgs.timeAtRisks[0].startAnchor)
  riskWindowEnd  = c(0), # (from Analysis Specifications: createStudyPopArgs.timeAtRisks[0].riskWindowEnd)
  endAnchor = c("cohort end") # "cohort start" | "cohort end" (from Analysis Specifications: createStudyPopArgs.timeAtRisks[0].endAnchor)
)

# Propensity Score settings - match on PS
# From Analysis Specifications: propensityScoreAdjustment.psSettings[0].matchOnPsArgs
matchOnPsArgsList <- tibble(
  label = c("1:1 PS Matching"), # A descriptive label for this PS matching strategy
  maxRatio  = c(1), # (from Analysis Specifications: propensityScoreAdjustment.psSettings[0].matchOnPsArgs.maxRatio)
  caliper = c(0.2), # (from Analysis Specifications: propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliper)
  caliperScale  = c("standardized logit") # "propensity score" | "standardized" | "standardized logit" (from Analysis Specifications: propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliperScale)
)

# Propensity Score settings - stratify by PS
# From Analysis Specifications: propensityScoreAdjustment.psSettings[0].stratifyByPsArgs (which is null).
# This will result in an empty tibble, correctly skipping stratification.
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0) # "all" | "target" | "comparator"
)

# Build a single PS configuration list (each entry has: method, label, params).
# This structure allows iterating through different PS adjustment methods (matching, stratification).
psConfigList <- list()

# If a data frame for "match on PS" exists and has rows, convert each row to a config.
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

# If a data frame for "stratify by PS" exists and has rows, convert each row to a config.
# This block will not execute as stratifyByPsArgsList is empty based on Analysis Specifications.
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


# Iterate through all analysis setting combinations (study periods, time-at-risks, PS methods).
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Configure PS adjustment arguments based on the current PS configuration.
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default, not specified in analysis specifications
          stratificationColumns = c() # Default, not specified in analysis specifications
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default, not specified in analysis specifications
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings for feature extraction.
      # The analysis specifications provided empty lists for conceptsToInclude/Exclude,
      # so we use default settings.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default, not specified in analysis specifications
      )
      # If there were specific concepts to exclude, they would be added here.
      # As per analysis specifications, excludedCovariateConcepts is empty.
      if (nrow(excludedCovariateConcepts) > 0) {
        covariateSettings$excludedCovariateConceptIds <- c(
          covariateSettings$excludedCovariateConceptIds,
          excludedCovariateConcepts$conceptId
        )
      }
      # If there were specific concepts to include, they would be used to create a custom covariateSettings.
      # As per analysis specifications, includedCovariateConcepts is empty, so we stick with default.
      # if (exists("includedCovariateConcepts") && nrow(includedCovariateConcepts) > 0) {
      #   covariateSettings <- FeatureExtraction::createCovariateSettings(
      #     # ... (detailed covariate settings) ...
      #     excludedCovariateConceptIds = excludedCovariateConcepts$conceptId,
      #     includedCovariateConceptIds = includedCovariateConcepts$conceptId,
      #     addDescendantsToExclude = TRUE,
      #     addDescendantsToInclude = TRUE,
      #     includedCovariateIds = c()
      #   )
      # }


      # Define the list of outcomes for the CohortMethod analysis.
      # This includes the primary outcome of interest and all negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE, # Mark as outcome of interest
            trueEffectSize = NA, # Not applicable for observational studies
            priorOutcomeLookback = 99999 # Look back for prior outcomes (default)
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE, # Mark as negative control
            trueEffectSize = 1 # Assumed true effect size of 1 for negative controls
          )
        })
      )

      # Define target-comparator-outcome combinations.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude concepts from covariates.
          # The template had placeholders for cmTcList$targetConceptId and cmTcList$comparatorConceptId,
          # but these are not provided in the analysis specifications.
          # We only include the general excludedCovariateConcepts, which is empty in this case.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Arguments for fetching data from the database for CohortMethod.
      # From Analysis Specifications: getDbCohortMethodDataArgs
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications: createStudyPopArgs.restrictToCommonPeriod (note: template default was TRUE, but spec says FALSE)
        studyStartDate = studyStartDate, # From current study period iteration
        studyEndDate = studyEndDate, # From current study period iteration
        maxCohortSize = 0, # From Analysis Specifications: getDbCohortMethodDataArgs.maxCohortSize (0 means no restriction)
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # From Analysis Specifications: propensityScoreAdjustment.createPsArgs
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail (default)
        estimator = "att", # Default, not specified in analysis specifications
        prior = Cyclops::createPrior( # prior = NULL if 'use regularization' == false
          priorType = "laplace", # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.prior.priorType
          exclude = c(0), # Default, not specified in analysis specifications
          useCrossValidation = TRUE # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # control = NULL if 'use regularization' == false
          noiseLevel = "silent", # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.noiseLevel
          cvType = "auto", # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.cvType
          seed = 1, # Default, not specified in analysis specifications
          resetCoefficients = TRUE, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.resetCoefficients
          tolerance = 2e-07, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.tolerance
          cvRepetitions = 10, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.cvRepetitions
          startingVariance = 0.01 # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.startingVariance
        )
      )

      # Arguments for computing shared covariate balance (e.g., for PS model diagnostics).
      # Not explicitly specified in analysis specifications, using template defaults.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default
        covariateFilter = NULL # Default
      )
      # Arguments for computing covariate balance for the final study population.
      # Not explicitly specified in analysis specifications, using template defaults.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default
      )

      # Arguments for fitting the outcome model.
      # From Analysis Specifications: fitOutcomeModelArgs
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From Analysis Specifications: fitOutcomeModelArgs.modelType
        stratified = FALSE, # From Analysis Specifications: fitOutcomeModelArgs.stratified
        useCovariates = FALSE, # From Analysis Specifications: fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE, # From Analysis Specifications: fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior( # prior = NULL if 'use regularization' == false
          priorType = "laplace", # From Analysis Specifications: fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE # From Analysis Specifications: fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # control = NULL if 'use regularization' == false
          cvType = "auto", # From Analysis Specifications: fitOutcomeModelArgs.control.cvType
          seed = 1, # Default, not specified in analysis specifications
          resetCoefficients = TRUE, # From Analysis Specifications: fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01, # From Analysis Specifications: fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07, # From Analysis Specifications: fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10, # From Analysis Specifications: fitOutcomeModelArgs.control.cvRepetitions
          noiseLevel = "quiet" # From Analysis Specifications: fitOutcomeModelArgs.control.noiseLevel
        )
      )

      # Arguments for creating the study population.
      # From Analysis Specifications: createStudyPopArgs
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications: createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = TRUE, # From Analysis Specifications: createStudyPopArgs.firstExposureOnly
        washoutPeriod = 365, # From Analysis Specifications: createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # From Analysis Specifications: createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # From Analysis Specifications: createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # From Analysis Specifications: createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # From Analysis Specifications: createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From current time-at-risk iteration
        startAnchor = timeAtRisks$startAnchor[t], # From current time-at-risk iteration
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From current time-at-risk iteration
        endAnchor = timeAtRisks$endAnchor[t], # From current time-at-risk iteration
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From Analysis Specifications: createStudyPopArgs.timeAtRisks[0].minDaysAtRisk
        maxDaysAtRisk = 99999 # Default, not specified in analysis specifications
      )


      # Append the settings to Analysis List.
      # Each entry in cmAnalysisList represents a unique CohortMethod analysis.
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

# Initializes the CohortMethodModule settings creator.
cmModuleSettingsCreator <- CohortMethodModule$new()
# Creates module specifications for the CohortMethod module.
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList, # List of all CohortMethod analyses to run
  targetComparatorOutcomesList = targetComparatorOutcomesList, # List of TCO combinations
  analysesToExclude = NULL, # No analyses to exclude (default)
  refitPsForEveryOutcome = FALSE, # Default, not specified in analysis specifications
  refitPsForEveryStudyPopulation = FALSE, # Default, not specified in analysis specifications
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default diagnostic thresholds
)

# Create the analysis specifications ------------------------------------------
# Initializes an empty Strategus analysis specifications object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Adds shared resources for cohort definitions.
  Strategus::addSharedResources(cohortDefinitionShared) |>
  # Adds shared resources for negative control outcomes.
  Strategus::addSharedResources(negativeControlsShared) |>
  # Adds the CohortGenerator module specifications.
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  # Adds the CohortDiagnostics module specifications.
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  # Adds the CohortMethod module specifications.
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Saves the complete analysis specifications to a JSON file.
# The file path is a placeholder and should be adjusted by the user.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "studyName", "studyNameAnalysisSpecification.json")
)