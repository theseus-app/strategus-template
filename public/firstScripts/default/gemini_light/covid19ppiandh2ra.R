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
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
# Base URL for the WebAPI instance to retrieve cohort definitions and concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions from WebAPI based on IDs specified in Analysis Specifications.
# These include Target, Comparator, and Outcome cohorts.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to a simplified scheme (1 for Target, 2 for Comparator, 3 for Outcome).
# This is a common practice in OHDSI studies for internal consistency.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Target cohort ID mapped to 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Comparator cohort ID mapped to 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Outcome cohort ID mapped to 3

# Negative control outcomes
# Retrieve negative control concept set definition from WebAPI.
# The concept set ID is specified in Analysis Specifications.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Negative control concept set ID from Analysis Specifications
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

# Check for duplicate cohort IDs to prevent conflicts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes:
# Filter for the outcome cohort (mapped to ID 3) and prepare for use in analyses.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not specified in Analysis Specifications

# Target and Comparator for the CohortMethod analysis
# Define the target and comparator cohorts using their re-mapped IDs and names from Analysis Specifications.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1", # Name from Analysis Specifications
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1" # Name from Analysis Specifications
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study.
# Based on Analysis Specifications, 'conceptsToExclude' is empty, so create an empty data frame.
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(0),
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# 'conceptsToInclude' in Analysis Specifications is empty, so this remains commented.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# Settings for the CohortGeneratorModule to generate cohorts.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default occurrence type for negative controls
  detectOnDescendants = TRUE # Default to detect on descendants
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Generate cohort statistics
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# Settings for the CohortDiagnosticsModule to run diagnostics on generated cohorts.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # Run diagnostics on all defined cohorts
  runInclusionStatistics = TRUE,
  runIncludedSourceConcepts = TRUE,
  runOrphanConcepts = TRUE,
  runTimeSeries = FALSE, # Not specified in Analysis Specifications, default to FALSE
  runVisitContext = TRUE,
  runBreakdownIndexEvents = TRUE,
  runIncidenceRate = TRUE,
  runCohortRelationship = TRUE,
  runTemporalCohortCharacterization = TRUE,
  minCharacterizationMean = 0.01 # Default minimum mean for characterization
)

# CohortMethodModule -----------------------------------------------------------

# Study periods for restricting the analysis to specific time windows.
# Extracted from 'getDbCohortMethodDataArgs.studyPeriods' in Analysis Specifications.
studyPeriods <- tibble(
  studyStartDate = c("20200101"), # YYYYMMDD
  studyEndDate   = c("20200515")  # YYYYMMDD
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# Extracted from 'createStudyPopArgs.timeAtRisks' in Analysis Specifications.
timeAtRisks <- tibble(
  label = c("TAR_1_99999_CS"), # A descriptive label for the time-at-risk window
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd  = c(99999),
  endAnchor = c("cohort start") # "cohort start" | "cohort end"
) 

# Propensity Score settings - match on PS
# Extracted from 'propensityScoreAdjustment.psSettings' in Analysis Specifications.
matchOnPsArgsList <- tibble(
  label = c("Match_MaxRatio4_Caliper0.2_SL"), # A descriptive label for the matching strategy
  maxRatio  = c(4),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # "propensity score" | "standardized" | "standardized logit"
) 

# Propensity Score settings - stratify by PS
# Extracted from 'propensityScoreAdjustment.psSettings' in Analysis Specifications.
stratifyByPsArgsList <- tibble(
  label = c("Stratify_5Strata_All"), # A descriptive label for the stratification strategy
  numberOfStrata  = c(5),
  baseSelection = c("all") # "all" | "target" | "comparator"
) 

# Build a single PS configuration list (each entry has: method, label, params)
# This list will contain all PS adjustment strategies to be iterated over.
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
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    # If no PS adjustment is specified in the JSON, we need to handle this case.
    # The template assumes at least one PS adjustment.
    # The JSON has an entry with both matchOnPsArgs and stratifyByPsArgs as null.
    # This implies a scenario where no PS adjustment is applied.
    # We will add a "no PS adjustment" configuration if the psConfigList is empty
    # or if the JSON explicitly indicates it.
    # Given the JSON has specific match/stratify settings, we will proceed with those.
    # The first entry in psSettings (both null) is effectively skipped by the above logic.
    
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      matchOnPsArgs <- NULL
      stratifyByPsArgs <- NULL

      if (psCfg$method == "match") {
        # Create arguments for matching on propensity score.
        # Parameters are from 'propensityScoreAdjustment.psSettings' in Analysis Specifications.
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default
          stratificationColumns = c() # Default
        )
      } else if (psCfg$method == "stratify") {
        # Create arguments for stratifying by propensity score.
        # Parameters are from 'propensityScoreAdjustment.psSettings' in Analysis Specifications.
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings for feature extraction.
      # Uses default settings, with descendants of excluded concepts also excluded.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Combine outcome cohorts (study outcomes and negative controls).
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not applicable for observed outcomes
            priorOutcomeLookback = 99999 # Default lookback for prior outcomes
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # For negative controls, true effect size is assumed to be 1 (no effect)
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
          # Excluded covariate concept IDs.
          # 'covariateSelection.conceptsToExclude' in Analysis Specifications is empty.
          # The template's original `cmTcList$targetConceptId[i]` and `cmTcList$comparatorConceptId[i]`
          # are removed as they are not provided in the Analysis Specifications.
          excludedCovariateConceptIds = c(
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # Arguments for getting data from the database for CohortMethod.
      # Parameters are from 'getDbCohortMethodDataArgs' in Analysis Specifications.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications
        studyStartDate = studyStartDate, # From studyPeriods loop
        studyEndDate = studyEndDate,     # From studyPeriods loop
        maxCohortSize = 0,               # From Analysis Specifications
        covariateSettings = covariateSettings,
        firstExposureOnly = TRUE,        # From Analysis Specifications
        washoutPeriod = 180,             # From Analysis Specifications
        removeDuplicateSubjects = "keep first" # From Analysis Specifications
      )

      # Arguments for creating propensity scores.
      # Parameters are from 'propensityScoreAdjustment.createPsArgs' in Analysis Specifications.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications
        errorOnHighCorrelation = TRUE,    # From Analysis Specifications
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att", # Default estimator
        prior = Cyclops::createPrior( # Prior settings from Analysis Specifications
          priorType = "laplace", 
          exclude = c(0), # Default
          useCrossValidation = TRUE # From Analysis Specifications
        ),
        control = Cyclops::createControl( # Control settings from Analysis Specifications
          noiseLevel = "silent", 
          cvType = "auto", 
          seed = 1, # Default seed
          resetCoefficients = TRUE, 
          tolerance = 2e-07, 
          cvRepetitions = 10, # From Analysis Specifications (fold: 10, cvRepetitions: 10)
          startingVariance = 0.01
        )
      )

      # Arguments for computing shared covariate balance.
      # Default settings.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      
      # Arguments for computing covariate balance.
      # Default settings.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting the outcome model.
      # Parameters are from 'fitOutcomeModelArgs' in Analysis Specifications.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",             # From Analysis Specifications
        stratified = TRUE,             # From Analysis Specifications
        useCovariates = FALSE,         # From Analysis Specifications
        inversePtWeighting = FALSE,    # From Analysis Specifications
        prior = Cyclops::createPrior(  # Prior settings from Analysis Specifications
          priorType = "laplace", 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl( # Control settings from Analysis Specifications
          cvType = "auto", 
          seed = 1, # Default seed
          resetCoefficients = TRUE,
          startingVariance = 0.01, 
          tolerance = 2e-07, 
          cvRepetitions = 10, # From Analysis Specifications (fold: 10, cvRepetitions: 10)
          noiseLevel = "quiet"
        )
      )
      
      # Arguments for creating the study population.
      # Parameters are from 'createStudyPopArgs' in Analysis Specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications
        firstExposureOnly = FALSE,      # From Analysis Specifications
        washoutPeriod = 0,              # From Analysis Specifications
        removeDuplicateSubjects = "keep all", # From Analysis Specifications
        censorAtNewRiskWindow = FALSE,  # From Analysis Specifications
        removeSubjectsWithPriorOutcome = FALSE, # From Analysis Specifications
        priorOutcomeLookback = 99999,   # From Analysis Specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From timeAtRisks loop
        startAnchor = timeAtRisks$startAnchor[t],         # From timeAtRisks loop
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],     # From timeAtRisks loop
        endAnchor = timeAtRisks$endAnchor[t],             # From timeAtRisks loop
        minDaysAtRisk = 1,              # From Analysis Specifications
        maxDaysAtRisk = 99999           # Default, not specified in Analysis Specifications
      )


      # Append the settings to Analysis List
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

# CohortMethodModule specifications.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # No analyses to exclude
  refitPsForEveryOutcome = FALSE, # Default
  refitPsForEveryStudyPopulation = FALSE, # Default
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default diagnostic thresholds
)

# Create the analysis specifications ------------------------------------------
# Combine all shared resources and module specifications into a single analysis specification object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path uses the 'name' from Analysis Specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "covid19ppiandh2ra", "covid19ppiandh2raAnalysisSpecification.json")
)