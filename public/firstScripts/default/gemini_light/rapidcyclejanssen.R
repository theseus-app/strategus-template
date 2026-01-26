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
# Export cohort definitions from WebAPI based on IDs provided in Analysis Specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1 (from Analysis Specifications -> cohortDefinitions -> targetCohort -> id)
    1794132, # Comparator: comparator1 (from Analysis Specifications -> cohortDefinitions -> comparatorCohort -> id)
    1794131  # Outcome: outcome1 (from Analysis Specifications -> cohortDefinitions -> outcomeCohort -> id)
  ),
  generateStats = TRUE
)

# Re-number cohorts to internal study IDs for consistency and to avoid conflicts.
# Target cohort re-numbered to 1.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# Comparator cohort re-numbered to 2.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# Outcome cohort re-numbered to 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve negative control concept set definition from WebAPI.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # From Analysis Specifications -> negativeControlConceptSet -> id
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(
    baseUrl = baseUrl
  ) %>%
  ROhdsiWebApi::getConcepts(
    baseUrl = baseUrl
  ) %>%
  # Rename columns to match expected format for outcome cohorts.
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  # Assign unique cohort IDs starting from 101 to avoid overlap with target/comparator/outcome.
  mutate(cohortId = row_number() + 100) %>%
  # Select relevant columns.
  select(cohortId, cohortName, outcomeConceptId)

# Ensure no duplicate cohort IDs exist across all defined cohorts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts used in each analysis ---------------
# Outcomes: Filter for the re-numbered outcome cohort (ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Outcome1 (re-numbered ID 3)
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not specified in analysis specifications.

# Target and Comparator for the CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1, # Target1 (re-numbered ID 1)
  targetCohortName = "target1", # From Analysis Specifications -> cohortDefinitions -> targetCohort -> name
  comparatorCohortId = 2, # Comparator1 (re-numbered ID 2)
  comparatorCohortName = "comparator1" # From Analysis Specifications -> cohortDefinitions -> comparatorCohort -> name
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study.
# From Analysis Specifications -> covariateSelection -> conceptsToExclude (empty in spec).
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(0),
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# From Analysis Specifications -> covariateSelection -> conceptsToInclude (empty in spec).
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Create shared resource specifications for cohort definitions.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Create shared resource specifications for negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default, not specified in analysis specifications.
  detectOnDescendants = TRUE # Default, not specified in analysis specifications.
)
# Create module specifications for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Default, not specified in analysis specifications.
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Create module specifications for CohortDiagnostics.
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # All defined cohorts.
  runInclusionStatistics = TRUE, # Default, not specified in analysis specifications.
  runIncludedSourceConcepts = TRUE, # Default, not specified in analysis specifications.
  runOrphanConcepts = TRUE, # Default, not specified in analysis specifications.
  runTimeSeries = FALSE, # Default, not specified in analysis specifications.
  runVisitContext = TRUE, # Default, not specified in analysis specifications.
  runBreakdownIndexEvents = TRUE, # Default, not specified in analysis specifications.
  runIncidenceRate = TRUE, # Default, not specified in analysis specifications.
  runCohortRelationship = TRUE, # Default, not specified in analysis specifications.
  runTemporalCohortCharacterization = TRUE, # Default, not specified in analysis specifications.
  minCharacterizationMean = 0.01 # Default, not specified in analysis specifications.
)

# CohortMethodModule -----------------------------------------------------------

# Study periods for restricting the analysis.
# From Analysis Specifications -> getDbCohortMethodDataArgs -> studyPeriods.
studyPeriods <- tibble(
  studyStartDate = c(20210101), # YYYYMMDD (from Analysis Specifications -> getDbCohortMethodDataArgs -> studyPeriods -> studyStartDate)
  studyEndDate   = c(NA_character_) # YYYYMMDD (NULL in spec, represented as NA_character_ for date strings)
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# From Analysis Specifications -> createStudyPopArgs -> timeAtRisks.
timeAtRisks <- tibble(
  label = c("TAR 1-14", "TAR 1-28", "TAR 1-42", "TAR 1-90", "TAR 0-2"), # Descriptive labels for each TAR.
  riskWindowStart  = c(1, 1, 1, 1, 0), # From Analysis Specifications -> createStudyPopArgs -> timeAtRisks -> riskWindowStart
  startAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start"), # From Analysis Specifications -> createStudyPopArgs -> timeAtRisks -> startAnchor
  riskWindowEnd  = c(14, 28, 42, 90, 2), # From Analysis Specifications -> createStudyPopArgs -> timeAtRisks -> riskWindowEnd
  endAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start") # From Analysis Specifications -> createStudyPopArgs -> timeAtRisks -> endAnchor
) 

# Propensity Score settings - match on PS
# From Analysis Specifications -> propensityScoreAdjustment -> psSettings -> matchOnPsArgs.
matchOnPsArgsList <- tibble(
  label = c("Match on PS"), # Descriptive label for this PS adjustment method.
  maxRatio  = c(100), # From Analysis Specifications -> propensityScoreAdjustment -> psSettings -> matchOnPsArgs -> maxRatio
  caliper = c(0.2), # From Analysis Specifications -> propensityScoreAdjustment -> psSettings -> matchOnPsArgs -> caliper
  caliperScale  = c("standardized logit") # From Analysis Specifications -> propensityScoreAdjustment -> psSettings -> matchOnPsArgs -> caliperScale
) 

# Propensity Score settings - stratify by PS
# From Analysis Specifications -> propensityScoreAdjustment -> psSettings -> stratifyByPsArgs (NULL in spec).
stratifyByPsArgsList <- tibble(
  label = character(0), # No stratification settings provided in analysis specifications.
  numberOfStrata  = numeric(0),
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


# Iterate through all analysis setting combinations
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment arguments based on the method (match or stratify).
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio, # From Analysis Specifications -> propensityScoreAdjustment -> psSettings -> matchOnPsArgs -> maxRatio
          caliper = psCfg$params$caliper, # From Analysis Specifications -> propensityScoreAdjustment -> psSettings -> matchOnPsArgs -> caliper
          caliperScale = psCfg$params$caliperScale, # From Analysis Specifications -> propensityScoreAdjustment -> psSettings -> matchOnPsArgs -> caliperScale
          allowReverseMatch = FALSE, # Default, not specified in analysis specifications.
          stratificationColumns = c() # Default, not specified in analysis specifications.
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata, # From Analysis Specifications -> propensityScoreAdjustment -> psSettings -> stratifyByPsArgs -> numberOfStrata
          stratificationColumns = c(), # Default, not specified in analysis specifications.
          baseSelection = psCfg$params$baseSelection # From Analysis Specifications -> propensityScoreAdjustment -> psSettings -> stratifyByPsArgs -> baseSelection
        )
      }

      # Covariate settings for feature extraction.
      # Using default settings as covariateSelection in spec is empty.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default, not specified in analysis specifications.
      )

      # Define outcome cohorts, including the primary outcome and negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i], # Primary outcome cohort ID (re-numbered 3).
            outcomeOfInterest = TRUE, # This is the outcome of interest.
            trueEffectSize = NA, # Not applicable for observed outcomes.
            priorOutcomeLookback = 99999 # Default, not specified in analysis specifications.
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i, # Negative control outcome cohort ID.
            outcomeOfInterest = FALSE, # These are not outcomes of interest.
            trueEffectSize = 1 # Assumed true effect size for negative controls.
          )
        })
      )
      
      # Define target-comparator-outcome combinations.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i], # Target cohort ID (re-numbered 1).
          comparatorId = cmTcList$comparatorCohortId[i], # Comparator cohort ID (re-numbered 2).
          outcomes = outcomeList, # List of all outcomes (primary + negative controls).
          # Excluded covariate concept IDs. From Analysis Specifications -> covariateSelection -> conceptsToExclude (empty).
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId 
        )
      }

      # Arguments for retrieving cohort method data from the database.
      # From Analysis Specifications -> getDbCohortMethodDataArgs.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate, # From loop (Analysis Specifications -> getDbCohortMethodDataArgs -> studyPeriods -> studyStartDate)
        studyEndDate = studyEndDate, # From loop (Analysis Specifications -> getDbCohortMethodDataArgs -> studyPeriods -> studyEndDate)
        maxCohortSize = 0, # From Analysis Specifications -> getDbCohortMethodDataArgs -> maxCohortSize
        restrictToCommonPeriod = TRUE, # From Analysis Specifications -> getDbCohortMethodDataArgs -> restrictToCommonPeriod
        firstExposureOnly = TRUE, # From Analysis Specifications -> getDbCohortMethodDataArgs -> firstExposureOnly
        washoutPeriod = 365, # From Analysis Specifications -> getDbCohortMethodDataArgs -> washoutPeriod
        removeDuplicateSubjects = "remove all", # From Analysis Specifications -> getDbCohortMethodDataArgs -> removeDuplicateSubjects
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> errorOnHighCorrelation
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att", # Default, not specified in analysis specifications.
        prior = Cyclops::createPrior( # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> prior
          priorType = "laplace", # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> prior -> priorType
          exclude = c(0), # Default, not specified in analysis specifications.
          useCrossValidation = TRUE # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> prior -> useCrossValidation
        ),
        control = Cyclops::createControl( # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> control
          noiseLevel = "silent", # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> control -> noiseLevel
          cvType = "auto", # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> control -> cvType
          seed = 1, # Default, not specified in analysis specifications.
          resetCoefficients = TRUE, # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> control -> resetCoefficients
          tolerance = 2e-07, # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> control -> tolerance
          cvRepetitions = 10, # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> control -> cvRepetitions
          startingVariance = 0.01, # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> control -> startingVariance
          fold = 10 # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> control -> fold
        )
      )

      # Arguments for computing shared covariate balance.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default, not specified in analysis specifications.
        covariateFilter = NULL # Default, not specified in analysis specifications.
      )
      # Arguments for computing covariate balance.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default, not specified in analysis specifications.
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default, not specified in analysis specifications.
      )

      # Arguments for fitting the outcome model.
      # From Analysis Specifications -> fitOutcomeModelArgs.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From Analysis Specifications -> fitOutcomeModelArgs -> modelType
        stratified = TRUE, # From Analysis Specifications -> fitOutcomeModelArgs -> stratified
        useCovariates = FALSE, # From Analysis Specifications -> fitOutcomeModelArgs -> useCovariates
        inversePtWeighting = FALSE, # From Analysis Specifications -> fitOutcomeModelArgs -> inversePtWeighting
        prior = Cyclops::createPrior( # From Analysis Specifications -> fitOutcomeModelArgs -> prior
          priorType = "laplace", # From Analysis Specifications -> fitOutcomeModelArgs -> prior -> priorType
          useCrossValidation = TRUE # From Analysis Specifications -> fitOutcomeModelArgs -> prior -> useCrossValidation
        ),
        control = Cyclops::createControl( # From Analysis Specifications -> fitOutcomeModelArgs -> control
          cvType = "auto", # From Analysis Specifications -> fitOutcomeModelArgs -> control -> cvType
          seed = 1, # Default, not specified in analysis specifications.
          resetCoefficients = TRUE, # From Analysis Specifications -> fitOutcomeModelArgs -> control -> resetCoefficients
          startingVariance = 0.01, # From Analysis Specifications -> fitOutcomeModelArgs -> control -> startingVariance
          tolerance = 2e-07, # From Analysis Specifications -> fitOutcomeModelArgs -> control -> tolerance
          cvRepetitions = 10, # From Analysis Specifications -> fitOutcomeModelArgs -> control -> cvRepetitions
          noiseLevel = "quiet", # From Analysis Specifications -> fitOutcomeModelArgs -> control -> noiseLevel
          fold = 10 # From Analysis Specifications -> fitOutcomeModelArgs -> control -> fold
        )
      )
      
      # Arguments for creating the study population.
      # From Analysis Specifications -> createStudyPopArgs.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications -> createStudyPopArgs -> restrictToCommonPeriod
        firstExposureOnly = FALSE, # From Analysis Specifications -> createStudyPopArgs -> firstExposureOnly
        washoutPeriod = 0, # From Analysis Specifications -> createStudyPopArgs -> washoutPeriod
        removeDuplicateSubjects = "keep all", # From Analysis Specifications -> createStudyPopArgs -> removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # From Analysis Specifications -> createStudyPopArgs -> censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # From Analysis Specifications -> createStudyPopArgs -> removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # From Analysis Specifications -> createStudyPopArgs -> priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From loop (Analysis Specifications -> createStudyPopArgs -> timeAtRisks -> riskWindowStart)
        startAnchor = timeAtRisks$startAnchor[t], # From loop (Analysis Specifications -> createStudyPopArgs -> timeAtRisks -> startAnchor)
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From loop (Analysis Specifications -> createStudyPopArgs -> timeAtRisks -> riskWindowEnd)
        endAnchor = timeAtRisks$endAnchor[t], # From loop (Analysis Specifications -> createStudyPopArgs -> timeAtRisks -> endAnchor)
        minDaysAtRisk = 1, # From Analysis Specifications -> createStudyPopArgs -> timeAtRisks -> minDaysAtRisk (common for all TARs)
        maxDaysAtRisk = 99999 # Default, not specified in analysis specifications.
      )

      # Append the settings to Analysis List
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

cmModuleSettingsCreator <- CohortMethodModule$new()
# Create module specifications for CohortMethod.
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Default, not specified in analysis specifications.
  refitPsForEveryOutcome = FALSE, # Default, not specified in analysis specifications.
  refitPsForEveryStudyPopulation = FALSE, # Default, not specified in analysis specifications.
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default, not specified in analysis specifications.
)

# Create the analysis specifications ------------------------------------------
# Initialize empty analysis specifications and add shared resources and module specifications.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path uses the study name "rapidcyclejanssen" from Analysis Specifications -> name.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "rapidcyclejanssen", "rapidcyclejanssenAnalysisSpecification.json")
)