# Load Libraries ---------------------------------------------------------------
library(dplyr)
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)

# Placeholder for the analysis specifications JSON content.
# In a real scenario, this would typically be loaded from a JSON file using
# e.g., `jsonlite::read_json("path/to/your/analysis_spec.json")`.
# For this script, we define it directly based on the provided <Analysis Specifications>.
analysisSpecsInput <- list(
  name = "mars",
  cohortDefinitions = list(
    targetCohort = list(id = 1794126, name = "target1"),
    comparatorCohort = list(id = 1794132, name = "comparator1"),
    outcomeCohort = list(
      list(id = 1794131, name = "outcome1")
    )
  ),
  negativeControlConceptSet = list(id = 1888110, name = "negative"),
  covariateSelection = list(
    conceptsToInclude = list(
      list(id = NULL, name = "")
    ),
    conceptsToExclude = list(
      list(id = NULL, name = "")
    )
  ),
  getDbCohortMethodDataArgs = list(
    studyPeriods = list(
      list(studyStartDate = "20110101", studyEndDate = "20131231")
    ),
    maxCohortSize = 0,
    restrictToCommonPeriod = TRUE,
    firstExposureOnly = FALSE,
    washoutPeriod = 0,
    removeDuplicateSubjects = "keep all"
  ),
  createStudyPopArgs = list(
    restrictToCommonPeriod = FALSE,
    firstExposureOnly = FALSE,
    washoutPeriod = 0,
    removeDuplicateSubjects = "keep all",
    censorAtNewRiskWindow = FALSE,
    removeSubjectsWithPriorOutcome = TRUE,
    priorOutcomeLookBack = 99999, # Note: 'priorOutcomeLookBack' in specs, 'priorOutcomeLookback' in function
    timeAtRisks = list(
      list(riskWindowStart = 3, startAnchor = "cohort start", riskWindowEnd = 90, endAnchor = "cohort start", minDaysAtRisk = 1)
    )
  ),
  propensityScoreAdjustment = list(
    psSettings = list(
      list(
        matchOnPsArgs = list(maxRatio = 1, caliper = 0.2, caliperScale = "standardized logit"),
        stratifyByPsArgs = NULL
      )
    ),
    createPsArgs = list(
      maxCohortSizeForFitting = 250000,
      errorOnHighCorrelation = TRUE,
      prior = list(priorType = "laplace", useCrossValidation = TRUE),
      control = list(tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10, noiseLevel = "silent", resetCoefficients = TRUE, startingVariance = 0.01)
    )
  ),
  fitOutcomeModelArgs = list(
    modelType = "cox",
    stratified = FALSE,
    useCovariates = FALSE,
    inversePtWeighting = FALSE,
    prior = list(priorType = "laplace", useCrossValidation = TRUE),
    control = list(tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10, noiseLevel = "quiet", resetCoefficients = TRUE, startingVariance = 0.01)
  )
)


# Shared Resources -------------------------------------------------------------
# Base URL for the WebAPI, not provided in analysis specifications, using template default.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Extract cohort IDs and names from analysisSpecsInput.cohortDefinitions.
# These IDs will be re-numbered for internal consistency within Strategus.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    analysisSpecsInput$cohortDefinitions$targetCohort$id,    # Target Cohort ID
    analysisSpecsInput$cohortDefinitions$comparatorCohort$id, # Comparator Cohort ID
    analysisSpecsInput$cohortDefinitions$outcomeCohort[[1]]$id # Outcome Cohort ID
  ),
  generateStats = TRUE
)

# Re-number cohorts as per Strategus convention (1 for target, 2 for comparator, 3 for outcome).
# This ensures consistent internal IDs for the analysis modules.
cohortDefinitionSet[cohortDefinitionSet$cohortId == analysisSpecsInput$cohortDefinitions$targetCohort$id,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == analysisSpecsInput$cohortDefinitions$comparatorCohort$id,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == analysisSpecsInput$cohortDefinitions$outcomeCohort[[1]]$id,]$cohortId <- 3

# Negative control outcomes
# Extract negative control concept set ID from analysisSpecsInput.negativeControlConceptSet.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = analysisSpecsInput$negativeControlConceptSet$id,
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
  # Assign cohort IDs starting from 101 to avoid conflicts with target/comparator/outcome cohorts (1, 2, 3).
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs to prevent errors in Strategus.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts used in each analysis ---------------
# Outcomes: Filter for the main outcome cohort (re-numbered ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default cleanWindow from template

# Target and Comparator for the CohortMethod analysis
# Use the re-numbered target (ID 1) and comparator (ID 2) cohorts.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = cohortDefinitionSet[cohortDefinitionSet$cohortId == 1,]$cohortName,
  comparatorCohortId = 2,
  comparatorCohortName = cohortDefinitionSet[cohortDefinitionSet$cohortId == 2,]$cohortName
)

# Excluded covariate concepts for the CohortMethod analysis.
# From analysisSpecsInput.covariateSelection.conceptsToExclude.
# If the list is empty in the specs (i.e., first element's id is NULL), create an empty data frame.
if (length(analysisSpecsInput$covariateSelection$conceptsToExclude) > 0 &&
    !is.null(analysisSpecsInput$covariateSelection$conceptsToExclude[[1]]$id)) {
  excludedCovariateConcepts <- data.frame(
    conceptId = sapply(analysisSpecsInput$covariateSelection$conceptsToExclude, `[[`, "id"),
    conceptName = sapply(analysisSpecsInput$covariateSelection$conceptsToExclude, `[[`, "name")
  )
} else {
  excludedCovariateConcepts <- data.frame(conceptId = numeric(0), conceptName = character(0))
}

# Included covariate concepts (optional).
# From analysisSpecsInput.covariateSelection.conceptsToInclude.
# If the list is empty in the specs (i.e., first element's id is NULL), create an empty data frame.
if (length(analysisSpecsInput$covariateSelection$conceptsToInclude) > 0 &&
    !is.null(analysisSpecsInput$covariateSelection$conceptsToInclude[[1]]$id)) {
  includedCovariateConcepts <- data.frame(
    conceptId = sapply(analysisSpecsInput$covariateSelection$conceptsToInclude, `[[`, "id"),
    conceptName = sapply(analysisSpecsInput$covariateSelection$conceptsToInclude, `[[`, "name")
  )
} else {
  includedCovariateConcepts <- data.frame(conceptId = numeric(0), conceptName = character(0))
}


# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default from template
  detectOnDescendants = TRUE # Default from template
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Default from template
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # All re-numbered cohorts (target, comparator, outcome)
  runInclusionStatistics = TRUE, # Default from template
  runIncludedSourceConcepts = TRUE, # Default from template
  runOrphanConcepts = TRUE, # Default from template
  runTimeSeries = FALSE, # Default from template
  runVisitContext = TRUE, # Default from template
  runBreakdownIndexEvents = TRUE, # Default from template
  runIncidenceRate = TRUE, # Default from template
  runCohortRelationship = TRUE, # Default from template
  runTemporalCohortCharacterization = TRUE, # Default from template
  minCharacterizationMean = 0.01 # Default from template
)

# CohortMethodModule -----------------------------------------------------------

# Study periods from analysisSpecsInput.getDbCohortMethodDataArgs.studyPeriods.
# Convert the list of study period objects from the specs into a tibble.
studyPeriods <- tibble(
  studyStartDate = sapply(analysisSpecsInput$getDbCohortMethodDataArgs$studyPeriods, `[[`, "studyStartDate"),
  studyEndDate   = sapply(analysisSpecsInput$getDbCohortMethodDataArgs$studyPeriods, `[[`, "studyEndDate")
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# From analysisSpecsInput.createStudyPopArgs.timeAtRisks.
# Convert the list of TAR objects from the specs into a tibble and add a descriptive label.
timeAtRisks <- tibble(
  label = paste0("TAR_S", sapply(analysisSpecsInput$createStudyPopArgs$timeAtRisks, `[[`, "riskWindowStart"),
                 "_E", sapply(analysisSpecsInput$createStudyPopArgs$timeAtRisks, `[[`, "riskWindowEnd")),
  riskWindowStart  = sapply(analysisSpecsInput$createStudyPopArgs$timeAtRisks, `[[`, "riskWindowStart"),
  startAnchor = sapply(analysisSpecsInput$createStudyPopArgs$timeAtRisks, `[[`, "startAnchor"),
  riskWindowEnd  = sapply(analysisSpecsInput$createStudyPopArgs$timeAtRisks, `[[`, "riskWindowEnd"),
  endAnchor = sapply(analysisSpecsInput$createStudyPopArgs$timeAtRisks, `[[`, "endAnchor"),
  minDaysAtRisk = sapply(analysisSpecsInput$createStudyPopArgs$timeAtRisks, `[[`, "minDaysAtRisk")
)

# Propensity Score settings - match on PS.
# From analysisSpecsInput.propensityScoreAdjustment.psSettings.matchOnPsArgs.
# Initialize as an empty tibble, then populate if matchOnPsArgs are specified in the input.
matchOnPsArgsList <- tibble(
  label = character(0),
  maxRatio  = numeric(0),
  caliper = numeric(0),
  caliperScale  = character(0)
)
if (!is.null(analysisSpecsInput$propensityScoreAdjustment$psSettings[[1]]$matchOnPsArgs)) {
  matchOnPsArgsList <- tibble(
    label = "MatchOnPs", # A generic label for matching
    maxRatio  = analysisSpecsInput$propensityScoreAdjustment$psSettings[[1]]$matchOnPsArgs$maxRatio,
    caliper = analysisSpecsInput$propensityScoreAdjustment$psSettings[[1]]$matchOnPsArgs$caliper,
    caliperScale  = analysisSpecsInput$propensityScoreAdjustment$psSettings[[1]]$matchOnPsArgs$caliperScale
  )
}

# Propensity Score settings - stratify by PS.
# From analysisSpecsInput.propensityScoreAdjustment.psSettings.stratifyByPsArgs.
# Initialize as an empty tibble, then populate if stratifyByPsArgs are specified in the input.
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = numeric(0),
  baseSelection = character(0)
)
if (!is.null(analysisSpecsInput$propensityScoreAdjustment$psSettings[[1]]$stratifyByPsArgs)) {
  stratifyByPsArgsList <- tibble(
    label = "StratifyByPs", # A generic label for stratification
    numberOfStrata  = analysisSpecsInput$propensityScoreAdjustment$psSettings[[1]]$stratifyByPsArgs$numberOfStrata,
    baseSelection = analysisSpecsInput$propensityScoreAdjustment$psSettings[[1]]$stratifyByPsArgs$baseSelection
  )
}

# Build a single PS configuration list. Each entry contains the method, a label, and parameters.
psConfigList <- list()

# If matchOnPsArgs are defined, convert each row to a PS configuration.
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "match", # Identify the PS adjustment method
      label  = matchOnPsArgsList$label[i], # Human-readable label
      params = list( # Parameters passed to createMatchOnPsArgs
        maxRatio     = matchOnPsArgsList$maxRatio[i],
        caliper      = matchOnPsArgsList$caliper[i],
        caliperScale = matchOnPsArgsList$caliperScale[i]
      )
    )
  }
}

# If stratifyByPsArgs are defined, convert each row to a PS configuration.
if (exists("stratifyByPsArgsList") && nrow(stratifyByPsArgsList) > 0) {
  for (i in seq_len(nrow(stratifyByPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "stratify", # Identify the PS adjustment method
      label  = stratifyByPsArgsList$label[i], # Human-readable label
      params = list( # Parameters passed to createStratifyByPsArgs
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection  = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}


# Iterate through all analysis setting combinations (study periods, time-at-risks, PS methods)
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create matchOnPsArgs or stratifyByPsArgs based on the current PS configuration.
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default from template
          stratificationColumns = c() # Default from template
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default from template
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings for FeatureExtraction.
      # Uses default settings, and includes/excludes concepts if specified in analysisSpecsInput.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE, # Default from template
        includedCovariateConceptIds = includedCovariateConcepts$conceptId,
        excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
      )

      # Define outcomes for CohortMethod.
      # Includes the main outcome cohort and all resolved negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Default from template
            priorOutcomeLookback = 99999 # Default from template
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # Default from template
          )
        })
      )

      # Define target-comparator-outcome combinations.
      # This list will contain one entry for each target-comparator pair.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude specified covariate concepts.
          # The template included `cmTcList$targetConceptId[i]` and `cmTcList$comparatorConceptId[i]`
          # which are not directly available in the provided specs. Using `excludedCovariateConcepts$conceptId` instead.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Arguments for getting cohort method data from the database.
      # Values are extracted from analysisSpecsInput.getDbCohortMethodDataArgs and current loop variables.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = analysisSpecsInput$getDbCohortMethodDataArgs$restrictToCommonPeriod,
        studyStartDate = studyStartDate, # From current study period iteration
        studyEndDate = studyEndDate,     # From current study period iteration
        maxCohortSize = analysisSpecsInput$getDbCohortMethodDataArgs$maxCohortSize,
        covariateSettings = covariateSettings,
        firstExposureOnly = analysisSpecsInput$getDbCohortMethodDataArgs$firstExposureOnly,
        washoutPeriod = analysisSpecsInput$getDbCohortMethodDataArgs$washoutPeriod,
        removeDuplicateSubjects = analysisSpecsInput$getDbCohortMethodDataArgs$removeDuplicateSubjects
      )

      # Arguments for creating propensity scores.
      # Values are extracted from analysisSpecsInput.propensityScoreAdjustment.createPsArgs.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = analysisSpecsInput$propensityScoreAdjustment$createPsArgs$maxCohortSizeForFitting,
        errorOnHighCorrelation = analysisSpecsInput$propensityScoreAdjustment$createPsArgs$errorOnHighCorrelation,
        stopOnError = FALSE, # Default from template (allows Strategus to complete even if PS model fails)
        estimator = "att", # Default from template
        prior = Cyclops::createPrior(
          priorType = analysisSpecsInput$propensityScoreAdjustment$createPsArgs$prior$priorType,
          exclude = c(0), # Default from template
          useCrossValidation = analysisSpecsInput$propensityScoreAdjustment$createPsArgs$prior$useCrossValidation
        ),
        control = Cyclops::createControl(
          noiseLevel = analysisSpecsInput$propensityScoreAdjustment$createPsArgs$control$noiseLevel,
          cvType = analysisSpecsInput$propensityScoreAdjustment$createPsArgs$control$cvType,
          seed = 1, # Default from template
          resetCoefficients = analysisSpecsInput$propensityScoreAdjustment$createPsArgs$control$resetCoefficients,
          tolerance = analysisSpecsInput$propensityScoreAdjustment$createPsArgs$control$tolerance,
          cvRepetitions = analysisSpecsInput$propensityScoreAdjustment$createPsArgs$control$cvRepetitions,
          startingVariance = analysisSpecsInput$propensityScoreAdjustment$createPsArgs$control$startingVariance
        )
      )

      # Arguments for computing shared covariate balance.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default from template
        covariateFilter = NULL # Default from template
      )
      # Arguments for computing covariate balance.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default from template
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default from template
      )

      # Arguments for fitting the outcome model.
      # Values are extracted from analysisSpecsInput.fitOutcomeModelArgs.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = analysisSpecsInput$fitOutcomeModelArgs$modelType,
        stratified = analysisSpecsInput$fitOutcomeModelArgs$stratified,
        useCovariates = analysisSpecsInput$fitOutcomeModelArgs$useCovariates,
        inversePtWeighting = analysisSpecsInput$fitOutcomeModelArgs$inversePtWeighting,
        prior = Cyclops::createPrior(
          priorType = analysisSpecsInput$fitOutcomeModelArgs$prior$priorType,
          useCrossValidation = analysisSpecsInput$fitOutcomeModelArgs$prior$useCrossValidation
        ),
        control = Cyclops::createControl(
          cvType = analysisSpecsInput$fitOutcomeModelArgs$control$cvType,
          seed = 1, # Default from template
          resetCoefficients = analysisSpecsInput$fitOutcomeModelArgs$control$resetCoefficients,
          startingVariance = analysisSpecsInput$fitOutcomeModelArgs$control$startingVariance,
          tolerance = analysisSpecsInput$fitOutcomeModelArgs$control$tolerance,
          cvRepetitions = analysisSpecsInput$fitOutcomeModelArgs$control$cvRepetitions,
          noiseLevel = analysisSpecsInput$fitOutcomeModelArgs$control$noiseLevel
        )
      )

      # Arguments for creating the study population.
      # Values are extracted from analysisSpecsInput.createStudyPopArgs and current timeAtRisks iteration.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = analysisSpecsInput$createStudyPopArgs$restrictToCommonPeriod,
        firstExposureOnly = analysisSpecsInput$createStudyPopArgs$firstExposureOnly,
        washoutPeriod = analysisSpecsInput$createStudyPopArgs$washoutPeriod,
        removeDuplicateSubjects = analysisSpecsInput$createStudyPopArgs$removeDuplicateSubjects,
        censorAtNewRiskWindow = analysisSpecsInput$createStudyPopArgs$censorAtNewRiskWindow,
        removeSubjectsWithPriorOutcome = analysisSpecsInput$createStudyPopArgs$removeSubjectsWithPriorOutcome,
        priorOutcomeLookback = analysisSpecsInput$createStudyPopArgs$priorOutcomeLookBack, # Corrected field name from JSON
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From current time-at-risk iteration
        startAnchor = timeAtRisks$startAnchor[t],         # From current time-at-risk iteration
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],     # From current time-at-risk iteration
        endAnchor = timeAtRisks$endAnchor[t],             # From current time-at-risk iteration
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],     # From current time-at-risk iteration
        maxDaysAtRisk = 99999 # Default from template
      )

      # Append the settings to the CohortMethod analysis list.
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
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Default from template
  refitPsForEveryOutcome = FALSE, # Default from template
  refitPsForEveryStudyPopulation = FALSE, # Default from template
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default from template
)

# Create the overall analysis specifications for Strategus ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The study name "mars" is taken from analysisSpecsInput$name.
studyName <- analysisSpecsInput$name
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", studyName, paste0(studyName, "AnalysisSpecification.json"))
)