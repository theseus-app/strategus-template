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
# Retrieve cohort definitions for target, comparator, and outcome cohorts
# based on IDs provided in Analysis Specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to internal IDs for consistency within the study.
# Target cohort (ID 1794126) is mapped to internal ID 1.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# Comparator cohort (ID 1794132) is mapped to internal ID 2.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# Outcome cohort (ID 1794131) is mapped to internal ID 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve concept set definition for negative controls from Analysis Specifications.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # From Analysis Specifications: negativeControlConceptSet.id
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
  # Assign unique cohort IDs for negative controls, starting from 101 to avoid collision with T/C/O.
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs to prevent conflicts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Filter for the outcome cohort (internal ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # cleanWindow is not specified in Analysis Specifications, keeping template default.
  mutate(cleanWindow = 365) 

# Target and Comparator for the CohortMethod analysis 
# Populate with internal IDs and names for target (1) and comparator (2).
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = cohortDefinitionSet[cohortDefinitionSet$cohortId == 1,]$cohortName,
  comparatorCohortId = 2,
  comparatorCohortName = cohortDefinitionSet[cohortDefinitionSet$cohortId == 2,]$cohortName
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study. This is populated from covariateSelection.conceptsToExclude.
# Filter out entries where conceptId is NULL or conceptName is empty.
excludedCovariateConcepts <- data.frame(
  conceptId = c(NA), # Placeholder for initial empty data frame
  conceptName = c(NA)
)
if (!is.null(analysisSpecifications$covariateSelection$conceptsToExclude)) {
  valid_exclusions <- analysisSpecifications$covariateSelection$conceptsToExclude %>%
    purrr::keep(~ !is.null(.x$id) && .x$id != "" && .x$id != 0) # Filter out null/empty IDs
  if (length(valid_exclusions) > 0) {
    excludedCovariateConcepts <- data.frame(
      conceptId = purrr::map_dbl(valid_exclusions, "id"),
      conceptName = purrr::map_chr(valid_exclusions, "name")
    )
  } else {
    excludedCovariateConcepts <- data.frame(conceptId = numeric(0), conceptName = character(0))
  }
} else {
  excludedCovariateConcepts <- data.frame(conceptId = numeric(0), conceptName = character(0))
}


# Optional: If you want to define covariates to include instead of including them all
# This is populated from covariateSelection.conceptsToInclude.
# Filter out entries where conceptId is NULL or conceptName is empty.
includedCovariateConcepts <- data.frame(
  conceptId = c(NA), # Placeholder for initial empty data frame
  conceptName = c(NA)
)
if (!is.null(analysisSpecifications$covariateSelection$conceptsToInclude)) {
  valid_inclusions <- analysisSpecifications$covariateSelection$conceptsToInclude %>%
    purrr::keep(~ !is.null(.x$id) && .x$id != "" && .x$id != 0) # Filter out null/empty IDs
  if (length(valid_inclusions) > 0) {
    includedCovariateConcepts <- data.frame(
      conceptId = purrr::map_dbl(valid_inclusions, "id"),
      conceptName = purrr::map_chr(valid_inclusions, "name")
    )
  } else {
    includedCovariateConcepts <- data.frame(conceptId = numeric(0), conceptName = character(0))
  }
} else {
  includedCovariateConcepts <- data.frame(conceptId = numeric(0), conceptName = character(0))
}


# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  # occurrenceType and detectOnDescendants are not in Analysis Specifications, keeping template defaults.
  occurrenceType = "first", 
  detectOnDescendants = TRUE
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  # Include all cohort IDs: target (1), comparator (2), outcome (3), and negative controls (101+).
  cohortIds = c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId),
  # Other Cohort Diagnostics settings are not specified in Analysis Specifications, keeping template defaults.
  runInclusionStatistics = TRUE,
  runIncludedSourceConcepts = TRUE,
  runOrphanConcepts = TRUE,
  runTimeSeries = FALSE,
  runVisitContext = TRUE,
  runBreakdownIndexEvents = TRUE,
  runIncidenceRate = TRUE,
  runCohortRelationship = TRUE,
  runTemporalCohortCharacterization = TRUE,
  minCharacterizationMean = 0.01
)

# CohortMethodModule -----------------------------------------------------------

# Study periods from getDbCohortMethodDataArgs.studyPeriods in Analysis Specifications.
studyPeriods <- tibble(
  studyStartDate = c(analysisSpecifications$getDbCohortMethodDataArgs$studyPeriods[[1]]$studyStartDate), # YYYYMMDD
  studyEndDate   = c(analysisSpecifications$getDbCohortMethodDataArgs$studyPeriods[[1]]$studyEndDate)   # YYYYMMDD
)

# Time-at-risks (TARs) for the outcomes of interest in your study
# Populated from createStudyPopArgs.timeAtRisks in Analysis Specifications.
timeAtRisks <- tibble(
  label = c("Default TAR"), # Adding a label for description
  riskWindowStart  = c(analysisSpecifications$createStudyPopArgs$timeAtRisks[[1]]$riskWindowStart),
  startAnchor = c(analysisSpecifications$createStudyPopArgs$timeAtRisks[[1]]$startAnchor), # "cohort start" | "cohort end"
  riskWindowEnd  = c(analysisSpecifications$createStudyPopArgs$timeAtRisks[[1]]$riskWindowEnd),
  endAnchor = c(analysisSpecifications$createStudyPopArgs$timeAtRisks[[1]]$endAnchor) # "cohort start" | "cohort end"
) 

# Propensity Score settings - match on PS
# Check if matchOnPsArgs is specified in Analysis Specifications.
matchOnPsArgsList <- tibble(
  label = character(0),
  maxRatio  = numeric(0),
  caliper = numeric(0),
  caliperScale  = character(0) # "propensity score" | "standardized" | "standardized logit"
) 
if (!is.null(analysisSpecifications$propensityScoreAdjustment$psSettings[[1]]$matchOnPsArgs)) {
  matchOnPsArgsList <- tibble(
    label = c("Match on PS"), # Adding a label for description
    maxRatio  = c(analysisSpecifications$propensityScoreAdjustment$psSettings[[1]]$matchOnPsArgs$maxRatio),
    caliper = c(analysisSpecifications$propensityScoreAdjustment$psSettings[[1]]$matchOnPsArgs$caliper),
    caliperScale  = c(analysisSpecifications$propensityScoreAdjustment$psSettings[[1]]$matchOnPsArgs$caliperScale)
  )
}

# Propensity Score settings - stratify by PS
# Check if stratifyByPsArgs is specified in Analysis Specifications.
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = numeric(0),
  baseSelection = character(0) # "all" | "target" | "comparator"
) 
if (!is.null(analysisSpecifications$propensityScoreAdjustment$psSettings[[1]]$stratifyByPsArgs)) {
  stratifyByPsArgsList <- tibble(
    label = c("Stratify by PS"), # Adding a label for description
    numberOfStrata  = c(analysisSpecifications$propensityScoreAdjustment$psSettings[[1]]$stratifyByPsArgs$numberOfStrata),
    baseSelection = c(analysisSpecifications$propensityScoreAdjustment$psSettings[[1]]$stratifyByPsArgs$baseSelection)
  )
}

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
      
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Not in Analysis Specifications, keeping template default.
          stratificationColumns = c() # Not in Analysis Specifications, keeping template default.
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Not in Analysis Specifications, keeping template default.
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings for FeatureExtraction.
      # Includes concepts to include/exclude from Analysis Specifications.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE, # Not in Analysis Specifications, keeping template default.
        # Exclude concepts specified in Analysis Specifications.
        excludedCovariateConceptIds = if (nrow(excludedCovariateConcepts) > 0) excludedCovariateConcepts$conceptId else c(),
        # Include concepts specified in Analysis Specifications.
        includedCovariateConceptIds = if (nrow(includedCovariateConcepts) > 0) includedCovariateConcepts$conceptId else c()
      )

      # Define outcomes for the CohortMethod analysis.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not in Analysis Specifications, keeping template default.
            priorOutcomeLookback = 99999 # Not in Analysis Specifications, keeping template default.
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # Not in Analysis Specifications, keeping template default.
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
          # Exclude general covariate concepts specified in Analysis Specifications.
          # Removed cmTcList$targetConceptId[i] and cmTcList$comparatorConceptId[i] as they are not in JSON.
          excludedCovariateConceptIds = if (nrow(excludedCovariateConcepts) > 0) excludedCovariateConcepts$conceptId else c()
        )
      }

      # Arguments for retrieving cohort method data.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Not in Analysis Specifications, keeping template default.
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = analysisSpecifications$getDbCohortMethodDataArgs$maxCohortSize, # From Analysis Specifications.
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = analysisSpecifications$propensityScoreAdjustment$createPsArgs$maxCohortSizeForFitting, # From Analysis Specifications.
        errorOnHighCorrelation = analysisSpecifications$propensityScoreAdjustment$createPsArgs$errorOnHighCorrelation, # From Analysis Specifications.
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att", # Not in Analysis Specifications, keeping template default.
        prior = Cyclops::createPrior( # Prior settings from Analysis Specifications.
          priorType = analysisSpecifications$propensityScoreAdjustment$createPsArgs$prior$priorType, 
          exclude = c(0), # Not in Analysis Specifications, keeping template default.
          useCrossValidation = analysisSpecifications$propensityScoreAdjustment$createPsArgs$prior$useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings from Analysis Specifications.
          noiseLevel = analysisSpecifications$propensityScoreAdjustment$createPsArgs$control$noiseLevel, 
          cvType = analysisSpecifications$propensityScoreAdjustment$createPsArgs$control$cvType, 
          seed = 1, # Not in Analysis Specifications, keeping template default.
          resetCoefficients = analysisSpecifications$propensityScoreAdjustment$createPsArgs$control$resetCoefficients, 
          tolerance = analysisSpecifications$propensityScoreAdjustment$createPsArgs$control$tolerance, 
          cvRepetitions = analysisSpecifications$propensityScoreAdjustment$createPsArgs$control$cvRepetitions, 
          startingVariance = analysisSpecifications$propensityScoreAdjustment$createPsArgs$control$startingVariance
        )
      )

      # Arguments for computing shared covariate balance. Not in Analysis Specifications, keeping template defaults.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      # Arguments for computing covariate balance. Not in Analysis Specifications, keeping template defaults.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting the outcome model.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = analysisSpecifications$fitOutcomeModelArgs$modelType, # From Analysis Specifications.
        stratified = analysisSpecifications$fitOutcomeModelArgs$stratified, # From Analysis Specifications.
        useCovariates = analysisSpecifications$fitOutcomeModelArgs$useCovariates, # From Analysis Specifications.
        inversePtWeighting = analysisSpecifications$fitOutcomeModelArgs$inversePtWeighting, # From Analysis Specifications.
        prior = Cyclops::createPrior( # Prior settings from Analysis Specifications.
          priorType = analysisSpecifications$fitOutcomeModelArgs$prior$priorType, 
          useCrossValidation = analysisSpecifications$fitOutcomeModelArgs$prior$useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings from Analysis Specifications.
          cvType = analysisSpecifications$fitOutcomeModelArgs$control$cvType, 
          seed = 1, # Not in Analysis Specifications, keeping template default.
          resetCoefficients = analysisSpecifications$fitOutcomeModelArgs$control$resetCoefficients,
          startingVariance = analysisSpecifications$fitOutcomeModelArgs$control$startingVariance, 
          tolerance = analysisSpecifications$fitOutcomeModelArgs$control$tolerance, 
          cvRepetitions = analysisSpecifications$fitOutcomeModelArgs$control$cvRepetitions, 
          noiseLevel = analysisSpecifications$fitOutcomeModelArgs$control$noiseLevel
        )
      )
      
      # Arguments for creating the study population.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = analysisSpecifications$createStudyPopArgs$restrictToCommonPeriod, # From Analysis Specifications.
        firstExposureOnly = analysisSpecifications$createStudyPopArgs$firstExposureOnly, # From Analysis Specifications.
        washoutPeriod = analysisSpecifications$createStudyPopArgs$washoutPeriod, # From Analysis Specifications.
        removeDuplicateSubjects = analysisSpecifications$createStudyPopArgs$removeDuplicateSubjects, # From Analysis Specifications.
        censorAtNewRiskWindow = analysisSpecifications$createStudyPopArgs$censorAtNewRiskWindow, # From Analysis Specifications.
        removeSubjectsWithPriorOutcome = analysisSpecifications$createStudyPopArgs$removeSubjectsWithPriorOutcome, # From Analysis Specifications.
        priorOutcomeLookback = analysisSpecifications$createStudyPopArgs$priorOutcomeLookback, # From Analysis Specifications.
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From timeAtRisks (derived from Analysis Specifications).
        startAnchor = timeAtRisks$startAnchor[t], # From timeAtRisks (derived from Analysis Specifications).
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From timeAtRisks (derived from Analysis Specifications).
        endAnchor = timeAtRisks$endAnchor[t], # From timeAtRisks (derived from Analysis Specifications).
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From timeAtRisks (derived from Analysis Specifications).
        maxDaysAtRisk = 99999 # Not in Analysis Specifications, keeping template default.
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
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Not in Analysis Specifications, keeping template default.
  refitPsForEveryOutcome = FALSE, # Not in Analysis Specifications, keeping template default.
  refitPsForEveryStudyPopulation = FALSE, # Not in Analysis Specifications, keeping template default.
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Not in Analysis Specifications, keeping template default.
)

# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the generated analysis specifications to a JSON file.
# The file path is constructed using the study name "corazon" from Analysis Specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "corazon", "corazonAnalysisSpecification.json")
)