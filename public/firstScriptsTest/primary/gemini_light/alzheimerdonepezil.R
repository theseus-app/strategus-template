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
# Base URL for the ATLAS/WebAPI instance to retrieve cohort definitions and concept sets.
# This URL is taken from the template.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Retrieve cohort definitions from WebAPI based on IDs provided in Analysis Specifications.
# The IDs are re-mapped to sequential integers (1, 2, 3...) for internal use in Strategus.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: alzheimerdonepezil.target1
    1794132, # Comparator: alzheimerdonepezil.comparator1
    1794131  # Outcome: alzheimerdonepezil.outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal consistency within the Strategus analysis.
# Target cohort ID is mapped to 1.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# Comparator cohort ID is mapped to 2.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# Outcome cohort ID is mapped to 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve the concept set for negative controls from WebAPI.
# The concept set ID is taken from Analysis Specifications.
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
  # Assign unique cohort IDs starting from 101 to avoid collision with T/C/O cohorts.
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs to prevent errors in Strategus.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts used in each analysis ----------------
# Outcomes: Filter for the outcome cohort (ID 3) defined above.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # cleanWindow is not specified in Analysis Specifications, using a default.
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis.
# IDs and names are taken from Analysis Specifications and re-mapped IDs.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1", # From Analysis Specifications: cohortDefinitions.targetCohort.name
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1" # From Analysis Specifications: cohortDefinitions.comparatorCohort.name
)

# For the CohortMethod LSPS, we'll need to exclude specific concepts.
# From Analysis Specifications: covariateSelection.conceptsToExclude.
# If the list is empty or contains null/empty entries, create an empty data frame.
if (length(analysisSpecifications$covariateSelection$conceptsToExclude) > 0 &&
    !is.null(analysisSpecifications$covariateSelection$conceptsToExclude[[1]]$id)) {
  excludedCovariateConcepts <- data.frame(
    conceptId = sapply(analysisSpecifications$covariateSelection$conceptsToExclude, function(x) x$id),
    conceptName = sapply(analysisSpecifications$covariateSelection$conceptsToExclude, function(x) x$name)
  )
} else {
  excludedCovariateConcepts <- data.frame(
    conceptId = integer(0),
    conceptName = character(0)
  )
}

# Optional: If you want to define covariates to include instead of including them all.
# From Analysis Specifications: covariateSelection.conceptsToInclude.
# If the list is empty or contains null/empty entries, create an empty data frame.
if (length(analysisSpecifications$covariateSelection$conceptsToInclude) > 0 &&
    !is.null(analysisSpecifications$covariateSelection$conceptsToInclude[[1]]$id)) {
  includedCovariateConcepts <- data.frame(
    conceptId = sapply(analysisSpecifications$covariateSelection$conceptsToInclude, function(x) x$id),
    conceptName = sapply(analysisSpecifications$covariateSelection$conceptsToInclude, function(x) x$name)
  )
} else {
  includedCovariateConcepts <- data.frame(
    conceptId = integer(0),
    conceptName = character(0)
  )
}

# CohortGeneratorModule --------------------------------------------------------
# Creates shared resources for cohort definitions and negative controls,
# and specifies settings for the CohortGenerator module.
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
# Specifies settings for the CohortDiagnostics module.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
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

# Study periods for restricting the analysis.
# From Analysis Specifications: getDbCohortMethodDataArgs.studyPeriods.
# If studyStartDate and studyEndDate are null, it implies no date restriction,
# represented by empty strings to ensure the loop runs once.
if (is.null(analysisSpecifications$getDbCohortMethodDataArgs$studyPeriods[[1]]$studyStartDate) &&
    is.null(analysisSpecifications$getDbCohortMethodDataArgs$studyPeriods[[1]]$studyEndDate)) {
  studyPeriods <- tibble(
    studyStartDate = c(""),
    studyEndDate   = c("")
  )
} else {
  studyPeriods <- tibble(
    studyStartDate = sapply(analysisSpecifications$getDbCohortMethodDataArgs$studyPeriods, function(x) x$studyStartDate),
    studyEndDate   = sapply(analysisSpecifications$getDbCohortMethodDataArgs$studyPeriods, function(x) x$studyEndDate)
  )
}

# Time-at-risks (TARs) for the outcomes of interest in your study.
# From Analysis Specifications: createStudyPopArgs.timeAtRisks.
timeAtRisks <- tibble(
  label = sprintf(
    "TAR_S%d_E%d",
    analysisSpecifications$createStudyPopArgs$timeAtRisks[[1]]$riskWindowStart,
    analysisSpecifications$createStudyPopArgs$timeAtRisks[[1]]$riskWindowEnd
  ),
  riskWindowStart  = analysisSpecifications$createStudyPopArgs$timeAtRisks[[1]]$riskWindowStart,
  startAnchor = analysisSpecifications$createStudyPopArgs$timeAtRisks[[1]]$startAnchor,
  riskWindowEnd  = analysisSpecifications$createStudyPopArgs$timeAtRisks[[1]]$riskWindowEnd,
  endAnchor = analysisSpecifications$createStudyPopArgs$timeAtRisks[[1]]$endAnchor,
  minDaysAtRisk = analysisSpecifications$createStudyPopArgs$timeAtRisks[[1]]$minDaysAtRisk # Added from spec
)

# Propensity Score settings - match on PS.
# From Analysis Specifications: propensityScoreAdjustment.psSettings.matchOnPsArgs.
# If stratifyByPsArgs is null, only matchOnPsArgs is used.
matchOnPsArgsList <- tibble(
  label = sprintf(
    "Match_R%d_C%g_%s",
    analysisSpecifications$propensityScoreAdjustment$psSettings[[1]]$matchOnPsArgs$maxRatio,
    analysisSpecifications$propensityScoreAdjustment$psSettings[[1]]$matchOnPsArgs$caliper,
    analysisSpecifications$propensityScoreAdjustment$psSettings[[1]]$matchOnPsArgs$caliperScale
  ),
  maxRatio  = analysisSpecifications$propensityScoreAdjustment$psSettings[[1]]$matchOnPsArgs$maxRatio,
  caliper = analysisSpecifications$propensityScoreAdjustment$psSettings[[1]]$matchOnPsArgs$caliper,
  caliperScale  = analysisSpecifications$propensityScoreAdjustment$psSettings[[1]]$matchOnPsArgs$caliperScale
)

# Propensity Score settings - stratify by PS.
# From Analysis Specifications: propensityScoreAdjustment.psSettings.stratifyByPsArgs.
# This is null in the provided spec, so this tibble will be empty.
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0)
)

# Build a single PS configuration list (each entry has: method, label, params).
# This structure allows iterating over different PS adjustment methods.
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

# Iterate through all analysis setting combinations (study periods, TARs, PS methods).
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
      # Uses default settings but incorporates specific included/excluded concepts from Analysis Specifications.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE, # Default from template
        excludedCovariateConceptIds = excludedCovariateConcepts$conceptId, # From Analysis Specifications
        includedCovariateConceptIds = includedCovariateConcepts$conceptId # From Analysis Specifications
      )

      # List of outcomes for the analysis. Includes both primary outcomes and negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not specified in Analysis Specifications
            priorOutcomeLookback = 99999 # From Analysis Specifications: createStudyPopArgs.priorOutcomeLookBack
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # Default for negative controls
          )
        })
      )

      # Target-Comparator-Outcome (TCO) list.
      # Defines the T-C pairs and their associated outcomes and covariate exclusions.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude specific covariate concepts.
          # The template included cmTcList$targetConceptId and cmTcList$comparatorConceptId,
          # which are not defined in the spec, so they are removed.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Arguments for retrieving cohort method data from the database.
      # From Analysis Specifications: getDbCohortMethodDataArgs.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Default from template
        studyStartDate = studyStartDate, # From current iteration of studyPeriods
        studyEndDate = studyEndDate, # From current iteration of studyPeriods
        maxCohortSize = analysisSpecifications$getDbCohortMethodDataArgs$maxCohortSize, # From Analysis Specifications
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = analysisSpecifications$propensityScoreAdjustment$createPsArgs$maxCohortSizeForFitting, # From Analysis Specifications
        errorOnHighCorrelation = analysisSpecifications$propensityScoreAdjustment$createPsArgs$errorOnHighCorrelation, # From Analysis Specifications
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail (default from template)
        estimator = "att", # Default from template
        prior = Cyclops::createPrior( # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.prior
          priorType = analysisSpecifications$propensityScoreAdjustment$createPsArgs$prior$priorType,
          exclude = c(0), # Default from template
          useCrossValidation = analysisSpecifications$propensityScoreAdjustment$createPsArgs$prior$useCrossValidation
        ),
        control = Cyclops::createControl( # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control
          noiseLevel = analysisSpecifications$propensityScoreAdjustment$createPsArgs$control$noiseLevel,
          cvType = analysisSpecifications$propensityScoreAdjustment$createPsArgs$control$cvType,
          seed = 1, # Default from template
          resetCoefficients = analysisSpecifications$propensityScoreAdjustment$createPsArgs$control$resetCoefficients,
          tolerance = analysisSpecifications$propensityScoreAdjustment$createPsArgs$control$tolerance,
          cvRepetitions = analysisSpecifications$propensityScoreAdjustment$createPsArgs$control$cvRepetitions, # From Analysis Specifications
          startingVariance = analysisSpecifications$propensityScoreAdjustment$createPsArgs$control$startingVariance
        )
      )

      # Arguments for computing shared covariate balance.
      # No specific settings in Analysis Specifications, using template defaults.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      # Arguments for computing covariate balance.
      # No specific settings in Analysis Specifications, using template defaults.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting the outcome model.
      # From Analysis Specifications: fitOutcomeModelArgs.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = analysisSpecifications$fitOutcomeModelArgs$modelType, # From Analysis Specifications
        stratified = analysisSpecifications$fitOutcomeModelArgs$stratified, # From Analysis Specifications
        useCovariates = analysisSpecifications$fitOutcomeModelArgs$useCovariates, # From Analysis Specifications
        inversePtWeighting = analysisSpecifications$fitOutcomeModelArgs$inversePtWeighting, # From Analysis Specifications
        prior = Cyclops::createPrior( # From Analysis Specifications: fitOutcomeModelArgs.prior
          priorType = analysisSpecifications$fitOutcomeModelArgs$prior$priorType,
          useCrossValidation = analysisSpecifications$fitOutcomeModelArgs$prior$useCrossValidation
        ),
        control = Cyclops::createControl( # From Analysis Specifications: fitOutcomeModelArgs.control
          cvType = analysisSpecifications$fitOutcomeModelArgs$control$cvType,
          seed = 1, # Default from template
          resetCoefficients = analysisSpecifications$fitOutcomeModelArgs$control$resetCoefficients,
          startingVariance = analysisSpecifications$fitOutcomeModelArgs$control$startingVariance,
          tolerance = analysisSpecifications$fitOutcomeModelArgs$control$tolerance,
          cvRepetitions = analysisSpecifications$fitOutcomeModelArgs$control$cvRepetitions, # From Analysis Specifications
          noiseLevel = analysisSpecifications$fitOutcomeModelArgs$control$noiseLevel
        )
      )

      # Arguments for creating the study population.
      # From Analysis Specifications: createStudyPopArgs.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = analysisSpecifications$createStudyPopArgs$restrictToCommonPeriod, # From Analysis Specifications
        firstExposureOnly = analysisSpecifications$createStudyPopArgs$firstExposureOnly, # From Analysis Specifications
        washoutPeriod = analysisSpecifications$createStudyPopArgs$washoutPeriod, # From Analysis Specifications
        removeDuplicateSubjects = analysisSpecifications$createStudyPopArgs$removeDuplicateSubjects, # From Analysis Specifications
        censorAtNewRiskWindow = analysisSpecifications$createStudyPopArgs$censorAtNewRiskWindow, # From Analysis Specifications
        removeSubjectsWithPriorOutcome = analysisSpecifications$createStudyPopArgs$removeSubjectsWithPriorOutcome, # From Analysis Specifications
        priorOutcomeLookback = analysisSpecifications$createStudyPopArgs$priorOutcomeLookBack, # From Analysis Specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From current iteration of timeAtRisks
        startAnchor = timeAtRisks$startAnchor[t], # From current iteration of timeAtRisks
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From current iteration of timeAtRisks
        endAnchor = timeAtRisks$endAnchor[t], # From current iteration of timeAtRisks
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From current iteration of timeAtRisks
        maxDaysAtRisk = 99999 # Not specified in Analysis Specifications, using a default
      )

      # Append the settings to the CohortMethod Analysis List.
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

# Create the CohortMethod module specifications.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Default from template
  refitPsForEveryOutcome = FALSE, # Default from template
  refitPsForEveryStudyPopulation = FALSE, # Default from template
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default from template
)

# Create the overall analysis specifications ----------------------------------
# Combine all shared resources and module specifications into a single Strategus analysis specification.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path uses the study name "alzheimerdonepezil" from Analysis Specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "alzheimerdonepezil", "alzheimerdonepezilAnalysisSpecification.json")
)