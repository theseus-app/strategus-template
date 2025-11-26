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
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(tibble)

# Replicating the structure of analysisSpec from the original script's JSON
# This replaces the jsonlite::fromJSON call from the original script to align with template style.
analysisSpecValues <- list(
  name = "ranitidinecancer",
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
      list(studyStartDate = NULL, studyEndDate = NULL)
    ),
    maxCohortSize = 0
  ),
  createStudyPopArgs = list(
    restrictToCommonPeriod = FALSE,
    firstExposureOnly = FALSE,
    washoutPeriod = 365,
    removeDuplicateSubjects = "keep all",
    censorAtNewRiskWindow = FALSE,
    removeSubjectsWithPriorOutcome = TRUE,
    priorOutcomeLookBack = 365, # Note: original script used 'priorOutcomeLookBack'
    timeAtRisks = list(
      list(
        riskWindowStart = 365,
        startAnchor = "cohort start",
        riskWindowEnd = 99999,
        endAnchor = "cohort start",
        minDaysAtRisk = 1
      )
    )
  ),
  propensityScoreAdjustment = list(
    psSettings = list(
      list(
        matchOnPsArgs = list(
          maxRatio = 1,
          caliper = 0.2,
          caliperScale = "standardized logit"
        ),
        stratifyByPsArgs = NULL
      )
    ),
    createPsArgs = list(
      maxCohortSizeForFitting = 250000,
      errorOnHighCorrelation = TRUE,
      prior = list(
        priorType = "laplace",
        useCrossValidation = TRUE
      ),
      control = list(
        tolerance = 2e-7,
        cvType = "auto",
        fold = 10,
        cvRepetitions = 10,
        noiseLevel = "silent",
        resetCoefficients = TRUE,
        startingVariance = 0.01
      )
    )
  ),
  fitOutcomeModelArgs = list(
    modelType = "cox",
    stratified = FALSE,
    useCovariates = FALSE,
    inversePtWeighting = FALSE,
    prior = list(
      priorType = "laplace",
      useCrossValidation = TRUE
    ),
    control = list(
      tolerance = 2e-7,
      cvType = "auto",
      fold = 10,
      cvRepetitions = 10,
      noiseLevel = "quiet",
      resetCoefficients = TRUE,
      startingVariance = 0.01
    )
  ),
  maxCohortSize = 0
)


# Shared Resources -------------------------------------------------------------
# Base URL for OHDSI Atlas/WebAPI to fetch cohort definitions and concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Extract cohort IDs for target, comparator, and outcome from analysis specifications.
targetCohortId <- analysisSpecValues$cohortDefinitions$targetCohort$id
comparatorCohortId <- analysisSpecValues$cohortDefinitions$comparatorCohort$id
outcomeCohortId <- analysisSpecValues$cohortDefinitions$outcomeCohort[[1]]$id # Assuming one outcome cohort

# Fetch cohort definitions from WebAPI
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(targetCohortId, comparatorCohortId, outcomeCohortId),
  generateStats = TRUE
)

# Re-number cohorts to internal study IDs for Strategus (Target: 1, Comparator: 2, Outcome: 3)
cohortDefinitionSet <- cohortDefinitionSet %>%
  mutate(
    cohortId = case_when(
      .data$cohortId == targetCohortId ~ 1,
      .data$cohortId == comparatorCohortId ~ 2,
      .data$cohortId == outcomeCohortId ~ 3,
      TRUE ~ .data$cohortId # Keep other cohortIds as is, though not expected here
    ),
    cohortName = case_when(
      .data$cohortId == 1 ~ analysisSpecValues$cohortDefinitions$targetCohort$name,
      .data$cohortId == 2 ~ analysisSpecValues$cohortDefinitions$comparatorCohort$name,
      .data$cohortId == 3 ~ analysisSpecValues$cohortDefinitions$outcomeCohort[[1]]$name,
      TRUE ~ .data$cohortName
    )
  )

# Negative control outcomes
# Extract the negative control concept set ID from analysis specifications.
negativeControlConceptSetId <- analysisSpecValues$negativeControlConceptSet$id
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = negativeControlConceptSetId,
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
  # Assign cohort IDs starting from 101 to avoid clashes with target/comparator/outcome.
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Corrected duplicate check using c() to combine vectors
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis
# Outcomes list: filters for the re-numbered outcome cohort (ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = .data$cohortId, outcomeCohortName = .data$cohortName) %>%
  select(.data$outcomeCohortId, .data$outcomeCohortName) %>%
  mutate(cleanWindow = 365) # `cleanWindow` is not specified in analysisSpec, using template default.

# Target and Comparator for the CohortMethod analysis using re-numbered IDs.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = analysisSpecValues$cohortDefinitions$targetCohort$name,
  comparatorCohortId = 2,
  comparatorCohortName = analysisSpecValues$cohortDefinitions$comparatorCohort$name
)

# Excluded covariate concepts: from `covariateSelection.conceptsToExclude` in analysis specifications.
excludedCovariateConceptsSpec <- analysisSpecValues$covariateSelection$conceptsToExclude
excludedCovariateConceptIds <- unlist(lapply(excludedCovariateConceptsSpec, `[[`, "id"))
excludedCovariateConceptIds <- excludedCovariateConceptIds[!sapply(excludedCovariateConceptIds, is.null)]
excludedCovariateConcepts <- data.frame(conceptId = as.numeric(excludedCovariateConceptIds))

# Included covariate concepts: from `covariateSelection.conceptsToInclude` in analysis specifications.
includedCovariateConceptsSpec <- analysisSpecValues$covariateSelection$conceptsToInclude
includedCovariateConceptIds <- unlist(lapply(includedCovariateConceptsSpec, `[[`, "id"))
includedCovariateConceptIds <- includedCovariateConceptIds[!sapply(includedCovariateConceptIds, is.null)]
includedCovariateConcepts <- data.frame(conceptId = as.numeric(includedCovariateConceptIds))


# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Template default
  detectOnDescendants = TRUE # Template default
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Template default
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE, # Template default
  runIncludedSourceConcepts = TRUE, # Template default
  runOrphanConcepts = TRUE, # Template default
  runTimeSeries = FALSE, # Template default
  runVisitContext = TRUE, # Template default
  runBreakdownIndexEvents = TRUE, # Template default
  runIncidenceRate = TRUE, # Template default
  runCohortRelationship = TRUE, # Template default
  runTemporalCohortCharacterization = TRUE, # Template default
  minCharacterizationMean = 0.01 # Template default
)

# CohortMethodModule -----------------------------------------------------------

# Study Periods for CohortMethod: extracted from `getDbCohortMethodDataArgs` in analysis specifications.
studyPeriodsSpec <- analysisSpecValues$getDbCohortMethodDataArgs$studyPeriods
if (length(studyPeriodsSpec) > 0 && (!is.null(studyPeriodsSpec[[1]]$studyStartDate) || !is.null(studyPeriodsSpec[[1]]$studyEndDate))) {
  studyPeriods <- tibble(
    studyStartDate = sapply(studyPeriodsSpec, `[[`, "studyStartDate"),
    studyEndDate = sapply(studyPeriodsSpec, `[[`, "studyEndDate"),
    label = paste0("StudyPeriod", seq_along(studyPeriodsSpec)) # Label for description
  )
} else {
  # If study periods are null or empty in the spec, create a single entry with empty strings for no restriction.
  studyPeriods <- tibble(
    studyStartDate = "",
    studyEndDate = "",
    label = "NoStudyPeriodRestriction"
  )
}

# Time-at-risks (TARs) for the outcomes of interest: extracted from `createStudyPopArgs` in analysis specifications.
timeAtRisksSpec <- analysisSpecValues$createStudyPopArgs$timeAtRisks
timeAtRisks <- tibble(
  label = sapply(seq_along(timeAtRisksSpec), function(i) {
    tar <- timeAtRisksSpec[[i]]
    # Create an informative label based on risk window settings.
    sprintf("TAR%d_S%d%s_E%d%s", i, tar$riskWindowStart, substr(tar$startAnchor, 1, 1), tar$riskWindowEnd, substr(tar$endAnchor, 1, 1))
  }),
  riskWindowStart  = sapply(timeAtRisksSpec, `[[`, "riskWindowStart"),
  startAnchor = sapply(timeAtRisksSpec, `[[`, "startAnchor"),
  riskWindowEnd  = sapply(timeAtRisksSpec, `[[`, "riskWindowEnd"),
  endAnchor = sapply(timeAtRisksSpec, `[[`, "endAnchor"),
  minDaysAtRisk = sapply(timeAtRisksSpec, `[[`, "minDaysAtRisk") # Used in createStudyPopArgs
)

# Propensity Score settings - match on PS
matchOnPsArgsList <- tibble()
if (!is.null(analysisSpecValues$propensityScoreAdjustment$psSettings[[1]]$matchOnPsArgs)) {
  matchSettings <- analysisSpecValues$propensityScoreAdjustment$psSettings[[1]]$matchOnPsArgs
  matchOnPsArgsList <- tibble(
    label = "PS_Match_1", # Label for this PS adjustment setting
    maxRatio  = matchSettings$maxRatio,
    caliper = matchSettings$caliper,
    caliperScale  = matchSettings$caliperScale
  )
}

# Propensity Score settings - stratify by PS
# In this analysis specification, `stratifyByPsArgs` is null, so this will be an empty tibble.
stratifyByPsArgsList <- tibble()

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# If a data frame for "match on PS" exists and has rows, convert each row to a config
if (nrow(matchOnPsArgsList) > 0) {
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

# If a data frame for "stratify by PS" exists and has rows, convert each row to a config
# This block will not execute based on the current analysis specification.
if (nrow(stratifyByPsArgsList) > 0) {
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

# Determine covariate settings.
# If `conceptsToInclude` or `conceptsToExclude` are provided in the spec and are not empty,
# create specific covariate settings, otherwise use default settings.
if (length(includedCovariateConceptIds) > 0 || length(excludedCovariateConceptIds) > 0) {
  covariateSettings <- FeatureExtraction::createCovariateSettings(
    useDemographicsGender = TRUE, useDemographicsAgeGroup = TRUE, useDemographicsRace = TRUE,
    useDemographicsEthnicity = TRUE, useDemographicsPostObservationTime = TRUE,
    useDemographicsPriorObservationTime = TRUE, useDemographicsIndexYear = TRUE,
    useDemographicsIndexMonth = TRUE, useConditionOccurrenceLongTerm = TRUE,
    useConditionOccurrenceShortTerm = TRUE, useConditionOccurrenceMediumTerm = TRUE,
    useConditionOccurrencePrimaryInpatient = TRUE, useConditionEraLongTerm = TRUE,
    useConditionEraShortTerm = TRUE, useConditionEraMediumTerm = TRUE,
    useDrugExposureLongTerm = TRUE, useDrugExposureShortTerm = TRUE,
    useDrugExposureMediumTerm = TRUE, useDrugEraLongTerm = TRUE,
    useDrugEraShortTerm = TRUE, useDrugEraMediumTerm = TRUE,
    useProcedureOccurrenceLongTerm = TRUE, useProcedureOccurrenceShortTerm = TRUE,
    useProcedureOccurrenceMediumTerm = TRUE, useDeviceExposureLongTerm = TRUE,
    useDeviceExposureShortTerm = TRUE, useDeviceExposureMediumTerm = TRUE,
    useMeasurementLongTerm = TRUE, useMeasurementShortTerm = TRUE,
    useMeasurementMediumTerm = TRUE, useObservationLongTerm = TRUE,
    useObservationShortTerm = TRUE, useObservationMediumTerm = TRUE,
    useCharlsonIndex = TRUE, useDcsi = TRUE, useChads2 = TRUE, useChads2Vasc = TRUE,
    useInteractionCovariateStratos = FALSE,
    excludedCovariateConceptIds = excludedCovariateConceptIds,
    includedCovariateConceptIds = includedCovariateConceptIds,
    addDescendantsToInclude = TRUE,
    addDescendantsToExclude = TRUE,
    endDays = 0, shortTermWindow = 30, mediumTermWindow = 180, longTermWindow = 365
  )
} else {
  # If included/excluded concept lists are empty, use default covariate settings as in template.
  covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
    addDescendantsToExclude = TRUE
  )
}

# Combine outcome cohorts of interest with negative control outcome cohorts.
# This block is moved outside the nested loops as its dependencies (oList, negativeControlOutcomeCohortSet)
# are defined globally and do not change per analysis combination.
outcomeList <- append(
  lapply(seq_len(nrow(oList)), function(i) {
    CohortMethod::createOutcome(
      outcomeId = oList$outcomeCohortId[i],
      outcomeOfInterest = TRUE,
      trueEffectSize = NA, # For true outcomes, trueEffectSize is NA.
      priorOutcomeLookback = 99999 # Template default
    )
  }),
  lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
    CohortMethod::createOutcome(
      outcomeId = i,
      outcomeOfInterest = FALSE,
      trueEffectSize = 1 # For negative controls, trueEffectSize is 1.
    )
  })
)

# Create target-comparator-outcome list.
# This block is also moved outside the nested loops as its dependencies (cmTcList, outcomeList, excludedCovariateConceptIds)
# are constant across all analysis combinations.
targetComparatorOutcomesList <- list()
for (i in seq_len(nrow(cmTcList))) {
  # The template included cmTcList$targetConceptId[i] and cmTcList$comparatorConceptId[i]
  # in excludedCovariateConceptIds, which are not valid columns.
  # The original script correctly used only the general excludedCovariateConceptIds.
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    excludedCovariateConceptIds = excludedCovariateConceptIds # Only use concepts from `covariateSelection.conceptsToExclude`.
  )
}


# Iterate through all analysis setting combinations
cmAnalysisList <- list()
analysisId <- 1

# Main loop for generating CohortMethod analyses specifications
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Determine PS adjustment method based on configuration
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Template default
          stratificationColumns = c() # Template default
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Template default
          baseSelection = psCfg$params$baseSelection
        )
      }

      # getDbCohortMethodDataArgs: arguments for fetching data
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Template default
        studyStartDate = studyStartDate, # From current loop iteration
        studyEndDate = studyEndDate, # From current loop iteration
        maxCohortSize = analysisSpecValues$getDbCohortMethodDataArgs$maxCohortSize, # From analysis spec
        covariateSettings = covariateSettings # Defined earlier
      )

      # Parameters for Cyclops::createControl from `propensityScoreAdjustment.createPsArgs.control` in analysis specifications
      psControlSpec <- analysisSpecValues$propensityScoreAdjustment$createPsArgs$control
      psControl <- Cyclops::createControl(
        tolerance = psControlSpec$tolerance,
        cvType = psControlSpec$cvType,
        seed = 1, # Template default
        resetCoefficients = psControlSpec$resetCoefficients,
        cvRepetitions = psControlSpec$cvRepetitions, # From analysis spec
        startingVariance = psControlSpec$startingVariance, # From analysis spec
        noiseLevel = psControlSpec$noiseLevel
      )

      # Parameters for Cyclops::createPrior from `propensityScoreAdjustment.createPsArgs.prior` in analysis specifications
      psPriorSpec <- analysisSpecValues$propensityScoreAdjustment$createPsArgs$prior
      psPrior <- Cyclops::createPrior(
        priorType = psPriorSpec$priorType,
        exclude = c(0), # Template default
        useCrossValidation = psPriorSpec$useCrossValidation
      )

      # createPsArgs: arguments for creating propensity scores
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = analysisSpecValues$propensityScoreAdjustment$createPsArgs$maxCohortSizeForFitting, # From analysis spec
        errorOnHighCorrelation = analysisSpecValues$propensityScoreAdjustment$createPsArgs$errorOnHighCorrelation, # From analysis spec
        stopOnError = FALSE, # Template default
        estimator = "att", # Template default
        prior = psPrior, # From analysis spec
        control = psControl # From analysis spec
      )

      # computeSharedCovariateBalanceArgs: arguments for computing balance across cohorts
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Template default
        covariateFilter = NULL # Template default
      )
      # computeCovariateBalanceArgs: arguments for computing balance after PS adjustment
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Template default
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Template default
      )

      # Parameters for Cyclops::createControl from `fitOutcomeModelArgs.control` in analysis specifications
      omControlSpec <- analysisSpecValues$fitOutcomeModelArgs$control
      omControl <- Cyclops::createControl(
        cvType = omControlSpec$cvType,
        seed = 1, # Template default
        resetCoefficients = omControlSpec$resetCoefficients,
        startingVariance = omControlSpec$startingVariance, # From analysis spec
        tolerance = omControlSpec$tolerance,
        cvRepetitions = omControlSpec$cvRepetitions, # From analysis spec
        noiseLevel = omControlSpec$noiseLevel
      )

      # Parameters for Cyclops::createPrior from `fitOutcomeModelArgs.prior` in analysis specifications
      omPriorSpec <- analysisSpecValues$fitOutcomeModelArgs$prior
      omPrior <- Cyclops::createPrior(
        priorType = omPriorSpec$priorType,
        useCrossValidation = omPriorSpec$useCrossValidation
      )

      # fitOutcomeModelArgs: arguments for fitting the outcome model
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = analysisSpecValues$fitOutcomeModelArgs$modelType, # From analysis spec
        stratified = analysisSpecValues$fitOutcomeModelArgs$stratified, # From analysis spec
        useCovariates = analysisSpecValues$fitOutcomeModelArgs$useCovariates, # From analysis spec
        inversePtWeighting = analysisSpecValues$fitOutcomeModelArgs$inversePtWeighting, # From analysis spec
        prior = omPrior, # From analysis spec
        control = omControl # From analysis spec
      )

      # createStudyPopArgs: arguments for defining the study population
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = analysisSpecValues$createStudyPopArgs$restrictToCommonPeriod, # From analysis spec
        firstExposureOnly = analysisSpecValues$createStudyPopArgs$firstExposureOnly, # From analysis spec
        washoutPeriod = analysisSpecValues$createStudyPopArgs$washoutPeriod, # From analysis spec
        removeDuplicateSubjects = analysisSpecValues$createStudyPopArgs$removeDuplicateSubjects, # From analysis spec
        censorAtNewRiskWindow = analysisSpecValues$createStudyPopArgs$censorAtNewRiskWindow, # From analysis spec
        removeSubjectsWithPriorOutcome = analysisSpecValues$createStudyPopArgs$removeSubjectsWithPriorOutcome, # From analysis spec
        priorOutcomeLookback = analysisSpecValues$createStudyPopArgs$priorOutcomeLookBack, # From analysis spec (corrected to 'priorOutcomeLookback' as per function arg name)
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From current loop iteration
        startAnchor = timeAtRisks$startAnchor[t], # From current loop iteration
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From current loop iteration
        endAnchor = timeAtRisks$endAnchor[t], # From current loop iteration
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From current loop iteration
        maxDaysAtRisk = 99999 # Template default
      )

      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        # FIX: Corrected sprintf arguments to match the format string and template.
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          studyStartDate, # Argument for first %s
          studyEndDate,   # Argument for second %s
          timeAtRisks$label[t], # Argument for third %s
          psCfg$label # Argument for fourth %s
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
  # targetComparatorOutcomesList is now correctly defined outside the loops.
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Template default
  refitPsForEveryOutcome = FALSE, # Template default
  refitPsForEveryStudyPopulation = FALSE, # Template default
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Template default
)

# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file
# Preserving the original script's dynamic output path, which is more robust.
outputDir <- file.path("inst", analysisSpecValues$name)
if (!dir.exists(outputDir)) {
  dir.create(outputDir, recursive = TRUE)
}
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path(outputDir, paste0(analysisSpecValues$name, "AnalysisSpecification.json"))
)