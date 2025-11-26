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

# Define study parameters directly, derived from the original script's JSON input.
# This aligns with the template's programmatic approach for defining specifications.
studyName <- "semaglutideandnaion"

# Original Cohort IDs and names from the input JSON
# These will be re-numbered internally for the study.
targetOrigId <- 1794126
targetOrigName <- "target1"
comparatorOrigId <- 1794132
comparatorOrigName <- "comparator1"
outcomeOrigId <- 1794131
outcomeOrigName <- "outcome1"

# Negative Control Concept Set ID from the input JSON
negativeControlConceptSetId <- 1888110

# Study periods from the input JSON
studyPeriods <- tibble::tibble(
  studyStartDate = 20171201, # YYYYMMDD
  studyEndDate   = 20231231  # YYYYMMDD
)

# createStudyPopArgs parameters and time-at-risks from the input JSON
createStudyPopArgsParams <- list(
  restrictToCommonPeriod = FALSE,
  firstExposureOnly = FALSE,
  washoutPeriod = 365,
  removeDuplicateSubjects = "keep all",
  censorAtNewRiskWindow = FALSE,
  removeSubjectsWithPriorOutcome = TRUE,
  # Corrected: Parameter name 'priorOutcomeLookBack' from JSON adjusted to 'priorOutcomeLookback'
  # to match CohortMethod::createCreateStudyPopulationArgs argument naming.
  priorOutcomeLookback = 99999,
  timeAtRisks = tibble::tibble(
    riskWindowStart = 1,
    startAnchor = "cohort start",
    riskWindowEnd = 0,
    endAnchor = "cohort end",
    minDaysAtRisk = 1
  )
)

# getDbCohortMethodDataArgs parameters from the input JSON
getDbCohortMethodDataArgsParams <- list(
  maxCohortSize = 0
)

# Propensity Score adjustment settings from the input JSON
psSettingsJson <- list(
  matchOnPsArgs = list( # Values for matching on PS
    maxRatio = 1,
    caliper = 0.2,
    caliperScale = "standardized logit"
  ),
  stratifyByPsArgs = NULL # If stratification was not specified in the JSON
)

# createPsArgs parameters from the input JSON
createPsArgsParams <- list(
  maxCohortSizeForFitting = 250000,
  errorOnHighCorrelation = TRUE,
  prior = list(
    priorType = "laplace",
    useCrossValidation = TRUE
  ),
  control = list(
    tolerance = 2e-7,
    cvType = "auto",
    cvRepetitions = 10,
    noiseLevel = "silent",
    resetCoefficients = TRUE,
    startingVariance = 0.01
    # 'fold' parameter from original JSON removed as it's not a direct argument for Cyclops::createControl
  )
)

# Fit Outcome Model arguments from the input JSON
fitOutcomeModelArgsParams <- list(
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
    cvRepetitions = 10,
    noiseLevel = "quiet",
    resetCoefficients = TRUE,
    startingVariance = 0.01
    # 'fold' parameter from original JSON removed as it's not a direct argument for Cyclops::createControl
  )
)

# Covariate selection from the input JSON (originally empty)
excludedCovariateConcepts <- data.frame(conceptId = numeric(), conceptName = character())
includedCovariateConcepts <- data.frame(conceptId = numeric(), conceptName = character())


# Shared Resources -------------------------------------------------------------
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    targetOrigId,
    comparatorOrigId,
    outcomeOrigId
  ),
  generateStats = TRUE
)

# Re-number cohorts to a standard internal scheme (1=target, 2=comparator, 3=outcome)
cohortDefinitionSet[cohortDefinitionSet$cohortId == targetOrigId,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == comparatorOrigId,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == outcomeOrigId,]$cohortId <- 3

# Update cohort names for clarity
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1,]$cohortName <- paste("Target:", targetOrigName)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 2,]$cohortName <- paste("Comparator:", comparatorOrigName)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 3,]$cohortName <- paste("Outcome:", outcomeOrigName)

# Negative control outcomes
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
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)


# Check for duplicate cohort IDs (original script's correct usage preserved)
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: 
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Value from original script, not derived from JSON.

# Target and Comparator for the CohortMethod analysis 
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = cohortDefinitionSet[cohortDefinitionSet$cohortId == 1,]$cohortName,
  comparatorCohortId = 2,
  comparatorCohortName = cohortDefinitionSet[cohortDefinitionSet$cohortId == 2,]$cohortName
)

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
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

# Time-at-risks (TARs) for the outcomes of interest in your study
timeAtRisks <- createStudyPopArgsParams$timeAtRisks %>%
  mutate(label = "Primary_TAR") # Adding label as per original script's logic

# Propensity Score settings - match on PS
matchOnPsArgsList <- tibble::tibble(
  label = character(),
  maxRatio  = numeric(),
  caliper = numeric(),
  caliperScale  = character()
) 
if (!is.null(psSettingsJson$matchOnPsArgs)) {
  matchOnPsArgsList <- tibble::tibble(
    label = "1:1 Matching on Std. Logit PS", # Label from original script
    maxRatio  = psSettingsJson$matchOnPsArgs$maxRatio,
    caliper = psSettingsJson$matchOnPsArgs$caliper,
    caliperScale  = psSettingsJson$matchOnPsArgs$caliperScale
  )
}

# Propensity Score settings - stratify by PS
stratifyByPsArgsList <- tibble::tibble(
  label = character(),
  numberOfStrata  = numeric(),
  baseSelection = character()
) 
if (!is.null(psSettingsJson$stratifyByPsArgs)) {
  stratifyByPsArgsList <- tibble::tibble(
    label = "Stratification by PS", # Label from original script
    numberOfStrata  = psSettingsJson$stratifyByPsArgs$numberOfStrata,
    baseSelection = psSettingsJson$stratifyByPsArgs$baseSelection
  )
}

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# If a data frame for "match on PS" exists and has rows, convert each row to a config
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
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
if (exists("stratifyByPsArgsList") && nrow(stratifyByPsArgsList) > 0) {
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
          caliper = psCfg$params$caliper, # Corrected missing parenthesis from template
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

      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE,
        excludeCovariateIds = excludedCovariateConcepts$conceptId, # Using identified concepts from original JSON
        includeCovariateIds = includedCovariateConcepts$conceptId  # Using identified concepts from original JSON
      )

      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Corrected: Removed non-existent cmTcList$targetConceptId and cmTcList$comparatorConceptId
          # from the template, as per original script's correct logic.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId 
        )
      }

      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = getDbCohortMethodDataArgsParams$maxCohortSize,
        covariateSettings = covariateSettings
      )

      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = createPsArgsParams$maxCohortSizeForFitting,
        errorOnHighCorrelation = createPsArgsParams$errorOnHighCorrelation,
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = createPsArgsParams$prior$priorType, 
          exclude = c(0), 
          useCrossValidation = createPsArgsParams$prior$useCrossValidation
        ),
        control = Cyclops::createControl(
          noiseLevel = createPsArgsParams$control$noiseLevel, 
          cvType = createPsArgsParams$control$cvType, 
          seed = 1, 
          resetCoefficients = createPsArgsParams$control$resetCoefficients, 
          tolerance = createPsArgsParams$control$tolerance, 
          cvRepetitions = createPsArgsParams$control$cvRepetitions, 
          startingVariance = createPsArgsParams$control$startingVariance
          # 'fold' parameter removed as it's not a direct argument for Cyclops::createControl
        )
      )

      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = fitOutcomeModelArgsParams$modelType,
        stratified = fitOutcomeModelArgsParams$stratified,
        useCovariates = fitOutcomeModelArgsParams$useCovariates,
        inversePtWeighting = fitOutcomeModelArgsParams$inversePtWeighting,
        prior = Cyclops::createPrior(
          priorType = fitOutcomeModelArgsParams$prior$priorType, 
          useCrossValidation = fitOutcomeModelArgsParams$prior$useCrossValidation
        ),
        control = Cyclops::createControl(
          cvType = fitOutcomeModelArgsParams$control$cvType, 
          seed = 1, 
          resetCoefficients = fitOutcomeModelArgsParams$control$resetCoefficients,
          startingVariance = fitOutcomeModelArgsParams$control$startingVariance, 
          tolerance = fitOutcomeModelArgsParams$control$tolerance, 
          cvRepetitions = fitOutcomeModelArgsParams$control$cvRepetitions, 
          noiseLevel = fitOutcomeModelArgsParams$control$noiseLevel
          # 'fold' parameter removed as it's not a direct argument for Cyclops::createControl
        )
      )
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = createStudyPopArgsParams$restrictToCommonPeriod,
        firstExposureOnly = createStudyPopArgsParams$firstExposureOnly,
        washoutPeriod = createStudyPopArgsParams$washoutPeriod,
        removeDuplicateSubjects = createStudyPopArgsParams$removeDuplicateSubjects,
        censorAtNewRiskWindow = createStudyPopArgsParams$censorAtNewRiskWindow,
        removeSubjectsWithPriorOutcome = createStudyPopArgsParams$removeSubjectsWithPriorOutcome,
        priorOutcomeLookback = createStudyPopArgsParams$priorOutcomeLookback,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999
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
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Define output folder and file name based on the study name
outputFolder <- file.path("inst", studyName)
if (!dir.exists(outputFolder)) {
  dir.create(outputFolder, recursive = TRUE)
}
outputFileName <- file.path(outputFolder, paste0(studyName, "AnalysisSpecification.json"))

ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  outputFileName
)