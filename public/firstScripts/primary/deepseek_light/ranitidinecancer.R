################################################################################
# CreateStrategusAnalysisSpecification.R
# Analysis specification for ranitidinecancer study
# 
# This script creates Strategus analysis specifications based on the provided
# settings. It includes modules for cohort generation, cohort diagnostics,
# and cohort method analysis.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
################################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions - using exact IDs from analysis specifications
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal use in Strategus
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes - using exact concept set ID from analysis specifications
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
  mutate(cohortId = row_number() + 100) %>% # target/comparator cohort ids start with 1, 2, 3... negativeControl -> 101, 102, 103...
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: 
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis 
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# For the CohortMethod analysis we'll need to exclude the drugs of interest in this
# study. Since conceptsToInclude and conceptsToExclude are empty in the specifications,
# we create empty data frames.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# Optional: If you want to define covariates to include instead of including them all
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

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

# CohortDiagnosticsModule Settings ---------------------------------------------
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

# Study periods - using settings from getDbCohortMethodDataArgs
# Since studyStartDate and studyEndDate are null in specifications, we create empty vectors
studyPeriods <- tibble(
  studyStartDate = character(), #YYYYMMDD
  studyEndDate   = character()  #YYYYMMDD
)

# Time-at-risks (TARs) for the outcomes of interest in your study
# Using exact settings from createStudyPopArgs.timeAtRisks
timeAtRisks <- tibble(
  label = "TAR1",
  riskWindowStart  = 365,
  startAnchor = "cohort start",
  riskWindowEnd  = 99999,
  endAnchor = "cohort start",
  minDaysAtRisk = 1
) 

# Propensity Score settings - match on PS
# Using exact settings from propensityScoreAdjustment.psSettings.matchOnPsArgs
matchOnPsArgsList <- tibble(
  label = "PSMatch1",
  maxRatio  = 1,
  caliper = 0.2,
  caliperScale  = "standardized logit"
) 

# Propensity Score settings - stratify by PS
# Since stratifyByPsArgs is null in specifications, we create empty data frame
stratifyByPsArgsList <- tibble(
  label = character(),
  numberOfStrata  = integer(),
  baseSelection = character()
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

      # Create covariate settings - using default settings since conceptsToInclude/Exclude are empty
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome list including both primary outcomes and negative controls
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 365  # Using priorOutcomeLookBack from createStudyPopArgs
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
      
      # Create target comparator outcomes list
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # GetDbCohortMethodDataArgs - using settings from getDbCohortMethodDataArgs
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,  # Using restrictToCommonPeriod from createStudyPopArgs
        studyStartDate = ifelse(length(studyStartDate) > 0, studyStartDate, ""),
        studyEndDate = ifelse(length(studyEndDate) > 0, studyEndDate, ""),
        maxCohortSize = 0,  # Using maxCohortSize from getDbCohortMethodDataArgs
        covariateSettings = covariateSettings
      )

      # CreatePsArgs - using settings from propensityScoreAdjustment.createPsArgs
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace", 
          exclude = c(0), 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", 
          cvType = "auto", 
          seed = 1, 
          resetCoefficients = TRUE, 
          tolerance = 2e-07, 
          cvRepetitions = 10,  # Using cvRepetitions from propensityScoreAdjustment.createPsArgs.control
          startingVariance = 0.01,
          fold = 10  # Using fold from propensityScoreAdjustment.createPsArgs.control
        )
      )

      # Compute covariate balance arguments
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # FitOutcomeModelArgs - using settings from fitOutcomeModelArgs
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = FALSE,  # Using stratified from fitOutcomeModelArgs
        useCovariates = FALSE,
        inversePtWeighting = FALSE,
        prior = Cyclops::createPrior(
          priorType = "laplace", 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          cvType = "auto", 
          seed = 1, 
          resetCoefficients = TRUE,
          startingVariance = 0.01, 
          tolerance = 2e-07, 
          cvRepetitions = 10,  # Using cvRepetitions from fitOutcomeModelArgs.control
          noiseLevel = "quiet",
          fold = 10  # Using fold from fitOutcomeModelArgs.control
        )
      )
      
      # CreateStudyPopArgs - using settings from createStudyPopArgs
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 365,
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
          ifelse(length(studyStartDate) > 0, studyStartDate, "All"),
          ifelse(length(studyEndDate) > 0, studyEndDate, "All"),
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

# If no study periods were defined (empty studyPeriods), create a single analysis
if (length(cmAnalysisList) == 0) {
  # Create default analysis with no study period restrictions
  psCfg <- psConfigList[[1]]
  
  matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
    maxRatio = psCfg$params$maxRatio,
    caliper = psCfg$params$caliper,
    caliperScale = psCfg$params$caliperScale,
    allowReverseMatch = FALSE,
    stratificationColumns = c()
  )
  
  covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
    addDescendantsToExclude = TRUE
  )
  
  outcomeList <- append(
    lapply(seq_len(nrow(oList)), function(i) {
      CohortMethod::createOutcome(
        outcomeId = oList$outcomeCohortId[i],
        outcomeOfInterest = TRUE,
        trueEffectSize = NA,
        priorOutcomeLookback = 365
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
      excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
    )
  }
  
  getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
    restrictToCommonPeriod = FALSE,
    studyStartDate = "",
    studyEndDate = "",
    maxCohortSize = 0,
    covariateSettings = covariateSettings
  )
  
  createPsArgs = CohortMethod::createCreatePsArgs(
    maxCohortSizeForFitting = 250000,
    errorOnHighCorrelation = TRUE,
    stopOnError = FALSE,
    estimator = "att",
    prior = Cyclops::createPrior(
      priorType = "laplace", 
      exclude = c(0), 
      useCrossValidation = TRUE
    ),
    control = Cyclops::createControl(
      noiseLevel = "silent", 
      cvType = "auto", 
      seed = 1, 
      resetCoefficients = TRUE, 
      tolerance = 2e-07, 
      cvRepetitions = 10,
      startingVariance = 0.01,
      fold = 10
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
    modelType = "cox",
    stratified = FALSE,
    useCovariates = FALSE,
    inversePtWeighting = FALSE,
    prior = Cyclops::createPrior(
      priorType = "laplace", 
      useCrossValidation = TRUE
    ),
    control = Cyclops::createControl(
      cvType = "auto", 
      seed = 1, 
      resetCoefficients = TRUE,
      startingVariance = 0.01, 
      tolerance = 2e-07, 
      cvRepetitions = 10,
      noiseLevel = "quiet",
      fold = 10
    )
  )
  
  createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
    restrictToCommonPeriod = FALSE,
    firstExposureOnly = FALSE,
    washoutPeriod = 365,
    removeDuplicateSubjects = "keep all",
    censorAtNewRiskWindow = FALSE,
    removeSubjectsWithPriorOutcome = TRUE,
    priorOutcomeLookback = 365,
    riskWindowStart = timeAtRisks$riskWindowStart[1],
    startAnchor = timeAtRisks$startAnchor[1],
    riskWindowEnd = timeAtRisks$riskWindowEnd[1],
    endAnchor = timeAtRisks$endAnchor[1],
    minDaysAtRisk = timeAtRisks$minDaysAtRisk[1],
    maxDaysAtRisk = 99999
  )
  
  cmAnalysisList[[1]] <- CohortMethod::createCmAnalysis(
    analysisId = 1,
    description = sprintf(
      "Study: All-All; TAR: %s; PS: %s",
      timeAtRisks$label[1],
      psCfg$label
    ),
    getDbCohortMethodDataArgs = getDbCohortMethodDataArgs,
    createStudyPopArgs = createStudyPopArgs,
    createPsArgs = createPsArgs,
    matchOnPsArgs = matchOnPsArgs,
    stratifyByPsArgs = NULL,
    computeSharedCovariateBalanceArgs = computeSharedCovariateBalanceArgs,
    computeCovariateBalanceArgs = computeCovariateBalanceArgs,
    fitOutcomeModelArgs = fitOutcomeModelArgs
  )
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

# Save the analysis specifications to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "ranitidinecancer", "ranitidinecancerAnalysisSpecification.json")
)