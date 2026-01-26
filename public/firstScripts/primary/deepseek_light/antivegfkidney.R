################################################################################
# CreateStrategusAnalysisSpecification.R
# Analysis specification for antivegfkidney study
# Generated based on provided analysis specifications
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

# Re-number cohorts to sequential IDs starting from 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes - using exact concept set ID from specifications
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
# Outcomes: using outcome1 from specifications
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

# For the CohortMethod analysis, we'll exclude the target and comparator drugs
# Note: Since concept IDs are not provided in specifications, we leave this empty
# Users should populate this with actual concept IDs if needed
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

# Study periods - using settings from specifications (no restriction)
# Since studyStartDate and studyEndDate are null in specifications, we leave empty
studyPeriods <- tibble(
  studyStartDate = character(), #YYYYMMDD
  studyEndDate   = character()  #YYYYMMDD
)

# Time-at-risks (TARs) for the outcomes - using exact settings from specifications
# Only one TAR specified: riskWindowStart=1, startAnchor="cohort start", 
# riskWindowEnd=0, endAnchor="cohort end", minDaysAtRisk=1
timeAtRisks <- tibble(
  label = c("TAR1"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
) 

# Propensity Score settings - match on PS (using exact settings from specifications)
# Only one PS method specified: matchOnPs with maxRatio=1, caliper=0.2, caliperScale="standardized logit"
matchOnPsArgsList <- tibble(
  label = c("PS Match"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
) 

# Propensity Score settings - stratify by PS (not used in this specification)
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

      # Covariate settings - using default settings since no specific inclusions/exclusions provided
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome list including both primary outcome and negative controls
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999  # From specifications
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

      # GetDbCohortMethodDataArgs - using settings from specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,  # From specifications
        studyStartDate = ifelse(length(studyStartDate) > 0, studyStartDate, ""),
        studyEndDate = ifelse(length(studyEndDate) > 0, studyEndDate, ""),
        maxCohortSize = 0,  # From specifications (0 means no limit)
        covariateSettings = covariateSettings
      )

      # CreatePsArgs - using exact settings from specifications
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # From specifications
        errorOnHighCorrelation = TRUE,     # From specifications
        stopOnError = FALSE,  # Setting to FALSE to allow Strategus complete all CM operations
        estimator = "att",
        prior = Cyclops::createPrior(  # Using laplace prior with cross-validation as specified
          priorType = "laplace", 
          exclude = c(0), 
          useCrossValidation = TRUE  # From specifications
        ),
        control = Cyclops::createControl(  # Using control settings from specifications
          noiseLevel = "silent",      # From specifications
          cvType = "auto",            # From specifications
          seed = 1, 
          resetCoefficients = TRUE,   # From specifications
          tolerance = 2e-07,          # From specifications
          cvRepetitions = 10,         # From specifications (10 repetitions)
          startingVariance = 0.01     # From specifications
        )
      )

      # Covariate balance computation arguments
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # FitOutcomeModelArgs - using exact settings from specifications
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",            # From specifications
        stratified = FALSE,           # From specifications (not stratified)
        useCovariates = FALSE,        # From specifications
        inversePtWeighting = FALSE,   # From specifications
        prior = Cyclops::createPrior(  # Using laplace prior with cross-validation as specified
          priorType = "laplace", 
          useCrossValidation = TRUE   # From specifications
        ),
        control = Cyclops::createControl(  # Using control settings from specifications
          cvType = "auto",            # From specifications
          seed = 1, 
          resetCoefficients = TRUE,   # From specifications
          startingVariance = 0.01,    # From specifications
          tolerance = 2e-07,          # From specifications
          cvRepetitions = 10,         # From specifications (10 repetitions)
          noiseLevel = "quiet"        # From specifications
        )
      )
      
      # CreateStudyPopArgs - using exact settings from specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,           # From specifications
        firstExposureOnly = TRUE,                 # From specifications
        washoutPeriod = 365,                      # From specifications
        removeDuplicateSubjects = "keep all",     # From specifications
        censorAtNewRiskWindow = FALSE,            # From specifications
        removeSubjectsWithPriorOutcome = TRUE,    # From specifications
        priorOutcomeLookback = 99999,             # From specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,                        # From specifications
        maxDaysAtRisk = 99999
      )

      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(length(studyStartDate) > 0, studyStartDate, "Unrestricted"),
          ifelse(length(studyEndDate) > 0, studyEndDate, "Unrestricted"),
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

# If no study periods were defined (unrestricted analysis), create one analysis
if (length(cmAnalysisList) == 0) {
  # Use the first (and only) TAR and PS configuration
  t <- 1
  p <- 1
  psCfg <- psConfigList[[p]]
  
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
      startingVariance = 0.01
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
      noiseLevel = "quiet"
    )
  )
  
  createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
    restrictToCommonPeriod = FALSE,
    firstExposureOnly = TRUE,
    washoutPeriod = 365,
    removeDuplicateSubjects = "keep all",
    censorAtNewRiskWindow = FALSE,
    removeSubjectsWithPriorOutcome = TRUE,
    priorOutcomeLookback = 99999,
    riskWindowStart = timeAtRisks$riskWindowStart[t],
    startAnchor = timeAtRisks$startAnchor[t],
    riskWindowEnd = timeAtRisks$riskWindowEnd[t],
    endAnchor = timeAtRisks$endAnchor[t],
    minDaysAtRisk = 1,
    maxDaysAtRisk = 99999
  )
  
  cmAnalysisList[[1]] <- CohortMethod::createCmAnalysis(
    analysisId = 1,
    description = sprintf(
      "Study: Unrestricted; TAR: %s; PS: %s",
      timeAtRisks$label[t],
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
  file.path("inst", "antivegfkidney", "antivegfkidneyAnalysisSpecification.json")
)