################################################################################
# CreateStrategusAnalysisSpecification.R
# Analysis specification for ranitidinecancer study
# 
# This script creates Strategus analysis specifications based on the provided
# configuration for the ranitidinecancer study.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from Atlas
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

# Negative control outcomes - using exact ID from analysis specifications
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
# Outcomes: outcome1
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

# No specific covariate concepts to include or exclude based on analysis specifications
# excludedCovariateConcepts remains empty as per specifications

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

# Study periods - using null values from specifications (no restriction)
studyPeriods <- tibble(
  studyStartDate = character(), # Empty as per specifications
  studyEndDate   = character()  # Empty as per specifications
)

# Time-at-risks (TARs) - using exact values from analysis specifications
timeAtRisks <- tibble(
  label = "365-9999_cohort_start",
  riskWindowStart  = 365,
  startAnchor = "cohort start", 
  riskWindowEnd  = 9999,
  endAnchor = "cohort start",
  minDaysAtRisk = 1
) 

# Propensity Score settings - match on PS (using exact values from specifications)
matchOnPsArgsList <- tibble(
  label = "match_1_0.2_std_logit",
  maxRatio  = 1,
  caliper = 0.2,
  caliperScale  = "standardized logit"
) 

# Build PS configuration list
psConfigList <- list()

# Add match on PS configuration
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

# Iterate through all analysis setting combinations
cmAnalysisList <- list()
analysisId <- 1

# Since studyPeriods is empty, we'll create one analysis with no date restrictions
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

    # Create covariate settings - no specific inclusions/exclusions based on specifications
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
          priorOutcomeLookback = 365  # Using 365 days as per specifications
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
        excludedCovariateConceptIds = c()  # No specific exclusions based on specifications
      )
    }

    # GetDbCohortMethodDataArgs - using exact values from specifications
    getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
      restrictToCommonPeriod = FALSE,  # Using FALSE as per specifications
      studyStartDate = "",  # Empty as per specifications
      studyEndDate = "",    # Empty as per specifications
      maxCohortSize = 0,    # Using 0 as per specifications
      covariateSettings = covariateSettings
    )

    # CreatePsArgs - using exact values from specifications
    createPsArgs = CohortMethod::createCreatePsArgs(
      maxCohortSizeForFitting = 250000,  # Using 250000 as per specifications
      errorOnHighCorrelation = TRUE,     # Using TRUE as per specifications
      stopOnError = FALSE,               # Setting to FALSE to allow Strategus complete all CM operations
      estimator = "att",
      prior = Cyclops::createPrior(      # Using laplace prior with cross-validation as per specifications
        priorType = "laplace", 
        exclude = c(0), 
        useCrossValidation = TRUE
      ),
      control = Cyclops::createControl(  # Using control parameters from specifications
        noiseLevel = "silent", 
        cvType = "auto", 
        seed = 1, 
        resetCoefficients = TRUE, 
        tolerance = 2e-07, 
        cvRepetitions = 10,              # Using 10 as per specifications
        startingVariance = 0.01
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

    # FitOutcomeModelArgs - using exact values from specifications
    fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
      modelType = "cox",                 # Using cox as per specifications
      stratified = FALSE,                # Using FALSE as per specifications
      useCovariates = FALSE,             # Using FALSE as per specifications
      inversePtWeighting = FALSE,        # Using FALSE as per specifications
      prior = Cyclops::createPrior(      # Using laplace prior with cross-validation as per specifications
        priorType = "laplace", 
        useCrossValidation = TRUE
      ),
      control = Cyclops::createControl(  # Using control parameters from specifications
        cvType = "auto", 
        seed = 1, 
        resetCoefficients = TRUE,
        startingVariance = 0.01, 
        tolerance = 2e-07, 
        cvRepetitions = 10,              # Using 10 as per specifications
        noiseLevel = "quiet"             # Using quiet as per specifications
      )
    )
    
    # CreateStudyPopArgs - using exact values from specifications
    createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
      restrictToCommonPeriod = FALSE,        # Using FALSE as per specifications
      firstExposureOnly = FALSE,             # Using FALSE as per specifications
      washoutPeriod = 365,                   # Using 365 as per specifications
      removeDuplicateSubjects = "keep all",  # Using "keep all" as per specifications
      censorAtNewRiskWindow = FALSE,         # Using FALSE as per specifications
      removeSubjectsWithPriorOutcome = TRUE, # Using TRUE as per specifications
      priorOutcomeLookback = 365,            # Using 365 as per specifications
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
        "Study: no_date_restriction; TAR: %s; PS: %s",
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

# Save the analysis specifications
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "ranitidinecancer", "ranitidinecancerAnalysisSpecification.json")
)