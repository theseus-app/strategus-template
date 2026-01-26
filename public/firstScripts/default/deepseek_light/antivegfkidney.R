################################################################################
# CreateStrategusAnalysisSpecification.R
# Analysis specification for antivegfkidney study
# 
# This script creates Strategus analysis specifications for the antivegfkidney
# study using the OHDSI Strategus package.
# 
# Settings are applied according to the provided analysis specifications.
# ##############################################################################
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

# Negative control outcomes - using concept set ID 1888110 from specifications
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

# Create data frames to hold the cohorts for analysis --------------------------
# Outcomes: outcome1 (cohort ID 3)
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

# No specific concepts to exclude based on analysis specifications
# excludedCovariateConcepts is left empty as per specifications
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# No specific concepts to include based on analysis specifications
# includedCovariateConcepts is not defined as per specifications

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

# Study periods - left empty as per analysis specifications (no date restrictions)
studyPeriods <- tibble(
  studyStartDate = character(), # Empty string as per specifications
  studyEndDate   = character()  # Empty string as per specifications
)

# Time-at-risks (TARs) for the outcomes - using exact values from analysis specifications
timeAtRisks <- tibble(
  label = c("TAR1", "TAR2"),
  riskWindowStart  = c(1, 1),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd  = c(0, 99999),
  endAnchor = c("cohort end", "cohort start"),
  minDaysAtRisk = c(1, 1)
) 

# Propensity Score settings - match on PS (only match method specified)
matchOnPsArgsList <- tibble(
  label = c("MatchOnPS"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
) 

# Propensity Score settings - stratify by PS (not used, set to NULL as per specifications)
stratifyByPsArgsList <- NULL

# Build a single PS configuration list
psConfigList <- list()

# Add match on PS configuration (only method specified in analysis specifications)
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

# Note: Since studyPeriods is empty (no date restrictions), we create one iteration
# with empty study dates as per analysis specifications
studyStartDate <- ""
studyEndDate <- ""

for (t in seq_len(nrow(timeAtRisks))) {
  for (p in seq_along(psConfigList)) {
    psCfg <- psConfigList[[p]]
    
    # Create PS adjustment arguments based on configuration
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
    
    # Create covariate settings - using default settings as no specific inclusions/exclusions
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
          priorOutcomeLookback = 99999  # From analysis specifications
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
        excludedCovariateConceptIds = excludedCovariateConcepts$conceptId  # Empty as per specifications
      )
    }
    
    # Create getDbCohortMethodDataArgs using settings from analysis specifications
    getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
      restrictToCommonPeriod = TRUE,  # From analysis specifications
      studyStartDate = studyStartDate,
      studyEndDate = studyEndDate,
      maxCohortSize = 0,  # From analysis specifications (0 = no limit)
      covariateSettings = covariateSettings,
      firstExposureOnly = FALSE,  # From analysis specifications
      washoutPeriod = 0,  # From analysis specifications
      removeDuplicateSubjects = "keep all"  # From analysis specifications
    )
    
    # Create createPsArgs using settings from analysis specifications
    createPsArgs = CohortMethod::createCreatePsArgs(
      maxCohortSizeForFitting = 250000,  # From analysis specifications
      errorOnHighCorrelation = TRUE,  # From analysis specifications
      stopOnError = FALSE,  # Setting to FALSE to allow Strategus complete all CM operations
      estimator = "att",
      prior = Cyclops::createPrior(
        priorType = "laplace",  # From analysis specifications
        exclude = c(0),
        useCrossValidation = TRUE  # From analysis specifications
      ),
      control = Cyclops::createControl(
        noiseLevel = "silent",  # From analysis specifications
        cvType = "auto",  # From analysis specifications
        seed = 1,
        resetCoefficients = TRUE,  # From analysis specifications
        tolerance = 2e-07,  # From analysis specifications
        cvRepetitions = 10,  # From analysis specifications
        fold = 10,  # From analysis specifications
        startingVariance = 0.01  # From analysis specifications
      )
    )
    
    # Create covariate balance computation arguments
    computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
      maxCohortSize = 250000,
      covariateFilter = NULL
    )
    computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
      maxCohortSize = 250000,
      covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
    )
    
    # Create fitOutcomeModelArgs using settings from analysis specifications
    fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
      modelType = "cox",  # From analysis specifications
      stratified = FALSE,  # From analysis specifications
      useCovariates = FALSE,  # From analysis specifications
      inversePtWeighting = FALSE,  # From analysis specifications
      prior = Cyclops::createPrior(
        priorType = "laplace",  # From analysis specifications
        useCrossValidation = TRUE  # From analysis specifications
      ),
      control = Cyclops::createControl(
        cvType = "auto",  # From analysis specifications
        seed = 1,
        resetCoefficients = TRUE,  # From analysis specifications
        startingVariance = 0.01,  # From analysis specifications
        tolerance = 2e-07,  # From analysis specifications
        cvRepetitions = 10,  # From analysis specifications
        fold = 10,  # From analysis specifications
        noiseLevel = "quiet"  # From analysis specifications
      )
    )
    
    # Create createStudyPopArgs using settings from analysis specifications
    createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
      restrictToCommonPeriod = FALSE,  # From analysis specifications
      firstExposureOnly = FALSE,  # From analysis specifications
      washoutPeriod = 0,  # From analysis specifications
      removeDuplicateSubjects = "keep all",  # From analysis specifications
      censorAtNewRiskWindow = FALSE,  # From analysis specifications
      removeSubjectsWithPriorOutcome = TRUE,  # From analysis specifications
      priorOutcomeLookback = 99999,  # From analysis specifications
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
        ifelse(studyStartDate == "", "NoDateRestriction", studyStartDate),
        ifelse(studyEndDate == "", "NoDateRestriction", studyEndDate),
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

# Create CohortMethod module specifications
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