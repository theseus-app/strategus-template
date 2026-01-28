################################################################################
# CreateStrategusAnalysisSpecification.R for glp1radepression study
# 
# This script creates analysis specifications for the Strategus framework
# based on the provided analysis settings.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------

# Get the list of cohorts from Atlas
# Note: Update baseUrl to your Atlas instance URL
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions from analysis specifications
# Target cohort ID: 1794126, Comparator cohort ID: 1794132, Outcome cohort ID: 1794131
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to standard Strategus convention (1, 2, 3...)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes from concept set ID 1888110
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
  mutate(cohortId = row_number() + 100) %>% # Target/comparator IDs: 1, 2, 3; negative controls: 101, 102, 103...
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames for analysis configurations --------------------------------

# Outcomes: Single outcome cohort from analysis specifications
# Using cleanWindow = 365 days as specified in analysis specs (removeSubjectsWithPriorOutcome = TRUE with priorOutcomeLookBack = 99999)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for CohortMethod analysis
# Note: Exact names from analysis specifications are used
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate settings: Based on analysis specifications
# Empty data frames since conceptsToInclude and conceptsToExclude are empty in specs
# Note: Target and comparator drug concepts will be excluded automatically in CohortMethod
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(),
  conceptName = character()
)

# No included covariates specified in analysis specs
# includedCovariateConcepts <- data.frame(
#   conceptId = numeric(),
#   conceptName = character()
# )

# Study periods from analysis specifications
# Single study period: 2013-01-01 to 2020-12-31
studyPeriods <- tibble(
  studyStartDate = c("20130101"),  # From getDbCohortMethodDataArgs.studyPeriods[0].studyStartDate
  studyEndDate   = c("20201231")   # From getDbCohortMethodDataArgs.studyPeriods[0].studyEndDate
)

# Time-at-risks (TARs) from analysis specifications
# Single TAR: riskWindowStart = 1, riskWindowEnd = 730 (both anchored at cohort start)
timeAtRisks <- tibble(
  label = c("1 to 730 days from cohort start"),
  riskWindowStart  = c(1),          # From createStudyPopArgs.timeAtRisks[0].riskWindowStart
  startAnchor = c("cohort start"),  # From createStudyPopArgs.timeAtRisks[0].startAnchor
  riskWindowEnd  = c(730),          # From createStudyPopArgs.timeAtRisks[0].riskWindowEnd
  endAnchor = c("cohort start")     # From createStudyPopArgs.timeAtRisks[0].endAnchor
)

# Propensity Score settings from analysis specifications
# Only matchOnPsArgs is specified (stratifyByPsArgs is null)
matchOnPsArgsList <- tibble(
  label = c("1:1 matching with 0.05 caliper"),
  maxRatio  = c(1),                   # From propensityScoreAdjustment.psSettings[0].matchOnPsArgs.maxRatio
  caliper = c(0.05),                  # From propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliper
  caliperScale  = c("propensity score") # From propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliperScale
)

# No stratification by PS in analysis specifications
# stratifyByPsArgsList <- tibble()

# Build PS configuration list
psConfigList <- list()

# Add matchOnPs configurations if specified
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

# Iterate through all analysis setting combinations
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]
  
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
      
      # Covariate settings - default with exclusion of target/comparator concepts
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )
      
      # Create outcome list: primary outcome + negative controls
      outcomeList <- append(
        # Primary outcome from analysis specifications
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999  # From createStudyPopArgs.priorOutcomeLookBack
          )
        }),
        # Negative control outcomes
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )
      
      # Create target-comparator-outcomes combinations
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude target and comparator drug concepts from covariates
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }
      
      # GetDbCohortMethodDataArgs from analysis specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,  # From getDbCohortMethodDataArgs.restrictToCommonPeriod
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,  # From getDbCohortMethodDataArgs.maxCohortSize (0 = no limit)
        firstExposureOnly = FALSE,  # From getDbCohortMethodDataArgs.firstExposureOnly
        washoutPeriod = 0,  # From getDbCohortMethodDataArgs.washoutPeriod
        removeDuplicateSubjects = "keep all",  # From getDbCohortMethodDataArgs.removeDuplicateSubjects
        covariateSettings = covariateSettings
      )
      
      # CreatePsArgs from analysis specifications
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # From propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE,  # From propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE,  # Setting to FALSE to allow Strategus to complete all CM operations
        estimator = "att",
        prior = Cyclops::createPrior(  # From propensityScoreAdjustment.createPsArgs.prior
          priorType = "laplace",  # From propensityScoreAdjustment.createPsArgs.prior.priorType
          exclude = c(0),
          useCrossValidation = TRUE  # From propensityScoreAdjustment.createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl(  # From propensityScoreAdjustment.createPsArgs.control
          noiseLevel = "silent",  # From propensityScoreAdjustment.createPsArgs.control.noiseLevel
          cvType = "auto",  # From propensityScoreAdjustment.createPsArgs.control.cvType
          seed = 1,
          resetCoefficients = TRUE,  # From propensityScoreAdjustment.createPsArgs.control.resetCoefficients
          tolerance = 2e-7,  # From propensityScoreAdjustment.createPsArgs.control.tolerance
          cvRepetitions = 10,  # From propensityScoreAdjustment.createPsArgs.control.cvRepetitions
          startingVariance = 0.01,  # From propensityScoreAdjustment.createPsArgs.control.startingVariance
          fold = 10  # From propensityScoreAdjustment.createPsArgs.control.fold
        )
      )
      
      # Covariate balance computation arguments
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )
      
      # FitOutcomeModelArgs from analysis specifications
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",  # From fitOutcomeModelArgs.modelType
        stratified = TRUE,  # From fitOutcomeModelArgs.stratified
        useCovariates = FALSE,  # From fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE,  # From fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior(  # From fitOutcomeModelArgs.prior
          priorType = "laplace",  # From fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE  # From fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl(  # From fitOutcomeModelArgs.control
          cvType = "auto",  # From fitOutcomeModelArgs.control.cvType
          seed = 1,
          resetCoefficients = TRUE,  # From fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01,  # From fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-7,  # From fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10,  # From fitOutcomeModelArgs.control.cvRepetitions
          noiseLevel = "quiet",  # From fitOutcomeModelArgs.control.noiseLevel
          fold = 10  # From fitOutcomeModelArgs.control.fold
        )
      )
      
      # CreateStudyPopArgs from analysis specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,  # From createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE,  # From createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0,  # From createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all",  # From createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE,  # From createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE,  # From createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999,  # From createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,  # From createStudyPopArgs.timeAtRisks[0].minDaysAtRisk
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
  file.path("inst", "glp1radepression", "glp1radepressionAnalysisSpecification.json")
)