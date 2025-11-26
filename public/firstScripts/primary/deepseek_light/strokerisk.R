################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates analysis specifications for the strokerisk study using
# the OHDSI Strategus package. The specifications include:
# - Cohort definitions for target, comparator, and outcome cohorts
# - Negative control outcomes
# - CohortMethod analysis settings including propensity score adjustment and outcome modeling
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

# Re-number cohorts for internal use
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

# No specific concepts to exclude based on analysis specifications
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# No specific concepts to include based on analysis specifications
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

# Study periods from analysis specifications
studyPeriods <- tibble(
  studyStartDate = c("20010101"), # From getDbCohortMethodDataArgs.studyPeriods
  studyEndDate   = c("20171231")  # From getDbCohortMethodDataArgs.studyPeriods
)

# Time-at-risks (TARs) from analysis specifications
timeAtRisks <- tibble(
  label = c("Main TAR"),
  riskWindowStart  = c(1),           # From createStudyPopArgs.timeAtRisks
  startAnchor = c("cohort start"),   # From createStudyPopArgs.timeAtRisks
  riskWindowEnd  = c(0),             # From createStudyPopArgs.timeAtRisks
  endAnchor = c("cohort end"),       # From createStudyPopArgs.timeAtRisks
  minDaysAtRisk = c(1)               # From createStudyPopArgs.timeAtRisks
) 

# Propensity Score settings - match on PS (only method specified in analysis)
matchOnPsArgsList <- tibble(
  label = c("Match on PS"),
  maxRatio  = c(10),                           # From propensityScoreAdjustment.psSettings.matchOnPsArgs
  caliper = c(0.2),                            # From propensityScoreAdjustment.psSettings.matchOnPsArgs
  caliperScale  = c("standardized logit")      # From propensityScoreAdjustment.psSettings.matchOnPsArgs
) 

# No stratification by PS specified in analysis
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c()
) 

# Build a single PS configuration list
psConfigList <- list()

# Add match on PS configurations
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

# Add stratify by PS configurations (none in this analysis)
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

      # Create default covariate settings (no specific inclusion/exclusion specified)
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
            priorOutcomeLookback = 99999  # From createStudyPopArgs.priorOutcomeLookBack
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
      
      # Create target-comparator-outcomes combinations
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # GetDbCohortMethodDataArgs from analysis specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,  # From getDbCohortMethodDataArgs (not specified, using default)
        studyStartDate = studyStartDate, # From getDbCohortMethodDataArgs.studyPeriods
        studyEndDate = studyEndDate,     # From getDbCohortMethodDataArgs.studyPeriods
        maxCohortSize = 0,               # From getDbCohortMethodDataArgs.maxCohortSize
        covariateSettings = covariateSettings
      )

      # CreatePsArgs from analysis specifications
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # From propensityScoreAdjustment.createPsArgs
        errorOnHighCorrelation = TRUE,     # From propensityScoreAdjustment.createPsArgs
        stopOnError = FALSE,               # Setting to FALSE to allow Strategus complete all CM operations
        estimator = "att",
        prior = Cyclops::createPrior(      # From propensityScoreAdjustment.createPsArgs.prior
          priorType = "laplace", 
          exclude = c(0), 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(  # From propensityScoreAdjustment.createPsArgs.control
          noiseLevel = "silent", 
          cvType = "auto", 
          seed = 1, 
          resetCoefficients = TRUE, 
          tolerance = 2e-07, 
          cvRepetitions = 1, 
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

      # FitOutcomeModelArgs from analysis specifications
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",                # From fitOutcomeModelArgs.modelType
        stratified = TRUE,                # From fitOutcomeModelArgs.stratified
        useCovariates = FALSE,            # From fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE,       # From fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior(     # From fitOutcomeModelArgs.prior
          priorType = "laplace", 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl( # From fitOutcomeModelArgs.control
          cvType = "auto", 
          seed = 1, 
          resetCoefficients = TRUE,
          startingVariance = 0.01, 
          tolerance = 2e-07, 
          cvRepetitions = 1, 
          noiseLevel = "quiet"
        )
      )
      
      # CreateStudyPopArgs from analysis specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,           # From createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE,                # From createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0,                        # From createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all",     # From createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE,            # From createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE,    # From createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999,             # From createStudyPopArgs.priorOutcomeLookBack
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

# Save the analysis specifications to JSON
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "strokerisk", "strokeriskAnalysisSpecification.json")
)