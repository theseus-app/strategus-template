################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates analysis specifications for the ticagrelorclopidogrel study
# using the OHDSI Strategus package.
# 
# The analysis includes:
# - Cohort generation for target, comparator, and outcome cohorts
# - Negative control outcomes
# - Cohort diagnostics
# - Cohort method analyses with multiple propensity score adjustment strategies
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

# Re-number cohorts for internal use (maintains original Atlas IDs in metadata)
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

# No specific concepts to include or exclude based on analysis specifications
# excludedCovariateConcepts <- data.frame(
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
  studyStartDate = c("20111101", "20130301"), # From getDbCohortMethodDataArgs.studyPeriods
  studyEndDate   = c("20190331", "20161231")
)

# Time-at-risks (TARs) from createStudyPopArgs.timeAtRisks
timeAtRisks <- tibble(
  label = c("TAR1-365", "TAR1-1825", "TAR1-cohort_end", "TAR29-365", "TAR29-1825", "TAR29-cohort_end"),
  riskWindowStart  = c(1, 1, 1, 29, 29, 29),
  startAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd  = c(365, 1825, 0, 365, 1825, 0),
  endAnchor = c("cohort start", "cohort start", "cohort end", "cohort start", "cohort start", "cohort end"),
  minDaysAtRisk = c(1, 1, 1, 1, 1, 1)
) 

# Propensity Score settings - match on PS (from propensityScoreAdjustment.psSettings)
matchOnPsArgsList <- tibble(
  label = c("1:1 matching", "1:10 matching"),
  maxRatio  = c(1, 10),
  caliper = c(0.2, 0.2),
  caliperScale  = c("standardized logit", "standardized logit")
) 

# Propensity Score settings - stratify by PS (from propensityScoreAdjustment.psSettings)
stratifyByPsArgsList <- tibble(
  label = c("PS stratification"),
  numberOfStrata  = c(10),
  baseSelection = c("all"),
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

      # Default covariate settings (no specific inclusion/exclusion per specifications)
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
          excludedCovariateConceptIds = c()  # No specific exclusions per specifications
        )
      }

      # GetDbCohortMethodDataArgs from analysis specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,  # From getDbCohortMethodDataArgs.restrictToCommonPeriod
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,  # From getDbCohortMethodDataArgs.maxCohortSize
        firstExposureOnly = FALSE,  # From getDbCohortMethodDataArgs.firstExposureOnly
        washoutPeriod = 0,  # From getDbCohortMethodDataArgs.washoutPeriod
        removeDuplicateSubjects = "keep first",  # From getDbCohortMethodDataArgs.removeDuplicateSubjects
        covariateSettings = covariateSettings
      )

      # CreatePsArgs from propensityScoreAdjustment.createPsArgs
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # From createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE,  # From createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",  # From createPsArgs.prior.priorType
          exclude = c(0), 
          useCrossValidation = TRUE  # From createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",  # From createPsArgs.control.noiseLevel
          cvType = "auto",  # From createPsArgs.control.cvType
          seed = 1, 
          resetCoefficients = TRUE,  # From createPsArgs.control.resetCoefficients
          tolerance = 2e-07,  # From createPsArgs.control.tolerance
          cvRepetitions = 10,  # From createPsArgs.control.cvRepetitions
          startingVariance = 0.01,  # From createPsArgs.control.startingVariance
          fold = 10  # From createPsArgs.control.fold
        )
      )

      # Covariate balance computation
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # FitOutcomeModelArgs from fitOutcomeModelArgs
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",  # From fitOutcomeModelArgs.modelType
        stratified = TRUE,  # From fitOutcomeModelArgs.stratified
        useCovariates = FALSE,  # From fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE,  # From fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior(
          priorType = "laplace",  # From fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE  # From fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          cvType = "auto",  # From fitOutcomeModelArgs.control.cvType
          seed = 1, 
          resetCoefficients = TRUE,  # From fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01,  # From fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07,  # From fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10,  # From fitOutcomeModelArgs.control.cvRepetitions
          noiseLevel = "quiet",  # From fitOutcomeModelArgs.control.noiseLevel
          fold = 10  # From fitOutcomeModelArgs.control.fold
        )
      )
      
      # CreateStudyPopArgs from createStudyPopArgs
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,  # From createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE,  # From createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0,  # From createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all",  # From createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE,  # From createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = FALSE,  # From createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999,  # From createStudyPopArgs.priorOutcomeLookback
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
  Strategus::addSharedResources(negativeControlsShared)