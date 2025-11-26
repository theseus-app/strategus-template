################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates analysis specifications for the COVID-19 PPI and H2RA study
# using the OHDSI Strategus package. The specifications are based on the provided
# JSON configuration and follow the Strategus template structure.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from the provided specifications
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions based on the analysis specifications
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for consistent internal use
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes from the specified concept set
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

# Check for duplicate cohort IDs to prevent conflicts
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts used in each analysis -----------------
# Outcome definitions
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

# No specific covariates to exclude based on specifications
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
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

# Study period based on specifications: January 1, 2020 to May 15, 2020
studyPeriods <- tibble(
  studyStartDate = c("20200101"), # From getDbCohortMethodDataArgs.studyPeriods[0].studyStartDate
  studyEndDate   = c("20200515")  # From getDbCohortMethodDataArgs.studyPeriods[0].studyEndDate
)

# Time-at-risk settings based on specifications: risk window from day 1 to 99999 after cohort start
timeAtRisks <- tibble(
  label = c("Day 1 to end of data"),
  riskWindowStart  = c(1),           # From createStudyPopArgs.timeAtRisks[0].riskWindowStart
  startAnchor = c("cohort start"),   # From createStudyPopArgs.timeAtRisks[0].startAnchor
  riskWindowEnd  = c(99999),         # From createStudyPopArgs.timeAtRisks[0].riskWindowEnd
  endAnchor = c("cohort start")      # From createStudyPopArgs.timeAtRisks[0].endAnchor
) 

# Propensity Score settings - stratify by PS (no matching specified)
stratifyByPsArgsList <- tibble(
  label = c("Stratify by PS - 5 strata"),
  numberOfStrata  = c(5),            # From propensityScoreAdjustment.psSettings[0].stratifyByPsArgs.numberOfStrata
  baseSelection = c("all")           # From propensityScoreAdjustment.psSettings[0].stratifyByPsArgs.baseSelection
) 

# Build PS configuration list
psConfigList <- list()

# Convert stratify by PS settings to configuration
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
      
      if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings using default configuration
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

      # GetDbCohortMethodDataArgs based on specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,   # From getDbCohortMethodDataArgs.restrictToCommonPeriod
        studyStartDate = studyStartDate,  # From getDbCohortMethodDataArgs.studyPeriods[0].studyStartDate
        studyEndDate = studyEndDate,      # From getDbCohortMethodDataArgs.studyPeriods[0].studyEndDate
        maxCohortSize = 0,                # From getDbCohortMethodDataArgs.maxCohortSize
        covariateSettings = covariateSettings
      )

      # CreatePsArgs based on propensity score adjustment specifications
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # From propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE,     # From propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE,               # Setting to FALSE to allow Strategus complete all CM operations
        estimator = "att",
        prior = Cyclops::createPrior(      # From propensityScoreAdjustment.createPsArgs.prior
          priorType = "laplace",           # From propensityScoreAdjustment.createPsArgs.prior.priorType
          exclude = c(0), 
          useCrossValidation = TRUE        # From propensityScoreAdjustment.createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl(  # From propensityScoreAdjustment.createPsArgs.control
          noiseLevel = "silent",           # From propensityScoreAdjustment.createPsArgs.control.noiseLevel
          cvType = "auto",                 # From propensityScoreAdjustment.createPsArgs.control.cvType
          seed = 1, 
          resetCoefficients = TRUE,        # From propensityScoreAdjustment.createPsArgs.control.resetCoefficients
          tolerance = 2e-07,               # From propensityScoreAdjustment.createPsArgs.control.tolerance
          cvRepetitions = 10,              # From propensityScoreAdjustment.createPsArgs.control.cvRepetitions
          startingVariance = 0.01          # From propensityScoreAdjustment.createPsArgs.control.startingVariance
        )
      )

      # Covariate balance computation settings
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # FitOutcomeModelArgs based on specifications
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",                 # From fitOutcomeModelArgs.modelType
        stratified = TRUE,                 # From fitOutcomeModelArgs.stratified
        useCovariates = FALSE,             # From fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE,        # From fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior(      # From fitOutcomeModelArgs.prior
          priorType = "laplace",           # From fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE        # From fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl(  # From fitOutcomeModelArgs.control
          cvType = "auto",                 # From fitOutcomeModelArgs.control.cvType
          seed = 1, 
          resetCoefficients = TRUE,        # From fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01,         # From fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07,               # From fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10,              # From fitOutcomeModelArgs.control.cvRepetitions
          noiseLevel = "quiet"             # From fitOutcomeModelArgs.control.noiseLevel
        )
      )
      
      # CreateStudyPopArgs based on specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,            # From createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = TRUE,                  # From createStudyPopArgs.firstExposureOnly
        washoutPeriod = 365,                       # From createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all",      # From createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE,             # From createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE,     # From createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999,              # From createStudyPopArgs.priorOutcomeLookback
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,                         # From createStudyPopArgs.timeAtRisks[0].minDaysAtRisk
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
  file.path("inst", "covid19ppiandh2ra", "covid19ppiandh2raAnalysisSpecification.json")
)