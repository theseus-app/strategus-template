################################################################################
# CreateStrategusAnalysisSpecification.R
# Analysis specification script for covid19famotidine study
# Generated based on provided analysis specifications
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from Atlas
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions - Using exact IDs from analysis specifications
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to use sequential IDs starting from 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

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
  mutate(cohortId = row_number() + 100) %>% # Start negative control IDs from 101
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames for outcomes, target/comparator pairs, and excluded concepts
# Outcomes configuration
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Outcome cohort has ID 3
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Standard clean window for outcomes

# Target and Comparator pairs for CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,          # Target cohort ID (target1)
  targetCohortName = "target1",
  comparatorCohortId = 2,      # Comparator cohort ID (comparator1)
  comparatorCohortName = "comparator1"
)

# Excluded covariate concepts - none specified in analysis specifications
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Included covariate concepts - none specified in analysis specifications
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

# Study periods from analysis specifications
studyPeriods <- tibble(
  studyStartDate = c("20200201"),  # From getDbCohortMethodDataArgs.studyPeriods[0].studyStartDate
  studyEndDate   = c("20200530")   # From getDbCohortMethodDataArgs.studyPeriods[0].studyEndDate
)

# Time-at-risks (TARs) from createStudyPopArgs.timeAtRisks
timeAtRisks <- tibble(
  label = c("TAR_1-30"),                      # Descriptive label
  riskWindowStart  = c(1),                    # From timeAtRisks[0].riskWindowStart
  startAnchor = c("cohort start"),            # From timeAtRisks[0].startAnchor
  riskWindowEnd  = c(30),                     # From timeAtRisks[0].riskWindowEnd
  endAnchor = c("cohort start"),              # From timeAtRisks[0].endAnchor
  minDaysAtRisk = c(1)                        # From timeAtRisks[0].minDaysAtRisk
)

# Propensity Score settings - match on PS (second PS setting)
matchOnPsArgsList <- tibble(
  label = c("Matching"),                      # Descriptive label for matching PS adjustment
  maxRatio  = c(1),                           # From propensityScoreAdjustment.psSettings[1].matchOnPsArgs.maxRatio
  caliper = c(0.2),                           # From propensityScoreAdjustment.psSettings[1].matchOnPsArgs.caliper
  caliperScale  = c("standardized logit")     # From propensityScoreAdjustment.psSettings[1].matchOnPsArgs.caliperScale
)

# Propensity Score settings - stratify by PS (first PS setting)
stratifyByPsArgsList <- tibble(
  label = c("Stratification"),                # Descriptive label for stratification PS adjustment
  numberOfStrata  = c(5),                     # From propensityScoreAdjustment.psSettings[0].stratifyByPsArgs.numberOfStrata
  baseSelection = c("all")                    # From propensityScoreAdjustment.psSettings[0].stratifyByPsArgs.baseSelection
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Add stratification PS configuration
psConfigList[[length(psConfigList) + 1]] <- list(
  method = "stratify",
  label  = stratifyByPsArgsList$label[1],
  params = list(
    numberOfStrata = stratifyByPsArgsList$numberOfStrata[1],
    baseSelection  = stratifyByPsArgsList$baseSelection[1]
  )
)

# Add matching PS configuration
psConfigList[[length(psConfigList) + 1]] <- list(
  method = "match",
  label  = matchOnPsArgsList$label[1],
  params = list(
    maxRatio     = matchOnPsArgsList$maxRatio[1],
    caliper      = matchOnPsArgsList$caliper[1],
    caliperScale = matchOnPsArgsList$caliperScale[1]
  )
)

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

      # Covariate settings - using default settings
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
      
      # Create target-comparator-outcomes list
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Create getDbCohortMethodDataArgs with settings from analysis specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,      # From getDbCohortMethodDataArgs.restrictToCommonPeriod
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,                   # From getDbCohortMethodDataArgs.maxCohortSize (0 = no limit)
        firstExposureOnly = TRUE,            # From getDbCohortMethodDataArgs.firstExposureOnly
        washoutPeriod = 0,                   # From getDbCohortMethodDataArgs.washoutPeriod
        removeDuplicateSubjects = "remove all", # From getDbCohortMethodDataArgs.removeDuplicateSubjects
        covariateSettings = covariateSettings
      )

      # Create propensity score arguments with settings from analysis specifications
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,    # From propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE,       # From propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE,                 # Allow Strategus to complete all CM operations
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",             # From propensityScoreAdjustment.createPsArgs.prior.priorType
          exclude = c(0),
          useCrossValidation = TRUE          # From propensityScoreAdjustment.createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",             # From propensityScoreAdjustment.createPsArgs.control.noiseLevel
          cvType = "auto",                   # From propensityScoreAdjustment.createPsArgs.control.cvType
          seed = 1,
          resetCoefficients = TRUE,          # From propensityScoreAdjustment.createPsArgs.control.resetCoefficients
          tolerance = 2e-07,                 # From propensityScoreAdjustment.createPsArgs.control.tolerance
          cvRepetitions = 10,                # From propensityScoreAdjustment.createPsArgs.control.cvRepetitions
          startingVariance = 0.01,           # From propensityScoreAdjustment.createPsArgs.control.startingVariance
          fold = 10                          # From propensityScoreAdjustment.createPsArgs.control.fold
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

      # Outcome model fitting arguments with settings from analysis specifications
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",                   # From fitOutcomeModelArgs.modelType
        stratified = TRUE,                   # From fitOutcomeModelArgs.stratified
        useCovariates = FALSE,               # From fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE,          # From fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior(
          priorType = "laplace",             # From fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE          # From fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          cvType = "auto",                   # From fitOutcomeModelArgs.control.cvType
          seed = 1,
          resetCoefficients = TRUE,          # From fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01,           # From fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07,                 # From fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10,                # From fitOutcomeModelArgs.control.cvRepetitions
          noiseLevel = "quiet",              # From fitOutcomeModelArgs.control.noiseLevel
          fold = 10                          # From fitOutcomeModelArgs.control.fold
        )
      )
      
      # Create study population arguments with settings from analysis specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,      # From createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE,           # From createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0,                   # From createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # From createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE,       # From createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = FALSE, # From createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999,        # From createStudyPopArgs.priorOutcomeLookback
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

# Save analysis specifications to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "covid19famotidine", "covid19famotidineAnalysisSpecification.json")
)