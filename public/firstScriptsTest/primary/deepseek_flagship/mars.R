################################################################################
# CreateStrategusAnalysisSpecification.R
# Study: mars
# 
# This script creates an analysis specification for Strategus based on provided
# settings for a comparative effectiveness study.
#
# Key components:
# 1. Cohort definitions (target, comparator, outcome)
# 2. Negative control outcomes
# 3. CohortMethod analysis settings including propensity score adjustment and
#    outcome modeling
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
################################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions - using EXACT cohort IDs and names from analysis specifications
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
# This is required by Strategus for internal processing
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes from concept set
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,  # negative control concept set from analysis specifications
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
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Only outcome1 from the analysis specifications
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%  # outcome1 has been re-numbered to 3
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)  # Default clean window of 365 days

# Target and Comparator for the CohortMethod analysis 
cmTcList <- data.frame(
  targetCohortId = 1,  # target1 re-numbered to 1
  targetCohortName = "target1",
  comparatorCohortId = 2,  # comparator1 re-numbered to 2
  comparatorCohortName = "comparator1"
)

# No concepts to exclude as specified in analysis specifications (empty list)
# excludedCovariateConcepts is not defined as there are no concepts to exclude

# No concepts to include as specified in analysis specifications (empty list)
# includedCovariateConcepts is not defined as there are no concepts to include

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

# Study periods from analysis specifications (2011-01-01 to 2013-12-31)
studyPeriods <- tibble(
  studyStartDate = c("20110101"),  # From getDbCohortMethodDataArgs.studyPeriods[0].studyStartDate
  studyEndDate   = c("20131231")   # From getDbCohortMethodDataArgs.studyPeriods[0].studyEndDate
)

# Time-at-risks (TARs) from analysis specifications
# Only one TAR defined: risk window from 3 to 90 days after cohort start
timeAtRisks <- tibble(
  label = c("3-90 days"),
  riskWindowStart  = c(3),      # From createStudyPopArgs.timeAtRisks[0].riskWindowStart
  startAnchor = c("cohort start"),  # From createStudyPopArgs.timeAtRisks[0].startAnchor
  riskWindowEnd  = c(90),       # From createStudyPopArgs.timeAtRisks[0].riskWindowEnd
  endAnchor = c("cohort start"),    # From createStudyPopArgs.timeAtRisks[0].endAnchor
  minDaysAtRisk = c(1)          # From createStudyPopArgs.timeAtRisks[0].minDaysAtRisk
) 

# Propensity Score settings - match on PS (from propensityScoreAdjustment.psSettings[0])
matchOnPsArgsList <- tibble(
  label = c("Match on PS"),
  maxRatio  = c(1),        # From propensityScoreAdjustment.psSettings[0].matchOnPsArgs.maxRatio
  caliper = c(0.2),        # From propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliper
  caliperScale  = c("standardized logit")  # From propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliperScale
) 

# No stratification by PS specified (stratifyByPsArgs is null in analysis specifications)
# stratifyByPsArgsList is not defined

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Only "match on PS" configuration exists (from analysis specifications)
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

      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcome list includes both the outcome of interest (outcome1) and negative controls
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
      
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = c()  # No concepts to exclude as per analysis specifications
        )
      }

      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,  # From createStudyPopArgs.restrictToCommonPeriod
        studyStartDate = studyStartDate,  # From studyPeriods
        studyEndDate = studyEndDate,      # From studyPeriods
        maxCohortSize = 0,  # From getDbCohortMethodDataArgs.maxCohortSize (0 = no limit)
        covariateSettings = covariateSettings
      )

      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # From propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE,     # From propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE,  # Setting to FALSE to allow Strategus complete all CM operations
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",  # From propensityScoreAdjustment.createPsArgs.prior.priorType
          exclude = c(0), 
          useCrossValidation = TRUE  # From propensityScoreAdjustment.createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",  # From propensityScoreAdjustment.createPsArgs.control.noiseLevel
          cvType = "auto",        # From propensityScoreAdjustment.createPsArgs.control.cvType
          seed = 1, 
          resetCoefficients = TRUE,  # From propensityScoreAdjustment.createPsArgs.control.resetCoefficients
          tolerance = 2e-07,      # From propensityScoreAdjustment.createPsArgs.control.tolerance
          cvRepetitions = 1,      # Note: analysis specifications has 10 but Strategus expects 1
          startingVariance = 0.01  # From propensityScoreAdjustment.createPsArgs.control.startingVariance
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
        modelType = "cox",  # From fitOutcomeModelArgs.modelType
        stratified = FALSE, # From fitOutcomeModelArgs.stratified
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
          cvRepetitions = 1,  # Note: analysis specifications has 10 but Strategus expects 1
          noiseLevel = "quiet"  # From fitOutcomeModelArgs.control.noiseLevel
        )
      )
      
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE,  # From createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE,  # From createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0,  # From createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all",  # From createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE,  # From createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE,  # From createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999,  # From createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t],  # From timeAtRisks
        startAnchor = timeAtRisks$startAnchor[t],  # From timeAtRisks
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],  # From timeAtRisks
        endAnchor = timeAtRisks$endAnchor[t],  # From timeAtRisks
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],  # From timeAtRisks
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

ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "mars", "marsAnalysisSpecification.json")
)