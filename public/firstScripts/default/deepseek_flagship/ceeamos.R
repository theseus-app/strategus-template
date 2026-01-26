################################################################################
# CreateStrategusAnalysisSpecification.R for study: ceeamos
# 
# This script creates analysis specifications for the Strategus framework
# using parameters from the provided Analysis Specifications JSON.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# ##############################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from ATLAS
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions from Analysis Specifications
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to sequential IDs for internal processing
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes from Analysis Specifications
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,  # From negativeControlConceptSet.id
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
  mutate(cohortId = row_number() + 100) %>% # Start negative controls at ID 101+
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames for analysis configuration --------------------------------

# Outcomes: from Analysis Specifications outcomeCohort array
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%  # Outcome cohort from re-numbered IDs
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)  # Default clean window for outcomes

# Target and Comparator for CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,  # Re-numbered target ID
  targetCohortName = "target1",
  comparatorCohortId = 2,  # Re-numbered comparator ID
  comparatorCohortName = "comparator1"
)

# Excluded covariate concepts - from covariateSelection.conceptsToExclude
# Note: Analysis Specifications has empty arrays, so creating empty data frame
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# Included covariate concepts - from covariateSelection.conceptsToInclude
# Note: Analysis Specifications has empty arrays, so commented out
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# Study periods from getDbCohortMethodDataArgs.studyPeriods
# Empty strings indicate no restriction
studyPeriods <- tibble(
  studyStartDate = "",  # From studyStartDate in studyPeriods array
  studyEndDate   = ""   # From studyEndDate in studyPeriods array
)

# Time-at-risks (TARs) from createStudyPopArgs.timeAtRisks
timeAtRisks <- tibble(
  label = c("TAR1", "TAR2"),  # Labels for the two TARs specified
  riskWindowStart  = c(1, 1),  # From riskWindowStart in each TAR
  startAnchor = c("cohort start", "cohort start"),  # From startAnchor
  riskWindowEnd  = c(0, 99999),  # From riskWindowEnd in each TAR
  endAnchor = c("cohort end", "cohort start"),  # From endAnchor
  minDaysAtRisk = c(1, 1)  # From minDaysAtRisk in each TAR
) 

# Propensity Score settings - match on PS from propensityScoreAdjustment.psSettings
matchOnPsArgsList <- tibble(
  label = "PS Match",  # Label for this PS configuration
  maxRatio  = 10,  # From matchOnPsArgs.maxRatio
  caliper = 0.2,  # From matchOnPsArgs.caliper
  caliperScale  = "standardized logit"  # From matchOnPsArgs.caliperScale
) 

# Propensity Score settings - stratify by PS
# Not used in this analysis (stratifyByPsArgs is null)
# stratifyByPsArgsList <- tibble(
#   label = c(),
#   numberOfStrata  = c(),
#   baseSelection = c(),
# ) 

# Build PS configuration list
psConfigList <- list()

# Add match on PS configuration if specified
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

# Add stratify by PS configuration if specified (not used in this analysis)
# if (exists("stratifyByPsArgsList") && nrow(stratifyByPsArgsList) > 0) {
#   for (i in seq_len(nrow(stratifyByPsArgsList))) {
#     psConfigList[[length(psConfigList) + 1]] <- list(
#       method = "stratify",
#       label  = stratifyByPsArgsList$label[i],
#       params = list(
#         numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
#         baseSelection  = stratifyByPsArgsList$baseSelection[i]
#       )
#     )
#   }
# }

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

      # Covariate settings - using default with addDescendantsToExclude = TRUE
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome list including primary and negative control outcomes
      outcomeList <- append(
        # Primary outcomes
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
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # GetDbCohortMethodDataArgs from Analysis Specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,  # From getDbCohortMethodDataArgs.restrictToCommonPeriod
        studyStartDate = ifelse(studyStartDate == "", "", studyStartDate),
        studyEndDate = ifelse(studyEndDate == "", "", studyEndDate),
        maxCohortSize = 0,  # From getDbCohortMethodDataArgs.maxCohortSize
        covariateSettings = covariateSettings
      )

      # CreatePsArgs from propensityScoreAdjustment.createPsArgs
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # From createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE,  # From createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE,  # Allow Strategus to complete all CM operations
        estimator = "att",
        prior = Cyclops::createPrior(  # From createPsArgs.prior
          priorType = "laplace", 
          exclude = c(0), 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(  # From createPsArgs.control
          noiseLevel = "silent",  # From control.noiseLevel
          cvType = "auto",  # From control.cvType
          seed = 1, 
          resetCoefficients = TRUE,  # From control.resetCoefficients
          tolerance = 2e-07,  # From control.tolerance
          cvRepetitions = 10,  # From control.cvRepetitions
          fold = 10,  # From control.fold
          startingVariance = 0.01  # From control.startingVariance
        )
      )

      # Covariate balance arguments
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # FitOutcomeModelArgs from Analysis Specifications
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",  # From fitOutcomeModelArgs.modelType
        stratified = TRUE,  # From fitOutcomeModelArgs.stratified
        useCovariates = FALSE,  # From fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE,  # From fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior(  # From fitOutcomeModelArgs.prior
          priorType = "laplace", 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(  # From fitOutcomeModelArgs.control
          cvType = "auto", 
          seed = 1, 
          resetCoefficients = TRUE,  # From control.resetCoefficients
          startingVariance = 0.01,  # From control.startingVariance
          tolerance = 2e-07,  # From control.tolerance
          cvRepetitions = 10,  # From control.cvRepetitions
          fold = 10,  # From control.fold
          noiseLevel = "quiet"  # From control.noiseLevel
        )
      )
      
      # CreateStudyPopArgs from Analysis Specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,  # From createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE,  # From createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0,  # From createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all",  # From createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE,  # From createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE,  # From createStudyPopArgs.removeSubjectsWithPriorOutcome
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
          ifelse(studyStartDate == "", "NoStart", studyStartDate),
          ifelse(studyEndDate == "", "NoEnd", studyEndDate),
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
  file.path("inst", "ceeamos", "ceeamosAnalysisSpecification.json")
)