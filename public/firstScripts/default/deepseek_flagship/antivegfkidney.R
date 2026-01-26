################################################################################
# CreateStrategusAnalysisSpecification.R
# Analysis specification for antivegfkidney study using Strategus package
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# ##############################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from Atlas
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal Strategus use
# Strategus expects sequential IDs starting from 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes from concept set
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
  mutate(cohortId = row_number() + 100) %>% # Negative controls start at 101 to avoid conflicts with main cohorts
  select(cohortId, cohortName, outcomeConceptId)

# Verify no duplicate cohort IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames for analysis specifications ------------------------------
# Outcomes of interest
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, 
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default 365-day clean window for outcomes

# Target and Comparator for CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate exclusion concepts (empty as per specifications)
# Note: covariateSelection.conceptsToExclude is empty in specifications
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# Covariate inclusion concepts (empty as per specifications)
# Note: covariateSelection.conceptsToInclude is empty in specifications
includedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
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

# CohortMethodModule Settings --------------------------------------------------
# Study periods (empty strings indicate no date restriction)
studyPeriods <- tibble(
  studyStartDate = "", # Empty string = no restriction (getDbCohortMethodDataArgs.studyPeriods[0].studyStartDate)
  studyEndDate   = ""  # Empty string = no restriction (getDbCohortMethodDataArgs.studyPeriods[0].studyEndDate)
)

# Time-at-risks (TARs) for outcomes as specified in createStudyPopArgs.timeAtRisks
# First TAR: riskWindowStart = 1, riskWindowEnd = 0 (cohort start to cohort end)
# Second TAR: riskWindowStart = 1, riskWindowEnd = 99999 (cohort start to +99999 days)
timeAtRisks <- tibble(
  label = c("cohort_start_to_end", "cohort_start_to_99999"),
  riskWindowStart  = c(1, 1), # createStudyPopArgs.timeAtRisks[0].riskWindowStart
  startAnchor = c("cohort start", "cohort start"), # createStudyPopArgs.timeAtRisks[0].startAnchor
  riskWindowEnd  = c(0, 99999), # createStudyPopArgs.timeAtRisks[0].riskWindowEnd
  endAnchor = c("cohort end", "cohort start"), # createStudyPopArgs.timeAtRisks[0].endAnchor
  minDaysAtRisk = c(1, 1) # createStudyPopArgs.timeAtRisks[0].minDaysAtRisk
)

# Propensity Score settings - only matchOnPsArgs specified
# propensityScoreAdjustment.psSettings[0].matchOnPsArgs
matchOnPsArgsList <- tibble(
  label = c("match_1to1_caliper0.2"),
  maxRatio  = c(1), # propensityScoreAdjustment.psSettings[0].matchOnPsArgs.maxRatio
  caliper = c(0.2), # propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliper
  caliperScale  = c("standardized logit") # propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliperScale
)

# No stratifyByPsArgs specified (propensityScoreAdjustment.psSettings[0].stratifyByPsArgs is null)
# stratifyByPsArgsList remains empty

# Build PS configuration list from specified settings
psConfigList <- list()

# Add matchOnPsArgs configurations
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

# No stratifyByPsArgs configurations to add

# Iterate through all analysis setting combinations
# This creates one analysis per TAR since we have one study period and one PS method
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

      # Covariate settings (default with option to include/exclude specific concepts)
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )
      
      # Apply covariate inclusion/exclusion if specified
      if (nrow(includedCovariateConcepts) > 0) {
        covariateSettings$includedCovariateConceptIds <- includedCovariateConcepts$conceptId
      }
      if (nrow(excludedCovariateConcepts) > 0) {
        covariateSettings$excludedCovariateConceptIds <- excludedCovariateConcepts$conceptId
      }

      # Outcome list combining primary outcomes and negative controls
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999 # createStudyPopArgs.priorOutcomeLookBack
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
      
      # Target-comparator-outcomes combinations
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # getDbCohortMethodDataArgs as per specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # getDbCohortMethodDataArgs.restrictToCommonPeriod
        studyStartDate = studyStartDate, # Empty string = no restriction
        studyEndDate = studyEndDate,     # Empty string = no restriction
        maxCohortSize = 0, # getDbCohortMethodDataArgs.maxCohortSize (0 = no restriction)
        covariateSettings = covariateSettings,
        firstExposureOnly = FALSE, # getDbCohortMethodDataArgs.firstExposureOnly
        washoutPeriod = 0, # getDbCohortMethodDataArgs.washoutPeriod
        removeDuplicateSubjects = "keep all" # getDbCohortMethodDataArgs.removeDuplicateSubjects
      )

      # createPsArgs as per propensityScoreAdjustment.createPsArgs
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Setting to FALSE to allow Strategus to complete all CM operations
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace", # propensityScoreAdjustment.createPsArgs.prior.priorType
          exclude = c(0),
          useCrossValidation = TRUE # propensityScoreAdjustment.createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", # propensityScoreAdjustment.createPsArgs.control.noiseLevel
          cvType = "auto", # propensityScoreAdjustment.createPsArgs.control.cvType
          seed = 1,
          resetCoefficients = TRUE, # propensityScoreAdjustment.createPsArgs.control.resetCoefficients
          tolerance = 2e-07, # propensityScoreAdjustment.createPsArgs.control.tolerance
          cvRepetitions = 10, # propensityScoreAdjustment.createPsArgs.control.cvRepetitions
          fold = 10, # propensityScoreAdjustment.createPsArgs.control.fold
          startingVariance = 0.01 # propensityScoreAdjustment.createPsArgs.control.startingVariance
        )
      )

      # Covariate balance computation settings
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs as per specifications
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # fitOutcomeModelArgs.modelType
        stratified = FALSE, # fitOutcomeModelArgs.stratified
        useCovariates = FALSE, # fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE, # fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior(
          priorType = "laplace", # fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE # fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          cvType = "auto", # fitOutcomeModelArgs.control.cvType
          seed = 1,
          resetCoefficients = TRUE, # fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01, # fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07, # fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10, # fitOutcomeModelArgs.control.cvRepetitions
          fold = 10, # fitOutcomeModelArgs.control.fold
          noiseLevel = "quiet" # fitOutcomeModelArgs.control.noiseLevel
        )
      )
      
      # createStudyPopArgs as per specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE, # createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0, # createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999 # Default maximum
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

# Create CohortMethodModule specifications
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

# Save the specifications to JSON
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "antivegfkidney", "antivegfkidneyAnalysisSpecification.json")
)