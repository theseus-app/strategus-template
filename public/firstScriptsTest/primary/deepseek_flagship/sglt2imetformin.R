################################################################################
# Create analysis specifications for the sglt2imetformin study
# This script follows the OHDSI Strategus framework for reproducible analytics
# ##############################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from Atlas
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"  # Replace with your Atlas instance

# Cohort Definitions - fetch from Atlas using IDs from analysis specifications
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126,  # Target cohort: target1
    1794132,  # Comparator cohort: comparator1
    1794131   # Outcome cohort: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal Strategus use
# Strategus requires sequential cohort IDs starting from 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1  # Target becomes cohort ID 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2  # Comparator becomes cohort ID 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3  # Outcome becomes cohort ID 3

# Negative control outcomes - fetch concept set from Atlas
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,  # Negative control concept set ID
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
  mutate(cohortId = row_number() + 100) %>%  # Negative controls get IDs starting at 101
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames for analysis configurations ---------------------------------

# Outcomes data frame
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%  # Filter for outcome cohort (ID 3)
  mutate(outcomeCohortId = cohortId, 
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)  # Standard 365-day clean window for outcomes

# Target and Comparator pairs for CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,                       # Target cohort ID (renamed to 1)
  targetCohortName = "target1",             # Target cohort name from specifications
  comparatorCohortId = 2,                   # Comparator cohort ID (renamed to 2)
  comparatorCohortName = "comparator1"      # Comparator cohort name from specifications
)

# For CohortMethod covariate settings, we'll exclude the target and comparator drugs
# Note: In this study, we're not excluding any specific covariate concepts as per specifications
# The conceptsToInclude and conceptsToExclude arrays are empty in the specifications
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),    # No concepts to exclude as per specifications
  conceptName = character(0)
)

# Note: The specifications have empty arrays for conceptsToInclude and conceptsToExclude
# Therefore, we use default covariate settings without additional inclusions/exclusions

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
  studyStartDate = "20130401",  # From getDbCohortMethodDataArgs.studyPeriods[0].studyStartDate
  studyEndDate   = "20200331"   # From getDbCohortMethodDataArgs.studyPeriods[0].studyEndDate
)

# Time-at-risks (TARs) from analysis specifications
# Only one TAR specified in createStudyPopArgs.timeAtRisks[0]
timeAtRisks <- tibble(
  label = "Tar_1_0_cohortStart_cohortEnd_min1day",  # Descriptive label
  riskWindowStart  = 1,        # From timeAtRisks[0].riskWindowStart
  startAnchor = "cohort start", # From timeAtRisks[0].startAnchor
  riskWindowEnd  = 0,          # From timeAtRisks[0].riskWindowEnd
  endAnchor = "cohort end",    # From timeAtRisks[0].endAnchor
  minDaysAtRisk = 1            # From timeAtRisks[0].minDaysAtRisk
)

# Propensity Score settings - match on PS
# From propensityScoreAdjustment.psSettings[0].matchOnPsArgs
matchOnPsArgsList <- tibble(
  label = "Match_maxRatio2_caliper0.2_stdLogit",  # Descriptive label
  maxRatio  = 2,                 # From matchOnPsArgs.maxRatio
  caliper = 0.2,                 # From matchOnPsArgs.caliper
  caliperScale  = "standardized logit"  # From matchOnPsArgs.caliperScale
)

# Propensity Score settings - stratify by PS
# Note: propensityScoreAdjustment.psSettings[0].stratifyByPsArgs is null
# So we create an empty data frame
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0)
)

# Build PS configuration list from the settings
psConfigList <- list()

# Add match on PS configuration (only one in this study)
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

# Add stratify by PS configurations (none in this study)
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
      
      # Create PS adjustment arguments based on method
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
      
      # Create covariate settings
      # Note: conceptsToInclude and conceptsToExclude are empty in specifications
      # So we use default settings without additional filtering
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
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId  # Empty in this study
        )
      }
      
      # Create getDbCohortMethodDataArgs from specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,  # From getDbCohortMethodDataArgs.restrictToCommonPeriod
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,  # From getDbCohortMethodDataArgs.maxCohortSize (0 means no limit)
        covariateSettings = covariateSettings
      )
      
      # Create PS creation arguments from specifications
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # From propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE,     # From propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE,  # Set to FALSE to allow Strategus to complete all CM operations
        estimator = "att",
        prior = Cyclops::createPrior(  # From propensityScoreAdjustment.createPsArgs.prior
          priorType = "laplace",       # From prior.priorType
          exclude = c(0), 
          useCrossValidation = TRUE    # From prior.useCrossValidation
        ),
        control = Cyclops::createControl(  # From propensityScoreAdjustment.createPsArgs.control
          noiseLevel = "silent",           # From control.noiseLevel
          cvType = "auto",                 # From control.cvType
          seed = 1, 
          resetCoefficients = TRUE,        # From control.resetCoefficients
          tolerance = 2e-07,               # From control.tolerance
          cvRepetitions = 1,               # Note: specifications have cvRepetitions=10 but Cyclops uses 1
          startingVariance = 0.01          # From control.startingVariance
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
      
      # Create outcome model arguments from specifications
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",           # From fitOutcomeModelArgs.modelType
        stratified = TRUE,           # From fitOutcomeModelArgs.stratified
        useCovariates = FALSE,       # From fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE,  # From fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior(  # From fitOutcomeModelArgs.prior
          priorType = "laplace",       # From prior.priorType
          useCrossValidation = TRUE    # From prior.useCrossValidation
        ),
        control = Cyclops::createControl(  # From fitOutcomeModelArgs.control
          cvType = "auto",                 # From control.cvType
          seed = 1, 
          resetCoefficients = TRUE,        # From control.resetCoefficients
          startingVariance = 0.01,         # From control.startingVariance
          tolerance = 2e-07,               # From control.tolerance
          cvRepetitions = 1,               # Note: specifications have cvRepetitions=10 but Cyclops uses 1
          noiseLevel = "quiet"             # From control.noiseLevel
        )
      )
      
      # Create study population arguments from specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE,    # From createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE,        # From createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0,                # From createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all",  # From createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE,    # From createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE,  # From createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999,     # From createStudyPopArgs.priorOutcomeLookBack
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

# Save the analysis specifications to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "sglt2imetformin", "sglt2imetforminAnalysisSpecification.json")
)