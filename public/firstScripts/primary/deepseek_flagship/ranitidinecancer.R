################################################################################
# CreateStrategusAnalysisSpecification.R
# Analysis specification for ranitidinecancer study
# 
# This script creates a Strategus analysis specification for a comparative
# effectiveness study of ranitidine vs comparator on cancer outcomes.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
################################################################################
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

# Create data frames to hold the cohorts for analysis ---------------------------
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

# No specific covariate concepts to include or exclude based on specifications
# excludedCovariateConcepts remains empty as per analysis specifications

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

# Study periods - using null dates from specifications (no restriction)
studyPeriods <- tibble(
  studyStartDate = "", # No restriction as per specifications
  studyEndDate   = ""  # No restriction as per specifications
)

# Time-at-risks (TARs) - using exact settings from analysis specifications
timeAtRisks <- tibble(
  label = "365-99999_cohort_start",
  riskWindowStart  = 365,
  startAnchor = "cohort start", # As specified
  riskWindowEnd  = 99999,
  endAnchor = "cohort start", # As specified
  minDaysAtRisk = 1
) 

# Propensity Score settings - match on PS (only method specified)
matchOnPsArgsList <- tibble(
  label = "1:1_matching_0.2_caliper",
  maxRatio  = 1, # 1:1 matching as specified
  caliper = 0.2, # Caliper 0.2 as specified
  caliperScale  = "standardized logit" # Exact scale from specifications
) 

# Build PS configuration list (only matching method specified)
psConfigList <- list()

# Add matching configuration (only method in specifications)
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
      }

      # Default covariate settings (no specific concepts to include/exclude per specifications)
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
            priorOutcomeLookback = 365 # As specified in analysis specifications
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
          excludedCovariateConceptIds = c() # No specific exclusions per specifications
        )
      }

      # GetDbCohortMethodDataArgs - using exact settings from specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE, # As specified
        studyStartDate = if(studyStartDate == "") NULL else studyStartDate,
        studyEndDate = if(studyEndDate == "") NULL else studyEndDate,
        maxCohortSize = 0, # No limit as specified
        covariateSettings = covariateSettings
      )

      # CreatePsArgs - using exact settings from specifications
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # As specified
        errorOnHighCorrelation = TRUE, # As specified
        stopOnError = FALSE, # Allow Strategus to complete all operations
        estimator = "att",
        prior = Cyclops::createPrior( # Using laplace prior with cross-validation as specified
          priorType = "laplace", 
          exclude = c(0), 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl( # Using exact control parameters from specifications
          noiseLevel = "silent", # As specified
          cvType = "auto", # As specified  
          seed = 1, 
          resetCoefficients = TRUE, # As specified
          tolerance = 2e-07, # As specified
          cvRepetitions = 10, # As specified (note: template had 1 but specs say 10)
          startingVariance = 0.01, # As specified
          fold = 10 # As specified
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

      # FitOutcomeModelArgs - using exact settings from specifications
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # As specified
        stratified = FALSE, # As specified (not stratified)
        useCovariates = FALSE, # As specified
        inversePtWeighting = FALSE, # As specified
        prior = Cyclops::createPrior( # Using laplace prior with cross-validation as specified
          priorType = "laplace", 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl( # Using exact control parameters from specifications
          cvType = "auto", # As specified
          seed = 1, 
          resetCoefficients = TRUE, # As specified
          startingVariance = 0.01, # As specified
          tolerance = 2e-07, # As specified
          cvRepetitions = 10, # As specified (note: template had 1 but specs say 10)
          noiseLevel = "quiet", # As specified
          fold = 10 # As specified
        )
      )
      
      # CreateStudyPopArgs - using exact settings from specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # As specified
        firstExposureOnly = FALSE, # As specified
        washoutPeriod = 365, # As specified
        removeDuplicateSubjects = "keep all", # As specified
        censorAtNewRiskWindow = FALSE, # As specified
        removeSubjectsWithPriorOutcome = TRUE, # As specified
        priorOutcomeLookback = 365, # As specified
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
          ifelse(studyStartDate == "", "no_restriction", studyStartDate),
          ifelse(studyEndDate == "", "no_restriction", studyEndDate),
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

# Save the analysis specifications
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "ranitidinecancer", "ranitidinecancerAnalysisSpecification.json")
)