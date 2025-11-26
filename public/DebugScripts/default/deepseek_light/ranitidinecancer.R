################################################################################
# CreateStrategusAnalysisSpecification.R
# Analysis specification for ranitidinecancer study
# 
# This script creates a Strategus analysis specification for a comparative 
# effectiveness study comparing ranitidine to other H2 blockers and their
# association with cancer outcomes.
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

# Re-number cohorts for internal use in Strategus
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

# Create data frames to hold the cohorts for analysis --------------------------
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

# CohortDiagnoticsModule Settings ----------------------------------------------
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

# Study periods - empty strings as specified
studyPeriods <- tibble(
  studyStartDate = c(""), # Empty string as specified
  studyEndDate   = c("")  # Empty string as specified
)

# Time-at-risks (TARs) - using exact specifications with 4 different risk windows
timeAtRisks <- tibble(
  label = c("Tar1", "Tar2", "Tar3", "Tar4"),
  riskWindowStart  = c(1, 365, 1, 365),
  startAnchor = c("cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd  = c(99999, 99999, 0, 0),
  endAnchor = c("cohort start", "cohort start", "cohort end", "cohort end")
) 

# Propensity Score settings - match on PS (2 configurations)
matchOnPsArgsList <- tibble(
  label = c("Match1", "Match10"),
  maxRatio  = c(1, 10),
  caliper = c(0.2, 0.2),
  caliperScale  = c("standardized logit", "standardized logit")
) 

# Propensity Score settings - stratify by PS (1 configuration)
stratifyByPsArgsList <- tibble(
  label = c("Stratify10"),
  numberOfStrata  = c(10),
  baseSelection = c("all")
) 

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Add match configurations
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

# Add stratification configurations
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

# Add unadjusted analysis (no PS adjustment)
psConfigList[[length(psConfigList) + 1]] <- list(
  method = "none",
  label  = "Unadjusted",
  params = list()
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
      
      # Configure PS adjustment method based on specification
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
      } else if (psCfg$method == "none") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- NULL
      }

      # Default covariate settings - no specific inclusions/exclusions per specifications
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
            priorOutcomeLookback = 99999  # As specified in createStudyPopArgs
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

      # GetDbCohortMethodDataArgs - using specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,  # As specified - 0 means no limit
        covariateSettings = covariateSettings
      )

      # CreatePsArgs - using exact specifications
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # As specified
        errorOnHighCorrelation = TRUE,  # As specified
        stopOnError = FALSE,  # Setting to FALSE to allow Strategus complete all CM operations
        estimator = "att",
        prior = Cyclops::createPrior(  # Using laplace prior as specified
          priorType = "laplace", 
          exclude = c(0), 
          useCrossValidation = TRUE  # As specified
        ),
        control = Cyclops::createControl(  # Using control parameters as specified
          noiseLevel = "silent",  # As specified
          cvType = "auto",  # As specified  
          seed = 1, 
          resetCoefficients = TRUE,  # As specified
          tolerance = 2e-07,  # As specified
          cvRepetitions = 10,  # As specified
          fold = 10,  # As specified
          startingVariance = 0.01  # As specified
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

      # Fix: For unadjusted analyses, set stratified = FALSE in outcome model
      if (psCfg$method == "none") {
        stratifiedForOutcomeModel <- FALSE
      } else {
        stratifiedForOutcomeModel <- TRUE
      }
      
      # FitOutcomeModelArgs - using exact specifications
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",  # As specified
        stratified = stratifiedForOutcomeModel,  # Fix: Only stratify when using PS adjustment
        useCovariates = FALSE,  # As specified
        inversePtWeighting = FALSE,  # As specified
        prior = Cyclops::createPrior(  # Using laplace prior as specified
          priorType = "laplace", 
          useCrossValidation = TRUE  # As specified
        ),
        control = Cyclops::createControl(  # Using control parameters as specified
          cvType = "auto",  # As specified
          seed = 1, 
          resetCoefficients = TRUE,  # As specified
          startingVariance = 0.01,  # As specified
          tolerance = 2e-07,  # As specified
          cvRepetitions = 10,  # As specified
          fold = 10,  # As specified
          noiseLevel = "quiet"  # As specified
        )
      )
      
      # CreateStudyPopArgs - using exact specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,  # As specified
        firstExposureOnly = FALSE,  # As specified
        washoutPeriod = 0,  # As specified
        removeDuplicateSubjects = "keep first",  # Fix: Changed from "keep all" to match template
        censorAtNewRiskWindow = TRUE,  # Fix: Changed from FALSE to match template
        removeSubjectsWithPriorOutcome = TRUE,  # As specified
        priorOutcomeLookback = 99999,  # As specified
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,  # Fix: Removed timeAtRisks$minDaysAtRisk[t] as it's not in template
        maxDaysAtRisk = 99999
      )

      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(studyStartDate == "", "All", studyStartDate),
          ifelse(studyEndDate == "", "All", studyEndDate),
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

# Save the analysis specifications
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "ranitidinecancer", "ranitidinecancerAnalysisSpecification.json")
)