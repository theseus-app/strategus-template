################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates analysis specifications for the uveitissafety study
# using the OHDSI Strategus package.
# 
# The analysis includes:
# - Cohort generation for target, comparator, and outcome cohorts
# - Negative control outcomes for empirical calibration
# - CohortMethod analysis with propensity score matching
# - Cox proportional hazards outcome models
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

# Create data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: using the re-numbered outcome cohort
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

# For the CohortMethod analysis we'll exclude the target and comparator drugs
# Note: No specific concepts to exclude were provided in specifications
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# No concepts to include were specified - using default covariate settings
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

# Study periods - using specifications (no restrictions on study period)
studyPeriods <- tibble(
  studyStartDate = c(), # No start date restriction
  studyEndDate   = c()  # No end date restriction
)

# Time-at-risks (TARs) - using exact settings from specifications
timeAtRisks <- tibble(
  label = c("Primary TAR"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
) 

# Propensity Score settings - match on PS (using specifications)
matchOnPsArgsList <- tibble(
  label = c("PS Matching"),
  maxRatio  = c(10),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
) 

# No stratification by PS specified
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c()
) 

# Build a single PS configuration list
psConfigList <- list()

# If a data frame for "match on PS" exists and has rows, convert each row to a config
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

# If a data frame for "stratify by PS" exists and has rows, convert each row to a config
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

# Create target comparator outcomes list outside the loop to fix the error
targetComparatorOutcomesList <- list()

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

      # Covariate settings - using default settings as no specific concepts to include/exclude were provided
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
            priorOutcomeLookback = 99999
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
      
      # Create target comparator outcomes list
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # GetDbCohortMethodDataArgs - using specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # No maximum cohort size restriction
        covariateSettings = covariateSettings
      )

      # CreatePsArgs - using exact specifications for propensity score model
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace", 
          exclude = c(0), 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", 
          cvType = "auto", 
          seed = 1, 
          resetCoefficients = TRUE, 
          tolerance = 2e-07, 
          cvRepetitions = 10, # Using 10 repetitions as specified
          startingVariance = 0.01,
          fold = 10 # Using fold = 10 as specified
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

      # FitOutcomeModelArgs - using exact specifications for Cox model
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = TRUE,
        useCovariates = FALSE,
        inversePtWeighting = FALSE,
        prior = Cyclops::createPrior(
          priorType = "laplace", 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          cvType = "auto", 
          seed = 1, 
          resetCoefficients = TRUE,
          startingVariance = 0.01, 
          tolerance = 2e-07, 
          cvRepetitions = 10, # Using 10 repetitions as specified
          noiseLevel = "quiet",
          fold = 10 # Using fold = 10 as specified
        )
      )
      
      # CreateStudyPopArgs - using exact specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE,
        firstExposureOnly = TRUE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = TRUE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(is.null(studyStartDate), "Unrestricted", studyStartDate),
          ifelse(is.null(studyEndDate), "Unrestricted", studyEndDate),
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
  file.path("inst", "uveitissafety", "uveitissafetyAnalysisSpecification.json")
)