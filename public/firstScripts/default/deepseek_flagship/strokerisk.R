################################################################################
# CreateStrategusAnalysisSpecification.R
# Analysis specifications for: strokerisk
# 
# This script creates a Strategus analysis specification for a comparative
# effectiveness study of stroke risk.
# 
# Key settings from the JSON specification:
# - Target cohort ID: 1794126 (target1)
# - Comparator cohort ID: 1794132 (comparator1)
# - Outcome cohort ID: 1794131 (outcome1)
# - Negative control concept set ID: 1888110 (negative)
# - Two study periods: 2001-2017 and 2001-2015
# - Three propensity score adjustment strategies (no adjustment, 1:1 matching, 
#   1:10 matching)
# - Cox proportional hazards outcome model with regularization
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions from JSON specification
# Fetch target, comparator, and outcome cohort definitions
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
# Strategus requires sequential IDs for internal processing
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes from JSON specification (ID: 1888110)
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
  mutate(cohortId = row_number() + 100) %>% # Negative controls start at 101
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no duplicate cohort IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes from JSON specification
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)  # Standard 365-day clean window for outcomes

# Target and Comparator for the CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate exclusion: From JSON specification, no concepts to include or exclude
# Note: covariateSelection in JSON had empty arrays for both include and exclude
# Therefore we create empty data frames
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(0),
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# Included covariate concepts are empty per JSON specification
includedCovariateConcepts <- data.frame(
  conceptId = numeric(0),
  conceptName = character(0)
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

# Study periods from JSON specification
studyPeriods <- tibble(
  studyStartDate = c("20010101", "20010101"),
  studyEndDate   = c("20171231", "20150930")
)

# Time-at-risks (TARs) from JSON specification: 1 day after cohort start to cohort end
timeAtRisks <- tibble(
  label = c("Tar1"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end"),
  minDaysAtRisk = c(1)  # From JSON: minDaysAtRisk = 1
) 

# Propensity Score settings - match on PS from JSON specification
# Three settings: no PS adjustment, 1:1 matching, 1:10 matching
matchOnPsArgsList <- tibble(
  label = c("NoPS", "Match1to1", "Match1to10"),
  maxRatio  = c(NA, 1, 10),  # NA for no matching
  caliper = c(NA, 0.05, 0.2),
  caliperScale  = c(NA, "propensity score", "standardized logit")
) 

# No stratification by PS in this analysis (stratifyByPsArgs is null in all JSON settings)
# Create empty data frame for consistency
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = numeric(0),
  baseSelection = character(0)
) 

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# First configuration: No PS adjustment (from JSON first setting)
psConfigList[[1]] <- list(
  method = "none",
  label  = "NoPS",
  params = list()
)

# Second configuration: 1:1 matching with propensity score caliper (0.05)
psConfigList[[2]] <- list(
  method = "match",
  label  = "Match1to1",
  params = list(
    maxRatio     = 1,
    caliper      = 0.05,
    caliperScale = "propensity score"
  )
)

# Third configuration: 1:10 matching with standardized logit caliper (0.2)
psConfigList[[3]] <- list(
  method = "match",
  label  = "Match1to10",
  params = list(
    maxRatio     = 10,
    caliper      = 0.2,
    caliperScale = "standardized logit"
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
      
      # Set PS adjustment arguments based on configuration
      if (psCfg$method == "none") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "match") {
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

      # Covariate settings: use default settings since no specific concepts to include/exclude
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcome list includes both primary outcome and negative controls
      outcomeList <- append(
        # Primary outcome from JSON
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999  # From JSON specification
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
          excludedCovariateConceptIds = c(
            excludedCovariateConcepts$conceptId  # Empty per JSON specification
          )
        )
      }

      # GetDbCohortMethodDataArgs from JSON specification
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,  # From JSON: restrictToCommonPeriod = false
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,  # 0 means no limit (from JSON)
        firstExposureOnly = TRUE,  # From JSON
        washoutPeriod = 183,  # From JSON
        removeDuplicateSubjects = "keep first",  # From JSON
        covariateSettings = covariateSettings
      )

      # CreatePsArgs from JSON specification
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # From JSON
        errorOnHighCorrelation = TRUE,  # From JSON
        stopOnError = FALSE,  # Allow Strategus to complete all CM operations
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",  # From JSON
          exclude = c(0), 
          useCrossValidation = TRUE  # From JSON
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",  # From JSON
          cvType = "auto",  # From JSON
          seed = 1, 
          resetCoefficients = TRUE,  # From JSON
          tolerance = 2e-07,  # From JSON
          cvRepetitions = 10,  # From JSON: cvRepetitions = 10
          fold = 10,  # From JSON: fold = 10
          startingVariance = 0.01  # From JSON
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

      # FitOutcomeModelArgs from JSON specification
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",  # From JSON
        stratified = TRUE,  # From JSON
        useCovariates = FALSE,  # From JSON
        inversePtWeighting = FALSE,  # From JSON
        prior = Cyclops::createPrior(
          priorType = "laplace",  # From JSON
          useCrossValidation = TRUE  # From JSON
        ),
        control = Cyclops::createControl(
          cvType = "auto",  # From JSON
          seed = 1, 
          resetCoefficients = TRUE,  # From JSON
          startingVariance = 0.01,  # From JSON
          tolerance = 2e-07,  # From JSON
          cvRepetitions = 10,  # From JSON
          fold = 10,  # From JSON
          noiseLevel = "quiet"  # From JSON
        )
      )
      
      # CreateStudyPopArgs from JSON specification
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,  # From JSON
        firstExposureOnly = FALSE,  # From JSON (note: different from getDbCohortMethodDataArgs)
        washoutPeriod = 0,  # From JSON
        removeDuplicateSubjects = "keep all",  # From JSON
        censorAtNewRiskWindow = FALSE,  # From JSON
        removeSubjectsWithPriorOutcome = FALSE,  # From JSON
        priorOutcomeLookback = 99999,  # From JSON
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
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "strokerisk", "strokeriskAnalysisSpecification.json")
)