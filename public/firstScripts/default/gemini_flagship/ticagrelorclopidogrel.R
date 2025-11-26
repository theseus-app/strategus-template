################################################################################
# This script uses the OHDSI Strategus framework to create a study analysis
# specification based on the settings provided in a JSON file.
#
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# further details.
# ##############################################################################
library(dplyr)
library(Strategus)
library(CohortMethod)

# Name of the study, used to create a subdirectory for the results.
# From <Analysis Specifications> "name"
studyName <- "ticagrelorclopidogrel"

# Shared Resources -------------------------------------------------------------
# In this section, we define resources that are shared across execution modules.
# This includes the cohort definitions that will be used for targets, comparators,
# and outcomes.

# Details for connecting to the OHDSI WebAPI from which to retrieve cohorts.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Retrieve cohort definitions from WebAPI and build a cohortDefinitionSet data frame.
# From <Analysis Specifications> "cohortDefinitions"
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to a consistent ID space for the Strategus study.
# Target cohort is re-assigned to ID 1.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# Comparator cohort is re-assigned to ID 2.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# Outcome cohort is re-assigned to ID 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Retrieve negative control outcome concepts.
# From <Analysis Specifications> "negativeControlConceptSet"
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # id for "negative"
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
  # Assign cohort IDs starting from 101 for negative controls to avoid collision with other cohorts.
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)


# Sanity check to ensure no cohort IDs are duplicated across definitions.
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts we'll use in the analysis.
# From <Analysis Specifications> "cohortDefinitions" -> "outcomeCohort"
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Using re-assigned ID for outcome1
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Standard setting, not in JSON. Defines outcome clean window.

# From <Analysis Specifications> "cohortDefinitions" -> "targetCohort" and "comparatorCohort"
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# The <Analysis Specifications> "covariateSelection" section for excluding concepts is empty.
# CohortMethod automatically excludes the target and comparator cohorts from the covariates,
# so no additional concepts need to be specified here.
# excludedCovariateConcepts <- data.frame(conceptId = c(), conceptName = c())

# CohortGeneratorModule --------------------------------------------------------
# This module is responsible for instantiating the cohorts.
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
# This module runs diagnostics on the instantiated cohorts.
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
# This module performs the comparative cohort analysis.

# Define the study periods for the analysis.
# From <Analysis Specifications> "getDbCohortMethodDataArgs" -> "studyPeriods"
studyPeriods <- tibble(
  studyStartDate = c("20111101", "20130301"),
  studyEndDate   = c("20190331", "20161231")
)

# Define the time-at-risk (TAR) windows.
# From <Analysis Specifications> "createStudyPopArgs" -> "timeAtRisks"
timeAtRisks <- tibble(
  label = c(
    "1-365d post-start", "1-1825d post-start", "1d post-start to end",
    "29-365d post-start", "29-1825d post-start", "29d post-start to end"
  ),
  riskWindowStart  = c(1, 1, 1, 29, 29, 29),
  startAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd  = c(365, 1825, 0, 365, 1825, 0),
  endAnchor = c("cohort start", "cohort start", "cohort end", "cohort start", "cohort start", "cohort end")
)

# Define propensity score matching arguments.
# From <Analysis Specifications> "propensityScoreAdjustment" -> "psSettings" -> "matchOnPsArgs"
matchOnPsArgsList <- tibble(
  label = c("1-to-1 matching", "1-to-10 matching"),
  maxRatio  = c(1, 10),
  caliper = c(0.2, 0.2),
  caliperScale  = c("standardized logit", "standardized logit")
)

# Define propensity score stratification arguments.
# From <Analysis Specifications> "propensityScoreAdjustment" -> "psSettings" -> "stratifyByPsArgs"
stratifyByPsArgsList <- tibble(
  label = c("10 strata"),
  numberOfStrata  = c(10),
  baseSelection = c("all")
)

# Build a single list of PS configurations from the matching and stratification settings.
psConfigList <- list()

# Convert each row in matchOnPsArgsList to a configuration object.
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

# Convert each row in stratifyByPsArgsList to a configuration object.
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


# Iterate through all combinations of study periods, TARs, and PS settings to create a list of analyses.
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Set up matching or stratification arguments based on the current PS configuration.
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

      # Use default covariate settings from FeatureExtraction.
      # From <Analysis Specifications> "covariateSelection", which is empty/default.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Define the list of outcomes, combining the main outcome and negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA
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
      
      # Define the target-comparator-outcome combinations for the analysis.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList
          # The "covariateSelection" -> "conceptsToExclude" was empty, so no excludedCovariateConceptIds are passed.
        )
      }

      # Define arguments for retrieving data from the database.
      # From <Analysis Specifications> "getDbCohortMethodDataArgs"
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # from restrictToCommonPeriod
        studyStartDate = studyStartDate, # from studyPeriods
        studyEndDate = studyEndDate, # from studyPeriods
        maxCohortSize = 0, # from maxCohortSize
        covariateSettings = covariateSettings
        # Note: firstExposureOnly, washoutPeriod, and removeDuplicateSubjects from the JSON's
        # getDbCohortMethodDataArgs section are applied in createCreateStudyPopulationArgs
        # as that is their correct location in the CohortMethod R package.
      )

      # Define arguments for creating the propensity score model.
      # From <Analysis Specifications> "propensityScoreAdjustment" -> "createPsArgs"
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # from maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # from errorOnHighCorrelation
        stopOnError = FALSE, # Standard setting to allow Strategus to complete all operations
        estimator = "att", # Standard setting for matching
        prior = Cyclops::createPrior(
          priorType = "laplace", # from prior -> priorType
          useCrossValidation = TRUE # from prior -> useCrossValidation
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", # from control -> noiseLevel
          cvType = "auto", # from control -> cvType
          seed = 1, # Standard setting for reproducibility
          resetCoefficients = TRUE, # from control -> resetCoefficients
          tolerance = 2e-07, # from control -> tolerance
          cvRepetitions = 10, # from control -> cvRepetitions
          fold = 10, # from control -> fold
          startingVariance = 0.01 # from control -> startingVariance
        )
      )
      
      # Define arguments for computing covariate balance.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Define arguments for fitting the outcome model.
      # From <Analysis Specifications> "fitOutcomeModelArgs"
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # from modelType
        stratified = TRUE, # from stratified
        useCovariates = FALSE, # from useCovariates
        inversePtWeighting = FALSE, # from inversePtWeighting
        prior = Cyclops::createPrior(
          priorType = "laplace", # from prior -> priorType
          useCrossValidation = TRUE # from prior -> useCrossValidation
        ),
        control = Cyclops::createControl(
          cvType = "auto", # from control -> cvType
          seed = 1, # Standard setting for reproducibility
          resetCoefficients = TRUE, # from control -> resetCoefficients
          startingVariance = 0.01, # from control -> startingVariance
          tolerance = 2e-07, # from control -> tolerance
          cvRepetitions = 10, # from control -> cvRepetitions
          fold = 10, # from control -> fold
          noiseLevel = "quiet" # from control -> noiseLevel
        )
      )
      
      # Define arguments for creating the study population.
      # From <Analysis Specifications> "createStudyPopArgs"
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # from restrictToCommonPeriod
        firstExposureOnly = FALSE, # from firstExposureOnly
        washoutPeriod = 0, # from washoutPeriod
        removeDuplicateSubjects = "keep all", # from removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # from censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = FALSE, # from removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # from priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t], # from timeAtRisks
        startAnchor = timeAtRisks$startAnchor[t], # from timeAtRisks
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # from timeAtRisks
        endAnchor = timeAtRisks$endAnchor[t], # from timeAtRisks
        minDaysAtRisk = 1, # from timeAtRisks -> minDaysAtRisk
        maxDaysAtRisk = 99999 # Standard setting
      )

      # Append the complete analysis settings to the list.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study Period: %s-%s; TAR: %s; PS: %s",
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

# Create the final CohortMethod module specifications.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the final analysis specifications object --------------------------------
# This combines all shared resources and module specifications into a single object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", studyName, paste0(studyName, "AnalysisSpecification.json"))
)