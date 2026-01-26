################################################################################
# This script generates the analysis specifications for a Strategus study.
# It is based on the settings provided in the <Analysis Specifications> section
# and uses the structure from the provided <Template>.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# provides further details.
################################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# This section defines the cohorts and concept sets that are used across the
# different analysis modules.

# The baseUrl for the WebAPI instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Here we retrieve the cohort definitions from ATLAS.
# The cohort IDs are taken from the "cohortDefinitions" section of the JSON specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # From "targetCohort":"id"
    1794132, # From "comparatorCohort":"id"
    1794131  # From "outcomeCohort":"id"
  ),
  generateStats = TRUE
)

# Re-numbering cohorts for internal consistency within Strategus.
# It is a convention to use simple integers (1, 2, 3, ...) as cohort IDs
# within the analysis specification.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative Control Outcomes ----------------------------------------------------
# This section defines the negative control outcomes using a concept set.
# Negative controls are outcomes not believed to be caused by the exposure and are
# used for empirical calibration.
# The concept set ID is from "negativeControlConceptSet":"id".
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # From "negativeControlConceptSet":"id"
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
  # Assign unique cohort IDs starting from 101 to avoid collision with other cohorts.
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Sanity check to ensure no cohort IDs are duplicated.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Analysis-specific Data Frames ------------------------------------------------
# These data frames hold the specific cohorts and settings for each analysis.

# Outcomes of interest list.
# This uses the re-numbered outcome cohort ID (3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # cleanWindow is a standard parameter for defining outcomes, not specified in the JSON.
  # A default of 365 days is used here.
  mutate(cleanWindow = 365)

# Target and Comparator list for the CohortMethod analysis.
# Names are from "cohortDefinitions" in the JSON.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1", # From "targetCohort":"name"
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1" # From "comparatorCohort":"name"
)

# Covariate Selection:
# The "covariateSelection" section in the JSON is empty for both include and exclude lists.
# This means we will not specify any additional concepts to include or exclude
# beyond the default covariate settings. The data frame below is empty to reflect this.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all.
# This is commented out as "conceptsToInclude" in the JSON is empty.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# Module Settings --------------------------------------------------------------
# This section defines the settings for each Strategus module.

# CohortGeneratorModule --------------------------------------------------------
# This module is responsible for generating the cohorts defined above.
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

# CohortDiagnosticsModule ------------------------------------------------------
# This module runs diagnostics on the generated cohorts.
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

# Study periods for the analysis.
# From "getDbCohortMethodDataArgs":"studyPeriods".
studyPeriods <- tibble(
  studyStartDate = c("20010101"), # From "studyStartDate"
  studyEndDate   = c("20171231")  # From "studyEndDate"
)

# Time-at-risks (TARs) for the outcomes.
# From "createStudyPopArgs":"timeAtRisks".
timeAtRisks <- tibble(
  label = c("On Treatment (1d start to cohort end)"), # A descriptive label for this TAR
  riskWindowStart  = c(1),       # From "riskWindowStart"
  startAnchor = c("cohort start"), # From "startAnchor"
  riskWindowEnd  = c(0),       # From "riskWindowEnd"
  endAnchor = c("cohort end")  # From "endAnchor"
)

# Propensity Score settings - match on PS.
# From "propensityScoreAdjustment":"psSettings":"matchOnPsArgs".
matchOnPsArgsList <- tibble(
  label = c("10 to 1 matching"), # A descriptive label for this matching strategy
  maxRatio  = c(10),            # From "maxRatio"
  caliper = c(0.2),           # From "caliper"
  caliperScale  = c("standardized logit") # From "caliperScale"
)

# Propensity Score settings - stratify by PS.
# This is empty because "stratifyByPsArgs" is null in the JSON.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c(),
)

# Build a single PS configuration list from the tibbles defined above.
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


# Iterate through all analysis setting combinations to create a list of analyses.
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create propensity score adjustment arguments based on the method.
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

      # Define covariate settings. Since "covariateSelection" in the JSON is empty,
      # we use the default settings.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Combine the outcomes of interest and the negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            # This lookback is applied when checking for prior outcomes.
            # From "createStudyPopArgs":"priorOutcomeLookBack".
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

      # Define the target-comparator-outcomes list.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Concepts to exclude from covariates. Based on the empty "covariateSelection"
          # in the JSON, this list is empty.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Define arguments for getting data from the database.
      # Settings from "getDbCohortMethodDataArgs".
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        # From "maxCohortSize". 0 means no limit.
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Define arguments for creating the propensity score model.
      # Settings from "propensityScoreAdjustment":"createPsArgs".
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From "maxCohortSizeForFitting"
        errorOnHighCorrelation = TRUE,    # From "errorOnHighCorrelation"
        stopOnError = FALSE, # Setting to FALSE allows Strategus to complete all operations
        estimator = "att",   # A common default, not specified in JSON
        prior = Cyclops::createPrior(
          priorType = "laplace",        # From "prior":"priorType"
          useCrossValidation = TRUE     # From "prior":"useCrossValidation"
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",        # From "control":"noiseLevel"
          cvType = "auto",              # From "control":"cvType"
          seed = 1,                     # A common default, not specified in JSON
          resetCoefficients = TRUE,     # From "control":"resetCoefficients"
          tolerance = 2e-07,            # From "control":"tolerance"
          cvRepetitions = 10,           # From "control":"cvRepetitions"
          startingVariance = 0.01,      # From "control":"startingVariance"
          fold = 10                     # From "control":"fold"
        )
      )

      # Arguments for computing covariate balance. Standard defaults are used.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Define arguments for fitting the outcome model.
      # Settings from "fitOutcomeModelArgs".
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",              # From "modelType"
        stratified = TRUE,              # From "stratified"
        useCovariates = FALSE,          # From "useCovariates"
        inversePtWeighting = FALSE,     # From "inversePtWeighting"
        prior = Cyclops::createPrior(
          priorType = "laplace",        # From "prior":"priorType"
          useCrossValidation = TRUE     # From "prior":"useCrossValidation"
        ),
        control = Cyclops::createControl(
          cvType = "auto",              # From "control":"cvType"
          seed = 1,                     # A common default, not specified in JSON
          resetCoefficients = TRUE,     # From "control":"resetCoefficients"
          startingVariance = 0.01,      # From "control":"startingVariance"
          tolerance = 2e-07,            # From "control":"tolerance"
          cvRepetitions = 10,           # From "control":"cvRepetitions"
          noiseLevel = "quiet",         # From "control":"noiseLevel"
          fold = 10                     # From "control":"fold"
        )
      )

      # Define arguments for creating the study population.
      # Settings from "createStudyPopArgs".
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,       # From "restrictToCommonPeriod"
        firstExposureOnly = FALSE,            # From "firstExposureOnly"
        washoutPeriod = 0,                    # From "washoutPeriod"
        removeDuplicateSubjects = "keep all", # From "removeDuplicateSubjects"
        censorAtNewRiskWindow = FALSE,        # From "censorAtNewRiskWindow"
        removeSubjectsWithPriorOutcome = TRUE,# From "removeSubjectsWithPriorOutcome"
        priorOutcomeLookback = 99999,         # From "priorOutcomeLookBack"
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,                    # From "timeAtRisks":"minDaysAtRisk"
        maxDaysAtRisk = 99999                 # A common default, not specified in JSON
      )

      # Append the complete analysis settings to the list.
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

# Create the CohortMethod module specifications.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the final analysis specifications object -----------------------------
# This combines all the shared resources and module specifications into a single
# object that can be executed by Strategus.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The study name is from the "name" field in the JSON.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "strokerisk", "strokeriskAnalysisSpecification.json")
)