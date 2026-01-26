################################################################################
# This script uses the OHDSI Strategus package to create an analysis
# specification JSON file. The settings are derived from the provided
# <Analysis Specifications>.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# provides further details on the function arguments.
################################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# This section defines resources that are shared across different modules,
# such as cohort definitions and concept sets.

# The base URL for the WebAPI instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Here we export the cohort definitions from ATLAS into a data frame.
# The cohort IDs are taken from the "cohortDefinitions" section of the
# analysis specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # From cohortDefinitions.targetCohort: "target1"
    1794132, # From cohortDefinitions.comparatorCohort: "comparator1"
    1794131  # From cohortDefinitions.outcomeCohort: "outcome1"
  ),
  generateStats = TRUE
)

# Re-numbering cohorts for internal consistency within the Strategus study.
# It is a common practice to use simple, sequential IDs (e.g., 1, 2, 3)
# within the analysis specification for clarity.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes
# This section defines the negative control outcomes using a concept set.
# The concept set ID is taken from "negativeControlConceptSet" in the specs.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # From negativeControlConceptSet.id: "negative"
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
  # Assign unique cohort IDs to negative controls, starting from 101 to avoid conflicts.
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# A safety check to ensure there are no duplicate cohort IDs between the
# manually defined cohorts and the negative control cohorts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts for use in the analysis modules.
# Outcomes of interest list.
# From cohortDefinitions.outcomeCohort.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # A standard 365-day clean window for outcomes.

# Target and Comparator list for the CohortMethod analysis.
# From cohortDefinitions.targetCohort and cohortDefinitions.comparatorCohort.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate concepts to exclude from the analysis.
# The "covariateSelection.conceptsToExclude" in the specifications is empty,
# so we create an empty data frame.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# Optional: Define covariates to include.
# The "covariateSelection.conceptsToInclude" in the specifications is empty,
# so this section is commented out.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# This module is responsible for generating the cohort instances based on the
# definitions provided.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Define the main cohorts (T, C, O) as a shared resource.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Define the negative control outcome cohorts as a shared resource.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
# Create the module specifications for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# This module runs a standard set of diagnostic checks on the generated cohorts.
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
# This module executes the comparative cohort analysis.

# Study periods: Defines the calendar time range for the study.
# From getDbCohortMethodDataArgs.studyPeriods: studyStartDate and studyEndDate are null,
# indicating the study is not restricted to a specific time window.
# An empty tibble results in empty strings for start/end dates, which CohortMethod
# interprets as no date restriction.
studyPeriods <- tibble(
  studyStartDate = c(""), # YYYYMMDD
  studyEndDate   = c("")  # YYYYMMDD
)

# Time-at-risks (TARs) for the outcomes of interest.
# From createStudyPopArgs.timeAtRisks.
timeAtRisks <- tibble(
  label = c("Start 1d to End 0d"),
  riskWindowStart  = c(1),              # From riskWindowStart
  startAnchor = c("cohort start"),      # From startAnchor
  riskWindowEnd  = c(0),                # From riskWindowEnd
  endAnchor = c("cohort end")           # From endAnchor
)

# Propensity Score settings - match on PS.
# From propensityScoreAdjustment.psSettings.matchOnPsArgs.
matchOnPsArgsList <- tibble(
  label = c("10_to_1_match_on_ps"),
  maxRatio  = c(10),                    # From maxRatio
  caliper = c(0.2),                     # From caliper
  caliperScale  = c("standardized logit") # From caliperScale
)

# Propensity Score settings - stratify by PS.
# From propensityScoreAdjustment.psSettings.stratifyByPsArgs: null.
# This tibble is empty as no stratification settings are specified.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c(), # "all" | "target" | "comparator"
)

# Build a single PS configuration list from the tibbles above.
# This allows iterating through different PS adjustment strategies.
psConfigList <- list()

# Convert the matchOnPsArgsList tibble into a list of configurations.
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

# Convert the stratifyByPsArgsList tibble into a list of configurations.
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

# The loops will iterate through all combinations of study periods, TARs, and PS settings.
# In this case, there is one of each, so the loop will run once.
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create arguments for matching or stratification based on the PS configuration.
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

      # Use default covariate settings as none are specified in the specs.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create a list of all outcomes, including the primary outcome and negative controls.
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

      # Define the Target-Comparator-Outcome combinations.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorId[i],
          outcomes = outcomeList,
          # Exclude concepts specified in the `excludedCovariateConcepts` data frame.
          # In this case, the data frame is empty.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Define arguments for fetching data from the database.
      # From getDbCohortMethodDataArgs.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From maxCohortSize
        covariateSettings = covariateSettings
      )

      # Define arguments for creating the propensity score model.
      # From propensityScoreAdjustment.createPsArgs.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE,    # From errorOnHighCorrelation
        stopOnError = FALSE,
        estimator = "att",
        # From prior object
        prior = Cyclops::createPrior(
          priorType = "laplace",          # From priorType
          exclude = c(0),
          useCrossValidation = TRUE       # From useCrossValidation
        ),
        # From control object
        control = Cyclops::createControl(
          noiseLevel = "silent",          # From noiseLevel
          cvType = "auto",                # From cvType
          seed = 1,
          resetCoefficients = TRUE,       # From resetCoefficients
          tolerance = 2e-07,              # From tolerance
          cvRepetitions = 10,             # From cvRepetitions
          startingVariance = 0.01         # From startingVariance
        )
      )

      # Define arguments for computing covariate balance.
      # These are standard settings not specified in the JSON.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Define arguments for fitting the outcome model.
      # From fitOutcomeModelArgs.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",                # From modelType
        stratified = TRUE,                # From stratified
        useCovariates = FALSE,            # From useCovariates
        inversePtWeighting = FALSE,       # From inversePtWeighting
        # From prior object
        prior = Cyclops::createPrior(
          priorType = "laplace",          # From priorType
          useCrossValidation = TRUE       # From useCrossValidation
        ),
        # From control object
        control = Cyclops::createControl(
          cvType = "auto",                # From cvType
          seed = 1,
          resetCoefficients = TRUE,       # From resetCoefficients
          startingVariance = 0.01,        # From startingVariance
          tolerance = 2e-07,              # From tolerance
          cvRepetitions = 10,             # From cvRepetitions
          noiseLevel = "quiet"            # From noiseLevel
        )
      )

      # Define arguments for creating the study population.
      # From createStudyPopArgs.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE,        # From restrictToCommonPeriod
        firstExposureOnly = TRUE,             # From firstExposureOnly
        washoutPeriod = 365,                  # From washoutPeriod
        removeDuplicateSubjects = "keep all", # From removeDuplicateSubjects
        censorAtNewRiskWindow = TRUE,         # From censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE,# From removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999,         # From priorOutcomeLookBack
        # TAR settings from the `timeAtRisks` tibble defined earlier
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1                     # From timeAtRisks.minDaysAtRisk
      )

      # Append the complete analysis settings to the cmAnalysisList.
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
# This combines all shared resources and module specifications into a single
# object that Strategus can execute.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file name is based on the "name" field in the analysis specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "uveitissafety", "uveitissafetyAnalysisSpecification.json")
)