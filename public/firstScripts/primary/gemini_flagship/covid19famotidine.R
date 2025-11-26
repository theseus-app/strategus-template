################################################################################
# This script uses the OHDSI Strategus package to create analysis
# specifications for a comparative cohort study. The settings are derived
# from a JSON configuration file.
#
# Refer to the official Strategus documentation for more details on the
# available modules and their settings:
# https://ohdsi.github.io/Strategus/reference/index.html
################################################################################
library(dplyr)
library(Strategus)

# =========== Define Analysis Specifications from JSON ==============
# This section translates the settings from the <Analysis Specifications>
# JSON into R objects that will be used to build the Strategus analysis
# specification.

# --- Shared Resources: Cohorts and Concept Sets ---

# WebAPI baseUrl for fetching cohort and concept set definitions.
# This should point to the ATLAS instance where the assets are defined.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI" # Using demo ATLAS as a placeholder

# Cohort IDs from the JSON specification.
# FROM: cohortDefinitions
targetCohortId <- 1794126
comparatorCohortId <- 1794132
outcomeCohortIds <- c(1794131)

# Negative control concept set ID from the JSON specification.
# FROM: negativeControlConceptSet
negativeControlConceptSetId <- 1888110

# Export cohort definitions from WebAPI.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    targetCohortId,
    comparatorCohortId,
    outcomeCohortIds
  ),
  generateStats = TRUE
)

# Rename cohorts for clarity and consistency within the analysis specification.
# We map the WebAPI IDs to simple sequential IDs (1, 2, 3...).
# FROM: cohortDefinitions
cohortDefinitionSet <- cohortDefinitionSet %>%
  mutate(cohortId = case_when(
    cohortId == targetCohortId ~ 1,
    cohortId == comparatorCohortId ~ 2,
    cohortId == outcomeCohortIds[1] ~ 3,
    TRUE ~ cohortId
  ))

# Fetch, resolve, and format negative control concepts from the specified concept set.
# These will be used to generate negative control outcome cohorts.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = negativeControlConceptSetId,
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
  # Assign unique cohort IDs starting from 101 to avoid collision with main cohorts.
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)


# Sanity check to ensure no duplicate cohort IDs exist.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to define the analysis populations and outcomes.

# Define the outcomes of interest for the study.
# FROM: cohortDefinitions.outcomeCohort
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  select(outcomeCohortId = cohortId, outcomeCohortName = cohortName)

# Define the target and comparator cohort pairs for the CohortMethod analysis.
# FROM: cohortDefinitions.targetCohort, cohortDefinitions.comparatorCohort
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# --- Module Settings ---

# The JSON specification does not define specific concepts to include or exclude
# from the covariate analysis, so we will use the default covariate settings
# from FeatureExtraction. This automatically excludes the drug concepts that
# define the target and comparator cohorts.
# FROM: covariateSelection

# =========== Strategus Module Specifications ==============

# --- CohortGeneratorModule ---
# This module generates the cohorts defined in the cohortDefinitionSet and
# the negative control outcome cohorts.
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

# --- CohortDiagnosticsModule ---
# This module runs a standard set of diagnostics on the generated cohorts
# to assess their characteristics and quality.
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

# --- CohortMethodModule ---
# This section defines the settings for the comparative cohort analysis.

# Define the study period(s).
# FROM: getDbCohortMethodDataArgs.studyPeriods
studyPeriods <- tibble(
  studyStartDate = c("20200201"),
  studyEndDate   = c("20200530")
)

# Define the time-at-risk (TAR) windows.
# FROM: createStudyPopArgs.timeAtRisks
timeAtRisks <- tibble(
  label = c("1-30d post index"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(30),
  endAnchor = c("cohort start")
)

# Define propensity score (PS) adjustment settings.
# FROM: propensityScoreAdjustment.psSettings
# The specification calls for stratification, not matching.
matchOnPsArgsList <- tibble() # Empty as per specification

stratifyByPsArgsList <- tibble(
  # A label for this PS adjustment strategy.
  label = c("PS Stratification 5 Strata"),
  # Number of strata to create based on the propensity score.
  # FROM: propensityScoreAdjustment.psSettings.stratifyByPsArgs.numberOfStrata
  numberOfStrata  = c(5),
  # Defines which population's quantiles are used to define the strata boundaries.
  # FROM: propensityScoreAdjustment.psSettings.stratifyByPsArgs.baseSelection
  baseSelection = c("all"),
)

# Convert the PS settings tibbles into a list of configurations for the analysis loop.
psConfigList <- list()
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


# Main loop to create all combinations of analysis settings.
# In this case, there is only one of each setting (1 study period, 1 TAR, 1 PS method).
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Set up PS adjustment arguments based on the method defined.
      matchOnPsArgs <- NULL
      stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
        numberOfStrata = psCfg$params$numberOfStrata,
        baseSelection = psCfg$params$baseSelection
      )

      # Use default covariate settings as specified.
      # FROM: covariateSelection (empty include/exclude lists)
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create a list of all outcomes for the analysis, including both the
      # outcome of interest and the negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE
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
      
      # Combine target, comparator, and outcome lists.
      # The default covariate settings will handle exclusion of T & C drug concepts.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList
        )
      }

      # Define arguments for fetching data from the database.
      # FROM: getDbCohortMethodDataArgs
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        # A maxCohortSize of 0 means no limit.
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Define arguments for creating the study population.
      # FROM: createStudyPopArgs
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 30,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1
      )
      
      # Define arguments for creating the propensity score model.
      # FROM: propensityScoreAdjustment.createPsArgs
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          tolerance = 2e-7,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "silent",
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          seed = 1 # for reproducibility
        )
      )

      # Arguments for covariate balance computation (using defaults).
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs()
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs()

      # Define arguments for fitting the outcome model.
      # FROM: fitOutcomeModelArgs
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
          tolerance = 2e-7,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          seed = 1 # for reproducibility
        )
      )

      # Assemble the complete set of analysis settings for this iteration.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Analysis of %s vs %s during %s-%s; TAR: %s; PS: %s",
          cmTcList$targetCohortName,
          cmTcList$comparatorCohortName,
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
  refitPsForEveryStudyPopulation = FALSE
)

# =========== Assemble and Save Analysis Specifications ==============
# Combine all module specifications into a single Strategus analysis specification JSON file.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Add shared resources (cohorts, negative controls)
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  # Add module specifications
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the specifications to a JSON file.
# The file name is based on the 'name' field in the JSON specifications.
# FROM: name
analysisName <- "covid19famotidine"
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", analysisName, paste0(analysisName, "AnalysisSpecification.json"))
)