################################################################################
# This script uses the OHDSI Strategus package to create an analysis
# specification JSON file. The settings are derived from the provided
# <Analysis Specifications> document.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# provides further details.
# ##############################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# These resources are available to all modules.

# The baseUrl for the WebAPI instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# The following section defines the cohorts used in this analysis, retrieving
# them from the specified ATLAS instance.

# Export the cohort definitions from ATLAS.
# Per Analysis Specifications:
# - Target Cohort (ID: 1794126, Name: target1)
# - Comparator Cohort (ID: 1794132, Name: comparator1)
# - Outcome Cohort (ID: 1794131, Name: outcome1)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-numbering cohort IDs for consistency within the Strategus framework.
# It is a common practice to use simple, sequential IDs (e.g., 1, 2, 3)
# for the primary cohorts in the analysis.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1 # target1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2 # comparator1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3 # outcome1

# Negative Control Outcomes ----------------------------------------------------
# This section defines the negative control outcome cohorts. These are generated
# from a concept set of conditions that are not believed to be caused by the
# exposure.
# Per Analysis Specifications:
# - Negative Control Concept Set (ID: 1888110, Name: negative)
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
  # Assign unique cohort IDs starting from 101 to avoid conflicts with primary cohorts
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Verify that there are no duplicate cohort IDs between the primary cohorts and
# the negative control cohorts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Analysis-Specific Cohort Data Frames -----------------------------------------
# These data frames organize the cohorts for use in specific parts of the analysis,
# such as defining target-comparator-outcome relationships.

# Define the outcome(s) of interest for the CohortMethod analysis.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Filter for the re-numbered outcome cohort 'outcome1'
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # The cleanWindow is not specified in the JSON, using a common default of 365 days.
  # This setting is used by some diagnostic checks.
  mutate(cleanWindow = 365)

# Define the target and comparator pairs for the CohortMethod analysis.
# Per Analysis Specifications:
# - Target: cohortId=1 (target1)
# - Comparator: cohortId=2 (comparator1)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate Concept Exclusion --------------------------------------------------
# This section specifies concepts to be excluded from the covariate construction.
# The Analysis Specifications for `conceptsToInclude` and `conceptsToExclude` are
# empty, so we create an empty data frame.
# Note: Drug concepts for the target and comparator are typically excluded by default
# within the CohortMethod module itself.
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(),
  conceptName = character()
)

# Optional: To define specific covariates to include, you would create a
# data frame like this. It is commented out as it's not used in this analysis.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )


# =========== Strategus Module Specifications ===========

# CohortGeneratorModule Settings -----------------------------------------------
# This module is responsible for generating all the cohort definitions specified.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Defines the primary cohorts to be generated.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Defines the negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
# Specifies the module settings for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# This module runs a comprehensive set of diagnostics on the generated cohorts.
# The settings below are standard defaults as no specific settings were provided
# in the Analysis Specifications.
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
# This module executes the new-user cohort method analysis, including propensity
# score estimation and outcome modeling.

# Define Study Periods for the analysis.
# Per Analysis Specifications `getDbCohortMethodDataArgs.studyPeriods`.
studyPeriods <- tibble(
  studyStartDate = c("20100101", "20120101"),
  studyEndDate   = c("20191231", "20191231")
)

# Define Time-at-Risk (TAR) windows.
# Per Analysis Specifications `createStudyPopArgs.timeAtRisks`.
timeAtRisks <- tibble(
  label = c("On Treatment", "Intent to Treat"),
  riskWindowStart  = c(1, 1),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd  = c(0, 99999),
  endAnchor = c("cohort end", "cohort start")
)

# Define Propensity Score Matching settings.
# Per Analysis Specifications `propensityScoreAdjustment.psSettings`.
matchOnPsArgsList <- tibble(
  label = c("1-to-many PS Matching"),
  maxRatio  = c(0), # 0 indicates variable ratio matching
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# Define Propensity Score Stratification settings.
# Per Analysis Specifications `propensityScoreAdjustment.psSettings`.
stratifyByPsArgsList <- tibble(
  label = c("5 Strata PS Stratification"),
  numberOfStrata  = c(5),
  baseSelection = c("all")
)

# Dynamically build a single list of all Propensity Score configurations.
# This structure makes it easy to loop through different PS methods.
psConfigList <- list()

# Convert the matching settings tibble into the configuration list format.
if (exists("matchOnPsArgsList") && nrow(matchOnPsAgsList) > 0) {
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

# Convert the stratification settings tibble into the configuration list format.
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


# Main loop to create all analysis variants ------------------------------------
# This set of nested loops iterates through all combinations of study periods,
# TARs, and PS settings to create a comprehensive list of analyses.
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Based on the method in the config list, create either matching or
      # stratification arguments.
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
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create the list of outcomes, combining the primary outcome of interest
      # with the list of negative control outcomes.
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
      
      # Create the Target-Comparator-Outcomes list.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorId[i],
          outcomes = outcomeList,
          # Per Analysis Specifications, no additional concepts were specified for exclusion.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Define arguments for retrieving data from the database.
      # Settings from `getDbCohortMethodDataArgs` in Analysis Specifications.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # 0 means no limit
        covariateSettings = covariateSettings
      )

      # Define arguments for creating the propensity score model.
      # Settings from `propensityScoreAdjustment.createPsArgs` in Analysis Specifications.
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",
          exclude = c(0),
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",
          cvType = "auto",
          resetCoefficients = TRUE,
          tolerance = 2e-07,
          cvRepetitions = 10,
          startingVariance = 0.01
        )
      )

      # Arguments for computing covariate balance. Using standard defaults.
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Define arguments for fitting the outcome model.
      # Settings from `fitOutcomeModelArgs` in Analysis Specifications.
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
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
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          tolerance = 2e-07,
          cvRepetitions = 10,
          noiseLevel = "quiet"
        )
      )
      
      # Define arguments for creating the study population.
      # Settings from `createStudyPopArgs` in Analysis Specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        # These are taken from the `timeAtRisks` tibble defined earlier
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1
      )


      # Assemble all the arguments into a single CohortMethod analysis object.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Period: %s-%s; TAR: %s; PS: %s",
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

# Create the final CohortMethod module specifications, including the list of
# all analysis variants.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Assemble the final Analysis Specification ------------------------------------
# This combines all shared resources and module specifications into a single
# object that Strategus can execute.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# Per Analysis Specifications, the study name is "corazon".
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("corazonAnalysisSpecification.json")
)