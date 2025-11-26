################################################################################
# This script creates a Strategus analysis specification JSON file for a
# comparative cohort study. The settings are derived from the provided
# <Analysis Specifications>.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# provides further details.
# ##############################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# This section defines the cohorts and concept sets that are used across the
# different analysis modules.

# The baseUrl for the WebAPI instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Define the cohort IDs for target, comparator, and outcome cohorts based on the specifications.
# These IDs are used to fetch the cohort definitions from ATLAS.
targetCohortId <- 1794126
comparatorCohortId <- 1794132
outcomeCohortId <- 1794131

# Use ROhdsiWebApi to export the cohort definitions from ATLAS.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    targetCohortId,     # Target: target1
    comparatorCohortId, # Comparator: comparator1
    outcomeCohortId     # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-numbering cohorts to a consistent schema (e.g., 1 for target, 2 for comparator, 3 for outcome).
# This simplifies referencing them later in the analysis specifications.
cohortDefinitionSet[cohortDefinitionSet$cohortId == targetCohortId,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == comparatorCohortId,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == outcomeCohortId,]$cohortId <- 3

# Fetch the negative control outcome concept set from ATLAS.
# Negative controls are used for empirical calibration of p-values.
negativeControlConceptSetId <- 1888110 # "negative"
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
  # Rename columns to match the required format for Strategus.
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  # Assign unique cohort IDs for each negative control, starting from 101 to avoid conflicts.
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)


# Sanity check to ensure there are no duplicate cohort IDs between the main cohorts and negative controls.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the specific cohorts used in the CohortMethod analysis.

# Outcomes of interest for the study.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Filter for the re-numbered outcome cohort ID.
  rename(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName)

# Target and Comparator definitions for the CohortMethod analysis.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# The <Analysis Specifications> specified empty arrays for conceptsToInclude/Exclude.
# Therefore, this data frame is initialized as empty. It can be populated to
# exclude specific concepts from the covariate analysis (e.g., the study drugs).
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# CohortGeneratorModule --------------------------------------------------------
# This module is responsible for generating the cohort instances from their
# definitions in a database.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Define the set of cohorts to be generated.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Define the negative control outcome cohorts to be generated.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create the final module specifications for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# This module runs a standard set of diagnostics on the generated cohorts.
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
# This section defines the settings for the comparative cohort analysis.

# Define the overall study period. An empty string would mean no date restriction.
# As per "getDbCohortMethodDataArgs", "studyPeriods".
studyPeriods <- tibble(
  studyStartDate = c("20101019"),
  studyEndDate   = c("20181231")
)

# Define the time-at-risk (TAR) windows for the outcomes.
# As per "createStudyPopArgs", "timeAtRisks".
timeAtRisks <- tibble(
  label = c("On Treatment"), # A descriptive label for this TAR.
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
)

# Define propensity score matching settings.
# As per "propensityScoreAdjustment", "psSettings", "matchOnPsArgs".
matchOnPsArgsList <- tibble(
  label = c("1-to-1 matching"), # A descriptive label for this matching strategy.
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# Define propensity score stratification settings.
# As per "propensityScoreAdjustment", "psSettings", "stratifyByPsArgs" is null, so this is empty.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c()
)

# Build a single list of PS configurations. The loop below will iterate through this list.
psConfigList <- list()

# Convert the "matchOnPsArgsList" tibble into the psConfigList format.
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

# Convert the "stratifyByPsArgsList" tibble into the psConfigList format.
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


# Iterate through all combinations of study period, TAR, and PS settings to create
# a list of analyses to be executed.
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create either matchOnPsArgs or stratifyByPsArgs based on the method defined.
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

      # Use default covariate settings. The specifications did not require custom settings.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Combine the primary outcomes of interest with the negative control outcomes.
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

      # Define the target-comparator-outcomes list.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # As per the specifications, no concepts are excluded from covariates.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Settings for fetching data from the database.
      # As per "getDbCohortMethodDataArgs".
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # 0 means no limit on cohort size.
        covariateSettings = covariateSettings
      )

      # Settings for creating the propensity score model.
      # As per "propensityScoreAdjustment", "createPsArgs".
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE,
          exclude = c(0)
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",
          cvType = "auto",
          seed = 1,
          resetCoefficients = TRUE,
          tolerance = 2e-07,
          cvRepetitions = 10,
          startingVariance = 0.01,
          fold = 10
        )
      )

      # Settings for computing covariate balance.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Settings for fitting the outcome model.
      # As per "fitOutcomeModelArgs".
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = FALSE, # Matched sets are not used as strata in the outcome model.
        useCovariates = FALSE, # No covariates are included in the outcome model.
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
          cvRepetitions = 10,
          noiseLevel = "quiet",
          fold = 10
        )
      )

      # Settings for creating the study population.
      # As per "createStudyPopArgs".
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = TRUE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        # Time-at-risk settings are applied here, from the loop variable.
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
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

# Create the final module specifications for CohortMethod, including all defined analyses.
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
# This final step assembles all the module specifications into a single
# analysisSpecifications object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Add the shared resources (cohort definitions, negative controls).
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  # Add the module specifications for each analysis step.
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the final analysis specifications object to a JSON file.
# The file name is based on the "name" field from the specifications.
studyName <- "doacsandwarfarin"
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", studyName, paste0(studyName, "AnalysisSpecification.json"))
)