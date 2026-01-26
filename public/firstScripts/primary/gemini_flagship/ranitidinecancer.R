################################################################################
# This script uses the OHDSI Strategus package to create a JSON analysis
# specification for a comparative cohort study.
#
# The settings are derived from the provided <Analysis Specifications> JSON.
#
# For more information about Strategus and HADES modules, please refer to:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

# Load required libraries
library(dplyr)
library(Strategus)

# ==> Section 1: Shared Resources <==
# These resources are used across different analysis modules.
# ------------------------------------------------------------------------------

# Define the base URL for the WebAPI.
# This is used to retrieve cohort and concept set definitions from an ATLAS instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# --- Cohort Definitions ---
# Fetch cohort definitions from WebAPI using their IDs from the analysis specifications.
# The `cohortDefinitions` section specifies the target, comparator, and outcome cohorts.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: "target1"
    1794132, # Comparator: "comparator1"
    1794131  # Outcome: "outcome1"
  ),
  generateStats = TRUE
)

# Re-number cohort IDs for internal consistency within the Strategus study.
# This makes it easier to reference them in the analysis settings.
# Target cohort ID is set to 1.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
# Comparator cohort ID is set to 2.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
# Outcome cohort ID is set to 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# --- Negative Control Outcomes ---
# Fetch the concept set for negative controls specified in `negativeControlConceptSet`.
# These concepts are used to generate outcome cohorts for empirical calibration.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # "negative"
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
  # Assign unique cohort IDs to negative controls, starting from 101 to avoid conflicts.
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Sanity check to ensure there are no duplicate cohort IDs.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}


# ==> Section 2: Analysis-Specific Data Frames <==
# These data frames define the specific comparisons and settings for the study.
# ------------------------------------------------------------------------------

# --- Outcomes of Interest ---
# Create a data frame for the primary outcome(s) of interest.
# This is based on the `outcomeCohort` array in the specifications.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Filter for the outcome cohort (ID 3)
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName)

# --- Target-Comparator List ---
# Define the target-comparator pairs for the CohortMethod analysis.
# This corresponds to the `targetCohort` and `comparatorCohort` settings.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  # NOTE: The concept IDs for the T & C drugs are needed to exclude them from
  # the covariate construction. These were not in the JSON specification, so
  # placeholder values for Ranitidine (953020) and Cimetidine (950699) are used.
  # Please replace these with the actual ingredient concept IDs for your cohorts.
  targetConceptId = 953020,
  comparatorConceptId = 950699
)

# --- Covariate Selection ---
# The `covariateSelection` section in the specifications was empty.
# This means we are not excluding any additional concepts beyond the T & C drugs.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all.
# The `conceptsToInclude` section was also empty.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )


# ==> Section 3: Strategus Module Specifications <==
# Define the settings for each HADES module that will be executed.
# ------------------------------------------------------------------------------

# --- CohortGeneratorModule ---
# This module generates the cohort instances based on the definitions.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Define the primary cohorts (T, C, O) as a shared resource.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Define the negative control outcome cohorts as a shared resource.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
# Create the final module specification for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# --- CohortDiagnosticsModule ---
# This module computes diagnostics on the generated cohorts.
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
# This module performs the comparative cohort analysis.

# `getDbCohortMethodDataArgs$studyPeriods`: The JSON specifies null start/end dates,
# meaning no overall study period restriction. We represent this with empty strings.
studyPeriods <- tibble(
  studyStartDate = c(""), # YYYYMMDD format, empty for no restriction
  studyEndDate   = c("")  # YYYYMMDD format, empty for no restriction
)

# `createStudyPopArgs$timeAtRisks`: Define the time-at-risk windows.
timeAtRisks <- tibble(
  label = c("On Treatment (365d to 99999d)"),
  riskWindowStart  = c(365),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(99999),
  endAnchor = c("cohort start")
)

# `propensityScoreAdjustment$psSettings`: Define propensity score adjustment strategies.
# The specification includes one matching strategy.
matchOnPsArgsList <- tibble(
  label = c("1-to-1 Matching, 0.2 Caliper"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# The specification does not include stratification, so this is empty.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c(), # "all" | "target" | "comparator"
)

# Build a single list of all PS configurations to iterate over.
psConfigList <- list()
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


# --- Analysis Assembly Loop ---
# Iterate through all combinations of settings to create a list of analyses.
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Configure PS adjustment arguments based on the current iteration.
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

      # Use default covariate settings as none were specified.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Combine the primary outcome(s) and negative control outcomes into a single list.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            # This corresponds to `createStudyPopArgs$priorOutcomeLookBack`.
            priorOutcomeLookback = 365
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

      # Create the list of target-comparator-outcomes settings.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude the concept IDs of the T & C drugs and any other specified concepts.
          excludedCovariateConceptIds = c(
            cmTcList$targetConceptId[i],
            cmTcList$comparatorConceptId[i],
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # Define arguments for getting data from the database.
      # Corresponds to `getDbCohortMethodDataArgs`.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE, # From JSON
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From JSON
        covariateSettings = covariateSettings
      )

      # Define arguments for creating the study population.
      # Corresponds to `createStudyPopArgs`.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From JSON
        firstExposureOnly = FALSE, # From JSON
        washoutPeriod = 365, # From JSON
        removeDuplicateSubjects = "keep all", # From JSON
        censorAtNewRiskWindow = FALSE, # From JSON
        removeSubjectsWithPriorOutcome = TRUE, # From JSON
        priorOutcomeLookback = 365, # From JSON
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1, # From JSON
        maxDaysAtRisk = 99999 # Default, not specified
      )

      # Define arguments for creating the propensity score model.
      # Corresponds to `propensityScoreAdjustment$createPsArgs`.
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From JSON
        errorOnHighCorrelation = TRUE, # From JSON
        stopOnError = FALSE, # Setting to FALSE allows Strategus to complete all operations
        estimator = "att", # Default, not specified
        prior = Cyclops::createPrior(
          priorType = "laplace", # From JSON
          useCrossValidation = TRUE # From JSON
        ),
        control = Cyclops::createControl(
          tolerance = 2e-07, # From JSON
          cvType = "auto", # From JSON
          fold = 10, # From JSON
          cvRepetitions = 10, # From JSON
          noiseLevel = "silent", # From JSON
          resetCoefficients = TRUE, # From JSON
          startingVariance = 0.01, # From JSON
          seed = 1 # Default, for reproducibility
        )
      )

      # Define arguments for computing covariate balance.
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Define arguments for fitting the outcome model.
      # Corresponds to `fitOutcomeModelArgs`.
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From JSON
        stratified = FALSE, # From JSON
        useCovariates = FALSE, # From JSON
        inversePtWeighting = FALSE, # From JSON
        prior = Cyclops::createPrior(
          priorType = "laplace", # From JSON
          useCrossValidation = TRUE # From JSON
        ),
        control = Cyclops::createControl(
          tolerance = 2e-07, # From JSON
          cvType = "auto", # From JSON
          fold = 10, # From JSON
          cvRepetitions = 10, # From JSON
          noiseLevel = "quiet", # From JSON
          resetCoefficients = TRUE, # From JSON
          startingVariance = 0.01, # From JSON
          seed = 1 # Default, for reproducibility
        )
      )

      # Append the fully specified analysis to the list.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study Period: %s-%s; TAR: %s; PS: %s",
          ifelse(studyStartDate == "", "None", studyStartDate),
          ifelse(studyEndDate == "", "None", studyEndDate),
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

# Create the final module specification for CohortMethod.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)


# ==> Section 4: Create and Save Analysis Specifications <==
# Combine all module specifications into a single analysis specification JSON file.
# ------------------------------------------------------------------------------

analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Add shared resources (cohorts, negative controls).
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  # Add module specifications.
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the specifications to a JSON file.
# The file path is based on the study name from the specifications: "ranitidinecancer".
studyName <- "ranitidinecancer"
outputDir <- file.path("inst", studyName)
if (!dir.exists(outputDir)) {
  dir.create(outputDir, recursive = TRUE)
}
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path(outputDir, paste0(studyName, "AnalysisSpecification.json"))
)