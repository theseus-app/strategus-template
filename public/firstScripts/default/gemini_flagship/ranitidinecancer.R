################################################################################
# This script generates the analysis specifications for a Strategus study.
# It is based on the settings provided in a JSON file and a template script.
#
# Refer to the OHDSI Strategus documentation for more information:
# https://ohdsi.github.io/Strategus/
#
# More information about HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

library(dplyr)
library(Strategus)
library(ROhdsiWebApi)

# ==> Analysis Specifications <==
# This section defines the settings based on the provided JSON.

# --- Shared Resources ---
# These resources are used across different modules in the analysis.

# Define the baseUrl for the WebAPI from which to retrieve cohort and concept set definitions.
# Note: This should be the address of your organization's OHDSI ATLAS instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Provide the cohort IDs for the target, comparator, and outcome cohorts.
# These will be fetched from the WebAPI.
targetCohortId <- 1794126
targetCohortName <- "target1"
comparatorCohortId <- 1794132
comparatorCohortName <- "comparator1"
outcomeCohortIds <- c(1794131)
outcomeCohortNames <- c("outcome1")

# Negative Control settings
# ID of the concept set containing concepts for negative controls.
negativeControlConceptSetId <- 1888110

# Export cohort definitions from WebAPI
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    targetCohortId,
    comparatorCohortId,
    outcomeCohortIds
  ),
  generateStats = TRUE
)

# Rename cohorts for internal consistency within the Strategus framework.
# It is a convention to use small, consecutive integers for cohort IDs.
# Target -> 1, Comparator -> 2, Outcome(s) -> 3, 4, ...
cohortDefinitionSet <- cohortDefinitionSet %>%
  mutate(cohortId = case_when(
    cohortId == targetCohortId ~ 1,
    cohortId == comparatorCohortId ~ 2,
    cohortId == outcomeCohortIds[1] ~ 3,
    # Add more lines here if there are multiple outcome cohorts
    TRUE ~ cohortId
  ))

# Define negative control outcomes from the specified concept set.
# These are outcomes not believed to be caused by the exposure.
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
  # Assign cohort IDs starting from 101 to avoid collision with T, C, O cohorts.
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Sanity check to ensure no cohort IDs are duplicated across the different sets.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# --- Data Frames for Analysis Combinations ---
# These data frames define the combinations of T-C pairs, outcomes, and analysis settings.

# Outcomes of interest for the study.
oList <- tibble(
  outcomeCohortId = c(3),
  outcomeCohortName = outcomeCohortNames,
  # The clean window is the number of days to look back for the outcome before index.
  # If the outcome occurs in this window, the person is not included for this outcome.
  cleanWindow = 365
)

# Target and Comparator pairs for the CohortMethod analysis.
cmTcList <- tibble(
  targetCohortId = 1,
  targetCohortName = targetCohortName,
  comparatorCohortId = 2,
  comparatorCohortName = comparatorCohortName
)

# Concepts to exclude from covariates. Based on the specification, this is empty.
# By default, CohortMethod will exclude concepts related to the target and comparator.
excludedCovariateConcepts <- tibble(
  conceptId = numeric(),
  conceptName = character()
)


# ==> Module Settings <==
# This section defines the settings for each individual Strategus module.

# --- CohortGeneratorModule ---
# This module is responsible for generating the cohorts defined above.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Define the shared resource for the primary cohort definitions (T, C, O).
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Define the shared resource for the negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Specify the module settings for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# --- CohortDiagnosticsModule ---
# This module runs diagnostics on the generated cohorts.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  # Run diagnostics on all defined cohorts (T, C, O, and negative controls).
  cohortIds = c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId),
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

# Study periods: Define specific time windows for the analysis.
# An empty string for start/end date means no date restriction.
studyPeriods <- tibble(
  studyStartDate = c(""), # YYYYMMDD
  studyEndDate   = c("")  # YYYYMMDD
)

# Time-at-risk (TAR) windows. Four TARs are defined as per the specifications.
timeAtRisks <- tibble(
  label = c("1-99999d from start", "365-99999d from start", "1d from start to end", "365d from start to end"),
  riskWindowStart  = c(1, 365, 1, 365),
  startAnchor = c("cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd  = c(99999, 99999, 0, 0),
  endAnchor = c("cohort start", "cohort start", "cohort end", "cohort end"),
  minDaysAtRisk = c(1, 1, 1, 1)
)

# Propensity Score settings - Matching on PS.
matchOnPsArgsList <- tibble(
  label = c("1-to-1 Matching", "Variable Ratio Matching"),
  maxRatio  = c(1, 10),
  caliper = c(0.2, 0.2),
  caliperScale  = c("standardized logit", "standardized logit")
)

# Propensity Score settings - Stratifying by PS.
stratifyByPsArgsList <- tibble(
  label = c("10 Strata"),
  numberOfStrata  = c(10),
  baseSelection = c("all")
)

# Build a single list of all PS configurations to iterate over.
psConfigList <- list()

# Add matching configurations.
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

# Add stratification configurations.
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

# Add configuration for analysis with no PS adjustment (crude).
psConfigList[[length(psConfigList) + 1]] <- list(
  method = "none",
  label  = "No PS Adjustment",
  params = list()
)


# --- Generating the Full Set of Analysis Specifications ---
# Iterate through all combinations of settings to create a list of analyses.
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Set PS adjustment arguments based on the current configuration.
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
      } else {
        stop("Unknown PS method specified.")
      }
      
      # Use default covariate settings.
      # This will create a standard set of covariates (demographics, conditions, drugs, etc.).
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create the list of outcomes, including both outcomes of interest and negative controls.
      outcomeList <- append(
        # Outcomes of Interest
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA
          )
        }),
        # Negative Control Outcomes
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )
      
      # Create the list of T-C-O combinations.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Settings for fetching data from the database.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        # As per spec, no maximum cohort size.
        maxCohortSize = 0,
        # As per spec, do not restrict to a common period of observation.
        restrictToCommonPeriod = FALSE,
        # As per spec, keep all exposures, not just the first.
        firstExposureOnly = FALSE,
        # As per spec, require 365 days of observation prior to cohort entry.
        washoutPeriod = 365,
        # As per spec, handle duplicate subjects by keeping the first entry.
        removeDuplicateSubjects = "keep first",
        covariateSettings = covariateSettings
      )

      # Settings for creating the propensity score model.
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",
          cvType = "auto",
          seed = 1,
          resetCoefficients = TRUE,
          tolerance = 2e-07,
          # As per spec, 10 folds and 10 repetitions for cross-validation.
          fold = 10,
          cvRepetitions = 10,
          startingVariance = 0.01
        )
      )

      # Settings for computing covariate balance.
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Settings for fitting the outcome model.
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        # As per spec, the Cox model should be stratified by the matched/stratified sets.
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
          # As per spec, 10 folds and 10 repetitions for cross-validation.
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet"
        )
      )
      
      # Settings for creating the final study population for outcome analysis.
      # These are applied after PS adjustment and for each TAR.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        # No additional washout period at this stage.
        washoutPeriod = 0,
        # As per spec, keep all subjects.
        removeDuplicateSubjects = "keep all",
        # As per spec, do not censor at a new risk window.
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        # TAR settings are taken from the outer loop.
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999 # Default
      )

      # Append the complete set of analysis settings to the list.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(studyStartDate == "", "unbounded", studyStartDate),
          ifelse(studyEndDate == "", "unbounded", studyEndDate),
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

# Create the final module specifications for CohortMethod.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# ==> Final Assembly and Output <==
# Combine all module specifications into a single analysis specification object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Add shared resources (cohorts) that modules can use.
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  # Add the specifications for each module.
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the final analysis specifications to a JSON file.
# This file will be used by Strategus to execute the study.
studyName <- "ranitidinecancer"
outputFolder <- file.path("inst", studyName)
if (!dir.exists(outputFolder)) {
  dir.create(outputFolder, recursive = TRUE)
}
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path(outputFolder, paste0(studyName, "AnalysisSpecification.json"))
)