################################################################################
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
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)

# Shared Resources -------------------------------------------------------------
# Base URL for the WebAPI to retrieve cohort and concept set definitions.
# This value is not provided in the Analysis Specifications, using a common default.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# --- Cohort Definitions ---
# Retrieve cohort definitions from WebAPI based on IDs specified in Analysis Specifications.
# The 'id' values from "cohortDefinitions" in Analysis Specifications are used here.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: "target1"
    1794132, # Comparator: "comparator1"
    1794131  # Outcome: "outcome1"
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal consistency within the study.
# Target cohort ID 1794126 is mapped to internal ID 1.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# Comparator cohort ID 1794132 is mapped to internal ID 2.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# Outcome cohort ID 1794131 is mapped to internal ID 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# --- Negative control outcomes ---
# Retrieve negative control outcome concept set definition from WebAPI.
# The 'id' from "negativeControlConceptSet" in Analysis Specifications is used.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Negative Control Concept Set ID: "negative"
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
  mutate(cohortId = row_number() + 100) %>% # Assign unique internal IDs for negative controls, starting from 101.
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs to prevent conflicts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts for specific analysis roles. ---------------
# Outcomes: Extract outcome cohort information.
# Uses the re-numbered outcome cohort ID (3) and its name "outcome1".
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not specified in Analysis Specifications.

# Target and Comparator for the CohortMethod analysis.
# Uses the re-numbered target (1) and comparator (2) cohort IDs and names.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# For the CohortMethod LSPS we'll need to exclude specific concepts as covariates.
# Based on "covariateSelection.conceptsToExclude" from Analysis Specifications,
# which is an empty array, no specific concepts are excluded beyond default behavior.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# Based on "covariateSelection.conceptsToInclude" from Analysis Specifications,
# which is an empty array, no specific concepts are included.
# includedCovariateConcepts <- data.frame(
#   conceptId = integer(0),
#   conceptName = character(0)
# )

# CohortGeneratorModule --------------------------------------------------------
# Configure the CohortGeneratorModule to create and manage cohorts.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Configure negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Not specified in analysis spec, using default
  detectOnDescendants = TRUE # Not specified in analysis spec, using default
)
# Create module specifications for cohort generation, generating statistics.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Not specified in analysis spec, using default
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# Configure the CohortDiagnosticsModule to run various diagnostic checks on cohorts.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # All defined cohorts will be diagnosed.
  runInclusionStatistics = TRUE, # Default setting.
  runIncludedSourceConcepts = TRUE, # Default setting.
  runOrphanConcepts = TRUE, # Default setting.
  runTimeSeries = FALSE, # Default setting.
  runVisitContext = TRUE, # Default setting.
  runBreakdownIndexEvents = TRUE, # Default setting.
  runIncidenceRate = TRUE, # Default setting.
  runCohortRelationship = TRUE, # Default setting.
  runTemporalCohortCharacterization = TRUE, # Default setting.
  minCharacterizationMean = 0.01 # Default setting.
)

# CohortMethodModule -----------------------------------------------------------

# --- Study Periods ---
# Define the study start and end dates.
# From "getDbCohortMethodDataArgs.studyPeriods" in Analysis Specifications.
studyPeriods <- tibble(
  studyStartDate = c("20100101"), # YYYYMMDD
  studyEndDate   = c("20191231")  # YYYYMMDD
)

# --- Time-at-risks (TARs) ---
# Define the time-at-risk windows for the outcomes.
# From "createStudyPopArgs.timeAtRisks" in Analysis Specifications.
# A label is added for easier identification in analysis descriptions.
timeAtRisks <- tibble(
  label = c("1d_to_cohort_end"), # Custom label for this TAR.
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end"),
  minDaysAtRisk = c(1) # From "createStudyPopArgs.timeAtRisks.minDaysAtRisk".
)

# --- Propensity Score settings - match on PS ---
# Based on "propensityScoreAdjustment.psSettings.matchOnPsArgs" which is null,
# this list is left empty, meaning no matching on PS will be performed.
matchOnPsArgsList <- tibble(
  label = character(0),
  maxRatio  = numeric(0),
  caliper = numeric(0),
  caliperScale  = character(0)
)

# --- Propensity Score settings - stratify by PS ---
# Based on "propensityScoreAdjustment.psSettings.stratifyByPsArgs" in Analysis Specifications.
stratifyByPsArgsList <- tibble(
  label = c("Stratify_5_All"), # Custom label for this PS stratification setting.
  numberOfStrata  = c(5), # From "propensityScoreAdjustment.psSettings.stratifyByPsArgs.numberOfStrata".
  baseSelection = c("all") # From "propensityScoreAdjustment.psSettings.stratifyByPsArgs.baseSelection".
)

# Build a single PS configuration list (each entry has: method, label, params).
# This list will be iterated over to create different CM analyses.
psConfigList <- list()

# Convert "match on PS" settings from tibble to a list of configurations.
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

# Convert "stratify by PS" settings from tibble to a list of configurations.
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


# Iterate through all analysis setting combinations (study periods, TARs, PS methods).
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Configure PS adjustment arguments based on the current PS configuration.
      if (psCfg$method == "match") {
        # createMatchOnPsArgs is NULL because "matchOnPsArgs" was null in spec.
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default setting.
          stratificationColumns = c() # Default setting.
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        # createStratifyByPsArgs from "propensityScoreAdjustment.psSettings.stratifyByPsArgs".
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default setting.
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Define covariate settings.
      # "covariateSelection" in Analysis Specifications is empty, so default settings are used.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default setting.
      )

      # Create a list of outcome definitions for CohortMethod.
      # Includes both outcomes of interest and negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i], # Outcome cohort ID (e.g., 3).
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # No true effect size specified for outcome of interest.
            priorOutcomeLookback = 99999 # From "createStudyPopArgs.priorOutcomeLookBack".
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i, # Negative control cohort ID (e.g., 101, 102...).
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # Default true effect size for negative controls.
          )
        })
      )

      # Create target-comparator-outcome combinations for analysis.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i], # Target cohort ID (e.g., 1).
          comparatorId = cmTcList$comparatorCohortId[i], # Comparator cohort ID (e.g., 2).
          outcomes = outcomeList,
          # Exclude covariate concepts. "covariateSelection.conceptsToExclude" is empty.
          # The template had placeholders for target/comparator concepts, but these are not in spec.
          # Only `excludedCovariateConcepts$conceptId` will be used which is an empty vector.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Arguments for retrieving data from the CDM.
      # From "getDbCohortMethodDataArgs" in Analysis Specifications.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Template default, not explicitly in spec's getDbCohortMethodDataArgs.
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From "getDbCohortMethodDataArgs.maxCohortSize".
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # From "propensityScoreAdjustment.createPsArgs" in Analysis Specifications.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From "createPsArgs.maxCohortSizeForFitting".
        errorOnHighCorrelation = TRUE, # From "createPsArgs.errorOnHighCorrelation".
        stopOnError = FALSE, # Default setting.
        estimator = "att", # Default setting.
        prior = Cyclops::createPrior(
          priorType = "laplace", # From "createPsArgs.prior.priorType".
          exclude = c(0), # Default setting.
          useCrossValidation = TRUE # From "createPsArgs.prior.useCrossValidation".
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", # From "createPsArgs.control.noiseLevel".
          cvType = "auto", # From "createPsArgs.control.cvType".
          seed = 1, # Default setting.
          resetCoefficients = TRUE, # From "createPsArgs.control.resetCoefficients".
          tolerance = 2e-07, # From "createPsArgs.control.tolerance".
          cvRepetitions = 10, # From "createPsArgs.control.cvRepetitions".
          startingVariance = 0.01 # From "createPsArgs.control.startingVariance".
        )
      )

      # Arguments for computing shared covariate balance.
      # Not explicitly in Analysis Specifications, using template defaults.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default setting.
        covariateFilter = NULL # Default setting.
      )
      # Arguments for computing covariate balance.
      # Not explicitly in Analysis Specifications, using template defaults.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default setting.
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default setting.
      )

      # Arguments for fitting the outcome model.
      # From "fitOutcomeModelArgs" in Analysis Specifications.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From "fitOutcomeModelArgs.modelType".
        stratified = TRUE, # From "fitOutcomeModelArgs.stratified".
        useCovariates = FALSE, # From "fitOutcomeModelArgs.useCovariates".
        inversePtWeighting = FALSE, # From "fitOutcomeModelArgs.inversePtWeighting".
        prior = Cyclops::createPrior(
          priorType = "laplace", # From "fitOutcomeModelArgs.prior.priorType".
          useCrossValidation = TRUE # From "fitOutcomeModelArgs.prior.useCrossValidation".
        ),
        control = Cyclops::createControl(
          cvType = "auto", # From "fitOutcomeModelArgs.control.cvType".
          seed = 1, # Default setting.
          resetCoefficients = TRUE, # From "fitOutcomeModelArgs.control.resetCoefficients".
          startingVariance = 0.01, # From "fitOutcomeModelArgs.control.startingVariance".
          tolerance = 2e-07, # From "fitOutcomeModelArgs.control.tolerance".
          cvRepetitions = 10, # From "fitOutcomeModelArgs.control.cvRepetitions".
          noiseLevel = "quiet" # From "fitOutcomeModelArgs.control.noiseLevel".
        )
      )

      # Arguments for creating the study population.
      # From "createStudyPopArgs" in Analysis Specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From "createStudyPopArgs.restrictToCommonPeriod".
        firstExposureOnly = FALSE, # From "createStudyPopArgs.firstExposureOnly".
        washoutPeriod = 0, # From "createStudyPopArgs.washoutPeriod".
        removeDuplicateSubjects = "keep all", # From "createStudyPopArgs.removeDuplicateSubjects".
        censorAtNewRiskWindow = FALSE, # From "createStudyPopArgs.censorAtNewRiskWindow".
        removeSubjectsWithPriorOutcome = TRUE, # From "createStudyPopArgs.removeSubjectsWithPriorOutcome".
        priorOutcomeLookback = 99999, # From "createStudyPopArgs.priorOutcomeLookBack".
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From "createStudyPopArgs.timeAtRisks.minDaysAtRisk".
        maxDaysAtRisk = 99999 # Default setting.
      )

      # Append the current set of analysis settings to the CohortMethod analysis list.
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

# Create the CohortMethodModule specifications using the generated analysis list.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # No analyses specified for exclusion.
  refitPsForEveryOutcome = FALSE, # Default setting.
  refitPsForEveryStudyPopulation = FALSE, # Default setting.
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default diagnostic thresholds.
)

# Create the overall analysis specifications object ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Add shared resources (cohort definitions, negative controls).
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  # Add module specifications in the order they should be executed.
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path uses the name from "name" in Analysis Specifications ("corazon").
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "corazon", "corazonAnalysisSpecification.json")
)