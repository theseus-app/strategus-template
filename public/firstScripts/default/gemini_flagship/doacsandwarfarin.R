################################################################################
# This script uses the OHDSI Strategus framework to create a study analysis
# specification based on the provided JSON configuration.
#
# It defines the cohorts, analysis settings, and modules required to execute
# a comparative cohort study.
#
# For more information about Strategus and HADES modules, visit:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

# Load required libraries
library(dplyr)
library(Strategus)

# ======== Shared Resources Definition ========
# This section defines resources that are shared across different analysis modules,
# such as cohort definitions and concept sets.

# Set the base URL for the WebAPI instance. This is used to retrieve cohort
# and concept set definitions.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# --- Cohort Definitions ---
# Retrieve cohort definitions from ATLAS using their IDs.
# These IDs correspond to the 'cohortDefinitions' section of the Analysis Specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: "target1" from specifications
    1794132, # Comparator: "comparator1" from specifications
    1794131  # Outcome: "outcome1" from specifications
  ),
  generateStats = TRUE
)

# Re-number cohort IDs for internal consistency within the Strategus study.
# This mapping simplifies referencing cohorts in the analysis settings.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1 # Target
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2 # Comparator
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3 # Outcome

# --- Negative Control Outcome Cohorts ---
# Retrieve the concept set for negative controls and resolve it to a list of concepts.
# These concepts will be used to generate negative control outcome cohorts.
# The concept set ID comes from 'negativeControlConceptSet' in the specifications.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Corresponds to "negative" concept set
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


# Sanity check to ensure no cohort IDs are duplicated across different sets.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# ======== Analysis-Specific Data Frames ========
# These data frames organize cohorts and parameters for the analysis modules.

# Define the primary outcome(s) for the study.
# Corresponds to 'outcomeCohort' in the specifications.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Filter for the re-numbered outcome cohort
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Standard clean window for outcome definition

# Define the target and comparator cohort pairs for the CohortMethod analysis.
# Corresponds to 'targetCohort' and 'comparatorCohort' in the specifications.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Define concepts to exclude from covariate construction.
# In this case, the 'covariateSelection.conceptsToExclude' section in the
# specifications is empty, so we create an empty data frame.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# ======== Module Specifications ========
# This section defines the settings for each Strategus module to be used in the analysis.

# --- CohortGeneratorModule ---
# This module is responsible for generating all the cohort instances needed for the study.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Define the main cohorts (Target, Comparator, Outcome) as a shared resource.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Define the negative control cohorts as a shared resource.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
# Create the module specification for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# --- CohortDiagnosticsModule ---
# This module runs a set of diagnostics on the generated cohorts to assess their quality.
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

# Define study period(s).
# Based on 'getDbCohortMethodDataArgs.studyPeriods' in the specifications.
studyPeriods <- tibble(
  studyStartDate = c("20101019"),
  studyEndDate   = c("20181231")
)

# Define Time-at-Risks (TARs).
# Based on 'createStudyPopArgs.timeAtRisks' in the specifications.
timeAtRisks <- tibble(
  label = c("1d-start to 5d-before-end", "On treatment", "ITT"),
  riskWindowStart  = c(1, 1, 1),
  startAnchor = c("cohort start", "cohort start", "cohort start"),
  riskWindowEnd  = c(5, 0, 99999),
  endAnchor = c("cohort end", "cohort end", "cohort start")
)

# Define Propensity Score matching settings.
# Based on 'propensityScoreAdjustment.psSettings' in the specifications.
matchOnPsArgsList <- tibble(
  label = c("1-to-1 matching", "Variable ratio matching"),
  maxRatio  = c(1, 100),
  caliper = c(0.2, 0.2),
  caliperScale  = c("standardized logit", "standardized logit")
)

# Define Propensity Score stratification settings.
# This is empty as it is not specified in the 'propensityScoreAdjustment.psSettings'.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c(),
)

# Consolidate all PS adjustment settings into a single list for iteration.
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


# ======== Build CM Analysis List ========
# Iterate through all combinations of study periods, TARs, and PS settings
# to create a list of 'cmAnalysis' objects.
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

      # Use default covariate settings as none are specified.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )
      
      # Combine the main outcome and the negative control outcomes.
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
      
      # Define the Target, Comparator, and Outcome list for CohortMethod.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude concepts based on the 'covariateSelection' specification (currently empty).
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Define arguments for retrieving data from the database.
      # Based on 'getDbCohortMethodDataArgs' in the specifications.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = TRUE,
        removeDuplicateSubjects = "remove all",
        washoutPeriod = 0,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Define arguments for creating the study population.
      # Based on 'createStudyPopArgs' in the specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = FALSE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1
      )

      # Define arguments for creating the propensity score model.
      # Based on 'propensityScoreAdjustment.createPsArgs' in the specifications.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",
          cvType = "auto",
          fold = 10,
          resetCoefficients = TRUE,
          tolerance = 2e-07,
          cvRepetitions = 10,
          startingVariance = 0.01
        )
      )
      
      # Standard settings for covariate balance computation.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Define arguments for fitting the outcome model.
      # Based on 'fitOutcomeModelArgs' in the specifications.
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
          cvType = "auto",
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          tolerance = 2e-07,
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet"
        )
      )

      # Combine all settings into a single cmAnalysis object.
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

# ======== Assemble and Save Analysis Specifications ========
# Combine all shared resources and module specifications into a single
# analysis specification object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the final analysis specifications to a JSON file.
# The file path is based on the 'name' property in the specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("doacsandwarfarinAnalysisSpecification.json")
)