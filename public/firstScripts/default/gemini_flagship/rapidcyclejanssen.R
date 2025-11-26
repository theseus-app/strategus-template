################################################################################
# This script creates the Strategus analysis specifications for the
# "rapidcyclejanssen" study, based on the provided JSON settings.
#
# It uses the OHDSI Strategus and HADES packages to define the study design,
# including cohort definitions, analysis settings for CohortGenerator,
# CohortDiagnostics, and CohortMethod modules.
#
# Refer to the OHDSI Strategus documentation for more information:
# https://ohdsi.github.io/Strategus/
################################################################################
library(dplyr)
library(Strategus)

# --- Shared Resources Setup ---------------------------------------------------
# This section defines resources shared across different Strategus modules,
# such as cohort definitions and negative control concept sets.

# The base URL for the WebAPI instance. This is used to retrieve cohort and
# concept set definitions.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Annotation: Retrieving cohort definitions from WebAPI using IDs specified
# in `cohortDefinitions` from the Analysis Specifications.
# targetCohort: 1794126 ("target1")
# comparatorCohort: 1794132 ("comparator1")
# outcomeCohort: 1794131 ("outcome1")
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-numbering cohorts to a local, simple ID system (1, 2, 3) for use
# within the Strategus analysis. This simplifies referencing them later.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Annotation: Retrieving negative control concepts from the concept set specified
# in `negativeControlConceptSet` (ID: 1888110, Name: "negative").
# These concepts are then resolved and formatted into a cohort set for use as
# negative control outcomes in the CohortMethod analysis.
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
  # Assign unique cohort IDs starting from 101 to avoid collision with main cohorts.
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# --- Analysis-Specific Data Frames --------------------------------------------
# These data frames structure the inputs for the different analyses, particularly
# for the CohortMethod module, defining target-comparator-outcome relationships.

# A data frame listing the primary outcomes of interest.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Standard clean window for outcomes.

# Annotation: Defining the target and comparator cohorts for the CohortMethod analysis
# based on the `cohortDefinitions` section of the Analysis Specifications.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Annotation: The `covariateSelection` in the Analysis Specifications is empty,
# indicating that default covariate settings should be used without any specific
# concepts to include or exclude. This data frame is initialized as empty.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# --- Module Settings: CohortGenerator -----------------------------------------
# This module is responsible for generating the cohort instances from the
# definitions on the target CDM.
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

# --- Module Settings: CohortDiagnostics ---------------------------------------
# This module runs a set of diagnostics on the generated cohorts to assess their
# characteristics and quality.
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

# --- Module Settings: CohortMethod --------------------------------------------
# This section defines the settings for the comparative cohort analysis.

# Annotation: Study period is defined based on `getDbCohortMethodDataArgs.studyPeriods`.
# studyStartDate: 20210101, studyEndDate: null (represented as NA).
studyPeriods <- tibble(
  studyStartDate = c("20210101"),
  studyEndDate   = c(NA_character_)
)

# Annotation: Time-at-risk (TAR) windows are defined based on `createStudyPopArgs.timeAtRisks`.
# A label is created for each TAR for easy identification in the results.
timeAtRisks <- tibble(
  label = c("1-14d", "1-28d", "1-42d", "1-90d", "0-2d"),
  riskWindowStart  = c(1, 1, 1, 1, 0),
  startAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd  = c(14, 28, 42, 90, 2),
  endAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start")
)

# Annotation: Propensity score matching settings from `propensityScoreAdjustment.psSettings`.
# maxRatio: 100, caliper: 0.2, caliperScale: "standardized logit".
matchOnPsArgsList <- tibble(
  label = c("MaxRatio 100 Caliper 0.2 SL"),
  maxRatio  = c(100),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# Annotation: `stratifyByPsArgs` is null in the specifications, so this is empty.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c(),
)

# Build a list of propensity score configurations to iterate over.
# This structure allows for easily adding more PS adjustment strategies.
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

# --- Iteration over Analysis Settings to Create Analysis List -----------------
# This loop programmatically creates all combinations of analysis settings
# (e.g., study periods, TARS, PS methods) to be executed by CohortMethod.
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Set up PS adjustment arguments based on the current configuration.
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

      # Use default covariate settings as per the empty `covariateSelection`.
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
      
      # Define the Target, Comparator, and Outcome sets for the analysis.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Annotation: Settings for extracting data from the database, based on
      # `getDbCohortMethodDataArgs` from the Analysis Specifications.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        restrictToCommonPeriod = TRUE,
        firstExposureOnly = TRUE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "remove all",
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Annotation: Settings for creating the propensity score model, based on
      # `propensityScoreAdjustment.createPsArgs` from the Analysis Specifications.
      createPsArgs = CohortMethod::createCreatePsArgs(
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
          cvRepetitions = 10,
          startingVariance = 0.01,
          fold = 10
        )
      )

      # Default arguments for computing covariate balance.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Annotation: Settings for fitting the outcome model, based on
      # `fitOutcomeModelArgs` from the Analysis Specifications.
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
          seed = 1,
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          tolerance = 2e-07,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          fold = 10
        )
      )
      
      # Annotation: Settings for creating the final study population, based on
      # `createStudyPopArgs` from the Analysis Specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Append the fully specified analysis to the list of analyses to be run.
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

# Create the final CohortMethod module specifications using the analysis list.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# --- Create and Save Analysis Specifications --------------------------------
# This final section assembles all the defined module specifications into a
# single Strategus analysis specification JSON file.

analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Annotation: Save the final JSON specification file. The name and path are
# based on the `name` field ("rapidcyclejanssen") in the Analysis Specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "rapidcyclejanssen", "rapidcyclejanssenAnalysisSpecification.json")
)