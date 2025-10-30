################################################################################
# CreateStrategusAnalysisSpecification.R
# This script constructs Strategus analysis specifications tailored to the
# analysis settings provided in <Analysis Specifications>, using the structure
# shown in <Template>. Detailed annotations are included to explain how each
# setting is applied.
#
# Notes:
# - Be sure your R session has access to the OHDSI HADES ecosystem packages.
# - This script only prepares the analysis specifications JSON. Execution of
#   the analysis is done separately by Strategus using those specifications.
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# WebAPI where the cohort and concept set definitions exist
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Pull the T (target), C (comparator), and O (outcome) cohorts from ATLAS by ID.
# IDs and names are taken directly from <Analysis Specifications>.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to small consecutive ids used downstream by CohortMethod.
# 1 -> target, 2 -> comparator, 3 -> primary outcome
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# Concept set ID taken from <Analysis Specifications>:
# "negativeControlConceptSet": { "id": 1888110, "name": "negative" }
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
  # Standardize names to what the negative control shared resource expects
  dplyr::rename(outcomeConceptId = .data$conceptId,
                cohortName = .data$conceptName) %>%
  # Assign unique negative control cohortIds starting at 101 to avoid clashes
  dplyr::mutate(cohortId = dplyr::row_number() + 100) %>%
  dplyr::select(.data$cohortId, .data$cohortName, .data$outcomeConceptId)

# Sanity check: ensure no overlapping cohortIds between study cohorts and NCs
if (any(cohortDefinitionSet$cohortId %in% negativeControlOutcomeCohortSet$cohortId)) {
  stop("*** Error: duplicate cohort IDs found between primary cohorts and negative controls ***")
}

# Create some data frames to hold the cohorts used in each analysis ------------
# Outcomes (primary outcome + cleaning window if desired)
oList <- cohortDefinitionSet %>%
  dplyr::filter(.data$cohortId == 3) %>%
  dplyr::mutate(outcomeCohortId = .data$cohortId,
                outcomeCohortName = .data$cohortName) %>%
  dplyr::select(.data$outcomeCohortId, .data$outcomeCohortName) %>%
  dplyr::mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis (names exactly as specified)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  stringsAsFactors = FALSE
)

# Covariate selection from <Analysis Specifications> is empty (ids null),
# so we do not include or exclude any specific concepts here.
# You can populate these if you later define concepts to include/exclude.
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(0),
  conceptName = character(0)
)

# Optional placeholder for included covariates (not used by default)
# includedCovariateConcepts <- data.frame(
#   conceptId = numeric(0),
#   conceptName = character(0)
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Share the cohort definitions (T, C, and O) with all modules
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

# Share negative control outcomes with downstream modules
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Tell the CohortGenerator module to generate stats for the cohorts
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# Run common diagnostics on the primary (non-negative-control) cohorts
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
# Study periods from <Analysis Specifications> $getDbCohortMethodDataArgs.studyPeriods
# Each row creates a separate CM analysis over the defined time window.
studyPeriods <- tibble::tibble(
  studyStartDate = c("20100101", "20120101"), # YYYYMMDD
  studyEndDate   = c("20191231", "20191231")  # YYYYMMDD
)

# Time-at-risk settings from <Analysis Specifications> $createStudyPopArgs.timeAtRisks
# We preserve the same anchors and durations, and add descriptive labels.
timeAtRisks <- tibble::tibble(
  label = c(
    "TAR1: cohort start to cohort end",
    "TAR2: 0 to 9999 days after cohort start"
  ),
  riskWindowStart  = c(0, 0),
  startAnchor      = c("cohort start", "cohort start"),
  riskWindowEnd    = c(0, 9999),
  endAnchor        = c("cohort end", "cohort start"),
  minDaysAtRisk    = c(1, 1) # both TARs specify minDaysAtRisk = 1
)

# Propensity Score settings from <Analysis Specifications> $propensityScoreAdjustment
# - One stratification configuration
# - One matching configuration
stratifyByPsArgsList <- tibble::tibble(
  label = c("PS-stratify: 5 strata, base=all"),
  numberOfStrata = c(5),
  baseSelection  = c("all") # "all" | "target" | "comparator"
)

matchOnPsArgsList <- tibble::tibble(
  label = c("PS-match: caliper=0.2 (standardized logit), maxRatio=0"),
  maxRatio  = c(0),
  caliper   = c(0.2),
  caliperScale = c("standardized logit") # "propensity score" | "standardized" | "standardized logit"
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Convert "match on PS" rows to config entries
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

# Convert "stratify by PS" rows to config entries
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

# Iterate through all analysis setting combinations ----------------------------
cmAnalysisList <- list()
targetComparatorOutcomesList <- list() # Will be filled once (same for all analyses)
analysisId <- 1

# Prepare outcome objects (primary + negative controls) once; re-used in all T/C sets
outcomeList <- append(
  # Primary outcomes of interest
  lapply(seq_len(nrow(oList)), function(i) {
    CohortMethod::createOutcome(
      outcomeId = oList$outcomeCohortId[i],
      outcomeOfInterest = TRUE,
      trueEffectSize = NA,
      priorOutcomeLookback = 99999
    )
  }),
  # Negative control outcomes
  lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
    CohortMethod::createOutcome(
      outcomeId = i,
      outcomeOfInterest = FALSE,
      trueEffectSize = 1
    )
  })
)

# Create the T/C/O bundle(s) used by the CohortMethod module
for (i in seq_len(nrow(cmTcList))) {
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
  )
}

# Default covariate settings for PS modeling and balance computation.
# Add descendants to exclude helps ensure clean removal when exclusions are provided.
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE
)

# Settings mapping for createPsArgs from <Analysis Specifications>
createPsArgs <- CohortMethod::createCreatePsArgs(
  maxCohortSizeForFitting = 250000,
  errorOnHighCorrelation = TRUE,
  stopOnError = FALSE, # Let Strategus continue even if a PS model fails to fit
  estimator = "att",
  prior = Cyclops::createPrior(
    priorType = "laplace",
    exclude = c(0),
    useCrossValidation = TRUE
  ),
  control = Cyclops::createControl(
    tolerance = 2e-07,
    cvType = "auto",
    fold = 10,
    cvRepetitions = 10,
    noiseLevel = "silent",
    resetCoefficients = TRUE,
    startingVariance = 0.01,
    seed = 1
  )
)

# Covariate balance args (shared and per analysis)
computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
  maxCohortSize = 250000,
  covariateFilter = NULL
)
computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
  maxCohortSize = 250000,
  covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
)

# Outcome model settings from <Analysis Specifications> $fitOutcomeModelArgs
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
    tolerance = 2e-07,
    cvType = "auto",
    fold = 10,
    cvRepetitions = 10,
    noiseLevel = "quiet",
    resetCoefficients = TRUE,
    startingVariance = 0.01,
    seed = 1
  )
)

# Loop over all combinations of study periods, TARs, and PS configurations
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Decide whether this configuration uses matching or stratification
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

      # Map <Analysis Specifications> getDbCohortMethodDataArgs into CM call.
      # Using restrictToCommonPeriod = TRUE because the analysis defines explicit study periods.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Map <Analysis Specifications> createStudyPopArgs (applied per TAR)
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
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t]
      )

      # Append the settings to Analysis List
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

# Finalize CohortMethod module specifications
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
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications JSON using the study name from <Analysis Specifications>
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "corazon", "corazonAnalysisSpecification.json")
)