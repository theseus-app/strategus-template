################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script builds a complete Strategus analysis specification using the
# OHDSI HADES modules. It follows the provided Template and applies the exact
# settings from <Analysis Specifications> for:
# - Cohorts (target/comparator/outcome)
# - Negative controls
# - Study periods, time-at-risk, and population construction
# - Propensity score estimation and adjustment configurations
# - Outcome model fitting
#
# Detailed annotations are included throughout to explain how the settings map
# to the specification and how Strategus will consume them.
################################################################################

# Core packages
library(dplyr)
library(tibble)

# Strategus and HADES modules
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(ParallelLogger)

# Shared Resources -------------------------------------------------------------
# Base URL of the Atlas/WebAPI source where the cohorts and concept sets live.
# This follows the template's default. Update if your assets are elsewhere.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export the exact target/comparator/outcome cohorts specified.
# We re-number these to 1 (target), 2 (comparator), 3 (outcome) for internal use.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to 1, 2, 3 for a clean internal mapping across modules
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# Pull the concept set for negative control outcomes and resolve it to concepts.
# We then map each concept to a 'negative control outcome cohort' record with a
# unique cohortId starting from 101+ to avoid overlap with 1/2/3 used above.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # EXACT id from the specification: "negative"
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
  ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
  dplyr::rename(outcomeConceptId = "conceptId",
                cohortName = "conceptName") %>%
  dplyr::mutate(cohortId = dplyr::row_number() + 100) %>% # 101, 102, ...
  dplyr::select(cohortId, cohortName, outcomeConceptId)

# Safety check to ensure no cohortId collisions between primary cohorts and NCs
if (any(negativeControlOutcomeCohortSet$cohortId %in% cohortDefinitionSet$cohortId)) {
  stop("*** Error: duplicate cohort IDs found between primary and negative control cohorts ***")
}

# Create small data frames to hold the cohorts used across modules -------------
# Outcomes (primary outcomes list used by CohortMethod)
oList <- cohortDefinitionSet %>%
  dplyr::filter(.data$cohortId == 3) %>%
  dplyr::mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  dplyr::select(outcomeCohortId, outcomeCohortName) %>%
  dplyr::mutate(cleanWindow = 365) # commonly used outcome clean window; not altering studyPop prior lookback

# Target and Comparator for CohortMethod analysis (using EXACT cohort ids/names)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  stringsAsFactors = FALSE
)

# Covariate selection placeholders from spec (both empty):
# conceptsToInclude: [{id: null, name:""}]
# conceptsToExclude: [{id: null, name:""}]
# We'll keep default covariate settings and not force includes/excludes.
includedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# CohortGeneratorModule --------------------------------------------------------
# Shared resources for cohorts and negative control outcomes
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Cohort definitions (shared resource)
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Negative control outcome cohorts (shared resource)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications: generateStats = TRUE per template
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# Note: We run diagnostics for the primary cohort set (IDs 1,2,3). Negative controls
# are not required here but could be added if desired.
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
# Translate analysis specification to CM configurations.

# StudyPeriods (exact from specification: 20171201 to 20231231)
# We allow vectorization if more periods are added, but currently it's 1.
studyPeriods <- tibble::tibble(
  studyStartDate = c(20171201),
  studyEndDate   = c(20231231)
)

# Time-at-risk windows (exact from specification)
# One TAR: start=0 at "cohort start" to end=0 at "cohort end", with minDaysAtRisk = 1
timeAtRisks <- tibble::tibble(
  label = c("TAR1"),
  riskWindowStart = c(0),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(0),
  endAnchor = c("cohort end"),
  minDaysAtRisk = c(1)
)

# PS adjustment strategy configurations (exactly two per specification)
# 1) Match on PS with maxRatio=1, caliper=0.2, caliperScale="standardized logit"
matchOnPsArgsList <- tibble::tibble(
  label = c("PS-Match-1x-0.2-slogit"),
  maxRatio = c(1),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# 2) Stratify by PS with numberOfStrata=5, baseSelection="all"
stratifyByPsArgsList <- tibble::tibble(
  label = c("PS-Stratify-5-all"),
  numberOfStrata = c(5),
  baseSelection = c("all")
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Convert each row of the match list to a config
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

# Convert each row of the stratify list to a config
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
targetComparatorOutcomesList <- list()
analysisId <- 1

# Prepare default covariate settings:
# - We use FeatureExtraction defaults and allow descendants for excludes.
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE
)

# Create the outcome set: primary outcome(s) + negative controls
outcomeList <- append(
  # Primary outcome(s)
  lapply(seq_len(nrow(oList)), function(i) {
    CohortMethod::createOutcome(
      outcomeId = oList$outcomeCohortId[i],
      outcomeOfInterest = TRUE,
      trueEffectSize = NA,
      priorOutcomeLookback = 99999 # exact from createStudyPopArgs in specification
    )
  }),
  # Negative control outcomes (each outcomeOfInterest = FALSE)
  lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
    CohortMethod::createOutcome(
      outcomeId = i,
      outcomeOfInterest = FALSE,
      trueEffectSize = 1
    )
  })
)

# TargetComparatorOutcomes: build for all T/C pairs in cmTcList
for (i in seq_len(nrow(cmTcList))) {
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    excludedCovariateConceptIds = excludedCovariateConcepts$conceptId # empty per spec
  )
}

# Loop over study period(s), TAR(s), and PS configuration(s)
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Build PS adjustment args for the current PS method
      matchOnPsArgs <- NULL
      stratifyByPsArgs <- NULL

      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale
        )
      } else if (psCfg$method == "stratify") {
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Database extraction args (get DB CM data) ------------------------------
      # From spec: studyPeriods (start/end) and maxCohortSize = 0, with default covariates.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # PS estimation args (exact from specification) --------------------------
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = FALSE,
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
          resetCoefficients = FALSE,
          startingVariance = 0.01
        )
      )

      # Covariate balance computations (kept standard as not specified)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model settings (exact from specification) ----------------------
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = FALSE,
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
          startingVariance = 0.01
        )
      )

      # Study population settings (exact from specification) -------------------
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 365,
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

      # Aggregate into a CmAnalysis object ------------------------------------
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

# Build the CohortMethod module specifications --------------------------------
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
# This combines the shared resources and module specifications into a single
# object that Strategus can serialize and execute.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Persist to JSON for downstream execution ------------------------------------
# Use the EXACT study name from the specification: "semaglutideandnaion"
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "semaglutideandnaion", "semaglutideandnaionAnalysisSpecification.json")
)