################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script builds a Strategus analysis specification using the OHDSI
# HADES modules as a template, applying the settings provided in the
# Analysis Specifications. The goal is to make it easy to understand how each
# section is configured and how it maps to the Strategus pipeline.
#
# See "UsingThisTemplate.md" for more details about Strategus workflows.
# For module details see:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

# Load packages used in this script
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# 1) Source of cohorts and concept sets (ATLAS/WebAPI)
# 2) Export and re-number cohort IDs so the T, C, O are 1, 2, 3 respectively
# 3) Resolve Negative Control outcomes from a concept set
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions (Target, Comparator, Outcome) -----------------------------
# - These IDs are taken directly from the Analysis Specifications
# - We immediately re-number them to 1 / 2 / 3 internally to have consistent IDs
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to 1, 2, 3 for target/comparator/outcome
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# - From Analysis Specifications: negativeControlConceptSet id = 1888110 (name: "negative")
# - Resolve to concepts and assign unique cohortIds (starting at 101)
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
  ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
  rename(
    outcomeConceptId = "conceptId",
    cohortName = "conceptName"
  ) %>%
  mutate(cohortId = dplyr::row_number() + 100) %>% # Negative controls start at 101, 102, ...
  select(cohortId, cohortName, outcomeConceptId)

# Sanity check: ensure no cohortId collision between "true" cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# Outcome list for CohortMethod ------------------------------------------------
# - Outcome(s) of interest: use cohortId == 3
# - We can set a "cleanWindow" (washout of prior outcomes for IR calculation in diagnostics)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target-Comparator (single pair for this study)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate selection (Concepts to include/exclude) ----------------------------
# From Analysis Specifications (both empty here)
includedCovariateConcepts <- tibble::tibble(
  conceptId = integer(),
  conceptName = character()
)

excludedCovariateConcepts <- tibble::tibble(
  conceptId = integer(),
  conceptName = character()
)

# CohortGeneratorModule --------------------------------------------------------
# - Shared resources: cohort definitions and negative control outcome concept set
# - Settings: generate stats during cohort generation
cgModuleSettingsCreator <- CohortGeneratorModule$new()

cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule ------------------------------------------------------
# - Run a broad set of diagnostics on all generated cohorts (including negative controls)
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
# This section encodes the comparisons, study periods, time-at-risk windows,
# propensity score adjustment strategies, and outcome model settings based on
# the Analysis Specifications.

# Study periods (from Analysis Specifications -> getDbCohortMethodDataArgs.studyPeriods)
# Note: These are applied when fetching data (studyStartDate/studyEndDate)
studyPeriods <- tibble::tibble(
  studyStartDate = c("20100101", "20120101"),  # YYYYMMDD
  studyEndDate   = c("20191231", "20191231")   # YYYYMMDD
)

# Time-at-risk (TAR) windows (from Analysis Specifications -> createStudyPopArgs.timeAtRisks)
# Include a label for readability in the analysis description
timeAtRisks <- tibble::tibble(
  label = c("TAR1_start1_endCohortEnd", "TAR2_start1_end99999FromStart"),
  riskWindowStart = c(1, 1),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd = c(0, 99999),
  endAnchor = c("cohort end", "cohort start"),
  minDaysAtRisk = c(1, 1)
)

# Propensity score configurations (from Analysis Specifications -> propensityScoreAdjustment.psSettings)
# 1) Stratify by PS
stratifyByPsArgsList <- tibble::tibble(
  label = c("PS_stratify_5_all"),
  numberOfStrata = c(5),
  baseSelection = c("all") # "all" | "target" | "comparator"
)

# 2) Match on PS
matchOnPsArgsList <- tibble::tibble(
  label = c("PS_match_maxRatio0_caliper0.2_stdLogit"),
  maxRatio = c(0),
  caliper = c(0.2),
  caliperScale = c("standardized logit") # "propensity score" | "standardized" | "standardized logit"
)

# Build a unified list of PS configurations to iterate over
psConfigList <- list()

# Convert "match on PS" settings into a config list
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

# Convert "stratify by PS" settings into a config list
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

# Build outcome set used across all analyses:
# - Include "true" outcome(s) of interest
# - Append negative controls as non-interest outcomes
outcomeList <- append(
  lapply(seq_len(nrow(oList)), function(i) {
    CohortMethod::createOutcome(
      outcomeId = oList$outcomeCohortId[i],
      outcomeOfInterest = TRUE,
      trueEffectSize = NA,
      priorOutcomeLookback = 99999
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

# Create the Target-Comparator-Outcome definitions used by the CM module
targetComparatorOutcomesList <- list()
for (i in seq_len(nrow(cmTcList))) {
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
  )
}

# Create CM analyses by iterating over:
# - Study periods
# - TARs
# - PS approaches
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Configure PS adjustment method for this analysis
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
      } else {
        stop("Unknown PS method specified.")
      }

      # Covariate extraction settings (default FE settings; no explicit include/exclude concepts here)
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # GetDbCohortMethodData (from Analysis Specifications -> getDbCohortMethodDataArgs)
      # - restrictToCommonPeriod: false
      # - studyStartDate/studyEndDate (iterated)
      # - maxCohortSize: 0
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Create PS (from Analysis Specifications -> propensityScoreAdjustment.createPsArgs)
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          tolerance = 2e-07,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "silent",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Compute covariate balance settings
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit outcome model (from Analysis Specifications -> fitOutcomeModelArgs)
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
          startingVariance = 0.01
        )
      )

      # Create study population (from Analysis Specifications -> createStudyPopArgs + TAR)
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

      # Append a CM analysis configuration for this (study period x TAR x PS) combo
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

# Build the CohortMethod module specification with the full set of analyses
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the overall Strategus analysis specifications -------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Persist analysis specifications to JSON --------------------------------------
# - Using the "name" from Analysis Specifications: "corazon"
dir.create(file.path("inst", "corazon"), recursive = TRUE, showWarnings = FALSE)
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "corazon", "corazonAnalysisSpecification.json")
)