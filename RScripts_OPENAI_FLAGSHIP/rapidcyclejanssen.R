################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script builds a Strategus analysis specification using settings provided
# in the <Analysis Specifications>. It follows the structure of the provided
# <Template> and maps each setting explicitly into the appropriate HADES module
# arguments with extensive annotations.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

# Load required packages
library(dplyr)
library(tibble)
library(Strategus)
library(ROhdsiWebApi)
library(FeatureExtraction)
library(CohortMethod)
library(Cyclops)
library(ParallelLogger)

# Shared Resources -------------------------------------------------------------
# Base URL for your ATLAS/WebAPI instance (update as needed)
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Analysis name from <Analysis Specifications>
analysisName <- "rapidcyclejanssen"

# 1) Cohort Definitions --------------------------------------------------------
# Pull the target, comparator, and outcome cohorts from WebAPI (by their integer IDs)
# Using EXACT IDs from <Analysis Specifications>
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts locally so downstream modules can use canonical IDs: 1=T, 2=C, 3=O
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# 2) Negative control outcomes concept set ------------------------------------
# Pull the negative control concept set (EXACT ID from <Analysis Specifications>)
# and expand to the standard list of concepts to evaluate as outcome cohorts
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,  # negative
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
  ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
  # Standardize to columns needed by Strategus/CM negative control framework:
  rename(
    outcomeConceptId = "conceptId",
    cohortName = "conceptName"
  ) %>%
  # Assign local cohortIds for negative controls so these do not collide with 1/2/3
  mutate(cohortId = row_number() + 100) %>%   # 101, 102, ...
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure cohort IDs do not collide
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found across main cohorts and negative controls ***")
}

# 3) Create helper frames for downstream modules -------------------------------
# - Outcomes to analyze (primary outcome(s) and their clean windows)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(
    outcomeCohortId = cohortId,
    outcomeCohortName = cohortName
  ) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # The Template sets a fixed clean window of 365 days for primary outcomes
  mutate(cleanWindow = 365)

# - The (T, C) setup for CohortMethod
cmTcList <- tibble::tibble(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# - Optional: explicitly excluded covariate concepts for LSPS (not specified here)
#   In many LSPS studies, drug exposure concepts of interest are excluded to avoid
#   perfect prediction. The <Analysis Specifications> do not provide such concepts,
#   so we keep this empty.
excludedCovariateConcepts <- tibble::tibble(
  conceptId = integer(0),
  conceptName = character(0)
)

# CohortGeneratorModule --------------------------------------------------------
# Shared resources for Strategus:
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Cohort definition shared resource
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

# Negative control outcomes shared resource
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specification for cohort generation
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Run a broad set of diagnostics on cohorts 1, 2, 3 and negative controls
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
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

# CohortMethodModule -----------------------------------------------------------
# Map <Analysis Specifications> to CM settings

# Study period restriction for data extraction (GetDb step)
# From <Analysis Specifications>. Use string format "YYYYMMDD" as expected by HADES.
studyPeriods <- tibble::tibble(
  studyStartDate = c("20210101"),
  studyEndDate   = c("")  # NULL in specs -> empty string means "no end restriction"
)

# Time-at-risk windows (TARs)
# From <Analysis Specifications> createStudyPopArgs$timeAtRisks
timeAtRisks <- tibble::tibble(
  label = c(
    "RWS1:1-14(cs->cs)",
    "RWS2:1-28(cs->cs)",
    "RWS3:1-42(cs->cs)",
    "RWS4:1-90(cs->cs)",
    "RWS5:0-2(cs->cs)"
  ),
  riskWindowStart = c(1, 1, 1, 1, 0),
  startAnchor     = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd   = c(14, 28, 42, 90, 2),
  endAnchor       = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start"),
  minDaysAtRisk   = c(1, 1, 1, 1, 1)
)

# Propensity Score settings: "match on PS" entries
# From <Analysis Specifications> propensityScoreAdjustment$psSettings (single with matchOnPsArgs)
matchOnPsArgsList <- tibble::tibble(
  label = c("match: caliper=0.2 (standardized logit)"),
  maxRatio = c(0),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# Propensity Score settings: "stratify by PS" entries
# From <Analysis Specifications> stratifyByPsArgs = null (none)
stratifyByPsArgsList <- tibble::tibble(
  label = character(0),
  numberOfStrata = integer(0),
  baseSelection = character(0)
)

# Build a consolidated PS configuration list, each entry identifies the method and its parameters
psConfigList <- list()

# Convert "match on PS" rows to configurations
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

# Convert "stratify by PS" rows to configurations (none for this analysis)
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

# Define the target-comparator-outcomes list once (used across analyses)
# Includes both primary outcome(s) and negative control outcomes
outcomeList <- append(
  # Primary outcomes from oList (marked as outcomes of interest)
  lapply(seq_len(nrow(oList)), function(i) {
    CohortMethod::createOutcome(
      outcomeId = oList$outcomeCohortId[i],
      outcomeOfInterest = TRUE,
      trueEffectSize = NA,
      priorOutcomeLookback = 99999  # From <Analysis Specifications>
    )
  }),
  # Negative control outcomes (not outcomes of interest)
  lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
    CohortMethod::createOutcome(
      outcomeId = i,
      outcomeOfInterest = FALSE,
      trueEffectSize = 1
    )
  })
)

targetComparatorOutcomesList <- list()
for (i in seq_len(nrow(cmTcList))) {
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
  )
}

# Covariate settings for LSPS:
# <Analysis Specifications> provide empty include/exclude lists, so use defaults.
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE
)

# Iterate through all analysis setting combinations and build CM analyses --------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate   <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # PS adjustment method-specific args
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

      # 1) Data retrieval settings (GetDb step) --------------------------------
      # From <Analysis Specifications> getDbCohortMethodDataArgs
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate, # "YYYYMMDD"
        studyEndDate = studyEndDate,     # "" -> no end restriction
        maxCohortSize = 0,               # EXACT from specs
        covariateSettings = covariateSettings
      )

      # 2) Propensity score creation settings ----------------------------------
      # From <Analysis Specifications> propensityScoreAdjustment$createPsArgs
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # Allow the run to proceed even if some PS fits fail
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

      # 3) Covariate balance settings ------------------------------------------
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # 4) Outcome model settings ----------------------------------------------
      # From <Analysis Specifications> fitOutcomeModelArgs
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

      # 5) Study population settings including TAR ------------------------------
      # From <Analysis Specifications> createStudyPopArgs + current TAR row
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = TRUE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "remove all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t]
      )

      # Assemble the analysis definition ---------------------------------------
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study period: %s-%s; TAR: %s; PS: %s",
          studyStartDate,
          ifelse(nchar(studyEndDate) == 0, "NA", studyEndDate),
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

# Wrap CohortMethod module specifications --------------------------------------
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the full Strategus analysis specifications ----------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save to JSON with EXACT analysis name
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", analysisName, paste0(analysisName, "AnalysisSpecification.json"))
)