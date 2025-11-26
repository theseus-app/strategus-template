################################################################################
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# further details.
################################################################################

# Load required packages for Strategus and supporting HADES modules/utilities
library(dplyr)
library(tibble)
library(ROhdsiWebApi)
library(Strategus)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(ParallelLogger)

# Shared Resources -------------------------------------------------------------
# WebAPI endpoint hosting the Atlas cohorts and concept set definitions
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export Target, Comparator, Outcome cohorts by their Atlas IDs.
# We'll re-number them locally (1: target, 2: comparator, 3: outcome) for analysis.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to stable local IDs used by the CohortMethod analysis:
# 1 = target, 2 = comparator, 3 = primary outcome
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# Resolve the negative control concept set, and create a "cohort-like" set
# that Strategus will use to generate negative control outcome cohorts.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # negative
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
  ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
  rename(
    outcomeConceptId = "conceptId",
    cohortName = "conceptName"
  ) %>%
  # Assign negative control cohort IDs starting at 101 to avoid collision with 1,2,3
  mutate(cohortId = dplyr::row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Validate that we do not have duplicate IDs across primary cohorts and NCs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found across primary and negative control cohorts ***")
}

# Analysis cohort lists --------------------------------------------------------
# Outcomes (primary outcome only here; NCs are appended in the CM analysis creation)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # Clean window is documented here; it's not directly used by CM functions but often relevant in diagnostics
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  stringsAsFactors = FALSE
)

# Covariate selection per Analysis Specifications -----------------------------
# The provided specifications contain empty include/exclude concept sets.
# We'll define an empty excluded concept list for the LSPS (Large-Scale PS) build.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),    # no concepts to exclude
  conceptName = character() # no concepts to exclude
)

# Optional: If you want to define covariates to include instead of including them all
# (Empty by specification)
# includedCovariateConcepts <- data.frame(
#   conceptId = integer(),
#   conceptName = character()
# )

# CohortGeneratorModule --------------------------------------------------------
# This module will create/generate the cohorts specified above.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared Resource: Cohort Definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

# Shared Resource: Negative Control Outcome Cohorts (from the concept set)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",     # first occurrence of each NC concept per person
  detectOnDescendants = TRUE    # include descendants when creating the NC outcome cohorts
)

# Module specifications for cohort generation
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # create cohort stats during generation
)

# CohortDiagnosticsModule Settings --------------------------------------------
# Configure cohort diagnostics for all generated cohorts, including NCs (via shared resource)
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
# Strategus executes multiple CM analyses by taking the cartesian product across:
# - studyPeriods
# - timeAtRisks
# - PS adjustment methods (match / stratify configurations)
# The settings below are mapped from the Analysis Specifications.

# 1) Study Periods:
# Per specifications we have two periods; CM will subset to these windows at data extraction.
studyPeriods <- tibble::tibble(
  studyStartDate = c("20111101", "20130301"), # YYYYMMDD
  studyEndDate   = c("20190331", "20161231")  # YYYYMMDD
)

# 2) Time-at-risks (TARs):
# Per specifications we have six TAR settings. We'll label them for readability in the analysis descriptions.
timeAtRisks <- tibble::tibble(
  label = c(
    "TAR_1",
    "TAR_2",
    "TAR_3",
    "TAR_4",
    "TAR_5",
    "TAR_6"
  ),
  riskWindowStart = c(1, 1, 1, 29, 29, 29),
  startAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd = c(365, 1825, 0, 365, 1825, 0),
  endAnchor = c("cohort start", "cohort start", "cohort end", "cohort start", "cohort start", "cohort end"),
  minDaysAtRisk = c(1, 1, 1, 1, 1, 1)
)

# 3) Propensity Score (PS) adjustment configurations:
# We create "match on PS" and "stratify by PS" configurations as separate entries,
# which will be expanded across study periods and TARs.

# PS: match on PS settings (two configurations)
matchOnPsArgsList <- tibble::tibble(
  label = c(
    "Match_1to1_cal0.2_stdlogit",
    "Match_1to10_cal0.2_stdlogit"
  ),
  maxRatio  = c(1, 10),
  caliper = c(0.2, 0.2),
  caliperScale  = c("standardized logit", "standardized logit")
)

# PS: stratify by PS settings (one configuration)
stratifyByPsArgsList <- tibble::tibble(
  label = c("Stratify_10_all"),
  numberOfStrata  = c(10),
  baseSelection = c("all") # "all" | "target" | "comparator"
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Convert "match on PS" rows to configuration entries
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

# Convert "stratify by PS" rows to configuration entries
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

# Prepare CohortMethod analyses -----------------------------------------------
cmAnalysisList <- list()
targetComparatorOutcomesList <- list()
analysisId <- 1

# Build the Outcome list (primary outcome + all negative controls)
# - priorOutcomeLookback is aligned with createStudyPopulation setting (99999).
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

# Build the Target-Comparator-Outcomes collection used across analyses
for (i in seq_len(nrow(cmTcList))) {
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    excludedCovariateConceptIds = excludedCovariateConcepts$conceptId # empty per specifications
  )
}

# Iterate through all combinations of study periods, TARs, and PS methods
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # PS adjustment: create either match or stratify arguments based on configuration
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

      # Covariate settings for LSPS (default large-scale covariates, excluding descendants of excluded concepts)
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Data extraction settings (GetDbCohortMethodData)
      # - restrictToCommonPeriod per specifications
      # - studyStartDate/studyEndDate vary with studyPeriods
      # - maxCohortSize per specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # PS model fitting settings per specifications
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

      # Covariate balance settings (summary/table1 level and shared covariate balance)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model fitting settings per specifications
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

      # Study population settings:
      # - Mapped from createStudyPopArgs specifications
      # - TARs are varied per 'timeAtRisks' rows
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
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t]
      )

      # Append the full CM analysis specification (one per combination)
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

# Create CM module specs for Strategus
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

# Persist the Strategus analysis specification JSON for execution
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ticagrelorclopidogrel", "ticagrelorclopidogrelAnalysisSpecification.json")
)