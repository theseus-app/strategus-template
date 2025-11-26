################################################################################
# CreateStrategusAnalysisSpecification.R
# Build Strategus analysis specifications for the "corazon" study using the
# provided Analysis Specifications and the OHDSI HADES modules template.
#
# This script:
# - Retrieves target, comparator, and outcome cohorts from ATLAS/WebAPI.
# - Resolves the negative control outcome concept set into concepts.
# - Constructs module specifications for CohortGenerator, CohortDiagnostics,
#   and CohortMethod using the parameters provided in the Analysis Specifications.
# - Saves the assembled Strategus specification JSON to inst/corazon/corazonAnalysisSpecification.json
#
# Notes:
# - The analysis settings are annotated where they map directly to the
#   Analysis Specifications entries.
# - Only function argument names from the packages are used (they must match the API).
#   Cohort and resource names/IDs are used exactly as provided.
################################################################################

# Libraries --------------------------------------------------------------------
library(dplyr)
library(tibble)
library(ROhdsiWebApi)
library(FeatureExtraction)
library(CohortMethod)
library(Cyclops)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# ATLAS/WebAPI source for cohort definitions and concept sets
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Analysis Specifications cohorts:
# - targetCohort:     id = 1794126, name = "target1"
# - comparatorCohort: id = 1794132, name = "comparator1"
# - outcomeCohort:    id = 1794131, name = "outcome1"
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target:     target1
    1794132, # Comparator: comparator1
    1794131  # Outcome:    outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for downstream consistency: 1 = Target, 2 = Comparator, 3 = Outcome
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# Analysis Specifications negativeControlConceptSet:
# - id = 1888110, name = "negative"
# Resolve the concept set to concrete concept IDs to serve as negative control outcomes.
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
  mutate(
    # Assign cohort IDs for negative controls starting at 101 to avoid overlap with 1..3
    cohortId = dplyr::row_number() + 100
  ) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no overlap between main cohorts and negative control cohort IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between primary cohorts and negative controls ***")
}

# Cohort lists for analysis ----------------------------------------------------
# Outcomes:
# - Outcome cohort (id 3) for primary analysis
# - Clean window is not specified in Analysis Specifications; using 365 days as example placeholder
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
cmTcList <- tibble::tibble(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate inclusion/exclusion lists -----------------------------------------
# Analysis Specifications covariateSelection:
# - conceptsToInclude: empty
# - conceptsToExclude: empty
# For this analysis, we do not specify additional inclusions/exclusions.
# Note: If you want to exclude specific exposure concepts, add them here.
excludedCovariateConcepts <- tibble::tibble(
  conceptId = integer(),
  conceptName = character()
)
# includedCovariateConcepts <- tibble::tibble(
#   conceptId = integer(),
#   conceptName = character()
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource for user-defined cohorts
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Shared resource for negative control outcomes
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module settings (generateStats = TRUE per Template and export choice above)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
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
# Study periods from Analysis Specifications getDbCohortMethodDataArgs:
# - studyStartDate = "20100101"
# - studyEndDate   = "20191231"
studyPeriods <- tibble::tibble(
  studyStartDate = c("20100101"),
  studyEndDate   = c("20191231")
)

# Time-at-risk (TAR) from Analysis Specifications createStudyPopArgs:
# - riskWindowStart = 1, startAnchor = "cohort start"
# - riskWindowEnd   = 0, endAnchor   = "cohort end"
# - minDaysAtRisk   = 1
timeAtRisks <- tibble::tibble(
  label = c("tar1"),
  riskWindowStart = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(0),
  endAnchor = c("cohort end"),
  minDaysAtRisk = c(1)
)

# Propensity Score settings ----------------------------------------------------
# From Analysis Specifications propensityScoreAdjustment:
# - psSettings: stratifyByPsArgs = { numberOfStrata = 5, baseSelection = "all" }
# - createPsArgs: see below
matchOnPsArgsList <- tibble::tibble(
  label = character(),
  maxRatio = numeric(),
  caliper = numeric(),
  caliperScale = character()
)

stratifyByPsArgsList <- tibble::tibble(
  label = c("strata5_all"),
  numberOfStrata = c(5),
  baseSelection = c("all")
)

# Build combined PS configuration list
psConfigList <- list()

# If any match-on-PS configurations exist, add them
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

# Add stratify-by-PS configuration from Analysis Specifications
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

# Iterate over analysis combinations ------------------------------------------
cmAnalysisList <- list()
analysisId <- 1
targetComparatorOutcomesList <- list()

# Outcomes for CM:
# - Primary outcome(s) based on oList
# - Negative control outcomes from negativeControlOutcomeCohortSet
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

# Build target-comparator-outcomes set (no additional excluded covariate concept IDs provided)
for (i in seq_len(nrow(cmTcList))) {
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
  )
}

# Loop across Study Periods, TARs, and PS configurations
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # PS adjustment method selection
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
        stop("Unsupported PS method in configuration: ", psCfg$method)
      }

      # Covariate settings:
      # - Using default covariate set; Analysis Specifications did not request custom inclusions.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Data retrieval settings from Analysis Specifications getDbCohortMethodDataArgs
      # studyPeriods = [{"studyStartDate": "20100101", "studyEndDate": "20191231"}]
      # maxCohortSize = 0
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE, # Not specified; set to FALSE to follow Study Population setting
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Propensity score model settings (Analysis Specifications createPsArgs)
      # - maxCohortSizeForFitting = 250000
      # - errorOnHighCorrelation = TRUE
      # - prior: { priorType = "laplace", useCrossValidation = TRUE }
      # - control: { tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      #              noiseLevel = "silent", resetCoefficients = TRUE, startingVariance = 0.01 }
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # keep FALSE to allow completion even if a PS model cannot be fit
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",
          exclude = c(0),
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

      # Covariate balance computation settings (retain defaults; not driven by spec)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model settings from Analysis Specifications fitOutcomeModelArgs:
      # - modelType = "cox", stratified = TRUE, useCovariates = FALSE, inversePtWeighting = FALSE
      # - prior: { priorType = "laplace", useCrossValidation = TRUE }
      # - control: { tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      #              noiseLevel = "quiet", resetCoefficients = TRUE, startingVariance = 0.01 }
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

      # Study population settings from Analysis Specifications createStudyPopArgs:
      # - restrictToCommonPeriod = FALSE
      # - firstExposureOnly = FALSE
      # - washoutPeriod = 0
      # - removeDuplicateSubjects = "keep all"
      # - censorAtNewRiskWindow = FALSE
      # - removeSubjectsWithPriorOutcome = TRUE
      # - priorOutcomeLookback = 99999
      # - timeAtRisks: riskWindowStart = 1, startAnchor = "cohort start",
      #                riskWindowEnd = 0, endAnchor = "cohort end",
      #                minDaysAtRisk = 1
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
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999
      )

      # Assemble the analysis entry
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

# Build CohortMethod module specification
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

# Save to JSON -----------------------------------------------------------------
# Study name from Analysis Specifications: "corazon"
# Output: inst/corazon/corazonAnalysisSpecification.json
if (!dir.exists(file.path("inst", "corazon"))) {
  dir.create(file.path("inst", "corazon"), recursive = TRUE, showWarnings = FALSE)
}
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "corazon", "corazonAnalysisSpecification.json")
)