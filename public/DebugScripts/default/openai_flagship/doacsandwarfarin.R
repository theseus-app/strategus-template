################################################################################
# CreateStrategusAnalysisSpecification.R
# Study: doacsandwarfarin
#
# This script builds a Strategus analysis specification using settings provided
# in <Analysis Specifications>. It follows the structure of the provided
# <Template> and adds detailed annotations that explain how each setting is applied.
#
# Strategus modules used:
# - CohortGeneratorModule
# - CohortDiagnosticsModule
# - CohortMethodModule
################################################################################

# Libraries --------------------------------------------------------------------
library(dplyr)
library(tibble)
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(ParallelLogger)

# Shared Resources -------------------------------------------------------------
# WebAPI endpoint hosting the referenced cohort and concept set definitions
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Study name used for output file naming
studyName <- "doacsandwarfarin"

# Cohort Definitions -----------------------------------------------------------
# Pull the 3 cohorts (Target, Comparator, Outcome) by their source IDs from WebAPI
# These IDs and names are specified in <Analysis Specifications>
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal analysis consistency:
# - Target -> 1
# - Comparator -> 2
# - Outcome -> 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# Resolve a negative control concept set to concrete concepts.
# This is the "negativeControlConceptSet" from <Analysis Specifications>.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
  ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
  # Standardize names to those expected by the negative control shared resource
  rename(
    outcomeConceptId = "conceptId",
    cohortName = "conceptName"
  ) %>%
  # Assign cohortIds starting at 101 to avoid overlap with T/C/O ids (1,2,3)
  mutate(cohortId = dplyr::row_number() + 100L) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Sanity check: ensure no overlap between main cohorts and negative controls
if (any(cohortDefinitionSet$cohortId %in% negativeControlOutcomeCohortSet$cohortId)) {
  stop("*** Error: duplicate cohort IDs found between main cohorts and negative controls ***")
}

# Create helper data for outcomes and T/C mappings -----------------------------
# Outcomes: choose the single specified outcome (renumbered to 3)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # Clean window not explicitly specified; a default is shown for completeness
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis (names preserved exactly)
cmTcList <- tibble::tibble(
  targetCohortId = 1L,
  targetCohortName = "target1",
  comparatorCohortId = 2L,
  comparatorCohortName = "comparator1"
)

# Covariate Selection ----------------------------------------------------------
# The <Analysis Specifications> provide empty selections for both include and exclude.
# We capture those as empty data frames to be passed/available for use, and in this
# script we will keep the default covariate settings (i.e., not restricting by these).
includedCovariateConcepts <- tibble::tibble(
  conceptId = integer(),
  conceptName = character()
)

excludedCovariateConcepts <- tibble::tibble(
  conceptId = integer(),
  conceptName = character()
)

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resources: cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Shared resources: negative control outcomes (occurrence/detection defaults from template)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications: generate stats in cohort tables
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# We use template defaults (not specified in <Analysis Specifications>)
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
# Study periods from <Analysis Specifications> -> getDbCohortMethodDataArgs.studyPeriods
# Only one study period is provided here; additional rows could be added if needed.
studyPeriods <- tibble::tibble(
  studyStartDate = c("20101019"),
  studyEndDate   = c("20181231")
)

# Time-at-risk windows from <Analysis Specifications> -> createStudyPopArgs.timeAtRisks
# Provide a label for traceability in analysis descriptions
timeAtRisks <- tibble::tibble(
  label = c("TAR_1", "TAR_2", "TAR_3"),
  riskWindowStart = c(1L, 1L, 1L),
  startAnchor     = c("cohort start", "cohort start", "cohort start"),
  riskWindowEnd   = c(5L, 0L, 99999L),
  endAnchor       = c("cohort end", "cohort end", "cohort start")
)

# Propensity Score settings from <Analysis Specifications>
# Two PS-match configurations are defined; no stratify configs.
matchOnPsArgsList <- tibble::tibble(
  label = c("PS_match_1", "PS_match_2"),
  maxRatio = c(1, 100),
  caliper = c(0.2, 0.2),
  caliperScale = c("standardized logit", "standardized logit")
)

stratifyByPsArgsList <- tibble::tibble(
  label = character(),
  numberOfStrata = integer(),
  baseSelection = character()
)

# Build PS configuration list (method + label + params)
psConfigList <- list()

# Convert "match on PS" rows to config
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

# Convert "stratify by PS" rows to config (none provided in specs)
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

# Create covariate settings
# Default settings are used; the spec provides empty include/exclude lists.
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE
)

# Build outcome objects: include the main outcome(s) and all negative controls
outcomeList <- append(
  lapply(seq_len(nrow(oList)), function(i) {
    CohortMethod::createOutcome(
      outcomeId = oList$outcomeCohortId[i],
      outcomeOfInterest = TRUE,
      trueEffectSize = NA,
      # From <Analysis Specifications>.createStudyPopArgs.priorOutcomeLookBack
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

# Assemble T-C-O lists: excludedCovariateConcepts comes from <Analysis Specifications>
targetComparatorOutcomesList <- list()
for (i in seq_len(nrow(cmTcList))) {
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
  )
}

# Iterate through combinations of study period(s), TAR(s), and PS configs
cmAnalysisList <- list()
analysisId <- 1L

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

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
      } else {
        stop("Unknown PS configuration method: ", psCfg$method)
      }

      # getDbCohortMethodDataArgs (from <Analysis Specifications>)
      # Note: firstExposureOnly/removeDuplicateSubjects/washoutPeriod apply to study population,
      # not to data extraction; we set those later in createStudyPopulation.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs (from <Analysis Specifications>.propensityScoreAdjustment.createPsArgs)
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

      # Covariate balance settings (template defaults)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs (from <Analysis Specifications>.fitOutcomeModelArgs)
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

      # createStudyPopulationArgs (from <Analysis Specifications>.createStudyPopArgs)
      # We construct one analysis per TAR definition.
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
        # maxDaysAtRisk is not provided in <Analysis Specifications> and thus not set
      )

      # Append the settings to Analysis List with a descriptive label
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
      analysisId <- analysisId + 1L
    }
  }
}

# Create CohortMethod module specifications
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the Strategus analysis specifications ---------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Persist the analysis specification to JSON -----------------------------------
# Ensure the output directory exists to avoid "No such file or directory" errors
outputDir <- file.path("inst", studyName)
dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)

ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path(outputDir, sprintf("%sAnalysisSpecification.json", studyName))
)