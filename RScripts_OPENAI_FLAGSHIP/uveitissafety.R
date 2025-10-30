################################################################################
# CreateStrategusAnalysisSpecification.R
#
# Purpose:
#   Build a Strategus analysis specification for the "uveitissafety" study using
#   the CohortGenerator, CohortDiagnostics, and CohortMethod modules. Settings
#   are derived directly from the provided Analysis Specifications and follow
#   the structure shown in the Template, with annotations to clarify how each
#   setting is applied.
#
# Reference:
#   - Strategus modules: https://ohdsi.github.io/Strategus/reference/index.html
################################################################################

# Libraries --------------------------------------------------------------------
# Note: we explicitly load packages referenced in the template and code below.
library(dplyr)
library(tibble)
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(ParallelLogger)

# Shared Resources -------------------------------------------------------------
# WebAPI base URL (per the template)
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Analysis Specifications:
#   - targetCohort:    id = 1794126, name = "target1"
#   - comparatorCohort:id = 1794132, name = "comparator1"
#   - outcomeCohort:   id = 1794131, name = "outcome1"
#
# 1) Export the cohort definitions from WebAPI
# 2) Re-number IDs locally so that target=1, comparator=2, outcome=3
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to fixed local IDs for downstream modules
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# Analysis Specifications:
#   - negativeControlConceptSet: id = 1888110, name = "negative"
#
# We retrieve concepts in the negative control concept set, and map them into
# a "cohort-like" object with unique cohortId values that do not clash with the
# target/comparator/outcome cohorts (we start at 101).
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
  ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
  dplyr::rename(
    outcomeConceptId = "conceptId",
    cohortName = "conceptName"
  ) %>%
  dplyr::mutate(cohortId = dplyr::row_number() + 100) %>% # 101, 102, 103, ...
  dplyr::select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no duplicate cohort IDs across primary and negative control cohorts
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found across primary and negative control cohorts ***")
}

# Data frames describing the cohorts used in each analysis ---------------------
# Outcomes: prepare a list that includes the primary outcome (ID = 3)
# Note: cleanWindow is included to match the template pattern; not used directly in this script.
oList <- cohortDefinitionSet %>%
  dplyr::filter(.data$cohortId == 3) %>%
  dplyr::mutate(outcomeCohortId = .data$cohortId, outcomeCohortName = .data$cohortName) %>%
  dplyr::select(.data$outcomeCohortId, .data$outcomeCohortName) %>%
  dplyr::mutate(cleanWindow = 365)

# Target and Comparator pairs for the CohortMethod analysis
# Use EXACT names from Analysis Specifications: "target1" and "comparator1"
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  # The template references targetConceptId/comparatorConceptId later when
  # excluding drug concepts; we include columns as NA to keep the structure.
  targetConceptId = NA_integer_,
  comparatorConceptId = NA_integer_
)

# Covariate selection (include/exclude) ----------------------------------------
# Analysis Specifications indicate empty include/exclude sets.
# We'll build empty data frames to reflect "include none explicitly" and
# "exclude none explicitly" in the low-dimensional propensity score (LDPS) step.
includedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# CohortGeneratorModule --------------------------------------------------------
# 1) Shared resource specifications for the primary cohorts
# 2) Shared resource specifications for negative control outcomes
# 3) CohortGenerator module specifications (e.g., generateStats)
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
# Diagnostics will run on all cohorts we exported.
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
# Build CM settings using the provided Analysis Specifications.

# Study Periods ---------------------------------------------------------------
# Analysis Specifications: studyStartDate = NULL, studyEndDate = NULL
# As per the template, when not restricting to a time window, make these strings empty.
studyPeriods <- tibble::tibble(
  studyStartDate = c(""),  # "" indicates no restriction
  studyEndDate   = c("")   # "" indicates no restriction
)

# Time-at-risk windows (TARs) -------------------------------------------------
# Analysis Specifications:
#   - riskWindowStart = 1, startAnchor = "cohort start"
#   - riskWindowEnd   = 0, endAnchor   = "cohort end"
#   - minDaysAtRisk   = 1
timeAtRisks <- tibble::tibble(
  label = c("TAR1"),
  riskWindowStart = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(0),
  endAnchor = c("cohort end"),
  minDaysAtRisk = c(1)
)

# Propensity Score (PS) adjustment specifications -----------------------------
# Analysis Specifications: matching on PS with:
#   - maxRatio = 10
#   - caliper = 0.2
#   - caliperScale = "standardized logit"
matchOnPsArgsList <- tibble::tibble(
  label = c("Match-PS-1"),
  maxRatio = c(10),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# No stratify-by-PS configuration provided
stratifyByPsArgsList <- tibble::tibble(
  label = character(0),
  numberOfStrata = integer(0),
  baseSelection = character(0)
)

# Build a unified PS adjustment configuration list ----------------------------
psConfigList <- list()

# Convert "match on PS" data frame into configs
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

# Convert "stratify by PS" data frame into configs (none in this analysis)
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

# Outcome objects for CohortMethod --------------------------------------------
# Compose the outcome list:
#   - The primary outcome from oList (outcomeOfInterest = TRUE, priorOutcomeLookback = 99999)
#   - Negative controls mapped from the concept set (outcomeOfInterest = FALSE, trueEffectSize = 1)
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

# TargetComparatorOutcomes list for CM -----------------------------------------
# Per the template, we can exclude covariate concepts (e.g., ingredients of
# exposure drugs). The Analysis Specifications indicate no explicit include/exclude
# concept sets; we will therefore pass an empty set of exclusions.
targetComparatorOutcomesList <- list()
for (i in seq_len(nrow(cmTcList))) {
  # Combine any specified exclusions; remove NAs and duplicates
  excludedIds <- unique(stats::na.omit(c(
    cmTcList$targetConceptId[i],
    cmTcList$comparatorConceptId[i],
    excludedCovariateConcepts$conceptId
  )))

  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    excludedCovariateConceptIds = excludedIds
  )
}

# Covariate settings for FeatureExtraction -------------------------------------
# Using default covariates; descendants will be excluded when we pass excluded IDs.
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE
)

# Build list of CM analyses by iterating settings ------------------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # PS matching or stratification settings based on psCfg$method
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

      # GetDbCohortMethodData arguments --------------------------------------
      # Analysis Specifications: maxCohortSize = 0 (unlimited)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate, # "" => no restriction
        studyEndDate = studyEndDate,     # "" => no restriction
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # PS estimation arguments ----------------------------------------------
      # Analysis Specifications:
      #   - maxCohortSizeForFitting = 250000
      #   - errorOnHighCorrelation = TRUE
      #   - prior: laplace, useCrossValidation = TRUE
      #   - control:
      #       tolerance = 2e-7, cvType = "auto",
      #       fold = 10, cvRepetitions = 10,
      #       noiseLevel = "silent", resetCoefficients = TRUE,
      #       startingVariance = 0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,            # Allow the pipeline to continue if fitting fails
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",
          cvType = "auto",
          tolerance = 2e-07,
          folds = 10,
          cvRepetitions = 10,
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Covariate balance arguments ------------------------------------------
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model fitting arguments ---------------------------------------
      # Analysis Specifications:
      #   - modelType = "cox", stratified = TRUE, useCovariates = FALSE,
      #     inversePtWeighting = FALSE
      #   - prior: laplace, useCrossValidation = TRUE
      #   - control:
      #       tolerance = 2e-7, cvType = "auto",
      #       fold = 10, cvRepetitions = 10,
      #       noiseLevel = "quiet", resetCoefficients = TRUE,
      #       startingVariance = 0.01
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
          tolerance = 2e-07,
          folds = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Study population arguments -------------------------------------------
      # Analysis Specifications:
      #   restrictToCommonPeriod = TRUE
      #   firstExposureOnly = TRUE
      #   washoutPeriod = 365
      #   removeDuplicateSubjects = "keep all"
      #   censorAtNewRiskWindow = TRUE
      #   removeSubjectsWithPriorOutcome = TRUE
      #   priorOutcomeLookBack = 99999
      #   TAR: riskWindowStart = 1, startAnchor = "cohort start",
      #        riskWindowEnd = 0, endAnchor = "cohort end",
      #        minDaysAtRisk = 1
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE,
        firstExposureOnly = TRUE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = TRUE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t]
      )

      # Append the analysis configuration to the list -------------------------
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(nchar(studyStartDate) == 0, "noStartDate", studyStartDate),
          ifelse(nchar(studyEndDate) == 0, "noEndDate", studyEndDate),
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

# CohortMethod Module specifications -------------------------------------------
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

# Save the analysis specifications to JSON -------------------------------------
# Use the exact study name "uveitissafety" for the file path.
outputDir <- file.path("inst", "uveitissafety")
if (!dir.exists(outputDir)) dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)

ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path(outputDir, "uveitissafetyAnalysisSpecification.json")
)