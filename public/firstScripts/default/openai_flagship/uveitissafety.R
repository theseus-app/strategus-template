################################################################################
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
#
# This script creates Strategus analysis specifications using the provided
# Analysis Specifications. It follows the structure laid out in the Template,
# with detailed annotations to explain how each setting is applied.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# further details.
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Configure the ATLAS/WebAPI source where the cohorts and concept sets reside.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export the Target, Comparator, and Outcome cohorts by their WebAPI IDs.
# These IDs come directly from the Analysis Specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to local IDs used within the analysis (1 = T, 2 = C, 3 = O)
# These are the internal IDs referenced by CohortMethod settings below.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# Resolve a concept set into a list of concepts to serve as negative control outcomes.
# The conceptSetId is provided in the Analysis Specifications.
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
  rename(
    outcomeConceptId = "conceptId",
    cohortName = "conceptName"
  ) %>%
  mutate(
    # Assign negative control cohortIds that won't collide with 1,2,3 used for T/C/O
    cohortId = dplyr::row_number() + 100
  ) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no overlap in cohort IDs between primary cohorts and NCs
if (length(intersect(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)) > 0) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis -------
# Outcomes: These are the primary outcomes of interest defined in the specs.
# A 'cleanWindow' can be used downstream by CohortMethod diagnostics.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis --------------------------
# Use exact names from the Analysis Specifications.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  # Optional columns included for completeness with the Template, but left NA
  # because the Analysis Specifications did not provide these concept IDs.
  targetConceptId = NA_integer_,
  comparatorConceptId = NA_integer_
)

# Covariate inclusion/exclusion ------------------------------------------------
# Analysis Specifications list empty include/exclude sets, so we define empty data frames.
# If desired, populate these with concepts to include/exclude in covariates.
includedCovariateConcepts <- tibble::tibble(
  conceptId = integer(),
  conceptName = character()
)
excludedCovariateConcepts <- tibble::tibble(
  conceptId = integer(),
  conceptName = character()
)

# CohortGeneratorModule --------------------------------------------------------
# Create shared resources and module specifications for cohort generation.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# All study cohorts (T/C/O) as a shared resource
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Negative control outcomes as a shared resource
# occurrenceType "first" and detectOnDescendants TRUE are common defaults for NC detection.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# CohortGenerator module will also generate stats for these cohorts
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# These settings will drive the standard cohort diagnostics suite for all cohorts.
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
# Construct the CohortMethod analyses based on the provided Analysis Specifications.

# Study period(s) --------------------------------------------------------------
# Per the Analysis Specifications, there is one study period with empty dates,
# which means "no date restriction".
studyPeriods <- tibble::tibble(
  studyStartDate = c(""),
  studyEndDate   = c("")
)

# Time-at-risk windows ---------------------------------------------------------
# Two TARs are defined in the Analysis Specifications (TAR1, TAR2).
# We'll carry the exact anchors and windows through to createStudyPopArgs.
timeAtRisks <- tibble::tibble(
  label = c(
    "TAR1: 1d to cohort end (cohort start -> cohort end)",
    "TAR2: 1d to 99999d (cohort start -> cohort start)"
  ),
  riskWindowStart = c(1, 1),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd = c(0, 99999),
  endAnchor = c("cohort end", "cohort start"),
  minDaysAtRisk = c(1, 1)
)

# Propensity score (PS) adjustment strategies ---------------------------------
# The Analysis Specifications list two PS matching configurations.
matchOnPsArgsList <- tibble::tibble(
  label = c(
    "PS match (maxRatio=10, caliper=0.2 standardized logit)",
    "PS match (maxRatio=1, caliper=0.2 standardized logit)"
  ),
  maxRatio = c(10, 1),
  caliper = c(0.2, 0.2),
  caliperScale = c("standardized logit", "standardized logit")
)

# No PS stratification settings in the Analysis Specifications
stratifyByPsArgsList <- tibble::tibble(
  label = character(),
  numberOfStrata = integer(),
  baseSelection = character()
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

# Convert "stratify by PS" rows to configuration entries (none here)
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

# Build the outcome definitions for CohortMethod:
# - The primary outcome(s) of interest (from oList)
# - The negative control outcomes (from the resolved concept set)
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

# Build the T/C/O triplets for CohortMethod once
for (i in seq_len(nrow(cmTcList))) {
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    # Per Analysis Specifications, no additional covariates to exclude.
    # If you want to exclude specific exposure concepts, add them here.
    excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
  )
}

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Configure PS adjustment method for this analysis combo
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
        stop("Unknown PS configuration method.")
      }

      # Covariate settings -----------------------------------------------------
      # Using the default covariate settings and allowing exclusion propagation
      # if excluded covariate concept IDs are supplied above.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Data extraction settings (getDbCohortMethodDataArgs) -------------------
      # These directly reflect the Analysis Specifications under getDbCohortMethodDataArgs
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        covariateSettings = covariateSettings
      )

      # PS estimation (createPsArgs) -------------------------------------------
      # Settings mapped from Analysis Specifications (propensityScoreAdjustment.createPsArgs)
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

      # Covariate balance (shared/unshared) ------------------------------------
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model fit (fitOutcomeModelArgs) ---------------------------------
      # Settings mapped from Analysis Specifications (fitOutcomeModelArgs)
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

      # Study population creation (createStudyPopArgs) --------------------------
      # Settings mapped from Analysis Specifications (createStudyPopArgs),
      # with TARs linked from timeAtRisks for risk windows and anchors.
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

      # Append the settings to analysis list -----------------------------------
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(nchar(studyStartDate) == 0, "noStart", studyStartDate),
          ifelse(nchar(studyEndDate) == 0, "noEnd", studyEndDate),
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

# Finish module specification creation for CohortMethod ------------------------
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
# Chain all shared resources and module specifications into a single Settings object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save to JSON so Strategus can execute this bundle later ----------------------
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "uveitissafety", "uveitissafetyAnalysisSpecification.json")
)