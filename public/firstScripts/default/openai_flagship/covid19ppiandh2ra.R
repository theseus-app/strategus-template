################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script creates a Strategus analysis specification JSON for the study:
#   name: covid19ppiandh2ra
#
# It follows the provided <Template> structure and applies the exact settings
# from <Analysis Specifications>.
#
# Notes on key mappings from the specifications:
# - Cohort IDs are exported from ATLAS WebAPI, then re-numbered to:
#     1 = target1 (original 1794126)
#     2 = comparator1 (original 1794132)
#     3 = outcome1 (original 1794131)
# - Negative controls are pulled from concept set id 1888110 ("negative") and
#   assigned cohortIds starting at 101.
# - getDbCohortMethodDataArgs uses:
#     studyStartDate = 20200101, studyEndDate = 20200515
#     maxCohortSize = 0
#     restrictToCommonPeriod = FALSE
#     firstExposureOnly = TRUE
#     washoutPeriod = 180
#     removeDuplicateSubjects = "keep first"
# - createStudyPopArgs uses:
#     restrictToCommonPeriod = FALSE
#     firstExposureOnly = FALSE
#     washoutPeriod = 0
#     removeDuplicateSubjects = "keep all"
#     censorAtNewRiskWindow = FALSE
#     removeSubjectsWithPriorOutcome = FALSE
#     priorOutcomeLookBack = 99999
#     TAR: start=1 from cohort start; end=99999 from cohort start; minDaysAtRisk=1
# - PS adjustment configurations (3):
#     1) No match, no stratify (unadjusted / default weighting=none)
#     2) Match: maxRatio=4, caliper=0.2, caliperScale="standardized logit"
#     3) Stratify: numberOfStrata=5, baseSelection="all"
# - createPsArgs uses Laplace prior with cross-validation and Cyclops control
#   settings as specified (fold=10, cvRepetitions=10, tolerance=2e-7, etc.)
# - fitOutcomeModelArgs uses Cox, stratified=TRUE, useCovariates=FALSE, and
#   Laplace prior with CV and specified Cyclops control.
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# WebAPI endpoint used to export cohort definitions and resolve concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export the target, comparator, and outcome cohort definitions from WebAPI.
# These are the *original* ATLAS cohort IDs from <Analysis Specifications>.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts ------------------------------------------------------------
# Strategus/CohortMethod analyses typically use small integer cohort IDs.
# We re-map the original cohort IDs to:
#   1 = target, 2 = comparator, 3 = outcome
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# Pull negative control outcome concepts from the concept set definition and
# resolve to a list of concepts. Each concept becomes a negative control outcome.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # negative
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
  # Assign cohort IDs for negative controls starting at 101 to avoid collisions
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no overlap between main cohort IDs (1..3) and negative controls (101+)
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create cohort lists used by CohortMethod ------------------------------------
# Outcomes of interest list (primary outcomes). Here: only cohortId == 3.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # cleanWindow is used by some workflows; keep a standard value.
  mutate(cleanWindow = 365)

# Target and Comparator list for CohortMethod
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate inclusion/exclusion concept lists ---------------------------------
# <Analysis Specifications> provides empty placeholders (id=null, name="") for
# conceptsToInclude and conceptsToExclude. We therefore do not define explicit
# include/exclude lists and rely on default covariate settings.
#
# However, the template expects an excludedCovariateConcepts data frame to exist.
# We create an empty one to indicate "no additional exclusions".
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource: cohort definitions (target/comparator/outcome)
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Shared resource: negative control outcomes (as concept-based outcomes)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specs: generate cohort stats
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

# Study periods ---------------------------------------------------------------
# <Analysis Specifications> provides exactly one study period window.
studyPeriods <- tibble(
  studyStartDate = c("20200101"),
  studyEndDate   = c("20200515")
)

# Time-at-risk (TAR) settings -------------------------------------------------
# <Analysis Specifications> provides exactly one TAR:
#   start: 1 day after cohort start
#   end: 99999 days after cohort start
#   minDaysAtRisk: 1
timeAtRisks <- tibble(
  label = c("TAR_1_to_99999_from_cohort_start"),
  riskWindowStart = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(99999),
  endAnchor = c("cohort start"),
  minDaysAtRisk = c(1)
)

# Propensity score adjustment configurations ----------------------------------
# We build a unified psConfigList with 3 entries matching <Analysis Specifications>:
#  1) No match/stratify (both NULL)
#  2) Match on PS
#  3) Stratify by PS
psConfigList <- list(
  list(
    method = "none",
    label = "PS_none",
    params = list()
  ),
  list(
    method = "match",
    label = "PS_match_maxRatio4_caliper0.2_stdLogit",
    params = list(
      maxRatio = 4,
      caliper = 0.2,
      caliperScale = "standardized logit"
    )
  ),
  list(
    method = "stratify",
    label = "PS_stratify_5strata_all",
    params = list(
      numberOfStrata = 5,
      baseSelection = "all"
    )
  )
)

# Iterate through all analysis setting combinations ---------------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Translate PS config into CohortMethod args ----------------------------
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
        # "none": both match and stratify are NULL, meaning no PS adjustment step
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- NULL
      }

      # Covariate settings ----------------------------------------------------
      # <Analysis Specifications> does not specify custom include/exclude concept
      # lists (placeholders are empty), so we use default covariates.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcomes list ---------------------------------------------------------
      # Build outcomes list containing:
      #  - the outcome(s) of interest (outcome1)
      #  - all negative control outcomes (concept-set derived)
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            # <Analysis Specifications> priorOutcomeLookBack is 99999 in createStudyPopArgs;
            # we also set it here for the outcome definition.
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

      # Target-comparator-outcomes bundle ------------------------------------
      # Excluded covariate concept IDs:
      # - No explicit include/exclude concepts were provided in the specs.
      # - We therefore only pass excludedCovariateConcepts (empty).
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # getDbCohortMethodDataArgs --------------------------------------------
      # Apply <Analysis Specifications> getDbCohortMethodDataArgs settings.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        firstExposureOnly = TRUE,
        washoutPeriod = 180,
        removeDuplicateSubjects = "keep first",
        covariateSettings = covariateSettings
      )

      # createPsArgs ----------------------------------------------------------
      # Apply <Analysis Specifications> createPsArgs settings.
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # keep FALSE to allow pipeline completion if a model fails
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

      # Covariate balance args ------------------------------------------------
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs ---------------------------------------------------
      # Apply <Analysis Specifications> fitOutcomeModelArgs settings.
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

      # createStudyPopArgs ----------------------------------------------------
      # Apply <Analysis Specifications> createStudyPopArgs settings.
      # Note: CohortMethod uses riskWindowStart/end + anchors; we also set
      # maxDaysAtRisk to align with the provided end (99999).
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
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = timeAtRisks$riskWindowEnd[t]
      )

      # Append analysis -------------------------------------------------------
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

# CohortMethod module specifications ------------------------------------------
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

# Save to JSON ----------------------------------------------------------------
# Keep the output path pattern from the template; users should replace folder
# names as needed for their package structure.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "studyName", "studyNameAnalysisSpecification.json")
)