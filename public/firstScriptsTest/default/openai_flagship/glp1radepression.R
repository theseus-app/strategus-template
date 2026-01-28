################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script creates a Strategus analysis specification JSON for the study:
#   name: glp1radepression
#
# It follows the provided Template structure and applies the exact settings from
# <Analysis Specifications>.
#
# Notes on key mappings from the specifications:
# - Cohort IDs from ATLAS are exported, then re-numbered to 1..N for Strategus:
#     Target (ATLAS 1794126)     -> cohortId 1
#     Comparator (ATLAS 1794132) -> cohortId 2
#     Outcome (ATLAS 1794131)    -> cohortId 3
# - Negative controls are pulled from concept set id 1888110 ("negative") and
#   assigned cohortIds starting at 101.
# - getDbCohortMethodDataArgs and createStudyPopArgs are configured to match the
#   provided settings (including study period restriction, TAR, prior outcome
#   removal, etc.).
# - PS adjustment: match on PS with maxRatio=1, caliper=0.05, caliperScale=
#   "propensity score". No stratification.
# - Cyclops prior/control settings are applied exactly as specified.
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# WebAPI base URL used to export cohort definitions and concept sets:
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export the ATLAS cohort definitions for target, comparator, and outcome.
# The cohortIds below are EXACTLY those provided in the Analysis Specifications.
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
# We map the exported ATLAS cohort IDs to:
#   target -> 1, comparator -> 2, outcome -> 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# Pull negative control outcome concepts from the ATLAS concept set:
#   negativeControlConceptSet.id = 1888110
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
  # Assign cohort IDs for negative controls starting at 101 to avoid collisions
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create cohort lists used by CohortMethod ------------------------------------
# Outcomes of interest list (oList): only the primary outcome cohort (cohortId == 3).
# cleanWindow is included per template; not used directly by CohortMethod here.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target/Comparator list for CohortMethod
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate include/exclude concept lists -------------------------------------
# The Analysis Specifications provide conceptsToInclude / conceptsToExclude with
# null/empty entries. We therefore do not define includedCovariateConcepts and
# keep excludedCovariateConcepts empty.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource: cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Shared resource: negative control outcome cohorts (concept-based)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications: generate cohort generation stats
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Using the template defaults for diagnostics; cohortIds are the re-numbered IDs.
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
# From Analysis Specifications:
#   studyStartDate = "20130101"
#   studyEndDate   = "20201231"
#   restrictToCommonPeriod (getDbCohortMethodDataArgs) = TRUE
studyPeriods <- tibble::tibble(
  studyStartDate = c("20130101"),
  studyEndDate   = c("20201231")
)

# Time-at-risk (TAR) ----------------------------------------------------------
# From Analysis Specifications createStudyPopArgs.timeAtRisks:
#   startAnchor = "cohort start", riskWindowStart = 1
#   endAnchor   = "cohort start", riskWindowEnd   = 730
#   minDaysAtRisk = 1
timeAtRisks <- tibble::tibble(
  label = c("TAR_1_730_from_cohort_start"),
  riskWindowStart = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(730),
  endAnchor = c("cohort start")
)

# Propensity Score settings ----------------------------------------------------
# From Analysis Specifications propensityScoreAdjustment.psSettings:
# - matchOnPsArgs: maxRatio=1, caliper=0.05, caliperScale="propensity score"
# - stratifyByPsArgs: null
matchOnPsArgsList <- tibble::tibble(
  label = c("match_maxRatio1_caliper0.05_ps"),
  maxRatio = c(1),
  caliper = c(0.05),
  caliperScale = c("propensity score")
)

# No stratification configs provided (explicitly null in specifications)
stratifyByPsArgsList <- tibble::tibble(
  label = character(),
  numberOfStrata = numeric(),
  baseSelection = character()
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

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

# Iterate through all analysis setting combinations ---------------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create PS adjustment args based on method -----------------------------
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

      # Covariate settings ----------------------------------------------------
      # The Analysis Specifications do not define custom include/exclude concept
      # sets (they are null/empty). We therefore use default covariates.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcomes list ---------------------------------------------------------
      # - Primary outcome: outcomeOfInterest = TRUE, priorOutcomeLookback = 99999
      # - Negative controls: outcomeOfInterest = FALSE, trueEffectSize = 1
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

      # Target-Comparator-Outcomes bundle ------------------------------------
      # Excluded covariate concept IDs:
      # - The template shows excluding target/comparator concepts plus additional
      #   excludedCovariateConcepts. The provided Analysis Specifications do not
      #   supply any concept IDs to exclude, so we only pass excludedCovariateConcepts.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = c(
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # getDbCohortMethodDataArgs --------------------------------------------
      # From Analysis Specifications:
      # - restrictToCommonPeriod = TRUE
      # - studyStartDate / studyEndDate as above
      # - maxCohortSize = 0
      # - firstExposureOnly = FALSE (note: not a parameter in createGetDbCohortMethodDataArgs)
      # - washoutPeriod = 0 (note: not a parameter in createGetDbCohortMethodDataArgs)
      # - removeDuplicateSubjects = "keep all" (note: handled in createStudyPopArgs)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs ----------------------------------------------------------
      # From Analysis Specifications propensityScoreAdjustment.createPsArgs:
      # - maxCohortSizeForFitting = 250000
      # - errorOnHighCorrelation = TRUE
      # - prior: laplace, useCrossValidation = TRUE
      # - control: tolerance=2e-7, cvType="auto", fold=10, cvRepetitions=10,
      #            noiseLevel="silent", resetCoefficients=TRUE, startingVariance=0.01
      #
      # Note: CohortMethod::createCreatePsArgs uses Cyclops::createControl; the
      # template includes seed and cvRepetitions. We set cvRepetitions=10 and
      # keep seed=1 as in template (not specified, but required for reproducibility).
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # keep FALSE to allow pipeline completion if a fit fails
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
          # 'fold' is represented as 'fold' in Cyclops control:
          fold = 10,
          cvRepetitions = 10,
          startingVariance = 0.01
        )
      )

      # Covariate balance args (template defaults) ----------------------------
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs ---------------------------------------------------
      # From Analysis Specifications fitOutcomeModelArgs:
      # - modelType="cox", stratified=TRUE, useCovariates=FALSE, inversePtWeighting=FALSE
      # - prior: laplace, useCrossValidation=TRUE
      # - control: tolerance=2e-7, cvType="auto", fold=10, cvRepetitions=10,
      #            noiseLevel="quiet", resetCoefficients=TRUE, startingVariance=0.01
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
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet"
        )
      )

      # createStudyPopArgs ----------------------------------------------------
      # From Analysis Specifications createStudyPopArgs:
      # - restrictToCommonPeriod = FALSE
      # - firstExposureOnly = FALSE
      # - washoutPeriod = 0
      # - removeDuplicateSubjects = "keep all"
      # - censorAtNewRiskWindow = FALSE
      # - removeSubjectsWithPriorOutcome = TRUE
      # - priorOutcomeLookBack = 99999
      # - TAR: start=1 from cohort start; end=730 from cohort start; minDaysAtRisk=1
      #
      # Note: CohortMethod::createCreateStudyPopulationArgs uses:
      #   removeDuplicateSubjects = "keep all" (valid option)
      #   maxDaysAtRisk is not specified in the Analysis Specifications; we set
      #   it to 99999 as in the template.
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
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Append the settings to Analysis List ---------------------------------
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
# Output path follows the Template convention; replace folder names as needed.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "glp1radepression", "glp1radepressionAnalysisSpecification.json")
)