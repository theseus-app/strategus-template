################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script creates a Strategus analysis specification JSON for the study:
#   name: uveitissafety
#
# It follows the provided template and applies the settings from
# <Analysis Specifications> exactly (no name auto-correct).
#
# Key components configured here:
#  1) Cohort definitions (target/comparator/outcome) exported from ATLAS WebAPI
#  2) Negative control outcomes resolved from an ATLAS concept set
#  3) CohortGenerator + CohortDiagnostics module specifications
#  4) CohortMethod module specifications:
#     - getDbCohortMethodDataArgs
#     - createStudyPopArgs (two time-at-risk windows)
#     - propensity score settings (two match configurations)
#     - outcome model settings (Cox, stratified, no covariates)
#
# Notes:
#  - The template contains some syntactic issues; this script corrects them while
#    keeping the same structure and EXACT setting names from the specifications.
#  - Study periods are empty strings in the specifications; we implement this as
#    a single row with empty strings so the loops run once without restricting
#    the study period.
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# WebAPI endpoint used to export cohort definitions and resolve concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export the ATLAS cohort definitions for:
#  - Target:     id 1794126 (name: target1)
#  - Comparator: id 1794132 (name: comparator1)
#  - Outcome:    id 1794131 (name: outcome1)
#
# We then re-number them to 1, 2, 3 to align with the template's downstream
# assumptions (target/comparator/outcome start at 1/2/3).
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts (ATLAS IDs -> local IDs used by Strategus/CohortMethod)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# Resolve the ATLAS concept set (id 1888110, name: negative) into a list of
# concepts, then map each concept to a "negative control outcome cohort".
#
# Strategus/CohortMethod convention in the template:
#  - main cohorts use IDs 1..N
#  - negative controls start at 101, 102, ...
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
    cohortId = row_number() + 100
  ) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no cohortId collisions between main cohorts and NCs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis --------
# Outcomes of interest list (from the exported cohort definitions).
# cleanWindow is included per template; not used directly by CohortMethod here.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate selection ----------------------------------------------------------
# <Analysis Specifications> provides conceptsToInclude / conceptsToExclude with
# null/empty entries. We therefore do not define includedCovariateConcepts and
# keep excludedCovariateConcepts empty.
#
# Note: The template uses excludedCovariateConcepts to exclude drugs of interest.
# Here, no valid concept IDs are provided, so we exclude none.
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

# Shared resource: negative control outcome cohorts (first occurrence, descendants)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specs: generateStats TRUE (as in template)
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
# <Analysis Specifications>:
#   getDbCohortMethodDataArgs.studyPeriods = [{studyStartDate:"", studyEndDate:""}]
#
# We implement a single-row tibble with empty strings to indicate "no restriction".
studyPeriods <- tibble::tibble(
  studyStartDate = c(""),
  studyEndDate = c("")
)

# Time-at-risks (TARs) --------------------------------------------------------
# <Analysis Specifications>.createStudyPopArgs.timeAtRisks has two TARs:
#  1) start: 1 day after cohort start; end: cohort end; minDaysAtRisk: 1
#  2) start: 1 day after cohort start; end: 99999 days after cohort start; minDaysAtRisk: 1
#
# The template expects a data frame with label + TAR parameters.
timeAtRisks <- tibble::tibble(
  label = c(
    "TAR_1_to_end_cohort_end",
    "TAR_1_to_99999_from_cohort_start"
  ),
  riskWindowStart = c(1, 1),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd = c(0, 99999),
  endAnchor = c("cohort end", "cohort start"),
  minDaysAtRisk = c(1, 1)
)

# Propensity Score settings ----------------------------------------------------
# <Analysis Specifications>.propensityScoreAdjustment.psSettings has two entries,
# both "match on PS" with different maxRatio:
#  - maxRatio 10, caliper 0.2, caliperScale "standardized logit"
#  - maxRatio 1,  caliper 0.2, caliperScale "standardized logit"
matchOnPsArgsList <- tibble::tibble(
  label = c("match_maxRatio_10", "match_maxRatio_1"),
  maxRatio = c(10, 1),
  caliper = c(0.2, 0.2),
  caliperScale = c("standardized logit", "standardized logit")
)

# No stratifyByPsArgs in the specifications (both are null)
stratifyByPsArgsList <- tibble::tibble(
  label = character(),
  numberOfStrata = integer(),
  baseSelection = character()
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Convert each row of matchOnPsArgsList into a PS config entry
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "match",
      label = matchOnPsArgsList$label[i],
      params = list(
        maxRatio = matchOnPsArgsList$maxRatio[i],
        caliper = matchOnPsArgsList$caliper[i],
        caliperScale = matchOnPsArgsList$caliperScale[i]
      )
    )
  }
}

# Convert each row of stratifyByPsArgsList into a PS config entry (none expected)
if (exists("stratifyByPsArgsList") && nrow(stratifyByPsArgsList) > 0) {
  for (i in seq_len(nrow(stratifyByPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "stratify",
      label = stratifyByPsArgsList$label[i],
      params = list(
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}

# Iterate through all analysis setting combinations ---------------------------
# We create one CohortMethod analysis per combination of:
#  - study period (1 row: empty start/end)
#  - time-at-risk (2 TARs)
#  - PS config (2 match configs)
#
# Total analyses: 1 * 2 * 2 = 4
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # PS adjustment args -----------------------------------------------------
      # Only "match" is expected from the specifications.
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
        stop("Unknown PS config method: ", psCfg$method)
      }

      # Covariate settings -----------------------------------------------------
      # <Analysis Specifications> does not provide custom include/exclude concept
      # lists (null/empty), so we use default covariates.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcomes list ----------------------------------------------------------
      # Outcomes of interest: the single outcome cohort (id 3)
      # Negative controls: all resolved concepts mapped to cohort IDs 101+
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            # <Analysis Specifications>.createStudyPopArgs.priorOutcomeLookBack = 99999
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

      # Target-Comparator-Outcomes list ---------------------------------------
      # Excluded covariate concept IDs:
      #  - In the template, target/comparator concept IDs are sometimes excluded.
      #  - Here, no such concept IDs are provided in <Analysis Specifications>.
      #  - excludedCovariateConcepts is empty, so no exclusions are applied.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # getDbCohortMethodDataArgs ---------------------------------------------
      # <Analysis Specifications>.getDbCohortMethodDataArgs:
      #  - restrictToCommonPeriod: true
      #  - maxCohortSize: 0
      #  - firstExposureOnly: false (handled in createStudyPopArgs; CM data args
      #    does not have firstExposureOnly)
      #  - washoutPeriod: 0 (handled in createStudyPopArgs)
      #  - removeDuplicateSubjects: "keep all" (handled in createStudyPopArgs)
      #
      # studyStartDate/studyEndDate are empty strings => no restriction.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs -----------------------------------------------------------
      # <Analysis Specifications>.propensityScoreAdjustment.createPsArgs:
      #  - maxCohortSizeForFitting: 250000
      #  - errorOnHighCorrelation: true
      #  - prior: laplace, useCrossValidation: true
      #  - control: tolerance 2e-7, cvType auto, fold 10, cvRepetitions 10,
      #             noiseLevel silent, resetCoefficients true, startingVariance 0.01
      #
      # Note: CohortMethod::createCreatePsArgs uses Cyclops control; we map:
      #  - fold -> Cyclops::createControl(fold = 10)
      #  - cvRepetitions -> 10
      #  - tolerance -> 2e-07
      #  - noiseLevel -> "silent"
      #  - resetCoefficients -> TRUE
      #  - startingVariance -> 0.01
      #
      # stopOnError is set FALSE (template rationale) to allow pipeline completion.
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,
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

      # Covariate balance args -------------------------------------------------
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs ----------------------------------------------------
      # <Analysis Specifications>.fitOutcomeModelArgs:
      #  - modelType: cox
      #  - stratified: true
      #  - useCovariates: false
      #  - inversePtWeighting: false
      #  - prior: laplace, useCrossValidation: true
      #  - control: tolerance 2e-7, cvType auto, fold 10, cvRepetitions 10,
      #             noiseLevel quiet, resetCoefficients true, startingVariance 0.01
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

      # createStudyPopArgs -----------------------------------------------------
      # <Analysis Specifications>.createStudyPopArgs:
      #  - restrictToCommonPeriod: false
      #  - firstExposureOnly: false
      #  - washoutPeriod: 0
      #  - removeDuplicateSubjects: "keep all"
      #  - censorAtNewRiskWindow: false
      #  - removeSubjectsWithPriorOutcome: true
      #  - priorOutcomeLookBack: 99999
      #  - timeAtRisks: (two TARs; we iterate and set riskWindowStart/end etc.)
      #
      # Note: CohortMethod expects removeDuplicateSubjects values like
      # "keep first" / "keep all". We pass EXACT: "keep all".
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
        # Not specified explicitly; keep a permissive maximum consistent with template.
        maxDaysAtRisk = 99999
      )

      # Append the settings to Analysis List ----------------------------------
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

# Create CohortMethod module specifications -----------------------------------
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
# Output path follows the template pattern:
#   inst/<studyName>/<studyName>AnalysisSpecification.json
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "uveitissafety", "uveitissafetyAnalysisSpecification.json")
)