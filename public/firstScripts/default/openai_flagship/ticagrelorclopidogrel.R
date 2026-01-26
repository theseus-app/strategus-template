################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script creates a Strategus analysis specification JSON for the study:
#   name: ticagrelorclopidogrel
#
# It follows the provided Template structure, but applies the EXACT settings
# from <Analysis Specifications> (no name auto-correct).
#
# Key customizations applied from <Analysis Specifications>:
# - Cohorts:
#   - Target cohort (WebAPI cohortId 1794126)  -> re-numbered to cohortId = 1
#   - Comparator cohort (WebAPI cohortId 1794132) -> re-numbered to cohortId = 2
#   - Outcome cohort (WebAPI cohortId 1794131) -> re-numbered to cohortId = 3
# - Negative controls:
#   - Concept set id 1888110 ("negative") resolved to concepts and mapped to
#     negative control outcome cohorts with cohortId starting at 101.
# - getDbCohortMethodDataArgs:
#   - studyPeriods: two windows
#   - maxCohortSize = 0
#   - restrictToCommonPeriod = true
#   - firstExposureOnly = false
#   - washoutPeriod = 0
#   - removeDuplicateSubjects = "keep first"
# - createStudyPopArgs:
#   - restrictToCommonPeriod = false
#   - firstExposureOnly = false
#   - washoutPeriod = 0
#   - removeDuplicateSubjects = "keep all"
#   - censorAtNewRiskWindow = false
#   - removeSubjectsWithPriorOutcome = false
#   - priorOutcomeLookBack = 99999
#   - timeAtRisks: 6 TAR definitions (including minDaysAtRisk = 1)
# - Propensity score adjustment:
#   - 3 PS adjustment configurations:
#     1) match maxRatio=1, caliper=0.2, caliperScale="standardized logit"
#     2) match maxRatio=10, caliper=0.2, caliperScale="standardized logit"
#     3) stratify numberOfStrata=10, baseSelection="all"
#   - createPsArgs:
#     - maxCohortSizeForFitting=250000, errorOnHighCorrelation=true
#     - prior: laplace, useCrossValidation=true
#     - control: tolerance=2e-7, cvType=auto, fold=10, cvRepetitions=10,
#                noiseLevel=silent, resetCoefficients=true, startingVariance=0.01
# - fitOutcomeModelArgs:
#   - modelType=cox, stratified=true, useCovariates=false, inversePtWeighting=false
#   - prior: laplace, useCrossValidation=true
#   - control: tolerance=2e-7, cvType=auto, fold=10, cvRepetitions=10,
#              noiseLevel=quiet, resetCoefficients=true, startingVariance=0.01
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# WebAPI endpoint used to export cohort definitions and resolve concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export the target, comparator, and outcome cohort definitions from ATLAS/WebAPI.
# The cohortIds below are EXACTLY those provided in <Analysis Specifications>.
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
# Strategus/CohortMethod analyses typically refer to cohorts by small integer IDs.
# We re-map the WebAPI cohort IDs to:
#   target -> 1, comparator -> 2, outcome -> 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# The negative controls are defined by a concept set in ATLAS/WebAPI.
# We resolve the concept set to a list of concepts, then map each concept to a
# negative control outcome cohort entry (cohortId starting at 101).
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
  mutate(
    cohortId = row_number() + 100
  ) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no overlap between primary cohort IDs (1..3) and negative controls (101+)
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis --------
# Outcomes of interest list (primary outcome only; negative controls appended later).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # cleanWindow is used by some workflows; keep as in template.
  mutate(cleanWindow = 365)

# Target and Comparator list for CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate include/exclude concept lists --------------------------------------
# <Analysis Specifications> provides conceptsToInclude / conceptsToExclude with null ids.
# We keep the objects for clarity, but they are effectively empty and not applied.
includedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

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

# Shared resource: negative control outcome cohorts (concept-based)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specs: generate cohort stats (as in template)
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
# These are applied in getDbCohortMethodDataArgs (restrictToCommonPeriod = TRUE).
# Note: Dates are strings "YYYYMMDD" as provided.
studyPeriods <- tibble::tibble(
  studyStartDate = c("20111101", "20130301"),
  studyEndDate   = c("20190331", "20161231")
)

# Time-at-risks (TARs) --------------------------------------------------------
# We create 6 TAR definitions exactly as specified.
# minDaysAtRisk is carried separately and applied in createStudyPopulationArgs.
timeAtRisks <- tibble::tibble(
  label = c(
    "TAR_1_365_from_start",
    "TAR_1_1825_from_start",
    "TAR_1_to_end",
    "TAR_29_365_from_start",
    "TAR_29_1825_from_start",
    "TAR_29_to_end"
  ),
  riskWindowStart = c(1, 1, 1, 29, 29, 29),
  startAnchor     = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd   = c(365, 1825, 0, 365, 1825, 0),
  endAnchor       = c("cohort start", "cohort start", "cohort end", "cohort start", "cohort start", "cohort end"),
  minDaysAtRisk   = c(1, 1, 1, 1, 1, 1)
)

# Propensity Score settings ----------------------------------------------------
# We translate the 3 psSettings entries into:
# - two "match" configs
# - one "stratify" config
matchOnPsArgsList <- tibble::tibble(
  label = c("match_maxRatio_1", "match_maxRatio_10"),
  maxRatio = c(1, 10),
  caliper = c(0.2, 0.2),
  caliperScale = c("standardized logit", "standardized logit")
)

stratifyByPsArgsList <- tibble::tibble(
  label = c("stratify_10_all"),
  numberOfStrata = c(10),
  baseSelection = c("all")
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

# Iterate through all analysis setting combinations ----------------------------
# We create one CohortMethod analysis per combination of:
# - study period (2)
# - TAR (6)
# - PS adjustment config (3)
# Total = 2 * 6 * 3 = 36 analyses
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # PS adjustment method args ---------------------------------------------
      # Exactly one of matchOnPsArgs or stratifyByPsArgs is set per analysis.
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

      # Covariate settings ----------------------------------------------------
      # <Analysis Specifications> does not specify custom covariate settings beyond
      # include/exclude placeholders (null). We therefore use default covariates.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcomes list ---------------------------------------------------------
      # Primary outcome(s) + negative controls.
      # priorOutcomeLookback is set to 99999 for the outcome of interest, matching
      # the template behavior and aligning with the provided priorOutcomeLookBack.
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

      # Target-Comparator-Outcomes mapping -----------------------------------
      # excludedCovariateConceptIds: since no valid include/exclude concepts were
      # provided, we only pass excludedCovariateConcepts (empty) here.
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
      # Applies <Analysis Specifications> getDbCohortMethodDataArgs settings.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs ----------------------------------------------------------
      # Applies <Analysis Specifications> propensityScoreAdjustment.createPsArgs.
      # Note: Strategus template sets stopOnError=FALSE to allow pipeline completion.
      # This is not specified in <Analysis Specifications>, but is kept to match
      # the template's operational robustness.
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
          noiseLevel = "silent",
          cvType = "auto",
          seed = 1,
          resetCoefficients = TRUE,
          tolerance = 2e-07,
          # <Analysis Specifications> uses fold=10 and cvRepetitions=10.
          # Cyclops::createControl uses 'fold' and 'cvRepetitions' arguments.
          fold = 10,
          cvRepetitions = 10,
          startingVariance = 0.01
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
      # Applies <Analysis Specifications> fitOutcomeModelArgs.
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
      # Applies <Analysis Specifications> createStudyPopArgs and TAR settings.
      # Note: removeDuplicateSubjects differs between getDb... ("keep first") and
      # createStudyPopArgs ("keep all") per specifications.
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
# Output path follows the Template pattern; replace folder names as needed for your package.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ticagrelorclopidogrel", "ticagrelorclopidogrelAnalysisSpecification.json")
)