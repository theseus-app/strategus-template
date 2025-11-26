################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script creates an analysis specification JSON for the Strategus package
# using the analysis settings defined in the provided Analysis Specifications.
#
# Important:
# - Cohort IDs and names are taken EXACTLY from the Analysis Specifications.
# - This script includes extensive inline annotations to show how each setting
#   from the Analysis Specifications is applied when building the Strategus
#   analysis specification object.
################################################################################

# Load required packages -------------------------------------------------------
# dplyr and Strategus are required; CohortMethod, Cyclops and ROhdsiWebApi are
# used through explicit namespace calls but we load them here for clarity and
# convenience when building objects.
library(dplyr)
library(Strategus)
library(CohortMethod)
library(Cyclops)
library(ROhdsiWebApi)
library(FeatureExtraction)
library(ParallelLogger)

# Shared Resources -------------------------------------------------------------
# Set the ATLAS WebAPI base URL. Change this to your WebAPI instance if needed.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ------------------------------------------------------------------------------
# Cohort Definitions
# ------------------------------------------------------------------------------
# Export the cohort definitions for the target, comparator and outcome using
# EXACT cohort IDs from the Analysis Specifications:
#   targetCohort id = 1794126  (name: "target1")
#   comparatorCohort id = 1794132 (name: "comparator1")
#   outcomeCohort id = 1794131 (name: "outcome1")
#
# generateStats = TRUE will produce summary statistics for these cohort
# definitions at export time.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts so that Strategus/CohortMethod analyses use compact ids:
#   target -> 1
#   comparator -> 2
#   outcome -> 3
# This renumbering mirrors the approach in the Template.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# ------------------------------------------------------------------------------
# Negative control outcomes
# ------------------------------------------------------------------------------
# The Analysis Specifications provide a concept set ID for negative controls:
#   negativeControlConceptSet id = 1888110 (name: "negative")
#
# We resolve this concept set to individual concepts and translate them into a
# cohort-like table with cohortId values that do not clash with 1,2,3. We
# follow the Template's convention to start negative control cohortIds at 101.
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
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  mutate(cohortId = row_number() + 100) %>% # 101, 102, ...
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no duplicated cohort IDs between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# ------------------------------------------------------------------------------
# Outcome list (oList)
# ------------------------------------------------------------------------------
# Build a small table describing the primary outcome(s) used in CohortMethod.
# We use the renumbered cohortId == 3 for the single outcome described in the
# Analysis Specifications (name: "outcome1"). We set a cleanWindow of 365 as in
# the Template example.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# ------------------------------------------------------------------------------
# Target and Comparator for the CohortMethod analysis (cmTcList)
# ------------------------------------------------------------------------------
# Use the renumbered ids: target = 1, comparator = 2
# Names are EXACT from the Analysis Specifications.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# Excluded / Included covariate concept lists
# ------------------------------------------------------------------------------
# The Analysis Specifications provide empty lists for covariatesToInclude and
# covariatesToExclude. Represent these as empty data.frames to be used when
# creating exclude lists in CohortMethod (they will simply be empty).
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# (Optional) If you had included covariates, you would set them here similarly:
# includedCovariateConcepts <- data.frame(conceptId = c(...), conceptName = c(...))

# ------------------------------------------------------------------------------
# CohortGeneratorModule Shared Resources & Module Specifications
# ------------------------------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ------------------------------------------------------------------------------
# CohortDiagnosticsModule Settings (follow Template defaults where appropriate)
# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
# CohortMethodModule Settings
# ------------------------------------------------------------------------------
# The Analysis Specifications define two study periods to restrict cohort
# extraction. We translate them into a tibble for iteration.
studyPeriods <- tibble::tibble(
  studyStartDate = c("20111101", "20130301"), # EXACT strings from Analysis Specifications
  studyEndDate   = c("20190331", "20161231")
)

# The Analysis Specifications define six time-at-risk windows (createStudyPopArgs).
# We create a tibble with human-readable labels and the TAR settings. The
# labels are for readability and used in analysis descriptions.
timeAtRisks <- tibble::tibble(
  label = c(
    "TAR_start1_end365_cohortStart",
    "TAR_start1_end1825_cohortStart",
    "TAR_start1_end0_cohortEnd",
    "TAR_start29_end365_cohortStart",
    "TAR_start29_end1825_cohortStart",
    "TAR_start29_end0_cohortEnd"
  ),
  riskWindowStart = c(1, 1, 1, 29, 29, 29),
  startAnchor = c(
    "cohort start",
    "cohort start",
    "cohort start",
    "cohort start",
    "cohort start",
    "cohort start"
  ),
  riskWindowEnd = c(365, 1825, 0, 365, 1825, 0),
  endAnchor = c(
    "cohort start",
    "cohort start",
    "cohort end",
    "cohort start",
    "cohort start",
    "cohort end"
  ),
  minDaysAtRisk = rep(1, 6)
)

# ------------------------------------------------------------------------------
# Propensity Score Configuration
# ------------------------------------------------------------------------------
# The Analysis Specifications list three PS settings:
#  1) match on PS: maxRatio = 1, caliper = 0.2, caliperScale = "standardized logit"
#  2) match on PS: maxRatio = 10, caliper = 0.2, caliperScale = "standardized logit"
#  3) stratify by PS: numberOfStrata = 10, baseSelection = "all"
#
# We build two separate data.frames for "match" and "stratify" PS configs and
# convert them into a unified psConfigList that the Template loop consumes.
matchOnPsArgsList <- tibble::tibble(
  label = c("match_max1", "match_max10"),
  maxRatio = c(1L, 10L),
  caliper = c(0.2, 0.2),
  caliperScale = c("standardized logit", "standardized logit")
)

stratifyByPsArgsList <- tibble::tibble(
  label = c("stratify_10"),
  numberOfStrata = c(10L),
  baseSelection = c("all") # EXACT from Analysis Specifications
)

# Build a single PS configuration list (each element: method, label, params)
psConfigList <- list()

# Convert match rows to psConfigList entries
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

# Convert stratify rows to psConfigList entries
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

# ------------------------------------------------------------------------------
# Build CohortMethod Analyses (iterate study periods x TARs x PS configurations)
# ------------------------------------------------------------------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Build matchOnPsArgs or stratifyByPsArgs depending on the PS method
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = as.integer(psCfg$params$maxRatio),
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = as.integer(psCfg$params$numberOfStrata),
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      } else {
        stop("Unknown PS method in psConfigList")
      }

      # Covariate construction: use default covariates but exclude descendants
      # for excluded concepts. This mirrors the Template approach and is
      # consistent with the blank covariate selection in the Analysis Specifications.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build the list of outcomes for this analysis:
      # - Primary outcomes (from oList) are set as outcomes of interest.
      # - Negative controls are appended from negativeControlOutcomeCohortSet.
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

      # Build targetComparatorOutcomesList: one element per target-comparator pair.
      # Excluded covariate concept ids are provided (empty in this spec).
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # getDbCohortMethodDataArgs: use EXACT settings from Analysis Specifications
      # (restrictToCommonPeriod = true, firstExposureOnly = false, washoutPeriod = 0,
      # removeDuplicateSubjects = "keep first", maxCohortSize = 0)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        restrictToCommonPeriod = TRUE,     # EXACT from Analysis Specifications
        maxCohortSize = 0,                 # 0 means no size limit; EXACT
        firstExposureOnly = FALSE,         # EXACT
        washoutPeriod = 0,                 # EXACT
        removeDuplicateSubjects = "keep first", # EXACT
        covariateSettings = covariateSettings
      )

      # createCreatePsArgs: EXACT settings from Analysis Specifications
      # - maxCohortSizeForFitting = 250000
      # - errorOnHighCorrelation = TRUE
      # - prior: laplace with cross-validation
      # - control: tolerance, cvType, fold, cvRepetitions, noiseLevel, resetCoefficients, startingVariance
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # allow Strategus to continue even if a PS fit fails for a single analysis
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",
          exclude = c(0),
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          tolerance = 2e-7,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "silent",
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          seed = 1
        )
      )

      # Covariate balance computations: shared and per-analysis settings
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit outcome model args: EXACT settings from Analysis Specifications
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = TRUE,   # EXACT
        useCovariates = FALSE, # EXACT
        inversePtWeighting = FALSE, # EXACT
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          tolerance = 2e-7,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          seed = 1
        )
      )

      # createStudyPopArgs: use EXACT settings from Analysis Specifications for
      # study population creation. Note that riskWindowStart / riskWindowEnd /
      # anchors are supplied from the timeAtRisks tibble above.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,         # EXACT
        firstExposureOnly = FALSE,              # EXACT
        washoutPeriod = 0,                      # EXACT
        removeDuplicateSubjects = "keep all",   # EXACT
        censorAtNewRiskWindow = FALSE,          # EXACT
        removeSubjectsWithPriorOutcome = FALSE, # EXACT
        priorOutcomeLookback = 99999,           # EXACT
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999
      )

      # Prepare a human-readable description for this analysis
      description <- sprintf(
        "Study: %s-%s; TAR: %s; PS: %s",
        studyStartDate,
        studyEndDate,
        timeAtRisks$label[t],
        psCfg$label
      )

      # Append the CohortMethod analysis to the list
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = description,
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
    } # end psConfigList loop
  } # end timeAtRisks loop
} # end studyPeriods loop

# ------------------------------------------------------------------------------
# CohortMethod Module Specifications
# ------------------------------------------------------------------------------
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# ------------------------------------------------------------------------------
# Build the full Strategus analysisSpecifications object and save to JSON
# ------------------------------------------------------------------------------
# The Analysis Specifications name at the root of the JSON will correspond to
# the folder/file names you provide. Per the Analysis Specifications top-level
# "name" field, we use "ticagrelorclopidogrel".
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the resulting specification JSON to inst/<studyName>/<studyName>AnalysisSpecification.json
# Use the exact study name from Analysis Specifications: "ticagrelorclopidogrel"
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ticagrelorclopidogrel", "ticagrelorclopidogrelAnalysisSpecification.json")
)

# End of script ----------------------------------------------------------------
# The saved JSON can be inspected or used as input to Strategus-driven study
# execution pipelines.