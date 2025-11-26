library(dplyr)
library(Strategus)
library(ROhdsiWebApi)

################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script builds an OHDSI Strategus analysis specification JSON for the
# study "covid19ppiandh2ra" using the settings provided in the Analysis
# Specifications. The script follows the template layout and includes detailed
# annotations to explain how each setting is applied.
#
# Notes:
# - Cohort definitions are exported from the WebAPI (baseUrl). Change baseUrl
#   to point to your Atlas/WebAPI if needed.
# - The script creates shared resources and module specifications for:
#     - CohortGeneratorModule
#     - CohortDiagnosticsModule
#     - CohortMethodModule
#
# Output:
# - A JSON file saved to inst/covid19ppiandh2ra/covid19ppiandh2raAnalysisSpecification.json
#
################################################################################

# -----------------------------------------------------------------------------
# Base URL for the Atlas/WebAPI where cohort definitions and concept sets live.
# Change this to your environment if necessary.
# -----------------------------------------------------------------------------
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# -----------------------------------------------------------------------------
# Cohort Definitions
# We export the cohort definitions for:
#  - Target cohort (id 1794126) -> renumbered to cohortId = 1
#  - Comparator cohort (id 1794132) -> renumbered to cohortId = 2
#  - Outcome cohort (id 1794131) -> renumbered to cohortId = 3
#
# generateStats = TRUE will include cohort stats in the exported definition set.
# -----------------------------------------------------------------------------
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to small integers used by the Strategus workflow:
# - target -> 1; comparator -> 2; outcome -> 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# -----------------------------------------------------------------------------
# Negative control outcomes: concept set id 1888110 (named "negative")
# We resolve the concept set into individual concepts and convert them into
# a cohort-like table with unique cohortIds 101, 102, 103, ...
# -----------------------------------------------------------------------------
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
  ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  mutate(cohortId = row_number() + 100) %>% # negative control cohortIds start at 101
  select(cohortId, cohortName, outcomeConceptId)

# Sanity check: ensure no duplicate cohortIds between main cohorts and negatives
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# -----------------------------------------------------------------------------
# Build lists/data.frames describing the cohorts used in analysis:
# - oList: outcome cohort(s) (from exported cohortDefinitionSet). We add
#   a cleanWindow as required by many downstream analyses.
# - cmTcList: a data.frame of target/comparator pairs for CohortMethod.
# -----------------------------------------------------------------------------

# Outcomes: select the exported outcome (renumbered as cohortId == 3)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId,
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # cleanWindow indicates the washout/clean period for the outcome (days)
  mutate(cleanWindow = 365)

# Target and Comparator definitions for CohortMethod
# Use EXACT names from Analysis Specifications:
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# Covariate selection - The Analysis Specifications provide empty include/exclude
# lists. We'll create an empty excludedCovariateConcepts table (no exclusions).
# If you need to exclude drug concepts (e.g., the exposures of interest),
# populate this data.frame with conceptId / conceptName here.
# -----------------------------------------------------------------------------
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# CohortGeneratorModule shared resources and module specifications
# -----------------------------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource: the exported cohort definition set
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

# Shared resource: negative control outcome cohort set (resolved concept set)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# CohortGenerator module specification: instruct Strategus to generate cohorts and
# produce statistics (generateStats = TRUE)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# -----------------------------------------------------------------------------
# CohortDiagnosticsModule specifications
# We request a typical set of diagnostics. These can be adjusted as needed.
# -----------------------------------------------------------------------------
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

# -----------------------------------------------------------------------------
# CohortMethodModule: build analyses according to the Analysis Specifications
#
# Key settings in Analysis Specifications used here:
# - studyPeriods: one window 20200101 -> 20200515
# - getDbCohortMethodDataArgs: maxCohortSize = 0, restrictToCommonPeriod = FALSE,
#   firstExposureOnly = TRUE, washoutPeriod = 180, removeDuplicateSubjects = "keep first"
# - createStudyPopArgs: restrictToCommonPeriod = FALSE, firstExposureOnly = FALSE,
#   washoutPeriod = 0, removeDuplicateSubjects = "keep all", censorAtNewRiskWindow = FALSE,
#   removeSubjectsWithPriorOutcome = FALSE, priorOutcomeLookBack = 99999
# - timeAtRisks: one TAR: start=1 anchored to "cohort start", end=99999 anchored to "cohort start"
# - propensity score adjustments: three configs:
#     1) no PS adjustment (unadjusted)
#     2) matching: maxRatio=4, caliper=0.2, caliperScale="standardized logit"
#     3) stratification: numberOfStrata=5, baseSelection="all"
# - createPsArgs: settings for Cyclops prior/control (laplace, cross-validation, control params)
# - fitOutcomeModelArgs: Cox model, stratified = TRUE, useCovariates = FALSE, inversePtWeighting = FALSE,
#   prior = laplace with CV, control with specified CV settings
# -----------------------------------------------------------------------------

# Study period(s): use exact dates from Analysis Specifications
studyPeriods <- tibble::tibble(
  studyStartDate = c("20200101"),
  studyEndDate   = c("20200515")
)

# Time-at-risks: single TAR definition from Analysis Specifications
timeAtRisks <- tibble::tibble(
  label = c("primary"),                    # human-readable label for the TAR
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),        # "cohort start" | "cohort end"
  riskWindowEnd  = c(99999),
  endAnchor = c("cohort start")           # "cohort start" | "cohort end"
)

# Build PS configuration list explicitly to represent the three PS settings
psConfigList <- list(
  list(
    method = "none",   # unadjusted analysis: no matching / stratification
    label = "unadjusted",
    params = NULL
  ),
  list(
    method = "match",
    label = "match_maxRatio4_caliper0.2_stdlogit",
    params = list(
      maxRatio = 4,
      caliper = 0.2,
      caliperScale = "standardized logit"
    )
  ),
  list(
    method = "stratify",
    label = "stratify_5_all",
    params = list(
      numberOfStrata = 5,
      baseSelection = "all"
    )
  )
)

# -----------------------------------------------------------------------------
# Pre-build objects that will be reused for each CohortMethod analysis:
# - covariateSettings: default covariates (can be customized if required)
# - createPsArgs: Cyclops prior/control & other PS-fitting options (from specs)
# - computeCovariateBalanceArgs and computeSharedCovariateBalanceArgs
# - fitOutcomeModelArgs: configuration for the outcome model (Cox)
# -----------------------------------------------------------------------------

# Covariate settings: use default covariates; keep ability to exclude descendants
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE
)

# createPsArgs: uses settings from Analysis Specifications
createPsArgs_template <- CohortMethod::createCreatePsArgs(
  maxCohortSizeForFitting = 250000,
  errorOnHighCorrelation = TRUE,
  stopOnError = FALSE,    # allow Strategus to proceed even if individual fits fail
  estimator = "att",      # ATT estimator by default
  prior = Cyclops::createPrior(
    priorType = "laplace",
    exclude = c(0),      # exclude intercept
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

# Covariate balance computation arguments
computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
  maxCohortSize = 250000,
  covariateFilter = NULL
)
computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
  maxCohortSize = 250000,
  covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
)

# fitOutcomeModelArgs - follow Analysis Specifications for Cox model settings
fitOutcomeModelArgs_template <- CohortMethod::createFitOutcomeModelArgs(
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

# -----------------------------------------------------------------------------
# Build CohortMethod analyses by iterating studyPeriods x timeAtRisks x psConfigList
# Each combination results in a CohortMethod analysis specification.
# -----------------------------------------------------------------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {
    # Extract TAR details
    tar_label <- timeAtRisks$label[t]
    tar_riskWindowStart <- timeAtRisks$riskWindowStart[t]
    tar_startAnchor <- timeAtRisks$startAnchor[t]
    tar_riskWindowEnd <- timeAtRisks$riskWindowEnd[t]
    tar_endAnchor <- timeAtRisks$endAnchor[t]

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Build matchOnPsArgs / stratifyByPsArgs according to the PS method
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
        # "none" -> no PS adjustment: both args NULL
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- NULL
      }

      # Build outcome list: primary outcome(s) from oList + negative controls
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
          # negative controls are treated as non-outcomes-of-interest for effect-size calibration
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Build targetComparatorOutcomesList (one element per row in cmTcList)
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude covariates if provided (empty here)
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # getDbCohortMethodDataArgs: use settings from Analysis Specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        restrictToCommonPeriod = FALSE,    # as specified
        firstExposureOnly = TRUE,          # getDbCohortMethodDataArgs setting
        washoutPeriod = 180,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs: reuse template object (same settings for all PS-enabled analyses)
      createPsArgs <- createPsArgs_template

      # createStudyPopArgs: from Analysis Specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = FALSE,
        priorOutcomeLookback = 99999,
        riskWindowStart = tar_riskWindowStart,
        startAnchor = tar_startAnchor,
        riskWindowEnd = tar_riskWindowEnd,
        endAnchor = tar_endAnchor,
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Append the settings to the CM analysis list
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          studyStartDate,
          studyEndDate,
          tar_label,
          psCfg$label
        ),
        getDbCohortMethodDataArgs = getDbCohortMethodDataArgs,
        createStudyPopArgs = createStudyPopArgs,
        createPsArgs = createPsArgs,
        matchOnPsArgs = matchOnPsArgs,
        stratifyByPsArgs = stratifyByPsArgs,
        computeSharedCovariateBalanceArgs = computeSharedCovariateBalanceArgs,
        computeCovariateBalanceArgs = computeCovariateBalanceArgs,
        fitOutcomeModelArgs = fitOutcomeModelArgs_template
      )

      analysisId <- analysisId + 1
    }
  }
}

# -----------------------------------------------------------------------------
# Build CohortMethod module specifications
# - cmAnalysisList: list of CohortMethod analysis specifications created above
# - targetComparatorOutcomesList: specify T/C pairs and their outcomes
# -----------------------------------------------------------------------------
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# -----------------------------------------------------------------------------
# Assemble the full analysis specifications object and write to JSON
# -----------------------------------------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specification JSON into the inst/<studyName> folder
outputDir <- file.path("inst", "covid19ppiandh2ra")
if (!dir.exists(outputDir)) dir.create(outputDir, recursive = TRUE)

ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path(outputDir, "covid19ppiandh2raAnalysisSpecification.json")
)

# End of CreateStrategusAnalysisSpecification.R
# The generated JSON can be inspected and then used with Strategus to orchestrate the
# pipeline execution for the "covid19ppiandh2ra" study.