library(dplyr)
library(Strategus)
library(ROhdsiWebApi)
# Note: The script below uses functions from CohortMethod, FeatureExtraction, Cyclops, and ParallelLogger
# via their namespaces where appropriate. Loading those packages (e.g. library(CohortMethod)) is optional
# but may be helpful when running interactively.

################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script builds a Strategus analysis specification for the "strokerisk"
# study using the OHDSI Strategus framework.  The analysis settings are taken
# from the provided Analysis Specifications and mirrored into Strategus/
# CohortMethod/Cyclops argument objects.
#
# The script is annotated in detail so users can see how each setting in the
# Analysis Specifications is applied to Strategus arguments.
################################################################################

# ------------------------------------------------------------------------------
# Shared Resources -------------------------------------------------------------
# ------------------------------------------------------------------------------

# Base URL for the WebAPI where cohort and concept set definitions will be downloaded.
# The template uses the Atlas demo server; change this if you use a different server.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# We export the cohort definitions for the target, comparator, and outcome.
# The Analysis Specifications define the following cohort ids and names:
#  - targetCohort: id = 1794126, name = "target1"
#  - comparatorCohort: id = 1794132, name = "comparator1"
#  - outcomeCohort: id = 1794131, name = "outcome1"
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts so that target=1, comparator=2, outcome(s)=3, ...
# This renumbering is commonly used by Strategus workflows so cohorts can be
# referenced by small integers.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control concept set -------------------------------------------------
# The specifications list a negative control concept set id 1888110 named "negative".
# We retrieve and expand the concept set into concrete concepts and assign them
# cohortIds starting at 101 to avoid collisions with target/comparator/outcome ids.
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
  dplyr::rename(outcomeConceptId = "conceptId",
                cohortName = "conceptName") %>%
  dplyr::mutate(cohortId = dplyr::row_number() + 100) %>% # negative control ids: 101, 102, ...
  dplyr::select(cohortId, cohortName, outcomeConceptId)

# Sanity-check: ensure no duplicate cohort ids across primary cohorts and negatives
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# ------------------------------------------------------------------------------
# Build Data Frames for Analyses -----------------------------------------------
# ------------------------------------------------------------------------------

# Outcomes: create oList from the cohortDefinitionSet where cohortId == 3 (outcome)
# We also provide a cleanWindow (prior outcome washout used in diagnostics) set to 365
oList <- cohortDefinitionSet %>%
  dplyr::filter(.data$cohortId == 3) %>%
  dplyr::mutate(outcomeCohortId = cohortId,
                outcomeCohortName = cohortName) %>%
  dplyr::select(outcomeCohortId, outcomeCohortName) %>%
  dplyr::mutate(cleanWindow = 365)

# Target and Comparator table for the CohortMethod analyses
# We include columns for targetConceptId/comparatorConceptId (NA) because some template
# code expects these fields when assembling excluded covariates.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  targetConceptId = NA_integer_,
  comparatorConceptId = NA_integer_,
  stringsAsFactors = FALSE
)

# Excluded covariate concepts: the Analysis Specifications provided no concepts
# to exclude, so we create an empty data.frame. This object is passed to the
# CohortMethod targetComparatorOutcomes creation and will be combined with any
# target/comparator drug concept ids to exclude from PS/model fitting.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# Optional: Included covariates (empty because the specification contains no entries)
# includedCovariateConcepts <- data.frame(
#   conceptId = integer(0),
#   conceptName = character(0)
# )

# ------------------------------------------------------------------------------
# CohortGeneratorModule specifications -----------------------------------------
# ------------------------------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource for cohort generation (the cohortDefinitionSet we downloaded)
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

# Shared resources for negative control outcomes (expanded concept set)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",       # use first occurrence per patient
  detectOnDescendants = TRUE     # include descendant concepts
)

# Module specification to run cohort generation; we request generated stats
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ------------------------------------------------------------------------------
# CohortDiagnosticsModule specifications ---------------------------------------
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
# CohortMethodModule settings --------------------------------------------------
# ------------------------------------------------------------------------------

# Study periods
# The Analysis Specifications define two study periods:
# 1) 20010101 - 20171231
# 2) 20010101 - 20150930
studyPeriods <- tibble::tibble(
  studyStartDate = c("20010101", "20010101"), # YYYYMMDD strings
  studyEndDate   = c("20171231", "20150930")
)

# Time-at-risk specification(s) for the study populations:
# The Analysis Specifications list a single TAR:
#  - riskWindowStart = 1, startAnchor = "cohort start"
#  - riskWindowEnd   = 0, endAnchor   = "cohort end"
#  - minDaysAtRisk = 1
timeAtRisks <- tibble::tibble(
  label = c("TAR1"),
  riskWindowStart = c(1),
  startAnchor = c("cohort start"), # allowed: "cohort start" | "cohort end"
  riskWindowEnd = c(0),
  endAnchor = c("cohort end")      # allowed: "cohort start" | "cohort end"
)

# ---------------------------------------------------------------------------
# Propensity Score Adjustment configurations (psConfigList)
# ---------------------------------------------------------------------------
# The Analysis Specifications provide three PS settings entries:
# 1) both matchOnPsArgs and stratifyByPsArgs == null -> represents "no PS adjustment"
# 2) matchOnPsArgs: maxRatio=1, caliper=0.05, caliperScale="propensity score"
# 3) matchOnPsArgs: maxRatio=10, caliper=0.2, caliperScale="standardized logit"
#
# We'll construct psConfigList directly to reflect these three configs.
psConfigList <- list(
  # 1) Unadjusted (no PS matching/stratification)
  list(
    method = "none",
    label = "unadjusted",
    params = NULL
  ),
  # 2) Match on PS with maxRatio=1, caliper=0.05 (caliper scale: propensity score)
  list(
    method = "match",
    label = "match_ps_caliper_0.05_ratio_1",
    params = list(
      maxRatio = 1,
      caliper = 0.05,
      caliperScale = "propensity score"
    )
  ),
  # 3) Match on PS with maxRatio=10, caliper=0.2 (caliper scale: standardized logit)
  list(
    method = "match",
    label = "match_logit_caliper_0.2_ratio_10",
    params = list(
      maxRatio = 10,
      caliper = 0.2,
      caliperScale = "standardized logit"
    )
  )
)

# ------------------------------------------------------------------------------
# Build CohortMethod analysis list by iterating over study periods, TARs, and PS
# ------------------------------------------------------------------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Build matching/stratification args based on the psCfg method
      if (psCfg$method == "match") {
        # Create MatchOnPs args using the CohortMethod helper
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
        # "none" or any other -> no PS adjustment
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- NULL
      }

      # Covariate settings: use default covariates and add descendants to exclude
      # as per common practice. The Analysis Specifications didn't provide custom
      # covariate lists, so we use the default FeatureExtraction covariates.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build outcome list for CohortMethod:
      # - Outcomes of interest (from oList)
      # - Negative control outcomes (from negativeControlOutcomeCohortSet)
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

      # targetComparatorOutcomesList: build for each target-comparator pair (only one here)
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        # Excluded covariate concept ids: combine any explicit excluded covariates with
        # the target/comparator drug concept IDs (if provided). Our spec does not
        # supply targetConceptId/comparatorConceptId, so these are NA and will be
        # ignored when combined with excludedCovariateConcepts$conceptId.
        excludedIds <- c(
          cmTcList$targetConceptId[i],
          cmTcList$comparatorConceptId[i],
          excludedCovariateConcepts$conceptId
        )
        # Drop NA entries
        excludedIds <- excludedIds[!is.na(excludedIds)]

        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedIds
        )
      }

      # Get DB CohortMethod data args:
      # Using the Analysis Specifications (getDbCohortMethodDataArgs):
      # - studyPeriods replaced by the loop variables studyStartDate/studyEndDate
      # - maxCohortSize = 0 (no truncation)
      # - restrictToCommonPeriod = FALSE
      # - firstExposureOnly = TRUE
      # - washoutPeriod = 183
      # - removeDuplicateSubjects = "keep first"
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        firstExposureOnly = TRUE,
        washoutPeriod = 183,
        removeDuplicateSubjects = "keep first",
        covariateSettings = covariateSettings
      )

      # Create PS args (createPsArgs) from Analysis Specifications (propensityScoreAdjustment$createPsArgs)
      # - maxCohortSizeForFitting = 250000
      # - errorOnHighCorrelation = TRUE
      # - prior: laplace with useCrossValidation = TRUE
      # - control: tolerance, cvType, fold, cvRepetitions, noiseLevel, resetCoefficients, startingVariance
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # keep false to allow Strategus to continue analyses even if PS fails
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
          startingVariance = 0.01
        )
      )

      # Covariate balance computation args (shared and per-analysis)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit outcome model args based on Analysis Specifications (fitOutcomeModelArgs)
      # - modelType = "cox", stratified = TRUE, useCovariates = FALSE, inversePtWeighting = FALSE
      # - prior: laplace with useCrossValidation = TRUE
      # - control: tolerance, cvType, fold, cvRepetitions, noiseLevel, resetCoefficients, startingVariance
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = TRUE,
        useCovariates = FALSE,
        inversePtWeighting = FALSE,
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
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Create study population args from the Analysis Specifications (createStudyPopArgs)
      # - restrictToCommonPeriod = FALSE
      # - firstExposureOnly = FALSE
      # - washoutPeriod = 0
      # - removeDuplicateSubjects = "keep all"
      # - censorAtNewRiskWindow = FALSE
      # - removeSubjectsWithPriorOutcome = FALSE
      # - priorOutcomeLookBack = 99999
      # - riskWindowStart/End & anchors taken from timeAtRisks for this loop
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
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Assemble the CohortMethod analysis object and append to the list
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

# ------------------------------------------------------------------------------
# Create CohortMethod Module Specifications ------------------------------------
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
# Assemble final analysisSpecifications and save to JSON -----------------------
# ------------------------------------------------------------------------------

analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the resulting analysis specification JSON to inst/strokerisk/
# The file name follows the template naming convention: <studyName>AnalysisSpecification.json
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "strokerisk", "strokeriskAnalysisSpecification.json")
)

# End of script ----------------------------------------------------------------
# Notes:
# - This script uses the exact names and numeric ids provided in the Analysis Specifications.
# - If you wish to change the baseUrl, cohort ids, or other settings, edit the blocks above.
# - If you want to include specific covariates (covariate selection), populate the
#   includedCovariateConcepts/excludedCovariateConcepts data.frames accordingly.
# - When running Strategus on a compute environment, ensure all required OHDSI packages
#   (CohortMethod, FeatureExtraction, Cyclops, ROhdsiWebApi, ParallelLogger) are installed.