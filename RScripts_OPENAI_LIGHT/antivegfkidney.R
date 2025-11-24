library(dplyr)
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(ParallelLogger)

################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script builds a Strategus analysis specification JSON for the study
# described in <Analysis Specifications>.
#
# Study name: antivegfkidney
#
# Notes:
# - The script follows the template structure and applies the exact cohort IDs
#   and names provided in the Analysis Specifications.
# - Detailed comments are included to explain how each setting from the
#   Analysis Specifications is translated into Strategus / module settings.
################################################################################

# Base Atlas WebAPI used to fetch cohort and concept set definitions.
# You can change this to your environment's WebAPI if needed.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ------------------------------------------------------------------------------
# Cohort Definitions ----------------------------------------------------------
# ------------------------------------------------------------------------------
# The Analysis Specifications list three cohorts:
# - targetCohort  : id = 1794126, name = "target1"
# - comparatorCohort : id = 1794132, name = "comparator1"
# - outcomeCohort : id = 1794131, name = "outcome1"
#
# We export these cohorts from the Atlas/WebAPI so that Strategus/CohortGenerator
# has the cohort JSON definitions stored as shared resources.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts within the exported set to the small integers that
# Strategus / CohortMethod examples commonly use:
#   target  -> 1
#   comparator -> 2
#   outcome -> 3
#
# This renumbering does not change the cohort definitions themselves; it only
# assigns cohortId integers for downstream module references.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, "cohortId"] <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, "cohortId"] <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, "cohortId"] <- 3

# ------------------------------------------------------------------------------
# Negative control outcomes ---------------------------------------------------
# ------------------------------------------------------------------------------
# The Analysis Specifications include a negative control concept set:
# concept set id = 1888110, name = "negative"
# We fetch the concept set, resolve it to concepts, and convert to a
# negativeControlOutcomeCohortSet that will be used as shared resources.
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
  # Align column names with the expected format for negative control cohorts
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  # Assign cohortIds for negative controls starting at 101 to avoid
  # colliding with the target/comparator/outcome numerical ids (1,2,3).
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Sanity check: ensure no duplicate cohort IDs across cohortDefinitionSet and
# negativeControlOutcomeCohortSet.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# ------------------------------------------------------------------------------
# Build cohort lists for downstream modules -----------------------------------
# ------------------------------------------------------------------------------
# Outcomes list (oList): build from the renumbered cohortDefinitionSet.
# We find the row where cohortId == 3 (the outcome) and prepare the fields
# CohortMethod expects (outcomeCohortId, outcomeCohortName, cleanWindow).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId,
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # The Analysis Specifications provide a cleanWindow of 365 days.
  mutate(cleanWindow = 365)

# Target/Comparator table (cmTcList) expected by CohortMethod module.
# Use exact cohort names from the Analysis Specifications.
# We also include columns targetConceptId and comparatorConceptId as NA_integer_
# for compatibility with the template's downstream usage.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  targetConceptId = as.integer(NA),
  comparatorConceptId = as.integer(NA),
  stringsAsFactors = FALSE
)

# Excluded covariate concepts:
# The Analysis Specifications for covariateSelection provided empty includes/excludes.
# We create an empty data.frame for excludedCovariateConcepts so downstream code can
# safely reference this object without errors.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# CohortGeneratorModule -------------------------------------------------------
# ------------------------------------------------------------------------------
# Create shared resources and module specifications for cohort generation.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Share the cohort definitions we downloaded above.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Share the negative control outcome cohort set.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specification: run cohort generation and compute basic cohort stats.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ------------------------------------------------------------------------------
# CohortDiagnosticsModule -----------------------------------------------------
# ------------------------------------------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# We request a broad set of diagnostics. These flags follow the Template
# diagnostics configuration and can be adjusted if you prefer fewer/more diagnostics.
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
# CohortMethodModule ----------------------------------------------------------
# ------------------------------------------------------------------------------
# The Analysis Specifications provide the following CohortMethod-relevant settings:
# - getDbCohortMethodDataArgs: studyPeriods contains a single entry with studyStartDate = null,
#   studyEndDate = null, and maxCohortSize = 0.
# - createStudyPopArgs: many settings (firstExposureOnly = TRUE, washoutPeriod = 365, ...)
# - propensity score adjustment: one PS setting that uses matching with maxRatio = 1,
#   caliper = 0.2, caliperScale = "standardized logit"
# - createPsArgs: contains Cyclops prior and control settings
# - fitOutcomeModelArgs: Cox model, stratified = FALSE, etc.
#
# Build studyPeriods tibble (null dates in the Analysis Specifications are translated to NA)
studyPeriods <- tibble::tibble(
  studyStartDate = c(NA_character_), # NA means "no restriction"
  studyEndDate = c(NA_character_)
)

# Time-at-risk (TAR) from the Analysis Specifications:
# single TAR with riskWindowStart = 0 (start anchored at cohort start),
# riskWindowEnd = 0 (end anchored at cohort end). We set a label to identify it.
timeAtRisks <- tibble::tibble(
  label = c("TAR_0_to_0_cohortStart_cohortEnd"),
  riskWindowStart = c(0),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(0),
  endAnchor = c("cohort end")
)

# Propensity Score - Match on PS settings (as provided)
matchOnPsArgsList <- tibble::tibble(
  label = c("match_on_ps"),
  maxRatio = c(1),
  caliper = c(0.2),
  caliperScale = c("standardized logit") # matches the Analysis Specifications
)

# No stratify-by-PS configurations provided in the Analysis Specifications
stratifyByPsArgsList <- tibble::tibble(
  label = character(0),
  numberOfStrata = integer(0),
  baseSelection = character(0)
)

# Build psConfigList combining match and stratify configurations (if any).
psConfigList <- list()

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

# Prepare lists that will be filled with CohortMethod analysis definitions
cmAnalysisList <- list()
analysisId <- 1

# targetComparatorOutcomesList needs to be created once; it is used by the
# CohortMethod module specifications below. We will build it inside the loops
# (as in the Template) but ensure it exists in the global environment.
targetComparatorOutcomesList <- list()

# Loop over study periods, time-at-risks, and ps configurations to construct
# a list of CohortMethod analyses (cmAnalysisList). For this study there is
# a single studyPeriod (with NA dates), a single TAR, and a single PS config.
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Build PS adjustment args (either matching or stratification).
      if (psCfg$method == "match") {
        # We do not directly construct CohortMethod::createMatchOnPsArgs here
        # because later we pass the matching parameters into the createCmAnalysis
        # helper. That helper expects "matchOnPsArgs" to be a CohortMethod
        # arguments object. We construct it using the template function below.
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

      # Covariate settings:
      # The Analysis Specifications provided empty include / exclude concept lists.
      # We therefore use the default covariates, but ensure descendants are
      # added to the exclude list where relevant.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build the outcome list: include the 'true' outcome(s) (from oList)
      # and the negative control outcomes (from negativeControlOutcomeCohortSet).
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
          # Negative controls are marked as outcomeOfInterest = FALSE and
          # the trueEffectSize is set to 1 (null effect) per template convention.
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Build targetComparatorOutcomesList entries based on cmTcList and the
      # outcomes built above. The createTargetComparatorOutcomes call accepts
      # excludedCovariateConceptIds: combine the target/comparator concept IDs
      # (if provided) and any excludedCovariateConcepts.
      for (i in seq_len(nrow(cmTcList))) {
        excludedIds <- c(
          cmTcList$targetConceptId[i],
          cmTcList$comparatorConceptId[i],
          excludedCovariateConcepts$conceptId
        )
        # Remove NA values to ensure we pass only valid integers.
        excludedIds <- excludedIds[!is.na(excludedIds)]

        targetComparatorOutcomesList[[length(targetComparatorOutcomesList) + 1]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedIds
        )
      }

      # getDbCohortMethodDataArgs: construct the arguments used to extract
      # the CohortMethod data from the CDM. The Analysis Specifications
      # specified studyPeriods with null dates and maxCohortSize = 0.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs: follow settings from Analysis Specifications
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # keep FALSE to allow Strategus to continue where PS fitting fails
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

      # Covariate balance computation args (shared and per-analysis)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs: follow Analysis Specifications (Cox, not stratified)
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = FALSE,
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

      # createStudyPopArgs: map the createStudyPopArgs from the Analysis Specifications:
      # - restrictToCommonPeriod = FALSE
      # - firstExposureOnly = TRUE
      # - washoutPeriod = 365
      # - removeDuplicateSubjects = "keep all"
      # - censorAtNewRiskWindow = FALSE
      # - removeSubjectsWithPriorOutcome = TRUE
      # - priorOutcomeLookback = 99999
      # - timeAtRisks: use values from the timeAtRisks tibble
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = TRUE,
        washoutPeriod = 365,
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

      # Assemble the CohortMethod analysis configuration and append to the list.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(is.na(studyStartDate), "", studyStartDate),
          ifelse(is.na(studyEndDate), "", studyEndDate),
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

# Create CohortMethod module specifications from the analysis list we built.
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
# Assemble final Strategus analysis specifications ----------------------------
# ------------------------------------------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Persist the analysis specifications to JSON in the study's inst/ directory.
# File path uses the exact study name "antivegfkidney" from the Analysis Specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "antivegfkidney", "antivegfkidneyAnalysisSpecification.json")
)