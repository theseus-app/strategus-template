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
# This script builds a Strategus analysis specification JSON for the study:
#   name: doacsandwarfarin
#
# The settings are taken from the provided Analysis Specifications and are
# applied verbatim (exact cohort IDs, time-at-risk definitions, PS/fitting
# parameters, outcome settings, etc.).  Detailed inline annotations explain
# how each setting from the specification maps to Strategus/CohortMethod
# arguments.
################################################################################

# ---------------------------------------------------------------------
# Shared Resources ----------------------------------------------------
# ---------------------------------------------------------------------
# The web API base URL used to export cohort and concept set definitions.
# NOTE: replace with your Atlas WebAPI endpoint if needed.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ---------------------------------------------------------------------
# Cohort Definitions (Target / Comparator / Outcome)
# ---------------------------------------------------------------------
# According to the Analysis Specifications:
#   targetCohort:      id = 1794126, name = "target1"
#   comparatorCohort:  id = 1794132, name = "comparator1"
#   outcomeCohort(s):  id = 1794131, name = "outcome1"
#
# We export the cohort definitions by id and then re-number them so that
# the study-specific target/comparator/outcome IDs are compact (1,2,3).
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts so that the target becomes cohortId 1, comparator 2,
# and outcome 3. Many Strategus examples assume compact numbering starting
# at 1 for the main study cohorts.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# ---------------------------------------------------------------------
# Negative Control Outcomes
# ---------------------------------------------------------------------
# Analysis Specifications specify a negative control concept set:
#   id = 1888110, name = "negative"
#
# We export the concept set definition and resolve it into individual
# concepts. Each negative control is then represented as a cohortId,
# and we offset those ids by +100 to avoid colliding with main study
# cohort IDs (1,2,3,... -> 101,102,...).
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
  # Assign cohort IDs to negative controls starting at 101 to avoid overlap
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure there are no duplicated cohort IDs between
# primary cohorts and negative controls.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between primary cohorts and negative controls ***")
}

# ---------------------------------------------------------------------
# Create data frames representing study elements used in CohortMethod
# ---------------------------------------------------------------------
# Outcomes: build an outcomes list from the outcome cohort (id == 3).
# The template uses a 'cleanWindow' (washout) for cohort characterization;
# we keep that default 365 days here (does not affect the outcome generation
# other than cohort diagnostics).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator table for the CohortMethod analyses.
# Use exact names from Analysis Specifications for cohort names.
# We include placeholder columns targetConceptId / comparatorConceptId (NA)
# because the template references them when building excluded covariates.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  # No explicit concept ids for the drugs were provided in the spec;
  # include NA columns to preserve the template structure.
  targetConceptId = NA_integer_,
  comparatorConceptId = NA_integer_,
  stringsAsFactors = FALSE
)

# Covariate inclusion/exclusion from the Analysis Specifications:
# Both lists are effectively empty / unspecified (contained nulls). We
# represent excludedCovariateConcepts as an empty data.frame. This will
# be used when building excluded covariate id vectors for each target/comparator.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------
# CohortGeneratorModule ------------------------------------------------
# ---------------------------------------------------------------------
# Create shared resources and module specifications for cohort generation.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared cohort definitions resource
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Shared negative control cohort resource: we want the 'first' occurrence
# and to detect on descendants to get all relevant concepts included.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# CohortGenerator module specifications: generateStats = TRUE to collect
# cohort generation statistics (useful for diagnostics).
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ---------------------------------------------------------------------
# CohortDiagnosticsModule ---------------------------------------------
# ---------------------------------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Configure cohort diagnostics to run a broad set of diagnostics.
# These settings are not explicitly specified in the Analysis Specifications,
# but follow the Template defaults to produce useful diagnostics.
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

# ---------------------------------------------------------------------
# CohortMethodModule --------------------------------------------------
# ---------------------------------------------------------------------
# The Analysis Specifications define study-period(s), time-at-risks (TARs),
# propensity-score adjustment settings, PS fitting options, and outcome model
# fitting options. We explicitly map each element below.

# Study periods:
# Analysis Specifications provide a single study period:
#   studyStartDate = "20101019"
#   studyEndDate   = "20181231"
studyPeriods <- tibble::tibble(
  studyStartDate = c("20101019"),
  studyEndDate   = c("20181231")
)

# Time-at-risk (TAR) definitions (three TARs specified):
# 1) riskWindowStart = 1, startAnchor = "cohort start",
#    riskWindowEnd = 5, endAnchor = "cohort end", minDaysAtRisk = 1
# 2) riskWindowStart = 1, startAnchor = "cohort start",
#    riskWindowEnd = 0, endAnchor = "cohort end", minDaysAtRisk = 1
# 3) riskWindowStart = 1, startAnchor = "cohort start",
#    riskWindowEnd = 99999, endAnchor = "cohort start", minDaysAtRisk = 1
timeAtRisks <- tibble::tibble(
  label = c("TAR_1to5_cohortEnd", "TAR_1to0_cohortEnd", "TAR_1toInf_cohortStart"),
  riskWindowStart = c(1L, 1L, 1L),
  startAnchor = c("cohort start", "cohort start", "cohort start"),
  riskWindowEnd = c(5L, 0L, 99999L),
  endAnchor = c("cohort end", "cohort end", "cohort start"),
  minDaysAtRisk = c(1L, 1L, 1L)
)

# Propensity Score - "match" configurations specified in the Analysis Specifications:
# Two PS configurations (both matching):
#  - maxRatio = 1,   caliper = 0.2, caliperScale = "standardized logit"
#  - maxRatio = 100, caliper = 0.2, caliperScale = "standardized logit"
matchOnPsArgsList <- tibble::tibble(
  label = c("match_ratio_1", "match_ratio_100"),
  maxRatio = c(1L, 100L),
  caliper = c(0.2, 0.2),
  caliperScale = c("standardized logit", "standardized logit")
)

# No stratify-by-PS settings were provided in the Analysis Specifications,
# so we leave stratifyByPsArgsList empty.
stratifyByPsArgsList <- tibble::tibble(
  label = character(0),
  numberOfStrata = integer(0),
  baseSelection = character(0)
)

# Build a unified psConfigList from the two data.frames above. Each element
# describes whether we will "match" or "stratify" and carries the params.
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

# ---------------------------------------------------------------------
# Iterate through study periods x TARs x PS configurations to build
# CohortMethod analyses.
# ---------------------------------------------------------------------
cmAnalysisList <- list()
analysisId <- 1

# For each study period (only one in this specification)
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  # For each time-at-risk definition
  for (t in seq_len(nrow(timeAtRisks))) {

    # For each PS configuration (matching in this spec)
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Build matchOnPsArgs or stratifyByPsArgs depending on method.
      # The Analysis Specifications request matchOnPs with particular
      # caliper scales and max ratios.
      matchOnPsArgs <- NULL
      stratifyByPsArgs <- NULL
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

      # Covariate settings: use default covariate extraction but ensure
      # we add descendants to exclude (template default).
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build the list of outcomes for CohortMethod analyses:
      #  - the investigator-defined outcome(s) (oList)
      #  - the negative control outcomes (negativeControlOutcomeCohortSet)
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          # For the investigator-defined outcome(s) we set outcomeOfInterest = TRUE.
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          # Negative controls: outcomeOfInterest = FALSE, trueEffectSize = 1
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Build target-comparator-outcome mapping list.
      # Each entry also sets excluded covariate concept ids. We combine:
      #   - potential per-target / per-comparator concept ids (NA in this spec)
      #   - excludedCovariateConcepts$conceptId (empty in this spec)
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        # Combine the per-comparator / per-target concept ids with the
        # global excluded concept ids. The Analysis Specification provided
        # no explicit concept ids, so this will be empty/NA; we remove NA.
        excludedIds <- na.omit(c(
          cmTcList$targetConceptId[i],
          cmTcList$comparatorConceptId[i],
          excludedCovariateConcepts$conceptId
        ))

        # Ensure excludedIds is a simple integer vector or NULL if empty.
        if (length(excludedIds) == 0) {
          excludedIds <- NULL
        }

        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedIds
        )
      }

      # getDbCohortMethodDataArgs: These control cohort data extraction for CM.
      # The Analysis Specifications:
      #   - restrictToCommonPeriod = false
      #   - studyStartDate / studyEndDate set above
      #   - maxCohortSize = 0 (no truncation)
      # Note: other flags from the spec like firstExposureOnly / removeDuplicateSubjects
      # are primarily used when creating the study population (createStudyPopArgs),
      # so we keep the standard fields here and pass covariateSettings.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs: Use the PS-fitting options provided in Analysis Specifications:
      #   maxCohortSizeForFitting = 250000
      #   errorOnHighCorrelation = TRUE
      #   prior: laplace, useCrossValidation = TRUE
      #   control: tolerance = 2e-7, cvType = "auto", fold = 10,
      #            cvRepetitions = 10, noiseLevel = "silent",
      #            resetCoefficients = TRUE, startingVariance = 0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # allow Strategus to continue even if a single PS-fit fails
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",
          exclude = c(0),
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          tolerance = 2e-07,
          cvType = "auto",
          seed = 1,
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "silent",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Covariate balance computation arguments (shared and per-analysis)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs: Use fit settings from the Analysis Specifications:
      #   modelType = "cox", stratified = TRUE, useCovariates = FALSE,
      #   inversePtWeighting = FALSE
      #   prior: laplace, useCrossValidation = TRUE
      #   control: tolerance = 2e-7, cvType = "auto", fold = 10,
      #            cvRepetitions = 10, noiseLevel = "quiet",
      #            resetCoefficients = TRUE, startingVariance = 0.01
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
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          tolerance = 2e-07
        )
      )

      # createStudyPopArgs: Settings from Analysis Specifications:
      #   restrictToCommonPeriod = FALSE
      #   firstExposureOnly = FALSE
      #   washoutPeriod = 0
      #   removeDuplicateSubjects = "keep all"
      #   censorAtNewRiskWindow = FALSE
      #   removeSubjectsWithPriorOutcome = FALSE
      #   priorOutcomeLookBack = 99999
      #   risk window parameters derived from timeAtRisks row
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

      # Compose a human-readable description of the analysis to carry
      # through into outputs; this uses study period, TAR label and PS label.
      description <- sprintf(
        "Study: %s-%s; TAR: %s; PS: %s",
        studyStartDate,
        studyEndDate,
        timeAtRisks$label[t],
        psCfg$label
      )

      # Append the CM analysis definition to the list
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
    } # end psConfig loop
  } # end TAR loop
} # end study period loop

# Create the CohortMethod module specification with the cmAnalysisList and
# the targetComparatorOutcomesList defined above.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# ---------------------------------------------------------------------
# Final: Assemble the Strategus analysis specifications object
# ---------------------------------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Ensure the output directory exists before attempting to save the JSON.
outDir <- file.path("inst", "doacsandwarfarin")
if (!dir.exists(outDir)) {
  dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
}

# Save the analysis specifications JSON to the inst directory for the
# study name: doacsandwarfarin. This produces:
#   inst/doacsandwarfarin/doacsandwarfarinAnalysisSpecification.json
# which is the file Strategus expects to discover for running the study.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path(outDir, "doacsandwarfarinAnalysisSpecification.json")
)