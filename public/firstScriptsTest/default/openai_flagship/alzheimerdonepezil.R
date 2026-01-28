################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script creates a Strategus analysis specification JSON for the study:
#   name: alzheimerdonepezil
#
# It follows the provided template structure and applies the exact settings from
# <Analysis Specifications>.
#
# Notes on key conventions used here:
# - We export 3 ATLAS cohort definitions (target, comparator, outcome) and then
#   re-number them to 1, 2, 3 for internal use in Strategus/CohortMethod.
# - Negative controls are created from a concept set and assigned cohortIds
#   starting at 101 to avoid collisions with 1..3.
# - We build a CohortMethod analysis grid over:
#     * studyPeriods (1 row; empty dates mean "no restriction")
#     * timeAtRisks (1 row; 1-180 days from cohort start)
#     * PS adjustment settings (2 match configurations: ratio 1 and ratio 3)
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# WebAPI base URL used to export cohort definitions and resolve concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export the ATLAS cohort definitions for:
# - Target cohort:     id 1794126 (name: target1)
# - Comparator cohort: id 1794132 (name: comparator1)
# - Outcome cohort:    id 1794131 (name: outcome1)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to 1/2/3 for internal use in the analysis.
# This is a common pattern in Strategus templates to keep cohort IDs compact and
# consistent across modules.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# Negative controls are defined by a concept set:
# - conceptSetId: 1888110 (name: negative)
#
# We resolve the concept set to a list of concepts, then map each concept to a
# negative control outcome cohort definition (handled later by CohortGenerator).
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
  # with the main cohorts (1,2,3).
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no cohortId collisions between main cohorts and negative controls.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis --------
# Outcomes of interest list (oList):
# - We include the single outcome cohort (cohortId == 3) as the outcome of interest.
# - cleanWindow is included per template; not used directly in CM settings below.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator list for CohortMethod:
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate include/exclude concept lists --------------------------------------
# The provided specification includes placeholders (id: null, name: "") for both
# include and exclude lists. Therefore, we do not define includedCovariateConcepts
# or excludedCovariateConcepts beyond an empty data frame.
#
# IMPORTANT: We keep the object name from the template ("excludedCovariateConcepts")
# but set it to empty to reflect the specification.
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

# Shared resource: negative control outcome cohorts derived from concept set
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications: generate cohort stats as in template
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
# From specification:
# getDbCohortMethodDataArgs.studyPeriods has one entry with empty start/end dates.
# In this template, empty strings are represented by empty vectors, meaning:
# "do not restrict to a specific time window".
studyPeriods <- tibble::tibble(
  studyStartDate = c(""),
  studyEndDate   = c("")
)

# Time-at-risk (TAR) settings --------------------------------------------------
# From specification:
# - riskWindowStart: 1, startAnchor: "cohort start"
# - riskWindowEnd: 180, endAnchor: "cohort start"
# - minDaysAtRisk: 1
timeAtRisks <- tibble::tibble(
  label = c("TAR_1_180_from_cohort_start"),
  riskWindowStart = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(180),
  endAnchor = c("cohort start"),
  minDaysAtRisk = c(1)
)

# Propensity Score settings ----------------------------------------------------
# From specification: two PS match configurations, no stratification configs.
matchOnPsArgsList <- tibble::tibble(
  label = c("match_maxRatio_1_caliper_0.2_stdLogit", "match_maxRatio_3_caliper_0.2_stdLogit"),
  maxRatio = c(1, 3),
  caliper = c(0.2, 0.2),
  caliperScale = c("standardized logit", "standardized logit")
)

# Explicitly define stratifyByPsArgsList as empty (since stratifyByPsArgs is null in both psSettings)
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
      label  = matchOnPsArgsList$label[i],
      params = list(
        maxRatio     = matchOnPsArgsList$maxRatio[i],
        caliper      = matchOnPsArgsList$caliper[i],
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
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create PS adjustment args based on method (match vs stratify)
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

      # Covariate settings -----------------------------------------------------
      # The specification provides no explicit include/exclude concept lists
      # (placeholders only). Therefore we use default covariates.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcomes list ----------------------------------------------------------
      # - Outcome of interest: outcome1 (cohortId 3), with priorOutcomeLookback = 99999
      # - Negative controls: each concept-derived cohortId (101+), outcomeOfInterest = FALSE
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

      # Target-Comparator-Outcomes mapping ------------------------------------
      # The template includes excludedCovariateConceptIds; our specification does
      # not provide target/comparator concept IDs to exclude, so we only pass the
      # (empty) excludedCovariateConcepts$conceptId.
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
      # Apply exact settings from specification:
      # - restrictToCommonPeriod: TRUE
      # - maxCohortSize: 0
      # - firstExposureOnly: FALSE (note: this is a createStudyPop setting in CM;
      #   getDbCohortMethodDataArgs does not use it directly)
      # - washoutPeriod: 0 (createStudyPop setting)
      # - removeDuplicateSubjects: "keep all" (createStudyPop setting)
      #
      # Study start/end dates are empty strings here, meaning no restriction.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs -----------------------------------------------------------
      # Apply exact settings from specification:
      # - maxCohortSizeForFitting: 250000
      # - errorOnHighCorrelation: TRUE
      # - prior: laplace with useCrossValidation TRUE
      # - control: tolerance 2e-7, cvType auto, fold 10, cvRepetitions 10,
      #            noiseLevel silent, resetCoefficients TRUE, startingVariance 0.01
      #
      # Note: stopOnError is set to FALSE (as in template rationale) to allow
      # Strategus to continue even if a PS model fails in some strata.
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
          fold = 10,
          cvRepetitions = 10,
          seed = 1,
          resetCoefficients = TRUE,
          tolerance = 2e-07,
          startingVariance = 0.01
        )
      )

      # Covariate balance args (kept as template defaults) ----------------------
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs ----------------------------------------------------
      # Apply exact settings from specification:
      # - modelType: "logistic"
      # - stratified: TRUE
      # - useCovariates: FALSE
      # - inversePtWeighting: FALSE
      # - prior: laplace with useCrossValidation TRUE
      # - control: tolerance 2e-7, cvType auto, fold 10, cvRepetitions 10,
      #            noiseLevel quiet, resetCoefficients TRUE, startingVariance 0.01
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "logistic",
        stratified = TRUE,
        useCovariates = FALSE,
        inversePtWeighting = FALSE,
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          seed = 1,
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          tolerance = 2e-07,
          noiseLevel = "quiet"
        )
      )

      # createStudyPopArgs -----------------------------------------------------
      # Apply exact settings from specification:
      # - restrictToCommonPeriod: FALSE
      # - firstExposureOnly: FALSE
      # - washoutPeriod: 0
      # - removeDuplicateSubjects: "keep all"
      # - censorAtNewRiskWindow: FALSE
      # - removeSubjectsWithPriorOutcome: TRUE
      # - priorOutcomeLookBack: 99999
      # - TAR: 1 to 180 from cohort start, minDaysAtRisk: 1
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
        maxDaysAtRisk = 99999
      )

      # Append the settings to Analysis List -----------------------------------
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

# Save to JSON -----------------------------------------------------------------
# Per template, write to inst/<studyName>/<studyName>AnalysisSpecification.json
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "alzheimerdonepezil", "alzheimerdonepezilAnalysisSpecification.json")
)