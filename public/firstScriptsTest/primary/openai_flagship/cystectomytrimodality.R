################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script creates a Strategus analysis specification JSON for the study:
#   name: cystectomytrimodality
#
# It follows the provided template and applies the settings exactly as provided
# in <Analysis Specifications>. Detailed annotations are included to explain how
# each setting is mapped into Strategus / CohortMethod arguments.
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# WebAPI endpoint used to export cohort definitions and concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export the ATLAS cohort definitions for:
#   - Target cohort:     id 1794126 (name: target1)
#   - Comparator cohort: id 1794132 (name: comparator1)
#   - Outcome cohort:    id 1794131 (name: outcome1)
#
# Note: We re-number these cohorts to 1, 2, 3 to align with the template's
# expectation that target/comparator/outcome are small integers, and to avoid
# collisions with negative control cohort IDs (which we will assign starting at 101).
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts (EXACT mapping):
#   1794126 -> 1 (target)
#   1794132 -> 2 (comparator)
#   1794131 -> 3 (outcome)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# The negative control concept set is:
#   id 1888110 (name: negative)
#
# We resolve the concept set to a list of concepts, then convert each concept
# into a "negative control outcome cohort" entry. Strategus/CohortMethod will
# later treat these as negative control outcomes (outcomeOfInterest = FALSE).
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
  mutate(
    # Assign cohort IDs for negative controls starting at 101 to avoid collisions
    # with the main cohorts (1,2,3).
    cohortId = row_number() + 100
  ) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no cohortId collisions between main cohorts and negative controls.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis --------
# Outcomes of interest list:
# - We include the single outcome cohort (cohortId == 3) as the primary outcome.
# - cleanWindow is included per template; not used directly by CohortMethod here,
#   but kept for compatibility with common Strategus patterns.
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

# Covariate include/exclude concept lists --------------------------------------
# <Analysis Specifications> provides:
#   covariateSelection.conceptsToInclude: [{id: null, name: ""}]
#   covariateSelection.conceptsToExclude: [{id: null, name: ""}]
#
# Interpretation:
# - No explicit include/exclude concept IDs were provided (null), so we do not
#   add any custom include/exclude concept constraints here.
# - We keep placeholders as empty data frames for clarity and future extension.
includedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# CohortGeneratorModule --------------------------------------------------------
# Shared resources:
# - cohortDefinitionShared: the cohort definitions to be instantiated in the CDM
# - negativeControlsShared: negative control outcome concept set to be converted
#   into outcome cohorts by the CohortGenerator module
cgModuleSettingsCreator <- CohortGeneratorModule$new()

cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# The template includes a comprehensive diagnostics configuration. Since no
# diagnostics settings were specified in <Analysis Specifications>, we keep the
# template defaults.
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
#   getDbCohortMethodDataArgs.studyPeriods = [{studyStartDate: "20050101", studyEndDate: "20171231"}]
#
# We create a tibble with one row, so the analysis loop will generate analyses
# for this single study period.
studyPeriods <- tibble(
  studyStartDate = c("20050101"), # YYYYMMDD
  studyEndDate   = c("20171231")  # YYYYMMDD
)

# Time-at-risk (TAR) ----------------------------------------------------------
# <Analysis Specifications>:
#   createStudyPopArgs.timeAtRisks = [{
#     riskWindowStart: 1,
#     startAnchor: "cohort start",
#     riskWindowEnd: 99999,
#     endAnchor: "cohort start",
#     minDaysAtRisk: 1
#   }]
#
# We create a single TAR row. The label is user-defined here for readability in
# the analysis description; it does not change the actual TAR parameters.
timeAtRisks <- tibble(
  label = c("TAR_1_to_99999_from_cohort_start"),
  riskWindowStart = c(1),
  startAnchor = c("cohort start"),  # "cohort start" | "cohort end"
  riskWindowEnd = c(99999),
  endAnchor = c("cohort start")     # "cohort start" | "cohort end"
)

# Propensity Score settings ----------------------------------------------------
# <Analysis Specifications>:
#   propensityScoreAdjustment.psSettings = [{
#     matchOnPsArgs: {maxRatio: 3, caliper: 0.2, caliperScale: "standardized logit"},
#     stratifyByPsArgs: null
#   }]
#
# Therefore, we create a single "match" PS config.
matchOnPsArgsList <- tibble(
  label = c("match_maxRatio3_caliper0.2_stdLogit"),
  maxRatio = c(3),
  caliper = c(0.2),
  caliperScale = c("standardized logit") # "propensity score" | "standardized" | "standardized logit"
)

# No stratification configs provided (null), so keep an empty tibble.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata = c(),
  baseSelection = c()
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
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create PS adjustment args based on the selected PS config --------------
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
      # <Analysis Specifications> does not provide explicit FeatureExtraction
      # settings. We use default covariates, and set addDescendantsToExclude=TRUE
      # as in the template.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcomes list ----------------------------------------------------------
      # Primary outcome(s) + negative controls.
      #
      # <Analysis Specifications>:
      #   createStudyPopArgs.removeSubjectsWithPriorOutcome = TRUE
      #   createStudyPopArgs.priorOutcomeLookBack = 99999
      #
      # We apply priorOutcomeLookback to outcomes of interest. Negative controls
      # use default behavior (trueEffectSize=1, outcomeOfInterest=FALSE).
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
      # Excluded covariate concept IDs:
      # - The template excludes target/comparator concepts plus any additional
      #   excludedCovariateConcepts.
      # - In this study, no excluded covariate concepts were provided, so only
      #   excludedCovariateConcepts$conceptId (empty) is used.
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

      # getDbCohortMethodDataArgs ---------------------------------------------
      # <Analysis Specifications>:
      #   getDbCohortMethodDataArgs.maxCohortSize = 0
      #   createStudyPopArgs.restrictToCommonPeriod = TRUE
      #
      # Note: In CohortMethod, restrictToCommonPeriod is part of
      # createGetDbCohortMethodDataArgs (as in the template). We set it to TRUE
      # to match the study population restriction intent.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs -----------------------------------------------------------
      # <Analysis Specifications>:
      #   maxCohortSizeForFitting = 250000
      #   errorOnHighCorrelation = TRUE
      #   prior: laplace, useCrossValidation=TRUE
      #   control: tolerance=2e-7, cvType=auto, fold=10, cvRepetitions=10,
      #            noiseLevel=silent, resetCoefficients=TRUE, startingVariance=0.01
      #
      # Note: CohortMethod::createCreatePsArgs uses Cyclops::createControl.
      # The template includes some fields (seed, cvRepetitions) but not fold.
      # Cyclops supports 'fold' in createControl; we pass it through.
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
      # <Analysis Specifications>:
      #   modelType = "cox"
      #   stratified = TRUE
      #   useCovariates = TRUE
      #   inversePtWeighting = FALSE
      #   prior: laplace, useCrossValidation=TRUE
      #   control: tolerance=2e-7, cvType=auto, fold=10, cvRepetitions=10,
      #            noiseLevel=quiet, resetCoefficients=TRUE, startingVariance=0.01
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = TRUE,
        useCovariates = TRUE,
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
      # <Analysis Specifications>:
      #   restrictToCommonPeriod = true
      #   firstExposureOnly = false
      #   washoutPeriod = 0
      #   removeDuplicateSubjects = "keep all"
      #   censorAtNewRiskWindow = false
      #   removeSubjectsWithPriorOutcome = true
      #   priorOutcomeLookBack = 99999
      #   timeAtRisks: start=1 from cohort start; end=99999 from cohort start; minDaysAtRisk=1
      #
      # Note: CohortMethod::createCreateStudyPopulationArgs uses:
      #   removeDuplicateSubjects = "keep all" is a valid option in CohortMethod.
      # Also note: restrictToCommonPeriod is typically handled in getDb args; we
      # set it here to TRUE to reflect the specification, but CohortMethod may
      # ignore it depending on version. The key restriction is already applied
      # in getDbCohortMethodDataArgs above.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE,
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
# The template uses "inst/studyName/studyNameAnalysisSpecification.json".
# Here we use the study name "cystectomytrimodality" exactly as specified.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "cystectomytrimodality", "cystectomytrimodalityAnalysisSpecification.json")
)