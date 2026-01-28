################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script creates a Strategus analysis specification JSON using the OHDSI
# Strategus package, following the provided Template and applying the exact
# settings from <Analysis Specifications>.
#
# Study name (used in output file path): cystectomytrimodality
#
# Notes on "EXACT names":
# - Cohort names, IDs, and concept set IDs are used exactly as provided.
# - The target/comparator/outcome cohort IDs from ATLAS are re-numbered to 1/2/3
#   for internal Strategus/CohortMethod usage, matching the Template pattern.
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# WebAPI base URL used to export cohort definitions and resolve concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export the ATLAS cohort definitions for:
# - Target cohort:     1794126 (name: target1)
# - Comparator cohort: 1794132 (name: comparator1)
# - Outcome cohort:    1794131 (name: outcome1)
#
# generateStats = TRUE to include cohort generation stats in the exported set.
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
# Strategus/CohortMethod analyses typically assume small integer cohort IDs.
# We map:
# - target1     -> cohortId 1
# - comparator1 -> cohortId 2
# - outcome1    -> cohortId 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# The negative control concept set is resolved from ATLAS concept set id 1888110
# (name: negative). Each concept becomes a negative control outcome.
#
# We assign cohortIds starting at 101 to avoid collisions with 1/2/3.
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
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no duplicate cohort IDs between main cohorts and NCOs.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis --------
# Outcomes of interest list (oList):
# - Only the primary outcome cohort (cohortId == 3) is included here.
# - cleanWindow is included to match the Template structure (not used directly
#   by CohortMethod::createOutcome in this script).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator list for CohortMethod:
# Names are set exactly as provided in <Analysis Specifications>.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate include/exclude concept lists --------------------------------------
# <Analysis Specifications> provides conceptsToInclude and conceptsToExclude with
# null IDs and empty names. We keep these objects but leave them empty so they
# do not affect covariate construction.
includedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource: cohort definitions (target/comparator/outcome)
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Shared resource: negative control outcomes (as concept-based outcomes)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications: generateStats = TRUE to compute cohort generation stats.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# The Template includes a CohortDiagnostics module. Since <Analysis Specifications>
# does not override these settings, we keep the Template defaults.
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
# <Analysis Specifications> provides one study period:
# - studyStartDate: 20050101
# - studyEndDate:   20171231
studyPeriods <- tibble::tibble(
  studyStartDate = c("20050101"),
  studyEndDate   = c("20171231")
)

# Time-at-risk (TAR) ----------------------------------------------------------
# <Analysis Specifications> provides one TAR:
# - riskWindowStart: 1, startAnchor: "cohort start"
# - riskWindowEnd: 99999, endAnchor: "cohort start"
# - minDaysAtRisk: 1
#
# We include a label for readability in analysis descriptions.
timeAtRisks <- tibble::tibble(
  label = c("TAR_1_to_99999_from_cohort_start"),
  riskWindowStart = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(99999),
  endAnchor = c("cohort start"),
  minDaysAtRisk = c(1)
)

# Propensity Score settings ----------------------------------------------------
# <Analysis Specifications> provides one PS adjustment setting:
# - matchOnPsArgs: maxRatio=3, caliper=0.2, caliperScale="standardized logit"
# - stratifyByPsArgs: null
matchOnPsArgsList <- tibble::tibble(
  label = c("match_maxRatio3_caliper0.2_stdLogit"),
  maxRatio = c(3),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# No stratification configurations provided.
stratifyByPsArgsList <- tibble::tibble(
  label = character(0),
  numberOfStrata = integer(0),
  baseSelection = character(0)
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

      # Create PS adjustment args based on configuration type
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
      # <Analysis Specifications> does not specify custom FeatureExtraction
      # settings beyond include/exclude concept lists (which are empty here).
      # We use default covariates and set addDescendantsToExclude = TRUE as in
      # the Template.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcomes list ----------------------------------------------------------
      # - Primary outcome: outcome1 (cohortId 3) marked as outcomeOfInterest=TRUE
      # - Negative controls: each concept-based outcome marked as outcomeOfInterest=FALSE
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
      # excludedCovariateConceptIds:
      # - In the Template, this includes target/comparator concept IDs plus an
      #   excludedCovariateConcepts list.
      # - <Analysis Specifications> provides no valid concept IDs to include/exclude,
      #   so we only pass excludedCovariateConcepts$conceptId (empty).
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = c(excludedCovariateConcepts$conceptId)
        )
      }

      # getDbCohortMethodDataArgs ---------------------------------------------
      # Apply <Analysis Specifications>:
      # - studyStartDate / studyEndDate from studyPeriods
      # - maxCohortSize = 0
      #
      # Note: restrictToCommonPeriod is specified under createStudyPopArgs in
      # <Analysis Specifications>, but in CohortMethod it is applied here as well.
      # We set it to TRUE to match the intent and the Template usage.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs -----------------------------------------------------------
      # Apply <Analysis Specifications>:
      # - maxCohortSizeForFitting = 250000
      # - errorOnHighCorrelation = TRUE
      # - prior: laplace, useCrossValidation = TRUE
      # - control: tolerance=2e-7, cvType=auto, fold=10, cvRepetitions=10,
      #            noiseLevel=silent, resetCoefficients=TRUE, startingVariance=0.01
      #
      # Additional parameters retained from Template for operational stability:
      # - stopOnError = FALSE (allows pipeline to continue if a model fails)
      # - estimator = "att" (Template default)
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
      # Apply <Analysis Specifications>:
      # - modelType = "cox"
      # - stratified = TRUE
      # - useCovariates = TRUE
      # - inversePtWeighting = FALSE
      # - prior: laplace, useCrossValidation = TRUE
      # - control: tolerance=2e-7, cvType=auto, fold=10, cvRepetitions=10,
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
      # Apply <Analysis Specifications>:
      # - restrictToCommonPeriod = TRUE
      # - firstExposureOnly = FALSE
      # - washoutPeriod = 0
      # - removeDuplicateSubjects = "keep all"
      # - censorAtNewRiskWindow = FALSE
      # - removeSubjectsWithPriorOutcome = TRUE
      # - priorOutcomeLookBack = 99999
      # - TAR: start=1 from cohort start; end=99999 from cohort start; minDaysAtRisk=1
      #
      # Note: CohortMethod::createCreateStudyPopulationArgs uses:
      # - removeDuplicateSubjects values like "keep first"/"keep all" depending on version.
      # We pass the exact string from <Analysis Specifications>: "keep all".
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
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
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
# Output path follows the Template pattern: inst/<studyName>/<studyName>AnalysisSpecification.json
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "cystectomytrimodality", "cystectomytrimodalityAnalysisSpecification.json")
)