################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script creates a Strategus analysis specification JSON for the study:
#   name: sglt2imetformin
#
# It follows the provided Template structure and applies the settings from the
# provided Analysis Specifications exactly (no name auto-correct).
#
# Notes:
# - Cohort IDs are exported from ATLAS/WebAPI, then re-numbered to 1/2/3 for
#   target/comparator/outcome to align with Strategus/HADES module conventions.
# - Negative controls are created from a Concept Set and assigned cohortIds
#   starting at 101 to avoid collisions with 1/2/3.
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# WebAPI endpoint used to export cohort definitions and resolve concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export the cohort definitions for:
# - Target cohort:     id 1794126 (name: target1)
# - Comparator cohort: id 1794132 (name: comparator1)
# - Outcome cohort:    id 1794131 (name: outcome1)
#
# generateStats = TRUE to include cohort generation stats metadata.
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
# Strategus modules often assume small sequential cohort IDs for the main study
# cohorts. We map:
#   1794126 -> 1 (target)
#   1794132 -> 2 (comparator)
#   1794131 -> 3 (outcome)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# Negative controls are defined by a Concept Set:
#   conceptSetId = 1888110 (name: negative)
#
# We resolve the concept set to a list of concepts, then create a table with:
# - outcomeConceptId: the conceptId
# - cohortName:       the conceptName
# - cohortId:         sequential IDs starting at 101 (to avoid 1/2/3 collisions)
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
    cohortId = row_number() + 100
  ) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no cohortId collisions between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis --------
# Outcomes of interest list (oList)
# - We include the single outcome cohort (cohortId == 3).
# - cleanWindow is included per template; not used directly by CohortMethod here.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator list for CohortMethod
# Names are set to the exact cohort names from the Analysis Specifications.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate include/exclude concept lists --------------------------------------
# The Analysis Specifications provide "conceptsToInclude" and "conceptsToExclude"
# but both contain null/empty entries. We keep placeholders consistent with the
# template, but do not apply any include/exclude filtering.
#
# For LSPS-style exclusion of drugs of interest, the template expects a data
# frame of excluded covariate concepts. Since no concept IDs were provided, this
# is an empty data frame.
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

# Shared resource: negative control outcome cohorts
# occurrenceType = "first" and detectOnDescendants = TRUE follow the template.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications: generateStats = TRUE to compute cohort generation stats
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Run diagnostics for all cohorts (including target/comparator/outcome and any
# additional cohorts generated by negative controls are handled separately by
# CohortGenerator shared resources).
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
# Apply getDbCohortMethodDataArgs.studyPeriods:
#   studyStartDate = "20130401"
#   studyEndDate   = "20200331"
studyPeriods <- tibble::tibble(
  studyStartDate = c("20130401"),
  studyEndDate   = c("20200331")
)

# Time-at-risk (TAR) settings -------------------------------------------------
# Apply createStudyPopArgs.timeAtRisks (single TAR):
#   riskWindowStart = 1, startAnchor = "cohort start"
#   riskWindowEnd   = 0, endAnchor   = "cohort end"
#   minDaysAtRisk   = 1
#
# The template expects a TAR table with a label column; we create a descriptive label.
timeAtRisks <- tibble::tibble(
  label = c("TAR: start+1 to cohort end"),
  riskWindowStart = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(0),
  endAnchor = c("cohort end")
)

# Propensity Score settings ----------------------------------------------------
# Apply propensityScoreAdjustment.psSettings:
# - matchOnPsArgs:
#     maxRatio = 2
#     caliper = 0.2
#     caliperScale = "standardized logit"
# - stratifyByPsArgs: null (so no stratification config)
matchOnPsArgsList <- tibble::tibble(
  label = c("PS match (maxRatio=2, caliper=0.2, scale=standardized logit)"),
  maxRatio = c(2),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# No stratification settings provided (null), keep an empty table for template compatibility
stratifyByPsArgsList <- tibble::tibble(
  label = character(),
  numberOfStrata = numeric(),
  baseSelection = character()
)

# Build a single PS configuration list (each entry has: method, label, params)
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

# Iterate through all analysis setting combinations ----------------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # PS adjustment method selection ----------------------------------------
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

      # Covariate settings ----------------------------------------------------
      # The Analysis Specifications do not define custom include/exclude concept
      # lists (they are null/empty). Therefore, we use default covariates.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcomes list ---------------------------------------------------------
      # - Outcome of interest: cohortId 3, with priorOutcomeLookback = 99999
      #   (matches createStudyPopArgs.priorOutcomeLookBack)
      # - Negative controls: each negative control cohortId (101+), marked as
      #   outcomeOfInterest = FALSE and trueEffectSize = 1 (template convention)
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
      # - The template attempts to exclude target/comparator concepts plus any
      #   additional excluded covariate concepts. Since no concept IDs were
      #   provided in Analysis Specifications, we only pass the (empty) excluded
      #   list here.
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
      # Apply:
      # - restrictToCommonPeriod = TRUE (from createStudyPopArgs.restrictToCommonPeriod)
      # - studyStartDate/studyEndDate from getDbCohortMethodDataArgs.studyPeriods
      # - maxCohortSize = 0
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs ----------------------------------------------------------
      # Apply propensityScoreAdjustment.createPsArgs:
      # - maxCohortSizeForFitting = 250000
      # - errorOnHighCorrelation = TRUE
      # - prior: laplace, useCrossValidation = TRUE
      # - control: tolerance=2e-7, cvType=auto, fold=10, cvRepetitions=10,
      #            noiseLevel=silent, resetCoefficients=TRUE, startingVariance=0.01
      #
      # Note: CohortMethod/Cyclops control uses 'fold' and 'cvRepetitions' in
      # some versions; we pass them as provided.
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # keep FALSE to allow Strategus to continue if a PS model fails
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
      # Apply fitOutcomeModelArgs:
      # - modelType = "cox"
      # - stratified = TRUE
      # - useCovariates = FALSE
      # - inversePtWeighting = FALSE
      # - prior: laplace, useCrossValidation = TRUE
      # - control: tolerance=2e-7, cvType=auto, fold=10, cvRepetitions=10,
      #            noiseLevel=quiet, resetCoefficients=TRUE, startingVariance=0.01
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

      # createStudyPopArgs ----------------------------------------------------
      # Apply createStudyPopArgs exactly:
      # - restrictToCommonPeriod = TRUE
      # - firstExposureOnly = FALSE
      # - washoutPeriod = 0
      # - removeDuplicateSubjects = "keep all"
      # - censorAtNewRiskWindow = FALSE
      # - removeSubjectsWithPriorOutcome = TRUE
      # - priorOutcomeLookBack = 99999
      # - TAR: start+1 (cohort start) to end+0 (cohort end)
      # - minDaysAtRisk = 1
      #
      # Note: CohortMethod::createCreateStudyPopulationArgs uses
      # priorOutcomeLookback (lowercase 'b') and removeDuplicateSubjects values
      # like "keep first"/"keep all" depending on version; we pass the exact
      # string from the Analysis Specifications: "keep all".
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
# The template uses inst/studyName/studyNameAnalysisSpecification.json.
# Here we use the study name "sglt2imetformin" exactly.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "sglt2imetformin", "sglt2imetforminAnalysisSpecification.json")
)