################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script builds a Strategus analysis specification using the analysis
# settings provided in <Analysis Specifications>. It follows the structure of
# the provided <Template>, with detailed annotations to explain how the settings
# are mapped and applied.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# The ATLAS/WebAPI source for cohorts and concept sets
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# - We export the target, comparator, and outcome cohorts by their ATLAS IDs.
# - After export, we renumber the cohorts to 1, 2, and 3 to match the template
#   and to provide stable IDs across data sources.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to local (stable) IDs (1=T, 2=C, 3=O)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes
# - Resolve the "negative" concept set and generate a set of outcome cohorts,
#   each assigned a unique cohortId starting at 101 (to avoid overlap with 1,2,3).
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # negative control concept set ID
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
  ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  mutate(cohortId = dplyr::row_number() + 100) %>% # 101, 102, ...
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no ID overlap between local cohorts (1–3) and negative controls (>=101)
allCohortIds <- c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)
if (any(duplicated(allCohortIds))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create simple helper tables for downstream modules ---------------------------

# Outcomes (primary outcome[s] of interest). We carry forward a cleanWindow example (not used by CM directly).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for CohortMethod
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate selection notes:
# The <Analysis Specifications> provided empty include/exclude lists:
#   "conceptsToInclude": [{ "id": null, "name": "" }]
#   "conceptsToExclude": [{ "id": null, "name": "" }]
# Hence, we include the default covariates (no manual include/exclude concepts).
# If you later wish to customize, define data.frames with conceptId and conceptName.

# CohortGeneratorModule --------------------------------------------------------
# This module creates and optionally generates the cohorts in the CDM.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource: cohort definitions for T, C, and O
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Shared resource: Negative control outcome definitions built from the concept set
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# CohortGenerator module configuration (generateStats=TRUE like template)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule ------------------------------------------------------
# Default set of diagnostics as in the template (with a broad set of diagnostics).
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
# We now map the <Analysis Specifications> into CM study-periods, TARs, PS settings, and model args.

# Study periods (from getDbCohortMethodDataArgs.studyPeriods)
# - Single overall window: 19920101 to 20211231
studyPeriods <- tibble::tibble(
  studyStartDate = c("19920101"),
  studyEndDate   = c("20211231")
)

# Time-at-risk (from createStudyPopArgs.timeAtRisks)
# - Single TAR: start=cohort start +1 day, end=cohort end, minDaysAtRisk=1
#   We carry minDaysAtRisk separately since createStudyPopulationArgs uses it directly.
timeAtRisks <- tibble::tibble(
  label = c("TAR_cohortStart+1_to_cohortEnd"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
)
tarMinDaysAtRisk <- 1

# Propensity Score settings (from propensityScoreAdjustment.psSettings)
# - We define two configurations: one for matching on PS, one for stratifying by PS.
matchOnPsArgsList <- tibble::tibble(
  label = c("match"),
  maxRatio  = c(100),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # "propensity score" | "standardized" | "standardized logit"
)

stratifyByPsArgsList <- tibble::tibble(
  label = c("stratify"),
  numberOfStrata  = c(5),
  baseSelection = c("all") # "all" | "target" | "comparator"
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Convert the matching configs to a unified list
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

# Convert the stratification configs to a unified list
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

# Iterate through all setting combinations to build CohortMethod analyses -------
cmAnalysisList <- list()
analysisId <- 1

# Prepare negative control + primary outcome list once
outcomeList <- append(
  # Primary outcomes (from oList; priorOutcomeLookback per <Analysis Specifications>)
  lapply(seq_len(nrow(oList)), function(i) {
    CohortMethod::createOutcome(
      outcomeId = oList$outcomeCohortId[i],
      outcomeOfInterest = TRUE,
      trueEffectSize = NA,
      priorOutcomeLookback = 99999
    )
  }),
  # Negative control outcomes: effect size=1, not outcomes of interest
  lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
    CohortMethod::createOutcome(
      outcomeId = i,
      outcomeOfInterest = FALSE,
      trueEffectSize = 1
    )
  })
)

# Build Target-Comparator-Outcome(s) set (no covariate exclusions per specs)
targetComparatorOutcomesList <- list()
for (i in seq_len(nrow(cmTcList))) {
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    excludedCovariateConceptIds = integer(0) # No include/exclude concepts specified in specs
  )
}

# Covariate settings (default FE covariates; no manual include/exclude lists specified)
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE
)

# Traverse the Cartesian product: StudyPeriod x TAR x PS-config
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create PS adjustment arguments per configuration
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

      # Database extraction args (from getDbCohortMethodDataArgs in specs)
      # - restrictToCommonPeriod: FALSE
      # - studyStartDate/studyEndDate as above
      # - maxCohortSize: 0 (no cap)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Create PS args (from propensityScoreAdjustment.createPsArgs)
      # - Prior: laplace, useCrossValidation=TRUE
      # - Control: settings per specs (tolerance, cvType, fold, cvRepetitions, noiseLevel, resetCoefficients, startingVariance)
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # As in template: allow workflow to complete even if PS fails for some strata
        prior = Cyclops::createPrior(
          priorType = "laplace",
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

      # Covariate balance settings (default)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model args (from fitOutcomeModelArgs in specs)
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
          tolerance = 2e-7,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Study population args (from createStudyPopArgs in specs)
      # - firstExposureOnly: FALSE, washoutPeriod: 0, removeDuplicateSubjects: "keep all"
      # - censorAtNewRiskWindow: FALSE, removeSubjectsWithPriorOutcome: TRUE, priorOutcomeLookback: 99999
      # - TAR settings from timeAtRisks table; minDaysAtRisk from specs (1)
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
        minDaysAtRisk = tarMinDaysAtRisk,
        maxDaysAtRisk = 99999
      )

      # Append the analysis definition
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

# Build the CohortMethod module specification
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
# We assemble all shared resources and modules into one Strategus analysis spec.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Persist the analysis specification to JSON
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "legendt2dm", "legendt2dmAnalysisSpecification.json")
)