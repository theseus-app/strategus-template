################################################################################
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# further details.
# ##############################################################################
library(dplyr)
library(Strategus)
library(ROhdsiWebApi)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions from the original script
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to a standard 1, 2, 3... convention
# This mapping is based on the order in the cohortIds vector above
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes
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
  mutate(cohortId = row_number() + 100) %>% # ids start at 101 to avoid collision
  select(cohortId, cohortName, outcomeConceptId)


if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes:
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  select(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# NOTE: The targetConceptId and comparatorConceptId columns are required by the template's
# logic for excluding drug concepts from covariates. Please fill these with the
# appropriate concept IDs for your target and comparator drugs. They are set to NA here as placeholders.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  targetConceptId = NA_real_,
  comparatorConceptId = NA_real_
)

# Concepts to exclude from covariates, in addition to the T and C concepts themselves.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# CohortGeneratorModule --------------------------------------------------------
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

# CohortDiagnosticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  # NOTE: Running diagnostics on all generated cohorts (T, C, O, and Negative Controls)
  cohortIds = c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId),
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

# Define study periods. Empty strings mean no date restrictions.
studyPeriods <- tibble(
  studyStartDate = c(""), #YYYYMMDD
  studyEndDate   = c("") #YYYYMMDD
)

# Time-at-risks (TARs) for the outcomes of interest
timeAtRisks <- tibble(
  label = c("1-99999d from start", "365-99999d from start", "1d from start to end", "365d from start to end"),
  riskWindowStart  = c(1, 365, 1, 365),
  startAnchor = c("cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd  = c(99999, 99999, 0, 0),
  endAnchor = c("cohort start", "cohort start", "cohort end", "cohort end"),
  minDaysAtRisk = c(1, 1, 1, 1)
)

# Propensity Score settings - match on PS
matchOnPsArgsList <- tibble(
  label = c("1-to-1 Matching", "Variable Ratio Matching"),
  maxRatio  = c(1, 10),
  caliper = c(0.2, 0.2),
  caliperScale  = c("standardized logit", "standardized logit")
)

# Propensity Score settings - stratify by PS
stratifyByPsArgsList <- tibble(
  label = c("10 Strata"),
  numberOfStrata  = c(10),
  baseSelection = c("all")
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()
# Add matching configurations
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
# Add stratification configurations
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
# Add configuration for crude (unadjusted) analysis
psConfigList[[length(psConfigList) + 1]] <- list(
  method = "none",
  label  = "No PS Adjustment",
  params = list()
)

# NOTE: Moved TCO list creation outside of the main loop for efficiency
# Create the list of outcomes, including both outcomes of interest and negative controls.
outcomeList <- append(
  lapply(seq_len(nrow(oList)), function(i) {
    CohortMethod::createOutcome(
      outcomeId = oList$outcomeCohortId[i],
      outcomeOfInterest = TRUE,
      trueEffectSize = NA
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

targetComparatorOutcomesList <- list()
for (i in seq_len(nrow(cmTcList))) {
  # Combine T, C, and other excluded concepts, removing any NAs that may be placeholders
  conceptsToExclude <- unique(na.omit(c(
    cmTcList$targetConceptId[i],
    cmTcList$comparatorConceptId[i],
    excludedCovariateConcepts$conceptId
  )))

  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    excludedCovariateConceptIds = conceptsToExclude
  )
}

# Define constant settings objects outside the loop
createPsArgs <- CohortMethod::createCreatePsArgs(
  maxCohortSizeForFitting = 250000,
  errorOnHighCorrelation = TRUE,
  stopOnError = FALSE,
  estimator = "att",
  prior = Cyclops::createPrior(
    priorType = "laplace",
    useCrossValidation = TRUE
  ),
  control = Cyclops::createControl(
    noiseLevel = "silent",
    cvType = "auto",
    seed = 1,
    resetCoefficients = TRUE,
    tolerance = 2e-07,
    cvRepetitions = 1,
    startingVariance = 0.01
  )
)

computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
  maxCohortSize = 250000,
  covariateFilter = NULL
)
computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
  maxCohortSize = 250000,
  covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
)

covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE
)


# Iterate through all analysis setting combinations
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  # NOTE: Moved argument changes from getDbCohortMethodDataArgs to createCreateStudyPopulationArgs
  # to align with updated CohortMethod functions.
  getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
    studyStartDate = studyStartDate,
    studyEndDate = studyEndDate,
    maxCohortSize = 0,
    covariateSettings = covariateSettings
  )

  for (t in seq_len(nrow(timeAtRisks))) {
    createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
      restrictToCommonPeriod = FALSE,
      firstExposureOnly = FALSE,
      washoutPeriod = 365,
      removeDuplicateSubjects = "keep first",
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

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

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
      } else { # "none"
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- NULL
      }

      # FIX: The original error was caused by setting stratified = TRUE for the "none"
      # PS method. A stratified model requires matching or stratification.
      # This logic ensures stratified is FALSE for the unadjusted (crude) analysis.
      isStratified <- psCfg$method != "none"
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = isStratified,
        useCovariates = FALSE,
        inversePtWeighting = FALSE,
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          cvType = "auto",
          seed = 1,
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          tolerance = 2e-07,
          cvRepetitions = 1,
          noiseLevel = "quiet"
        )
      )

      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(studyStartDate == "", "unbounded", studyStartDate),
          ifelse(studyEndDate == "", "unbounded", studyEndDate),
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

# Save the analysis specifications file
if (!dir.exists(file.path("inst", "ranitidinecancer"))) {
  dir.create(file.path("inst", "ranitidinecancer"), recursive = TRUE)
}
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ranitidinecancer", "ranitidinecancerAnalysisSpecification.json")
)