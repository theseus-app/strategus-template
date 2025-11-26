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

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Name of the analysis (used for output file path)
studyName <- "ranitidinecancer"

# ---------------------------------------------------------------------------
# Cohort Definitions
# - Export target, comparator, and primary outcome cohorts from ATLAS/WebAPI
#   using the EXACT cohort IDs from the analysis specifications.
# ---------------------------------------------------------------------------
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to local IDs 1, 2, 3 for Target, Comparator, Outcome respectively.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# ---------------------------------------------------------------------------
# Negative control outcomes
# - Pull concepts from the EXACT concept set ID provided.
# - Each negative control concept is assigned a unique cohortId starting at 101.
# ---------------------------------------------------------------------------
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # negative
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
  ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
  dplyr::rename(outcomeConceptId = "conceptId",
                cohortName = "conceptName") %>%
  dplyr::mutate(cohortId = dplyr::row_number() + 100) %>% # TC cohorts are small IDs; NCs start at 101
  dplyr::select(cohortId, cohortName, outcomeConceptId)

# Safety check to ensure no overlap in cohort IDs
if (any(cohortDefinitionSet$cohortId %in% negativeControlOutcomeCohortSet$cohortId)) {
  stop("*** Error: duplicate cohort IDs found between primary cohorts and negative control cohorts ***")
}

# ---------------------------------------------------------------------------
# Create data frames for the analyses
# ---------------------------------------------------------------------------

# Outcomes (primary outcome only here; NCs handled below)
oList <- cohortDefinitionSet %>%
  dplyr::filter(.data$cohortId == 3) %>%
  dplyr::mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  dplyr::select(outcomeCohortId, outcomeCohortName) %>%
  dplyr::mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  # The template references targetConceptId and comparatorConceptId downstream when excluding covariates.
  # The analysis specifications did not provide these, so we include them as NA and handle NA safely later.
  targetConceptId = NA_integer_,
  comparatorConceptId = NA_integer_
)

# Covariate selection (EXACTLY as specified: both include and exclude are empty)
includedCovariateConcepts <- data.frame(
  conceptId = integer(), # none to include
  conceptName = character()
)

excludedCovariateConcepts <- data.frame(
  conceptId = integer(), # none to exclude
  conceptName = character()
)

# ---------------------------------------------------------------------------
# CohortGeneratorModule
# ---------------------------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource: cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Shared resource: negative control outcomes (resolved to concepts)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# CohortGenerator module run-time settings
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ---------------------------------------------------------------------------
# CohortDiagnosticsModule Settings
# ---------------------------------------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # 1 (T), 2 (C), 3 (O)
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

# ---------------------------------------------------------------------------
# CohortMethodModule
# Map EXACTLY the analysis specification items to CohortMethod args.
# ---------------------------------------------------------------------------

# Study periods (from getDbCohortMethodDataArgs.studyPeriods)
# The specification has one period with empty start and end, which means no restriction.
studyPeriods <- tibble::tibble(
  studyStartDate = c(""),
  studyEndDate   = c("")
)

# Time-at-risks (from createStudyPopArgs.timeAtRisks)
# We create labels to help identify TARs in the analysis descriptions.
timeAtRisks <- tibble::tibble(
  label = c(
    "TAR1",
    "TAR2",
    "TAR3",
    "TAR4"
  ),
  riskWindowStart  = c(1,   365, 1,   365),
  startAnchor      = c("cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd    = c(99999, 99999, 0, 0),
  endAnchor        = c("cohort start", "cohort start", "cohort end", "cohort end"),
  minDaysAtRisk    = c(1, 1, 1, 1)
)

# Propensity Score settings (from propensityScoreAdjustment.psSettings)
# - Two 'match on PS' configurations
# - One 'stratify by PS' configuration
# - One 'no PS adjustment' configuration
matchOnPsArgsList <- tibble::tibble(
  label = c("match_max1", "match_max10"),
  maxRatio  = c(1, 10),
  caliper = c(0.2, 0.2),
  caliperScale  = c("standardized logit", "standardized logit")
)

stratifyByPsArgsList <- tibble::tibble(
  label = c("stratify_10"),
  numberOfStrata  = c(10),
  baseSelection = c("all")
)

# 'None' PS adjustment
noPsArgsList <- tibble::tibble(
  label = c("none")
)

# Build a PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# "match on PS" configurations
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

# "stratify by PS" configurations
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

# "no PS adjustment" configuration
if (exists("noPsArgsList") && nrow(noPsArgsList) > 0) {
  for (i in seq_len(nrow(noPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "none",
      label  = noPsArgsList$label[i],
      params = list()
    )
  }
}

# ---------------------------------------------------------------------------
# Build reusable elements used across analyses
# ---------------------------------------------------------------------------

# Covariate settings:
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE
)

# Outcome list:
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

# Target-Comparator-Outcomes list:
targetComparatorOutcomesList <- list()
for (i in seq_len(nrow(cmTcList))) {
  excludedIds <- c(
    cmTcList$targetConceptId[i],
    cmTcList$comparatorConceptId[i],
    excludedCovariateConcepts$conceptId
  )
  excludedIds <- excludedIds[!is.na(excludedIds)]

  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    excludedCovariateConceptIds = excludedIds
  )
}

# ---------------------------------------------------------------------------
# Iterate through all analysis combinations and assemble cmAnalysisList
# ---------------------------------------------------------------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # PS adjustment method arguments based on configuration
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
        # No PS adjustment: ensure no matching/stratification
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- NULL
      }

      # Get cohort method data args (from getDbCohortMethodDataArgs)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        restrictToCommonPeriod = FALSE,
        covariateSettings = covariateSettings
      )

      # Create PS args (from propensityScoreAdjustment.createPsArgs)
      # If no PS adjustment is requested, set createPsArgs = NULL
      createPsArgs <- if (psCfg$method == "none") {
        NULL
      } else {
        CohortMethod::createCreatePsArgs(
          maxCohortSizeForFitting = 250000,
          errorOnHighCorrelation = TRUE,
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
      }

      # Balance args
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit outcome model args (from fitOutcomeModelArgs)
      # IMPORTANT: When no PS adjustment is used, we must not fit a stratified model.
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = (psCfg$method != "none"),
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

      # Create study population args (from createStudyPopArgs + current TAR)
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep first",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t]
      )

      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(studyStartDate == "", "noStart", studyStartDate),
          ifelse(studyEndDate == "", "noEnd", studyEndDate),
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

# Finalize the CohortMethod module specification
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# ---------------------------------------------------------------------------
# Create the analysis specifications and save to JSON
# ---------------------------------------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", studyName, paste0(studyName, "AnalysisSpecification.json"))
)