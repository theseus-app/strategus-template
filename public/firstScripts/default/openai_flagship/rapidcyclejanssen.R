################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script builds a Strategus analysis specification using the provided
# Analysis Specifications and the template structure. It fetches cohorts
# from ATLAS/WebAPI, sets up negative control outcomes, and configures
# Cohort Diagnostics and Cohort Method modules with detailed settings.
#
# Notes:
# - All names and parameters are taken EXACTLY from the Template and/or
#   the provided Analysis Specifications.
# - Additional inline comments explain how each setting is applied.
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# ATLAS/WebAPI server to pull cohorts/concept sets from.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export the target, comparator, and outcome cohorts from WebAPI using their exact IDs.
# We then re-number them locally to 1, 2, 3 for convenience in downstream modules.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts (local IDs used by the analysis; the original IDs remain
# in the cohort JSON but we refer to these local IDs in module settings).
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# Build a negative control outcome "cohort set" from a concept set definition.
# We use the conceptSetId exactly as provided, and then map concepts into
# a table with columns: cohortId (local, >100), cohortName, and outcomeConceptId.
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
    # target/comparator/outcomes start from 1, 2, 3... so set negatives to 101, 102, ...
    cohortId = dplyr::row_number() + 100
  ) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure local cohort IDs do not overlap
if (length(intersect(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)) > 0) {
  stop("*** Error: duplicate cohort IDs found between primary and negative control cohorts ***")
}

# Create helper data frames for cohorts used in analyses -----------------------
# Outcomes list: use the (re-numbered) outcome cohort (local id == 3)
oList <- cohortDefinitionSet %>%
  dplyr::filter(.data$cohortId == 3) %>%
  mutate(
    outcomeCohortId = cohortId,
    outcomeCohortName = cohortName
  ) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # Optional: clean window for outcome recurrence (kept as per template)
  mutate(cleanWindow = 365)

# Target and Comparator pairing for CohortMethod
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  stringsAsFactors = FALSE
)

# Covariate concepts to include/exclude (FeatureExtraction)
# The provided Analysis Specifications list empty include/exclude entries.
# We build empty data frames accordingly.
includedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource: cohort definitions for generation
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

# Shared resource: negative control outcomes (from concept sets)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications for cohort generation
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

# Study period(s): from the provided getDbCohortMethodDataArgs.studyPeriods
# - We keep one row with studyStartDate = 20210101, studyEndDate = NA (open-ended).
studyPeriods <- tibble::tibble(
  studyStartDate = c(20210101),
  studyEndDate   = c(NA_integer_)
)

# Time-at-risks (TARs) defined in createStudyPopArgs.timeAtRisks
# We include a label for readability in analysis descriptions.
timeAtRisks <- tibble::tibble(
  label = c("TAR1_1-14d", "TAR2_1-28d", "TAR3_1-42d", "TAR4_1-90d", "TAR5_0-2d"),
  riskWindowStart  = c(1, 1, 1, 1, 0),
  startAnchor      = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd    = c(14, 28, 42, 90, 2),
  endAnchor        = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start"),
  minDaysAtRisk    = c(1, 1, 1, 1, 1)
)

# Propensity Score settings based on provided propensityScoreAdjustment
# - Only matchOnPsArgs is specified (stratifyByPsArgs is NULL)
matchOnPsArgsList <- tibble::tibble(
  label = c("PSMatch_maxR100_cal0.2_stdLogit"),
  maxRatio  = c(100),
  caliper   = c(0.2),
  caliperScale  = c("standardized logit")
)

stratifyByPsArgsList <- tibble::tibble(
  label = character(0),
  numberOfStrata = integer(0),
  baseSelection = character(0)
)

# Build PS configuration list from the two data frames
psConfigList <- list()

# Add match-on-PS configurations
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

# Add stratify-by-PS configurations (none in this specification)
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

# Iterate through all combinations of study periods, TARs, and PS configs ------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # PS adjustment method
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

      # Covariate settings (FeatureExtraction)
      # - include/exclude concept IDs are empty per provided specification.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        includedCovariateConceptIds = includedCovariateConcepts$conceptId,
        addDescendantsToInclude = TRUE,
        excludedCovariateConceptIds = excludedCovariateConcepts$conceptId,
        addDescendantsToExclude = TRUE
      )

      # Outcome list: primary outcome(s) + negative control outcomes
      outcomeList <- append(
        # Primary outcomes
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        # Negative control outcomes
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Target-Comparator-Outcomes definition
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude covariates tied to specific concepts (none provided here)
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Settings for data extraction (getDbCohortMethodDataArgs)
      # - Based on provided getDbCohortMethodDataArgs
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        firstExposureOnly = TRUE,              # From provided getDbCohortMethodDataArgs
        washoutPeriod = 365,                   # From provided getDbCohortMethodDataArgs
        removeDuplicateSubjects = "remove all",# From provided getDbCohortMethodDataArgs
        covariateSettings = covariateSettings
      )

      # Create PS settings (createPsArgs) per provided specification
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # allow downstream steps even if PS fit fails
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          tolerance = 2e-07,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "silent",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Covariate balance settings (shared and with a default Table 1 filter)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit outcome model settings (fitOutcomeModelArgs) per provided specification
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
          tolerance = 2e-07,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Study population settings (createStudyPopArgs) per provided specification and current TAR
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

      # Append the settings to the CohortMethod Analysis list
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "StudyPeriod: %s-%s; TAR: %s (%s %d to %s %d); PS: %s",
          as.character(studyStartDate),
          as.character(studyEndDate),
          timeAtRisks$label[t],
          timeAtRisks$startAnchor[t], timeAtRisks$riskWindowStart[t],
          timeAtRisks$endAnchor[t], timeAtRisks$riskWindowEnd[t],
          psCfg$label
        ),
        getDbCohortMethodDataArgs = getDbCohortMethodDataArgs,
        createStudyPopArgs = createStudyPopArgs,
        createPsArgs = createPsArgs,
        matchOnPsArgs = if (psCfg$method == "match") matchOnPsArgs else NULL,
        stratifyByPsArgs = if (psCfg$method == "stratify") stratifyByPsArgs else NULL,
        computeSharedCovariateBalanceArgs = computeSharedCovariateBalanceArgs,
        computeCovariateBalanceArgs = computeCovariateBalanceArgs,
        fitOutcomeModelArgs = fitOutcomeModelArgs
      )
      analysisId <- analysisId + 1
    }
  }
}

# Build CohortMethod Module specifications
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the full analysis specifications --------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save to JSON (ensure output directory exists)
outputDir <- file.path("inst", "rapidcyclejanssen")
if (!dir.exists(outputDir)) {
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
}
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path(outputDir, "rapidcyclejanssenAnalysisSpecification.json")
)