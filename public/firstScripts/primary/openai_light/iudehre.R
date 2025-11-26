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
# This script builds a Strategus analysis specification using the OHDSI
# Strategus/CohortMethod/CohortGenerator/CohortDiagnostics stack.
#
# The settings are taken from the provided "Analysis Specifications" JSON and
# placed into Strategus module specifications. Variable names and cohort names
# are used exactly as provided in the specifications where applicable.
#
# The final JSON is written to:
#   inst/iudehre/iudehreAnalysisSpecification.json
#
# Detailed inline annotations explain how each setting in the Analysis
# Specifications is applied.
################################################################################

# -----------------------------------------------------------------------------
# Shared Resources / Cohort Definitions
# -----------------------------------------------------------------------------
# Base WebAPI URL used to export cohort definitions and concept sets.
# (Using the Atlas demo server by default as in the template.)
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# The Analysis Specifications specify these cohort IDs and names:
# Target cohort:      id = 1794126, name = "target1"
# Comparator cohort:  id = 1794132, name = "comparator1"
# Outcome cohort:     id = 1794131, name = "outcome1"
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number the exported cohort IDs so they are 1, 2, 3 for convenience
# (this is a common pattern used by Strategus templates).
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Overwrite cohort names to match EXACT names from the Analysis Specifications.
# This prevents any automatic name differences from the exported cohort definitions.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1,]$cohortName <- "target1"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 2,]$cohortName <- "comparator1"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 3,]$cohortName <- "outcome1"

# -----------------------------------------------------------------------------
# Negative control concept set
# -----------------------------------------------------------------------------
# Analysis Specifications list a negative control concept set:
# concept set id = 1888110, name = "negative"
# We export and resolve it to a set of concepts, convert to a cohort-like table
# and assign cohort IDs starting at 101 to avoid clash with 1,2,3.
negativeControlConceptSetId <- 1888110
negativeControlName <- "negative" # used for annotation (kept EXACT as specified)

negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = negativeControlConceptSetId,
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
  mutate(cohortId = row_number() + 100) %>% # place negative controls at 101,102,...
  select(cohortId, cohortName, outcomeConceptId)

# Safety check for duplicated cohort IDs between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# -----------------------------------------------------------------------------
# Create data frames that describe targets, comparators, and outcomes
# -----------------------------------------------------------------------------
# Outcomes list: we will use the single outcome cohort (renumbered to id=3)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  # Force the outcome name to EXACT name from Analysis Specifications
  mutate(outcomeCohortId = 3,
         outcomeCohortName = "outcome1") %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # cleanWindow corresponds to the prior/outcome clean window in cohort diagnostics
  mutate(cleanWindow = 365)

# Target and Comparator for CohortMethod
# Use EXACT names from Analysis Specifications
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  stringsAsFactors = FALSE
)

# There are no explicitly-excluded covariate concepts in the Analysis Specifications
# (the conceptsToInclude / conceptsToExclude lists are empty/null), so create an
# empty excludedCovariateConcepts data.frame to pass through to the analysis.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# CohortGeneratorModule: shared resource + module specifications
# -----------------------------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specification for cohort definitions (the cohortDefinitionSet)
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

# Create shared resource specification for negative controls
# We set occurrenceType = "first" and detectOnDescendants = TRUE (common defaults).
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications for CohortGenerator: request generation + stats (generateStats = TRUE)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# -----------------------------------------------------------------------------
# CohortDiagnosticsModule: run diagnostics on defined cohorts
# -----------------------------------------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# We use a broad set of diagnostics as in the template.
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

# -----------------------------------------------------------------------------
# CohortMethodModule: build CohortMethod analyses based on specifications
# -----------------------------------------------------------------------------
# The Analysis Specifications include:
# - getDbCohortMethodDataArgs: studyPeriods with start "20030101", end = NULL
# - createStudyPopArgs: restrictToCommonPeriod = false, firstExposureOnly = true, washoutPeriod = 365,
#                      removeDuplicateSubjects = "keep all", censorAtNewRiskWindow = false,
#                      removeSubjectsWithPriorOutcome = true, priorOutcomeLookBack = 99999,
#                      timeAtRisks: single TAR with riskWindowStart=30 (cohort start) to riskWindowEnd=5475 (cohort start)
# - propensity score adjustment: matching with maxRatio=1, caliper=0.2, caliperScale="standardized logit"
# - createPsArgs settings (regularization/prior/control) provided
# - fitOutcomeModelArgs: modelType = "cox", stratified = false, useCovariates = false, inversePtWeighting = false,
#                        prior and control settings provided

# Study periods: Analysis Specifications provide a single study period start = "20030101"
studyPeriods <- tibble(
  studyStartDate = c("20030101"),  # YYYYMMDD
  studyEndDate   = c(NA_character_) # NA / no end date as specified
)

# Time-at-risk (TAR) as specified in createStudyPopArgs: one TAR
timeAtRisks <- tibble(
  label = c("main_TAR"),
  riskWindowStart  = c(30),
  startAnchor = c("cohort start"),  # EXACT string from specifications
  riskWindowEnd  = c(5475),
  endAnchor = c("cohort start")     # EXACT string from specifications
)

# Propensity Score settings - match on PS (one configuration)
matchOnPsArgsList <- tibble(
  label = c("match_on_ps"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # EXACT string from specifications
)

# No stratify-by-ps configurations specified (the JSON lists stratifyByPsArgs as null)
stratifyByPsArgsList <- tibble(
  label = character(),
  numberOfStrata  = integer(),
  baseSelection = character()
)

# Build psConfigList from matchOnPsArgsList and stratifyByPsArgsList
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
        baseSelection  = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}

# Pre-create FEATURE extraction covariate settings (default) and allow excluding
# descendants of excluded covariate concepts (common pattern).
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE
)

# Convert outcomes to CohortMethod outcome objects:
# - The true outcome(s) of interest (from oList) are set as outcomeOfInterest = TRUE
# - Negative control cohorts are appended as outcomeOfInterest = FALSE and trueEffectSize = 1
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

# Target-comparator-outcomes list: one entry per (target, comparator) pair
targetComparatorOutcomesList <- list()
for (i in seq_len(nrow(cmTcList))) {
  # Create TargetComparatorOutcomes; pass excludedCovariateConceptIds (empty here)
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
  )
}

# We'll now build CohortMethod analyses (cmAnalysisList) by iterating studyPeriods,
# timeAtRisks and psConfigList. The Analysis Specifications present single entries
# for each, so this will create a single CohortMethod analysis.

cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Build matchOnPsArgs or stratifyByPsArgs depending on config.
      if (psCfg$method == "match") {
        # Create matchOnPsArgs according to the specification:
        # maxRatio = 1, caliper = 0.2, caliperScale = "standardized logit"
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

      # Build getDbCohortMethodDataArgs from the study period + covariate settings.
      # The Analysis Specifications define studyStartDate = "20030101", no studyEndDate,
      # and maxCohortSize = 0.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE, # Align with createStudyPopArgs restrictToCommonPeriod = FALSE
        studyStartDate = studyStartDate,
        studyEndDate = if (is.na(studyEndDate)) NULL else studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Create createPsArgs according to Analysis Specifications:
      # maxCohortSizeForFitting = 250000, errorOnHighCorrelation = TRUE,
      # prior: laplace with useCrossValidation = TRUE
      # control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      # noiseLevel = "silent", resetCoefficients = TRUE, startingVariance = 0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # keep PS creation from halting the whole Strategus run
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",
          exclude = c(0), # exclude intercept
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

      # Covariate balance computations (shared + result-specific)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit outcome model arguments as per Analysis Specifications:
      # modelType = "cox", stratified = FALSE, useCovariates = FALSE, inversePtWeighting = FALSE
      # prior: laplace w/ useCrossValidation = TRUE
      # control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      # noiseLevel = "quiet", resetCoefficients = TRUE, startingVariance = 0.01
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = FALSE,
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
          startingVariance = 0.01
        )
      )

      # Create study population args from Analysis Specifications:
      # restrictToCommonPeriod = false, firstExposureOnly = true, washoutPeriod = 365,
      # removeDuplicateSubjects = "keep all", censorAtNewRiskWindow = false,
      # removeSubjectsWithPriorOutcome = true, priorOutcomeLookback = 99999,
      # riskWindowStart = 30, startAnchor = "cohort start",
      # riskWindowEnd = 5475, endAnchor = "cohort start", minDaysAtRisk = 1
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
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t] %||% 1,
        maxDaysAtRisk = 99999
      )

      # Assemble and append the CohortMethod analysis definition
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study period: %s - %s; TAR: %s; PS: %s",
          studyStartDate,
          ifelse(is.null(studyEndDate), "", studyEndDate),
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
    } # end psConfigList loop
  } # end timeAtRisks loop
} # end studyPeriods loop

# Create the CohortMethod module specifications:
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# -----------------------------------------------------------------------------
# Build the complete analysis specifications object and save it to disk
# -----------------------------------------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save to the location designated per template, but using the Analysis Spec name
# "iudehre" exactly as provided.
outputDir <- file.path("inst", "iudehre")
if (!dir.exists(outputDir)) dir.create(outputDir, recursive = TRUE)
outputFile <- file.path(outputDir, "iudehreAnalysisSpecification.json")

ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  outputFile
)

# End of script.