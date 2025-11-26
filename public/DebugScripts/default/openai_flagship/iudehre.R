################################################################################
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# further details.
################################################################################

# This script programmatically builds a Strategus analysis specification JSON
# using the settings provided in <Analysis Specifications>.
# It follows the structure of the provided <Template> and annotates how each
# setting is applied.

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from the specified WebAPI. This baseUrl is used to
# retrieve cohort definitions and concept sets (negative controls).
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Pull the Target, Comparator, and Outcome cohorts by their Atlas/WebAPI IDs.
# Provided in <Analysis Specifications>:
# - targetCohort: id = 1794126, name = "target1"
# - comparatorCohort: id = 1794132, name = "comparator1"
# - outcomeCohort: id = 1794131, name = "outcome1"
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts so that:
#  - target cohortId = 1
#  - comparator cohortId = 2
#  - outcome cohortId = 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes
# From <Analysis Specifications>:
# - negativeControlConceptSet id = 1888110, name = "negative"
# We resolve concepts as outcomeConceptId and assign synthetic cohort IDs
# starting at 101 to avoid ID clashes with T/C/O (1,2,3).
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
    cohortId = dplyr::row_number() + 100 # 101, 102, 103, ...
  ) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: Ensure T/C/O cohort IDs (1..3) do not overlap with the negative control IDs (101+)
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis -------
# Outcomes of interest list (main outcome(s) from cohortDefinitionSet)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%             # outcome cohort id (renumbered)
  mutate(
    outcomeCohortId = cohortId,
    outcomeCohortName = cohortName
  ) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(
    cleanWindow = 365                         # Standard practice to set a clean window; not specified, so using template's 365
  )

# Target and Comparator for the CohortMethod analysis
# Use the EXACT names provided in <Analysis Specifications> for human-readable labels.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate selection (include/exclude lists)
# From <Analysis Specifications>:
# - conceptsToInclude: [{"id": null, "name": ""}] -> effectively empty
# - conceptsToExclude: [{"id": null, "name": ""}] -> effectively empty
# Translate nulls into empty data frames (no specific concept restrictions).
includedCovariateConcepts <- tibble::tibble(
  conceptId = as.integer(c()),
  conceptName = character()
)
excludedCovariateConcepts <- tibble::tibble(
  conceptId = as.integer(c()),
  conceptName = character()
)

# CohortGeneratorModule --------------------------------------------------------
# Shared resources include:
# - The cohort definitions (T/C/O) to be generated
# - The negative control outcome concept set
cgModuleSettingsCreator <- CohortGeneratorModule$new()

cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",          # Detect first occurrence of NC outcome
  detectOnDescendants = TRUE         # Include descendant concepts
)

cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE               # Generate cohort statistics on creation
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# Diagnostics will run on all cohorts generated by CohortGenerator
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
# The CohortMethod module requires combinations of:
# - Study periods
# - Time-at-risk windows
# - PS creation and adjustment strategies
#
# We will construct all combinations according to <Analysis Specifications>,
# resulting in multiple analyses within the single module.

# Study periods:
# From <Analysis Specifications>.getDbCohortMethodDataArgs.studyPeriods:
# - One period with studyStartDate = "20030101", studyEndDate = null (-> empty string)
studyPeriods <- tibble::tibble(
  studyStartDate = c("20030101"),
  studyEndDate   = c("")   # empty string indicates no end date restriction
)

# Time-at-risks (TARs), from <Analysis Specifications>.createStudyPopArgs.timeAtRisks:
# - TAR 1: start 30 (cohort start), end 5475 (cohort start), minDaysAtRisk 1
# - TAR 2: start 365 (cohort start), end 5475 (cohort start), minDaysAtRisk 1
timeAtRisks <- tibble::tibble(
  label = c("TAR_30d_to_5475d", "TAR_365d_to_5475d"),
  riskWindowStart = c(30, 365),
  startAnchor = c("cohort start", "cohort start"),  # use EXACT anchor text
  riskWindowEnd = c(5475, 5475),
  endAnchor = c("cohort start", "cohort start")     # use EXACT anchor text
)

# Propensity Score settings
# From <Analysis Specifications>.propensityScoreAdjustment.psSettings
# We'll encode two PS adjustment strategies:
# 1) Match on PS (1:1, caliper=0.2, caliperScale="standardized logit")
# 2) Stratify by PS (numberOfStrata=5, baseSelection="all")
matchOnPsArgsList <- tibble::tibble(
  label = c("match"),
  maxRatio = c(1),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

stratifyByPsArgsList <- tibble::tibble(
  label = c("stratify"),
  numberOfStrata = c(5),
  baseSelection = c("all")
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Add 'match on PS' configurations
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

# Add 'stratify by PS' configurations
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

# Covariate settings
# We use default covariate settings and allow descendants to be excluded if an
# exclusion list is provided (here, exclusion lists are empty per the spec).
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE
)

# Build the outcome list once (main outcome + negative controls)
outcomeList <- append(
  # Main outcome(s) of interest as specified in oList
  lapply(seq_len(nrow(oList)), function(i) {
    CohortMethod::createOutcome(
      outcomeId = oList$outcomeCohortId[i],
      outcomeOfInterest = TRUE,
      trueEffectSize = NA,
      priorOutcomeLookback = 99999 # from createStudyPopArgs.priorOutcomeLookBack
    )
  }),
  # Negative controls from the resolved concept set
  lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
    CohortMethod::createOutcome(
      outcomeId = i,
      outcomeOfInterest = FALSE,
      trueEffectSize = 1
    )
  })
)

# Create the Target-Comparator-Outcomes (TCO) combinations
# Include any included/excluded covariate concept IDs as per covariateSelection.
targetComparatorOutcomesList <- list()
for (i in seq_len(nrow(cmTcList))) {
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    excludedCovariateConceptIds = excludedCovariateConcepts$conceptId,
    includedCovariateConceptIds = includedCovariateConcepts$conceptId
  )
}

# Iterate through all analysis setting combinations
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Configure PS adjustment method for this analysis
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

      # Pull data settings: map from getDbCohortMethodDataArgs in <Analysis Specifications>
      # - restrictToCommonPeriod = FALSE
      # - studyStartDate / studyEndDate as above (end is empty -> no restriction)
      # - maxCohortSize = 0 (no sampling)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Create PS settings from <Analysis Specifications>.propensityScoreAdjustment.createPsArgs
      # - maxCohortSizeForFitting = 250000
      # - errorOnHighCorrelation = TRUE
      # - prior: laplace with cross-validation
      # - control: tolerance=2e-7, cvType=auto, fold=10, cvRepetitions=10,
      #            noiseLevel=silent, resetCoefficients=TRUE, startingVariance=0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # helpful to allow the pipeline to proceed if a PS model fails
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

      # Compute covariate balance settings (template)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model settings from <Analysis Specifications>.fitOutcomeModelArgs
      # - modelType = "cox"
      # - stratified = TRUE
      # - useCovariates = FALSE
      # - inversePtWeighting = FALSE
      # - prior: laplace with cross-validation
      # - control: tolerance=2e-7, cvType=auto, fold=10, cvRepetitions=10,
      #            noiseLevel=quiet, resetCoefficients=TRUE, startingVariance=0.01
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

      # Study population settings from <Analysis Specifications>.createStudyPopArgs
      # Global (not TAR-specific) parts:
      # - restrictToCommonPeriod = FALSE
      # - firstExposureOnly = FALSE
      # - washoutPeriod = 0
      # - removeDuplicateSubjects = "keep all"
      # - censorAtNewRiskWindow = FALSE
      # - removeSubjectsWithPriorOutcome = FALSE
      # - priorOutcomeLookBack = 99999
      # TAR-specific parts (looping over timeAtRisks rows):
      # - riskWindowStart, startAnchor, riskWindowEnd, endAnchor
      # - minDaysAtRisk = 1
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
        minDaysAtRisk = 1
      )

      # Append the assembled CohortMethod analysis to the list
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

# Finalize the CohortMethod module specification using the assembled analyses
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
# This aggregates:
# - Shared resources (cohort definitions and negative controls)
# - Module specifications (CohortGenerator, CohortDiagnostics, CohortMethod)
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Ensure output directory exists before saving
dir.create(file.path("inst", "iudehre"), recursive = TRUE, showWarnings = FALSE)

# Save to JSON
# The name of the study per <Analysis Specifications> is "iudehre".
# We save under inst/iudehre/iudehreAnalysisSpecification.json
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "iudehre", "iudehreAnalysisSpecification.json")
)