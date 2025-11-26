################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script builds a Strategus analysis specification for the study:
#   name: ranitidinecancer
#
# It follows the structure of the provided template and applies settings
# from the provided Analysis Specifications block. Detailed comments explain
# how each specification is mapped into the Strategus/HADES module settings.
################################################################################

# Libraries --------------------------------------------------------------------
library(dplyr)
library(tibble)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Base URL for ROhdsiWebApi (as in the template)
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Pull the target, comparator, and outcome cohorts from ATLAS using the exact IDs
# provided in the Analysis Specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number imported cohorts to canonical IDs 1, 2, 3 as per template pattern.
# This is important to keep internal IDs small and stable in the downstream analysis.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative Control Outcomes ----------------------------------------------------
# From Analysis Specifications:
#   negativeControlConceptSet.id = 1888110, name = "negative"
# We resolve the concept set to concrete outcome conceptIds and assign synthetic
# cohortIds for negative control outcomes starting from 101 upward to avoid
# collisions with main T/C/O cohortIds (1, 2, 3).
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
  mutate(cohortId = dplyr::row_number() + 100) %>% # e.g., 101, 102, 103, ...
  select(cohortId, cohortName, outcomeConceptId)

# Verify that there are no cohortId collisions between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between primary cohorts and negative controls ***")
}

# Build Cohort Lists for Analyses ---------------------------------------------
# Outcomes list: outcome of interest (cohortId 3) with a clean window.
# The clean window here is set to 365; the study population priorOutcomeLookback
# is handled in the createStudyPopArgs (365 days).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator pair for CohortMethod analysis
# Using EXACT names from the Analysis Specifications: "target1" and "comparator1".
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate Selection ----------------------------------------------------------
# From Analysis Specifications:
#   "covariateSelection": conceptsToInclude = [{id: null, name: ""}],
#                         conceptsToExclude = [{id: null, name: ""}]
# This translates to empty selections.
includedCovariateConcepts <- tibble(
  conceptId = integer(),
  conceptName = character()
)

excludedCovariateConcepts <- tibble(
  conceptId = integer(),
  conceptName = character()
)

# CohortGeneratorModule --------------------------------------------------------
# Create shared resources containing the cohort definitions and negative controls
cgModuleSettingsCreator <- CohortGeneratorModule$new()

cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Configure CohortGenerator module to generate cohorts (with stats)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule ------------------------------------------------------
# Run a standard set of diagnostics for all cohorts we fetched/generated.
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
# Study periods from Analysis Specifications:
#   getDbCohortMethodDataArgs.studyPeriods: one period with null start/end -> not restricting.
# Following the template, we use empty strings to indicate no restriction.
studyPeriods <- tibble(
  studyStartDate = c(""),  # YYYYMMDD or "" if unrestricted
  studyEndDate   = c("")   # YYYYMMDD or "" if unrestricted
)

# Time-at-risk from Analysis Specifications:
#   riskWindowStart = 365, startAnchor = "cohort start"
#   riskWindowEnd   = 99999, endAnchor   = "cohort start"
#   minDaysAtRisk   = 1
timeAtRisks <- tibble(
  label = c("TAR_365_to_99999_from_cohort_start"),
  riskWindowStart = c(365),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(99999),
  endAnchor = c("cohort start"),
  minDaysAtRisk = c(1)
)

# Propensity Score settings from Analysis Specifications:
#   psSettings: matchOnPsArgs (maxRatio = 1, caliper = 0.2, caliperScale = "standardized logit")
matchOnPsArgsList <- tibble(
  label = c("Match_1to1_caliper_0.2_stdlogit"),
  maxRatio = c(1),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# No stratifyByPsArgs in specifications
stratifyByPsArgsList <- tibble(
  label = character(),
  numberOfStrata = integer(),
  baseSelection = character()
)

# Combine PS configurations (match and/or stratify) to a list of configs
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

# Build CohortMethod analyses for each combination --------------------------------
cmAnalysisList <- list()
targetComparatorOutcomesList <- list()
analysisId <- 1

# Prepare covariate settings from FeatureExtraction using selections
# (Both included and excluded sets are empty per Analysis Specifications)
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  includedCovariateConceptIds = includedCovariateConcepts$conceptId,
  excludedCovariateConceptIds = excludedCovariateConcepts$conceptId,
  addDescendantsToExclude = TRUE
)

# Build the outcome list: include specified outcome(s) of interest and
# negative control outcomes. For the outcome of interest we align priorOutcomeLookback
# to 365 days per Analysis Specifications' createStudyPopArgs.priorOutcomeLookBack.
outcomeList <- append(
  lapply(seq_len(nrow(oList)), function(i) {
    CohortMethod::createOutcome(
      outcomeId = oList$outcomeCohortId[i],
      outcomeOfInterest = TRUE,
      trueEffectSize = NA,
      priorOutcomeLookback = 365
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

# Build TCO list (single T/C pair) with no explicit excluded covariate IDs here
# (handled via covariateSettings). If needed, one can pass excluded conceptIds
# directly via excludedCovariateConceptIds = c().
for (i in seq_len(nrow(cmTcList))) {
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    excludedCovariateConceptIds = c()
  )
}

# Iterate over Study Periods, TARs, and PS configurations to create analyses
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {
    # One PS configuration (matching) per Analysis Specifications
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # PS adjustment method selection (match vs stratify)
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

      # Get data args as per Analysis Specifications:
      #   studyPeriods: unrestricted (empty strings), maxCohortSize = 0
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,  # Analysis Specifications: createStudyPopArgs.restrictToCommonPeriod = false
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,               # Analysis Specifications: getDbCohortMethodDataArgs.maxCohortSize = 0
        covariateSettings = covariateSettings
      )

      # PS creation args from Analysis Specifications (createPsArgs)
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
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

      # Covariate balance args (keep defaults; table1 spec for per-outcome balance)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit outcome model args from Analysis Specifications (fitOutcomeModelArgs)
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = FALSE,
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

      # Study population args from Analysis Specifications (createStudyPopArgs)
      #   restrictToCommonPeriod = FALSE
      #   firstExposureOnly = FALSE
      #   washoutPeriod = 365
      #   removeDuplicateSubjects = "keep all"
      #   censorAtNewRiskWindow = FALSE
      #   removeSubjectsWithPriorOutcome = TRUE
      #   priorOutcomeLookBack = 365
      #   TAR from timeAtRisks row t
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 365,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t]
      )

      # Add analysis config
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(nchar(studyStartDate) == 0, "unrestricted", studyStartDate),
          ifelse(nchar(studyEndDate) == 0, "unrestricted", studyEndDate),
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

# Wrap CohortMethod module specifications
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Build final analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save specification to JSON file under "inst/ranitidinecancer/"
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ranitidinecancer", "ranitidinecancerAnalysisSpecification.json")
)