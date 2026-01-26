################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script creates a Strategus analysis specification JSON using the OHDSI
# Strategus package, following the provided template and applying the settings
# from <Analysis Specifications>.
#
# Study name (from Analysis Specifications): glp1radepression
#
# Notes on "EXACT names":
# - Cohort names, concept set names, and the study folder/file names are kept
#   exactly as provided in <Analysis Specifications>/<Template>.
# - Cohort IDs are re-numbered to 1/2/3 for Strategus execution consistency,
#   while preserving the original ATLAS cohort definitions via export.
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# WebAPI base URL used to export cohort definitions and resolve concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ------------------------------------------------------------------------------
# Cohort Definitions (Target/Comparator/Outcome)
# ------------------------------------------------------------------------------
# Export the cohort definitions from ATLAS/WebAPI using the original cohort IDs:
# - Target cohort:     1794126 (name: "target1")
# - Comparator cohort: 1794132 (name: "comparator1")
# - Outcome cohort:    1794131 (name: "outcome1")
#
# generateStats = TRUE will request cohort generation stats (if supported).
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to the conventional Strategus IDs:
# 1 = target, 2 = comparator, 3 = outcome
# This is a common pattern in Strategus templates to simplify downstream lists.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# ------------------------------------------------------------------------------
# Negative control outcomes (concept set -> resolved concepts -> NC outcome list)
# ------------------------------------------------------------------------------
# Negative control concept set:
# - conceptSetId: 1888110 (name: "negative")
#
# The template resolves the concept set to a list of concepts and then maps each
# concept to a negative control outcome cohort "stub" (cohortId 101+).
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
    # Negative control cohort IDs start at 101 to avoid collision with 1/2/3.
    cohortId = row_number() + 100
  ) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no duplicate cohort IDs between main cohorts and NCs.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis --------
# Outcomes of interest list (primary outcomes):
# - Here we only have cohortId == 3 (outcome1) as the outcome of interest.
# - cleanWindow is included per template; not used directly in CM args below.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator list for CohortMethod:
# Use the re-numbered cohort IDs (1 and 2) and the EXACT cohort names from specs.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# ------------------------------------------------------------------------------
# Covariate include/exclude concept lists (from Analysis Specifications)
# ------------------------------------------------------------------------------
# The provided specs include placeholder entries with id = null and name = "".
# In R, we represent these as empty data frames (no concepts to include/exclude).
#
# IMPORTANT: The template uses excludedCovariateConcepts in the CM analysis to
# exclude concepts (e.g., drugs of interest) from the PS model. Since no valid
# concept IDs were provided, we keep this empty.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional include list (not used by default covariate settings in this template).
includedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource: cohort definitions to be generated on each site.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Shared resource: negative control outcomes (as concept-based outcomes).
# occurrenceType = "first" and detectOnDescendants = TRUE follow the template.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications: generateStats = TRUE to compute cohort generation stats.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Run diagnostics for all cohorts (target, comparator, outcome, and NCs are
# typically included; template uses cohortDefinitionSet$cohortId only).
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

# ------------------------------------------------------------------------------
# Study period restriction (getDbCohortMethodDataArgs)
# ------------------------------------------------------------------------------
# From Analysis Specifications:
# - studyStartDate: "20130101"
# - studyEndDate:   "20201231"
#
# These are applied in getDbCohortMethodDataArgs with restrictToCommonPeriod = TRUE.
studyPeriods <- tibble::tibble(
  studyStartDate = c("20130101"),
  studyEndDate   = c("20201231")
)

# ------------------------------------------------------------------------------
# Time-at-risk (createStudyPopArgs)
# ------------------------------------------------------------------------------
# From Analysis Specifications:
# - riskWindowStart: 1, startAnchor: "cohort start"
# - riskWindowEnd: 730, endAnchor: "cohort start"
# - minDaysAtRisk: 1
#
# We add a label for readability in analysis descriptions.
timeAtRisks <- tibble::tibble(
  label = c("TAR_1_730_from_cohort_start"),
  riskWindowStart = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(730),
  endAnchor = c("cohort start"),
  minDaysAtRisk = c(1)
)

# ------------------------------------------------------------------------------
# Propensity score adjustment settings
# ------------------------------------------------------------------------------
# From Analysis Specifications:
# - One PS setting: match on PS
#   maxRatio = 1
#   caliper = 0.05
#   caliperScale = "standardized logit"
#
# We create a matchOnPsArgsList with a single row and a descriptive label.
matchOnPsArgsList <- tibble::tibble(
  label = c("match_maxRatio1_caliper0.05_stdLogit"),
  maxRatio = c(1),
  caliper = c(0.05),
  caliperScale = c("standardized logit")
)

# No stratification settings provided (explicitly null in specs).
stratifyByPsArgsList <- tibble::tibble(
  label = character(),
  numberOfStrata = integer(),
  baseSelection = character()
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

# ------------------------------------------------------------------------------
# Iterate through all analysis setting combinations and build cmAnalysisList
# ------------------------------------------------------------------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create PS adjustment args based on method (match vs stratify)
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

      # Covariate settings:
      # The template uses default covariates and excludes descendants for excluded
      # concepts. Since no include/exclude concepts were provided, this remains
      # default behavior.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build outcome list:
      # - Outcomes of interest: outcome cohort(s) from oList
      # - Negative controls: each NC cohortId (101+) with trueEffectSize = 1
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            # From Analysis Specifications: priorOutcomeLookBack = 99999
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

      # Target-comparator-outcomes list:
      # The template expects targetConceptId/comparatorConceptId columns, but the
      # provided Analysis Specifications do not include these. We therefore only
      # exclude the (empty) excludedCovariateConcepts list.
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

      # getDbCohortMethodDataArgs (data extraction settings)
      # From Analysis Specifications:
      # - restrictToCommonPeriod = TRUE
      # - maxCohortSize = 0
      # - studyStartDate/studyEndDate as above
      # - firstExposureOnly, washoutPeriod, removeDuplicateSubjects are specified
      #   in the JSON, but these are applied in createStudyPopArgs rather than
      #   getDbCohortMethodDataArgs in this template.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs (PS model fitting settings)
      # From Analysis Specifications:
      # - maxCohortSizeForFitting = 250000
      # - errorOnHighCorrelation = TRUE
      # - prior: laplace, useCrossValidation = TRUE
      # - control: tolerance=2e-7, cvType=auto, fold=10, cvRepetitions=10,
      #            noiseLevel=silent, resetCoefficients=TRUE, startingVariance=0.01
      #
      # Note: stopOnError is set to FALSE per template rationale (allow pipeline
      # completion even if a model fails).
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
          seed = 1,
          resetCoefficients = TRUE,
          tolerance = 2e-07,
          fold = 10,
          cvRepetitions = 10,
          startingVariance = 0.01
        )
      )

      # Covariate balance computation settings (kept as in template)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs (outcome model settings)
      # From Analysis Specifications:
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
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          cvType = "auto",
          seed = 1,
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          tolerance = 2e-07,
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet"
        )
      )

      # createStudyPopArgs (study population settings)
      # From Analysis Specifications:
      # - restrictToCommonPeriod = FALSE
      # - firstExposureOnly = FALSE
      # - washoutPeriod = 0
      # - removeDuplicateSubjects = "keep all"
      # - censorAtNewRiskWindow = FALSE
      # - removeSubjectsWithPriorOutcome = TRUE
      # - priorOutcomeLookBack = 99999
      # - TAR: 1 to 730 from cohort start, minDaysAtRisk = 1
      #
      # Note: CohortMethod expects removeDuplicateSubjects values like
      # "keep first"/"keep all". We pass EXACT string "keep all" from specs.
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

      # Append the settings to Analysis List
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

# Create CohortMethod module specifications
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

# Save to JSON in the study folder (EXACT study name: glp1radepression)
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "glp1radepression", "glp1radepressionAnalysisSpecification.json")
)