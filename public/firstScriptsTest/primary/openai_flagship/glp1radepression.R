################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script creates a Strategus analysis specification JSON for the study:
#   name: glp1radepression
#
# It follows the structure of the provided <Template> and applies the exact
# settings from <Analysis Specifications>.
#
# Notes on key mappings from <Analysis Specifications>:
# - Cohort definitions (ATLAS cohort IDs) are exported and then re-numbered to
#   local IDs 1..N for Strategus/CohortMethod usage:
#     1 = target1 (ATLAS cohortId 1794126)
#     2 = comparator1 (ATLAS cohortId 1794132)
#     3 = outcome1 (ATLAS cohortId 1794131)
# - Negative controls are pulled from ATLAS concept set:
#     conceptSetId = 1888110 ("negative")
#   and are assigned cohortIds starting at 101 (per template convention).
# - Study period(s): 20130101 to 20201231
# - Study population settings:
#     restrictToCommonPeriod = true
#     firstExposureOnly = false
#     washoutPeriod = 0
#     removeDuplicateSubjects = "keep all"
#     censorAtNewRiskWindow = false
#     removeSubjectsWithPriorOutcome = true
#     priorOutcomeLookBack = 99999
#     time-at-risk: start=1 (cohort start) to end=730 (cohort start), minDaysAtRisk=1
# - PS adjustment: match on PS with maxRatio=1, caliper=0.05, caliperScale="propensity score"
# - PS model regularization/control and outcome model regularization/control are
#   set exactly as specified (Laplace + CV; Cyclops control parameters).
#
# Output:
#   inst/glp1radepression/glp1radepressionAnalysisSpecification.json
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# ATLAS WebAPI base URL used to export cohort definitions and concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export the ATLAS cohort definitions for target, comparator, and outcome.
# The IDs below are the *ATLAS* cohort IDs from <Analysis Specifications>.
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
# Strategus/CohortMethod typically uses small integer cohort IDs internally.
# We map the exported ATLAS cohort IDs to:
#   1 = target1, 2 = comparator1, 3 = outcome1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# Pull the negative control concept set from ATLAS and convert it into a set of
# negative control outcome cohort specifications.
#
# IMPORTANT:
# - The concept set ID is taken exactly from <Analysis Specifications>:
#     negativeControlConceptSet.id = 1888110
# - We assign cohort IDs starting at 101 to avoid collisions with 1..3.
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

# Safety check: ensure no duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames used by CohortMethodModule --------------------------------
# Outcomes of interest list (oList):
# - We include the single outcome cohort (local cohortId == 3).
# - cleanWindow is included per template; not specified in <Analysis Specifications>,
#   so we keep the template default of 365.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target/Comparator list for CohortMethod:
# Names are set to the exact cohort names from <Analysis Specifications>.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate include/exclude concept lists --------------------------------------
# <Analysis Specifications> provides conceptsToInclude / conceptsToExclude with null/empty.
# We therefore do not define includedCovariateConcepts and keep excludedCovariateConcepts empty.
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

# Shared resource: negative control outcomes (as concept-based cohorts)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications: generate cohort stats
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Diagnostics are not specified in <Analysis Specifications>, so we keep the
# template defaults.
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
# <Analysis Specifications> -> getDbCohortMethodDataArgs.studyPeriods:
#   20130101 to 20201231
studyPeriods <- tibble::tibble(
  studyStartDate = c("20130101"),
  studyEndDate   = c("20201231")
)

# Time-at-risk (TAR) ----------------------------------------------------------
# <Analysis Specifications> -> createStudyPopArgs.timeAtRisks:
#   start=1 (cohort start), end=730 (cohort start), minDaysAtRisk=1
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

# Propensity Score settings ----------------------------------------------------
# <Analysis Specifications> -> propensityScoreAdjustment.psSettings:
#   matchOnPsArgs: maxRatio=1, caliper=0.05, caliperScale="propensity score"
matchOnPsArgsList <- tibble::tibble(
  label = c("PS_match_1to1_caliper0.05_ps"),
  maxRatio = c(1),
  caliper = c(0.05),
  caliperScale = c("propensity score")
)

# No stratification settings provided (explicitly null in <Analysis Specifications>)
stratifyByPsArgsList <- tibble::tibble(
  label = character(),
  numberOfStrata = numeric(),
  baseSelection = character()
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Convert match-on-PS rows into config entries
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

# Convert stratify-by-PS rows into config entries (none expected here)
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

# Iterate through all analysis setting combinations ----------------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create PS adjustment args based on method ------------------------------
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
      # <Analysis Specifications> does not override FeatureExtraction settings,
      # so we use default covariates. Exclusions are handled via
      # excludedCovariateConceptIds in createTargetComparatorOutcomes below.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcomes list ----------------------------------------------------------
      # - outcome1 is marked outcomeOfInterest = TRUE
      # - negative controls are outcomeOfInterest = FALSE, trueEffectSize = 1
      # - priorOutcomeLookback for the outcome of interest is set to 99999 per spec
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

      # Target-Comparator-Outcomes bundle -------------------------------------
      # Excluded covariate concepts:
      # - The template shows excluding target/comparator concepts; however, our
      #   <Analysis Specifications> does not provide such concept IDs.
      # - We therefore only pass excludedCovariateConcepts (empty).
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # getDbCohortMethodDataArgs ---------------------------------------------
      # <Analysis Specifications> -> getDbCohortMethodDataArgs:
      #   maxCohortSize = 0
      #   studyStartDate/studyEndDate as above
      #
      # Note: restrictToCommonPeriod is a *study population* setting in the spec,
      # but CohortMethod::createGetDbCohortMethodDataArgs also has this argument.
      # We set it to TRUE to align with the overall intent and to ensure data
      # extraction respects the common period restriction.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs -----------------------------------------------------------
      # <Analysis Specifications> -> propensityScoreAdjustment.createPsArgs
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # keep FALSE to allow pipeline completion if a model fails
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

      # Covariate balance args -------------------------------------------------
      # Not specified in <Analysis Specifications>; keep template defaults.
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs ----------------------------------------------------
      # <Analysis Specifications> -> fitOutcomeModelArgs
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
          startingVariance = 0.01,
          seed = 1
        )
      )

      # createStudyPopArgs -----------------------------------------------------
      # <Analysis Specifications> -> createStudyPopArgs
      #
      # Note: CohortMethod::createCreateStudyPopulationArgs uses:
      #   removeDuplicateSubjects = "keep all" (exact string from spec)
      #   censorAtNewRiskWindow = FALSE
      #   restrictToCommonPeriod = TRUE (per spec)
      #
      # TAR is set using the single TAR row.
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

      # Append analysis settings ----------------------------------------------
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
# Output path uses the study name exactly: glp1radepression
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "glp1radepression", "glp1radepressionAnalysisSpecification.json")
)