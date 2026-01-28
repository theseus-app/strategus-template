################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script creates a Strategus analysis specification JSON for the study:
#   name: alzheimerdonepezil
#
# It follows the structure of the provided Template, but applies the EXACT
# settings from <Analysis Specifications>.
#
# Notes for users:
# - Cohort IDs are pulled from ATLAS/WebAPI, then re-numbered to 1/2/3 for
#   target/comparator/outcome to align with Strategus/CohortMethod conventions.
# - Negative controls are pulled from a concept set and assigned cohortIds
#   starting at 101 to avoid collisions with 1/2/3.
# - The CohortMethod analysis grid is built from:
#     * studyPeriods (here: a single row with NULL start/end -> no restriction)
#     * timeAtRisks (here: a single TAR: 1-180 days from cohort start)
#     * psSettings (here: a single PS config: match on PS)
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# WebAPI endpoint used to export cohort definitions and resolve concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export the target, comparator, and outcome cohort definitions from ATLAS.
# These are the *original* ATLAS cohort IDs from <Analysis Specifications>.
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
# Strategus/CohortMethod typically expects small integer cohort IDs for the
# target/comparator/outcome cohorts used in the analysis.
# We map:
#   target1      (1794126) -> 1
#   comparator1  (1794132) -> 2
#   outcome1     (1794131) -> 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# Pull the negative control concept set (conceptSetId = 1888110) and resolve it
# to a list of concepts. Each concept becomes a negative control outcome cohort.
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
  # Assign cohort IDs for negative controls starting at 101 to avoid collisions
  # with the main cohorts (1,2,3).
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no duplicate cohort IDs between main cohorts and NCs.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames used by CohortMethod module ------------------------------
# Outcomes of interest list (oList): only the primary outcome cohort (id=3).
# cleanWindow is included as in the template; it is not directly used by
# CohortMethod::createOutcome but can be used by other workflows.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target/Comparator list for CohortMethod.
# Names are taken EXACTLY from <Analysis Specifications>.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate include/exclude concept lists -------------------------------------
# <Analysis Specifications> provides conceptsToInclude / conceptsToExclude with
# null/empty entries. We keep these as empty data frames.
includedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource: cohort definitions (target/comparator/outcome).
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Shared resource: negative control outcome cohorts derived from concept set.
# occurrenceType = "first" and detectOnDescendants = TRUE are kept as in template.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specs: generateStats = TRUE (as in template).
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Run diagnostics for all cohorts (including target/comparator/outcome and NCs
# are not included here because cohortDefinitionSet only contains 1/2/3).
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
# <Analysis Specifications> has a single study period with NULL start/end.
# In Strategus template terms, this means "no restriction".
# We represent this as a single-row tibble with NA values.
studyPeriods <- tibble::tibble(
  studyStartDate = as.character(NA),
  studyEndDate   = as.character(NA)
)

# Time-at-risk (TAR) ----------------------------------------------------------
# <Analysis Specifications> defines one TAR:
#   start: 1 day after cohort start
#   end:   180 days after cohort start
#   minDaysAtRisk: 1
timeAtRisks <- tibble::tibble(
  label = c("TAR_1_180_from_cohort_start"),
  riskWindowStart = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(180),
  endAnchor = c("cohort start"),
  minDaysAtRisk = c(1)
)

# Propensity score settings ----------------------------------------------------
# <Analysis Specifications> provides one PS setting: matchOnPsArgs with:
#   maxRatio = 1, caliper = 0.2, caliperScale = "standardized logit"
matchOnPsArgsList <- tibble::tibble(
  label = c("match_maxRatio1_caliper0.2_stdLogit"),
  maxRatio = c(1),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# No stratification settings (explicitly null in <Analysis Specifications>).
stratifyByPsArgsList <- tibble::tibble(
  label = character(0),
  numberOfStrata = integer(0),
  baseSelection = character(0)
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

# Iterate through all analysis setting combinations ---------------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create PS adjustment args based on the selected PS config -------------
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
      # <Analysis Specifications> does not specify detailed FeatureExtraction
      # settings beyond include/exclude concept lists (which are empty here).
      # We use default covariate settings and enable descendant exclusion as in
      # the template.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcomes list ---------------------------------------------------------
      # Build outcomes list containing:
      #  - the outcome of interest (outcome1, cohortId=3)
      #  - all negative control outcomes (cohortIds 101+)
      #
      # priorOutcomeLookback is set from <Analysis Specifications>:
      #   priorOutcomeLookBack = 99999
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

      # Target-Comparator-Outcomes bundle ------------------------------------
      # Excluded covariate concept IDs:
      # - The template excludes target/comparator concepts plus additional
      #   excludedCovariateConcepts. Here, <Analysis Specifications> provides
      #   no valid concept IDs to exclude, so we only pass excludedCovariateConcepts
      #   (empty).
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # getDbCohortMethodDataArgs --------------------------------------------
      # Apply <Analysis Specifications>:
      # - restrictToCommonPeriod is part of createStudyPopArgs in the specs,
      #   but in CohortMethod it is also used at data extraction time.
      # - studyStartDate/studyEndDate are NA -> no restriction.
      # - maxCohortSize = 0 (no limit).
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs ----------------------------------------------------------
      # Apply <Analysis Specifications> exactly:
      # - maxCohortSizeForFitting = 250000
      # - errorOnHighCorrelation = TRUE
      # - prior: laplace with useCrossValidation = TRUE
      # - control: tolerance=2e-7, cvType=auto, fold=10, cvRepetitions=10,
      #            noiseLevel=silent, resetCoefficients=TRUE, startingVariance=0.01
      #
      # Note: CohortMethod::createCreatePsArgs uses Cyclops control; some fields
      # (e.g., fold) are represented via 'fold' in Cyclops::createControl.
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # keep FALSE to allow pipeline completion if a fit fails
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
      # Apply <Analysis Specifications> exactly:
      # - modelType = "logistic"
      # - stratified = TRUE
      # - useCovariates = FALSE
      # - inversePtWeighting = FALSE
      # - prior: laplace with useCrossValidation = TRUE
      # - control: tolerance=2e-7, cvType=auto, fold=10, cvRepetitions=10,
      #            noiseLevel=quiet, resetCoefficients=TRUE, startingVariance=0.01
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "logistic",
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

      # createStudyPopArgs ----------------------------------------------------
      # Apply <Analysis Specifications> exactly:
      # - restrictToCommonPeriod = TRUE
      # - firstExposureOnly = FALSE
      # - washoutPeriod = 0
      # - removeDuplicateSubjects = "keep all"
      # - censorAtNewRiskWindow = FALSE
      # - removeSubjectsWithPriorOutcome = TRUE
      # - priorOutcomeLookBack = 99999
      # - TAR: start=1 from cohort start; end=180 from cohort start; minDaysAtRisk=1
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

      # Append the settings to Analysis List ---------------------------------
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

# Create CohortMethod module specifications -----------------------------------
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
# IMPORTANT: The output path follows the Template pattern:
#   inst/<studyName>/<studyName>AnalysisSpecification.json
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "alzheimerdonepezil", "alzheimerdonepezilAnalysisSpecification.json")
)