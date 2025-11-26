################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script builds a Strategus analysis specification JSON for the study:
# name = "strokerisk"
#
# It follows the Template structure and applies the settings from the provided
# Analysis Specifications. Comments throughout explain how each setting is used.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

# Load libraries used directly by the script
library(dplyr)
library(Strategus)

# ------------------------------------------------------------------------------
# Shared Resources --------------------------------------------------------------
# ------------------------------------------------------------------------------

# Base URL for the ATLAS/WebAPI from which to fetch cohort definitions and
# negative control concept sets
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# - We fetch the 3 cohorts identified in the Analysis Specifications:
#   Target:      id = 1794126, name = "target1"
#   Comparator:  id = 1794132, name = "comparator1"
#   Outcome:     id = 1794131, name = "outcome1"
# - We re-number the local cohort IDs to 1, 2, 3 to simplify analysis references.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to internal IDs: 1 = Target, 2 = Comparator, 3 = Outcome
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes
# - The Analysis Specifications provide a single negative control concept set:
#   id = 1888110, name = "negative"
# - We create a "negative control outcome cohort set" table with unique synthetic
#   cohortIds starting at 101 to avoid collision with IDs 1,2,3 used above.
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
  dplyr::rename(
    outcomeConceptId = "conceptId",
    cohortName = "conceptName"
  ) %>%
  dplyr::mutate(cohortId = dplyr::row_number() + 100) %>% # negative controls start at 101, 102, ...
  dplyr::select(cohortId, cohortName, outcomeConceptId)

# Safety check: no duplicate cohort IDs across primary cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create small convenience frames used in downstream modules --------------------

# Outcomes list
# - The single outcome cohort to analyze is cohortId = 3 (renumbered above)
# - We also add a 'cleanWindow' commonly used in execution for prior outcome
#   cleanliness around index. The value here follows the Template example.
oList <- cohortDefinitionSet %>%
  dplyr::filter(.data$cohortId == 3) %>%
  dplyr::mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  dplyr::select(outcomeCohortId, outcomeCohortName) %>%
  dplyr::mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# - Use EXACT names from Analysis Specifications for documentation purposes.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate inclusion/exclusion lists
# - The Analysis Specifications provide empty lists for conceptsToInclude and
#   conceptsToExclude. We honor that here, keeping both empty.
includedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# ------------------------------------------------------------------------------
# CohortGeneratorModule ---------------------------------------------------------
# ------------------------------------------------------------------------------

cgModuleSettingsCreator <- CohortGeneratorModule$new()

# - Bundle the main cohort definitions as a shared resource so all modules can
#   refer to the same definitions and output.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# - Bundle the negative controls as a shared resource. Strategus will generate
#   these outcome cohorts at execution-time for use in diagnostics/calibration.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# - Specify generation options (e.g., generateStats = TRUE).
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ------------------------------------------------------------------------------
# CohortDiagnosticsModule Settings ---------------------------------------------
# ------------------------------------------------------------------------------

# - Enable recommended diagnostics, including IR and cohort relationship,
#   and temporal characterization.
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

# ------------------------------------------------------------------------------
# CohortMethodModule ------------------------------------------------------------
# ------------------------------------------------------------------------------

# 1) Study periods specified by the Analysis Specifications:
#    - Two windows:
#      a) 20010101 - 20171231
#      b) 20010101 - 20150930
studyPeriods <- tibble::tibble(
  studyStartDate = c("20010101", "20010101"),
  studyEndDate   = c("20171231", "20150930")
)

# 2) Time-at-risk (TAR) per the Analysis Specifications:
#    - Single TAR:
#      riskWindowStart = 1 day after "cohort start"
#      riskWindowEnd = 0 at "cohort end" (interpreted by CM as end anchored at "cohort end")
timeAtRisks <- tibble::tibble(
  label = c("cohort start +1 to cohort end"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
)

# 3) Propensity Score adjustment settings from the Analysis Specifications:
#    Three PS settings are provided:
#      - No PS adjustment (matchOnPsArgs = NULL, stratifyByPsArgs = NULL)
#      - Match on PS: maxRatio = 1,  caliper = 0.05, caliperScale = "propensity score"
#      - Match on PS: maxRatio = 10, caliper = 0.2,  caliperScale = "standardized logit"
psConfigList <- list(
  list(
    method = "none",
    label  = "no_ps",
    params = list()
  ),
  list(
    method = "match",
    label  = "match_1to1_caliper0.05_ps",
    params = list(
      maxRatio = 1,
      caliper = 0.05,
      caliperScale = "propensity score"
    )
  ),
  list(
    method = "match",
    label  = "match_1to10_caliper0.2_stdlogit",
    params = list(
      maxRatio = 10,
      caliper = 0.2,
      caliperScale = "standardized logit"
    )
  )
)

# 4) Build the CohortMethod analysis list by iterating all combinations:
#    - 2 study periods x 1 TAR x 3 PS configs = 6 analyses
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create PS adjustment arguments based on method
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
        # No PS adjustment
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- NULL
      }

      # Covariate settings for FeatureExtraction
      # - Default settings are used; any concept-level include/exclude is passed at
      #   the TargetComparatorOutcomes level (see 'includedCovariateConceptIds' and
      #   'excludedCovariateConceptIds' below).
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcomes list combines:
      # - The primary outcome(s) of interest (outcomeOfInterest = TRUE)
      # - The negative controls (outcomeOfInterest = FALSE, trueEffectSize = 1)
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

      # T/C/O combinations:
      # - includedCovariateConceptIds and excludedCovariateConceptIds reflect the
      #   Analysis Specifications covariateSelection (empty here).
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          includedCovariateConceptIds = includedCovariateConcepts$conceptId,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Data fetch settings (createGetDbCohortMethodDataArgs) per Analysis Specs:
      # - restrictToCommonPeriod = FALSE
      # - studyStartDate / studyEndDate come from the 'studyPeriods' loop
      # - maxCohortSize = 0 (no cap)
      # - firstExposureOnly = TRUE
      # - washoutPeriod = 183
      # - removeDuplicateSubjects = "keep first"
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        firstExposureOnly = TRUE,
        washoutPeriod = 183,
        removeDuplicateSubjects = "keep first",
        covariateSettings = covariateSettings
      )

      # Propensity score fitting settings (createCreatePsArgs) per Analysis Specs:
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

      # Covariate balance calculation args
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model settings (createFitOutcomeModelArgs) per Analysis Specs:
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

      # Study population settings (createCreateStudyPopulationArgs) per Analysis Specs:
      # - restrictToCommonPeriod = FALSE
      # - firstExposureOnly = FALSE
      # - washoutPeriod = 0
      # - removeDuplicateSubjects = "keep all"
      # - censorAtNewRiskWindow = FALSE
      # - removeSubjectsWithPriorOutcome = FALSE
      # - priorOutcomeLookback = 99999
      # - TAR taken from 'timeAtRisks'
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

# Build the CohortMethod module specification object
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# ------------------------------------------------------------------------------
# Create and save the overall Strategus Analysis Specifications ----------------
# ------------------------------------------------------------------------------

analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save analysis specifications JSON
# - The file path uses the EXACT study name from the Analysis Specifications
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "strokerisk", "strokeriskAnalysisSpecification.json")
)