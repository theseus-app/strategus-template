################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script creates a Strategus analysis specification JSON for the study:
#   name: ranitidinecancer
#
# It follows the provided <Template> structure and applies the exact settings
# from <Analysis Specifications>.
#
# Notes on key mappings from <Analysis Specifications> to this script:
# - cohortDefinitions:
#     targetCohort.id      -> exported then re-numbered to cohortId = 1
#     comparatorCohort.id  -> exported then re-numbered to cohortId = 2
#     outcomeCohort[1].id  -> exported then re-numbered to cohortId = 3
# - negativeControlConceptSet.id -> used to retrieve negative control outcomes
# - getDbCohortMethodDataArgs and createStudyPopArgs are applied per analysis
# - propensityScoreAdjustment.psSettings defines 4 PS adjustment configurations:
#     1) match (1:1) caliper 0.2 standardized logit
#     2) match (1:10) caliper 0.2 standardized logit
#     3) stratify 10 strata baseSelection all
#     4) no PS adjustment (both match/stratify NULL)
# - createStudyPopArgs.timeAtRisks defines 4 TARs; each becomes a separate analysis
#
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# ATLAS/WebAPI endpoint used to export cohort definitions and concept sets:
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export the target, comparator, and outcome cohort definitions from WebAPI.
# The cohort IDs below are the *original* ATLAS cohort definition IDs.
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
# analysis execution. We re-map:
#   target1      -> 1
#   comparator1  -> 2
#   outcome1     -> 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# Retrieve the negative control concept set definition and resolve it to a list
# of concepts. Each concept becomes a negative control outcome cohort later.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # negative
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
  # Negative control cohort IDs start at 101 to avoid collisions with 1..3:
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no duplicate cohort IDs between main cohorts and NCs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold cohorts used in each analysis ---------------------
# Outcomes of interest list (only cohortId == 3 is the primary outcome):
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # Template includes a cleanWindow column; keep it for compatibility:
  mutate(cleanWindow = 365)

# Target and Comparator list for CohortMethod:
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate include/exclude concept lists --------------------------------------
# <Analysis Specifications> provides empty placeholders (id = null, name = "").
# We keep the objects but leave them empty so they do not affect covariate
# construction. (No name auto-correct: keep EXACT object names from template.)
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional include list (not used because conceptsToInclude is empty):
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource: cohort definitions (target/comparator/outcome)
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Shared resource: negative control outcome cohort set
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
# <Analysis Specifications>.getDbCohortMethodDataArgs.studyPeriods contains one
# entry with empty start/end dates, meaning "no restriction".
# The template expects a tibble with potentially multiple rows; we create one row
# with empty strings to represent unrestricted study period.
studyPeriods <- tibble::tibble(
  studyStartDate = c(""),
  studyEndDate   = c("")
)

# Time-at-risks (TARs) --------------------------------------------------------
# Each TAR becomes a separate analysis setting. We add labels for readability.
# Settings are taken EXACTLY from createStudyPopArgs.timeAtRisks.
timeAtRisks <- tibble::tibble(
  label = c(
    "TAR1: start+1 to start+99999",
    "TAR2: start+365 to start+99999",
    "TAR3: start+1 to end+0",
    "TAR4: start+365 to end+0"
  ),
  riskWindowStart = c(1, 365, 1, 365),
  startAnchor     = c("cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd   = c(99999, 99999, 0, 0),
  endAnchor       = c("cohort start", "cohort start", "cohort end", "cohort end")
)

# Propensity Score settings ----------------------------------------------------
# We translate <Analysis Specifications>.propensityScoreAdjustment.psSettings
# into a unified psConfigList, including a "none" option.
matchOnPsArgsList <- tibble::tibble(
  label = c(
    "match_1to1_cal0.2_stdlogit",
    "match_1to10_cal0.2_stdlogit"
  ),
  maxRatio     = c(1, 10),
  caliper      = c(0.2, 0.2),
  caliperScale = c("standardized logit", "standardized logit")
)

stratifyByPsArgsList <- tibble::tibble(
  label = c("stratify_10_all"),
  numberOfStrata = c(10),
  baseSelection  = c("all")
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Add match configurations
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

# Add stratify configurations
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

# Add the "no adjustment" configuration (both matchOnPsArgs and stratifyByPsArgs NULL)
# This corresponds to psSettings entry where both are null.
psConfigList[[length(psConfigList) + 1]] <- list(
  method = "none",
  label  = "no_ps_adjustment",
  params = list()
)

# Iterate through all analysis setting combinations ---------------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Translate PS config into CohortMethod args ----------------------------
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
      } else if (psCfg$method == "none") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- NULL
      }

      # Covariate settings ----------------------------------------------------
      # <Analysis Specifications>.covariateSelection has no include/exclude
      # concepts specified (null/empty), so we use default covariates.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcomes list ---------------------------------------------------------
      # Primary outcome(s) of interest + negative controls.
      # priorOutcomeLookback is applied to outcomes of interest per template.
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

      # Target-comparator-outcomes bundle ------------------------------------
      # Excluded covariate concept IDs:
      # - <Analysis Specifications> does not provide explicit target/comparator
      #   drug concept IDs to exclude, and covariateSelection exclude list is empty.
      # - We therefore only pass excludedCovariateConcepts (empty).
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

      # getDbCohortMethodDataArgs --------------------------------------------
      # Apply <Analysis Specifications>.getDbCohortMethodDataArgs:
      # - restrictToCommonPeriod: false
      # - firstExposureOnly: false
      # - washoutPeriod: 365
      # - removeDuplicateSubjects: "keep first"
      # - maxCohortSize: 0
      # - studyStartDate/studyEndDate: empty strings (no restriction)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        firstExposureOnly = FALSE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "keep first",
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs ----------------------------------------------------------
      # Apply <Analysis Specifications>.propensityScoreAdjustment.createPsArgs:
      # - maxCohortSizeForFitting: 250000
      # - errorOnHighCorrelation: true
      # - prior: laplace with cross-validation
      # - control: tolerance 2e-7, cvType auto, fold 10, cvRepetitions 10,
      #            noiseLevel silent, resetCoefficients true, startingVariance 0.01
      #
      # Note: Strategus template sets stopOnError = FALSE to allow pipeline completion.
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
          fold = 10,
          cvRepetitions = 10,
          seed = 1,
          resetCoefficients = TRUE,
          tolerance = 2e-07,
          startingVariance = 0.01
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
      # Apply <Analysis Specifications>.fitOutcomeModelArgs:
      # - modelType: cox
      # - stratified: true
      # - useCovariates: false
      # - inversePtWeighting: false
      # - prior: laplace with cross-validation
      # - control: tolerance 2e-7, cvType auto, fold 10, cvRepetitions 10,
      #            noiseLevel quiet, resetCoefficients true, startingVariance 0.01
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
          fold = 10,
          cvRepetitions = 10,
          seed = 1,
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          tolerance = 2e-07,
          noiseLevel = "quiet"
        )
      )

      # createStudyPopArgs ----------------------------------------------------
      # Apply <Analysis Specifications>.createStudyPopArgs:
      # - restrictToCommonPeriod: false
      # - firstExposureOnly: false
      # - washoutPeriod: 0
      # - removeDuplicateSubjects: "keep all"
      # - censorAtNewRiskWindow: false
      # - removeSubjectsWithPriorOutcome: true
      # - priorOutcomeLookBack: 99999
      # - TAR-specific: riskWindowStart/startAnchor/riskWindowEnd/endAnchor
      # - minDaysAtRisk: 1
      #
      # Note: CohortMethod createCreateStudyPopulationArgs uses:
      #   priorOutcomeLookback (lowercase 'b') and maxDaysAtRisk.
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
        minDaysAtRisk = 1,
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
# Output path follows the template pattern. Replace folder names as needed for
# your package structure, but keep the filename consistent with your study.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ranitidinecancer", "ranitidinecancerAnalysisSpecification.json")
)