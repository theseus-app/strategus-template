################################################################################
# CohortMethod Analysis Specification for legendt2dm Study
#
# This script creates a comprehensive analysis specification using the OHDSI
# Strategus package for a comparative effectiveness study of treatments in
# Type 2 Diabetes Mellitus (T2DM).
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources: Cohort Definitions and Negative Controls -------------------

# Define cohort IDs from the Analysis Specifications
targetCohortId <- 1794126
comparatorCohortId <- 1794132
outcomeCohortId <- 1794131
negativeControlConceptSetId <- 1888110

baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Export target, comparator, and outcome cohort definitions from ATLAS
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    targetCohortId,      # Target: target1
    comparatorCohortId,  # Comparator: comparator1
    outcomeCohortId      # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to sequential IDs (1, 2, 3) for internal use
cohortDefinitionSet[cohortDefinitionSet$cohortId == targetCohortId, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == comparatorCohortId, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == outcomeCohortId, ]$cohortId <- 3

# Retrieve and process negative control outcomes from the concept set
# Negative controls are used to calibrate the false positive rate
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
  dplyr::rename(outcomeConceptId = "conceptId",
                cohortName = "conceptName") %>%
  # Assign negative control cohort IDs starting from 101 to avoid conflicts
  dplyr::mutate(cohortId = dplyr::row_number() + 100) %>%
  dplyr::select(cohortId, cohortName, outcomeConceptId)

# Validate that there are no duplicate cohort IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create outcome cohorts data frame for the study
# Specifies which cohorts are outcomes of interest
oList <- cohortDefinitionSet %>%
  dplyr::filter(.data$cohortId == 3) %>%
  dplyr::mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  dplyr::select(outcomeCohortId, outcomeCohortName) %>%
  # cleanWindow: lookback period for prior outcomes (99999 days = very long lookback)
  dplyr::mutate(cleanWindow = 99999)

# Create target and comparator cohorts data frame for CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# CohortGeneratorModule Settings -----------------------------------------------
# This module generates the cohorts defined in the analysis

cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specification for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Create shared resource specification for negative control outcomes
# Occurrence type "first" ensures we use first occurrence
# detectOnDescendants TRUE includes descendant concepts in the concept set
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create CohortGenerator module specifications with statistics generation enabled
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings -----------------------------------------------
# This module provides diagnostic analysis of cohort definitions

cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Create comprehensive diagnostics for all cohorts
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

# CohortMethodModule Settings ---------------------------------------------------
# This module performs comparative effectiveness analysis

# Study periods define the date range for the analysis
# From analysis specs: study period 1992-01-01 to 2021-12-31
studyPeriods <- tibble::tibble(
  studyStartDate = "19920101", # YYYYMMDD format
  studyEndDate = "20211231"    # YYYYMMDD format
)

# Time-at-risks (TAR) define the follow-up period for outcome ascertainment
# From analysis specs: TAR starts 1 day after cohort start and ends at cohort end
timeAtRisks <- tibble::tibble(
  label = c("TAR_1d_start_to_cohort_end"),
  riskWindowStart = c(1),           # Start 1 day after cohort start
  startAnchor = c("cohort start"),  # Anchor point is cohort start
  riskWindowEnd = c(0),             # End at cohort end
  endAnchor = c("cohort end"),      # Anchor point is cohort end
  minDaysAtRisk = c(1)              # Minimum 1 day at risk
)

# Propensity Score matching configuration
# From analysis specs: match on PS with maxRatio=100, caliper=0.2, scale=standardized logit
matchOnPsArgsList <- tibble::tibble(
  label = c("PS_match_100ratio_0.2cal"),
  maxRatio = c(100),
  caliper = c(0.2),
  caliperScale = c("standardized logit")  # Options: "propensity score", "standardized", "standardized logit"
)

# Propensity Score stratification configuration
# From analysis specs: stratify into 5 strata, base selection all
stratifyByPsArgsList <- tibble::tibble(
  label = c("PS_stratify_5strata"),
  numberOfStrata = c(5),
  baseSelection = c("all")  # Options: "all", "target", "comparator"
)

# Build propensity score configuration list
# This list will contain all PS adjustment methods to be evaluated
psConfigList <- list()

# Add match on PS configurations
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

# Add stratify by PS configurations
if (exists("stratifyByPsArgsList") && nrow(stratifyByPsArgsList) > 0) {
  for (i in seq_len(nrow(stratifyByPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "stratify",
      label = stratifyByPsArgsList$label[i],
      params = list(
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}

# Build CohortMethod analysis list by iterating through all combinations
# of study periods, time-at-risks, and PS adjustment methods
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create appropriate PS adjustment arguments based on method
      if (psCfg$method == "match") {
        # Matching configuration
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # Stratification configuration
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Define covariate settings for confounding adjustment
      # From analysis specs: conceptsToInclude and conceptsToExclude are empty (use defaults)
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome definitions combining true outcomes and negative controls
      outcomeList <- append(
        # True outcomes of interest with full prior lookback
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = oList$cleanWindow[i]
          )
        }),
        # Negative control outcomes (should show null effects)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Create target-comparator-outcomes combinations
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = c()  # From analysis specs: empty concept sets
        )
      }

      # Get database cohort method data arguments
      # From analysis specs: maxCohortSize=0 (no limit), restrictToCommonPeriod=false,
      # firstExposureOnly=false, washoutPeriod=0, removeDuplicateSubjects="keep all"
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Create propensity score model arguments
      # From analysis specs: Laplace prior with cross-validation, tolerance=2e-7
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,  # Continue even if PS model fails
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
          tolerance = 2e-7,
          cvRepetitions = 10,
          startingVariance = 0.01
        )
      )

      # Compute covariate balance before PS adjustment (shared)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      # Compute covariate balance after PS adjustment
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Create study population arguments
      # From analysis specs: removeSubjectsWithPriorOutcome=true, priorOutcomeLookBack=99999
      # removeDuplicateSubjects="keep all", washoutPeriod=0, firstExposureOnly=false
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

      # Fit outcome model arguments
      # From analysis specs: Cox model, stratified, useCovariates=false, inversePtWeighting=false
      # Laplace prior with cross-validation, tolerance=2e-7
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
          tolerance = 2e-7,
          cvRepetitions = 10,
          noiseLevel = "quiet"
        )
      )

      # Append complete analysis specification to the list
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

# Create the complete analysis specifications ------------------------------------
# Combine all modules and shared resources into one specification object

analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file for execution
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "legendt2dm", "legendt2dmAnalysisSpecification.json")
)