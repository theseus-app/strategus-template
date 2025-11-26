################################################################################
# Strategus Analysis Specification for rapidcyclejanssen Study
# 
# This script creates analysis specifications using the OHDSI Strategus package.
# It defines cohorts, negative controls, and analysis settings for a comparative
# effectiveness study using the CohortMethod module.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Connection to ATLAS WebAPI for retrieving cohort and concept set definitions
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export cohort definitions from ATLAS for:
# - Target cohort (ID: 1794126)
# - Comparator cohort (ID: 1794132)  
# - Outcome cohort (ID: 1794131)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to sequential IDs (1, 2, 3) for internal processing
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative Control Outcomes ----------------------------------------------------
# Retrieve negative control concept set (ID: 1888110) from ATLAS
# These are outcomes not expected to be associated with the exposures
# Used for empirical calibration of effect estimates
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
  mutate(cohortId = row_number() + 100) %>% # Negative control cohort IDs start at 101
  select(cohortId, cohortName, outcomeConceptId)

# Validation check to ensure no duplicate cohort IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Define Analysis Cohorts ------------------------------------------------------
# Outcomes: Define the outcome of interest (cohort ID 3)
# cleanWindow parameter specifies the minimum time between recurrent outcomes (365 days)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator cohorts for CohortMethod analysis
# Target: cohort ID 1 (target1)
# Comparator: cohort ID 2 (comparator1)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Excluded Covariate Concepts --------------------------------------------------
# No specific concepts to exclude based on specifications
# This would typically include the drugs of interest to avoid covariate overlap
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# CohortGeneratorModule --------------------------------------------------------
# Creates shared resources for cohort generation
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource specification for main cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Shared resource specification for negative control outcome cohorts
# occurrenceType = "first" means only the first occurrence of each outcome is considered
# detectOnDescendants = TRUE includes descendant concepts in outcome detection
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications for cohort generation with statistics enabled
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule ------------------------------------------------------
# Configures comprehensive diagnostics for all cohorts
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

# Study Period Settings --------------------------------------------------------
# studyStartDate: 20210101 (January 1, 2021)
# studyEndDate: null (no end date restriction)
studyPeriods <- tibble(
  studyStartDate = c("20210101"),
  studyEndDate   = c("")
)

# Time-at-Risk (TAR) Settings --------------------------------------------------
# Define multiple time-at-risk windows for outcome assessment
# All TARs start at day 1 from cohort start (riskWindowStart = 1 or 0)
# minDaysAtRisk = 1 means subjects must have at least 1 day of follow-up
timeAtRisks <- tibble(
  label = c("TAR_0_2", "TAR_1_14", "TAR_1_28", "TAR_1_42", "TAR_1_90"),
  riskWindowStart  = c(0, 1, 1, 1, 1),
  startAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd  = c(2, 14, 28, 42, 90),
  endAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start")
)

# Propensity Score Settings - Match on PS -------------------------------------
# maxRatio = 100: Up to 100 comparator subjects per target subject
# caliper = 0.2: Maximum distance for matching (0.2 standard deviations on logit PS scale)
# caliperScale = "standardized logit": Use standardized logit scale for caliper
matchOnPsArgsList <- tibble(
  label = c("Match_MaxRatio100_Caliper0.2"),
  maxRatio  = c(100),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# Propensity Score Settings - Stratify by PS ----------------------------------
# No stratification by PS specified in this analysis
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0)
)

# Build Propensity Score Configuration List ------------------------------------
# Consolidates all PS methods (match and/or stratify) into a single list
psConfigList <- list()

# Process "match on PS" configurations
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

# Process "stratify by PS" configurations
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

# Create CohortMethod Analysis List --------------------------------------------
# Iterate through all combinations of study periods, TARs, and PS methods
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS matching or stratification based on method
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

      # Covariate Settings -------------------------------------------------------
      # Use default covariate settings with descendants included for exclusion
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcome List -------------------------------------------------------------
      # Combine outcomes of interest and negative controls
      # Outcomes of interest: outcomeOfInterest = TRUE, priorOutcomeLookback = 99999
      # Negative controls: outcomeOfInterest = FALSE, trueEffectSize = 1
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
      
      # Target-Comparator-Outcomes List ------------------------------------------
      # Define the target-comparator pairs with their outcomes and excluded covariates
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Get Database CohortMethod Data Arguments ---------------------------------
      # restrictToCommonPeriod = TRUE: Only include time when both cohorts are observed
      # studyStartDate = "20210101": Start observation from January 1, 2021
      # studyEndDate = "": No end date restriction
      # maxCohortSize = 0: No maximum cohort size (include all subjects)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Create Propensity Score Arguments ----------------------------------------
      # maxCohortSizeForFitting = 250000: Maximum subjects for PS model fitting
      # errorOnHighCorrelation = TRUE: Error if high correlation detected
      # estimator = "att": Average treatment effect in the treated
      # prior: Laplace prior with cross-validation for regularization
      # control: Cyclops control settings for model convergence
      createPsArgs = CohortMethod::createCreatePsArgs(
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
          cvRepetitions = 10,
          startingVariance = 0.01,
          fold = 10
        )
      )

      # Covariate Balance Arguments ----------------------------------------------
      # Compute balance for all covariates (shared) and Table 1 covariates
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit Outcome Model Arguments ----------------------------------------------
      # modelType = "cox": Cox proportional hazards model
      # stratified = TRUE: Stratified Cox model (by matched sets)
      # useCovariates = FALSE: Do not include covariates in outcome model
      # inversePtWeighting = FALSE: Do not use inverse probability of treatment weighting
      # prior: Laplace prior with cross-validation for regularization
      # control: Cyclops control settings for model convergence (noiseLevel = "quiet")
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
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
          cvRepetitions = 10,
          noiseLevel = "quiet",
          fold = 10
        )
      )
      
      # Create Study Population Arguments ----------------------------------------
      # restrictToCommonPeriod = FALSE: Already applied in getDbCohortMethodDataArgs
      # firstExposureOnly = FALSE: Include all exposures (not just first)
      # washoutPeriod = 0: No washout period (already applied at data extraction)
      # removeDuplicateSubjects = "keep all": Keep all exposure episodes
      # censorAtNewRiskWindow = FALSE: Do not censor at new risk windows
      # removeSubjectsWithPriorOutcome = TRUE: Exclude subjects with prior outcome
      # priorOutcomeLookBack = 99999: Look back indefinitely for prior outcomes
      # Risk window parameters from timeAtRisks tibble
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

      # Create CohortMethod Analysis Object --------------------------------------
      # Combines all settings into a single analysis specification
      # Description includes study period, TAR, and PS method for identification
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

# CohortMethod Module Specifications -------------------------------------------
# Create module specifications with all analysis combinations
# refitPsForEveryOutcome = FALSE: Use same PS for all outcomes
# refitPsForEveryStudyPopulation = FALSE: Use same PS for all study populations
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create Complete Analysis Specifications -------------------------------------
# Combine all modules and shared resources into a single specification object
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save Analysis Specifications to JSON -----------------------------------------
# Save the complete analysis specifications to a JSON file for execution
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "rapidcyclejanssen", "rapidcyclejansenAnalysisSpecification.json")
)