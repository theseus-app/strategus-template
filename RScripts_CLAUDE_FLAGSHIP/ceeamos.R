################################################################################
# OHDSI Strategus Analysis Specification Script for ceeamos study
# 
# This script creates analysis specifications for:
# - Cohort Generation
# - Cohort Diagnostics
# - Cohort Method (comparative cohort study with propensity score adjustment)
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from ATLAS
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions for target (1794126), comparator (1794132), and outcome (1794131)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to sequential IDs (1, 2, 3) for internal use
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve negative control outcome concept set (ID: 1888110) from ATLAS
# These are used to evaluate residual bias in the study
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
  mutate(cohortId = row_number() + 100) %>% # Start negative control IDs from 101 to avoid conflict
  select(cohortId, cohortName, outcomeConceptId)

# Validate that there are no duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts for each analysis component ----------

# Outcomes: Define outcome cohort with clean window
# priorOutcomeLookBack is set to 365 days as specified in createStudyPopArgs
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis 
# Maps cohort ID 1 (target1) vs cohort ID 2 (comparator1)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this study
# Since covariateSelection.conceptsToExclude has null values, create empty data frame
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(0),
  conceptName = character(0)
)

# Note: covariateSelection.conceptsToInclude is also null, so we will not restrict included covariates
# The FeatureExtraction will use default covariate settings (all available covariates)

# CohortGeneratorModule --------------------------------------------------------
# Module responsible for generating cohorts in the CDM database
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specification for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource specification for negative control outcomes
# occurrenceType = "first" means we only count the first occurrence of each negative control
# detectOnDescendants = TRUE means we include descendant concepts in the outcome definition
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications with cohort statistics generation enabled
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# Module for performing diagnostics on generated cohorts
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Configure diagnostics to run for all cohorts (target, comparator, outcome)
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
# Module for comparative cohort analysis with propensity score methods

# Study Period Configuration
# studyStartDate and studyEndDate are both null in specifications
# Creating empty tibble means no restriction on study period
studyPeriods <- tibble(
  studyStartDate = character(0),
  studyEndDate = character(0)
)

# If no study periods are defined, add a single row with empty strings
if (nrow(studyPeriods) == 0) {
  studyPeriods <- tibble(
    studyStartDate = "",
    studyEndDate = ""
  )
}

# Time-at-risk (TAR) Configuration
# From createStudyPopArgs.timeAtRisks: one TAR definition
# riskWindowStart = 0 (start at cohort start)
# startAnchor = "cohort start"
# riskWindowEnd = 0 (end at cohort end)
# endAnchor = "cohort end"
# This creates an "on-treatment" time-at-risk window
timeAtRisks <- tibble(
  label = "OnTreatment",
  riskWindowStart = 0,
  startAnchor = "cohort start",
  riskWindowEnd = 0,
  endAnchor = "cohort end"
)

# Propensity Score Settings - Match on PS
# From propensityScoreAdjustment.psSettings[0].matchOnPsArgs
# maxRatio = 10 (up to 10 comparator subjects per target subject)
# caliper = 0.2 (maximum difference in PS for matching)
# caliperScale = "standardized logit" (caliper measured on standardized logit PS scale)
matchOnPsArgsList <- tibble(
  label = "Match_1to10_cal0.2",
  maxRatio = 10,
  caliper = 0.2,
  caliperScale = "standardized logit"
)

# Propensity Score Settings - Stratify by PS
# stratifyByPsArgs is null in specifications, so create empty tibble
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata = numeric(0),
  baseSelection = character(0)
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Convert matchOnPsArgsList to configuration objects
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

# Convert stratifyByPsArgsList to configuration objects
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

# Iterate through all analysis setting combinations
# This creates the full Cartesian product of: study periods × TARs × PS methods
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment method based on configuration
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

      # Covariate Settings
      # Using default covariates (demographics, conditions, drugs, procedures, etc.)
      # addDescendantsToExclude = TRUE means excluded concepts also exclude their descendants
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcome List: combine outcomes of interest and negative controls
      # For outcome1: outcomeOfInterest = TRUE, priorOutcomeLookback = 365 (from removeSubjectsWithPriorOutcome)
      # For negative controls: outcomeOfInterest = FALSE, trueEffectSize = 1 (null effect)
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
      
      # Create target-comparator-outcomes combinations
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # getDbCohortMethodDataArgs: parameters for extracting data from database
      # restrictToCommonPeriod = FALSE (as specified in createStudyPopArgs)
      # studyStartDate and studyEndDate: both null/empty (no date restrictions)
      # maxCohortSize = 0 (no limit on cohort size)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs: propensity score model estimation settings
      # From propensityScoreAdjustment.createPsArgs:
      # maxCohortSizeForFitting = 250000 (max subjects for fitting PS model)
      # errorOnHighCorrelation = TRUE (error if covariates are highly correlated)
      # prior.priorType = "laplace" (L1 regularization)
      # prior.useCrossValidation = TRUE (use cross-validation to select regularization)
      # control settings for optimization
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

      # computeSharedCovariateBalanceArgs: balance diagnostics for all covariates
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      
      # computeCovariateBalanceArgs: balance diagnostics for Table 1 covariates
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs: outcome model estimation settings
      # From fitOutcomeModelArgs:
      # modelType = "cox" (Cox proportional hazards model)
      # stratified = FALSE (as specified, though note PS matching may add stratification)
      # useCovariates = FALSE (don't use covariates in outcome model)
      # inversePtWeighting = FALSE (don't use inverse probability weighting)
      # prior.priorType = "laplace" (L1 regularization)
      # prior.useCrossValidation = TRUE
      # control settings for optimization (note: noiseLevel = "quiet" as specified)
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = FALSE,
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
      
      # createStudyPopArgs: study population creation settings
      # From createStudyPopArgs:
      # restrictToCommonPeriod = FALSE (don't restrict to common observation period)
      # firstExposureOnly = FALSE (allow multiple exposures per person)
      # washoutPeriod = 365 (require 365 days of prior observation)
      # removeDuplicateSubjects = "remove all" (remove subjects in both target and comparator)
      # censorAtNewRiskWindow = FALSE (don't censor at new exposure)
      # removeSubjectsWithPriorOutcome = TRUE (exclude subjects with prior outcome)
      # priorOutcomeLookBack = 365 (look back 365 days for prior outcome)
      # TAR parameters from timeAtRisks data frame
      # minDaysAtRisk = 1 (require at least 1 day at risk)
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "remove all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 365,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Append the complete analysis specification to the analysis list
      # Each analysis gets a unique ID and human-readable description
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

# Create the complete analysis specifications ---------------------------------
# Combine all modules and shared resources into a single analysis specification
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save analysis specifications to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ceeamos", "ceeamosAnalysisSpecification.json")
)