################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates an analysis specification for the "ceeamos" study
# using the OHDSI Strategus package. It configures cohort definitions,
# negative control outcomes, and CohortMethod analysis parameters.
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
# Export cohort definitions for:
# - Target cohort (ID: 1794126, name: target1)
# - Comparator cohort (ID: 1794132, name: comparator1)
# - Outcome cohort (ID: 1794131, name: outcome1)
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
# Retrieve negative control concept set (ID: 1888110, name: negative)
# and convert concepts to outcome cohorts with IDs starting at 101
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
  # Assign cohort IDs starting at 101 to avoid conflicts with target/comparator cohorts
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Validation: Check for duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to organize cohorts for analysis ----------------------------

# Outcomes: Extract outcome cohort(s) with prior outcome lookback window
# priorOutcomeLookback set to 365 days as specified in analysis specifications
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Maps target1 (ID 1) and comparator1 (ID 2)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Excluded covariate concepts: empty as specified in covariateSelection
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# CohortGeneratorModule --------------------------------------------------------
# This module handles cohort generation from cohort definitions

cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for main cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Create shared resource specifications for negative control outcomes
# Using first occurrence and descendant concept detection
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create cohort generator module specifications
# Generate statistics for cohort characterization
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# This module performs comprehensive cohort diagnostics

cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Configure diagnostics for all cohorts
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
# This module performs comparative effectiveness analysis using propensity score methods

# Study Period: No specific time window restriction (empty values from specifications)
# When studyStartDate and studyEndDate are NULL/empty, analysis uses full data
studyPeriods <- tibble(
  studyStartDate = as.character(NA), # YYYYMMDD format - NULL means use all data
  studyEndDate = as.character(NA)    # YYYYMMDD format - NULL means use all data
)

# Time-at-Risk (TAR) Configuration
# Single TAR: starts at cohort start, ends at cohort end, minimum 1 day at risk
# minDaysAtRisk = 1 ensures subjects have exposure time
timeAtRisks <- tibble(
  label = c("Full follow-up"),
  riskWindowStart = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(0),
  endAnchor = c("cohort end"),
  minDaysAtRisk = c(1)
)

# Propensity Score (PS) Matching Configuration
# Single PS matching strategy with:
# - maxRatio: 10 (each comparator matched to up to 10 targets)
# - caliper: 0.2 on standardized logit scale (caliper = 0.2 SD)
# - caliperScale: "standardized logit" for better balance on probability scale
matchOnPsArgsList <- tibble(
  label = c("PS matching"),
  maxRatio = c(10),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# No PS stratification configurations (stratifyByPsArgsList is not used)
# All PS adjustment uses matching only

# Build PS configuration list
# Each config specifies a PS adjustment method (match or stratify) with parameters
psConfigList <- list()

# Convert matching configurations to config list
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

# Initialize CohortMethod analysis list
cmAnalysisList <- list()
analysisId <- 1

# Iterate through all analysis setting combinations
# (study periods × time-at-risks × PS configurations)
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]
  
  # Handle NULL/NA dates
  if (is.na(studyStartDate)) studyStartDate <- ""
  if (is.na(studyEndDate)) studyEndDate <- ""

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Create PS adjustment arguments based on method type
      if (psCfg$method == "match") {
        # PS Matching configuration
        # allowReverseMatch = FALSE: only match targets to comparators, not vice versa
        # stratificationColumns = c(): no stratification variables
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # PS Stratification configuration (not used in this analysis)
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate Settings
      # Default covariates: all covariates from 365 days prior to cohort start
      # useLengthOfRiskWindow = FALSE by default
      # addDescendantsToExclude = TRUE: exclude descendant concepts in exclusion set
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcome List Construction
      # Combine true outcomes and negative control outcomes
      # True outcomes: outcomeOfInterest = TRUE, priorOutcomeLookback = 99999
      # NC outcomes: outcomeOfInterest = FALSE, trueEffectSize = 1
      outcomeList <- append(
        # True outcomes from outcome cohorts
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        # Negative control outcomes
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Target-Comparator-Outcome List
      # Maps each target-comparator pair with their outcomes and exclusions
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = c(excludedCovariateConcepts$conceptId)
        )
      }

      # getDbCohortMethodDataArgs: Data extraction settings
      # restrictToCommonPeriod = FALSE: no requirement for overlapping follow-up
      # maxCohortSize = 0: no size restriction
      # studyStartDate/studyEndDate: empty (use all available data)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs: Propensity Score Model Settings
      # modelType: generalized linear model (default logistic regression)
      # maxCohortSizeForFitting = 250000: PS model fit limit
      # errorOnHighCorrelation = TRUE: stop if multicollinearity detected
      # stopOnError = FALSE: continue analysis if PS model fails
      # estimator = "att": estimate average treatment effect on the treated
      # prior: Laplace prior with cross-validation
      # control: Cyclops optimization with 10-fold cross-validation, 10 repetitions
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
          cvRepetitions = 10,
          startingVariance = 0.01
        )
      )

      # computeSharedCovariateBalanceArgs: Balance before/after PS adjustment
      # maxCohortSize = 250000: limit for balance calculation
      # covariateFilter = NULL: include all covariates in balance assessment
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      # computeCovariateBalanceArgs: Table 1 covariates only
      # Uses default Table 1 specifications (demographics, conditions, drugs, etc.)
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs: Outcome Model (HR estimation) Settings
      # modelType = "cox": Cox proportional hazards model
      # stratified = TRUE: stratify by matched pairs (matching adjustment)
      # useCovariates = FALSE: no covariate adjustment in outcome model
      # inversePtWeighting = FALSE: no inverse probability of treatment weighting
      # prior: Laplace prior with cross-validation for regularization
      # control: Cyclops optimization with 10-fold CV, 10 repetitions, quiet output
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
          cvRepetitions = 10,
          noiseLevel = "quiet"
        )
      )

      # createStudyPopArgs: Study Population Definition
      # restrictToCommonPeriod = FALSE: no overlap requirement
      # firstExposureOnly = FALSE: include all exposures
      # washoutPeriod = 365: 365-day clean period before cohort start
      # removeDuplicateSubjects = "remove all": deduplicate subjects across cohorts
      # censorAtNewRiskWindow = FALSE: no censoring at new exposure
      # removeSubjectsWithPriorOutcome = TRUE: exclude prior outcome cases
      # priorOutcomeLookback = 365: look back 365 days for prior outcomes
      # Time-at-risk: starts day 1 of cohort start, ends day 0 after cohort end
      # minDaysAtRisk = 1: minimum 1 day of follow-up required
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
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999
      )

      # Create CohortMethod analysis object
      # Combines all settings: data extraction, population, PS, outcome modeling
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

# CohortMethodModule Configuration -----------------------------------------------
# Aggregate all CohortMethod analyses and specifications

cmModuleSettingsCreator <- CohortMethodModule$new()

# Create CohortMethod module specifications with all analyses
# analysesToExclude = NULL: run all analyses
# refitPsForEveryOutcome = FALSE: fit PS once per target-comparator pair
# refitPsForEveryStudyPopulation = FALSE: apply same PS model across outcomes
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create Analysis Specifications ---------------------------------------------------
# Combine all module specifications into one comprehensive specification

analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Add shared resources: cohort definitions
  Strategus::addSharedResources(cohortDefinitionShared) |>
  # Add shared resources: negative control outcomes
  Strategus::addSharedResources(negativeControlsShared) |>
  # Add module: cohort generation
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  # Add module: cohort diagnostics
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  # Add module: cohort method analysis
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Export Analysis Specifications to JSON Format -----------------------------------
# Save the complete specification for use with Strategus execution engine

ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ceeamos", "ceeamosAnalysisSpecification.json")
)