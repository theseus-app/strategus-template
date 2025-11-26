################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates a Strategus analysis specification for the "ceeamos" study
# using the OHDSI HADES modules: CohortGenerator, CohortDiagnostics, and CohortMethod.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# ##############################################################################

library(dplyr)
library(Strategus)

# Shared Resources: Cohort Definitions ----------------------------------------
# These cohort definitions will be downloaded from ATLAS and used throughout
# the analysis. The cohort IDs are renumbered for easier reference:
# - targetCohort (id: 1794126) -> cohortId: 1
# - comparatorCohort (id: 1794132) -> cohortId: 2
# - outcomeCohort (id: 1794131) -> cohortId: 3

baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Export cohort definitions from ATLAS
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Renumber cohorts for consistency
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Shared Resources: Negative Control Outcomes ---------------------------------
# Negative control outcomes are derived from a concept set (id: 1888110)
# These will be used to evaluate systematic bias in the analysis.
# Each concept becomes its own negative control outcome cohort.

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
  # Assign cohort IDs starting at 101 (to avoid conflict with primary cohorts)
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Validate: ensure no duplicate cohort IDs
if (any(cohortDefinitionSet$cohortId %in% negativeControlOutcomeCohortSet$cohortId)) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Data Frame: Outcomes --------------------------------------------------------
# Define the outcomes of interest from the cohort definitions
# outcomeCohortId: 3 (outcome1)
# cleanWindow: lookback period for prior outcome assessment

oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Data Frame: Target and Comparator -------------------------------------------
# Define the target (id: 1) and comparator (id: 2) cohort pairs for analysis

cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Data Frame: Excluded Covariate Concepts -------------------------------------
# Concepts to exclude from covariate selection (typically drug exposures being compared)
# Currently empty; can be populated with specific concept IDs to exclude

excludedCovariateConcepts <- data.frame(
  conceptId = numeric(0),
  conceptName = character(0)
)

# CohortGeneratorModule -------------------------------------------------------
# This module generates the cohorts defined above

cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specification for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

# Create shared resource specification for negative control outcomes
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications for CohortGenerator
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings -------------------------------------------
# This module runs diagnostics on the generated cohorts

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

# CohortMethodModule ----------------------------------------------------------
# This module performs comparative effectiveness analysis using propensity score methods

# Study Period Definition
# If empty strings are provided, the study will use all available data
# Format: YYYYMMDD

studyPeriods <- tibble(
  studyStartDate = c(""),  # Empty string = no start date restriction
  studyEndDate = c("")     # Empty string = no end date restriction
)

# Time-at-Risk (TAR) Definitions
# Define the observation windows for outcome ascertainment
# Two TARs are specified:
# TAR 1: From 1 day after cohort start to cohort end
# TAR 2: From 1 day after cohort start to 99,999 days after cohort start

timeAtRisks <- tibble(
  label = c("cohort start to cohort end", "cohort start + 99999 days"),
  riskWindowStart = c(1, 1),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd = c(0, 99999),
  endAnchor = c("cohort end", "cohort start"),
  minDaysAtRisk = c(1, 1)
)

# Propensity Score (PS) Configuration: Match on PS
# Settings for matching target and comparator subjects based on propensity score
# maxRatio: maximum ratio of comparators to targets (1:10 matching)
# caliper: maximum PS difference allowed (0.2 on standardized logit scale)
# caliperScale: scale for expressing the caliper

matchOnPsArgsList <- tibble(
  label = c("1:10 match, 0.2 caliper"),
  maxRatio = c(10),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# Propensity Score Configuration: Stratify by PS
# Currently not used (NULL if no stratification configurations)

stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata = numeric(0),
  baseSelection = character(0)
)

# Build PS Configuration List
# Iterate through PS adjustment methods and create a configuration for each

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

# Build CohortMethod Analysis List
# Create all combinations of: study period, TAR, and PS configuration

cmAnalysisList <- list()
analysisId <- 1

# Iterate through study periods
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- if (studyPeriods$studyStartDate[s] == "") NA else studyPeriods$studyStartDate[s]
  studyEndDate <- if (studyPeriods$studyEndDate[s] == "") NA else studyPeriods$studyEndDate[s]

  # Iterate through time-at-risks
  for (t in seq_len(nrow(timeAtRisks))) {

    # Iterate through PS configurations
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create PS adjustment arguments based on method type
      if (psCfg$method == "match") {
        # Match on PS: create matching parameters
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # Stratify by PS: create stratification parameters
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate Settings
      # Use default covariates (all diagnoses, procedures, measurements, drugs, etc.)
      # addDescendantsToExclude = TRUE includes descendant concepts when excluding covariates

      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcome List Construction
      # Combine outcomes of interest and negative control outcomes

      # First: create outcome objects for outcomes of interest
      outcomeList <- append(
        # Outcomes of interest from oList
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        # Negative control outcomes (assumed true effect size = 1, i.e., no effect)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Target-Comparator-Outcomes List Construction
      # For each target-comparator pair, specify which outcomes to study
      # and which covariates to exclude

      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude the target and comparator concept IDs from covariates
          excludedCovariateConceptIds = c(
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # Get Database Cohort Method Data Arguments
      # Settings for extracting the cohort data from the database
      # maxCohortSize = 0: no size limit
      # restrictToCommonPeriod = FALSE: allow different follow-up periods

      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Create Propensity Score Arguments
      # Settings for fitting the propensity score model
      # prior = Laplace prior with cross-validation
      # maxCohortSizeForFitting = 250000: limit PS model fitting to this cohort size
      # errorOnHighCorrelation = TRUE: stop if covariates are highly correlated

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

      # Compute Shared Covariate Balance Arguments
      # Compute balance on all covariates before adjustment

      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      # Compute Covariate Balance Arguments
      # Compute balance on Table 1 covariates after adjustment

      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit Outcome Model Arguments
      # Settings for fitting the outcome model after PS adjustment
      # modelType = "cox": Cox proportional hazards regression
      # stratified = TRUE: stratified analysis (matched pairs)
      # useCovariates = FALSE: do not adjust for covariates in outcome model
      # inversePtWeighting = FALSE: do not use inverse probability of treatment weighting

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

      # Create Study Population Arguments
      # Settings for creating the analysis population from the cohorts
      # removeSubjectsWithPriorOutcome = TRUE: exclude subjects with outcome before TAR
      # priorOutcomeLookback = 99999: lookback period for prior outcome assessment
      # riskWindowStart/End: define the time-at-risk window

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
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t]
      )

      # Add Analysis to List
      # Create a complete analysis specification combining all settings

      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study Period: %s to %s | TAR: %s | PS Method: %s",
          if (is.na(studyStartDate)) "All data" else studyStartDate,
          if (is.na(studyEndDate)) "All data" else studyEndDate,
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

# Create CohortMethod Module Specifications
# Combine all analyses into a single module specification

cmModuleSettingsCreator <- CohortMethodModule$new()

cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the Complete Analysis Specifications -----------------------------------
# Combine all module specifications into a single analysis specification object
# This is the final specification that will be executed by Strategus

analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the Analysis Specifications -------------------------------------------
# Export the analysis specifications to a JSON file for use with Strategus

ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ceeamos", "ceeamosAnalysisSpecification.json")
)

# Create output directory if it doesn't exist
if (!dir.exists(file.path("inst", "ceeamos"))) {
  dir.create(file.path("inst", "ceeamos"), recursive = TRUE)
}

# Save the analysis specifications
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ceeamos", "ceeamosAnalysisSpecification.json")
)