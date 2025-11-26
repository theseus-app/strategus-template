################################################################################
# Strategus Analysis Specification for ticagrelorclopidogrel Study
# 
# This script creates analysis specifications for a comparative effectiveness
# study comparing ticagrelor (target) vs clopidogrel (comparator) using the
# OHDSI Strategus framework.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Base URL for ATLAS WebAPI - used to retrieve cohort definitions and concept sets
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export cohort definitions from ATLAS using the cohort IDs specified in the
# analysis specifications:
# - 1794126: Target cohort (ticagrelor)
# - 1794132: Comparator cohort (clopidogrel)
# - 1794131: Outcome cohort
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to sequential IDs (1, 2, 3) for internal consistency
# This standardization simplifies cohort referencing throughout the analysis
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative Control Outcomes ----------------------------------------------------
# Retrieve negative control concept set (ID: 1888110) from ATLAS
# Negative controls are outcomes not expected to be associated with the exposure
# and are used to calibrate for residual bias in the study design
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
  # Assign cohort IDs starting at 101 to avoid conflicts with primary cohorts (1-3)
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Validate that there are no duplicate cohort IDs between primary and negative control cohorts
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create Analysis Configuration Data Frames -----------------------------------

# Outcomes Configuration -------------------------------------------------------
# Define the outcome cohort with a 365-day clean window (time before index when
# subjects must not have the outcome to be included)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator Configuration ------------------------------------------
# Define the target-comparator pairing for the CohortMethod analysis
# Target: ticagrelor (cohort ID 1)
# Comparator: clopidogrel (cohort ID 2)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate Exclusions ---------------------------------------------------------
# Note: The analysis specifications indicate conceptsToExclude have null IDs
# In practice, we would exclude the drugs of interest (target and comparator)
# to prevent them from being used as covariates in the propensity score model
# For now, defining an empty exclusion list based on the null values in specs
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# Optional: Covariate Inclusions -----------------------------------------------
# The analysis specifications show conceptsToInclude with null IDs
# This suggests we are not restricting to specific covariates (use all available)
# If specific covariates were needed, they would be defined here:
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# This module generates cohorts based on the definitions retrieved from ATLAS
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource for negative control outcome cohorts
# occurrenceType = "first": only the first occurrence of the outcome is counted
# detectOnDescendants = TRUE: include descendant concepts in outcome detection
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications with statistics generation enabled
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# This module performs diagnostics on the generated cohorts to assess their quality
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
# This module performs the comparative effectiveness analysis using propensity
# score methods and outcome modeling

# Study Period Configuration ---------------------------------------------------
# Define the study period from the analysis specifications
# studyStartDate: November 1, 2011 (20111101)
# studyEndDate: March 31, 2019 (20190331)
studyPeriods <- tibble(
  studyStartDate = c("20111101"),
  studyEndDate   = c("20190331")
)

# Time-at-Risk (TAR) Configuration ---------------------------------------------
# Define when outcomes are attributed to the exposure
# Based on analysis specifications:
# - riskWindowStart: 1 day after cohort start
# - riskWindowEnd: 365 days after cohort start
# - minDaysAtRisk: 1 day minimum follow-up required
timeAtRisks <- tibble(
  label = c("TAR_1_365"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(365),
  endAnchor = c("cohort start")
)

# Propensity Score Settings - Match on PS -------------------------------------
# Define propensity score matching parameters from analysis specifications:
# - maxRatio: 1 (1:1 matching)
# - caliper: 0.2 (maximum allowable difference in PS for a match)
# - caliperScale: "standardized logit" (scale on which caliper is defined)
matchOnPsArgsList <- tibble(
  label = c("PS_Match_1to1_cal0.2"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# Propensity Score Settings - Stratify by PS ----------------------------------
# The analysis specifications use matchOnPsArgs only (stratifyByPsArgs is null)
# Creating empty tibble to maintain template structure
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0)
)

# Build Propensity Score Configuration List ------------------------------------
# This list combines all PS adjustment methods (matching and/or stratification)
# into a unified structure for iteration during analysis creation
psConfigList <- list()

# Convert "match on PS" configurations to list format
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

# Convert "stratify by PS" configurations to list format
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

# Iterate Through All Analysis Setting Combinations ---------------------------
# Create CohortMethod analyses for each combination of:
# - Study period
# - Time-at-risk window
# - Propensity score adjustment method
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment method based on type (match vs stratify)
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
      # Use default covariate settings (demographics, conditions, drugs, procedures, etc.)
      # addDescendantsToExclude: TRUE ensures that when we exclude a concept,
      # all its descendants are also excluded
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcome List Creation ----------------------------------------------------
      # Combine outcomes of interest with negative control outcomes
      # Outcomes of interest: outcomeOfInterest = TRUE, trueEffectSize = NA
      # Negative controls: outcomeOfInterest = FALSE, trueEffectSize = 1 (null effect)
      outcomeList <- append(
        # Primary outcome(s) of interest
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999  # From analysis specs: priorOutcomeLookBack
          )
        }),
        # Negative control outcomes for empirical calibration
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1  # Null effect (HR = 1)
          )
        })
      )
      
      # Target-Comparator-Outcomes List ------------------------------------------
      # Create target-comparator-outcome combinations
      # Exclude target and comparator concepts from covariate construction to
      # prevent exposure from being used in the propensity score model
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Get Database Cohort Method Data Arguments --------------------------------
      # Configure data extraction parameters from analysis specifications:
      # - restrictToCommonPeriod: FALSE (use full follow-up for each subject)
      # - studyStartDate/studyEndDate: limit data to study period
      # - maxCohortSize: 0 (no limit on cohort size)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Create Propensity Score Arguments ----------------------------------------
      # Configure PS model estimation from analysis specifications:
      # - maxCohortSizeForFitting: 250000 (subsample if larger)
      # - errorOnHighCorrelation: TRUE (stop if covariates are highly correlated)
      # - prior: Laplace (LASSO) with cross-validation for regularization
      # - control: convergence and CV settings
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
          cvRepetitions = 10,  # From analysis specs: cvRepetitions
          startingVariance = 0.01,
          fold = 10  # From analysis specs: fold
        )
      )

      # Compute Covariate Balance Arguments --------------------------------------
      # Settings for assessing covariate balance after PS adjustment
      # Shared balance: compute for all covariates (covariateFilter = NULL)
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      
      # Standard balance: compute for Table 1 covariates only
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit Outcome Model Arguments ----------------------------------------------
      # Configure outcome model estimation from analysis specifications:
      # - modelType: "cox" (Cox proportional hazards)
      # - stratified: FALSE (unstratified Cox model, as per specs)
      # - useCovariates: FALSE (no covariate adjustment in outcome model)
      # - inversePtWeighting: FALSE (not using IPTW)
      # - prior: Laplace with cross-validation
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
          cvRepetitions = 10,  # From analysis specs: cvRepetitions
          noiseLevel = "quiet",
          fold = 10  # From analysis specs: fold
        )
      )
      
      # Create Study Population Arguments ----------------------------------------
      # Configure study population creation from analysis specifications:
      # - restrictToCommonPeriod: FALSE (keep all overlapping time)
      # - firstExposureOnly: FALSE (allow multiple exposures per person)
      # - washoutPeriod: 365 days (require 1 year of prior observation)
      # - removeDuplicateSubjects: "keep all" (allow same person in both cohorts)
      # - censorAtNewRiskWindow: FALSE (don't censor at new exposure)
      # - removeSubjectsWithPriorOutcome: TRUE (exclude prevalent cases)
      # - priorOutcomeLookBack: 99999 (look back indefinitely for prior outcome)
      # - Time-at-risk window from timeAtRisks configuration
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 365,
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

      # Create CohortMethod Analysis ---------------------------------------------
      # Append the complete analysis configuration to the analysis list
      # Each analysis has a unique ID and descriptive label
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

# Create CohortMethod Module Specifications ------------------------------------
# Combine all analysis configurations into a single module specification
# - refitPsForEveryOutcome: FALSE (use same PS model for all outcomes)
# - refitPsForEveryStudyPopulation: FALSE (use same PS model for all populations)
# - cmDiagnosticThresholds: use default diagnostic thresholds for quality checks
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the Complete Analysis Specifications ---------------------------------
# Combine all modules into a single analysis specification
# Order of operations:
# 1. Generate cohorts (CohortGeneratorModule)
# 2. Run cohort diagnostics (CohortDiagnosticsModule)
# 3. Run comparative effectiveness analysis (CohortMethodModule)
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save Analysis Specifications to JSON -----------------------------------------
# Save the complete analysis specification to a JSON file for execution
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ticagrelorclopidogrel", "ticagrelorclopidogrelAnalysisSpecification.json")
)