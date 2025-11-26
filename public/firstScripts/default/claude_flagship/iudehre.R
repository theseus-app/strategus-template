################################################################################
# OHDSI Strategus Analysis Specification for Study: iudehre
# 
# This script creates analysis specifications for a cohort method study
# using the OHDSI Strategus framework. The specifications include:
# - Cohort definitions (target, comparator, outcome)
# - Negative control outcomes
# - Cohort diagnostics
# - Cohort method analysis with propensity score adjustment
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Define the base URL for the ATLAS instance
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export cohort definitions from ATLAS using the exact cohort IDs specified
# Target cohort ID: 1794126 (target1)
# Comparator cohort ID: 1794132 (comparator1)
# Outcome cohort ID: 1794131 (outcome1)
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
# This standardization simplifies downstream analysis references
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative Control Outcomes ----------------------------------------------------
# Fetch the negative control outcome concept set from ATLAS
# Concept set ID: 1888110 (negative)
# These are used for empirical calibration to assess residual bias
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
  # Assign cohort IDs starting from 101 to avoid conflicts with main cohorts (1, 2, 3)
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create Analysis Data Frames --------------------------------------------------
# Outcomes: Define the outcome cohort with a clean window
# The outcome cohort is cohort ID 3 (originally 1794131: outcome1)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator Definitions for CohortMethod Analysis
# Target: cohort ID 1 (originally 1794126: target1)
# Comparator: cohort ID 2 (originally 1794132: comparator1)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate Selection ----------------------------------------------------------
# Note: Based on the analysis specifications, no specific concepts are included or excluded
# The conceptsToInclude and conceptsToExclude arrays contain only null/empty entries
# Therefore, we do not define excludedCovariateConcepts or includedCovariateConcepts
# All default covariates will be used in the analysis

# CohortGeneratorModule --------------------------------------------------------
# This module generates the cohorts in the CDM database
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specification for main cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource specification for negative control outcome cohorts
# occurrenceType = "first": only the first occurrence of the outcome is considered
# detectOnDescendants = TRUE: outcomes are detected on descendant concepts as well
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Specify module settings to generate cohort statistics
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule ------------------------------------------------------
# This module performs diagnostic checks on the generated cohorts
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
# This module performs the comparative cohort method analysis

# Study Period Definition ------------------------------------------------------
# Based on getDbCohortMethodDataArgs.studyPeriods
# studyStartDate: "20030101" (January 1, 2003)
# studyEndDate: null (no end date, study runs to end of available data)
studyPeriods <- tibble(
  studyStartDate = c("20030101"),
  studyEndDate   = c("")
)

# Time-at-Risk (TAR) Windows ---------------------------------------------------
# Define the time windows for outcome assessment relative to cohort entry
# Based on createStudyPopArgs.timeAtRisks specifications:
# 
# TAR 1: Days 30 to 5475 after cohort start
#   - Excludes the first 30 days (grace period)
#   - Follows patients for up to 15 years (5475 days)
# 
# TAR 2: Days 365 to 5475 after cohort start
#   - Excludes the first year (365 days)
#   - Follows patients for up to 15 years from day 365
timeAtRisks <- tibble(
  label = c("TAR_30_5475", "TAR_365_5475"),
  riskWindowStart  = c(30, 365),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd  = c(5475, 5475),
  endAnchor = c("cohort start", "cohort start")
)

# Propensity Score Configuration -----------------------------------------------
# Two PS adjustment methods are specified:

# Method 1: Match on Propensity Score
# maxRatio = 1: 1:1 matching
# caliper = 0.2: matches must be within 0.2 standard deviations
# caliperScale = "standardized logit": caliper applied on standardized logit of PS
matchOnPsArgsList <- tibble(
  label = c("PS_Match_1to1_Cal0.2"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# Method 2: Stratify by Propensity Score
# numberOfStrata = 5: divide population into 5 strata by PS
# baseSelection = "all": use all subjects for stratification
stratifyByPsArgsList <- tibble(
  label = c("PS_Stratify_5strata"),
  numberOfStrata  = c(5),
  baseSelection = c("all")
)

# Build PS Configuration List --------------------------------------------------
# Combine both PS methods into a unified configuration structure
psConfigList <- list()

# Add "match on PS" configurations
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

# Add "stratify by PS" configurations
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

# Generate All CohortMethod Analysis Combinations ------------------------------
# This creates a separate analysis for each combination of:
# - Study period
# - Time-at-risk window
# - PS adjustment method

cmAnalysisList <- list()
analysisId <- 1

# Loop through study periods
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  # Loop through time-at-risk windows
  for (t in seq_len(nrow(timeAtRisks))) {

    # Loop through PS adjustment methods
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment based on method type
      if (psCfg$method == "match") {
        # Matching on propensity score
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # Stratifying by propensity score
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate Settings -------------------------------------------------------
      # Use default covariate settings (demographics, conditions, drugs, etc.)
      # addDescendantsToExclude = TRUE: when excluding concepts, also exclude descendants
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcome List -------------------------------------------------------------
      # Combine outcomes of interest with negative control outcomes
      # Outcomes of interest (outcomeOfInterest = TRUE): from oList
      # Negative controls (outcomeOfInterest = FALSE, trueEffectSize = 1): for calibration
      outcomeList <- append(
        # Add the main outcome of interest
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999  # from createStudyPopArgs.priorOutcomeLookBack
          )
        }),
        # Add negative control outcomes
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )
      
      # Target-Comparator-Outcomes Combinations ----------------------------------
      # Create the combination of target, comparator, and outcomes for analysis
      # Note: excludedCovariateConceptIds is not populated since no specific
      # concepts are marked for exclusion in the analysis specifications
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = c()
        )
      }

      # GetDbCohortMethodData Arguments ------------------------------------------
      # Settings for extracting data from the database
      # Based on getDbCohortMethodDataArgs specifications:
      # - restrictToCommonPeriod = FALSE: don't restrict to overlapping observation
      # - studyStartDate = "20030101": start from January 1, 2003
      # - studyEndDate = null: no end date restriction
      # - maxCohortSize = 0: no limit on cohort size
      # - firstExposureOnly handled in createStudyPopArgs
      # - washoutPeriod handled in createStudyPopArgs
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Create Propensity Score Arguments ----------------------------------------
      # Settings for fitting the propensity score model
      # Based on propensityScoreAdjustment.createPsArgs:
      # - maxCohortSizeForFitting = 250000: downsample for PS fitting if needed
      # - errorOnHighCorrelation = TRUE: error if covariates are highly correlated
      # - prior: Laplace (L1 regularization) with cross-validation
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
          cvRepetitions = 10,  # from propensityScoreAdjustment.createPsArgs.control.cvRepetitions
          startingVariance = 0.01,
          fold = 10  # from propensityScoreAdjustment.createPsArgs.control.fold
        )
      )

      # Covariate Balance Arguments ----------------------------------------------
      # Settings for computing covariate balance diagnostics
      # computeSharedCovariateBalance: uses all covariates (covariateFilter = NULL)
      # computeCovariateBalance: uses Table 1 specifications (common covariates)
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit Outcome Model Arguments ----------------------------------------------
      # Settings for fitting the outcome model
      # Based on fitOutcomeModelArgs specifications:
      # - modelType = "cox": Cox proportional hazards model
      # - stratified = TRUE: stratified Cox model (by matched pairs or strata)
      # - useCovariates = FALSE: don't include covariates in outcome model
      # - inversePtWeighting = FALSE: don't use inverse probability of treatment weighting
      # - prior: Laplace regularization with cross-validation
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
          cvRepetitions = 10,  # from fitOutcomeModelArgs.control.cvRepetitions
          noiseLevel = "quiet",
          fold = 10  # from fitOutcomeModelArgs.control.fold
        )
      )
      
      # Create Study Population Arguments ----------------------------------------
      # Settings for creating the study population from cohorts
      # Based on createStudyPopArgs specifications:
      # - restrictToCommonPeriod = FALSE: use full observation period
      # - firstExposureOnly = FALSE: allow multiple exposures per person
      # - washoutPeriod = 0: no washout period required
      # - removeDuplicateSubjects = "keep all": keep all exposure episodes
      # - censorAtNewRiskWindow = FALSE: don't censor at new exposures
      # - removeSubjectsWithPriorOutcome = FALSE: allow subjects with prior outcomes
      # - priorOutcomeLookBack = 99999: lookback window for prior outcomes
      # - riskWindow: defined by the current timeAtRisks iteration
      # - minDaysAtRisk = 1: subjects must have at least 1 day of follow-up
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
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Create CohortMethod Analysis ---------------------------------------------
      # Combine all settings into a single analysis specification
      # Description includes study period, TAR, and PS method for traceability
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          studyStartDate,
          ifelse(studyEndDate == "", "NoEndDate", studyEndDate),
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
# Finalize the CohortMethod module with all analysis combinations
# - cmAnalysisList: all analysis specifications created above
# - targetComparatorOutcomesList: target-comparator-outcome combinations
# - analysesToExclude: none specified
# - refitPsForEveryOutcome = FALSE: reuse PS across outcomes
# - refitPsForEveryStudyPopulation = FALSE: reuse PS across study populations
# - cmDiagnosticThresholds: use default diagnostic thresholds
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create Final Analysis Specifications -----------------------------------------
# Combine all modules and shared resources into a single analysis specification
# The order of operations:
# 1. Add shared resources (cohorts, negative controls)
# 2. Add CohortGenerator module (creates cohorts)
# 3. Add CohortDiagnostics module (runs diagnostics)
# 4. Add CohortMethod module (performs comparative analysis)
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save Analysis Specifications -------------------------------------------------
# Save the complete analysis specification to a JSON file
# This file can be used to execute the study across multiple data sources
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "iudehre", "iudehreAnalysisSpecification.json")
)