################################################################################
# Strategus Analysis Specification for Ranitidine Cancer Study
# 
# This script creates a comprehensive analysis specification for a cohort method
# study examining the relationship between ranitidine exposure and cancer outcomes.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
# ##############################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Configure the base URL for ATLAS WebAPI
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions ----------------------------------------------------------
# Export cohort definitions from ATLAS using exact cohort IDs from specifications
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: ranitidine exposure cohort
    1794132, # Comparator: alternative H2 blocker cohort
    1794131  # Outcome: cancer cohort
  ),
  generateStats = TRUE
)

# Re-number cohorts to sequential IDs for internal analysis (1=target, 2=comparator, 3=outcome)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative Control Outcomes ---------------------------------------------------
# Retrieve negative control concept set to generate negative control outcome cohorts
# These are used to assess the specificity of the analysis (identify false positives)
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
  # Assign negative control cohort IDs starting from 101 to avoid conflicts with main cohorts
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Validation: ensure no duplicate cohort IDs between main and negative control cohorts
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create Analysis Cohort Lists ------------------------------------------------

# Outcomes: Define cancer outcomes of interest with 365-day clean window
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for CohortMethod analysis
# Links ranitidine users (target) to alternative H2 blocker users (comparator)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Excluded Covariate Concepts
# Exclude the exposure drugs themselves from covariate selection to prevent inclusion
# of confounding by indication
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# CohortGeneratorModule -------------------------------------------------------
# Initialize the CohortGenerator module to manage cohort definitions and generation

cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for main cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Create shared resource specifications for negative control outcomes
# Setting detectOnDescendants = TRUE ensures all descendant concepts are captured
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications for CohortGenerator with statistics generation enabled
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings -------------------------------------------
# Initialize diagnostics module to assess cohort quality and validity

cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Create comprehensive diagnostic specifications
# Includes inclusion statistics, orphan concepts, temporal characterization, etc.
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
# Main comparative effectiveness analysis using propensity score methods

# Study Periods: Define the calendar time window for analysis
# Empty values mean the study spans the entire available data period
studyPeriods <- tibble(
  studyStartDate = c(),  # Format: YYYYMMDD (empty = no restriction)
  studyEndDate = c()     # Format: YYYYMMDD (empty = no restriction)
)

# Time-at-Risk (TAR) Windows: Define the follow-up period for outcome occurrence
# Multiple TARs allow flexible outcome timing analysis
# TAR 1: Full exposure period (from day 1 of exposure forward)
# TAR 2: After 365-day stability period (from day 365 of exposure forward)
# TAR 3: Until end of continuous exposure (exposure period duration)
# TAR 4: After 365 days and until end of exposure
timeAtRisks <- tibble(
  label = c(
    "From exposure start",
    "From exposure start + 365 days",
    "Until exposure end",
    "From exposure start + 365 days until exposure end"
  ),
  riskWindowStart = c(1, 365, 1, 365),
  startAnchor = c("cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd = c(99999, 99999, 0, 0),
  endAnchor = c("cohort start", "cohort start", "cohort end", "cohort end"),
  minDaysAtRisk = c(1, 1, 1, 1)
)

# Propensity Score Configuration 1: Match on PS with 1:1 ratio and 0.2 caliper
# Strict matching to ensure well-balanced comparison groups
matchOnPsArgsList_1 <- tibble(
  label = "1:1 matching, caliper = 0.2",
  maxRatio = 1,
  caliper = 0.2,
  caliperScale = "standardized logit"
)

# Propensity Score Configuration 2: Match on PS with 1:10 ratio and 0.2 caliper
# Relaxed matching to improve sample size retention
matchOnPsArgsList_2 <- tibble(
  label = "1:10 matching, caliper = 0.2",
  maxRatio = 10,
  caliper = 0.2,
  caliperScale = "standardized logit"
)

# Propensity Score Configuration 3: Stratification into 10 quintiles
# Non-parametric approach to PS adjustment
stratifyByPsArgsList <- tibble(
  label = "Stratified by PS (10 strata)",
  numberOfStrata = 10,
  baseSelection = "all"
)

# Propensity Score Configuration 4: Inverse probability of treatment weighting
# No explicit PS adjustment object needed; handled in outcome model fitting
inverseWeightingConfig <- tibble(
  label = "IPTW (no explicit PS adjustment)"
)

# Build PS configuration list combining all PS adjustment strategies
psConfigList <- list()

# Add 1:1 PS matching configuration
psConfigList[[length(psConfigList) + 1]] <- list(
  method = "match",
  label = matchOnPsArgsList_1$label[1],
  params = list(
    maxRatio = matchOnPsArgsList_1$maxRatio[1],
    caliper = matchOnPsArgsList_1$caliper[1],
    caliperScale = matchOnPsArgsList_1$caliperScale[1]
  )
)

# Add 1:10 PS matching configuration
psConfigList[[length(psConfigList) + 1]] <- list(
  method = "match",
  label = matchOnPsArgsList_2$label[1],
  params = list(
    maxRatio = matchOnPsArgsList_2$maxRatio[1],
    caliper = matchOnPsArgsList_2$caliper[1],
    caliperScale = matchOnPsArgsList_2$caliperScale[1]
  )
)

# Add PS stratification configuration
psConfigList[[length(psConfigList) + 1]] <- list(
  method = "stratify",
  label = stratifyByPsArgsList$label[1],
  params = list(
    numberOfStrata = stratifyByPsArgsList$numberOfStrata[1],
    baseSelection = stratifyByPsArgsList$baseSelection[1]
  )
)

# Add IPTW configuration (no explicit PS adjustment, handled via outcome model)
psConfigList[[length(psConfigList) + 1]] <- list(
  method = "none",
  label = inverseWeightingConfig$label[1],
  params = list()
)

# Iterate through all analysis setting combinations ----------------------------
# Creates multiple analyses varying study periods, TARs, and PS methods
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {
    
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment arguments based on selected method
      if (psCfg$method == "match") {
        # PS Matching: pairs exposed and unexposed with similar propensity scores
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
        
      } else if (psCfg$method == "stratify") {
        # PS Stratification: divides sample into PS quintiles for comparison within strata
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
        
      } else if (psCfg$method == "none") {
        # IPTW: uses PS weights in outcome model; no explicit PS adjustment here
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- NULL
      }

      # Covariate Settings
      # Use default covariate set: demographics, conditions, drugs, procedures, measurements
      # addDescendantsToExclude = TRUE ensures concept descendants are also excluded
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome list combining positive outcomes and negative controls
      outcomeList <- append(
        # Positive outcomes of interest (cancer)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999  # Full history lookback per specifications
          )
        }),
        # Negative control outcomes (expected null effect size = 1)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1  # Null effect for negative controls
          )
        })
      )

      # Create target-comparator-outcome combinations
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude exposure drugs from covariates to avoid capturing their effect as covariates
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # GetDbCohortMethodData Arguments
      # Retrieves cohorts and covariates from database per specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,  # Per specification: no common period restriction
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,  # Per specification: 0 = no limit
        covariateSettings = covariateSettings
      )

      # Create Propensity Score Arguments
      # Estimates probability of exposure given covariates using Cyclops regularization
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # Per specification
        errorOnHighCorrelation = TRUE,     # Per specification
        stopOnError = FALSE,  # Continue even if PS fitting fails; equipoise checks should flag issues
        estimator = "att",  # Estimate Average Treatment Effect on Treated
        prior = Cyclops::createPrior(
          priorType = "laplace",  # Per specification: Laplace prior for regularization
          exclude = c(0),
          useCrossValidation = TRUE  # Per specification
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",  # Per specification
          cvType = "auto",        # Per specification
          seed = 1,
          resetCoefficients = TRUE,     # Per specification
          tolerance = 2e-7,             # Per specification
          cvRepetitions = 10,           # Per specification
          startingVariance = 0.01       # Per specification
        )
      )

      # Compute Shared Covariate Balance Arguments
      # Used to assess PS model fit and covariate balance before matching/stratification
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL  # Calculate for all covariates
      )

      # Compute Covariate Balance Arguments
      # Focuses on Table 1 (standard baseline characteristics) for reporting
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit Outcome Model Arguments
      # Estimates treatment effect via Cox proportional hazards model
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",           # Per specification: Cox model for time-to-event
        stratified = TRUE,           # Per specification: stratified analysis (matched pairs)
        useCovariates = FALSE,       # Per specification: no covariate adjustment in model
        inversePtWeighting = FALSE,  # Per specification: IPTW handled separately if needed
        prior = Cyclops::createPrior(
          priorType = "laplace",     # Per specification: Laplace prior
          useCrossValidation = TRUE  # Per specification
        ),
        control = Cyclops::createControl(
          cvType = "auto",           # Per specification
          seed = 1,
          resetCoefficients = TRUE,  # Per specification
          startingVariance = 0.01,   # Per specification
          tolerance = 2e-7,          # Per specification
          cvRepetitions = 10,        # Per specification
          noiseLevel = "quiet"       # Per specification
        )
      )

      # Create Study Population Arguments
      # Defines inclusion/exclusion criteria and risk window for each subject
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,        # Per specification
        firstExposureOnly = FALSE,             # Per specification
        washoutPeriod = 0,                     # Per specification
        removeDuplicateSubjects = "keep all",  # Per specification: keep all occurrences
        censorAtNewRiskWindow = FALSE,         # Per specification
        removeSubjectsWithPriorOutcome = TRUE, # Per specification
        priorOutcomeLookback = 99999,          # Per specification: full history
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t]
      )

      # Append analysis configuration to analysis list
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          if (studyStartDate == "") "no restriction" else studyStartDate,
          if (studyEndDate == "") "no restriction" else studyEndDate,
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

# CohortMethod Module Specifications ------------------------------------------
# Packages all CohortMethod analyses and configurations

cmModuleSettingsCreator <- CohortMethodModule$new()

cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,  # Include all analyses
  refitPsForEveryOutcome = FALSE,          # Reuse same PS model across outcomes
  refitPsForEveryStudyPopulation = FALSE,  # Reuse PS model across study populations
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create Comprehensive Analysis Specifications --------------------------------
# Combines all modules: CohortGenerator, Diagnostics, and CohortMethod

analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() %>%
  Strategus::addSharedResources(cohortDefinitionShared) %>%
  Strategus::addSharedResources(negativeControlsShared) %>%
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save Analysis Specifications to JSON ----------------------------------------
# Output file for deployment in Strategus execution framework

ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ranitidinecancer", "ranitidinecancerAnalysisSpecification.json")
)

cat("Analysis specification for 'ranitidinecancer' study created successfully!\n")