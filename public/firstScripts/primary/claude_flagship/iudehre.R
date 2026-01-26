################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates analysis specifications for the Strategus OHDSI package
# based on the provided analysis specifications for study: iudehre
#
# The script configures:
# - Cohort definitions (target, comparator, outcome)
# - Negative control outcomes
# - Cohort Method analysis settings including:
#   - Study periods
#   - Time-at-risk windows
#   - Propensity score matching settings
#   - Outcome model fitting parameters
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
################################################################################

library(dplyr)
library(Strategus)

# =============================================================================
# Shared Resources
# =============================================================================

# Define the base URL for the OHDSI WebAPI
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# -----------------------------------------------------------------------------
# Cohort Definitions
# -----------------------------------------------------------------------------
# Export cohort definitions from Atlas WebAPI
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

# Re-number cohorts to sequential IDs for internal use
# This ensures consistent referencing throughout the analysis
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# -----------------------------------------------------------------------------
# Negative Control Outcomes
# -----------------------------------------------------------------------------
# Retrieve negative control concept set from Atlas
# Concept Set ID: 1888110 (negative)
# Negative controls are used to detect residual confounding and systematic bias
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
  # Negative control cohort IDs start at 101 to avoid conflicts with main cohorts
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Validate that there are no duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# =============================================================================
# Define Analysis Cohort Lists
# =============================================================================

# -----------------------------------------------------------------------------
# Outcomes List
# -----------------------------------------------------------------------------
# Define outcome cohorts with their clean window period
# Clean window: 365 days (period to look back for prior outcomes)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# -----------------------------------------------------------------------------
# Target and Comparator List for CohortMethod Analysis
# -----------------------------------------------------------------------------
# Define the target-comparator pairs for the comparative effectiveness analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# -----------------------------------------------------------------------------
# Excluded Covariate Concepts
# -----------------------------------------------------------------------------
# No specific concepts to exclude from covariates as per specifications
# (conceptsToExclude id is null in the specifications)
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# =============================================================================
# CohortGeneratorModule Settings
# =============================================================================
# This module generates the cohorts defined above in the target database

cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource specifications for negative control outcome cohorts
# occurrenceType = "first": Use first occurrence of the outcome
# detectOnDescendants = TRUE: Include descendant concepts when detecting outcomes
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications with statistics generation enabled
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# =============================================================================
# CohortDiagnosticsModule Settings
# =============================================================================
# This module runs diagnostic analyses on the cohorts

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

# =============================================================================
# CohortMethodModule Settings
# =============================================================================
# This module performs the comparative cohort analysis using propensity scores

# -----------------------------------------------------------------------------
# Study Periods
# -----------------------------------------------------------------------------
# Define the study observation period
# studyStartDate: 20030101 (January 1, 2003)
# studyEndDate: NULL (no end date restriction)
studyPeriods <- tibble(
  studyStartDate = c("20030101"),
  studyEndDate = c("")
)

# -----------------------------------------------------------------------------
# Time-at-Risk Windows
# -----------------------------------------------------------------------------
# Define when outcomes are counted relative to exposure
# riskWindowStart: 30 days after cohort start
# riskWindowEnd: 5475 days (approximately 15 years) after cohort start
# minDaysAtRisk: 1 day minimum follow-up required
timeAtRisks <- tibble(
  label = c("TAR: 30-5475 days from cohort start"),
  riskWindowStart = c(30),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(5475),
  endAnchor = c("cohort start")
)

# -----------------------------------------------------------------------------
# Propensity Score Settings - Match on PS
# -----------------------------------------------------------------------------
# Configure propensity score matching parameters
# maxRatio: 1 (1:1 matching)
# caliper: 0.2 (maximum allowed difference in PS)
# caliperScale: "standardized logit" (caliper applied on standardized logit scale)
matchOnPsArgsList <- tibble(
  label = c("PS Matching 1:1, caliper 0.2 standardized logit"),
  maxRatio = c(1),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# -----------------------------------------------------------------------------
# Propensity Score Settings - Stratify by PS
# -----------------------------------------------------------------------------
# No stratification settings specified in this analysis
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata = integer(0),
  baseSelection = character(0)
)

# -----------------------------------------------------------------------------
# Build PS Configuration List
# -----------------------------------------------------------------------------
# Combine all PS adjustment methods into a single configuration list
psConfigList <- list()

# Add matching configurations if they exist
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

# Add stratification configurations if they exist
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

# =============================================================================
# Build CohortMethod Analysis List
# =============================================================================
# Iterate through all combinations of study periods, time-at-risk windows,
# and propensity score configurations to create analysis specifications

cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]
  
  for (t in seq_len(nrow(timeAtRisks))) {
    
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # -----------------------------------------------------------------------
      # Configure PS Adjustment Method
      # -----------------------------------------------------------------------
      if (psCfg$method == "match") {
        # Propensity score matching configuration
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # Propensity score stratification configuration
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }
      
      # -----------------------------------------------------------------------
      # Covariate Settings
      # -----------------------------------------------------------------------
      # Use default covariate settings with descendant concepts excluded
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )
      
      # -----------------------------------------------------------------------
      # Outcome List
      # -----------------------------------------------------------------------
      # Combine outcomes of interest with negative control outcomes
      outcomeList <- append(
        # Outcomes of interest (for which we want to estimate effects)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        # Negative control outcomes (true effect size = 1, i.e., no effect)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )
      
      # -----------------------------------------------------------------------
      # Target-Comparator-Outcomes List
      # -----------------------------------------------------------------------
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }
      
      # -----------------------------------------------------------------------
      # Get Database Cohort Method Data Arguments
      # -----------------------------------------------------------------------
      # Configure how to extract data from the database
      # restrictToCommonPeriod: FALSE (as per specifications)
      # maxCohortSize: 0 (no limit on cohort size)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )
      
      # -----------------------------------------------------------------------
      # Create Propensity Score Arguments
      # -----------------------------------------------------------------------
      # Configure the propensity score model fitting
      # maxCohortSizeForFitting: 250000 (maximum subjects for PS model fitting)
      # errorOnHighCorrelation: TRUE (stop if high correlation detected)
      # Prior: Laplace prior with cross-validation for regularization
      # Control: Cyclops control parameters for optimization
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
          fold = 10,
          startingVariance = 0.01
        )
      )
      
      # -----------------------------------------------------------------------
      # Covariate Balance Arguments
      # -----------------------------------------------------------------------
      # Configure covariate balance computation for diagnostics
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )
      
      # -----------------------------------------------------------------------
      # Fit Outcome Model Arguments
      # -----------------------------------------------------------------------
      # Configure the outcome model fitting
      # modelType: "cox" (Cox proportional hazards model)
      # stratified: FALSE (not stratified by PS strata)
      # useCovariates: FALSE (no additional covariates in outcome model)
      # inversePtWeighting: FALSE (not using inverse probability weighting)
      # Prior and Control: Laplace prior with cross-validation
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
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
          fold = 10,
          noiseLevel = "quiet"
        )
      )
      
      # -----------------------------------------------------------------------
      # Create Study Population Arguments
      # -----------------------------------------------------------------------
      # Configure the study population criteria
      # restrictToCommonPeriod: FALSE
      # firstExposureOnly: TRUE (only first exposure per subject)
      # washoutPeriod: 365 days (require 365 days of observation before exposure)
      # removeDuplicateSubjects: "keep all" (keep all subjects)
      # censorAtNewRiskWindow: FALSE
      # removeSubjectsWithPriorOutcome: TRUE (exclude subjects with prior outcome)
      # priorOutcomeLookBack: 99999 days (look back period for prior outcomes)
      # minDaysAtRisk: 1 day minimum follow-up
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = TRUE,
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
      
      # -----------------------------------------------------------------------
      # Create CM Analysis Object
      # -----------------------------------------------------------------------
      # Combine all settings into a single analysis specification
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          studyStartDate,
          ifelse(studyEndDate == "", "ongoing", studyEndDate),
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

# -----------------------------------------------------------------------------
# Create CohortMethod Module Specifications
# -----------------------------------------------------------------------------
cmModuleSettingsCreator <- CohortMethodModule$new()

cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# =============================================================================
# Create the Final Analysis Specifications
# =============================================================================
# Combine all module specifications into a single analysis specification object

analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# =============================================================================
# Save Analysis Specifications to JSON
# =============================================================================
# Save the complete analysis specifications to a JSON file for execution

ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "iudehre", "iudehreAnalysisSpecification.json")
)