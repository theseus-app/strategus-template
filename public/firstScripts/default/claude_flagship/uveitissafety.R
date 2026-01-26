################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates the analysis specifications for the "uveitissafety" study
# using the OHDSI Strategus package.
# 
# Study Overview:
# - Target Cohort: target1 (ID: 1794126)
# - Comparator Cohort: comparator1 (ID: 1794132)
# - Outcome Cohort: outcome1 (ID: 1794131)
# - Negative Control Concept Set: negative (ID: 1888110)
# 
# Analysis Settings:
# - Two time-at-risk windows defined
# - Two propensity score matching configurations (1:10 and 1:1 matching)
# - Cox proportional hazards model for outcome fitting
# - Subjects with prior outcomes are removed
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
################################################################################

library(dplyr)
library(Strategus)

# ==============================================================================
# Shared Resources
# ==============================================================================
# Define the base URL for the OHDSI WebAPI to retrieve cohort definitions
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ------------------------------------------------------------------------------
# Cohort Definitions
# ------------------------------------------------------------------------------
# Export cohort definitions from ATLAS using the specified cohort IDs
# These cohorts are defined in the Analysis Specifications:
# - Target: target1 (1794126)
# - Comparator: comparator1 (1794132)
# - Outcome: outcome1 (1794131)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to use simpler sequential IDs for internal processing
# This makes it easier to reference cohorts throughout the analysis
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1  # Target
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2  # Comparator
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3  # Outcome

# ------------------------------------------------------------------------------
# Negative Control Outcomes
# ------------------------------------------------------------------------------
# Retrieve the negative control concept set from ATLAS
# Negative controls are used to detect potential systematic bias in the study
# Concept Set ID: 1888110 (name: "negative")
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,  # negative
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
  # Assign cohort IDs starting at 101 to avoid conflicts with main cohorts
  # Target/comparator cohort IDs are 1, 2, 3... negative controls -> 101, 102, 103...
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Verify no duplicate cohort IDs exist between main cohorts and negative controls
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# ------------------------------------------------------------------------------
# Define Cohort Lists for Analysis
# ------------------------------------------------------------------------------
# Outcomes list: Contains the outcome cohort(s) for the study
# cleanWindow: The time window (in days) used to define a "clean" period
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%  # Filter to outcome cohort only
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator pairs for the CohortMethod analysis
# This defines which cohorts will be compared in the comparative effectiveness analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# ------------------------------------------------------------------------------
# Covariate Exclusions
# ------------------------------------------------------------------------------
# For the CohortMethod large-scale propensity score (LSPS), we need to exclude
# the drugs of interest to avoid including them as covariates
# Note: conceptsToExclude in specifications has null id, so no specific exclusions defined
# This is a placeholder for any concepts that should be excluded from covariate analysis
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# ==============================================================================
# CohortGeneratorModule Settings
# ==============================================================================
# The CohortGenerator module is responsible for creating the cohorts in the CDM
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource specifications for negative control outcome cohorts
# occurrenceType = "first": Use only the first occurrence of each negative control
# detectOnDescendants = TRUE: Include descendant concepts when identifying outcomes
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications with statistics generation enabled
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ==============================================================================
# CohortDiagnosticsModule Settings
# ==============================================================================
# The CohortDiagnostics module provides comprehensive diagnostics for cohort definitions
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE,           # Statistics on inclusion rule impact
  runIncludedSourceConcepts = TRUE,        # Source concepts included in cohort
  runOrphanConcepts = TRUE,                # Concepts that may be missing from definition
  runTimeSeries = FALSE,                   # Time series of cohort entry
  runVisitContext = TRUE,                  # Visit context at cohort entry
  runBreakdownIndexEvents = TRUE,          # Breakdown of index events
  runIncidenceRate = TRUE,                 # Incidence rate calculations
  runCohortRelationship = TRUE,            # Relationships between cohorts
  runTemporalCohortCharacterization = TRUE, # Temporal characterization
  minCharacterizationMean = 0.01           # Minimum mean for characterization features
)

# ==============================================================================
# CohortMethodModule Settings
# ==============================================================================

# ------------------------------------------------------------------------------
# Study Periods
# ------------------------------------------------------------------------------
# Define the study time windows
# From specifications: studyStartDate and studyEndDate are empty strings,
# meaning no restriction on study period
# restrictToCommonPeriod = TRUE will be used to ensure overlap between cohorts
studyPeriods <- tibble(
  studyStartDate = c(""),  # No start date restriction (empty string from specs)
  studyEndDate = c("")     # No end date restriction (empty string from specs)
)

# ------------------------------------------------------------------------------
# Time-at-Risk Windows
# ------------------------------------------------------------------------------
# Define the time-at-risk (TAR) windows for outcome assessment
# From specifications, two TAR windows are defined:
# 1. From day 1 after cohort start to cohort end (on-treatment)
# 2. From day 1 after cohort start to day 99999 after cohort start (intent-to-treat)
timeAtRisks <- tibble(
  label = c("On-treatment", "Intent-to-treat"),
  riskWindowStart = c(1, 1),                    # Both start at day 1

  startAnchor = c("cohort start", "cohort start"), # Both anchored to cohort start
  riskWindowEnd = c(0, 99999),                  # End at cohort end vs day 99999

  endAnchor = c("cohort end", "cohort start"),  # Different end anchors
  minDaysAtRisk = c(1, 1)                       # Minimum 1 day at risk for both
)

# ------------------------------------------------------------------------------
# Propensity Score Settings - Match on PS
# ------------------------------------------------------------------------------
# Define propensity score matching configurations
# From specifications, two matching configurations are defined:
# 1. Variable ratio matching (up to 1:10)
# 2. Fixed ratio matching (1:1)
# Both use caliper = 0.2 on standardized logit scale
matchOnPsArgsList <- tibble(
  label = c("Variable ratio 1:10", "Fixed ratio 1:1"),
  maxRatio = c(10, 1),                          # Maximum matching ratio
  caliper = c(0.2, 0.2),                        # Caliper width
  caliperScale = c("standardized logit", "standardized logit")  # Scale for caliper
)

# ------------------------------------------------------------------------------
# Propensity Score Settings - Stratify by PS
# ------------------------------------------------------------------------------
# No stratification settings defined in specifications (stratifyByPsArgs = null)
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata = integer(0),
  baseSelection = character(0)
)

# ------------------------------------------------------------------------------
# Build PS Configuration List
# ------------------------------------------------------------------------------
# Combine all PS adjustment methods into a single configuration list
psConfigList <- list()

# Add matching configurations if defined
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

# Add stratification configurations if defined
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

# ------------------------------------------------------------------------------
# Build Analysis List
# ------------------------------------------------------------------------------
# Iterate through all combinations of study periods, time-at-risk windows,
# and PS adjustment methods to create the full analysis list
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]
  
  for (t in seq_len(nrow(timeAtRisks))) {
    
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment method based on type (match or stratify)
      if (psCfg$method == "match") {
        # Create matching arguments from specifications
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
      
      # ----------------------------------------------------------------------
      # Covariate Settings
      # ----------------------------------------------------------------------
      # Use default covariate settings with descendant exclusion
      # Note: conceptsToInclude and conceptsToExclude are null in specifications
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )
      
      # ----------------------------------------------------------------------
      # Outcome List
      # ----------------------------------------------------------------------
      # Create outcome objects for both outcomes of interest and negative controls
      outcomeList <- append(
        # Outcomes of interest (from oList)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,  # Unknown true effect
            priorOutcomeLookback = 99999  # From specifications: priorOutcomeLookBack = 99999
          )
        }),
        # Negative control outcomes (true effect size = 1, i.e., no effect expected)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1  # Negative controls should have no effect
          )
        })
      )
      
      # ----------------------------------------------------------------------
      # Target-Comparator-Outcomes List
      # ----------------------------------------------------------------------
      # Create the target-comparator-outcomes combinations
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }
      
      # ----------------------------------------------------------------------
      # Get Database Cohort Method Data Arguments
      # ----------------------------------------------------------------------
      # Configure how to extract data from the CDM
      # From specifications:
      # - maxCohortSize = 0 (no limit)
      # - restrictToCommonPeriod = TRUE
      # - firstExposureOnly = FALSE
      # - washoutPeriod = 0
      # - removeDuplicateSubjects = "keep all"
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,  # From specifications
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,  # From specifications: no limit
        covariateSettings = covariateSettings
      )
      
      # ----------------------------------------------------------------------
      # Create Propensity Score Arguments
      # ----------------------------------------------------------------------
      # Configure propensity score model fitting
      # From specifications:
      # - maxCohortSizeForFitting = 250000
      # - errorOnHighCorrelation = TRUE
      # - prior: laplace with cross-validation
      # - control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # From specifications
        errorOnHighCorrelation = TRUE,      # From specifications
        stopOnError = FALSE,  # Allow Strategus to complete all operations even if model fitting fails
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",           # From specifications
          exclude = c(0),
          useCrossValidation = TRUE        # From specifications
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",           # From specifications
          cvType = "auto",                 # From specifications
          seed = 1,
          resetCoefficients = TRUE,        # From specifications
          tolerance = 2e-07,               # From specifications
          cvRepetitions = 10,              # From specifications
          fold = 10,                       # From specifications
          startingVariance = 0.01          # From specifications
        )
      )
      
      # ----------------------------------------------------------------------
      # Covariate Balance Arguments
      # ----------------------------------------------------------------------
      # Configure covariate balance computation for diagnostics
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )
      
      # ----------------------------------------------------------------------
      # Fit Outcome Model Arguments
      # ----------------------------------------------------------------------
      # Configure the outcome model fitting
      # From specifications:
      # - modelType = "cox"
      # - stratified = TRUE
      # - useCovariates = FALSE
      # - inversePtWeighting = FALSE
      # - prior: laplace with cross-validation
      # - control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10, noiseLevel = "quiet"
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",                 # From specifications
        stratified = TRUE,                 # From specifications
        useCovariates = FALSE,             # From specifications
        inversePtWeighting = FALSE,        # From specifications
        prior = Cyclops::createPrior(
          priorType = "laplace",           # From specifications
          useCrossValidation = TRUE        # From specifications
        ),
        control = Cyclops::createControl(
          cvType = "auto",                 # From specifications
          seed = 1,
          resetCoefficients = TRUE,        # From specifications
          startingVariance = 0.01,         # From specifications
          tolerance = 2e-07,               # From specifications
          cvRepetitions = 10,              # From specifications
          fold = 10,                       # From specifications
          noiseLevel = "quiet"             # From specifications
        )
      )
      
      # ----------------------------------------------------------------------
      # Create Study Population Arguments
      # ----------------------------------------------------------------------
      # Configure how to create the study population
      # From specifications:
      # - restrictToCommonPeriod = FALSE
      # - firstExposureOnly = FALSE
      # - washoutPeriod = 0
      # - removeDuplicateSubjects = "keep all"
      # - censorAtNewRiskWindow = FALSE
      # - removeSubjectsWithPriorOutcome = TRUE
      # - priorOutcomeLookBack = 99999
      # - minDaysAtRisk = 1
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,           # From specifications
        firstExposureOnly = FALSE,                # From specifications
        washoutPeriod = 0,                        # From specifications
        removeDuplicateSubjects = "keep all",     # From specifications
        censorAtNewRiskWindow = FALSE,            # From specifications
        removeSubjectsWithPriorOutcome = TRUE,    # From specifications
        priorOutcomeLookback = 99999,             # From specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999
      )
      
      # ----------------------------------------------------------------------
      # Create CM Analysis Object
      # ----------------------------------------------------------------------
      # Combine all settings into a single analysis specification
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(studyStartDate == "", "NoStart", studyStartDate),
          ifelse(studyEndDate == "", "NoEnd", studyEndDate),
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

# ------------------------------------------------------------------------------
# Create CohortMethod Module Specifications
# ------------------------------------------------------------------------------
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# ==============================================================================
# Create the Analysis Specifications
# ==============================================================================
# Combine all module specifications into a single analysis specification object
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# ==============================================================================
# Save the Analysis Specifications
# ==============================================================================
# Save the complete analysis specifications to a JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "uveitissafety", "uveitissafetyAnalysisSpecification.json")
)