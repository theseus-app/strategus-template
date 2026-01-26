################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates analysis specifications for the Strategus package based on
# the provided analysis specifications for study "iudehre".
# 
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# further details.
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
# Export cohort definitions from Atlas using the specified cohort IDs:
# - Target cohort: id 1794126, name "target1"
# - Comparator cohort: id 1794132, name "comparator1"
# - Outcome cohort: id 1794131, name "outcome1"
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to use sequential IDs for internal processing
# This ensures consistent referencing throughout the analysis
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1  # Target cohort
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2  # Comparator cohort
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3  # Outcome cohort

# ------------------------------------------------------------------------------
# Negative Control Outcomes
# ------------------------------------------------------------------------------
# Retrieve negative control concept set from Atlas
# Negative control concept set: id 1888110, name "negative"
# These are used to assess residual confounding and calibrate p-values
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,  # Negative control concept set ID
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
  # Assign cohort IDs starting from 101 to avoid conflicts with target/comparator/outcome cohorts
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Validate that there are no duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# ==============================================================================
# Create Data Frames for Analysis Configuration
# ==============================================================================

# ------------------------------------------------------------------------------
# Outcomes List
# ------------------------------------------------------------------------------
# Define the outcome cohorts for the analysis
# outcome1 (cohort ID 3) is the primary outcome of interest
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)  # Clean window for outcome assessment

# ------------------------------------------------------------------------------
# Target and Comparator Cohorts for CohortMethod Analysis
# ------------------------------------------------------------------------------
# Define the target-comparator pairs for the comparative effectiveness analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# ------------------------------------------------------------------------------
# Excluded Covariate Concepts
# ------------------------------------------------------------------------------
# Note: The analysis specifications indicate no specific concepts to exclude
# (conceptsToExclude has null id and empty name)
# Creating an empty data frame as placeholder
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# Note: The analysis specifications indicate no specific concepts to include
# (conceptsToInclude has null id and empty name)
# This means all default covariates will be used

# ==============================================================================
# CohortGeneratorModule Settings
# ==============================================================================
# The CohortGenerator module is responsible for generating cohorts in the CDM
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource specifications for negative control outcome cohorts
# occurrenceType = "first" means we only consider the first occurrence of each negative control
# detectOnDescendants = TRUE means we also look for descendant concepts
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

# ==============================================================================
# CohortMethodModule Settings
# ==============================================================================

# ------------------------------------------------------------------------------
# Study Periods Configuration
# ------------------------------------------------------------------------------
# Define the study period based on analysis specifications:
# - studyStartDate: "20030101" (January 1, 2003)
# - studyEndDate: null (no end date restriction)
studyPeriods <- tibble(
  studyStartDate = c("20030101"),
  studyEndDate = c("")  # Empty string represents no end date restriction
)

# ------------------------------------------------------------------------------
# Time-at-Risk (TAR) Configuration
# ------------------------------------------------------------------------------
# Define multiple time-at-risk windows as specified:
# TAR 1: Day 30 to Day 5475 from cohort start (approximately 30 days to 15 years)
# TAR 2: Day 365 to Day 5475 from cohort start (approximately 1 year to 15 years)
# Both TARs require minimum 1 day at risk
timeAtRisks <- tibble(
  label = c("TAR_30_5475", "TAR_365_5475"),
  riskWindowStart = c(30, 365),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd = c(5475, 5475),
  endAnchor = c("cohort start", "cohort start"),
  minDaysAtRisk = c(1, 1)
)

# ------------------------------------------------------------------------------
# Propensity Score Settings - Match on PS
# ------------------------------------------------------------------------------
# Configuration for PS matching:
# - maxRatio: 1 (1:1 matching)
# - caliper: 0.2
# - caliperScale: "standardized logit"
matchOnPsArgsList <- tibble(
  label = c("PS_Match_1to1"),
  maxRatio = c(1),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# ------------------------------------------------------------------------------
# Propensity Score Settings - Stratify by PS
# ------------------------------------------------------------------------------
# Configuration for PS stratification:
# - numberOfStrata: 5 (quintiles)
# - baseSelection: "all" (use all subjects for stratification)
stratifyByPsArgsList <- tibble(
  label = c("PS_Stratify_5"),
  numberOfStrata = c(5),
  baseSelection = c("all")
)

# ------------------------------------------------------------------------------
# Build PS Configuration List
# ------------------------------------------------------------------------------
# Combine matching and stratification configurations into a single list
# Each entry contains: method (match/stratify), label, and parameters
psConfigList <- list()

# Add PS matching configurations
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

# Add PS stratification configurations
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
# Build CohortMethod Analysis List
# ------------------------------------------------------------------------------
# Iterate through all combinations of:
# - Study periods (1 period)
# - Time-at-risk windows (2 TARs)
# - PS adjustment methods (2 methods: matching and stratification)
# This creates 1 x 2 x 2 = 4 total analyses
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]
  
  for (t in seq_len(nrow(timeAtRisks))) {
    
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment method based on the current configuration
      if (psCfg$method == "match") {
        # PS Matching configuration
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # PS Stratification configuration
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
      # Use default covariate settings with descendant exclusion enabled
      # No specific concepts to include or exclude based on analysis specifications
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )
      
      # ----------------------------------------------------------------------
      # Outcome List
      # ----------------------------------------------------------------------
      # Create outcome objects for both outcomes of interest and negative controls
      outcomeList <- append(
        # Outcomes of interest (outcome1)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,  # Unknown true effect size
            priorOutcomeLookback = 99999  # From createStudyPopArgs specification
          )
        }),
        # Negative control outcomes (true effect size = 1, i.e., no effect)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1  # Null effect for negative controls
          )
        })
      )
      
      # ----------------------------------------------------------------------
      # Target-Comparator-Outcomes List
      # ----------------------------------------------------------------------
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
      # getDbCohortMethodDataArgs
      # ----------------------------------------------------------------------
      # Configure data extraction settings based on analysis specifications:
      # - studyStartDate: "20030101"
      # - studyEndDate: null (empty)
      # - maxCohortSize: 0 (no limit)
      # - restrictToCommonPeriod: false
      # - firstExposureOnly: true
      # - washoutPeriod: 365 days
      # - removeDuplicateSubjects: "remove all"
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,  # From analysis specifications
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,  # No limit on cohort size
        firstExposureOnly = TRUE,  # Only consider first exposure
        washoutPeriod = 365,  # 365-day washout period
        removeDuplicateSubjects = "remove all",  # Remove all duplicates
        covariateSettings = covariateSettings
      )
      
      # ----------------------------------------------------------------------
      # createPsArgs
      # ----------------------------------------------------------------------
      # Configure propensity score model creation based on analysis specifications:
      # - maxCohortSizeForFitting: 250000
      # - errorOnHighCorrelation: true
      # - prior: Laplace with cross-validation
      # - control: tolerance 2e-7, auto CV, 10 folds, 10 repetitions, silent, reset coefficients, starting variance 0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,  # Allow Strategus to complete all operations even if model fitting fails
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
          cvRepetitions = 10,  # 10 CV repetitions as specified
          fold = 10,  # 10 folds as specified
          startingVariance = 0.01
        )
      )
      
      # ----------------------------------------------------------------------
      # Covariate Balance Arguments
      # ----------------------------------------------------------------------
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )
      
      # ----------------------------------------------------------------------
      # fitOutcomeModelArgs
      # ----------------------------------------------------------------------
      # Configure outcome model fitting based on analysis specifications:
      # - modelType: "cox"
      # - stratified: true
      # - useCovariates: false
      # - inversePtWeighting: false
      # - prior: Laplace with cross-validation
      # - control: tolerance 2e-7, auto CV, 10 folds, 10 repetitions, quiet, reset coefficients, starting variance 0.01
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
          cvRepetitions = 10,  # 10 CV repetitions as specified
          fold = 10,  # 10 folds as specified
          noiseLevel = "quiet"
        )
      )
      
      # ----------------------------------------------------------------------
      # createStudyPopArgs
      # ----------------------------------------------------------------------
      # Configure study population creation based on analysis specifications:
      # - restrictToCommonPeriod: false
      # - firstExposureOnly: false
      # - washoutPeriod: 0
      # - removeDuplicateSubjects: "keep all"
      # - censorAtNewRiskWindow: false
      # - removeSubjectsWithPriorOutcome: false
      # - priorOutcomeLookBack: 99999
      # - Time-at-risk settings from current TAR iteration
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
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999
      )
      
      # ----------------------------------------------------------------------
      # Create and Append CohortMethod Analysis
      # ----------------------------------------------------------------------
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
# Save Analysis Specifications to JSON
# ==============================================================================
# Save the complete analysis specifications to a JSON file for execution
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "iudehre", "iudehreAnalysisSpecification.json")
)