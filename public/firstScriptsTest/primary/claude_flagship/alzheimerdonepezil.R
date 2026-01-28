################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates the analysis specifications for the alzheimerdonepezil study
# using the OHDSI Strategus package.
# 
# Study Design:
# - Target Cohort: target1 (ID: 1794126)
# - Comparator Cohort: comparator1 (ID: 1794132)
# - Outcome Cohort: outcome1 (ID: 1794131)
# - Negative Control Concept Set: negative (ID: 1888110)
# 
# Analysis Settings:
# - Propensity Score Matching with maxRatio=1, caliper=0.2, standardized logit scale
# - Time-at-Risk: Day 1 to Day 180 from cohort start
# - Logistic regression outcome model with stratification
# - Prior outcome removal with 99999 days lookback
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
# - Target cohort (target1): 1794126
# - Comparator cohort (comparator1): 1794132
# - Outcome cohort (outcome1): 1794131
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
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1  # Target
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2  # Comparator
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3  # Outcome

# ------------------------------------------------------------------------------
# Negative Control Outcomes
# ------------------------------------------------------------------------------
# Retrieve negative control outcomes from the concept set definition
# Concept Set ID: 1888110 (negative)
# These are used to assess residual confounding and calibrate p-values
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
  # Assign cohort IDs starting from 101 to avoid conflicts with main cohorts
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Validate that there are no duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# ==============================================================================
# Define Cohort Lists for Analysis
# ==============================================================================

# ------------------------------------------------------------------------------
# Outcomes List
# ------------------------------------------------------------------------------
# Define the outcome cohort(s) for the analysis
# cleanWindow: 365 days - period to look for clean (outcome-free) time
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%  # Filter to outcome cohort only
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# ------------------------------------------------------------------------------
# Target and Comparator List for CohortMethod Analysis
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
# No specific concepts to exclude from covariates as per specifications
# (conceptsToExclude id is null in the specifications)
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# ==============================================================================
# CohortGeneratorModule Settings
# ==============================================================================
# This module generates the cohorts defined above in the target database
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource specifications for negative control outcome cohorts
# occurrenceType = "first": Use only the first occurrence of each negative control outcome
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
# This module runs diagnostic analyses on the generated cohorts
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE,        # Analyze inclusion rule statistics
  runIncludedSourceConcepts = TRUE,     # Identify source concepts in cohorts
  runOrphanConcepts = TRUE,             # Find potentially missing concepts
  runTimeSeries = FALSE,                # Skip time series analysis
  runVisitContext = TRUE,               # Analyze visit context of cohort entries
  runBreakdownIndexEvents = TRUE,       # Break down index events by concept
  runIncidenceRate = TRUE,              # Calculate incidence rates
  runCohortRelationship = TRUE,         # Analyze relationships between cohorts
  runTemporalCohortCharacterization = TRUE,  # Temporal characterization
  minCharacterizationMean = 0.01        # Minimum mean for characterization features
)

# ==============================================================================
# CohortMethodModule Settings
# ==============================================================================

# ------------------------------------------------------------------------------
# Study Periods
# ------------------------------------------------------------------------------
# No specific study period restrictions as per specifications
# (studyStartDate and studyEndDate are null)
studyPeriods <- tibble(
  studyStartDate = c(""),  # Empty string indicates no restriction
  studyEndDate   = c("")   # Empty string indicates no restriction
)

# ------------------------------------------------------------------------------
# Time-at-Risk (TAR) Settings
# ------------------------------------------------------------------------------
# Define the time-at-risk window for outcome assessment
# Based on specifications:
# - riskWindowStart: 1 (day 1 after cohort start)
# - startAnchor: "cohort start"
# - riskWindowEnd: 180 (day 180 after cohort start)
# - endAnchor: "cohort start"
# - minDaysAtRisk: 1
timeAtRisks <- tibble(
  label = c("TAR_1_180_cohort_start"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(180),
  endAnchor = c("cohort start")
)

# ------------------------------------------------------------------------------
# Propensity Score Settings - Match on PS
# ------------------------------------------------------------------------------
# Configure propensity score matching based on specifications:
# - maxRatio: 1 (1:1 matching)
# - caliper: 0.2
# - caliperScale: "standardized logit"
matchOnPsArgsList <- tibble(
  label = c("PS_Match_1to1_caliper0.2"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# ------------------------------------------------------------------------------
# Propensity Score Settings - Stratify by PS
# ------------------------------------------------------------------------------
# No stratification settings as per specifications (stratifyByPsArgs is null)
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
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
      label  = matchOnPsArgsList$label[i],
      params = list(
        maxRatio     = matchOnPsArgsList$maxRatio[i],
        caliper      = matchOnPsArgsList$caliper[i],
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
      label  = stratifyByPsArgsList$label[i],
      params = list(
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection  = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}

# ==============================================================================
# Build CohortMethod Analysis List
# ==============================================================================
# Iterate through all combinations of study periods, time-at-risks, and PS settings
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
        # Propensity Score Matching
        # Settings from specifications:
        # - maxRatio: 1 (1:1 matching)
        # - caliper: 0.2
        # - caliperScale: "standardized logit"
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # Propensity Score Stratification
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
      # No specific concepts to include or exclude as per specifications
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # ----------------------------------------------------------------------
      # Outcome List
      # ----------------------------------------------------------------------
      # Combine outcomes of interest with negative control outcomes
      outcomeList <- append(
        # Outcomes of interest
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,  # Unknown true effect size
            priorOutcomeLookback = 99999  # From specifications: priorOutcomeLookBack
          )
        }),
        # Negative control outcomes (true effect size = 1, i.e., no effect)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1  # Negative controls have no true effect
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
      # Get Database Cohort Method Data Arguments
      # ----------------------------------------------------------------------
      # Settings from specifications:
      # - restrictToCommonPeriod: TRUE (from createStudyPopArgs)
      # - studyStartDate/studyEndDate: null (no restriction)
      # - maxCohortSize: 0 (no limit)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # ----------------------------------------------------------------------
      # Create Propensity Score Arguments
      # ----------------------------------------------------------------------
      # Settings from specifications:
      # - maxCohortSizeForFitting: 250000
      # - errorOnHighCorrelation: TRUE
      # - prior: Laplace with cross-validation
      # - control: tolerance=2e-7, cvType="auto", fold=10, cvRepetitions=10
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,  # Allow Strategus to complete all operations
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

      # ----------------------------------------------------------------------
      # Compute Covariate Balance Arguments
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
      # Fit Outcome Model Arguments
      # ----------------------------------------------------------------------
      # Settings from specifications:
      # - modelType: "logistic"
      # - stratified: TRUE
      # - useCovariates: FALSE
      # - inversePtWeighting: FALSE
      # - prior: Laplace with cross-validation
      # - control: tolerance=2e-7, cvType="auto", fold=10, cvRepetitions=10, noiseLevel="quiet"
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "logistic",
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
          fold = 10,
          noiseLevel = "quiet"
        )
      )

      # ----------------------------------------------------------------------
      # Create Study Population Arguments
      # ----------------------------------------------------------------------
      # Settings from specifications:
      # - restrictToCommonPeriod: TRUE
      # - firstExposureOnly: FALSE
      # - washoutPeriod: 0
      # - removeDuplicateSubjects: "keep all"
      # - censorAtNewRiskWindow: FALSE
      # - removeSubjectsWithPriorOutcome: TRUE
      # - priorOutcomeLookBack: 99999
      # - riskWindowStart: 1, startAnchor: "cohort start"
      # - riskWindowEnd: 180, endAnchor: "cohort start"
      # - minDaysAtRisk: 1
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE,
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

      # ----------------------------------------------------------------------
      # Create CohortMethod Analysis
      # ----------------------------------------------------------------------
      # Combine all settings into a single analysis configuration
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
# Save the Analysis Specifications to JSON
# ==============================================================================
# Save the complete analysis specifications to a JSON file for execution
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "alzheimerdonepezil", "alzheimerdonepezilAnalysisSpecification.json")
)