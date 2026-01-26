################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates the analysis specifications for the "ceeamos" study
# using the OHDSI Strategus package.
# 
# Analysis Overview:
# - Target Cohort: target1 (ID: 1794126)
# - Comparator Cohort: comparator1 (ID: 1794132)
# - Outcome Cohort: outcome1 (ID: 1794131)
# - Negative Control Concept Set: negative (ID: 1888110)
# - Propensity Score Adjustment: Matching (maxRatio=10, caliper=0.2, standardized logit)
# - Outcome Model: Cox proportional hazards, stratified
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
# Export cohort definitions from ATLAS using the specified cohort IDs:
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

# Re-number cohorts to use sequential IDs for easier reference in the analysis
# Target cohort -> ID 1
# Comparator cohort -> ID 2
# Outcome cohort -> ID 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# ------------------------------------------------------------------------------
# Negative Control Outcomes
# ------------------------------------------------------------------------------
# Retrieve the negative control concept set from ATLAS
# Concept Set ID: 1888110 (name: negative)
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

# Verify no duplicate cohort IDs exist between main cohorts and negative controls
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# ==============================================================================
# Define Analysis Cohorts
# ==============================================================================

# ------------------------------------------------------------------------------
# Outcomes List
# ------------------------------------------------------------------------------
# Define the outcome cohort(s) for the analysis
# cleanWindow: 365 days - used for priorOutcomeLookback to remove subjects with prior outcome
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# ------------------------------------------------------------------------------
# Target and Comparator for CohortMethod Analysis
# ------------------------------------------------------------------------------
# Define the target-comparator pair for the comparative effectiveness analysis
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

# Create shared resource for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource for negative control outcome cohorts
# occurrenceType = "first": Use first occurrence of the negative control outcome
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

# ==============================================================================
# CohortMethodModule Settings
# ==============================================================================

# ------------------------------------------------------------------------------
# Study Periods
# ------------------------------------------------------------------------------
# No specific study period restrictions (studyStartDate and studyEndDate are null)
# Using empty strings to indicate no date restrictions
studyPeriods <- tibble(
  studyStartDate = c(""), # No start date restriction
  studyEndDate   = c("")  # No end date restriction
)

# ------------------------------------------------------------------------------
# Time-at-Risk (TAR) Settings
# ------------------------------------------------------------------------------
# Define the time-at-risk window for outcome assessment
# From specifications:
# - riskWindowStart: 1 (day after cohort start)
# - startAnchor: "cohort start"
# - riskWindowEnd: 0 (at cohort end)
# - endAnchor: "cohort end"
# - minDaysAtRisk: 1
timeAtRisks <- tibble(
  label = c("TAR: 1d after start to cohort end"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
)

# ------------------------------------------------------------------------------
# Propensity Score Settings - Match on PS
# ------------------------------------------------------------------------------
# From specifications:
# - maxRatio: 10 (up to 10:1 matching)
# - caliper: 0.2
# - caliperScale: "standardized logit"
matchOnPsArgsList <- tibble(
  label = c("PS Matching (1:10, caliper 0.2 std logit)"),
  maxRatio  = c(10),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# ------------------------------------------------------------------------------
# Propensity Score Settings - Stratify by PS
# ------------------------------------------------------------------------------
# Not used in this analysis (stratifyByPsArgs is null in specifications)
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0)
)

# ------------------------------------------------------------------------------
# Build PS Configuration List
# ------------------------------------------------------------------------------
# Combine matching and stratification settings into a single configuration list
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
# Iterate through all combinations of study periods, time-at-risk windows,
# and propensity score adjustment methods to create analysis specifications

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
        # PS Matching settings from specifications:
        # - maxRatio: 10
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
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # --------------------------------------------------------------------
      # Covariate Settings
      # --------------------------------------------------------------------
      # Use default covariate settings
      # No specific concepts to include or exclude (both are null in specifications)
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # --------------------------------------------------------------------
      # Outcome List
      # --------------------------------------------------------------------
      # Combine outcomes of interest with negative control outcomes
      outcomeList <- append(
        # Outcomes of interest (outcome1)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 365  # From specifications: priorOutcomeLookBack = 365
          )
        }),
        # Negative control outcomes (for empirical calibration)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1  # True effect size is 1 (no effect) for negative controls
          )
        })
      )

      # --------------------------------------------------------------------
      # Target-Comparator-Outcomes List
      # --------------------------------------------------------------------
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # --------------------------------------------------------------------
      # Get Database Cohort Method Data Arguments
      # --------------------------------------------------------------------
      # From specifications:
      # - studyStartDate: null (no restriction)
      # - studyEndDate: null (no restriction)
      # - maxCohortSize: 0 (no limit)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,  # From specifications: restrictToCommonPeriod = false
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,  # From specifications: maxCohortSize = 0
        covariateSettings = covariateSettings
      )

      # --------------------------------------------------------------------
      # Create Propensity Score Arguments
      # --------------------------------------------------------------------
      # From specifications:
      # - maxCohortSizeForFitting: 250000
      # - errorOnHighCorrelation: true
      # - prior: laplace with cross-validation
      # - control: tolerance 2e-7, fold 10, cvRepetitions 10, startingVariance 0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,  # Allow Strategus to complete all operations even if model fitting fails
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",
          exclude = c(0),
          useCrossValidation = TRUE  # From specifications
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",  # From specifications
          cvType = "auto",        # From specifications
          seed = 1,
          resetCoefficients = TRUE,   # From specifications
          tolerance = 2e-07,          # From specifications
          cvRepetitions = 10,         # From specifications
          fold = 10,                  # From specifications
          startingVariance = 0.01     # From specifications
        )
      )

      # --------------------------------------------------------------------
      # Compute Covariate Balance Arguments
      # --------------------------------------------------------------------
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # --------------------------------------------------------------------
      # Fit Outcome Model Arguments
      # --------------------------------------------------------------------
      # From specifications:
      # - modelType: "cox"
      # - stratified: true
      # - useCovariates: false
      # - inversePtWeighting: false
      # - prior: laplace with cross-validation
      # - control: tolerance 2e-7, fold 10, cvRepetitions 10, noiseLevel "quiet"
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",           # From specifications
        stratified = TRUE,           # From specifications
        useCovariates = FALSE,       # From specifications
        inversePtWeighting = FALSE,  # From specifications
        prior = Cyclops::createPrior(
          priorType = "laplace",     # From specifications
          useCrossValidation = TRUE  # From specifications
        ),
        control = Cyclops::createControl(
          cvType = "auto",           # From specifications
          seed = 1,
          resetCoefficients = TRUE,  # From specifications
          startingVariance = 0.01,   # From specifications
          tolerance = 2e-07,         # From specifications
          cvRepetitions = 10,        # From specifications
          fold = 10,                 # From specifications
          noiseLevel = "quiet"       # From specifications
        )
      )

      # --------------------------------------------------------------------
      # Create Study Population Arguments
      # --------------------------------------------------------------------
      # From specifications:
      # - restrictToCommonPeriod: false
      # - firstExposureOnly: false
      # - washoutPeriod: 365
      # - removeDuplicateSubjects: "remove all"
      # - censorAtNewRiskWindow: false
      # - removeSubjectsWithPriorOutcome: true
      # - priorOutcomeLookBack: 365
      # - riskWindowStart: 1, startAnchor: "cohort start"
      # - riskWindowEnd: 0, endAnchor: "cohort end"
      # - minDaysAtRisk: 1
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,           # From specifications
        firstExposureOnly = FALSE,                # From specifications
        washoutPeriod = 365,                      # From specifications
        removeDuplicateSubjects = "remove all",  # From specifications
        censorAtNewRiskWindow = FALSE,            # From specifications
        removeSubjectsWithPriorOutcome = TRUE,   # From specifications
        priorOutcomeLookback = 365,              # From specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t],  # 1
        startAnchor = timeAtRisks$startAnchor[t],          # "cohort start"
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],      # 0
        endAnchor = timeAtRisks$endAnchor[t],              # "cohort end"
        minDaysAtRisk = 1,                       # From specifications
        maxDaysAtRisk = 99999
      )

      # --------------------------------------------------------------------
      # Create CohortMethod Analysis
      # --------------------------------------------------------------------
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
  file.path("inst", "ceeamos", "ceeamosAnalysisSpecification.json")
)