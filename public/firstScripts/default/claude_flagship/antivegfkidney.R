################################################################################
# OHDSI Strategus Analysis Specification for antivegfkidney Study
# 
# This script creates the analysis specifications for a comparative cohort study
# using the Strategus framework and HADES modules.
#
# Study Design:
# - Target: cohort ID 1794126 (target1)
# - Comparator: cohort ID 1794132 (comparator1)
# - Outcome: cohort ID 1794131 (outcome1)
# - Negative Controls: concept set ID 1888110 (negative)
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Set the base URL for your ATLAS instance
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export cohort definitions from ATLAS using the original cohort IDs specified
# in the analysis specifications:
# - 1794126: target1 (Target cohort)
# - 1794132: comparator1 (Comparator cohort)
# - 1794131: outcome1 (Outcome cohort)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to sequential IDs (1, 2, 3) for internal processing
# This standardization simplifies downstream analysis configuration
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative Control Outcomes ----------------------------------------------------
# Retrieve negative control outcomes from the concept set defined in ATLAS
# Negative controls are used to calibrate effect size estimates and assess
# residual confounding. Concept set ID: 1888110 (negative)
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
  # Start negative control cohort IDs at 101 to avoid conflicts with primary cohorts (1-3)
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: Ensure no duplicate cohort IDs exist between primary and negative control cohorts
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create Analysis Configuration Data Frames -----------------------------------

# Outcomes List: Define the outcome of interest (cohort ID 3)
# cleanWindow is set to 365 days (not used in current specification but available for reference)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator Configuration for CohortMethod Analysis
# Target cohort ID: 1 (target1)
# Comparator cohort ID: 2 (comparator1)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Excluded Covariate Concepts --------------------------------------------------
# The analysis specifications indicate no specific concepts to exclude (both id and name are null)
# Creating an empty data frame as a placeholder. If specific concepts need to be excluded,
# add them to this data frame with their conceptId and conceptName
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# Included Covariate Concepts (Optional) ---------------------------------------
# The analysis specifications indicate no specific concepts to include (both id and name are null)
# Since no specific inclusion concepts are defined, we will use the default covariate settings
# which include all available covariates. If specific concepts need to be included,
# uncomment and populate this data frame:
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# This module generates the cohorts defined above in the target CDM database
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource for primary cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Shared resource for negative control outcome cohorts
# occurrenceType = "first": Only the first occurrence of the outcome is counted
# detectOnDescendants = TRUE: Include descendant concepts in outcome detection
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications for cohort generation with statistics generation enabled
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule ------------------------------------------------------
# This module performs comprehensive diagnostics on all generated cohorts
# to assess cohort quality and characteristics
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
# This module performs the comparative cohort analysis

# Study Period Configuration ---------------------------------------------------
# Analysis Specification: studyStartDate = "", studyEndDate = ""
# Empty strings indicate no restriction on study period
# Creating an empty tibble as no specific study period is defined
studyPeriods <- tibble(
  studyStartDate = character(0),
  studyEndDate = character(0)
)

# If no study periods are defined, create a default entry with empty strings
if (nrow(studyPeriods) == 0) {
  studyPeriods <- tibble(
    studyStartDate = "",
    studyEndDate = ""
  )
}

# Time-at-Risk (TAR) Configurations --------------------------------------------
# Analysis Specification defines two time-at-risk windows:
# 
# TAR 1: On-treatment period
#   - Start: 1 day after cohort start
#   - End: At cohort end (0 days offset)
#   - Captures outcomes during active exposure
#
# TAR 2: Intent-to-treat period  
#   - Start: 1 day after cohort start
#   - End: 99999 days after cohort start (essentially unlimited follow-up)
#   - Captures all outcomes regardless of treatment discontinuation
timeAtRisks <- tibble(
  label = c("On-treatment", "Intent-to-treat"),
  riskWindowStart = c(1, 1),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd = c(0, 99999),
  endAnchor = c("cohort end", "cohort start")
)

# Propensity Score Adjustment Configuration ------------------------------------
# Analysis Specification: matchOnPsArgs with maxRatio=1, caliper=0.2, 
# caliperScale="standardized logit"
# stratifyByPsArgs is null (no stratification)

# Match on Propensity Score Configuration
# - maxRatio = 1: 1:1 matching (each target patient matched to 1 comparator)
# - caliper = 0.2: Maximum allowed difference in PS for matching
# - caliperScale = "standardized logit": Caliper measured on standardized logit scale
matchOnPsArgsList <- tibble(
  label = c("Match 1:1 with caliper 0.2"),
  maxRatio = c(1),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# Stratify by Propensity Score Configuration
# Analysis Specification indicates stratifyByPsArgs is null, so creating empty tibble
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata = numeric(0),
  baseSelection = character(0)
)

# Build Unified PS Configuration List ------------------------------------------
# This list combines all PS adjustment methods (matching and stratification)
# into a single structure for iterating through analysis combinations
psConfigList <- list()

# Process "match on PS" configurations
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

# Process "stratify by PS" configurations
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

# Generate All Analysis Combinations -------------------------------------------
# Iterate through all combinations of:
# - Study periods (if multiple defined)
# - Time-at-risk windows (2 defined above)
# - PS adjustment methods (1 matching method defined above)
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
      # Analysis Specification: conceptsToInclude and conceptsToExclude are both null
      # Using default covariate settings which includes a comprehensive set of
      # baseline characteristics (demographics, conditions, drugs, procedures, etc.)
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcome List Configuration -----------------------------------------------
      # Create outcome objects for both the outcome of interest and negative controls
      # 
      # Analysis Specification: priorOutcomeLookBack = 99999
      # This setting is applied to the outcome of interest to remove subjects with
      # prior outcomes (essentially unlimited lookback)
      outcomeList <- append(
        # Outcome of interest: outcome1 (cohort ID 3)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        # Negative control outcomes (cohort IDs 101+)
        # trueEffectSize = 1 indicates we expect no true causal effect
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )
      
      # Target-Comparator-Outcomes Configuration ---------------------------------
      # Define which cohorts to compare and which outcomes to analyze
      # Also specify concepts to exclude from covariate construction
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Data Extraction Arguments ------------------------------------------------
      # Analysis Specification settings:
      # - maxCohortSize = 0 (no size limit)
      # - restrictToCommonPeriod = true (restrict to overlapping observation periods)
      # - studyStartDate = "" (no start restriction)
      # - studyEndDate = "" (no end restriction)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Propensity Score Model Arguments -----------------------------------------
      # Analysis Specification settings for createPsArgs:
      # - maxCohortSizeForFitting = 250000
      # - errorOnHighCorrelation = true
      # - prior.priorType = "laplace" (L1 regularization)
      # - prior.useCrossValidation = true
      # - control.tolerance = 2e-7
      # - control.cvType = "auto"
      # - control.fold = 10 (for cross-validation)
      # - control.cvRepetitions = 10
      # - control.noiseLevel = "silent"
      # - control.resetCoefficients = true
      # - control.startingVariance = 0.01
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
          startingVariance = 0.01,
          fold = 10
        )
      )

      # Covariate Balance Arguments ----------------------------------------------
      # Compute covariate balance for diagnostics
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome Model Arguments --------------------------------------------------
      # Analysis Specification settings for fitOutcomeModelArgs:
      # - modelType = "cox" (Cox proportional hazards model)
      # - stratified = false (not stratified on matched sets)
      # - useCovariates = false (no additional covariate adjustment)
      # - inversePtWeighting = false (no inverse probability of treatment weighting)
      # - prior.priorType = "laplace" (L1 regularization)
      # - prior.useCrossValidation = true
      # - control.tolerance = 2e-7
      # - control.cvType = "auto"
      # - control.fold = 10
      # - control.cvRepetitions = 10
      # - control.noiseLevel = "quiet"
      # - control.resetCoefficients = true
      # - control.startingVariance = 0.01
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
          noiseLevel = "quiet",
          fold = 10
        )
      )
      
      # Study Population Arguments -----------------------------------------------
      # Analysis Specification settings for createStudyPopArgs:
      # - restrictToCommonPeriod = false
      # - firstExposureOnly = false (include all exposures)
      # - washoutPeriod = 0 (no washout required)
      # - removeDuplicateSubjects = "keep all"
      # - censorAtNewRiskWindow = false
      # - removeSubjectsWithPriorOutcome = true
      # - priorOutcomeLookBack = 99999 (essentially unlimited lookback)
      # - minDaysAtRisk = 1
      #
      # Time-at-risk settings are applied from the current iteration (timeAtRisks row t)
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
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Create Complete Analysis Specification -----------------------------------
      # Combine all arguments into a single analysis object with descriptive label
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(studyStartDate == "", "No restriction", studyStartDate),
          ifelse(studyEndDate == "", "No restriction", studyEndDate),
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

# CohortMethod Module Specifications -------------------------------------------
# Create the module specifications that will execute all defined analyses
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create Complete Analysis Specifications -------------------------------------
# Combine all modules and shared resources into a single analysis specification object
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save Analysis Specifications to JSON ----------------------------------------
# Save the complete analysis specifications to a JSON file for execution
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "antivegfkidney", "antivegfkidneyAnalysisSpecification.json")
)