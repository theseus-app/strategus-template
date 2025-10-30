################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates analysis specifications for the rapidcyclejanssen study
# using the OHDSI Strategus package. It configures cohort generation, 
# diagnostics, and cohort method analyses with propensity score matching.
#
# See the Create analysis specifications section of the UsingThisTemplate.md 
# for more details.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
# ##############################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export target (1794126), comparator (1794132), and outcome (1794131) cohorts
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to sequential IDs for easier reference in analysis
# Target cohort -> ID 1, Comparator -> ID 2, Outcome -> ID 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Import negative control concept set (ID: 1888110, name: "negative")
# This will be used to create negative control outcome cohorts for validation
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
  # Assign negative control cohort IDs starting from 101 (after target/comparator: 1, 2, 3)
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Validate no duplicate cohort IDs exist
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts used in each analysis ----------------

# Outcomes: Extract outcome cohort (ID=3) with outcome lookback window
# priorOutcomeLookBack is set to 99999 days (removeSubjectsWithPriorOutcome uses this)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Maps target cohort ID 1 with comparator cohort ID 2
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Excluded covariate concepts (empty in this specification)
# These would typically include the drugs of interest to avoid post-treatment bias
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Create shared resource specifications for negative control outcomes
# First occurrence, detect on descendants enabled
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Configure cohort generator module to generate statistics
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Configure comprehensive cohort diagnostics
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

# Study periods: Single period from Jan 1, 2021 with no end date (ongoing)
# Study start date: 20210101, study end date: null (no restriction)
studyPeriods <- tibble(
  studyStartDate = c("20210101"),
  studyEndDate   = c("")
)

# Time-at-risk (TAR) windows for outcome occurrence assessment
# Five different TAR configurations:
# 1. Days 1-14 after cohort start
# 2. Days 1-28 after cohort start
# 3. Days 1-42 after cohort start
# 4. Days 1-90 after cohort start
# 5. Days 0-2 after cohort start (immediate)
# All require minimum 1 day at risk
timeAtRisks <- tibble(
  label = c("TAR 1-14", "TAR 1-28", "TAR 1-42", "TAR 1-90", "TAR 0-2"),
  riskWindowStart  = c(1, 1, 1, 1, 0),
  startAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd  = c(14, 28, 42, 90, 2),
  endAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start"),
  minDaysAtRisk = c(1, 1, 1, 1, 1)
)

# Propensity Score settings - match on PS
# Single matching configuration:
# - Maximum ratio: 0 (unlimited matching)
# - Caliper: 0.2 on standardized logit scale
matchOnPsArgsList <- tibble(
  label = c("PS Match 0.2 SL"),
  maxRatio  = c(0),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# Propensity Score settings - stratify by PS (not used in this analysis)
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c()
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Convert match on PS settings to configuration list
# Each row becomes a configuration element with method, label, and parameters
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    # Append a new element at the end of psConfigList
    psConfigList[[length(psConfigList) + 1]] <- list(
      # Identify the PS adjustment method for this config
      method = "match",
      # Human-readable label to carry through into descriptions
      label  = matchOnPsArgsList$label[i],
      # Parameter bundle passed to createMatchOnPsArgs later
      params = list(
        maxRatio     = matchOnPsArgsList$maxRatio[i],
        caliper      = matchOnPsArgsList$caliper[i],
        caliperScale = matchOnPsArgsList$caliperScale[i]
      )
    )
  }
}

# Convert stratify by PS settings to configuration list (if any exist)
if (exists("stratifyByPsArgsList") && nrow(stratifyByPsArgsList) > 0) {
  for (i in seq_len(nrow(stratifyByPsArgsList))) {
    # Append a new element at the end of psConfigList
    psConfigList[[length(psConfigList) + 1]] <- list(
      # Identify the PS adjustment method for this config
      method = "stratify",
      # Human-readable label to carry through into descriptions
      label  = stratifyByPsArgsList$label[i],
      # Parameter bundle passed to createStratifyByPsArgs later
      params = list(
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection  = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}

# Iterate through all analysis setting combinations ----
# Creates one analysis for each combination of:
# - Study period
# - Time-at-risk window
# - Propensity score configuration
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  # Extract study period dates (empty strings treated as NULL in CohortMethod)
  studyStartDate <- if (studyPeriods$studyStartDate[s] == "") NULL else studyPeriods$studyStartDate[s]
  studyEndDate <- if (studyPeriods$studyEndDate[s] == "") NULL else studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {
    # Extract time-at-risk parameters for this configuration
    riskWindowStart <- timeAtRisks$riskWindowStart[t]
    startAnchor <- timeAtRisks$startAnchor[t]
    riskWindowEnd <- timeAtRisks$riskWindowEnd[t]
    endAnchor <- timeAtRisks$endAnchor[t]
    minDaysAtRisk <- timeAtRisks$minDaysAtRisk[t]

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Create propensity score adjustment arguments based on method
      if (psCfg$method == "match") {
        # Match on propensity score configuration
        # maxRatio: 0 = unlimited matching
        # caliper: 0.2 = maximum distance between matched pairs
        # caliperScale: "standardized logit" = scale for caliper measurement
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # Stratify by propensity score configuration
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Default covariate settings
      # Includes all covariates with option to exclude descendants
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome list combining outcomes of interest and negative controls
      # Outcomes of interest: outcome1 (ID=3) with prior lookback of 99999 days
      # Negative controls: all negative control concepts with true effect size = 1
      outcomeList <- append(
        # Outcomes of interest (from oList)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        # Negative control outcomes
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Create target-comparator-outcome combinations
      # Maps target (ID=1) vs comparator (ID=2) with all outcomes
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Create arguments for getting cohort method data
      # Study period: 20210101 - no end date
      # maxCohortSize: 0 (no limit)
      # restrictToCommonPeriod: FALSE (cohorts can have different follow-up periods)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Create propensity score model arguments
      # Regularization: Laplace prior with cross-validation
      # maxCohortSizeForFitting: 250000 (maximum sample for PS model)
      # errorOnHighCorrelation: TRUE (fail if high correlation detected)
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
          cvRepetitions = 10,
          fold = 10,
          startingVariance = 0.01
        )
      )

      # Compute shared covariate balance (before matching/stratification)
      # Used for diagnostics only, maximum cohort size 250000
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      # Compute covariate balance after matching/stratification
      # Restricted to Table 1 specifications for reporting
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit outcome model arguments
      # Model type: Cox proportional hazards
      # stratified: FALSE (do not use stratification from PS matching)
      # useCovariates: FALSE (no covariate adjustment)
      # inversePtWeighting: FALSE (no IPW)
      # Regularization: Laplace prior with cross-validation
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
          cvRepetitions = 10,
          fold = 10,
          noiseLevel = "quiet"
        )
      )

      # Create study population arguments
      # restrictToCommonPeriod: FALSE (allow different follow-up periods)
      # firstExposureOnly: TRUE (only first exposure per subject)
      # washoutPeriod: 365 days (require 1 year prior observation)
      # removeDuplicateSubjects: "remove all" (exclude if in both cohorts)
      # censorAtNewRiskWindow: FALSE
      # removeSubjectsWithPriorOutcome: TRUE (exclude prior outcome)
      # priorOutcomeLookback: 99999 days (entire history)
      # Time-at-risk: configured from timeAtRisks table
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = TRUE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "remove all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = riskWindowStart,
        startAnchor = startAnchor,
        riskWindowEnd = riskWindowEnd,
        endAnchor = endAnchor,
        minDaysAtRisk = minDaysAtRisk,
        maxDaysAtRisk = 99999
      )

      # Append the settings to Analysis List
      # Each analysis is identified by unique ID and description
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          if (is.null(studyStartDate)) "unspecified" else studyStartDate,
          if (is.null(studyEndDate)) "unspecified" else studyEndDate,
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

# Create CohortMethodModule specifications
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the analysis specifications ------------------------------------------
# Combine all module specifications into single analysis specification object
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save analysis specifications to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "rapidcyclejanssen", "rapidcycleja

nssenAnalysisSpecification.json")
)