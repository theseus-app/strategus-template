################################################################################
# OHDSI Strategus Analysis Specification for uveitissafety Study
# 
# This script creates a complete analysis specification including:
# - Cohort generation (target, comparator, outcome cohorts)
# - Negative control outcomes
# - Cohort diagnostics
# - Cohort method comparative effectiveness analysis
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Set the base URL for your ATLAS instance
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions from ATLAS using the cohort IDs specified in the analysis
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
# Target cohort: 1794126 -> 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# Comparator cohort: 1794132 -> 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# Outcome cohort: 1794131 -> 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Extract negative control concepts from the concept set and create negative control cohorts
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Concept set ID for negative controls
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

# Check for duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to organize cohorts for analysis -------------------------

# Outcomes: Define the outcome cohort(s) of interest
# cohortId 3 corresponds to outcome1 (original ID: 1794131)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Clean window for outcome washout

# Target and Comparator for the CohortMethod analysis
# cohortId 1 = target1 (original ID: 1794126)
# cohortId 2 = comparator1 (original ID: 1794132)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate exclusion/inclusion settings
# According to specifications: conceptsToInclude and conceptsToExclude both have null IDs
# Therefore, we create empty data frames (no specific concepts to exclude/include beyond defaults)
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(0),
  conceptName = character(0)
)

# Optional: If specific concepts to include were specified, they would be defined here
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# This module generates the cohorts defined above in the target database
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Use first occurrence of negative control outcome
  detectOnDescendants = TRUE # Include descendant concepts
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Generate cohort statistics
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# Run comprehensive diagnostics on all generated cohorts
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

# Study period settings
# studyStartDate and studyEndDate are empty in specifications, so no restriction
studyPeriods <- tibble(
  studyStartDate = c(""), # Empty = no start date restriction
  studyEndDate   = c("")  # Empty = no end date restriction
)

# Time-at-risk (TAR) windows for outcome assessment
# Two TAR configurations from specifications:
# 1. On treatment: start at day 1 after cohort start, end at cohort end
# 2. Intent to treat: start at day 1 after cohort start, end at 99999 days after cohort start
timeAtRisks <- tibble(
  label = c("OnTreatment", "IntentToTreat"),
  riskWindowStart  = c(1, 1),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd  = c(0, 99999),
  endAnchor = c("cohort end", "cohort start")
) 

# Propensity Score settings - match on PS
# Two matching configurations specified:
# 1. Variable ratio matching (maxRatio=10) with caliper=0.2 on standardized logit scale
# 2. 1:1 matching (maxRatio=1) with caliper=0.2 on standardized logit scale
matchOnPsArgsList <- tibble(
  label = c("VarRatio10", "OneToOne"),
  maxRatio  = c(10, 1),
  caliper = c(0.2, 0.2),
  caliperScale  = c("standardized logit", "standardized logit")
) 

# Propensity Score settings - stratify by PS
# No stratification specified in the analysis specifications
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = numeric(0),
  baseSelection = character(0)
) 

# Build a unified PS configuration list
# Each entry contains: method type, label, and parameters
psConfigList <- list()

# Convert matchOnPsArgsList rows to PS configuration entries
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

# Convert stratifyByPsArgsList rows to PS configuration entries
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

# Generate all combinations of analysis settings
# Nested loops create analyses for each combination of:
# - Study period
# - Time-at-risk window
# - PS adjustment method
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment based on method type
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

      # Covariate settings: use default LSPS covariates
      # addDescendantsToExclude ensures descendant concepts of excluded concepts are also excluded
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build outcome list combining outcomes of interest and negative controls
      outcomeList <- append(
        # Outcomes of interest (outcomeOfInterest = TRUE)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Unknown true effect size
            priorOutcomeLookback = 99999 # Look back all available time for prior outcomes
          )
        }),
        # Negative control outcomes (outcomeOfInterest = FALSE, trueEffectSize = 1)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # Assume null effect for negative controls
          )
        })
      )
      
      # Create target-comparator-outcomes combinations
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude target and comparator concepts from covariates
          # No additional excluded concepts specified in this analysis
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Settings for extracting cohort method data from database
      # restrictToCommonPeriod = TRUE: only include time when both cohorts have data
      # maxCohortSize = 0: no limit on cohort size
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Propensity score model settings
      # Uses Laplace prior with cross-validation for regularization
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,
        estimator = "att", # Average treatment effect on the treated
        prior = Cyclops::createPrior(
          priorType = "laplace",
          exclude = c(0),
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",
          cvType = "auto",
          fold = 10, # 10-fold cross-validation
          cvRepetitions = 10, # 10 repetitions for CV
          seed = 1,
          resetCoefficients = TRUE,
          tolerance = 2e-07,
          startingVariance = 0.01
        )
      )

      # Settings for computing covariate balance (all covariates)
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL # Compute balance for all covariates
      )
      
      # Settings for computing covariate balance (Table 1 covariates only)
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model settings
      # Cox proportional hazards model, stratified by matched sets
      # Uses Laplace prior with cross-validation for regularization
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = TRUE, # Stratify by matched sets or strata
        useCovariates = FALSE, # Do not use covariates in outcome model (only PS adjustment)
        inversePtWeighting = FALSE,
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          seed = 1,
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          tolerance = 2e-07,
          noiseLevel = "quiet"
        )
      )
      
      # Study population settings
      # restrictToCommonPeriod = FALSE: allow different observation periods (overriding getDbCohortMethodDataArgs setting for study pop)
      # firstExposureOnly = FALSE: include all exposures
      # washoutPeriod = 0: no washout period required
      # removeDuplicateSubjects = "keep all": keep all exposure episodes
      # censorAtNewRiskWindow = FALSE: do not censor when new risk window starts
      # removeSubjectsWithPriorOutcome = TRUE: exclude subjects with outcome before exposure
      # priorOutcomeLookBack = 99999: look back all available time for prior outcomes
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
        minDaysAtRisk = 1, # Require at least 1 day at risk
        maxDaysAtRisk = 99999 # No maximum days at risk
      )

      # Combine all settings into a CohortMethod analysis
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

# Create CohortMethod module specifications
# refitPsForEveryOutcome = FALSE: use same PS model for all outcomes
# refitPsForEveryStudyPopulation = FALSE: use same PS model for all study populations
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Run all analyses
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Use default diagnostic thresholds
)

# Create the complete analysis specifications ---------------------------------
# Combine all modules into a single analysis specification
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save analysis specifications to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "uveitissafety", "uveitissafetyAnalysisSpecification.json")
)