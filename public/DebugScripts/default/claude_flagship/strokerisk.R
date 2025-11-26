################################################################################
# Analysis Specifications for strokerisk Study
# 
# This script creates Strategus analysis specifications based on the provided
# study settings. It includes:
# - Cohort definitions (target, comparator, outcomes)
# - Negative control outcomes
# - CohortGenerator, CohortDiagnostics, and CohortMethod modules
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from ATLAS
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions from ATLAS using the specified cohort IDs
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

# Re-number cohorts to sequential IDs for internal consistency
# Target cohort: 1794126 -> 1
# Comparator cohort: 1794132 -> 2
# Outcome cohort: 1794131 -> 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve negative control concept set (ID: 1888110, name: "negative")
# These are used to assess residual bias in the analysis
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
  mutate(cohortId = row_number() + 100) %>% # Negative control IDs start at 101 to avoid conflict with main cohorts
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts used in each analysis ----------------

# Outcomes: 
# Define the outcome cohort (cohortId = 3) for the analysis
# cleanWindow = 365 days is used for outcome cleaning
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Target cohort ID: 1 (originally 1794126, target1)
# Comparator cohort ID: 2 (originally 1794132, comparator1)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Excluded covariate concepts
# covariateSelection.conceptsToExclude is empty in specifications, so no exclusions
# We include an empty data frame for potential future use
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# CohortGeneratorModule --------------------------------------------------------
# Initialize the CohortGenerator module to create cohorts in the CDM
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Share cohort definitions with the module
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Share negative control outcome cohorts with the module
# occurrenceType = "first" means we look at first occurrence of the outcome
# detectOnDescendants = TRUE means we include descendant concepts
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications for cohort generation
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# Initialize the CohortDiagnostics module to assess cohort quality
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Create module specifications with diagnostic settings
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

# Study periods
# Two study periods are defined in the specifications:
# Period 1: 2001-01-01 to 2017-12-31
# Period 2: 2001-01-01 to 2015-09-30
studyPeriods <- tibble(
  studyStartDate = c("20010101", "20010101"),
  studyEndDate   = c("20171231", "20150930")
)

# Time-at-risk (TAR) settings
# Defines when to start and stop counting outcomes relative to cohort entry
# Settings from createStudyPopArgs.timeAtRisks:
# - riskWindowStart = 1 day after cohort start
# - startAnchor = "cohort start"
# - riskWindowEnd = 0 days after cohort end
# - endAnchor = "cohort end"
# - minDaysAtRisk = 1 day
timeAtRisks <- tibble(
  label = c("OnTreatment"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
)

# Propensity Score settings - match on PS
# Three PS adjustment strategies are defined:
# 1. No matching or stratification (baseline)
# 2. 1:1 matching with caliper = 0.05 on propensity score scale
# 3. 1:10 matching with caliper = 0.2 on standardized logit scale
matchOnPsArgsList <- tibble(
  label = c("1to1_PS005", "1to10_SL02"),
  maxRatio  = c(1, 10),
  caliper = c(0.05, 0.2),
  caliperScale  = c("propensity score", "standardized logit")
)

# Propensity Score settings - stratify by PS
# No stratification settings specified in the analysis specifications
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = numeric(0),
  baseSelection = character(0)
)

# Build a single PS configuration list
# This includes both matching and stratification approaches
# First entry: no PS adjustment (baseline comparison)
# NOTE: For "none" PS adjustment, we must set stratified = FALSE in fitOutcomeModelArgs
psConfigList <- list(
  list(
    method = "none",
    label = "No_PS_adjustment",
    params = list()
  )
)

# Add matching configurations
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

# Add stratification configurations
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

# Iterate through all analysis setting combinations
# This creates a separate analysis for each combination of:
# - Study period (2 options)
# - Time-at-risk window (1 option)
# - PS adjustment strategy (3 options: none, 1:1 match, 1:10 match)
# Total: 2 × 1 × 3 = 6 analyses
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
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
        # When matching, we can use stratified outcome model
        useStratifiedOutcomeModel <- TRUE
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
        # When stratifying, we can use stratified outcome model
        useStratifiedOutcomeModel <- TRUE
      } else {
        # No PS adjustment
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- NULL
        # FIX: When there is no PS adjustment (no matching or stratification),
        # we cannot use a stratified outcome model
        useStratifiedOutcomeModel <- FALSE
      }

      # Covariate settings using default feature extraction
      # covariateSelection.conceptsToInclude and conceptsToExclude are empty
      # so we use all available covariates
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome list combining outcomes of interest and negative controls
      outcomeList <- append(
        # Outcomes of interest
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999 # From specifications: priorOutcomeLookBack = 99999
          )
        }),
        # Negative control outcomes
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # Assumed true effect size for negative controls
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
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Settings for extracting data from the database
      # From getDbCohortMethodDataArgs specifications:
      # - maxCohortSize = 0 (no limit)
      # - restrictToCommonPeriod = FALSE
      # - firstExposureOnly = TRUE (enforced during data extraction)
      # - washoutPeriod = 183 days
      # - removeDuplicateSubjects = "keep first"
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = TRUE,
        washoutPeriod = 183,
        removeDuplicateSubjects = "keep first",
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Propensity score model settings
      # From propensityScoreAdjustment.createPsArgs:
      # - maxCohortSizeForFitting = 250000
      # - errorOnHighCorrelation = TRUE
      # - prior: Laplace with cross-validation
      # - control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10
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
          fold = 10,
          seed = 1,
          resetCoefficients = TRUE,
          tolerance = 2e-07,
          cvRepetitions = 10,
          startingVariance = 0.01
        )
      )

      # Covariate balance computation for all covariates (used for diagnostics)
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      
      # Covariate balance computation for Table 1 covariates (used for reporting)
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model settings
      # From fitOutcomeModelArgs specifications:
      # - modelType = "cox" (Cox proportional hazards model)
      # - stratified = TRUE (stratified by matched pairs/strata) ONLY when PS adjustment is used
      # - useCovariates = FALSE (no covariate adjustment in outcome model)
      # - inversePtWeighting = FALSE
      # - prior: Laplace with cross-validation
      # - control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10, noiseLevel = "quiet"
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = useStratifiedOutcomeModel,
        useCovariates = FALSE,
        inversePtWeighting = FALSE,
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          cvType = "auto",
          fold = 10,
          seed = 1,
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          tolerance = 2e-07,
          cvRepetitions = 10,
          noiseLevel = "quiet"
        )
      )
      
      # Study population settings
      # From createStudyPopArgs specifications:
      # - restrictToCommonPeriod = FALSE
      # - firstExposureOnly = FALSE (already applied in getDbCohortMethodDataArgs)
      # - washoutPeriod = 0 (already applied in getDbCohortMethodDataArgs)
      # - removeDuplicateSubjects = "keep all"
      # - censorAtNewRiskWindow = FALSE
      # - removeSubjectsWithPriorOutcome = FALSE
      # - priorOutcomeLookBack = 99999
      # - Time-at-risk window from timeAtRisks
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

      # Create the analysis specification by combining all settings
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

# Initialize CohortMethod module and create specifications
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
# Combine all module specifications into a single analysis specification object
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "strokerisk", "strokeriskAnalysisSpecification.json")
)