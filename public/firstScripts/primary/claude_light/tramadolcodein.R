################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates a comprehensive analysis specification for the 
# "tramadolcodein" study using the OHDSI Strategus package.
# 
# The analysis includes:
# - Cohort Generation
# - Cohort Diagnostics
# - Cohort Method analysis with propensity score matching
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
# ##############################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from WebAPI

baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions for:
# - Target cohort (ID: 1794126)
# - Comparator cohort (ID: 1794132)
# - Outcome cohort (ID: 1794131)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal use
# Target cohort -> ID 1, Comparator cohort -> ID 2, Outcome cohort -> ID 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Import the negative control concept set (ID: 1888110) and convert to cohort format
# Negative control cohorts will be assigned IDs starting from 101
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
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Verify no duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to organize cohorts for analysis -------------------------

# Outcome cohorts for the study
# priorOutcomeLookBack: 365 days - subjects with prior outcome within 365 days will be excluded
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator cohort pairs for CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  targetConceptId = NA,
  comparatorConceptId = NA,
  stringsAsFactors = FALSE
)

# Excluded covariate concepts (empty in this specification)
# These would be drug concepts to exclude from propensity score model
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c(),
  stringsAsFactors = FALSE
)

# CohortGeneratorModule --------------------------------------------------------
# Configure cohort generation module specifications

cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for main cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

# Create shared resource specifications for negative control outcomes
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications for cohort generation
# generateStats = TRUE: Generate inclusion rule statistics
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# Configure cohort diagnostics module specifications

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
# Configure cohort method analysis specifications

# Time-at-risk (TAR) definition
# riskWindowStart: 1 day after cohort start
# riskWindowEnd: 0 days after cohort end (at cohort end)
# minDaysAtRisk: 1 day minimum observation period
timeAtRisks <- tibble(
  label = c("On treatment"),
  riskWindowStart = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(0),
  endAnchor = c("cohort end"),
  minDaysAtRisk = c(1)
)

# Propensity Score matching configuration
# maxRatio: 1:1 matching
# caliper: 0.2 on standardized logit scale
# caliperScale: "standardized logit" - caliper expressed on logit scale
matchOnPsArgsList <- tibble(
  label = c("1:1 matching, caliper 0.2"),
  maxRatio = c(1),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# Build propensity score configuration list
# Each configuration specifies the PS adjustment method (match or stratify) and its parameters
psConfigList <- list()

# Convert matchOnPsArgsList rows to PS configuration list entries
if (nrow(matchOnPsArgsList) > 0) {
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

# Build CohortMethod analysis list
# Iterate through all combinations of TARs and PS configurations
cmAnalysisList <- list()
analysisId <- 1

for (t in seq_len(nrow(timeAtRisks))) {
  for (p in seq_along(psConfigList)) {
    psCfg <- psConfigList[[p]]
    
    # Create propensity score arguments based on PS method
    if (psCfg$method == "match") {
      # Propensity score matching arguments
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
    
    # Covariate settings for propensity score model
    # Use default covariate settings with descendant concepts excluded
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
          priorOutcomeLookback = 99999
        )
      }),
      # Negative control outcomes (for calibration)
      lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
        CohortMethod::createOutcome(
          outcomeId = i,
          outcomeOfInterest = FALSE,
          trueEffectSize = 1
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
        excludedCovariateConceptIds = c(
          excludedCovariateConcepts$conceptId
        )
      )
    }
    
    # Database data extraction arguments
    # maxCohortSize: 0 means no limit on cohort size
    # No study period restriction (studyStartDate and studyEndDate are empty)
    getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
      restrictToCommonPeriod = FALSE,
      studyStartDate = "",
      studyEndDate = "",
      maxCohortSize = 0,
      covariateSettings = covariateSettings
    )
    
    # Propensity score model creation arguments
    # maxCohortSizeForFitting: 250,000 - limit cohort size for fitting PS model
    # errorOnHighCorrelation: TRUE - stop if high correlation detected
    # prior: Laplace prior with cross-validation
    # control: Cyclops control with 10-fold cross-validation, 10 repetitions
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
    
    # Covariate balance computation (before matching)
    # maxCohortSize: 250,000
    computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
      maxCohortSize = 250000,
      covariateFilter = NULL
    )
    
    # Covariate balance computation (after matching)
    # Uses Table 1 specifications for reporting
    computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
      maxCohortSize = 250000,
      covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
    )
    
    # Outcome model fitting arguments
    # modelType: "cox" - Cox proportional hazards model
    # stratified: FALSE - do not stratify by matched sets
    # useCovariates: FALSE - do not adjust for covariates in outcome model
    # inversePtWeighting: FALSE - do not use inverse probability weighting
    # prior: Laplace prior with cross-validation
    # control: Cyclops control with 10-fold cross-validation, 10 repetitions
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
    
    # Study population creation arguments
    # restrictToCommonPeriod: FALSE - do not restrict to common exposure period
    # firstExposureOnly: FALSE - include all exposures
    # washoutPeriod: 0 days - no washout period required
    # removeDuplicateSubjects: "keep all" - keep all subject records
    # censorAtNewRiskWindow: FALSE - do not censor at new risk windows
    # removeSubjectsWithPriorOutcome: TRUE - exclude subjects with prior outcome
    # priorOutcomeLookback: 365 days - look back 365 days for prior outcomes
    # Time-at-risk: 1 day after cohort start to cohort end, minimum 1 day at risk
    createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
      restrictToCommonPeriod = FALSE,
      firstExposureOnly = FALSE,
      washoutPeriod = 0,
      removeDuplicateSubjects = "keep all",
      censorAtNewRiskWindow = FALSE,
      removeSubjectsWithPriorOutcome = TRUE,
      priorOutcomeLookback = 365,
      riskWindowStart = timeAtRisks$riskWindowStart[t],
      startAnchor = timeAtRisks$startAnchor[t],
      riskWindowEnd = timeAtRisks$riskWindowEnd[t],
      endAnchor = timeAtRisks$endAnchor[t],
      minDaysAtRisk = 1,
      maxDaysAtRisk = 99999
    )
    
    # Append analysis to analysis list
    cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
      analysisId = analysisId,
      description = sprintf(
        "TAR: %s; PS: %s",
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

# Create CohortMethod module specifications
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

# Save analysis specifications to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "tramadolcodein", "tramadolcodeinAnalysisSpecification.json")
)