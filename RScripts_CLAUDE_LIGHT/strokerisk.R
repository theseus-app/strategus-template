################################################################################
# CohortMethod Analysis Specification: strokerisk
# 
# This script creates a Strategus analysis specification for a stroke risk
# comparative effectiveness study using CohortMethod.
#
# Shared Resources:
# - Target Cohort ID: 1794126 (target1)
# - Comparator Cohort ID: 1794132 (comparator1)
# - Outcome Cohort ID: 1794131 (outcome1)
# - Negative Control Concept Set ID: 1888110 (negative)
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources: Cohort Definitions ----------------------------------------
# Export cohort definitions from ATLAS
# NOTE: Update baseUrl to your ATLAS instance
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions - Export target, comparator, and outcome cohorts
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Renumber cohorts for internal analysis IDs (1, 2, 3)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative Control Outcomes -------------------------------------------------------
# Export negative control concept set and resolve concepts
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
  # Assign negative control cohort IDs starting from 101 (to avoid conflict with cohorts 1-3)
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Validate no duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Data frames for analysis configuration ----------------------------------------

# Outcomes: Define outcome cohorts with clean window period (365 days lookback)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, 
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator definition for CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Study periods: Two distinct time windows for analysis
# Window 1: 2001-01-01 to 2017-12-31
# Window 2: 2001-01-01 to 2015-11-30
studyPeriods <- tibble(
  studyStartDate = c("20010101", "20010101"),
  studyEndDate   = c("20171231", "20151130")
)

# Time-at-Risk (TAR) definitions
# Single TAR: starts at cohort start, ends at cohort end, minimum 1 day at risk
timeAtRisks <- tibble(
  label = c("OnsetToEnd"),
  riskWindowStart = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(0),
  endAnchor = c("cohort end"),
  minDaysAtRisk = c(1)
)

# CohortGeneratorModule -------------------------------------------------------
# Configure module to generate cohorts from definitions
cgModuleSettingsCreator <- CohortGeneratorModule$new()

cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ----------------------------------------------
# Configure diagnostics module to run comprehensive cohort diagnostics
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

# CohortMethodModule Configuration -----------------------------------------------
# Build propensity score configuration list with three PS strategies:
# 1. No matching/stratification
# 2. Match on PS (1:1 matching, 0.05 caliper on PS)
# 3. Match on PS (1:10 matching, 0.2 caliper on standardized logit)

psConfigList <- list()

# Config 1: No PS adjustment (baseline)
psConfigList[[1]] <- list(
  method = "none",
  label = "NoPS",
  matchOnPsArgs = NULL,
  stratifyByPsArgs = NULL
)

# Config 2: 1:1 Match on Propensity Score with 0.05 caliper
psConfigList[[2]] <- list(
  method = "match",
  label = "Match1to1PS",
  matchOnPsArgs = CohortMethod::createMatchOnPsArgs(
    maxRatio = 1,
    caliper = 0.05,
    caliperScale = "propensity score",
    allowReverseMatch = FALSE,
    stratificationColumns = c()
  ),
  stratifyByPsArgs = NULL
)

# Config 3: 1:10 Match on Propensity Score with 0.2 caliper on standardized logit
psConfigList[[3]] <- list(
  method = "match",
  label = "Match1to10LogitPS",
  matchOnPsArgs = CohortMethod::createMatchOnPsArgs(
    maxRatio = 10,
    caliper = 0.2,
    caliperScale = "standardized logit",
    allowReverseMatch = FALSE,
    stratificationColumns = c()
  ),
  stratifyByPsArgs = NULL
)

# Create propensity score modeling arguments
# Uses Laplace prior with 10-fold cross-validation
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
    noiseLevel = "quiet",
    cvType = "auto",
    seed = 1,
    resetCoefficients = TRUE,
    tolerance = 2e-07,
    cvRepetitions = 10,
    startingVariance = 0.01
  )
)

# Build analysis configurations combining study periods, TARs, and PS strategies
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {
    
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Covariate settings: Default covariates with descendants excluded
      # Empty conceptsToInclude and conceptsToExclude (from spec)
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build outcome list combining positive outcomes and negative controls
      # Positive outcomes: outcomeOfInterest = TRUE
      outcomeList <- append(
        # Positive outcomes from main outcome cohorts
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        # Negative control outcomes: outcomeOfInterest = FALSE
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Create target-comparator-outcomes list
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = c()
        )
      }

      # Get database cohort method data arguments
      # No cohort size restriction (maxCohortSize = 0)
      # Covariates based on default feature extraction
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Create study population arguments
      # washoutPeriod = 0: no washout period
      # removeDuplicateSubjects = "keep all": retain all subject records
      # removeSubjectsWithPriorOutcome = TRUE: exclude subjects with prior outcome
      # priorOutcomeLookback = 99999: 99999-day lookback for prior outcomes
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
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999
      )

      # Compute covariate balance: shared balance (all covariates)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      # Compute covariate balance: analysis-specific balance (Table 1 specs)
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model fitting arguments
      # Cox proportional hazards model
      # Stratified: TRUE (stratified by matching strata if applicable)
      # useCovariates: FALSE (do not adjust for covariates in outcome model)
      # inversePtWeighting: FALSE (do not use inverse probability weighting)
      # Laplace prior with cross-validation
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
          cvRepetitions = 10,
          noiseLevel = "quiet"
        )
      )

      # Create CohortMethod analysis combining all arguments
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
        matchOnPsArgs = psCfg$matchOnPsArgs,
        stratifyByPsArgs = psCfg$stratifyByPsArgs,
        computeSharedCovariateBalanceArgs = computeSharedCovariateBalanceArgs,
        computeCovariateBalanceArgs = computeCovariateBalanceArgs,
        fitOutcomeModelArgs = fitOutcomeModelArgs
      )
      
      analysisId <- analysisId + 1
    }
  }
}

# Create CohortMethod Module Specifications
# Total analyses: 2 study periods × 1 TAR × 3 PS configs = 6 analyses
cmModuleSettingsCreator <- CohortMethodModule$new()

cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the analysis specifications ----
# Combines all module specifications into a single analysis specification document
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() %>%
  Strategus::addSharedResources(cohortDefinitionShared) %>%
  Strategus::addSharedResources(negativeControlsShared) %>%
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save analysis specifications to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "strokerisk", "strokeriskAnalysisSpecification.json")
)

print("Analysis specification 'strokerisk' created successfully!")