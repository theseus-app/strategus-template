################################################################################
# Strategus Analysis Specification: Semaglutide and NAION
# 
# This script creates a comprehensive analysis specification for studying
# the association between semaglutide exposure and NAION (non-arteritic 
# anterior ischemic optic neuropathy) outcomes using cohort methodology.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
# ##############################################################################

library(dplyr)
library(Strategus)

# Shared Resources: Cohort Definitions ----------------------------------------
# Retrieve cohort definitions from ATLAS WebAPI
# The analysis uses three main cohorts:
# - Target: semaglutide users (cohortId: 1794126)
# - Comparator: alternative exposure users (cohortId: 1794132)
# - Outcome: NAION cases (cohortId: 1794131)

baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Export cohort definitions with statistics
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: Semaglutide users
    1794132, # Comparator: Alternative exposure users
    1794131  # Outcome: NAION cases
  ),
  generateStats = TRUE
)

# Renumber cohorts to sequential IDs for internal processing
# Target=1, Comparator=2, Outcome=3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Retrieve and process negative control outcomes
# Negative controls help assess false positive rates in the analysis
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
  # Assign cohort IDs starting at 101 to avoid conflicts with primary cohorts
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Validation: ensure no duplicate cohort IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create outcome list for CohortMethod analysis
# Includes primary outcome (NAION) and negative control outcomes
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Create target-comparator pairs for CohortMethod analysis
# Defines the two exposure groups being compared
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# CohortGeneratorModule -------------------------------------------------------
# Generates the cohorts defined above in the analysis database
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resources for primary cohorts
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Create shared resources for negative control outcomes
# Uses first occurrence of concepts and searches descendant concepts
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications for cohort generation with statistics
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings -------------------------------------------
# Performs comprehensive diagnostics on cohort definitions
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

# CohortMethodModule ----------------------------------------------------------
# Performs propensity score-adjusted cohort analysis

# Define study period: December 1, 2017 to December 31, 2023
# Restricts analysis to this temporal window
studyPeriods <- tibble(
  studyStartDate = c(20171201),
  studyEndDate = c(20231231)
)

# Define time-at-risk (TAR) for outcome measurement
# Begins on exposure cohort start date
# Ends on exposure cohort end date
# Requires minimum 1 day of follow-up
timeAtRisks <- tibble(
  label = c("On Treatment"),
  riskWindowStart = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(0),
  endAnchor = c("cohort end")
)

# Define propensity score matching parameters
# Matches exposed to unexposed 1:1 using propensity score
# Caliper of 0.2 on standardized logit scale prevents poor matches
matchOnPsArgsList <- tibble(
  label = c("Match PS"),
  maxRatio = c(1),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# Build propensity score configuration list
# Each entry specifies a PS adjustment method with parameters
psConfigList <- list()

# Convert "match on PS" specifications to configuration format
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

# Iterate through all analysis setting combinations
# Creates separate analysis specifications for each combination
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Configure propensity score matching parameters
      # maxRatio: 1:1 matching
      # caliper: maximum PS difference of 0.2 (standardized logit scale)
      # allowReverseMatch: FALSE means targets must be matched to comparators only
      matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
        maxRatio = psCfg$params$maxRatio,
        caliper = psCfg$params$caliper,
        caliperScale = psCfg$params$caliperScale,
        allowReverseMatch = FALSE,
        stratificationColumns = c()
      )
      stratifyByPsArgs <- NULL

      # Define covariate settings for propensity score model
      # Uses default covariates: demographics, conditions, procedures, drugs
      # addDescendantsToExclude: applies exclusions to concept descendants
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome specifications
      # Primary outcomes: NAION with prior outcome lookback of 99999 days (~274 years)
      # Negative controls: concepts with expected true effect size of 1 (null)
      outcomeList <- append(
        # Primary outcome: NAION
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
      # Defines which outcomes to evaluate for each exposure comparison
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList
        )
      }

      # Get cohort method data arguments
      # Specifies study period (2017-12-01 to 2023-12-31)
      # maxCohortSize: 0 means no size limit
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Propensity score model fitting parameters
      # Uses Laplace prior with cross-validation for regularization
      # maxCohortSizeForFitting: 250,000 subjects max
      # errorOnHighCorrelation: raises error if high correlation detected
      # Cross-validation: 10-fold, 10 repetitions
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

      # Covariate balance computation arguments
      # First computation: all covariates after matching
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      # Second computation: Table 1 specifications after matching
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model fitting parameters
      # modelType: Cox proportional hazards model
      # stratified: FALSE (unstratified analysis)
      # useCovariates: FALSE (only propensity score adjustment, no covariate adjustment)
      # inversePtWeighting: FALSE (not using inverse probability weighting)
      # Prior: Laplace with cross-validation regularization
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

      # Study population definition parameters
      # restrictToCommonPeriod: FALSE (allows staggered cohort entry)
      # firstExposureOnly: FALSE (includes all exposures)
      # washoutPeriod: 365 days (requires 1 year prior observation)
      # removeDuplicateSubjects: "keep all" (keeps all exposure instances)
      # censorAtNewRiskWindow: FALSE (doesn't censor at new exposure)
      # removeSubjectsWithPriorOutcome: TRUE (excludes prevalent outcomes)
      # priorOutcomeLookback: 99999 days (~274 years)
      # Time-at-risk: from cohort start to cohort end, minimum 1 day at risk
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 365,
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

      # Combine all analysis specifications into single analysis
      # Assigns unique analysisId for tracking
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Semaglutide vs Alternative: %s-%s; TAR: %s; PS: %s",
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
# Includes all analysis configurations and target-comparator-outcome combinations
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create comprehensive analysis specifications
# Combines all module specifications into single analysis specification object
# Specifies execution order: CohortGenerator -> CohortDiagnostics -> CohortMethod
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() %>%
  Strategus::addSharedResources(cohortDefinitionShared) %>%
  Strategus::addSharedResources(negativeControlsShared) %>%
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Export analysis specifications to JSON file
# Enables sharing and reproducibility of analysis configuration
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "semaglutideandnaion", "semaglutideandnaionAnalysisSpecification.json")
)