################################################################################
# CreateStrategusAnalysisSpecification.R
# Analysis specification for: alzheimerdonepezil
# 
# This script creates a Strategus analysis specification for a comparative
# effectiveness study of target1 vs comparator1 for outcome1.
# Negative control outcomes are defined by concept set 1888110.
#
# Created from JSON specifications dated: [Current Date]
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Define the Atlas WebAPI base URL for cohort retrieval
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"  # Replace with your Atlas instance URL

# Cohort Definitions -----------------------------------------------------------
# Export cohort definitions from Atlas using the IDs specified in the JSON
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126,  # Target cohort: target1
    1794132,  # Comparator cohort: comparator1
    1794131   # Outcome cohort: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to sequential IDs starting from 1 for Strategus compatibility
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative Control Outcomes ----------------------------------------------------
# Retrieve and resolve the negative control concept set (ID: 1888110)
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
  mutate(cohortId = row_number() + 100) %>%  # Reserve IDs 101+ for negative controls
  select(cohortId, cohortName, outcomeConceptId)

# Verify no duplicate cohort IDs exist between main and negative control cohorts
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Analysis Components ----------------------------------------------------------
# Create data frames to hold analysis components as specified in the JSON

# Outcomes for analysis (only outcome1 in this specification)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%  # Outcome cohort ID 3
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)  # Standard 365-day clean window for outcomes

# Target-Comparator pairs for CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,                    # target1
  targetCohortName = "target1",
  comparatorCohortId = 2,                # comparator1
  comparatorCohortName = "comparator1"
)

# No specific drug exclusions specified in JSON (empty arrays for include/exclude)
# excludedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# Create specifications for cohort generation with negative control outcomes
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule ------------------------------------------------------
# Create specifications for cohort diagnostics on all three main cohorts
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
# Study Periods: No restrictions specified (empty start/end dates in JSON)
studyPeriods <- tibble(
  studyStartDate = c(),  # Empty string = no start date restriction
  studyEndDate   = c()   # Empty string = no end date restriction
)

# Time-at-Risks (TARs): Single TAR as specified in JSON (1-180 days from cohort start)
timeAtRisks <- tibble(
  label = c("1-180d"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(180),
  endAnchor = c("cohort start")
)

# Propensity Score Configuration ----------------------------------------------
# Based on JSON: Only matchOnPsArgs specified (1:1 matching with caliper 0.2)
# No stratification specified (stratifyByPsArgs = null)
matchOnPsArgsList <- tibble(
  label = c("1-to-1 matching"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# Build PS configuration list (only matching method in this specification)
psConfigList <- list()
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

# Create Outcome List ----------------------------------------------------------
# Combine main outcome (outcome1) and negative control outcomes
outcomeList <- append(
  lapply(seq_len(nrow(oList)), function(i) {
    CohortMethod::createOutcome(
      outcomeId = oList$outcomeCohortId[i],
      outcomeOfInterest = TRUE,
      trueEffectSize = NA,
      priorOutcomeLookback = 99999  # As specified in JSON
    )
  }),
  lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
    CohortMethod::createOutcome(
      outcomeId = i,
      outcomeOfInterest = FALSE,
      trueEffectSize = 1
    )
  })
)

# Target-Comparator-Outcomes List ---------------------------------------------
# Create a single TCO for the target-comparator pair
targetComparatorOutcomesList <- list()
for (i in seq_len(nrow(cmTcList))) {
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList
    # Note: No drug-specific exclusions specified in JSON
    # excludedCovariateConceptIds = c()
  )
}

# Build CohortMethod Analysis List --------------------------------------------
cmAnalysisList <- list()
analysisId <- 1

# Iterate through all combinations (though only one of each in this specification)
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]
  
  for (t in seq_len(nrow(timeAtRisks))) {
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Set PS adjustment method based on configuration
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else {
        # Not used in this specification
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- NULL
      }
      
      # Covariate settings: Default settings, no specific includes/excludes
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )
      
      # GetDbCohortMethodDataArgs: Restrict to common period as specified
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,           # From JSON: true
        studyStartDate = studyStartDate,         # Empty = no restriction
        studyEndDate = studyEndDate,             # Empty = no restriction
        maxCohortSize = 0,                       # From JSON: 0 = no limit
        covariateSettings = covariateSettings
      )
      
      # CreatePsArgs: Regularized logistic regression with cross-validation
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,        # From JSON
        errorOnHighCorrelation = TRUE,           # From JSON
        stopOnError = FALSE,                     # Allow Strategus to continue
        estimator = "att",
        prior = Cyclops::createPrior(            # Laplace prior with CV
          priorType = "laplace",
          exclude = c(0),
          useCrossValidation = TRUE              # From JSON
        ),
        control = Cyclops::createControl(        # Control settings from JSON
          noiseLevel = "silent",                 # From JSON
          cvType = "auto",                       # From JSON
          seed = 1,
          resetCoefficients = TRUE,              # From JSON
          tolerance = 2e-07,                     # From JSON
          cvRepetitions = 10,                    # From JSON (note: template had 1)
          startingVariance = 0.01                # From JSON
        )
      )
      
      # Covariate balance computation arguments
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )
      
      # FitOutcomeModelArgs: Logistic regression as specified in JSON
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "logistic",                  # From JSON (not cox as in template)
        stratified = TRUE,                       # From JSON
        useCovariates = FALSE,                   # From JSON
        inversePtWeighting = FALSE,              # From JSON
        prior = Cyclops::createPrior(            # Laplace prior with CV
          priorType = "laplace",
          useCrossValidation = TRUE              # From JSON
        ),
        control = Cyclops::createControl(        # Control settings from JSON
          cvType = "auto",                       # From JSON
          seed = 1,
          resetCoefficients = TRUE,              # From JSON
          startingVariance = 0.01,               # From JSON
          tolerance = 2e-07,                     # From JSON
          cvRepetitions = 10,                    # From JSON (note: template had 1)
          noiseLevel = "quiet"                   # From JSON
        )
      )
      
      # CreateStudyPopArgs: Study population creation arguments from JSON
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE,           # From JSON
        firstExposureOnly = FALSE,               # From JSON
        washoutPeriod = 0,                       # From JSON
        removeDuplicateSubjects = "keep all",    # From JSON
        censorAtNewRiskWindow = FALSE,           # From JSON
        removeSubjectsWithPriorOutcome = TRUE,   # From JSON
        priorOutcomeLookback = 99999,            # From JSON
        riskWindowStart = timeAtRisks$riskWindowStart[t],      # 1
        startAnchor = timeAtRisks$startAnchor[t],              # cohort start
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],          # 180
        endAnchor = timeAtRisks$endAnchor[t],                  # cohort start
        minDaysAtRisk = 1,                       # From JSON
        maxDaysAtRisk = 99999
      )
      
      # Append the analysis settings to the list
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

# Create CohortMethod Module Specifications ------------------------------------
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create Final Analysis Specifications -----------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save specifications to JSON file ---------------------------------------------
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "alzheimerdonepezil", "alzheimerdonepezilAnalysisSpecification.json")
)