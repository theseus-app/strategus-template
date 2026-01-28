################################################################################
# Create Strategus Analysis Specification for Cystectomy Trimodality Study
# 
# This script creates analysis specifications for a comparative effectiveness
# study comparing two cystectomy treatment approaches using the OHDSI Strategus
# framework. The study uses CohortMethod for propensity score matching and
# Cox proportional hazards models for outcome analysis.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Define Atlas WebAPI connection for cohort retrieval
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions - Using exact IDs from specifications
# Note: In production, replace with actual Atlas instance URL
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Negative control outcomes - Using exact concept set ID from specifications
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
  mutate(cohortId = row_number() + 100) %>% # Start negative control IDs at 101
  select(cohortId, cohortName, outcomeConceptId)

# Verify no duplicate cohort IDs between main and negative control cohorts
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data structures for analysis configurations ---------------------------

# Outcomes data frame for CohortMethod
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 1794131) %>% # Outcome cohort ID
  mutate(outcomeCohortId = cohortId, 
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Standard clean window for outcomes

# Target and Comparator pairs for CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1794126,
  targetCohortName = "target1",
  comparatorCohortId = 1794132,
  comparatorCohortName = "comparator1"
)

# No specific covariate exclusions specified in analysis settings
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# Note: covariateSelection in specifications has empty conceptsToInclude and
# conceptsToExclude, so we use default covariate settings

# Study periods from specifications - single period from 2005-01-01 to 2017-12-31
studyPeriods <- tibble(
  studyStartDate = "20050101",
  studyEndDate   = "20171231"
)

# Time-at-risks (TARs) from specifications - single TAR from day 1 to end of follow-up
timeAtRisks <- tibble(
  label = "TAR_1_to_99999",
  riskWindowStart  = 1,
  startAnchor = "cohort start",
  riskWindowEnd  = 99999,
  endAnchor = "cohort start",
  minDaysAtRisk = 1
)

# Propensity Score matching settings from specifications - four configurations
matchOnPsArgsList <- tibble(
  label = c("MaxRatio3", "MaxRatio1", "MaxRatio2", "MaxRatio4"),
  maxRatio  = c(3, 1, 2, 4),
  caliper = c(0.2, 0.2, 0.2, 0.2),
  caliperScale  = c("standardized logit", "standardized logit", 
                    "standardized logit", "standardized logit")
)

# Note: stratifyByPsArgs is null in all psSettings, so no stratification configurations

# Build propensity score configuration list
psConfigList <- list()

# Convert matchOnPsArgsList to configuration objects
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

# CohortGeneratorModule Settings -----------------------------------------------
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

# CohortDiagnosticsModule Settings ---------------------------------------------
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

# CohortMethodModule Settings --------------------------------------------------

# Initialize analysis list and ID counter
cmAnalysisList <- list()
analysisId <- 1

# Iterate through all analysis setting combinations
# Note: Only one study period and one TAR, but four PS configurations
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]
  
  for (t in seq_len(nrow(timeAtRisks))) {
    
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Create propensity score adjustment arguments based on configuration
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      }
      # Note: No stratification configurations in this study
      
      # Covariate settings - using default settings as no specific inclusions/exclusions
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )
      
      # Create outcome list including both primary and negative control outcomes
      outcomeList <- append(
        # Primary outcome
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999  # From specifications
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
      
      # Create target-comparator-outcomes structure
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }
      
      # GetDbCohortMethodData arguments from specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,  # From specifications
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,  # From specifications: 0 means no limit
        covariateSettings = covariateSettings,
        firstExposureOnly = FALSE,  # From specifications
        washoutPeriod = 0,  # From specifications
        removeDuplicateSubjects = "keep all"  # From specifications
      )
      
      # CreatePs arguments from specifications
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # From specifications
        errorOnHighCorrelation = TRUE,  # From specifications
        stopOnError = FALSE,  # Allow Strategus to complete all CM operations
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",  # From specifications
          exclude = c(0),
          useCrossValidation = TRUE  # From specifications
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",  # From specifications
          cvType = "auto",  # From specifications
          seed = 1,
          resetCoefficients = TRUE,  # From specifications
          tolerance = 2e-07,  # From specifications
          cvRepetitions = 10,  # From specifications
          startingVariance = 0.01,  # From specifications
          fold = 10  # From specifications
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
      
      # FitOutcomeModel arguments from specifications
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",  # From specifications
        stratified = TRUE,  # From specifications
        useCovariates = TRUE,  # From specifications
        inversePtWeighting = FALSE,  # From specifications
        prior = Cyclops::createPrior(
          priorType = "laplace",  # From specifications
          useCrossValidation = TRUE  # From specifications
        ),
        control = Cyclops::createControl(
          cvType = "auto",  # From specifications
          seed = 1,
          resetCoefficients = TRUE,  # From specifications
          startingVariance = 0.01,  # From specifications
          tolerance = 2e-07,  # From specifications
          cvRepetitions = 10,  # From specifications
          noiseLevel = "quiet",  # From specifications
          fold = 10  # From specifications
        )
      )
      
      # CreateStudyPop arguments from specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,  # From specifications
        firstExposureOnly = FALSE,  # From specifications
        washoutPeriod = 0,  # From specifications
        removeDuplicateSubjects = "keep all",  # From specifications
        censorAtNewRiskWindow = FALSE,  # From specifications
        removeSubjectsWithPriorOutcome = TRUE,  # From specifications
        priorOutcomeLookback = 99999,  # From specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999
      )
      
      # Append the settings to Analysis List
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
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save specifications to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "cystectomytrimodality", "cystectomytrimodalityAnalysisSpecification.json")
)