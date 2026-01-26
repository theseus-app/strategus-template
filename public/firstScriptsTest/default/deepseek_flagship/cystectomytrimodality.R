################################################################################
# Create Strategus Analysis Specification for Cystectomy Trimodality Study
# 
# This script creates an analysis specification for the Strategus framework
# based on the provided analysis settings. It sets up cohort generation,
# cohort diagnostics, and cohort method analysis modules with detailed
# configuration matching the study requirements.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# ##############################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Define the base URL for OHDSI WebAPI (Atlas instance)
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"  # Update with your Atlas instance

# Cohort Definitions - Fetch from Atlas using IDs from specifications
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126,  # Target cohort: target1
    1794132,  # Comparator cohort: comparator1  
    1794131   # Outcome cohort: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal use in Strategus analysis
# Target cohort becomes ID 1, comparator becomes ID 2, outcome becomes ID 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Update cohort names for clarity
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1,]$cohortName <- "target1"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 2,]$cohortName <- "comparator1"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 3,]$cohortName <- "outcome1"

# Negative control outcomes - Fetch concept set from Atlas
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,  # Negative control concept set ID
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
  mutate(cohortId = row_number() + 100) %>%  # Start negative control IDs from 101
  select(cohortId, cohortName, outcomeConceptId)

# Validate no duplicate cohort IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames for analysis configuration --------------------------------

# Outcomes for CohortMethod analysis
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%  # Outcome cohort
  mutate(outcomeCohortId = cohortId, 
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)  # Standard 365-day clean window for outcomes

# Target and Comparator pairs for CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Define study periods from specifications (2005-01-01 to 2017-12-31)
studyPeriods <- tibble(
  studyStartDate = c("20050101"),  # Format: YYYYMMDD
  studyEndDate   = c("20171231")   # Format: YYYYMMDD
)

# Define time-at-risk windows from specifications
# Single TAR: Risk window from day 1 after cohort start until end of follow-up
timeAtRisks <- tibble(
  label = c("Day 1 to end of follow-up"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),  # Anchor to cohort start date
  riskWindowEnd  = c(99999),
  endAnchor = c("cohort start"),    # Anchor to cohort start date
  minDaysAtRisk = c(1)              # Minimum 1 day at risk required
)

# Define propensity score matching configurations from specifications
# Four different matching ratios are specified
matchOnPsArgsList <- tibble(
  label = c("1:3 matching", "1:1 matching", "1:2 matching", "1:4 matching"),
  maxRatio  = c(3, 1, 2, 4),  # Maximum matching ratios
  caliper = c(0.2, 0.2, 0.2, 0.2),  # 0.2 caliper for all configurations
  caliperScale  = c("standardized logit", "standardized logit", 
                    "standardized logit", "standardized logit")  # Standardized logit scale
)

# Build propensity score configuration list
psConfigList <- list()

# Convert matchOnPsArgsList to configuration objects
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "match",  # Use matching on propensity score
      label  = matchOnPsArgsList$label[i],  # Descriptive label
      params = list(
        maxRatio     = matchOnPsArgsList$maxRatio[i],
        caliper      = matchOnPsArgsList$caliper[i],
        caliperScale = matchOnPsArgsList$caliperScale[i]
      )
    )
  }
}

# Covariate settings - No specific inclusion/exclusion concepts specified
# Empty data frames for included/excluded concepts (as per specifications)
includedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",  # Use first occurrence of negative control outcomes
  detectOnDescendants = TRUE  # Include descendant concepts
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE  # Generate cohort statistics
)

# CohortDiagnosticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,  # Diagnose all cohorts
  runInclusionStatistics = TRUE,
  runIncludedSourceConcepts = TRUE,
  runOrphanConcepts = TRUE,
  runTimeSeries = FALSE,
  runVisitContext = TRUE,
  runBreakdownIndexEvents = TRUE,
  runIncidenceRate = TRUE,
  runCohortRelationship = TRUE,
  runTemporalCohortCharacterization = TRUE,
  minCharacterizationMean = 0.01  # Minimum mean threshold for characterization
)

# CohortMethodModule -----------------------------------------------------------

# Create outcome list including both primary outcomes and negative controls
outcomeList <- append(
  # Primary outcome(s)
  lapply(seq_len(nrow(oList)), function(i) {
    CohortMethod::createOutcome(
      outcomeId = oList$outcomeCohortId[i],
      outcomeOfInterest = TRUE,  # Primary outcome of interest
      trueEffectSize = NA,  # Unknown true effect size
      priorOutcomeLookback = 99999  # Lookback period for prior outcomes
    )
  }),
  # Negative control outcomes
  lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
    CohortMethod::createOutcome(
      outcomeId = i,
      outcomeOfInterest = FALSE,  # Not primary outcome
      trueEffectSize = 1  # Null effect size for negative controls
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
    excludedCovariateConceptIds = excludedCovariateConcepts$conceptId  # Empty in this study
  )
}

# Initialize analysis list and ID counter
cmAnalysisList <- list()
analysisId <- 1

# Iterate through all analysis setting combinations
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {
    
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Create propensity score arguments based on configuration
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()  # No additional stratification columns
        )
        stratifyByPsArgs <- NULL  # Not using stratification in this study
      }
      
      # Create covariate settings using default settings
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE  # Include descendant concepts for exclusion
      )
      
      # GetDbCohortMethodDataArgs: Data extraction settings
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,  # Restrict to common period across databases
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,  # No maximum cohort size restriction
        covariateSettings = covariateSettings,
        firstExposureOnly = FALSE,  # Include all exposures, not just first
        washoutPeriod = 0,  # No washout period required
        removeDuplicateSubjects = "keep all"  # Keep all duplicate subjects as specified
      )
      
      # CreatePsArgs: Propensity score model fitting settings
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # Maximum size for PS model fitting
        errorOnHighCorrelation = TRUE,  # Error on high correlation between covariates
        stopOnError = FALSE,  # Continue even if PS model fails (for diagnostics)
        estimator = "att",  # Average treatment effect on treated
        prior = Cyclops::createPrior(  # Regularization prior for PS model
          priorType = "laplace",  # Laplace prior as specified
          exclude = c(0),  # Exclude intercept from regularization
          useCrossValidation = TRUE  # Use cross-validation for prior
        ),
        control = Cyclops::createControl(  # Cyclops control settings
          noiseLevel = "silent",  # Silent output during fitting
          cvType = "auto",  # Automatic cross-validation type
          seed = 1,  # Random seed for reproducibility
          resetCoefficients = TRUE,  # Reset coefficients before fitting
          tolerance = 2e-07,  # Convergence tolerance
          cvRepetitions = 10,  # 10-fold cross-validation
          startingVariance = 0.01  # Starting variance for prior
        )
      )
      
      # Covariate balance computation settings
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL  # No filter for shared balance computation
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()  # Standard Table 1 specifications
      )
      
      # FitOutcomeModelArgs: Outcome model settings
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",  # Cox proportional hazards model
        stratified = TRUE,  # Stratify by propensity score strata
        useCovariates = TRUE,  # Include covariates in outcome model
        inversePtWeighting = FALSE,  # Not using inverse probability weighting
        prior = Cyclops::createPrior(  # Regularization prior for outcome model
          priorType = "laplace",  # Laplace prior as specified
          useCrossValidation = TRUE  # Use cross-validation for prior
        ),
        control = Cyclops::createControl(  # Cyclops control settings
          cvType = "auto",  # Automatic cross-validation type
          seed = 1,  # Random seed for reproducibility
          resetCoefficients = TRUE,  # Reset coefficients before fitting
          startingVariance = 0.01,  # Starting variance for prior
          tolerance = 2e-07,  # Convergence tolerance
          cvRepetitions = 10,  # 10-fold cross-validation
          noiseLevel = "quiet"  # Quiet output during fitting
        )
      )
      
      # CreateStudyPopArgs: Study population creation settings
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,  # Already restricted in getDbCohortMethodData
        firstExposureOnly = FALSE,  # Include all exposures
        washoutPeriod = 0,  # No washout period
        removeDuplicateSubjects = "keep all",  # Keep all duplicates as specified
        censorAtNewRiskWindow = FALSE,  # Do not censor at new risk window
        removeSubjectsWithPriorOutcome = TRUE,  # Remove subjects with prior outcome
        priorOutcomeLookback = 99999,  # Lookback period for prior outcomes
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999  # Maximum days at risk
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
  analysesToExclude = NULL,  # Include all analyses
  refitPsForEveryOutcome = FALSE,  # Use same PS model for all outcomes
  refitPsForEveryStudyPopulation = FALSE,  # Use same PS model across study populations
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()  # Default diagnostic thresholds
)

# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save analysis specifications to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "cystectomytrimodality", "cystectomytrimodalityAnalysisSpecification.json")
)