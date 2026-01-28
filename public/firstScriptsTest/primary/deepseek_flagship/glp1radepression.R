################################################################################
# CreateStrategusAnalysisSpecification.R
# Analysis specification for glp1radepression study using Strategus
#
# This script creates an OHDSI Strategus analysis specification for the
# GLP1-RA depression study, incorporating all settings from the provided
# analysis specifications JSON.
#
# Key components:
# 1. Cohort definitions (target, comparator, outcome, negative controls)
# 2. CohortGenerator module for cohort creation
# 3. CohortDiagnostics module for cohort characterization
# 4. CohortMethod module for comparative effectiveness analysis
#
# Note: WebAPI URLs and cohort IDs must be configured for your environment.
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Configure WebAPI connection - UPDATE THIS URL FOR YOUR ENVIRONMENT
baseUrl <- "https://YOUR-ATLAS-ENVIRONMENT/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export cohort definitions from WebAPI
# Note: Update cohort IDs to match your Atlas environment
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1 (GLP1-RA)
    1794132, # Comparator: comparator1 (DPP4i)
    1794131  # Outcome: outcome1 (depression)
  ),
  generateStats = TRUE
)

# Renumber cohorts for internal Strategus use
# Target becomes cohortId 1, comparator becomes 2, outcome becomes 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative Control Outcomes ----------------------------------------------------
# Get negative control concept set and create negative control outcome cohorts
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,  # negative control concept set ID
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(
    baseUrl = baseUrl
  ) %>%
  ROhdsiWebApi::getConcepts(
    baseUrl = baseUrl
  ) %>%
  rename(
    outcomeConceptId = "conceptId",
    cohortName = "conceptName"
  ) %>%
  mutate(
    # Assign cohort IDs starting from 101 to avoid conflicts with main cohorts
    cohortId = row_number() + 100
  ) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Verify no duplicate cohort IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Analysis Configuration Data Frames ------------------------------------------
# Outcome definitions for CohortMethod analysis
# Includes both primary outcome (depression) and negative controls
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%  # Outcome cohort (depression)
  mutate(
    outcomeCohortId = cohortId,
    outcomeCohortName = cohortName,
    cleanWindow = 365  # Not used in this analysis but required by template
  ) %>%
  select(outcomeCohortId, outcomeCohortName, cleanWindow)

# Target-comparator pairs for CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate exclusion: empty as per analysis specifications
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# Study Periods ----------------------------------------------------------------
# Define study period from 2013-01-01 to 2020-12-31 as per analysis specifications
studyPeriods <- tibble(
  studyStartDate = "20130101",  # YYYYMMDD format
  studyEndDate   = "20201231"   # YYYYMMDD format
)

# Time-at-Risk Specifications --------------------------------------------------
# Single TAR: 1-730 days from cohort start with 1 day minimum at risk
timeAtRisks <- tibble(
  label = "1-730d from start",
  riskWindowStart  = 1,
  startAnchor = "cohort start",  # Anchor to cohort start date
  riskWindowEnd  = 730,
  endAnchor = "cohort start",    # Anchor to cohort start date
  minDaysAtRisk = 1
)

# Propensity Score Adjustment Configurations -----------------------------------
# Matching configuration: 1:1 matching with 0.05 caliper on propensity score
matchOnPsArgsList <- tibble(
  label = "1:1 matching, caliper=0.05",
  maxRatio  = 1,                 # 1:1 matching ratio
  caliper = 0.05,               # 0.05 caliper
  caliperScale  = "propensity score"  # Caliper on propensity score scale
)

# No stratification configuration (null as per analysis specifications)
# stratifyByPsArgsList not created since stratifyByPsArgs is null

# Build PS configuration list --------------------------------------------------
psConfigList <- list()

# Add matching configuration to PS config list
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

# CohortGenerator Module Specifications ----------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",      # First occurrence of negative control outcome
  detectOnDescendants = TRUE     # Include descendant concepts
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE  # Generate cohort statistics
)

# CohortDiagnostics Module Specifications --------------------------------------
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

# CohortMethod Module Analysis List --------------------------------------------
# Iterate through all analysis setting combinations to create analysis list
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]
  
  for (t in seq_len(nrow(timeAtRisks))) {
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Create PS adjustment arguments based on configuration
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
      
      # Covariate settings with default options
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )
      
      # Create outcome list including primary outcome and negative controls
      outcomeList <- append(
        # Primary outcome (depression)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,  # Unknown true effect for primary outcome
            priorOutcomeLookback = 99999  # Lookback for prior outcomes
          )
        }),
        # Negative control outcomes
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1  # Null effect for negative controls
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
          # No excluded covariate concepts as per analysis specifications
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }
      
      # GetDbCohortMethodDataArgs with study period restrictions
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,  # Restrict to common period as specified
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,  # 0 = no limit on cohort size
        covariateSettings = covariateSettings
      )
      
      # Propensity score model arguments
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,  # Continue even if model fitting fails
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",  # Laplace prior for regularization
          exclude = c(0),         # Exclude intercept from regularization
          useCrossValidation = TRUE  # Use cross-validation as specified
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",     # Silent mode during fitting
          cvType = "auto",           # Automatic cross-validation type
          seed = 1,                  # Random seed for reproducibility
          resetCoefficients = TRUE,  # Reset coefficients before fitting
          tolerance = 2e-07,         # Convergence tolerance
          cvRepetitions = 10,        # 10-fold cross-validation
          startingVariance = 0.01    # Starting variance for prior
        )
      )
      
      # Covariate balance computation arguments
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL  # No filter for shared balance
      )
      
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )
      
      # Outcome model arguments (Cox proportional hazards)
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",          # Cox proportional hazards model
        stratified = TRUE,          # Stratified by propensity score
        useCovariates = FALSE,      # No covariate adjustment in outcome model
        inversePtWeighting = FALSE, # No IP weighting
        prior = Cyclops::createPrior(
          priorType = "laplace",    # Laplace prior
          useCrossValidation = TRUE # Cross-validation for regularization
        ),
        control = Cyclops::createControl(
          cvType = "auto",          # Automatic cross-validation
          seed = 1,                 # Random seed
          resetCoefficients = TRUE, # Reset coefficients
          startingVariance = 0.01,  # Starting variance
          tolerance = 2e-07,        # Convergence tolerance
          cvRepetitions = 10,       # 10-fold cross-validation
          noiseLevel = "quiet"      # Quiet mode
        )
      )
      
      # Study population creation arguments
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE,           # Restrict to common period
        firstExposureOnly = FALSE,               # Allow multiple exposures
        washoutPeriod = 0,                       # No washout period
        removeDuplicateSubjects = "keep all",    # Keep all subjects as specified
        censorAtNewRiskWindow = FALSE,           # No censoring at new risk window
        removeSubjectsWithPriorOutcome = TRUE,   # Remove subjects with prior outcome
        priorOutcomeLookback = 99999,            # Lookback for prior outcomes
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,                       # Minimum 1 day at risk
        maxDaysAtRisk = 99999
      )
      
      # Append analysis to list
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

# CohortMethod Module Specifications ------------------------------------------
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,  # No analyses to exclude
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create Final Analysis Specifications ----------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save Analysis Specifications ------------------------------------------------
# Create directory if it doesn't exist
dir.create(file.path("inst", "glp1radepression"), showWarnings = FALSE, recursive = TRUE)

# Save specifications to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "glp1radepression", "glp1radepressionAnalysisSpecification.json")
)