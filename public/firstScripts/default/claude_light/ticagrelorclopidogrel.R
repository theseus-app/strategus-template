################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates a comprehensive analysis specification for a comparative
# cohort study using the OHDSI Strategus package. The analysis compares ticagrelor
# vs clopidogrel outcomes using propensity score adjustment methods.
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
# Get the list of cohorts from WebAPI
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Retrieve cohort definitions for target (ticagrelor), comparator (clopidogrel), 
# and outcome cohorts
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: ticagrelor
    1794132, # Comparator: clopidogrel
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal consistency (Target=1, Comparator=2, Outcome=3)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve negative control concept set to generate negative control outcome cohorts
# These are used to assess for systematic bias in the analysis
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
  dplyr::rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  dplyr::mutate(cohortId = dplyr::row_number() + 100) %>% 
  # Note: target/comparator cohort ids are 1, 2, 3; negative controls start at 101, 102, 103...
  dplyr::select(cohortId, cohortName, outcomeConceptId)

# Validation: Check for duplicate cohort IDs across definition and negative control sets
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to organize cohorts for analysis -------------------------

# Outcomes: Extract the outcome cohort from cohortDefinitionSet
# cleanWindow specifies the prior outcome look-back period (365 days)
oList <- cohortDefinitionSet %>%
  dplyr::filter(.data$cohortId == 3) %>%
  dplyr::mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  dplyr::select(outcomeCohortId, outcomeCohortName) %>%
  dplyr::mutate(cleanWindow = 365)

# Target and Comparator for CohortMethod analysis
# Defines the exposure comparison: ticagrelor (target) vs clopidogrel (comparator)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  stringsAsFactors = FALSE
)

# Covariates to exclude: the target and comparator concepts themselves
# This prevents the drugs being studied from being included as covariates
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c(),
  stringsAsFactors = FALSE
)

# CohortGeneratorModule --------------------------------------------------------
# This module handles the generation of cohorts from the OMOP CDM

cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resources for main cohorts (target, comparator, outcome)
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Create shared resources for negative control outcome cohorts
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications for cohort generation
# generateStats = TRUE will produce detailed statistics about generated cohorts
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# This module generates diagnostic statistics to assess cohort quality

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
# This module performs the comparative cohort analysis

# Study Periods
# Two separate study time windows are defined for the analysis
studyPeriods <- tibble::tibble(
  studyStartDate = c("20111101", "20130301"), # YYYYMMDD format
  studyEndDate   = c("20190331", "20161231")  # YYYYMMDD format
)

# Time-at-Risks (TARs)
# Defines multiple risk windows for outcome assessment:
# - 1-365 days after cohort start
# - 1-1825 days (5 years) after cohort start
# - From cohort start to cohort end
# - Same windows starting 29 days after cohort start (to allow stabilization)
timeAtRisks <- tibble::tibble(
  label = c(
    "1-365 days",
    "1-1825 days",
    "Cohort duration",
    "29-365 days",
    "29-1825 days",
    "29-Cohort duration"
  ),
  riskWindowStart  = c(1, 1, 1, 29, 29, 29),
  startAnchor = c(
    "cohort start", "cohort start", "cohort start",
    "cohort start", "cohort start", "cohort start"
  ),
  riskWindowEnd  = c(365, 1825, 0, 365, 1825, 0),
  endAnchor = c(
    "cohort start", "cohort start", "cohort end",
    "cohort start", "cohort start", "cohort end"
  ),
  minDaysAtRisk = c(1, 1, 1, 1, 1, 1)
)

# Propensity Score Settings - Match on PS
# Configuration 1: 1:1 matching with 0.2 caliper
# Configuration 2: 1:10 matching with 0.2 caliper
matchOnPsArgsList <- tibble::tibble(
  label = c("1:1 matching", "1:10 matching"),
  maxRatio  = c(1, 10),
  caliper = c(0.2, 0.2),
  caliperScale  = c("standardized logit", "standardized logit")
)

# Propensity Score Settings - Stratify by PS
# Configuration: 10 strata using all subjects as base selection
stratifyByPsArgsList <- tibble::tibble(
  label = c("10-strata stratification"),
  numberOfStrata  = c(10),
  baseSelection = c("all")
)

# Build a single PS configuration list
# Each entry contains the method, label, and parameters for propensity score adjustment
psConfigList <- list()

# Add match-on-PS configurations
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    # Append a new PS configuration using matching approach
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "match",                           # Adjustment method: matching
      label  = matchOnPsArgsList$label[i],        # Human-readable label
      params = list(
        maxRatio     = matchOnPsArgsList$maxRatio[i],
        caliper      = matchOnPsArgsList$caliper[i],
        caliperScale = matchOnPsArgsList$caliperScale[i]
      )
    )
  }
}

# Add stratify-by-PS configurations
if (exists("stratifyByPsArgsList") && nrow(stratifyByPsArgsList) > 0) {
  for (i in seq_len(nrow(stratifyByPsArgsList))) {
    # Append a new PS configuration using stratification approach
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "stratify",                        # Adjustment method: stratification
      label  = stratifyByPsArgsList$label[i],     # Human-readable label
      params = list(
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection  = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}

# Build CohortMethod analyses by iterating through all setting combinations
# This creates a complete factorial design of study periods × time-at-risks × PS methods
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create propensity score adjustment arguments based on method
      if (psCfg$method == "match") {
        # Matching approach
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # Stratification approach
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Default covariate settings
      # Includes all covariates from diagnosis, procedure, drug, and measurement domains
      # addDescendantsToExclude = TRUE will exclude descendants of excluded concepts
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome list combining outcomes of interest and negative controls
      outcomeList <- append(
        # Outcomes of interest (outcome1)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,                    # Unknown true effect for real outcomes
            priorOutcomeLookback = 99999
          )
        }),
        # Negative control outcomes (for bias assessment)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1                      # Expected null effect for negative controls
          )
        })
      )

      # Create target-comparator-outcome triplet(s)
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

      # Get database cohort method data arguments
      # Controls the initial data extraction parameters
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        maxCohortSize = 0,                          # 0 = no limit on cohort size
        restrictToCommonPeriod = TRUE,              # Both cohorts must overlap in follow-up
        firstExposureOnly = FALSE,                  # Include all exposures, not just first
        washoutPeriod = 0,                          # No washout period before exposure
        removeDuplicateSubjects = "keep first",     # Keep first occurrence if duplicates
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        covariateSettings = covariateSettings
      )

      # Create propensity score arguments
      # These control the fitting of the propensity score model
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,           # Max subjects for PS model fitting
        errorOnHighCorrelation = TRUE,              # Stop if high multicollinearity detected
        stopOnError = FALSE,                        # Continue even if PS model fails
        estimator = "att",                          # Estimate Average Treatment effect on Treated
        prior = Cyclops::createPrior(
          priorType = "laplace",                    # L1 regularization (lasso)
          exclude = c(0),                           # Exclude intercept from regularization
          useCrossValidation = TRUE                 # Use CV to select regularization strength
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",                    # Minimize console output
          cvType = "auto",                          # Automatic CV type selection
          seed = 1,
          resetCoefficients = TRUE,
          tolerance = 2e-07,
          cvRepetitions = 10,                       # Number of CV repetitions
          startingVariance = 0.01
        )
      )

      # Compute shared covariate balance (before matching/stratification)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000
      )

      # Compute stratified covariate balance (after matching/stratification)
      # Uses Table 1 specifications for balance assessment
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit outcome model arguments
      # Cox model with stratification by matched/stratified pairs
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",                          # Cox proportional hazards model
        stratified = TRUE,                          # Stratified by matched/stratified groups
        useCovariates = FALSE,                      # No additional covariates in outcome model
        inversePtWeighting = FALSE,                 # Don't use inverse probability weighting
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

      # Create study population arguments
      # Defines how to construct the study cohort from the exposed populations
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,             # Don't require common follow-up period
        firstExposureOnly = FALSE,                  # Include all exposures
        washoutPeriod = 0,                          # No washout period
        removeDuplicateSubjects = "keep all",       # Keep all subject occurrences
        censorAtNewRiskWindow = FALSE,              # Don't censor at new risk window start
        removeSubjectsWithPriorOutcome = FALSE,     # Include subjects with prior outcome
        priorOutcomeLookback = 99999,               # Look back 99999 days for prior outcome
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t]
      )

      # Append the complete analysis specification
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

# Create the complete analysis specifications --------------------------------
# Combines all modules (CohortGenerator, CohortDiagnostics, CohortMethod) into
# a single specification object that can be executed by Strategus

analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() %>%
  Strategus::addSharedResources(cohortDefinitionShared) %>%
  Strategus::addSharedResources(negativeControlsShared) %>%
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to JSON
# This file can be executed by Strategus to run the complete study
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ticagrelorclopidogrel", "ticagrelorclopidogrelAnalysisSpecification.json")
)