################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates analysis specifications for the covid19famotidine study
# using the OHDSI Strategus package. It configures cohort generation, cohort
# diagnostics, and cohort method analyses with propensity score stratification.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Configuration for retrieving cohort definitions from ATLAS
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions for: Target (1794126), Comparator (1794132), Outcome (1794131)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal consistency (1=Target, 2=Comparator, 3=Outcome)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve negative control concept set (ID: 1888110) and expand to individual cohorts
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
  # Assign cohort IDs starting at 101 for negative controls (to avoid conflicts with main cohorts 1-3)
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Verify no duplicate cohort IDs between main cohorts and negative control cohorts
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to organize cohorts for analysis
# Outcomes: Extract outcome cohort information
# outcomeCohortId=3, outcomeOf interest=TRUE, cleanWindow=365 days
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Associates target cohort 1 with comparator cohort 2
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# CohortGeneratorModule --------------------------------------------------------
# Configures cohort generation from cohort definitions

cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for main cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Create shared resource specifications for negative control outcome cohorts
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Configure cohort generator module to generate statistics
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# Configures diagnostic analyses on generated cohorts

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
# Configures comparative effectiveness analysis using propensity score stratification

# Study periods: Define the time window for the study (Feb 1, 2020 - May 30, 2020)
studyPeriods <- tibble(
  studyStartDate = "20200201", # YYYYMMDD format
  studyEndDate   = "20200530"  # YYYYMMDD format
)

# Time-at-risks (TARs): Define outcome risk windows relative to cohort start
# Risk window: 1 to 30 days after cohort start (exposure initiation)
timeAtRisks <- tibble(
  label = "1-30 days after exposure",
  riskWindowStart  = 1,
  startAnchor = "cohort start",
  riskWindowEnd  = 30,
  endAnchor = "cohort start",
  minDaysAtRisk = 1
)

# Propensity Score settings - stratify by PS
# Stratify population into 5 strata based on propensity score
stratifyByPsArgsList <- tibble(
  label = "5 strata",
  numberOfStrata  = 5,
  baseSelection = "all" # Use all subjects for stratification
)

# Build PS configuration list (method, label, parameters for each configuration)
psConfigList <- list()

# Process stratify by PS configurations
if (nrow(stratifyByPsArgsList) > 0) {
  for (i in seq_len(nrow(stratifyByPsArgsList))) {
    # Append a new PS configuration element
    psConfigList[[length(psConfigList) + 1]] <- list(
      # Identify the PS adjustment method: "stratify"
      method = "stratify",
      # Human-readable label for this configuration
      label  = stratifyByPsArgsList$label[i],
      # Parameter bundle passed to createStratifyByPsArgs
      params = list(
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection  = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}

# Iterate through all analysis setting combinations (study periods, TAR, PS configs)
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Initialize PS matching and stratification arguments
      matchOnPsArgs <- NULL
      
      # Configure stratify by PS parameters
      if (psCfg$method == "stratify") {
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Configure covariate settings: Use default covariates with descendant concept inclusion
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build outcome list: outcomes of interest + negative control outcomes
      # Outcomes of interest: trueEffectSize = NA, prior lookback = 99999
      # Negative controls: trueEffectSize = 1 (null effect expected), outcomeOfInterest = FALSE
      outcomeList <- append(
        # Outcomes of interest from oList
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

      # Build target-comparator-outcomes combinations
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = c() # No specific drug concepts to exclude in this analysis
        )
      }

      # Configure GetDbCohortMethodData: Retrieve cohort data for the specified study period
      # maxCohortSize=0 means no size restrictions, covariates extracted using default settings
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Configure CreatePs: Fit propensity score model
      # Uses Laplace prior with cross-validation (10-fold, 10 repetitions)
      # maxCohortSizeForFitting = 250000 subjects, errorOnHighCorrelation = TRUE
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
          seed = 1,
          resetCoefficients = TRUE,
          tolerance = 2e-07,
          cvRepetitions = 10,
          fold = 10,
          startingVariance = 0.01
        )
      )

      # Configure ComputeSharedCovariateBalance: Full covariate balance assessment
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      # Configure ComputeCovariateBalance: Table 1 covariates only
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Configure FitOutcomeModel: Cox proportional hazards model
      # stratified = TRUE (stratification by PS strata)
      # useCovariates = FALSE (no additional covariate adjustment)
      # inversePtWeighting = FALSE (not using IPW)
      # Laplace prior with cross-validation (10-fold, 10 repetitions)
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
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
          fold = 10,
          noiseLevel = "quiet"
        )
      )

      # Configure CreateStudyPopulation: Define study population inclusion/exclusion criteria
      # restrictToCommonPeriod = FALSE: not requiring common follow-up period
      # firstExposureOnly = FALSE: allow multiple exposures per subject
      # washoutPeriod = 0: no observation period requirement
      # removeDuplicateSubjects = "keep all": retain all subject records
      # censorAtNewRiskWindow = FALSE: no censoring at new exposure
      # removeSubjectsWithPriorOutcome = TRUE: exclude subjects with prior outcome in lookback
      # priorOutcomeLookback = 30: examine 30 days prior to cohort start
      # Risk window: 1-30 days after cohort start, minDaysAtRisk = 1
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 30,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Create analysis specification combining all configured arguments
      # analysisId: unique identifier for this analysis configuration
      # description: informative text describing the analysis settings
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

# Create CohortMethodModule specifications
# Combines all analysis configurations with target-comparator-outcome associations
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the complete analysis specifications -----------------------------------
# Combines all modules: cohort generation, diagnostics, and cohort method
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save analysis specifications to JSON file
# This file can be executed by Strategus to run the complete analysis
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "covid19famotidine", "covid19famotidineAnalysisSpecification.json")
)