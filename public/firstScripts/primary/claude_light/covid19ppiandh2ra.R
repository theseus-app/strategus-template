################################################################################
# CohortMethod Analysis Specifications for COVID-19 PPI and H2RA Study
# This script creates a Strategus analysis specification for comparing
# proton pump inhibitors (PPI) and H2-receptor antagonists (H2RA) in COVID-19 patients
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
# ##############################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from ATLAS
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions: target (PPI), comparator (H2RA), and outcome cohorts
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: PPI users
    1794132, # Comparator: H2RA users
    1794131  # Outcome: COVID-19 related outcome
  ),
  generateStats = TRUE
)

# Re-number cohorts for consistency (target=1, comparator=2, outcome=3)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes
# Retrieve the negative control concept set to create negative control outcome cohorts
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
  # Assign unique cohort IDs for negative control outcomes (101, 102, 103, etc.)
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Validate that there are no duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to organize cohorts for analysis -------------------------
# Define outcomes of interest
# priorOutcomeLookBack set to 99999 days to look back entire patient history
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Define target and comparator cohorts for the CohortMethod analysis
# Target: PPI users, Comparator: H2RA users
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Excluded covariate concepts - exclude the drugs of interest from covariate adjustment
# to prevent adjusting for the exposure itself
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# CohortGeneratorModule --------------------------------------------------------
# Module for generating cohorts from the CDM
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Create shared resource specifications for negative control outcomes
# Using first occurrence with descendant concept detection
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications for cohort generation with statistics
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# Module for diagnostics on generated cohorts
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
# Module for comparative effectiveness analysis

# Define study periods
# Study period: January 1, 2020 to May 15, 2020 (COVID-19 initial wave)
studyPeriods <- tibble(
  studyStartDate = c("20200101"),  # YYYYMMDD format
  studyEndDate   = c("20200515")   # YYYYMMDD format
)

# Define time-at-risk (TAR) periods
# TAR: From cohort start to 99999 days after cohort start with minimum 1 day at risk
# This captures the entire follow-up period post-exposure
timeAtRisks <- tibble(
  label = c("Full Follow-up"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),  # Anchored to exposure date
  riskWindowEnd  = c(99999),
  endAnchor = c("cohort start"),
  minDaysAtRisk = c(1)
)

# Propensity Score settings - stratify by PS
# Using 5 strata with all subjects to balance covariate distributions
stratifyByPsArgsList <- tibble(
  label = c("Stratify by PS (5 strata)"),
  numberOfStrata  = c(5),
  baseSelection = c("all")  # Use all subjects for stratification
)

# Build propensity score configuration list
# This example uses stratification by PS (matching could also be used)
psConfigList <- list()

# Convert stratification configurations to config list format
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

# Build CohortMethod analyses iterating through all setting combinations
cmAnalysisList <- list()
analysisId <- 1

# Iterate through study periods
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  # Iterate through time-at-risk definitions
  for (t in seq_len(nrow(timeAtRisks))) {

    # Iterate through propensity score adjustment methods
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Build propensity score adjustment arguments
      if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Define covariate settings
      # Using default covariates with descendant concept exclusion
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome list combining positive outcomes and negative controls
      outcomeList <- append(
        # Positive outcomes (outcomes of interest)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,  # Unknown true effect size
            priorOutcomeLookback = 99999
          )
        }),
        # Negative control outcomes (should show no effect if method is unbiased)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1  # Expected HR = 1 (no effect)
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

      # Get database cohort method data arguments
      # Specifies data extraction parameters
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,  # Allow different exposure periods
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,  # 0 = no limit
        covariateSettings = covariateSettings
      )

      # Create study population arguments
      # Specifies inclusion/exclusion criteria and follow-up periods
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,  # Do not require common exposure period
        firstExposureOnly = TRUE,  # Use only first exposure episode
        washoutPeriod = 365,  # 1-year washout before exposure
        removeDuplicateSubjects = "keep all",  # Keep all duplicate subjects
        censorAtNewRiskWindow = FALSE,  # Do not censor at new exposure
        removeSubjectsWithPriorOutcome = TRUE,  # Exclude subjects with prior outcome
        priorOutcomeLookback = 99999,  # Look back entire history for prior outcomes
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t]
      )

      # Propensity score estimation arguments
      # Using Laplace prior with cross-validation for regularization
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # Max subjects for PS model fitting
        errorOnHighCorrelation = TRUE,  # Stop if high correlation detected
        stopOnError = FALSE,  # Allow analysis to continue if PS fitting fails
        estimator = "att",  # Average treatment effect on the treated
        prior = Cyclops::createPrior(
          priorType = "laplace",  # Laplace (L1) regularization
          exclude = c(0),  # Intercept not penalized
          useCrossValidation = TRUE  # Use cross-validation for lambda selection
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",
          cvType = "auto",  # Automatic CV type selection
          seed = 1,
          resetCoefficients = TRUE,
          tolerance = 2e-07,
          cvRepetitions = 10,  # 10 repetitions for CV
          startingVariance = 0.01
        )
      )

      # Covariate balance assessment (shared covariates)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL  # Assess balance for all covariates
      )

      # Covariate balance assessment (Table 1 covariates)
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model fitting arguments
      # Using Cox proportional hazards model stratified by PS strata
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",  # Cox PH model for time-to-event outcomes
        stratified = TRUE,  # Stratified by PS strata
        useCovariates = FALSE,  # Do not adjust for covariates (stratification handles balance)
        inversePtWeighting = FALSE,  # Do not use inverse probability weighting
        prior = Cyclops::createPrior(
          priorType = "laplace",  # Laplace regularization
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          tolerance = 2e-07,
          cvType = "auto",
          fold = 10,  # 10-fold cross-validation
          cvRepetitions = 10,  # 10 repetitions
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Append analysis configuration to the analysis list
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
  file.path("inst", "covid19ppiandh2ra", "covid19ppiandh2raAnalysisSpecification.json")
)