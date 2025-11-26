################################################################################
# CohortMethod Analysis Specification Script
# Study: DOACs and Warfarin
# 
# This script creates an analysis specification using the OHDSI Strategus 
# package for comparing direct oral anticoagulants (DOACs) versus Warfarin
# with propensity score matching adjustment.
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
# Retrieve target, comparator, and outcome cohort definitions
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: DOAC cohort
    1794132, # Comparator: Warfarin cohort
    1794131  # Outcome: Clinical outcome of interest
  ),
  generateStats = TRUE
)

# Re-number cohorts to sequential IDs for internal use
# Target cohort ID: 1794126 -> 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# Comparator cohort ID: 1794132 -> 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# Outcome cohort ID: 1794131 -> 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Extract negative control concept set (ID: 1888110) to create negative control outcome cohorts
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
  # Assign cohort IDs starting at 101 for negative controls
  # (target/comparator cohort IDs are 1, 2, 3)
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Verify no duplicate cohort IDs exist
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomesCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to organize cohorts for analysis -------------------------

# Outcomes of interest
# Extract outcome cohort (cohortId == 3) from cohortDefinitionSet
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # Clean window: lookback period for prior outcome exclusion (365 days)
  mutate(cleanWindow = 365)

# Target and Comparator cohorts for CohortMethod analysis
# Maps DOAC (target) vs Warfarin (comparator) comparison
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# CohortGeneratorModule -------------------------------------------------------
# This module generates the cohort definitions in the database
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create cohort shared resource specifications
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Create negative control outcome cohort shared resource specifications
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  # Use first occurrence of each outcome concept
  occurrenceType = "first",
  # Detect outcomes on descendant concepts as well
  detectOnDescendants = TRUE
)

# Module specifications for cohort generation
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings -------------------------------------------
# This module generates diagnostic statistics for the cohorts
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  # Run diagnostics on all cohorts
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
# This module performs comparative effectiveness research using cohort method

# Study period: Oct 19, 2010 to Dec 31, 2018
# Data outside this window will be excluded from analysis
studyPeriods <- tibble(
  studyStartDate = "20101019", # YYYYMMDD format
  studyEndDate = "20181231"    # YYYYMMDD format
)

# Time-at-risk (TAR) definition
# TAR starts 1 day after cohort start and ends at cohort end
# Minimum 1 day at risk required for inclusion
timeAtRisks <- tibble(
  label = "ITT",  # Intent-to-treat
  riskWindowStart = 1,           # Start 1 day after exposure start
  startAnchor = "cohort start",  # Anchor to cohort start date
  riskWindowEnd = 0,             # End at exposure end date
  endAnchor = "cohort end",      # Anchor to cohort end date
  minDaysAtRisk = 1              # Minimum 1 day in follow-up period
)

# Propensity Score matching configuration
# Match on propensity score with caliper = 0.2 on standardized logit scale
# maxRatio = 1 means 1:1 matching (one comparator per target subject)
matchOnPsArgsList <- tibble(
  label = "1:1 PS matching (caliper=0.2)",
  maxRatio = 1,                     # 1:1 matching ratio
  caliper = 0.2,                    # Caliper width
  caliperScale = "standardized logit" # Scale for caliper measurement
)

# PS configuration list (combining match/stratify options)
# Initialize empty list to hold propensity score adjustment configurations
psConfigList <- list()

# Convert match on PS specifications to configuration list
# Each row in matchOnPsArgsList becomes one configuration in psConfigList
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      # Method identifier: "match" for matching on propensity score
      method = "match",
      # Human-readable label for this configuration
      label = matchOnPsArgsList$label[i],
      # Parameter bundle for PS matching
      params = list(
        maxRatio = matchOnPsArgsList$maxRatio[i],
        caliper = matchOnPsArgsList$caliper[i],
        caliperScale = matchOnPsArgsList$caliperScale[i]
      )
    )
  }
}

# Build analysis list iterating through all setting combinations -----------
cmAnalysisList <- list()
analysisId <- 1

# Iterate through study periods
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  # Iterate through time-at-risk definitions
  for (t in seq_len(nrow(timeAtRisks))) {
    
    # Iterate through propensity score configurations
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure propensity score adjustment method (match or stratify)
      if (psCfg$method == "match") {
        # Propensity score matching parameters
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

      # Covariate settings
      # Use default covariates (all conditions, drugs, procedures, etc.)
      # Include descendants when excluding drug concepts
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcome definitions
      # Combine outcomes of interest and negative control outcomes
      outcomeList <- append(
        # Outcomes of interest (true effects to be estimated)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,  # This is a true outcome of interest
            trueEffectSize = NA,       # Unknown true effect
            priorOutcomeLookback = 99999  # Lookback 99999 days for prior outcome
          )
        }),
        # Negative control outcomes (known to have no effect)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,  # Negative control outcome
            trueEffectSize = 1          # Known true effect = 1 (no effect)
          )
        })
      )

      # Target-Comparator-Outcome combinations
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        # Define DOAC vs Warfarin comparison
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],          # DOAC cohort
          comparatorId = cmTcList$comparatorCohortId[i],  # Warfarin cohort
          outcomes = outcomeList,                         # All outcomes (true + negative controls)
          # Exclude DOAC and Warfarin concepts from covariates
          # to avoid direct adjustment for exposure
          excludedCovariateConceptIds = c()
        )
      }

      # Database data extraction arguments
      # Specify study period and data extraction settings
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        # Do not restrict to common period (allow different follow-up start dates)
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        # Maximum cohort size: 0 = no limit
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Propensity score model fitting arguments
      # Uses Lasso regression via Cyclops for variable selection
      createPsArgs <- CohortMethod::createCreatePsArgs(
        # Maximum subjects to use for PS model fitting
        maxCohortSizeForFitting = 250000,
        # Throw error if high correlation detected between covariates
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,
        estimator = "att",  # Average treatment effect on treated
        # Lasso prior for regularization and variable selection
        prior = Cyclops::createPrior(
          priorType = "laplace",
          exclude = c(0),
          useCrossValidation = TRUE
        ),
        # Cyclops optimization control parameters
        control = Cyclops::createControl(
          noiseLevel = "silent",
          cvType = "auto",
          seed = 1,
          resetCoefficients = TRUE,
          tolerance = 2e-07,
          cvRepetitions = 10,  # 10-fold cross-validation from spec
          startingVariance = 0.01
        )
      )

      # Shared covariate balance computation (before PS adjustment)
      # Computes balance for all covariates
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      # Covariate balance computation (after PS adjustment)
      # Computes balance for Table 1 covariates (demographics, conditions, drugs)
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model fitting arguments
      # Cox proportional hazards model without stratification or covariates
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",        # Cox proportional hazards model
        stratified = FALSE,       # Do not stratify by matching set
        useCovariates = FALSE,    # Do not adjust for covariates (already PS matched)
        inversePtWeighting = FALSE,  # Do not use inverse probability weighting
        # Lasso prior for regularization
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE
        ),
        # Cyclops optimization control parameters
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

      # Study population creation arguments
      # Define inclusion/exclusion criteria and follow-up period
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        # Do not restrict to common period
        restrictToCommonPeriod = FALSE,
        # Include only first exposure for each subject
        firstExposureOnly = TRUE,
        # Require 365-day washout period before exposure
        washoutPeriod = 365,
        # Keep all subjects (do not remove duplicates)
        removeDuplicateSubjects = "keep all",
        # Do not censor at new exposure window
        censorAtNewRiskWindow = FALSE,
        # Remove subjects with prior outcome (in lookback period)
        removeSubjectsWithPriorOutcome = TRUE,
        # 99999-day lookback for prior outcome detection
        priorOutcomeLookback = 99999,
        # Risk window: 1 day after cohort start to cohort end
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        # Minimum 1 day at risk for inclusion
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t]
      )

      # Create analysis specification combining all arguments
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

# Create CohortMethod module specifications ----------------------------------
cmModuleSettingsCreator <- CohortMethodModule$new()

cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  # Refit PS model for each outcome using outcome cohort inclusion/exclusion
  refitPsForEveryOutcome = FALSE,
  # Refit PS model for each study population definition
  refitPsForEveryStudyPopulation = FALSE,
  # Diagnostic thresholds for CohortMethod diagnostics
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the analysis specifications ------------------------------------------
# Combine all modules into single analysis specification object
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Add shared resources (cohort definitions)
  Strategus::addSharedResources(cohortDefinitionShared) |>
  # Add shared resources (negative control outcomes)
  Strategus::addSharedResources(negativeControlsShared) |>
  # Add cohort generation module
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  # Add cohort diagnostics module
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  # Add cohort method module
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save analysis specifications to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "doacsandwarfarin", "doacsandwarfarinAnalysisSpecification.json")
)