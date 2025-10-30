################################################################################
# OHDSI Strategus Analysis Specification Script
# Study: semaglutideandnaion
# 
# This script creates the analysis specifications for a comparative cohort study
# using the Strategus framework with HADES modules.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Configuration: Set the base URL for your ATLAS instance
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export cohort definitions from ATLAS using the cohort IDs specified in the
# analysis specifications:
# - Target cohort ID: 1794126 (target1)
# - Comparator cohort ID: 1794132 (comparator1)
# - Outcome cohort ID: 1794131 (outcome1)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to sequential IDs (1, 2, 3) for internal consistency
# This standardization helps with downstream processing
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative Control Outcomes ----------------------------------------------------
# Retrieve the negative control concept set (ID: 1888110, name: "negative")
# and create cohorts for each negative control outcome
# These are used to assess residual bias in the study design
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
  # Assign cohort IDs starting from 101 to avoid conflicts with main cohorts (1, 2, 3)
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: Ensure no duplicate cohort IDs exist across all cohort sets
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create Analysis-Specific Data Frames -----------------------------------------

# Outcomes list: Define the outcome cohort(s) of interest
# Analysis specification includes outcome1 (re-numbered to cohortId = 3)
# cleanWindow not specified in specs, using default of 365 days
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for CohortMethod analysis
# Target: cohortId 1 (originally 1794126, target1)
# Comparator: cohortId 2 (originally 1794132, comparator1)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate Exclusions ---------------------------------------------------------
# Analysis specifications indicate no specific concepts to exclude (conceptsToExclude has null values)
# Creating empty data frame as placeholder; will exclude target/comparator in LSPS
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# Note: Analysis specifications indicate no specific concepts to include (conceptsToInclude has null values)
# Default behavior will include all available covariates

# CohortGeneratorModule --------------------------------------------------------
# This module generates the cohorts defined in ATLAS into the CDM database
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for main cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource specifications for negative control outcome cohorts
# occurrenceType = "first" means only the first occurrence of each outcome is considered
# detectOnDescendants = TRUE means descendant concepts are included
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications with statistics generation enabled
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule ------------------------------------------------------
# This module performs diagnostic checks on the generated cohorts
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
# This module performs the comparative cohort study analysis

# Study Period Configuration ---------------------------------------------------
# From getDbCohortMethodDataArgs.studyPeriods in analysis specifications
# Study restricted to: December 1, 2017 through December 31, 2023
studyPeriods <- tibble(
  studyStartDate = c("20171201"),
  studyEndDate   = c("20231231")
)

# Time-at-Risk (TAR) Configuration ---------------------------------------------
# From createStudyPopArgs.timeAtRisks in analysis specifications
# Single TAR configuration: on-treatment analysis (cohort start to cohort end)
# riskWindowStart = 0, startAnchor = "cohort start"
# riskWindowEnd = 0, endAnchor = "cohort end"
# minDaysAtRisk = 1 day
timeAtRisks <- tibble(
  label = c("OnTreatment"),
  riskWindowStart  = c(0),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
)

# Propensity Score Adjustment Settings -----------------------------------------
# Two PS adjustment methods specified in analysis specifications

# Method 1: Match on PS
# From propensityScoreAdjustment.psSettings[0].matchOnPsArgs
# maxRatio = 1 (1:1 matching)
# caliper = 0.2 on standardized logit scale
matchOnPsArgsList <- tibble(
  label = c("1:1 Match 0.2 Caliper"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# Method 2: Stratify by PS
# From propensityScoreAdjustment.psSettings[1].stratifyByPsArgs
# numberOfStrata = 5 strata
# baseSelection = "all" (use all subjects)
stratifyByPsArgsList <- tibble(
  label = c("Stratify 5 Strata"),
  numberOfStrata  = c(5),
  baseSelection = c("all")
)

# Build Unified PS Configuration List ------------------------------------------
# Consolidate all PS adjustment methods into a single list structure
# Each element contains: method type, label, and parameters
psConfigList <- list()

# Process "match on PS" configurations
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

# Process "stratify by PS" configurations
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

# Create CohortMethod Analysis List --------------------------------------------
# Iterate through all combinations of study periods, TARs, and PS methods
# to create comprehensive analysis specifications
cmAnalysisList <- list()
analysisId <- 1

# Loop through study periods
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  # Loop through time-at-risk windows
  for (t in seq_len(nrow(timeAtRisks))) {

    # Loop through PS adjustment methods
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment based on method type
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

      # Covariate Settings -------------------------------------------------------
      # Use default covariate settings with descendant exclusion enabled
      # Analysis specifications indicate no specific concepts to include/exclude
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcome List Configuration -----------------------------------------------
      # Combine outcomes of interest with negative controls
      # Outcomes of interest: outcomeOfInterest = TRUE, trueEffectSize = NA
      # Negative controls: outcomeOfInterest = FALSE, trueEffectSize = 1 (null effect)
      # priorOutcomeLookBack = 99999 from createStudyPopArgs.priorOutcomeLookBack
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
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
      
      # Target-Comparator-Outcomes List ------------------------------------------
      # Create target-comparator-outcomes objects for each T-C pair
      # Exclude target and comparator concept IDs from covariates to avoid confounding
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

      # Get Database Arguments ---------------------------------------------------
      # From getDbCohortMethodDataArgs in analysis specifications
      # restrictToCommonPeriod = FALSE (per createStudyPopArgs.restrictToCommonPeriod)
      # studyStartDate and studyEndDate from studyPeriods
      # maxCohortSize = 0 means no limit (from getDbCohortMethodDataArgs.maxCohortSize)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Create Propensity Score Arguments ----------------------------------------
      # From propensityScoreAdjustment.createPsArgs in analysis specifications
      # maxCohortSizeForFitting = 250000
      # errorOnHighCorrelation = FALSE
      # prior: Laplace with cross-validation (useCrossValidation = TRUE)
      # control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10, etc.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = FALSE,
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
          resetCoefficients = FALSE,
          tolerance = 2e-07,
          cvRepetitions = 10,
          startingVariance = 0.01,
          fold = 10
        )
      )

      # Covariate Balance Arguments ----------------------------------------------
      # Compute covariate balance for diagnostics
      # Shared balance: no filter (all covariates)
      # Standard balance: default Table 1 specifications
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit Outcome Model Arguments ----------------------------------------------
      # From fitOutcomeModelArgs in analysis specifications
      # modelType = "cox" (Cox proportional hazards model)
      # stratified = FALSE (analysis specification)
      # useCovariates = FALSE (no covariate adjustment in outcome model)
      # inversePtWeighting = FALSE
      # prior: Laplace with cross-validation
      # control: tolerance = 2e-7, cvType = "auto", resetCoefficients = TRUE, etc.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
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
      
      # Create Study Population Arguments ----------------------------------------
      # From createStudyPopArgs in analysis specifications
      # restrictToCommonPeriod = FALSE
      # firstExposureOnly = FALSE
      # washoutPeriod = 365 days
      # removeDuplicateSubjects = "keep all"
      # censorAtNewRiskWindow = FALSE
      # removeSubjectsWithPriorOutcome = TRUE
      # priorOutcomeLookBack = 99999 days
      # TAR settings from timeAtRisks iteration
      # minDaysAtRisk = 1 day (from timeAtRisks)
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

      # Append Analysis Configuration --------------------------------------------
      # Create a CmAnalysis object combining all settings above
      # Description includes study period, TAR, and PS method for tracking
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
# Combine all CM analyses into module specifications
# refitPsForEveryOutcome = FALSE (reuse PS across outcomes)
# refitPsForEveryStudyPopulation = FALSE (reuse PS across study populations)
# Use default diagnostic thresholds
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create Complete Analysis Specifications -------------------------------------
# Assemble all modules and shared resources into final specification
# Order: shared resources first, then modules (CohortGenerator, CohortDiagnostics, CohortMethod)
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save Analysis Specifications -------------------------------------------------
# Export the complete analysis specifications to JSON file
# File location: inst/semaglutideandnaion/semaglutideandnaionAnalysisSpecification.json
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "semaglutideandnaion", "semaglutideandnaionAnalysisSpecification.json")
)