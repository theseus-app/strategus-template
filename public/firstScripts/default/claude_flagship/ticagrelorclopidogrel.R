################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates analysis specifications for a comparative effectiveness
# study comparing ticagrelor vs clopidogrel using the OHDSI Strategus framework.
# 
# Study Name: ticagrelorclopidogrel
# Target Cohort: target1 (ID: 1794126)
# Comparator Cohort: comparator1 (ID: 1794132)
# Outcome Cohort: outcome1 (ID: 1794131)
# Negative Control Concept Set: negative (ID: 1888110)
#
# Study Periods:
#   1) 2011-11-01 to 2019-03-31
#   2) 2013-03-01 to 2016-12-31
#
# Time-at-Risk Windows:
#   - 1 to 365 days from cohort start
#   - 1 to 1825 days from cohort start
#   - 1 day from cohort start to cohort end
#   - 29 to 365 days from cohort start
#   - 29 to 1825 days from cohort start
#   - 29 days from cohort start to cohort end
#
# Propensity Score Adjustments:
#   - 1:1 matching (caliper 0.2, standardized logit)
#   - 1:10 variable ratio matching (caliper 0.2, standardized logit)
#   - Stratification (10 strata, all subjects)
#
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# further details.
################################################################################

library(dplyr)
library(Strategus)

# ==============================================================================
# Shared Resources
# ==============================================================================
# Define the base URL for the OHDSI WebAPI to retrieve cohort definitions
# and concept sets from ATLAS
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ------------------------------------------------------------------------------
# Cohort Definitions
# ------------------------------------------------------------------------------
# Export cohort definitions from ATLAS using the specified cohort IDs:
# - Target cohort (target1): ID 1794126
# - Comparator cohort (comparator1): ID 1794132
# - Outcome cohort (outcome1): ID 1794131
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to use sequential IDs for easier reference in the analysis
# This maps the original ATLAS IDs to simpler sequential IDs (1, 2, 3)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# ------------------------------------------------------------------------------
# Negative Control Outcomes
# ------------------------------------------------------------------------------
# Retrieve the negative control concept set from ATLAS
# Negative controls are outcomes known to have no causal relationship with the
# exposure, used to detect residual confounding and systematic bias
# Concept Set ID: 1888110 (name: negative)
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
  # Assign cohort IDs starting from 101 to avoid conflicts with main cohorts
  # Target/comparator cohort IDs are 1, 2, 3; negative controls start at 101
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Verify no duplicate cohort IDs exist between main cohorts and negative controls
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# ------------------------------------------------------------------------------
# Define Cohorts for Analysis
# ------------------------------------------------------------------------------
# Outcomes of interest for the study
# cleanWindow: lookback period (in days) for identifying prior outcomes
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator cohorts for CohortMethod analysis
# This defines the comparison: target1 (ticagrelor) vs comparator1 (clopidogrel)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# ------------------------------------------------------------------------------
# Covariate Exclusions
# ------------------------------------------------------------------------------
# Define concepts to exclude from covariate analysis
# Note: conceptsToExclude in specifications has null id, so no specific exclusions
# If specific concepts were provided, they would be listed here
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Note: conceptsToInclude in specifications has null id, so no specific inclusions
# If you want to define covariates to include instead of including them all,
# uncomment and populate the following:
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# ==============================================================================
# CohortGeneratorModule Settings
# ==============================================================================
# The CohortGenerator module is responsible for creating cohorts in the CDM
# It generates the target, comparator, outcome, and negative control cohorts
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource specifications for negative control outcome cohorts
# occurrenceType = "first": use only the first occurrence of each negative control
# detectOnDescendants = TRUE: include descendant concepts when identifying outcomes
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications with statistics generation enabled
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ==============================================================================
# CohortDiagnosticsModule Settings
# ==============================================================================
# The CohortDiagnostics module provides comprehensive diagnostics for cohorts
# including inclusion statistics, concept analysis, and characterization
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE,        # Analyze inclusion rule statistics
  runIncludedSourceConcepts = TRUE,     # Identify source concepts in cohorts
  runOrphanConcepts = TRUE,             # Find potentially missing concepts
  runTimeSeries = FALSE,                # Skip time series analysis
  runVisitContext = TRUE,               # Analyze visit context of cohort entries
  runBreakdownIndexEvents = TRUE,       # Break down index events by concept
  runIncidenceRate = TRUE,              # Calculate incidence rates
  runCohortRelationship = TRUE,         # Analyze relationships between cohorts
  runTemporalCohortCharacterization = TRUE,  # Temporal characterization
  minCharacterizationMean = 0.01        # Minimum mean for characterization features
)

# ==============================================================================
# CohortMethodModule Settings
# ==============================================================================

# ------------------------------------------------------------------------------
# Study Periods
# ------------------------------------------------------------------------------
# Define the study periods for the analysis
# Two study periods are specified:
#   1) November 1, 2011 to March 31, 2019
#   2) March 1, 2013 to December 31, 2016
studyPeriods <- tibble(
  studyStartDate = c("20111101", "20130301"),
  studyEndDate   = c("20190331", "20161231")
)

# ------------------------------------------------------------------------------
# Time-at-Risk Windows
# ------------------------------------------------------------------------------
# Define multiple time-at-risk (TAR) windows for outcome assessment
# Each TAR specifies when to start and stop counting outcomes relative to exposure
# 
# Six TAR configurations based on specifications:
#   1) 1-365 days from cohort start (1-year follow-up)
#   2) 1-1825 days from cohort start (5-year follow-up)
#   3) 1 day from cohort start to cohort end (on-treatment)
#   4) 29-365 days from cohort start (excluding first 28 days)
#   5) 29-1825 days from cohort start (excluding first 28 days, 5-year)
#   6) 29 days from cohort start to cohort end (on-treatment, excluding first 28 days)
#
# minDaysAtRisk = 1 for all configurations as specified
timeAtRisks <- tibble(
  label = c(
    "TAR 1-365 from start",
    "TAR 1-1825 from start",
    "TAR 1-cohort end",
    "TAR 29-365 from start",
    "TAR 29-1825 from start",
    "TAR 29-cohort end"
  ),
  riskWindowStart = c(1, 1, 1, 29, 29, 29),
  startAnchor = c("cohort start", "cohort start", "cohort start", 
                  "cohort start", "cohort start", "cohort start"),
  riskWindowEnd = c(365, 1825, 0, 365, 1825, 0),
  endAnchor = c("cohort start", "cohort start", "cohort end",
                "cohort start", "cohort start", "cohort end"),
  minDaysAtRisk = c(1, 1, 1, 1, 1, 1)
)

# ------------------------------------------------------------------------------
# Propensity Score Settings - Match on PS
# ------------------------------------------------------------------------------
# Define propensity score matching configurations
# Two matching strategies:
#   1) 1:1 matching with caliper 0.2 on standardized logit scale
#   2) 1:10 variable ratio matching with caliper 0.2 on standardized logit scale
matchOnPsArgsList <- tibble(
  label = c("1:1 PS Match", "1:10 PS Match"),
  maxRatio = c(1, 10),
  caliper = c(0.2, 0.2),
  caliperScale = c("standardized logit", "standardized logit")
)

# ------------------------------------------------------------------------------
# Propensity Score Settings - Stratify by PS
# ------------------------------------------------------------------------------
# Define propensity score stratification configuration
# One stratification strategy:
#   - 10 strata with all subjects included (baseSelection = "all")
stratifyByPsArgsList <- tibble(
  label = c("10 Strata"),
  numberOfStrata = c(10),
  baseSelection = c("all")
)

# ------------------------------------------------------------------------------
# Build PS Configuration List
# ------------------------------------------------------------------------------
# Combine matching and stratification configurations into a single list
# Each entry contains: method (match/stratify), label, and parameters
psConfigList <- list()

# Add matching configurations to the list
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

# Add stratification configurations to the list
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

# ------------------------------------------------------------------------------
# Build CohortMethod Analysis List
# ------------------------------------------------------------------------------
# Iterate through all combinations of:
#   - Study periods (2)
#   - Time-at-risk windows (6)
#   - PS adjustment methods (3: two matching + one stratification)
# Total: 2 x 6 x 3 = 36 analyses
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment based on method type
      if (psCfg$method == "match") {
        # Create matching arguments for PS matching
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # Create stratification arguments for PS stratification
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Create default covariate settings
      # addDescendantsToExclude = TRUE ensures descendant concepts are also excluded
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build outcome list combining outcomes of interest and negative controls
      # Outcomes of interest have trueEffectSize = NA (unknown)
      # Negative controls have trueEffectSize = 1 (null effect expected)
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
      
      # Create target-comparator-outcomes list
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Configure data extraction from the CDM
      # Settings from getDbCohortMethodDataArgs in specifications:
      #   - restrictToCommonPeriod = TRUE
      #   - maxCohortSize = 0 (no limit)
      #   - firstExposureOnly = FALSE
      #   - washoutPeriod = 0
      #   - removeDuplicateSubjects = "keep first"
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Configure propensity score model creation
      # Settings from createPsArgs in specifications:
      #   - maxCohortSizeForFitting = 250000
      #   - errorOnHighCorrelation = TRUE
      #   - prior: Laplace with cross-validation
      #   - control: tolerance 2e-7, auto CV, 10 folds, 10 repetitions
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
          fold = 10,
          startingVariance = 0.01
        )
      )

      # Configure covariate balance computation for shared covariates
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      
      # Configure covariate balance computation with Table 1 specifications
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Configure outcome model fitting
      # Settings from fitOutcomeModelArgs in specifications:
      #   - modelType = "cox" (Cox proportional hazards)
      #   - stratified = TRUE
      #   - useCovariates = FALSE
      #   - inversePtWeighting = FALSE
      #   - prior: Laplace with cross-validation
      #   - control: tolerance 2e-7, auto CV, 10 folds, 10 repetitions, quiet
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
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
      
      # Configure study population creation
      # Settings from createStudyPopArgs in specifications:
      #   - restrictToCommonPeriod = FALSE
      #   - firstExposureOnly = FALSE
      #   - washoutPeriod = 0
      #   - removeDuplicateSubjects = "keep all"
      #   - censorAtNewRiskWindow = FALSE
      #   - removeSubjectsWithPriorOutcome = FALSE
      #   - priorOutcomeLookBack = 99999
      #   - Time-at-risk settings from current iteration
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = FALSE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999
      )

      # Create the CohortMethod analysis object with all configured settings
      # Description includes study period, TAR, and PS method for identification
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

# ==============================================================================
# Create the Analysis Specifications
# ==============================================================================
# Combine all module specifications into a single analysis specification object
# This includes:
#   - Shared resources (cohort definitions, negative controls)
#   - CohortGenerator module (creates cohorts)
#   - CohortDiagnostics module (diagnostic analyses)
#   - CohortMethod module (comparative effectiveness analysis)
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file
# This file can be used to execute the study using Strategus
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ticagrelorclopidogrel", "ticagrelorclopidogrelAnalysisSpecification.json")
)