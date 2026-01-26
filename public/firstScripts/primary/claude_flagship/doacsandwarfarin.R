################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates analysis specifications for the "doacsandwarfarin" study
# using the OHDSI Strategus package.
# 
# Study Design:
# - Target Cohort: target1 (ID: 1794126)
# - Comparator Cohort: comparator1 (ID: 1794132)
# - Outcome Cohort: outcome1 (ID: 1794131)
# - Negative Control Concept Set: negative (ID: 1888110)
# - Study Period: 2010-10-19 to 2018-12-31
# - Propensity Score Adjustment: 1:1 matching with caliper 0.2 on standardized logit
# - Outcome Model: Cox regression (unstratified, no covariates)
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
################################################################################

library(dplyr)
library(Strategus)

# ==============================================================================
# Shared Resources
# ==============================================================================
# Define the base URL for the OHDSI WebAPI to retrieve cohort definitions
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ------------------------------------------------------------------------------
# Cohort Definitions
# ------------------------------------------------------------------------------
# Export cohort definitions from ATLAS using the specified cohort IDs:
# - Target cohort (target1): 1794126
# - Comparator cohort (comparator1): 1794132
# - Outcome cohort (outcome1): 1794131
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to sequential IDs for internal use
# This ensures consistent referencing throughout the analysis
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1  # Target
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2  # Comparator
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3  # Outcome

# ------------------------------------------------------------------------------
# Negative Control Outcomes
# ------------------------------------------------------------------------------
# Retrieve negative control concepts from the concept set defined in ATLAS
# Concept Set ID: 1888110 (name: negative)
# These are used for empirical calibration to assess residual bias
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,  # negative
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
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Validate that there are no duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# ==============================================================================
# Analysis Configuration Data Frames
# ==============================================================================

# ------------------------------------------------------------------------------
# Outcomes List
# ------------------------------------------------------------------------------
# Define the outcome cohorts for the analysis
# cleanWindow: lookback period for removing subjects with prior outcomes (365 days)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%  # Outcome cohort (outcome1)
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# ------------------------------------------------------------------------------
# Target and Comparator List for CohortMethod Analysis
# ------------------------------------------------------------------------------
# Define the target-comparator pairs for the comparative effectiveness analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# ------------------------------------------------------------------------------
# Excluded Covariate Concepts
# ------------------------------------------------------------------------------
# Note: No specific concepts to exclude were provided in the specifications
# (conceptsToExclude id is null). If needed, add concept IDs here.
# These would typically include the drugs being compared to avoid confounding.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Note: No specific concepts to include were provided in the specifications
# (conceptsToInclude id is null). The analysis will use default covariates.

# ==============================================================================
# CohortGeneratorModule Settings
# ==============================================================================
# This module generates the cohorts defined above in the target database
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource specifications for negative control outcome cohorts
# occurrenceType = "first": Use first occurrence of the negative control outcome
# detectOnDescendants = TRUE: Include descendant concepts when identifying outcomes
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
# This module runs comprehensive diagnostics on the generated cohorts
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE,        # Statistics on inclusion rule impact
  runIncludedSourceConcepts = TRUE,     # Source concepts included in cohort
  runOrphanConcepts = TRUE,             # Concepts that may be missing from definitions
  runTimeSeries = FALSE,                # Time series of cohort counts
  runVisitContext = TRUE,               # Visit context of index events
  runBreakdownIndexEvents = TRUE,       # Breakdown of index events by concept
  runIncidenceRate = TRUE,              # Incidence rates over time
  runCohortRelationship = TRUE,         # Relationships between cohorts
  runTemporalCohortCharacterization = TRUE,  # Temporal characterization
  minCharacterizationMean = 0.01        # Minimum mean for characterization features
)

# ==============================================================================
# CohortMethodModule Settings
# ==============================================================================

# ------------------------------------------------------------------------------
# Study Periods
# ------------------------------------------------------------------------------
# Define the study observation period
# From specifications: studyStartDate = "20101019", studyEndDate = "20181231"
studyPeriods <- tibble(
  studyStartDate = c("20101019"),  # October 19, 2010
  studyEndDate   = c("20181231")   # December 31, 2018
)

# ------------------------------------------------------------------------------
# Time-at-Risk (TAR) Settings
# ------------------------------------------------------------------------------
# Define when outcomes are counted relative to exposure
# From specifications:
# - riskWindowStart: 1 (day after cohort start)
# - startAnchor: "cohort start"
# - riskWindowEnd: 0 (at cohort end)
# - endAnchor: "cohort end"
# - minDaysAtRisk: 1
timeAtRisks <- tibble(
  label = c("On Treatment"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
)

# ------------------------------------------------------------------------------
# Propensity Score Settings - Match on PS
# ------------------------------------------------------------------------------
# From specifications:
# - maxRatio: 1 (1:1 matching)
# - caliper: 0.2
# - caliperScale: "standardized logit"
matchOnPsArgsList <- tibble(
  label = c("1:1 PS Matching"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# ------------------------------------------------------------------------------
# Propensity Score Settings - Stratify by PS
# ------------------------------------------------------------------------------
# From specifications: stratifyByPsArgs is null, so no stratification
stratifyByPsArgsList <- tibble(
  label = character(),
  numberOfStrata  = numeric(),
  baseSelection = character()
)

# ------------------------------------------------------------------------------
# Build PS Configuration List
# ------------------------------------------------------------------------------
# Combine matching and stratification configurations into a single list
psConfigList <- list()

# Add matching configurations if they exist
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

# Add stratification configurations if they exist
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
# Iterate through all combinations of study periods, time-at-risks, and PS settings
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment method based on the configuration
      if (psCfg$method == "match") {
        # PS Matching configuration
        # From specifications: maxRatio=1, caliper=0.2, caliperScale="standardized logit"
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # PS Stratification configuration (not used in this study)
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # ----------------------------------------------------------------------
      # Covariate Settings
      # ----------------------------------------------------------------------
      # Use default covariate settings from FeatureExtraction
      # addDescendantsToExclude = TRUE: When excluding concepts, also exclude descendants
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # ----------------------------------------------------------------------
      # Outcome List
      # ----------------------------------------------------------------------
      # Combine outcomes of interest with negative control outcomes
      outcomeList <- append(
        # Outcomes of interest (outcome1)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,  # Unknown true effect
            priorOutcomeLookback = 99999  # From specifications: priorOutcomeLookBack = 99999
          )
        }),
        # Negative control outcomes (for empirical calibration)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1  # True effect is null (HR = 1)
          )
        })
      )

      # ----------------------------------------------------------------------
      # Target-Comparator-Outcomes List
      # ----------------------------------------------------------------------
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # ----------------------------------------------------------------------
      # Get Database Cohort Method Data Arguments
      # ----------------------------------------------------------------------
      # From specifications:
      # - studyStartDate: "20101019"
      # - studyEndDate: "20181231"
      # - maxCohortSize: 0 (no limit)
      # - restrictToCommonPeriod: false (from createStudyPopArgs, applied here as well)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,  # From specifications: restrictToCommonPeriod = false
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,  # From specifications: maxCohortSize = 0 (no limit)
        covariateSettings = covariateSettings
      )

      # ----------------------------------------------------------------------
      # Create Propensity Score Arguments
      # ----------------------------------------------------------------------
      # From specifications:
      # - maxCohortSizeForFitting: 250000
      # - errorOnHighCorrelation: true
      # - prior: laplace with cross-validation
      # - control: tolerance=2e-7, cvType="auto", fold=10, cvRepetitions=10, etc.
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # From specifications
        errorOnHighCorrelation = TRUE,      # From specifications
        stopOnError = FALSE,  # Allow Strategus to complete all operations even if model fitting fails
        estimator = "att",    # Average treatment effect on the treated
        prior = Cyclops::createPrior(
          priorType = "laplace",           # From specifications: priorType = "laplace"
          exclude = c(0),
          useCrossValidation = TRUE        # From specifications: useCrossValidation = true
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",           # From specifications: noiseLevel = "silent"
          cvType = "auto",                 # From specifications: cvType = "auto"
          seed = 1,
          resetCoefficients = TRUE,        # From specifications: resetCoefficients = true
          tolerance = 2e-07,               # From specifications: tolerance = 2e-7
          cvRepetitions = 10,              # From specifications: cvRepetitions = 10
          startingVariance = 0.01,         # From specifications: startingVariance = 0.01
          fold = 10                        # From specifications: fold = 10
        )
      )

      # ----------------------------------------------------------------------
      # Compute Covariate Balance Arguments
      # ----------------------------------------------------------------------
      # Settings for computing covariate balance before and after PS adjustment
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL  # Include all covariates
      )
      
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()  # Table 1 covariates
      )

      # ----------------------------------------------------------------------
      # Fit Outcome Model Arguments
      # ----------------------------------------------------------------------
      # From specifications:
      # - modelType: "cox"
      # - stratified: false
      # - useCovariates: false
      # - inversePtWeighting: false
      # - prior: laplace with cross-validation
      # - control: tolerance=2e-7, cvType="auto", fold=10, cvRepetitions=10, etc.
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",                 # From specifications: modelType = "cox"
        stratified = FALSE,                # From specifications: stratified = false
        useCovariates = FALSE,             # From specifications: useCovariates = false
        inversePtWeighting = FALSE,        # From specifications: inversePtWeighting = false
        prior = Cyclops::createPrior(
          priorType = "laplace",           # From specifications: priorType = "laplace"
          useCrossValidation = TRUE        # From specifications: useCrossValidation = true
        ),
        control = Cyclops::createControl(
          cvType = "auto",                 # From specifications: cvType = "auto"
          seed = 1,
          resetCoefficients = TRUE,        # From specifications: resetCoefficients = true
          startingVariance = 0.01,         # From specifications: startingVariance = 0.01
          tolerance = 2e-07,               # From specifications: tolerance = 2e-7
          cvRepetitions = 10,              # From specifications: cvRepetitions = 10
          noiseLevel = "quiet",            # From specifications: noiseLevel = "quiet"
          fold = 10                        # From specifications: fold = 10
        )
      )

      # ----------------------------------------------------------------------
      # Create Study Population Arguments
      # ----------------------------------------------------------------------
      # From specifications:
      # - restrictToCommonPeriod: false
      # - firstExposureOnly: true
      # - washoutPeriod: 365
      # - removeDuplicateSubjects: "keep all"
      # - censorAtNewRiskWindow: false
      # - removeSubjectsWithPriorOutcome: true
      # - priorOutcomeLookBack: 99999
      # - riskWindowStart: 1, startAnchor: "cohort start"
      # - riskWindowEnd: 0, endAnchor: "cohort end"
      # - minDaysAtRisk: 1
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,           # From specifications
        firstExposureOnly = TRUE,                 # From specifications: firstExposureOnly = true
        washoutPeriod = 365,                      # From specifications: washoutPeriod = 365
        removeDuplicateSubjects = "keep all",     # From specifications: removeDuplicateSubjects = "keep all"
        censorAtNewRiskWindow = FALSE,            # From specifications: censorAtNewRiskWindow = false
        removeSubjectsWithPriorOutcome = TRUE,    # From specifications: removeSubjectsWithPriorOutcome = true
        priorOutcomeLookback = 99999,             # From specifications: priorOutcomeLookBack = 99999
        riskWindowStart = timeAtRisks$riskWindowStart[t],  # From specifications: 1
        startAnchor = timeAtRisks$startAnchor[t],          # From specifications: "cohort start"
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],      # From specifications: 0
        endAnchor = timeAtRisks$endAnchor[t],              # From specifications: "cohort end"
        minDaysAtRisk = 1,                        # From specifications: minDaysAtRisk = 1
        maxDaysAtRisk = 99999
      )

      # ----------------------------------------------------------------------
      # Create CohortMethod Analysis
      # ----------------------------------------------------------------------
      # Combine all settings into a single analysis specification
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

# ------------------------------------------------------------------------------
# Create CohortMethod Module Specifications
# ------------------------------------------------------------------------------
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
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# ==============================================================================
# Save the Analysis Specifications to JSON
# ==============================================================================
# Save the complete analysis specifications to a JSON file for execution
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "doacsandwarfarin", "doacsandwarfarinAnalysisSpecification.json")
)