################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates analysis specifications for the sglt2imetformin study
# using the OHDSI Strategus package.
# 
# Study Design:
# - Target Cohort: target1 (ID: 1794126)
# - Comparator Cohort: comparator1 (ID: 1794132)
# - Outcome Cohort: outcome1 (ID: 1794131)
# - Negative Control Concept Set: negative (ID: 1888110)
# - Study Period: 2013-04-01 to 2020-03-31
# - PS Adjustment: Matching with maxRatio=2, caliper=0.2, standardized logit scale
# - Outcome Model: Cox proportional hazards, stratified
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Define the base URL for the OHDSI WebAPI to retrieve cohort definitions
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export cohort definitions from ATLAS using the specified cohort IDs
# These cohorts define the target, comparator, and outcome populations
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to use sequential IDs for internal processing
# This ensures consistent referencing throughout the analysis
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1  # Target
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2  # Comparator
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3  # Outcome

# Negative Control Outcomes ----------------------------------------------------
# Retrieve negative control concepts from the specified concept set
# Negative controls are used to detect potential systematic bias in the study
# They are outcomes with no expected causal relationship to the exposure
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
  # Target/comparator cohort IDs are 1, 2, 3; negative controls start at 101, 102, 103...
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Validate that there are no duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create Data Frames for Analysis Configuration --------------------------------

# Outcomes of Interest ---------------------------------------------------------
# Define the outcome cohorts to be analyzed
# cleanWindow: lookback period (in days) for removing subjects with prior outcomes
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%  # Filter to outcome cohort (outcome1)
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator Cohorts ------------------------------------------------
# Define the target-comparator pairs for the CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Excluded Covariate Concepts --------------------------------------------------
# For the CohortMethod large-scale propensity score (LSPS), we need to exclude
# the drugs of interest to prevent them from being used as covariates
# Note: No specific concepts were provided in the specifications for exclusion
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# CohortGeneratorModule --------------------------------------------------------
# This module generates the cohorts defined above in the target database
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource specifications for negative control outcome cohorts
# occurrenceType = "first": Use only the first occurrence of each negative control outcome
# detectOnDescendants = TRUE: Include descendant concepts when identifying outcomes
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications for cohort generation
# generateStats = TRUE: Generate inclusion rule statistics for cohort diagnostics
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# This module runs comprehensive diagnostics on the generated cohorts
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE,        # Statistics on inclusion rule impact
  runIncludedSourceConcepts = TRUE,     # Source concepts included in cohort
  runOrphanConcepts = TRUE,             # Concepts that may be missing from definition
  runTimeSeries = FALSE,                # Time series of cohort entry
  runVisitContext = TRUE,               # Visit context at cohort entry
  runBreakdownIndexEvents = TRUE,       # Breakdown of index events
  runIncidenceRate = TRUE,              # Incidence rate calculations
  runCohortRelationship = TRUE,         # Relationships between cohorts
  runTemporalCohortCharacterization = TRUE,  # Temporal characterization
  minCharacterizationMean = 0.01        # Minimum mean for characterization features
)

# CohortMethodModule -----------------------------------------------------------
# This module performs the comparative cohort analysis using propensity score methods

# Study Period Configuration ---------------------------------------------------
# Define the study period based on the analysis specifications
# studyStartDate: 2013-04-01 (YYYYMMDD format)
# studyEndDate: 2020-03-31 (YYYYMMDD format)
studyPeriods <- tibble(
  studyStartDate = c("20130401"),
  studyEndDate   = c("20200331")
)

# Time-at-Risk Configuration ---------------------------------------------------
# Define the time-at-risk windows for outcome assessment
# Based on specifications:
# - riskWindowStart: 1 day after cohort start
# - riskWindowEnd: 0 days relative to cohort end (i.e., at cohort end)
# - minDaysAtRisk: 1 day minimum exposure required
timeAtRisks <- tibble(
  label = c("On Treatment"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
)

# Propensity Score Settings - Match on PS --------------------------------------
# Configure propensity score matching parameters
# Based on specifications:
# - maxRatio: 2 (up to 2 comparators matched per target)
# - caliper: 0.2 (maximum allowed difference in PS)
# - caliperScale: "standardized logit" (caliper applied on standardized logit scale)
matchOnPsArgsList <- tibble(
  label = c("PS Matching 1:2"),
  maxRatio  = c(2),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# Propensity Score Settings - Stratify by PS -----------------------------------
# No stratification settings specified in the analysis specifications
# stratifyByPsArgs is set to NULL in the specifications
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c()
)

# Build PS Configuration List --------------------------------------------------
# Combine all PS adjustment methods into a single configuration list
psConfigList <- list()

# Process "match on PS" configurations if they exist
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    # Append a new element at the end of psConfigList
    psConfigList[[length(psConfigList) + 1]] <- list(
      # Identify the PS adjustment method for this config
      method = "match",
      # Human-readable label to carry through into descriptions
      label  = matchOnPsArgsList$label[i],
      # Parameter bundle passed to createMatchOnPsArgs later
      params = list(
        maxRatio     = matchOnPsArgsList$maxRatio[i],
        caliper      = matchOnPsArgsList$caliper[i],
        caliperScale = matchOnPsArgsList$caliperScale[i]
      )
    )
  }
}

# Process "stratify by PS" configurations if they exist
if (exists("stratifyByPsArgsList") && nrow(stratifyByPsArgsList) > 0) {
  for (i in seq_len(nrow(stratifyByPsArgsList))) {
    # Append a new element at the end of psConfigList
    psConfigList[[length(psConfigList) + 1]] <- list(
      # Identify the PS adjustment method for this config
      method = "stratify",
      # Human-readable label to carry through into descriptions
      label  = stratifyByPsArgsList$label[i],
      # Parameter bundle passed to createStratifyByPsArgs later
      params = list(
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection  = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}

# Build CohortMethod Analysis List ---------------------------------------------
# Iterate through all combinations of study periods, time-at-risk windows, and PS settings
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment method based on the current configuration
      if (psCfg$method == "match") {
        # Create matching arguments for propensity score matching
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # Create stratification arguments for propensity score stratification
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate Settings -------------------------------------------------------
      # Use default covariate settings for propensity score estimation
      # addDescendantsToExclude = TRUE: Also exclude descendant concepts of excluded concepts
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcome List Configuration -----------------------------------------------
      # Create outcome objects for both outcomes of interest and negative controls
      outcomeList <- append(
        # Outcomes of interest (from oList)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,      # This is a primary outcome
            trueEffectSize = NA,           # Unknown true effect (what we're estimating)
            priorOutcomeLookback = 99999   # Based on specifications: priorOutcomeLookBack = 99999
          )
        }),
        # Negative control outcomes (for bias detection)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,     # Not a primary outcome
            trueEffectSize = 1             # Expected null effect (HR = 1)
          )
        })
      )
      
      # Target-Comparator-Outcomes Configuration ---------------------------------
      # Link target and comparator cohorts with their outcomes
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude treatment-related concepts from covariates to avoid confounding
          excludedCovariateConceptIds = c(
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # Database Cohort Method Data Arguments ------------------------------------
      # Configure how to extract data from the database
      # Based on specifications:
      # - restrictToCommonPeriod: TRUE (from createStudyPopArgs)
      # - studyStartDate/studyEndDate: From studyPeriods
      # - maxCohortSize: 0 (no limit)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Propensity Score Model Arguments -----------------------------------------
      # Configure the propensity score estimation model
      # Based on specifications:
      # - maxCohortSizeForFitting: 250000
      # - errorOnHighCorrelation: TRUE
      # - Prior: Laplace with cross-validation
      # - Control: tolerance=2e-7, cvType="auto", fold=10, cvRepetitions=10
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,  # Allow Strategus to complete all operations even if model fitting fails
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

      # Covariate Balance Arguments ----------------------------------------------
      # Configure covariate balance computation for diagnostics
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome Model Arguments --------------------------------------------------
      # Configure the outcome model for effect estimation
      # Based on specifications:
      # - modelType: "cox" (Cox proportional hazards)
      # - stratified: TRUE (stratified by matched sets)
      # - useCovariates: FALSE (no additional covariate adjustment)
      # - inversePtWeighting: FALSE (not using IPW)
      # - Prior: Laplace with cross-validation
      # - Control: tolerance=2e-7, cvType="auto", fold=10, cvRepetitions=10
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
      
      # Study Population Arguments -----------------------------------------------
      # Configure the study population definition
      # Based on specifications:
      # - restrictToCommonPeriod: TRUE
      # - firstExposureOnly: FALSE
      # - washoutPeriod: 0
      # - removeDuplicateSubjects: "keep all"
      # - censorAtNewRiskWindow: FALSE
      # - removeSubjectsWithPriorOutcome: TRUE
      # - priorOutcomeLookBack: 99999
      # - Time-at-risk: riskWindowStart=1, startAnchor="cohort start", 
      #                 riskWindowEnd=0, endAnchor="cohort end", minDaysAtRisk=1
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
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

      # Create CohortMethod Analysis Object --------------------------------------
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

# Create CohortMethod Module Specifications ------------------------------------
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the Analysis Specifications -------------------------------------------
# Combine all module specifications into a single analysis specification object
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the Analysis Specifications to JSON -------------------------------------
# Save the complete analysis specifications to a JSON file for execution
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "sglt2imetformin", "sglt2imetforminAnalysisSpecification.json")
)