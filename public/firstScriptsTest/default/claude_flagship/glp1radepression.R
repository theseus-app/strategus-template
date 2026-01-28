################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates the analysis specifications for the glp1radepression study
# using the OHDSI Strategus package.
# 
# Study Overview:
# - Target Cohort: target1 (ID: 1794126)
# - Comparator Cohort: comparator1 (ID: 1794132)
# - Outcome Cohort: outcome1 (ID: 1794131)
# - Negative Control Concept Set: negative (ID: 1888110)
# - Study Period: 2013-01-01 to 2020-12-31
# - Time-at-Risk: Day 1 to Day 730 from cohort start
# - PS Adjustment: 1:1 matching with caliper 0.05 on propensity score scale
# - Outcome Model: Stratified Cox regression
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
# These cohorts are defined in the Analysis Specifications:
# - Target: target1 (ID: 1794126)
# - Comparator: comparator1 (ID: 1794132)
# - Outcome: outcome1 (ID: 1794131)
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
# This simplifies referencing cohorts throughout the analysis
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1  # Target
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2  # Comparator
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3  # Outcome

# Negative Control Outcomes ----------------------------------------------------
# Retrieve the negative control concept set from ATLAS
# Negative controls are used to detect residual confounding and systematic bias
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
  # Main cohorts use IDs 1, 2, 3; negative controls use 101, 102, 103, etc.
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Validate that there are no duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create Data Frames for Analysis Configuration --------------------------------

# Outcomes of Interest
# Define the outcome cohorts to be analyzed
# cleanWindow: lookback period (in days) for removing subjects with prior outcomes
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%  # Filter to outcome cohort (outcome1)
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator Cohorts for CohortMethod Analysis
# This defines the treatment comparison: target1 vs comparator1
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Excluded Covariate Concepts --------------------------------------------------
# Note: The Analysis Specifications indicate no specific concepts to exclude
# (conceptsToExclude has null id). However, it's good practice to exclude
# the drugs being compared to avoid confounding.
# If specific concept IDs were provided, they would be listed here.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# CohortGeneratorModule --------------------------------------------------------
# This module generates the cohorts defined above in the target database
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource for negative control outcome cohorts
# occurrenceType = "first": Use only the first occurrence of each negative control outcome
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

# CohortDiagnosticsModule Settings ---------------------------------------------
# This module runs diagnostic analyses on the cohorts to assess their quality
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE,        # Statistics on inclusion rule impact
  runIncludedSourceConcepts = TRUE,     # Source concepts included in cohort
  runOrphanConcepts = TRUE,             # Concepts that might be missing
  runTimeSeries = FALSE,                # Time series of cohort entry
  runVisitContext = TRUE,               # Visit context of cohort entries
  runBreakdownIndexEvents = TRUE,       # Breakdown of index events
  runIncidenceRate = TRUE,              # Incidence rate calculations
  runCohortRelationship = TRUE,         # Relationships between cohorts
  runTemporalCohortCharacterization = TRUE,  # Temporal characterization
  minCharacterizationMean = 0.01        # Minimum mean for characterization features
)

# CohortMethodModule -----------------------------------------------------------
# This module performs the comparative cohort analysis using propensity score methods

# Study Periods ----------------------------------------------------------------
# Define the study observation window
# From Analysis Specifications: studyStartDate = "20130101", studyEndDate = "20201231"
studyPeriods <- tibble(
  studyStartDate = c("20130101"),  # January 1, 2013
  studyEndDate   = c("20201231")   # December 31, 2020
)

# Time-at-Risk Windows ---------------------------------------------------------
# Define when outcomes are counted relative to treatment initiation
# From Analysis Specifications:
# - riskWindowStart: 1 (day 1 after cohort start)
# - riskWindowEnd: 730 (day 730 = ~2 years)
# - startAnchor/endAnchor: "cohort start"
# - minDaysAtRisk: 1
timeAtRisks <- tibble(
  label = c("TAR_1_730"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(730),
  endAnchor = c("cohort start")
)

# Propensity Score Settings - Match on PS --------------------------------------
# From Analysis Specifications:
# - maxRatio: 1 (1:1 matching)
# - caliper: 0.05
# - caliperScale: "propensity score"
matchOnPsArgsList <- tibble(
  label = c("PS_Match_1to1_0.05"),
  maxRatio  = c(1),
  caliper = c(0.05),
  caliperScale  = c("propensity score")
)

# Propensity Score Settings - Stratify by PS -----------------------------------
# From Analysis Specifications: stratifyByPsArgs is null, so no stratification
# Creating empty tibble as stratification is not used in this analysis
stratifyByPsArgsList <- tibble(
  label = character(),
  numberOfStrata  = numeric(),
  baseSelection = character()
)

# Build PS Configuration List --------------------------------------------------
# This combines all PS adjustment methods into a single list for iteration
psConfigList <- list()

# Add matching configurations if defined
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

# Add stratification configurations if defined
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
        # PS Matching Configuration
        # From Analysis Specifications:
        # - maxRatio: 1 (1:1 matching)
        # - caliper: 0.05
        # - caliperScale: "propensity score"
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # PS Stratification Configuration (not used in this analysis)
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate Settings ------------------------------------------------------
      # Use default covariate settings from FeatureExtraction
      # addDescendantsToExclude: Include descendant concepts when excluding covariates
      # Note: No specific concepts to include or exclude per Analysis Specifications
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcome List -----------------------------------------------------------
      # Combine outcomes of interest with negative control outcomes
      outcomeList <- append(
        # Outcomes of interest (outcome1)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,  # Unknown true effect
            priorOutcomeLookback = 99999  # From Analysis Specifications
          )
        }),
        # Negative control outcomes (true effect size = 1, i.e., no effect)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1  # Null effect expected for negative controls
          )
        })
      )
      
      # Target-Comparator-Outcomes List ----------------------------------------
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude covariate concepts to avoid confounding
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Get Database Cohort Method Data Arguments ------------------------------
      # From Analysis Specifications (getDbCohortMethodDataArgs):
      # - studyStartDate: "20130101"
      # - studyEndDate: "20201231"
      # - maxCohortSize: 0 (no limit)
      # - restrictToCommonPeriod: TRUE
      # - firstExposureOnly: FALSE
      # - washoutPeriod: 0
      # - removeDuplicateSubjects: "keep all"
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Create Propensity Score Arguments --------------------------------------
      # From Analysis Specifications (createPsArgs):
      # - maxCohortSizeForFitting: 250000
      # - errorOnHighCorrelation: TRUE
      # - prior: Laplace with cross-validation
      # - control: tolerance 2e-7, cvType "auto", fold 10, cvRepetitions 10
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
          fold = 10,
          cvRepetitions = 10,
          startingVariance = 0.01
        )
      )

      # Covariate Balance Arguments --------------------------------------------
      # Settings for computing covariate balance before and after PS adjustment
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit Outcome Model Arguments --------------------------------------------
      # From Analysis Specifications (fitOutcomeModelArgs):
      # - modelType: "cox"
      # - stratified: TRUE
      # - useCovariates: FALSE
      # - inversePtWeighting: FALSE
      # - prior: Laplace with cross-validation
      # - control: tolerance 2e-7, cvType "auto", fold 10, cvRepetitions 10, noiseLevel "quiet"
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
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet"
        )
      )
      
      # Create Study Population Arguments --------------------------------------
      # From Analysis Specifications (createStudyPopArgs):
      # - restrictToCommonPeriod: FALSE
      # - firstExposureOnly: FALSE
      # - washoutPeriod: 0
      # - removeDuplicateSubjects: "keep all"
      # - censorAtNewRiskWindow: FALSE
      # - removeSubjectsWithPriorOutcome: TRUE
      # - priorOutcomeLookBack: 99999
      # - Time-at-risk: Day 1 to Day 730 from cohort start
      # - minDaysAtRisk: 1
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
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

      # Create CohortMethod Analysis -------------------------------------------
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
# Combine all modules and shared resources into the final analysis specification
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the Analysis Specifications to JSON -------------------------------------
# The output file will be saved to the inst/glp1radepression directory
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "glp1radepression", "glp1radepressionAnalysisSpecification.json")
)