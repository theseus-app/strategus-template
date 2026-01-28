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
# 
# Analysis includes:
# - Two study periods: 2013-04-01 to 2020-03-31 and 2013-04-01 to 2018-12-31
# - Two time-at-risk windows
# - Propensity score matching with 1:2 ratio
# - Cox proportional hazards model
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
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative Control Outcomes ----------------------------------------------------
# Retrieve negative control concepts from the specified concept set
# Negative controls are used to detect potential systematic bias in the study
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
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  # Assign cohort IDs starting from 101 to avoid conflicts with main cohorts
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Validate that there are no duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create Data Frames for Analysis Configuration --------------------------------

# Outcomes of Interest
# Define the outcome cohorts with a clean window of 365 days
# The clean window is used for removing subjects with prior outcomes
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator Cohorts for CohortMethod Analysis
# Defines the treatment groups being compared
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Excluded Covariate Concepts --------------------------------------------------
# No specific concepts to exclude as per specifications (conceptsToExclude is null)
# If concepts were specified, they would be excluded from the propensity score model
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# CohortGeneratorModule --------------------------------------------------------
# This module generates the cohorts defined above in the target database
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource specifications for negative control outcome cohorts
# occurrenceType = "first" means only the first occurrence of each outcome is considered
# detectOnDescendants = TRUE includes descendant concepts in the detection
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
  runInclusionStatistics = TRUE,      # Statistics on inclusion rule impact
  runIncludedSourceConcepts = TRUE,   # Source concepts included in cohort
  runOrphanConcepts = TRUE,           # Concepts that may be missing
  runTimeSeries = FALSE,              # Time series of cohort counts
  runVisitContext = TRUE,             # Visit context of cohort entries
  runBreakdownIndexEvents = TRUE,     # Breakdown of index events
  runIncidenceRate = TRUE,            # Incidence rate calculations
  runCohortRelationship = TRUE,       # Relationships between cohorts
  runTemporalCohortCharacterization = TRUE,  # Temporal characterization
  minCharacterizationMean = 0.01      # Minimum mean for characterization features
)

# CohortMethodModule -----------------------------------------------------------
# This module performs the comparative effectiveness analysis using propensity scores

# Study Periods ----------------------------------------------------------------
# Define the time windows for the study
# Two study periods are specified as per the analysis specifications:
# 1. Full period: 2013-04-01 to 2020-03-31
# 2. Earlier period: 2013-04-01 to 2018-12-31
studyPeriods <- tibble(
  studyStartDate = c("20130401", "20130401"),
  studyEndDate   = c("20200331", "20181231")
)

# Time-at-Risk Windows ---------------------------------------------------------
# Define when outcomes are counted relative to exposure
# Two TAR configurations as per specifications:
# 1. On-treatment: from day 1 after cohort start to cohort end
# 2. Intent-to-treat: from day 1 after cohort start to day 99999 (essentially unlimited)
timeAtRisks <- tibble(
  label = c("On-treatment", "Intent-to-treat"),
  riskWindowStart  = c(1, 1),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd  = c(0, 99999),
  endAnchor = c("cohort end", "cohort start"),
  minDaysAtRisk = c(1, 1)
)

# Propensity Score Settings - Match on PS --------------------------------------
# Configure propensity score matching parameters
# maxRatio = 2: Up to 2 comparators matched to each target
# caliper = 0.2: Maximum allowed difference in propensity scores
# caliperScale = "standardized logit": Caliper applied on standardized logit scale
matchOnPsArgsList <- tibble(
  label = c("PS Matching 1:2"),
  maxRatio  = c(2),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# Propensity Score Settings - Stratify by PS -----------------------------------
# No stratification specified in this analysis (stratifyByPsArgs is null)
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0)
)

# Build PS Configuration List --------------------------------------------------
# Combine all PS adjustment methods into a single configuration list
psConfigList <- list()

# Add matching configurations if specified
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

# Add stratification configurations if specified
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

# Build Analysis List ----------------------------------------------------------
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

      # Covariate Settings ------------------------------------------------------
      # Use default covariate settings with descendant concepts included in exclusions
      # No specific concepts to include or exclude as per specifications
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcome List ------------------------------------------------------------
      # Create outcome objects for both outcomes of interest and negative controls
      outcomeList <- append(
        # Outcomes of interest (for hypothesis testing)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,  # Unknown true effect
            priorOutcomeLookback = 99999  # As specified in createStudyPopArgs
          )
        }),
        # Negative control outcomes (for bias detection, true effect size = 1)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1  # Null effect expected
          )
        })
      )
      
      # Target-Comparator-Outcomes List -----------------------------------------
      # Link target and comparator cohorts with outcomes
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Get Database Cohort Method Data Arguments -------------------------------
      # Configure how data is extracted from the database
      # restrictToCommonPeriod = TRUE: Restrict to period where both cohorts have data
      # maxCohortSize = 0: No limit on cohort size
      # firstExposureOnly = FALSE: Include all exposures (as per specifications)
      # washoutPeriod = 0: No washout period required (as per specifications)
      # removeDuplicateSubjects = "keep all": Keep all subjects (as per specifications)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        covariateSettings = covariateSettings
      )

      # Create Propensity Score Arguments ---------------------------------------
      # Configure the propensity score model fitting
      # Uses Lasso regularization with cross-validation as specified
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # As specified
        errorOnHighCorrelation = TRUE,      # As specified
        stopOnError = FALSE,  # Allow Strategus to complete all operations
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",           # As specified
          exclude = c(0),
          useCrossValidation = TRUE        # As specified
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",           # As specified
          cvType = "auto",                 # As specified
          seed = 1,
          resetCoefficients = TRUE,        # As specified
          tolerance = 2e-07,               # As specified
          cvRepetitions = 10,              # As specified
          fold = 10,                       # As specified
          startingVariance = 0.01          # As specified
        )
      )

      # Covariate Balance Arguments ---------------------------------------------
      # Configure covariate balance computation for diagnostics
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit Outcome Model Arguments ---------------------------------------------
      # Configure the outcome model (Cox proportional hazards)
      # modelType = "cox": Cox regression as specified
      # stratified = TRUE: Stratified analysis as specified
      # useCovariates = FALSE: No additional covariates in outcome model
      # inversePtWeighting = FALSE: Not using IPW as specified
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",                 # As specified
        stratified = TRUE,                 # As specified
        useCovariates = FALSE,             # As specified
        inversePtWeighting = FALSE,        # As specified
        prior = Cyclops::createPrior(
          priorType = "laplace",           # As specified
          useCrossValidation = TRUE        # As specified
        ),
        control = Cyclops::createControl(
          cvType = "auto",                 # As specified
          seed = 1,
          resetCoefficients = TRUE,        # As specified
          startingVariance = 0.01,         # As specified
          tolerance = 2e-07,               # As specified
          cvRepetitions = 10,              # As specified
          fold = 10,                       # As specified
          noiseLevel = "quiet"             # As specified
        )
      )
      
      # Create Study Population Arguments ---------------------------------------
      # Configure how the study population is created
      # removeSubjectsWithPriorOutcome = TRUE: Exclude subjects with prior outcome
      # priorOutcomeLookBack = 99999: Look back period for prior outcomes
      # censorAtNewRiskWindow = FALSE: As specified
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,           # As specified
        firstExposureOnly = FALSE,                # As specified
        washoutPeriod = 0,                        # As specified
        removeDuplicateSubjects = "keep all",     # As specified
        censorAtNewRiskWindow = FALSE,            # As specified
        removeSubjectsWithPriorOutcome = TRUE,    # As specified
        priorOutcomeLookback = 99999,             # As specified
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999
      )

      # Create CohortMethod Analysis Object -------------------------------------
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
# The JSON file can be used to execute the analysis using Strategus
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "sglt2imetformin", "sglt2imetforminAnalysisSpecification.json")
)