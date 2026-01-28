################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates analysis specifications for the "alzheimerdonepezil" study
# using the OHDSI Strategus package.
#
# Study Overview:
# - Target Cohort: target1 (ID: 1794126)
# - Comparator Cohort: comparator1 (ID: 1794132)
# - Outcome Cohort: outcome1 (ID: 1794131)
# - Negative Control Concept Set: negative (ID: 1888110)
#
# Propensity Score Adjustment:
# - Two matching strategies: 1:1 and 1:3 matching with caliper 0.2 on standardized logit
#
# Time-at-Risk:
# - Day 1 to Day 180 from cohort start
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

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from ATLAS WebAPI
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions from ATLAS for:
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

# Re-number cohorts to use sequential IDs starting from 1
# This simplifies cohort management within the analysis
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve the negative control concept set (ID: 1888110) from ATLAS
# These are used to assess residual confounding and calibrate p-values
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
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Verify no duplicate cohort IDs exist between main cohorts and negative controls
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold cohorts for each analysis -------------------------

# Outcomes of interest
# cleanWindow: 365 days - period to look back for prior outcomes
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Defines the comparison: target1 vs comparator1
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Excluded covariate concepts
# Note: No specific concepts to exclude were provided in the specifications
# (conceptsToExclude id is null), so we create an empty data frame
# If you need to exclude specific drug concepts, add them here
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# Optional: Included covariate concepts
# Note: No specific concepts to include were provided in the specifications
# (conceptsToInclude id is null), so this is commented out
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# This module generates the cohorts defined above in the target database
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource for negative control outcome cohorts
# occurrenceType = "first": Use first occurrence of the outcome
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
# This module runs diagnostic analyses on the cohorts
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE,      # Statistics on inclusion rule impact
  runIncludedSourceConcepts = TRUE,   # Source concepts included in cohort
  runOrphanConcepts = TRUE,           # Concepts that may be missing from definition
  runTimeSeries = FALSE,              # Time series of cohort entry
  runVisitContext = TRUE,             # Visit context at cohort entry
  runBreakdownIndexEvents = TRUE,     # Breakdown of index events
  runIncidenceRate = TRUE,            # Incidence rate calculations
  runCohortRelationship = TRUE,       # Relationships between cohorts
  runTemporalCohortCharacterization = TRUE,  # Temporal characterization
  minCharacterizationMean = 0.01      # Minimum mean for characterization features
)

# CohortMethodModule -----------------------------------------------------------
# This module performs the comparative cohort analysis

# Study periods configuration
# Note: studyStartDate and studyEndDate are empty in specifications,
# meaning no restriction on study period
# restrictToCommonPeriod = TRUE will be used to restrict to overlapping observation periods
studyPeriods <- tibble(
  studyStartDate = c(""),  # Empty string means no start date restriction
  studyEndDate   = c("")   # Empty string means no end date restriction
)

# Time-at-risks (TARs) for the outcomes of interest
# Based on specifications:
# - riskWindowStart: 1 (day 1 after cohort start)
# - startAnchor: "cohort start"
# - riskWindowEnd: 180 (day 180 after cohort start)
# - endAnchor: "cohort start"
# - minDaysAtRisk: 1
timeAtRisks <- tibble(
  label = c("TAR 1-180 days"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(180),
  endAnchor = c("cohort start")
)

# Propensity Score settings - match on PS
# Two matching configurations from specifications:
# 1. 1:1 matching with caliper 0.2 on standardized logit scale
# 2. 1:3 matching with caliper 0.2 on standardized logit scale
matchOnPsArgsList <- tibble(
  label = c("1:1 PS Matching", "1:3 PS Matching"),
  maxRatio  = c(1, 3),
  caliper = c(0.2, 0.2),
  caliperScale  = c("standardized logit", "standardized logit")
)

# Propensity Score settings - stratify by PS
# Note: stratifyByPsArgs is null in specifications, so no stratification is used
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0)
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# If a data frame for "match on PS" exists and has rows, convert each row to a config
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

# If a data frame for "stratify by PS" exists and has rows, convert each row to a config
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

# Iterate through all analysis setting combinations
# This creates a full factorial design across:
# - Study periods (1 period with no date restrictions)
# - Time-at-risk windows (1 TAR: days 1-180)
# - PS adjustment methods (2 matching strategies: 1:1 and 1:3)
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
        # PS Matching configuration
        # allowReverseMatch = FALSE: Only match target to comparator, not vice versa
        # stratificationColumns = c(): No additional stratification variables
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

      # Covariate settings for propensity score model
      # Uses default covariates with descendants added to exclusion list
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome list combining outcomes of interest and negative controls
      outcomeList <- append(
        # Outcomes of interest (outcome1)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,      # This is a primary outcome
            trueEffectSize = NA,           # Unknown true effect
            priorOutcomeLookback = 99999   # Look back period for prior outcomes
          )
        }),
        # Negative control outcomes (for calibration)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,     # Not a primary outcome
            trueEffectSize = 1             # True effect is null (HR = 1)
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
          # Exclude treatment concepts from covariates to avoid confounding
          excludedCovariateConceptIds = c(
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # Arguments for extracting cohort method data from database
      # Based on getDbCohortMethodDataArgs from specifications:
      # - restrictToCommonPeriod: TRUE (restrict to overlapping observation periods)
      # - maxCohortSize: 0 (no limit)
      # - firstExposureOnly: FALSE (include all exposures)
      # - washoutPeriod: 0 (no washout required)
      # - removeDuplicateSubjects: "keep all"
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores
      # Based on createPsArgs from specifications:
      # - maxCohortSizeForFitting: 250000
      # - errorOnHighCorrelation: TRUE
      # - prior: Laplace with cross-validation
      # - control: tolerance 2e-7, auto CV, 10 folds, 10 repetitions
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations
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
          cvRepetitions = 10,  # From specifications
          fold = 10,           # From specifications
          startingVariance = 0.01
        )
      )

      # Arguments for computing covariate balance (shared across outcomes)
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      
      # Arguments for computing covariate balance (per outcome)
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting outcome model
      # Based on fitOutcomeModelArgs from specifications:
      # - modelType: "logistic" (logistic regression)
      # - stratified: TRUE (stratified analysis)
      # - useCovariates: FALSE (no outcome model covariates)
      # - inversePtWeighting: FALSE (not using IPW)
      # - prior: Laplace with cross-validation
      # - control: tolerance 2e-7, auto CV, 10 folds, 10 repetitions, quiet noise
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "logistic",
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
          cvRepetitions = 10,  # From specifications
          fold = 10,           # From specifications
          noiseLevel = "quiet"
        )
      )
      
      # Arguments for creating study population
      # Based on createStudyPopArgs from specifications:
      # - restrictToCommonPeriod: FALSE
      # - firstExposureOnly: FALSE
      # - washoutPeriod: 0
      # - removeDuplicateSubjects: "keep all"
      # - censorAtNewRiskWindow: FALSE
      # - removeSubjectsWithPriorOutcome: TRUE
      # - priorOutcomeLookBack: 99999
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

      # Append the settings to Analysis List
      # Each analysis is uniquely identified by analysisId and described by
      # the combination of study period, TAR, and PS adjustment method
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
  refitPsForEveryOutcome = FALSE,           # Use same PS for all outcomes
  refitPsForEveryStudyPopulation = FALSE,   # Use same PS for all study populations
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the analysis specifications ------------------------------------------
# Combine all modules into a single analysis specification
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "alzheimerdonepezil", "alzheimerdonepezilAnalysisSpecification.json")
)