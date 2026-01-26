################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates the analysis specifications for the "ceeamos" study
# using the OHDSI Strategus package.
# 
# Analysis Overview:
# - Target Cohort: target1 (ID: 1794126)
# - Comparator Cohort: comparator1 (ID: 1794132)
# - Outcome Cohort: outcome1 (ID: 1794131)
# - Negative Control Concept Set: negative (ID: 1888110)
# - Propensity Score Adjustment: Matching with maxRatio=10, caliper=0.2
# - Outcome Model: Cox proportional hazards, stratified
# - Two Time-at-Risk windows defined
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
# - Target: target1 (1794126)
# - Comparator: comparator1 (1794132)
# - Outcome: outcome1 (1794131)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to use sequential IDs for easier reference
# This maps the original ATLAS IDs to simpler sequential IDs:
# - Target (1794126) -> 1
# - Comparator (1794132) -> 2
# - Outcome (1794131) -> 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative Control Outcomes ----------------------------------------------------
# Retrieve the negative control concept set from ATLAS
# Negative controls are used to detect residual confounding and systematic bias
# Concept Set ID: 1888110 (name: "negative")
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
  # Assign cohort IDs starting at 101 to avoid conflicts with target/comparator/outcome cohorts
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Validate that there are no duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold cohorts for each analysis -------------------------

# Outcomes of Interest:
# Define the outcome cohort(s) for the study
# cleanWindow: lookback period for removing subjects with prior outcomes (365 days)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for CohortMethod Analysis:
# Defines the treatment comparison groups
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Excluded Covariate Concepts:
# Note: In the Analysis Specifications, conceptsToExclude has null/empty values
# If specific concepts need to be excluded, they would be defined here
# For this study, no specific concepts are excluded from covariates
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# CohortGeneratorModule --------------------------------------------------------
# This module generates the cohorts in the CDM database
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

# Create module specifications for cohort generation
# generateStats = TRUE: Generate inclusion rule statistics
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# This module runs diagnostic analyses on the generated cohorts
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
# This module performs the comparative cohort analysis

# Study Periods:
# From Analysis Specifications: studyStartDate and studyEndDate are empty strings
# This means no restriction on study period - use all available data
studyPeriods <- tibble(
  studyStartDate = c(""), # Empty string means no start date restriction
  studyEndDate = c("")    # Empty string means no end date restriction
)

# Time-at-Risk (TAR) Windows:
# From Analysis Specifications, two TAR windows are defined:
# 1. TAR1: From day 1 after cohort start to cohort end (on-treatment analysis)
# 2. TAR2: From day 1 after cohort start to day 99999 after cohort start (intent-to-treat analysis)
timeAtRisks <- tibble(
  label = c("OnTreatment", "IntentToTreat"),
  riskWindowStart = c(1, 1),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd = c(0, 99999),
  endAnchor = c("cohort end", "cohort start")
)

# Propensity Score Settings - Match on PS:
# From Analysis Specifications:
# - maxRatio: 10 (up to 10 comparators matched to each target)
# - caliper: 0.2 (maximum allowed difference in propensity score)
# - caliperScale: "standardized logit" (caliper applied on standardized logit of PS)
matchOnPsArgsList <- tibble(
  label = c("Match_1to10_Caliper0.2"),
  maxRatio = c(10),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# Propensity Score Settings - Stratify by PS:
# From Analysis Specifications: stratifyByPsArgs is null, so no stratification
# Creating empty tibble as placeholder
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata = integer(0),
  baseSelection = character(0)
)

# Build PS Configuration List --------------------------------------------------
# This combines all PS adjustment methods (matching and/or stratification) into a single list
psConfigList <- list()

# Add matching configurations if defined
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "match",
      label = matchOnPsArgsList$label[i],
      params = list(
        maxRatio = matchOnPsArgsList$maxRatio[i],
        caliper = matchOnPsArgsList$caliper[i],
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
      label = stratifyByPsArgsList$label[i],
      params = list(
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection = stratifyByPsArgsList$baseSelection[i]
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
        # PS Matching Configuration
        # From Analysis Specifications:
        # - maxRatio: 10
        # - caliper: 0.2
        # - caliperScale: "standardized logit"
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # PS Stratification Configuration
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }
      
      # Covariate Settings:
      # Using default covariate settings from FeatureExtraction
      # addDescendantsToExclude = TRUE: When excluding concepts, also exclude their descendants
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )
      
      # Outcome List:
      # Combines outcomes of interest and negative control outcomes
      # From Analysis Specifications:
      # - removeSubjectsWithPriorOutcome: TRUE
      # - priorOutcomeLookBack: 99999 days
      outcomeList <- append(
        # Outcomes of interest (outcome1)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,  # Unknown true effect size
            priorOutcomeLookback = 99999  # From Analysis Specifications
          )
        }),
        # Negative control outcomes (true effect size = 1, i.e., no effect expected)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1  # Negative controls should have no effect
          )
        })
      )
      
      # Target-Comparator-Outcomes List:
      # Links target and comparator cohorts with outcomes
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }
      
      # Get Database Cohort Method Data Arguments:
      # From Analysis Specifications (getDbCohortMethodDataArgs):
      # - maxCohortSize: 0 (no limit)
      # - restrictToCommonPeriod: FALSE
      # - firstExposureOnly: FALSE
      # - washoutPeriod: 0
      # - removeDuplicateSubjects: "keep first"
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,  # From Analysis Specifications
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,  # From Analysis Specifications (0 = no limit)
        covariateSettings = covariateSettings
      )
      
      # Create Propensity Score Arguments:
      # From Analysis Specifications (createPsArgs):
      # - maxCohortSizeForFitting: 250000
      # - errorOnHighCorrelation: TRUE
      # - prior: Laplace with cross-validation
      # - control: tolerance=2e-7, cvType="auto", fold=10, cvRepetitions=10
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # From Analysis Specifications
        errorOnHighCorrelation = TRUE,     # From Analysis Specifications
        stopOnError = FALSE,  # Allow Strategus to complete all operations even if model fitting fails
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",           # From Analysis Specifications
          exclude = c(0),
          useCrossValidation = TRUE        # From Analysis Specifications
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",           # From Analysis Specifications
          cvType = "auto",                 # From Analysis Specifications
          seed = 1,
          resetCoefficients = TRUE,        # From Analysis Specifications
          tolerance = 2e-07,               # From Analysis Specifications
          cvRepetitions = 10,              # From Analysis Specifications
          startingVariance = 0.01,         # From Analysis Specifications
          fold = 10                        # From Analysis Specifications
        )
      )
      
      # Compute Shared Covariate Balance Arguments:
      # Used to assess balance across all covariates
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      
      # Compute Covariate Balance Arguments:
      # Used to assess balance for Table 1 covariates
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )
      
      # Fit Outcome Model Arguments:
      # From Analysis Specifications (fitOutcomeModelArgs):
      # - modelType: "cox"
      # - stratified: TRUE
      # - useCovariates: FALSE
      # - inversePtWeighting: FALSE
      # - prior: Laplace with cross-validation
      # - control: tolerance=2e-7, cvType="auto", fold=10, cvRepetitions=10, noiseLevel="quiet"
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",                 # From Analysis Specifications
        stratified = TRUE,                 # From Analysis Specifications
        useCovariates = FALSE,             # From Analysis Specifications
        inversePtWeighting = FALSE,        # From Analysis Specifications
        prior = Cyclops::createPrior(
          priorType = "laplace",           # From Analysis Specifications
          useCrossValidation = TRUE        # From Analysis Specifications
        ),
        control = Cyclops::createControl(
          cvType = "auto",                 # From Analysis Specifications
          seed = 1,
          resetCoefficients = TRUE,        # From Analysis Specifications
          startingVariance = 0.01,         # From Analysis Specifications
          tolerance = 2e-07,               # From Analysis Specifications
          cvRepetitions = 10,              # From Analysis Specifications
          noiseLevel = "quiet",            # From Analysis Specifications
          fold = 10                        # From Analysis Specifications
        )
      )
      
      # Create Study Population Arguments:
      # From Analysis Specifications (createStudyPopArgs):
      # - restrictToCommonPeriod: FALSE
      # - firstExposureOnly: FALSE
      # - washoutPeriod: 0
      # - removeDuplicateSubjects: "keep all"
      # - censorAtNewRiskWindow: FALSE
      # - removeSubjectsWithPriorOutcome: TRUE
      # - priorOutcomeLookBack: 99999
      # - minDaysAtRisk: 1 (from timeAtRisks)
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,           # From Analysis Specifications
        firstExposureOnly = FALSE,                # From Analysis Specifications
        washoutPeriod = 0,                        # From Analysis Specifications
        removeDuplicateSubjects = "keep all",     # From Analysis Specifications
        censorAtNewRiskWindow = FALSE,            # From Analysis Specifications
        removeSubjectsWithPriorOutcome = TRUE,    # From Analysis Specifications
        priorOutcomeLookback = 99999,             # From Analysis Specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,                        # From Analysis Specifications (timeAtRisks)
        maxDaysAtRisk = 99999
      )
      
      # Create the CohortMethod Analysis object and append to the list
      # Each analysis represents a unique combination of:
      # - Study period
      # - Time-at-risk window
      # - Propensity score adjustment method
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(studyStartDate == "", "NoStart", studyStartDate),
          ifelse(studyEndDate == "", "NoEnd", studyEndDate),
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
# Combine all modules into a single analysis specification object
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the Analysis Specifications to JSON -------------------------------------
# The output file will be saved to inst/ceeamos/ceeamosAnalysisSpecification.json
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ceeamos", "ceeamosAnalysisSpecification.json")
)