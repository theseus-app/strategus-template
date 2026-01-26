library(dplyr)
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
# library(purrr) # Not strictly needed after fixing covariate concept parsing, but harmless to keep if other parts might use it.

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
# Base URL for the WebAPI instance to retrieve cohort definitions and concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Retrieve cohort definitions for target, comparator, and outcome cohorts
# based on IDs provided in Analysis Specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to internal IDs for consistency within the study.
# Target cohort (ID 1794126) is mapped to internal ID 1.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# Comparator cohort (ID 1794132) is mapped to internal ID 2.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# Outcome cohort (ID 1794131) is mapped to internal ID 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve concept set definition for negative controls from Analysis Specifications.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # From Analysis Specifications: negativeControlConceptSet.id
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
  # Assign unique cohort IDs for negative controls, starting from 101 to avoid collision with T/C/O.
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs to prevent conflicts.
# The original script's fix for `duplicated` function usage is preserved.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Filter for the outcome cohort (internal ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # cleanWindow is not specified in Analysis Specifications, keeping template default.
  mutate(cleanWindow = 365) 

# Target and Comparator for the CohortMethod analysis 
# Populate with internal IDs and names for target (1) and comparator (2).
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = cohortDefinitionSet[cohortDefinitionSet$cohortId == 1,]$cohortName,
  comparatorCohortId = 2,
  comparatorCohortName = cohortDefinitionSet[cohortDefinitionSet$cohortId == 2,]$cohortName
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study. Reverting to template's static definition, as the original script
# prematurely tried to read from 'analysisSpecifications'.
excludedCovariateConcepts <- data.frame(
  conceptId = c(2345678, 3456789), # Example values from template
  conceptName = c("target concept name", "comparator concept name") # Example values from template
)

# Optional: If you want to define covariates to include instead of including them all
# Reverting to template's static definition (empty), as the original script
# prematurely tried to read from 'analysisSpecifications'.
includedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  # occurrenceType and detectOnDescendants are not in Analysis Specifications, keeping template defaults.
  occurrenceType = "first", 
  detectOnDescendants = TRUE
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  # Include all cohort IDs: target (1), comparator (2), outcome (3), and negative controls (101+).
  cohortIds = c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId),
  # Other Cohort Diagnostics settings are not specified in Analysis Specifications, keeping template defaults.
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

# If you are not restricting your study to a specific time window, 
# please make these strings empty. Populated with example data to make loops runnable.
studyPeriods <- tibble(
  studyStartDate = c("20000101"), # YYYYMMDD
  studyEndDate   = c("20201231")  # YYYYMMDD
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# Populated with example data to make loops runnable.
timeAtRisks <- tibble(
  label = c("1-365 days after cohort start"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd  = c(365),
  endAnchor = c("cohort start") # "cohort start" | "cohort end"
) 

# Propensity Score settings - match on PS.
# Populated with example data to make loops runnable.
matchOnPsArgsList <- tibble(
  label = c("Match on PS 1:1, caliper 0.2"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized") # "propensity score" | "standardized" | "standardized logit"
) 

# Propensity Score settings - stratify by PS.
# Populated with example data to make loops runnable.
stratifyByPsArgsList <- tibble(
  label = c("Stratify by PS 5 strata"),
  numberOfStrata  = c(5),
  baseSelection = c("all") # "all" | "target" | "comparator"
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

# Define outcomes for the CohortMethod analysis.
# Moved outside the loop as it's common for all analyses.
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

# Define target-comparator-outcome combinations.
# Moved outside the loop as it's common for all analyses.
targetComparatorOutcomesList <- list()
for (i in seq_len(nrow(cmTcList))) {
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    # Corrected: cmTcList does not have targetConceptId/comparatorConceptId.
    # Using only excludedCovariateConcepts$conceptId as in the original script's fix.
    excludedCovariateConceptIds = if (nrow(excludedCovariateConcepts) > 0) excludedCovariateConcepts$conceptId else c()
  )
}

# Iterate through all analysis setting combinations
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Not in Analysis Specifications, keeping template default.
          stratificationColumns = c() # Not in Analysis Specifications, keeping template default.
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Not in Analysis Specifications, keeping template default.
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings for FeatureExtraction.
      # Includes concepts to include/exclude from Analysis Specifications.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE, # Not in Analysis Specifications, keeping template default.
        excludedCovariateConceptIds = if (nrow(excludedCovariateConcepts) > 0) excludedCovariateConcepts$conceptId else c(),
        includedCovariateConceptIds = if (nrow(includedCovariateConcepts) > 0) includedCovariateConcepts$conceptId else c()
      )

      # Arguments for retrieving cohort method data.
      # Reverted to template's static values, as original script prematurely read from 'analysisSpecifications'.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Not in Analysis Specifications, keeping template default.
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From template
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # Reverted to template's static values, as original script prematurely read from 'analysisSpecifications'.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From template
        errorOnHighCorrelation = TRUE, # From template
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att", # From template
        prior = Cyclops::createPrior( # Prior settings from template.
          priorType = "laplace", 
          exclude = c(0), 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl( # Control settings from template.
          noiseLevel = "silent", 
          cvType = "auto", 
          seed = 1, 
          resetCoefficients = TRUE, 
          tolerance = 2e-07, 
          cvRepetitions = 1, 
          startingVariance = 0.01
        )
      )

      # Arguments for computing shared covariate balance. Not in Analysis Specifications, keeping template defaults.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      # Arguments for computing covariate balance. Not in Analysis Specifications, keeping template defaults.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting the outcome model.
      # Reverted to template's static values, as original script prematurely read from 'analysisSpecifications'.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From template
        stratified = TRUE, # From template
        useCovariates = FALSE, # From template
        inversePtWeighting = FALSE, # From template
        prior = Cyclops::createPrior( # Prior settings from template.
          priorType = "laplace", 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl( # Control settings from template.
          cvType = "auto", 
          seed = 1, 
          resetCoefficients = TRUE,
          startingVariance = 0.01, 
          tolerance = 2e-07, 
          cvRepetitions = 1, 
          noiseLevel = "quiet"
        )
      )
      
      # Arguments for creating the study population.
      # Reverted to template's static values, as original script prematurely read from 'analysisSpecifications'.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From template
        firstExposureOnly = FALSE, # From template
        washoutPeriod = 0, # From template
        removeDuplicateSubjects = "keep first", # From template
        censorAtNewRiskWindow = TRUE, # From template
        removeSubjectsWithPriorOutcome = TRUE, # From template
        priorOutcomeLookback = 99999, # From template
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From timeAtRisks (derived from Analysis Specifications).
        startAnchor = timeAtRisks$startAnchor[t], # From timeAtRisks (derived from Analysis Specifications).
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From timeAtRisks (derived from Analysis Specifications).
        endAnchor = timeAtRisks$endAnchor[t], # From timeAtRisks (derived from Analysis Specifications).
        minDaysAtRisk = 1, # From template (original script tried to read from non-existent timeAtRisks$minDaysAtRisk)
        maxDaysAtRisk = 99999 # From template
      )

      # Append the settings to Analysis List
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

cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList, # Now correctly defined once before the loops
  analysesToExclude = NULL, # Not in Analysis Specifications, keeping template default.
  refitPsForEveryOutcome = FALSE, # Not in Analysis Specifications, keeping template default.
  refitPsForEveryStudyPopulation = FALSE, # Not in Analysis Specifications, keeping template default.
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Not in Analysis Specifications, keeping template default.
)

# Create the analysis specifications ------------------------------------------
# This object is created here, not read from.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the generated analysis specifications to a JSON file.
# Using template's placeholder 'studyName'.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "studyName", "studyNameAnalysisSpecification.json")
)