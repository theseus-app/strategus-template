################################################################################
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# further details.
# ##############################################################################
library(dplyr)
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
# Base URL for the WebAPI instance to retrieve cohort definitions and concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions from WebAPI based on IDs provided in Analysis Specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal use within the study package.
# Target cohort ID is mapped to 1.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# Comparator cohort ID is mapped to 2.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# Outcome cohort ID is mapped to 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve negative control concept set definition from WebAPI.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Negative control concept set ID from Analysis Specifications.
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
  # Assign unique cohort IDs for negative controls, starting from 101 to avoid
  # collision with target/comparator/outcome (1, 2, 3).
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs between study cohorts and negative controls.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Filter for the re-numbered outcome cohort (ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName)

# Target and Comparator for the CohortMethod analysis
# Use the re-numbered target (ID 1) and comparator (ID 2) cohorts.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study. The Analysis Specifications indicate empty conceptsToExclude.
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(0), # Empty as per Analysis Specifications
  conceptName = character(0) # Empty as per Analysis Specifications
)

# Optional: If you want to define covariates to include instead of including them all
# The Analysis Specifications indicate empty conceptsToInclude, so this remains commented.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# Create shared resource specifications for cohort definitions and negative controls.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default setting
  detectOnDescendants = TRUE # Default setting
)
# Create module specifications for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Generate cohort statistics.
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# Create module specifications for CohortDiagnostics.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # Run diagnostics for all defined cohorts.
  runInclusionStatistics = TRUE,
  runIncludedSourceConcepts = TRUE,
  runOrphanConcepts = TRUE,
  runTimeSeries = FALSE, # Default setting
  runVisitContext = TRUE,
  runBreakdownIndexEvents = TRUE,
  runIncidenceRate = TRUE,
  runCohortRelationship = TRUE,
  runTemporalCohortCharacterization = TRUE,
  minCharacterizationMean = 0.01 # Default setting
)

# CohortMethodModule -----------------------------------------------------------

# Study periods from Analysis Specifications.
studyPeriods <- tibble(
  studyStartDate = c("20130101"), # YYYYMMDD format
  studyEndDate   = c("20201231")  # YYYYMMDD format
)

# Time-at-risks (TARs) for the outcomes of interest in your study from Analysis Specifications.
timeAtRisks <- tibble(
  label = c("1-730_days_after_cohort_start"), # Descriptive label for the TAR.
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd  = c(730),
  endAnchor = c("cohort start") # "cohort start" | "cohort end"
)

# Propensity Score settings - match on PS from Analysis Specifications.
matchOnPsArgsList <- tibble(
  label = c("MatchOnPs_MaxRatio1_Caliper0.05"), # Descriptive label for PS matching.
  maxRatio  = c(1), # Max ratio for matching.
  caliper = c(0.05), # Caliper for matching.
  caliperScale  = c("propensity score") # "propensity score" | "standardized" | "standardized logit"
)

# Propensity Score settings - stratify by PS. Analysis Specifications has this as null.
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = numeric(0),
  baseSelection = character(0) # "all" | "target" | "comparator"
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# If a data frame for "match on PS" exists and has rows, convert each row to a config.
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

# If a data frame for "stratify by PS" exists and has rows, convert each row to a config.
# This block will not execute as stratifyByPsArgsList is empty.
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
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Configure PS adjustment arguments based on the method (match or stratify).
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default setting
          stratificationColumns = c() # Default setting
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default setting
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Default covariate settings. Analysis Specifications has empty covariateSelection.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default setting
      )

      # Create a list of outcomes, including the study outcome and negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not applicable for observed outcomes.
            priorOutcomeLookback = 99999 # From Analysis Specifications (createStudyPopArgs)
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # For negative controls, true effect size is assumed to be 1.
          )
        })
      )

      # Define target-comparator-outcome combinations.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Excluded covariate concept IDs. Since 'conceptsToExclude' is empty in Analysis Specifications,
          # and target/comparator concept IDs are not provided, this will be an empty numeric vector.
          excludedCovariateConceptIds = c(
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # Arguments for fetching cohort method data from the database.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # From Analysis Specifications.
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From Analysis Specifications.
        covariateSettings = covariateSettings
        # Note: firstExposureOnly, washoutPeriod, removeDuplicateSubjects from Analysis Specifications
        # under getDbCohortMethodDataArgs are applied in createStudyPopArgs as per CohortMethod package structure.
      )

      # Arguments for creating propensity scores.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications.
        errorOnHighCorrelation = TRUE, # From Analysis Specifications.
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att", # Default setting
        prior = Cyclops::createPrior( # Prior settings from Analysis Specifications.
          priorType = "laplace",
          exclude = c(0), # Default setting
          useCrossValidation = TRUE # From Analysis Specifications.
        ),
        control = Cyclops::createControl( # Control settings from Analysis Specifications.
          noiseLevel = "silent",
          cvType = "auto",
          seed = 1, # Default setting
          resetCoefficients = TRUE,
          tolerance = 2e-07,
          cvRepetitions = 10, # From Analysis Specifications (template had 1).
          startingVariance = 0.01
        )
      )

      # Arguments for computing shared covariate balance.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default setting
        covariateFilter = NULL # Default setting
      )
      # Arguments for computing covariate balance.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default setting
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default setting
      )

      # Arguments for fitting the outcome model.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From Analysis Specifications.
        stratified = TRUE, # From Analysis Specifications.
        useCovariates = FALSE, # From Analysis Specifications.
        inversePtWeighting = FALSE, # From Analysis Specifications.
        prior = Cyclops::createPrior( # Prior settings from Analysis Specifications.
          priorType = "laplace",
          useCrossValidation = TRUE # From Analysis Specifications.
        ),
        control = Cyclops::createControl( # Control settings from Analysis Specifications.
          cvType = "auto",
          seed = 1, # Default setting
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          tolerance = 2e-07,
          cvRepetitions = 10, # From Analysis Specifications (template had 1).
          noiseLevel = "quiet"
        )
      )

      # Arguments for creating the study population.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications.
        firstExposureOnly = FALSE, # From Analysis Specifications.
        washoutPeriod = 0, # From Analysis Specifications.
        removeDuplicateSubjects = "keep all", # From Analysis Specifications (template had "keep first").
        censorAtNewRiskWindow = FALSE, # From Analysis Specifications (template had TRUE).
        removeSubjectsWithPriorOutcome = TRUE, # From Analysis Specifications.
        priorOutcomeLookback = 99999, # From Analysis Specifications.
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1, # From Analysis Specifications.
        maxDaysAtRisk = 99999 # Default setting
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

# Create module specifications for CohortMethod.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # No analyses to exclude.
  refitPsForEveryOutcome = FALSE, # Default setting
  refitPsForEveryStudyPopulation = FALSE, # Default setting
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default diagnostic thresholds.
)

# Create the analysis specifications ------------------------------------------
# Initialize empty analysis specifications and add shared resources and module specifications.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "glp1radepression", "glp1radepressionAnalysisSpecification.json")
)