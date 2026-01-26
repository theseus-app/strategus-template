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
library(ParallelLogger) # Required for saveSettingsToJson

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
# Base URL for the WebAPI instance where cohort definitions are stored.
# This is a placeholder and should be updated to your specific WebAPI instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions from WebAPI based on their IDs.
# The IDs are extracted from the <Analysis Specifications>.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to a simplified scheme (1, 2, 3) for internal use in the study.
# This makes it easier to refer to target, comparator, and outcome cohorts consistently.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortName <- "target1"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortName <- "comparator1"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortName <- "outcome1"

# Negative control outcomes
# Retrieve negative control concept set definition from WebAPI.
# The conceptSetId is extracted from <Analysis Specifications>.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # negativeControlConceptSet.id from <Analysis Specifications>
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
  # Assign unique cohort IDs for negative controls, starting from 101 to avoid collision
  # with target/comparator/outcome cohorts (1, 2, 3).
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs across study cohorts and negative controls.
# This is a critical check to ensure unique identifiers for all cohorts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Filter for the outcome cohort (ID 3 after re-numbering).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not specified in <Analysis Specifications>

# Target and Comparator for the CohortMethod analysis 
# Use the re-numbered target (1) and comparator (2) cohort IDs and names.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study. This list is derived from `covariateSelection.conceptsToExclude` in <Analysis Specifications>.
# Since `id` is null in the specifications, this list will be empty.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0), # No concepts to exclude specified in <Analysis Specifications>
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# This list is derived from `covariateSelection.conceptsToInclude` in <Analysis Specifications>.
# Since `id` is null in the specifications, this list will be empty.
includedCovariateConcepts <- data.frame(
  conceptId = integer(0), # No concepts to include specified in <Analysis Specifications>
  conceptName = character(0)
)

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default, not specified in <Analysis Specifications>
  detectOnDescendants = TRUE # Default, not specified in <Analysis Specifications>
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Default, not specified in <Analysis Specifications>
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE, # Default, not specified in <Analysis Specifications>
  runIncludedSourceConcepts = TRUE, # Default, not specified in <Analysis Specifications>
  runOrphanConcepts = TRUE, # Default, not specified in <Analysis Specifications>
  runTimeSeries = FALSE, # Default, not specified in <Analysis Specifications>
  runVisitContext = TRUE, # Default, not specified in <Analysis Specifications>
  runBreakdownIndexEvents = TRUE, # Default, not specified in <Analysis Specifications>
  runIncidenceRate = TRUE, # Default, not specified in <Analysis Specifications>
  runCohortRelationship = TRUE, # Default, not specified in <Analysis Specifications>
  runTemporalCohortCharacterization = TRUE, # Default, not specified in <Analysis Specifications>
  minCharacterizationMean = 0.01 # Default, not specified in <Analysis Specifications>
)

# CohortMethodModule -----------------------------------------------------------

# Study periods for restricting data extraction.
# Extracted from `getDbCohortMethodDataArgs.studyPeriods` in <Analysis Specifications>.
# `studyEndDate` is null in spec, so it's set to an empty string.
studyPeriods <- tibble(
  studyStartDate = c("20030101"), # YYYYMMDD from <Analysis Specifications>
  studyEndDate   = c("")          # YYYYMMDD from <Analysis Specifications> (null in spec means no end date restriction)
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# Extracted from `createStudyPopArgs.timeAtRisks` in <Analysis Specifications>.
timeAtRisks <- tibble(
  label = c("TAR_30_5475_CS_CS"), # Descriptive label for the TAR
  riskWindowStart  = c(30),        # From <Analysis Specifications>
  startAnchor = c("cohort start"), # From <Analysis Specifications>
  riskWindowEnd  = c(5475),       # From <Analysis Specifications>
  endAnchor = c("cohort start")    # From <Analysis Specifications>
) 

# Propensity Score settings - match on PS
# Extracted from `propensityScoreAdjustment.psSettings[0].matchOnPsArgs` in <Analysis Specifications>.
matchOnPsArgsList <- tibble(
  label = c("Match_MaxRatio1_Caliper0.2_StdLogit"), # Descriptive label for PS matching
  maxRatio  = c(1),                                 # From <Analysis Specifications>
  caliper = c(0.2),                                 # From <Analysis Specifications>
  caliperScale  = c("standardized logit")           # From <Analysis Specifications>
) 

# Propensity Score settings - stratify by PS
# `propensityScoreAdjustment.psSettings[0].stratifyByPsArgs` is null in <Analysis Specifications>,
# so this list remains empty.
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0)
) 

# Build a single PS configuration list (each entry has: method, label, params)
# This structure allows iterating through different PS adjustment methods.
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
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment arguments based on the method specified in psConfigList
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default, not specified in <Analysis Specifications>
          stratificationColumns = c() # Default, not specified in <Analysis Specifications>
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default, not specified in <Analysis Specifications>
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings.
      # Since `covariateSelection.conceptsToInclude` and `covariateSelection.conceptsToExclude`
      # are empty in <Analysis Specifications>, we use default settings.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default, not specified in <Analysis Specifications>
      )

      # Combine study outcomes and negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not applicable for observed outcomes
            priorOutcomeLookback = 99999 # Default, not specified in <Analysis Specifications>
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # For negative controls, true effect size is assumed to be 1
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
          # `excludedCovariateConceptIds` is empty as per `covariateSelection.conceptsToExclude`
          # in <Analysis Specifications>. The template's default inclusion of target/comparator
          # concept IDs is removed as it's not explicitly in the spec.
          excludedCovariateConceptIds = c(excludedCovariateConcepts$conceptId) 
        )
      }

      # Arguments for fetching cohort method data from the database.
      # `maxCohortSize` is from `getDbCohortMethodDataArgs.maxCohortSize` in <Analysis Specifications>.
      # `studyStartDate` and `studyEndDate` are from the current `studyPeriods` iteration.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Default, not specified in <Analysis Specifications>
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From <Analysis Specifications>
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # Parameters are extracted from `propensityScoreAdjustment.createPsArgs` in <Analysis Specifications>.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From <Analysis Specifications>
        errorOnHighCorrelation = TRUE,    # From <Analysis Specifications>
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att",   # Default, not specified in <Analysis Specifications>
        prior = Cyclops::createPrior( # Prior settings from <Analysis Specifications>
          priorType = "laplace", 
          exclude = c(0), # Default, not specified in <Analysis Specifications>
          useCrossValidation = TRUE # From <Analysis Specifications>
        ),
        control = Cyclops::createControl( # Control settings from <Analysis Specifications>
          noiseLevel = "silent", 
          cvType = "auto", 
          seed = 1, # Default, not specified in <Analysis Specifications>
          resetCoefficients = TRUE, 
          tolerance = 2e-07, 
          cvRepetitions = 10, # From <Analysis Specifications>
          startingVariance = 0.01,
          fold = 10 # From <Analysis Specifications>
        )
      )

      # Arguments for computing shared covariate balance.
      # Default values are used as not specified in <Analysis Specifications>.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      
      # Arguments for computing covariate balance.
      # Default values are used as not specified in <Analysis Specifications>.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting the outcome model.
      # Parameters are extracted from `fitOutcomeModelArgs` in <Analysis Specifications>.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",          # From <Analysis Specifications>
        stratified = FALSE,         # From <Analysis Specifications>
        useCovariates = FALSE,      # From <Analysis Specifications>
        inversePtWeighting = FALSE, # From <Analysis Specifications>
        prior = Cyclops::createPrior( # Prior settings from <Analysis Specifications>
          priorType = "laplace", 
          useCrossValidation = TRUE # From <Analysis Specifications>
        ),
        control = Cyclops::createControl( # Control settings from <Analysis Specifications>
          cvType = "auto", 
          seed = 1, # Default, not specified in <Analysis Specifications>
          resetCoefficients = TRUE,
          startingVariance = 0.01, 
          tolerance = 2e-07, 
          cvRepetitions = 10, # From <Analysis Specifications>
          noiseLevel = "quiet",
          fold = 10 # From <Analysis Specifications>
        )
      )
      
      # Arguments for creating the study population.
      # Parameters are extracted from `createStudyPopArgs` in <Analysis Specifications>.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From <Analysis Specifications>
        firstExposureOnly = TRUE,       # From <Analysis Specifications>
        washoutPeriod = 365,            # From <Analysis Specifications>
        removeDuplicateSubjects = "keep all", # From <Analysis Specifications>
        censorAtNewRiskWindow = FALSE,  # From <Analysis Specifications>
        removeSubjectsWithPriorOutcome = TRUE, # From <Analysis Specifications>
        priorOutcomeLookback = 99999,   # From <Analysis Specifications>
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1, # From <Analysis Specifications>
        maxDaysAtRisk = 99999 # Default, not specified in <Analysis Specifications>
      )


      # Append the settings to Analysis List
      # Each entry in cmAnalysisList represents a unique CohortMethod analysis.
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
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Default, not specified in <Analysis Specifications>
  refitPsForEveryOutcome = FALSE, # Default, not specified in <Analysis Specifications>
  refitPsForEveryStudyPopulation = FALSE, # Default, not specified in <Analysis Specifications>
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default, not specified in <Analysis Specifications>
)

# Create the analysis specifications ------------------------------------------
# Combine all shared resources and module specifications into a single
# Strategus analysis specification object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path uses the `name` from <Analysis Specifications> ("iudehre").
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "iudehre", "iudehreAnalysisSpecification.json")
)