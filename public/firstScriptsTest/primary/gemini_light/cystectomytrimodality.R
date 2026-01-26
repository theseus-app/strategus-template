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
library(ParallelLogger)

# Shared Resources -------------------------------------------------------------
# Base URL for the WebAPI to retrieve cohort definitions and concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Retrieve cohort definitions for target, comparator, and outcome from WebAPI.
# The IDs are taken directly from the <Analysis Specifications>.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to standard IDs (1 for target, 2 for comparator, 3 for outcome)
# This simplifies referencing them within the study design.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Target
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Comparator
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Outcome

# Negative control outcomes
# Retrieve the concept set definition for negative controls from WebAPI.
# The conceptSetId is taken from <Analysis Specifications>.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # negativeControlConceptSet.id
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
  # Assign unique cohort IDs for negative controls, starting after the main cohorts.
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs to prevent conflicts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts used in each analysis ---------------
# Outcomes: Filter for the main outcome cohort (ID 3 after re-numbering).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # cleanWindow is not specified in <Analysis Specifications>, using template default.
  mutate(cleanWindow = 365) 

# Target and Comparator for the CohortMethod analysis.
# Using the re-numbered IDs and names from <Analysis Specifications>.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# For the CohortMethod LSPS we'll need to exclude specific concepts.
# <Analysis Specifications> has empty `conceptsToExclude`, so this will be an empty data frame.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all.
# <Analysis Specifications> has empty `conceptsToInclude`, so this will be an empty data frame.
includedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# CohortGeneratorModule --------------------------------------------------------
# This module generates the cohorts defined in the shared resources.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for cohort definitions.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource specifications for negative control outcome cohorts.
# Occurrence type and detect on descendants are template defaults, not in spec.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications for CohortGenerator.
# generateStats is set to TRUE to compute cohort statistics.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# This module runs diagnostics on the generated cohorts.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Create module specifications for CohortDiagnostics.
# All diagnostic options are set to TRUE/FALSE based on template defaults,
# as no specific overrides are provided in <Analysis Specifications>.
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

# Study periods for restricting the analysis to specific time windows.
# Taken from `getDbCohortMethodDataArgs.studyPeriods` in <Analysis Specifications>.
studyPeriods <- tibble(
  studyStartDate = c("20050101"), # YYYYMMDD
  studyEndDate   = c("20171231")  # YYYYMMDD
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# Taken from `createStudyPopArgs.timeAtRisks` in <Analysis Specifications>.
timeAtRisks <- tibble(
  label = c("TAR_1_99999_CS_CS"), # A descriptive label for this TAR
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd  = c(99999),
  endAnchor = c("cohort start"), # "cohort start" | "cohort end"
  minDaysAtRisk = c(1) # From createStudyPopArgs.timeAtRisks
) 

# Propensity Score settings - match on PS.
# Taken from `propensityScoreAdjustment.psSettings.matchOnPsArgs` in <Analysis Specifications>.
matchOnPsArgsList <- tibble(
  label = c("Match_MaxRatio3_Caliper0.2_StdLogit"), # A descriptive label for this PS setting
  maxRatio  = c(3),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # "propensity score" | "standardized" | "standardized logit"
) 

# Propensity Score settings - stratify by PS.
# `stratifyByPsArgs` is null in <Analysis Specifications>, so this list will be empty.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c() # "all" | "target" | "comparator"
) 

# Build a single PS configuration list (each entry has: method, label, params).
# This structure allows iterating through different PS adjustment methods.
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
# This block will not execute as stratifyByPsArgsList is empty based on <Analysis Specifications>.
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

# Iterate through all analysis setting combinations (study periods, TARs, PS settings).
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Create PS adjustment arguments based on the current PS configuration.
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Template default, not in spec
          stratificationColumns = c() # Template default, not in spec
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Template default, not in spec
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Define covariate settings.
      # Since `covariateSelection.conceptsToInclude` is empty in <Analysis Specifications>,
      # we use default covariate settings. `addDescendantsToExclude` is a template default.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Combine main outcome and negative control outcomes into a single list.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not applicable for observed outcomes
            priorOutcomeLookback = 99999 # From createStudyPopArgs.priorOutcomeLookBack
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

      # Create target-comparator-outcomes list for each T-C pair.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # `excludedCovariateConcepts` is empty based on <Analysis Specifications>.
          # `cmTcList$targetConceptId` and `cmTcList$comparatorConceptId` are not defined in cmTcList,
          # so they are removed from the excluded list.
          excludedCovariateConceptIds = c(
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # Arguments for retrieving cohort method data from the database.
      # `studyStartDate`, `studyEndDate` are from the current loop iteration.
      # `maxCohortSize` is from `getDbCohortMethodDataArgs` in <Analysis Specifications>.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Template default, not in spec for getDbCohortMethodDataArgs
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From <Analysis Specifications> getDbCohortMethodDataArgs.maxCohortSize
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # Parameters are taken from `propensityScoreAdjustment.createPsArgs` in <Analysis Specifications>.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From <Analysis Specifications>
        errorOnHighCorrelation = TRUE,    # From <Analysis Specifications>
        stopOnError = FALSE, # Template default, allows Strategus to complete even if PS model fails
        estimator = "att",   # Template default, not in spec
        prior = Cyclops::createPrior( # Prior settings from <Analysis Specifications>
          priorType = "laplace", 
          exclude = c(0), # Template default, not in spec
          useCrossValidation = TRUE # From <Analysis Specifications>
        ),
        control = Cyclops::createControl( # Control settings from <Analysis Specifications>
          noiseLevel = "silent", # From <Analysis Specifications>
          cvType = "auto",       # From <Analysis Specifications>
          seed = 1,              # Template default, not in spec
          resetCoefficients = TRUE, # From <Analysis Specifications>
          tolerance = 2e-07,     # From <Analysis Specifications>
          cvRepetitions = 10,    # From <Analysis Specifications>
          startingVariance = 0.01, # From <Analysis Specifications>
          fold = 10 # From <Analysis Specifications>
        )
      )

      # Arguments for computing shared covariate balance.
      # Using template defaults as no specific overrides in <Analysis Specifications>.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      # Arguments for computing covariate balance.
      # Using template defaults as no specific overrides in <Analysis Specifications>.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting the outcome model.
      # Parameters are taken from `fitOutcomeModelArgs` in <Analysis Specifications>.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",       # From <Analysis Specifications>
        stratified = TRUE,       # From <Analysis Specifications>
        useCovariates = TRUE,    # From <Analysis Specifications>
        inversePtWeighting = FALSE, # From <Analysis Specifications>
        prior = Cyclops::createPrior( # Prior settings from <Analysis Specifications>
          priorType = "laplace", 
          useCrossValidation = TRUE # From <Analysis Specifications>
        ),
        control = Cyclops::createControl( # Control settings from <Analysis Specifications>
          cvType = "auto",       # From <Analysis Specifications>
          seed = 1,              # Template default, not in spec
          resetCoefficients = TRUE, # From <Analysis Specifications>
          startingVariance = 0.01, # From <Analysis Specifications>
          tolerance = 2e-07,     # From <Analysis Specifications>
          cvRepetitions = 10,    # From <Analysis Specifications>
          noiseLevel = "quiet",  # From <Analysis Specifications>
          fold = 10 # From <Analysis Specifications>
        )
      )

      # Arguments for creating the study population.
      # Parameters are taken from `createStudyPopArgs` in <Analysis Specifications>.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE, # From <Analysis Specifications>
        firstExposureOnly = FALSE,     # From <Analysis Specifications>
        washoutPeriod = 0,             # From <Analysis Specifications>
        removeDuplicateSubjects = "keep all", # From <Analysis Specifications>
        censorAtNewRiskWindow = FALSE, # From <Analysis Specifications>
        removeSubjectsWithPriorOutcome = TRUE, # From <Analysis Specifications>
        priorOutcomeLookback = 99999,  # From <Analysis Specifications>
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From current loop iteration
        startAnchor = timeAtRisks$startAnchor[t],         # From current loop iteration
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],     # From current loop iteration
        endAnchor = timeAtRisks$endAnchor[t],             # From current loop iteration
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],     # From current loop iteration
        maxDaysAtRisk = 99999 # Template default, not in spec
      )

      # Append the settings for the current analysis combination to the list.
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
  analysesToExclude = NULL, # Template default, not in spec
  refitPsForEveryOutcome = FALSE, # Template default, not in spec
  refitPsForEveryStudyPopulation = FALSE, # Template default, not in spec
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Template default, not in spec
)

# Create the analysis specifications ------------------------------------------
# Combine all shared resources and module specifications into a single object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path uses the `name` from <Analysis Specifications>.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "cystectomytrimodality", "cystectomytrimodalityAnalysisSpecification.json")
)