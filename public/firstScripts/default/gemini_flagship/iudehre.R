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

# This script creates the analysis specifications for the "iudehre" study.
# It defines the cohorts, analysis settings, and modules to be executed by Strategus.
# The settings are derived from the <Analysis Specifications> JSON provided.

# Shared Resources -------------------------------------------------------------
# This section defines resources that are shared across different analysis modules,
# such as cohort definitions and concept sets.

# A WebAPI endpoint is required to retrieve cohort and concept set definitions.
# Using the OHDSI demo ATLAS instance as an example.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# ------------------
# Retrieving cohort definitions from the WebAPI for the target, comparator, and outcome cohorts.
# The cohort IDs are specified in the "cohortDefinitions" section of the JSON.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-numbering cohorts for internal consistency within the Strategus framework.
# It's a common practice to use simple, sequential IDs (1, 2, 3, ...) in the analysis specifications.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Target
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Comparator
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Outcome

# Negative Control Outcomes
# -------------------------
# Retrieving the concept set for negative controls, as specified in "negativeControlConceptSet".
# These concepts are then resolved to create a set of negative control outcome cohorts.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # from "negativeControlConceptSet.id"
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
  # Assigning unique cohort IDs to negative controls, starting from 101 to avoid conflicts.
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)

# A safety check to ensure there are no duplicate cohort IDs between the main cohorts and negative controls.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold cohort information for analysis settings
# -------------------------------------------------------------------
# Outcomes of interest list, based on the re-numbered outcome cohort.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Filtering for the outcome cohort (ID 3)
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window

# Target and Comparator list for the CohortMethod analysis.
# Names are taken from "cohortDefinitions" in the JSON.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate settings for exclusion.
# The "covariateSelection.conceptsToExclude" in the JSON is empty, so we create an empty data frame.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# The "covariateSelection.conceptsToInclude" is also empty, so this is commented out.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# This module is responsible for generating the cohort instances on the CDM.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Defining the main cohort definitions as a shared resource.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Defining the negative control outcome cohorts as a shared resource.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
# Creating the module specifications for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# This module runs a set of diagnostics on the generated cohorts.
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
# This module performs the comparative cohort analysis.

# Study Periods, from "getDbCohortMethodDataArgs.studyPeriods".
# The JSON specifies a start date of 2003-01-01 and no end date.
studyPeriods <- tibble(
  studyStartDate = c("20030101"), # YYYYMMDD
  studyEndDate   = c("")          # Empty string for no end date
)

# Time-at-risks (TARs) for the outcomes, from "createStudyPopArgs.timeAtRisks".
timeAtRisks <- tibble(
  label = c("Start 30d to 5475d", "Start 365d to 5475d"),
  riskWindowStart  = c(30, 365),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd  = c(5475, 5475),
  endAnchor = c("cohort start", "cohort start")
) 

# Propensity Score settings - match on PS, from "propensityScoreAdjustment.psSettings".
# This defines the first PS adjustment strategy: matching.
matchOnPsArgsList <- tibble(
  label = c("Match 1:1 Caliper 0.2 SL"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
) 

# Propensity Score settings - stratify by PS, from "propensityScoreAdjustment.psSettings".
# This defines the second PS adjustment strategy: stratification.
stratifyByPsArgsList <- tibble(
  label = c("Stratify 5 strata"),
  numberOfStrata  = c(5),
  baseSelection = c("all")
) 

# Build a single PS configuration list to iterate over.
# This combines the matching and stratification settings into a unified list.
psConfigList <- list()

# Convert the matching data frame to a configuration list.
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

# Convert the stratification data frame to a configuration list.
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


# Iterate through all analysis setting combinations to create a list of analyses.
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Create either matching or stratification arguments based on the PS configuration.
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Using default covariate settings as none are specified in the JSON.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Combining the main outcome of interest with the negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            # This lookback is for createStudyPopArgs, but defined here.
            # It is overridden by the priorOutcomeLookback in createStudyPopArgs.
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
      
      # Defining the target-comparator-outcomes list.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Excluded concepts from the empty data frame defined earlier.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Settings for creating the cohort method data object, from "getDbCohortMethodDataArgs".
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE, # from JSON
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        firstExposureOnly = TRUE, # from JSON
        washoutPeriod = 365, # from JSON
        removeDuplicateSubjects = "remove all", # from JSON
        maxCohortSize = 0, # from JSON
        covariateSettings = covariateSettings
      )

      # Settings for creating the propensity score model, from "propensityScoreAdjustment.createPsArgs".
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # from JSON
        errorOnHighCorrelation = TRUE, # from JSON
        stopOnError = FALSE, # Setting to FALSE to allow Strategus to complete all CM operations
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace", # from JSON
          exclude = c(0), 
          useCrossValidation = TRUE # from JSON
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", # from JSON
          cvType = "auto", # from JSON
          seed = 1, 
          resetCoefficients = TRUE, # from JSON
          tolerance = 2e-07, # from JSON
          cvRepetitions = 10, # from JSON
          startingVariance = 0.01 # from JSON
        )
      )

      # Settings for computing covariate balance.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Settings for fitting the outcome model, from "fitOutcomeModelArgs".
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # from JSON
        stratified = TRUE, # from JSON
        useCovariates = FALSE, # from JSON
        inversePtWeighting = FALSE, # from JSON
        prior = Cyclops::createPrior(
          priorType = "laplace", # from JSON
          useCrossValidation = TRUE # from JSON
        ),
        control = Cyclops::createControl(
          cvType = "auto", # from JSON
          seed = 1, 
          resetCoefficients = TRUE, # from JSON
          startingVariance = 0.01, # from JSON
          tolerance = 2e-07, # from JSON
          cvRepetitions = 10, # from JSON
          noiseLevel = "quiet" # from JSON
        )
      )
      
      # Settings for creating the study population, from "createStudyPopArgs".
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # from JSON
        firstExposureOnly = FALSE, # from JSON
        washoutPeriod = 0, # from JSON
        removeDuplicateSubjects = "keep all", # from JSON
        censorAtNewRiskWindow = FALSE, # from JSON
        removeSubjectsWithPriorOutcome = FALSE, # from JSON
        priorOutcomeLookback = 99999, # from JSON
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1, # from JSON
        maxDaysAtRisk = 99999
      )

      # Append the full analysis settings to the list.
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

# Create the CohortMethod module specifications.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the final analysis specifications object -----------------------------
# This combines all shared resources and module specifications into a single object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path uses the study name "iudehre" from the JSON.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "iudehre", "iudehreAnalysisSpecification.json")
)