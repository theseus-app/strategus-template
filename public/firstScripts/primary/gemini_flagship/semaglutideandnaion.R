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

# Shared Resources -------------------------------------------------------------
# This section defines the cohorts and concept sets that are used across the various
# analysis modules.

# The baseUrl is the base URL for the WebApi instance.
# We are using the OHDSI demo Atlas instance as an example.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Here we are retrieving the cohort definitions from Atlas via the WebAPI.
# These are the cohorts that will be instantiated by the CohortGenerator module.
# The cohort IDs are specified in the <Analysis Specifications>.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-numbering cohort IDs for internal consistency within the Strategus study.
# It is a good practice to use simple, sequential IDs (e.g., 1, 2, 3)
# within the study package to avoid dependencies on external Atlas cohort IDs.
# Target cohort (1794126) is re-assigned to ID 1.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# Comparator cohort (1794132) is re-assigned to ID 2.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# Outcome cohort (1794131) is re-assigned to ID 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative Control Outcomes ----------------------------------------------------
# Negative controls are concepts that are not believed to be caused by the exposure.
# They are used for empirical calibration of p-values.
# We retrieve the concept set for negative controls from Atlas.
# The concept set ID is specified in <Analysis Specifications>.
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
  # Renaming columns to match the required format for Strategus.
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  # Assigning unique cohort IDs to each negative control concept.
  # We start from 101 to avoid collision with T, C, and O cohorts.
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)

# A check to ensure there are no duplicate cohort IDs across all defined cohorts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Analysis-specific Cohort Data Frames -----------------------------------------
# These data frames organize the cohorts for use in the analysis modules.

# Outcomes of interest for the CohortMethod analysis.
# This is based on the "outcomeCohort" setting in <Analysis Specifications>.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Filtering for the outcome cohort (ID 3)
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName)

# Target and Comparator cohorts for the CohortMethod analysis.
# This is based on the "targetCohort" and "comparatorCohort" settings.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate settings for CohortMethod.
# The <Analysis Specifications> for "covariateSelection" has empty arrays for
# conceptsToInclude and conceptsToExclude. This means we will use the default
# covariate settings from FeatureExtraction, and CohortMethod will automatically
# exclude the target and comparator drug concepts.
# We create an empty data frame for any additional concepts to exclude.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# Optional: If you want to define covariates to include instead of including them all.
# This is not used based on the <Analysis Specifications>.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# This module is responsible for creating the cohort instances in the database.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Shared resource for the cohort definitions retrieved from Atlas.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Shared resource for the negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
# Module specifications for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# This module runs a set of diagnostics on the instantiated cohorts.
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

# Study period settings from "getDbCohortMethodDataArgs".
# If you are not restricting your study to a specific time window, 
# please make these strings empty.
studyPeriods <- tibble(
  studyStartDate = c("20171201"), # getDbCohortMethodDataArgs.studyPeriods.studyStartDate
  studyEndDate   = c("20231231")  # getDbCohortMethodDataArgs.studyPeriods.studyEndDate
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# Settings from "createStudyPopArgs.timeAtRisks".
timeAtRisks <- tibble(
  label = c("On Treatment"),
  riskWindowStart  = c(1),       # timeAtRisks.riskWindowStart
  startAnchor = c("cohort start"), # timeAtRisks.startAnchor
  riskWindowEnd  = c(0),       # timeAtRisks.riskWindowEnd
  endAnchor = c("cohort end")    # timeAtRisks.endAnchor
) 

# Propensity Score settings - match on PS.
# Settings from "propensityScoreAdjustment.psSettings.matchOnPsArgs".
matchOnPsArgsList <- tibble(
  label = c("1-to-1 Matching"),
  maxRatio  = c(1),     # matchOnPsArgs.maxRatio
  caliper = c(0.2),   # matchOnPsArgs.caliper
  caliperScale  = c("standardized logit") # matchOnPsArgs.caliperScale
) 

# Propensity Score settings - stratify by PS.
# The "stratifyByPsArgs" is null in the specifications, so we create an empty tibble.
stratifyByPsArgsList <- tibble()

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# If a data frame for "match on PS" exists and has rows, convert each row to a config
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

# If a data frame for "stratify by PS" exists and has rows, convert each row to a config
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
      
      # Configure PS matching or stratification based on the psConfigList.
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

      # Use default covariate settings from FeatureExtraction.
      # This is appropriate as no specific include/exclude concepts were provided.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Combine the outcomes of interest and the negative control outcomes into one list.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA
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
      
      # Create the list of target-comparator-outcomes settings.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Excluded concepts are from the empty data frame defined earlier.
          # Note: targetId and comparatorId are automatically excluded from covariates.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Settings for retrieving data from the database.
      # Based on "getDbCohortMethodDataArgs" from specifications.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # getDbCohortMethodDataArgs.maxCohortSize
        covariateSettings = covariateSettings
      )

      # Settings for creating the study population.
      # Based on "createStudyPopArgs" from specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,         # createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE,              # createStudyPopArgs.firstExposureOnly
        washoutPeriod = 365,                    # createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all",   # createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE,          # createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE,  # createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999,           # createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,                      # timeAtRisks.minDaysAtRisk
        maxDaysAtRisk = 99999                   # Default value
      )

      # Settings for creating the propensity score model.
      # Based on "propensityScoreAdjustment.createPsArgs" from specifications.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE,    # createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Setting to FALSE to allow Strategus to complete all CM operations
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",          # createPsArgs.prior.priorType
          useCrossValidation = TRUE,      # createPsArgs.prior.useCrossValidation
          exclude = c(0)                  # Exclude intercept from regularization
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",          # createPsArgs.control.noiseLevel
          cvType = "auto",                # createPsArgs.control.cvType
          fold = 10,                      # createPsArgs.control.fold
          cvRepetitions = 10,             # createPsArgs.control.cvRepetitions
          tolerance = 2e-07,              # createPsArgs.control.tolerance
          resetCoefficients = TRUE,       # createPsArgs.control.resetCoefficients
          startingVariance = 0.01,        # createPsArgs.control.startingVariance
          seed = 1
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

      # Settings for fitting the outcome model.
      # Based on "fitOutcomeModelArgs" from specifications.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",                # fitOutcomeModelArgs.modelType
        stratified = FALSE,               # fitOutcomeModelArgs.stratified
        useCovariates = FALSE,            # fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE,       # fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior(
          priorType = "laplace",          # fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE       # fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          cvType = "auto",                # fitOutcomeModelArgs.control.cvType
          fold = 10,                      # fitOutcomeModelArgs.control.fold
          cvRepetitions = 10,             # fitOutcomeModelArgs.control.cvRepetitions
          noiseLevel = "quiet",           # fitOutcomeModelArgs.control.noiseLevel
          resetCoefficients = TRUE,       # fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01,        # fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07,              # fitOutcomeModelArgs.control.tolerance
          seed = 1
        )
      )

      # Append the complete analysis settings to the list.
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

# Create the module specifications for CohortMethod with the defined analyses.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the final analysis specifications ------------------------------------------
# This combines all the shared resources and module specifications into a single
# JSON object that can be executed by Strategus.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path is based on the study name from the specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "semaglutideandnaion", "semaglutideandnaionAnalysisSpecification.json")
)