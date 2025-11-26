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
# Analysis Specifications:
# "name": "iudehre"
# This name will be used for the output JSON file path.

# Get the list of cohorts from a WebAPI instance
# Using the OHDSI demo ATLAS for this example
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions from <Analysis Specifications>
# "cohortDefinitions": {
#   "targetCohort": { "id": 1794126, "name": "target1" },
#   "comparatorCohort": { "id": 1794132, "name": "comparator1" },
#   "outcomeCohort": [ { "id": 1794131, "name": "outcome1" } ]
# }
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohort IDs for use within the Strategus study.
# This avoids potential ID conflicts and makes them easier to reference.
# It is a good practice to start with 1, 2, 3, ...
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1 # Target: target1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2 # Comparator: comparator1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3 # Outcome: outcome1

# Negative control outcomes from <Analysis Specifications>
# "negativeControlConceptSet": { "id": 1888110, "name": "negative" }
# These are concepts that are not believed to be caused by the exposure.
# They are used to calibrate p-values and estimate residual systematic error.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # "negative"
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
  # Assign cohort IDs starting from 101 to avoid collision with main cohorts
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)


# Sanity check to ensure no cohort IDs are duplicated
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes of interest list
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # The cleanWindow parameter is for some legacy functionality and not directly used
  # in the CohortMethod settings below, but can be useful for organizing outcomes.
  mutate(cleanWindow = 365)

# Target and Comparator pairs for the CohortMethod analysis 
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate Selection from <Analysis Specifications>
# "covariateSelection": { "conceptsToInclude": [], "conceptsToExclude": [] }
# In this study, no specific concepts are included or excluded from the default
# covariate settings. An empty data frame is created to reflect this.
# It is standard practice to exclude the exposure concepts themselves, but since
# the specification is empty, we will not exclude any concepts here.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# Optional: If you want to define covariates to include instead of including them all
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# This module is responsible for instantiating the cohorts on the target CDM.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Define the cohort definitions as a shared resource for other modules
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Define the negative control outcome cohorts as a shared resource
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
# Define the module specifications for CohortGenerator
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# This module runs a standardized set of diagnostics on the instantiated cohorts.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  # Run diagnostics on all defined cohorts (Target, Comparator, Outcome)
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
# This module executes the comparative cohort study.

# Study period settings from <Analysis Specifications>
# "getDbCohortMethodDataArgs": { "studyPeriods": [ { "studyStartDate": "20030101", "studyEndDate": null } ] }
# An empty string for studyEndDate means there is no end date restriction.
studyPeriods <- tibble(
  studyStartDate = c("20030101"), # YYYYMMDD
  studyEndDate   = c("") # YYYYMMDD
)

# Time-at-risks (TARs) from <Analysis Specifications>
# "createStudyPopArgs": { "timeAtRisks": [ { "riskWindowStart": 30, "startAnchor": "cohort start", 
# "riskWindowEnd": 5475, "endAnchor": "cohort start", "minDaysAtRisk": 1 } ] }
timeAtRisks <- tibble(
  label = c("On Treatment (30d to 5475d post-index)"),
  riskWindowStart  = c(30),
  startAnchor = c("cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd  = c(5475),
  endAnchor = c("cohort start") # "cohort start" | "cohort end"
) 

# Propensity Score settings from <Analysis Specifications>
# "propensityScoreAdjustment": { "psSettings": [ { "matchOnPsArgs": { "maxRatio": 1, "caliper": 0.2, "caliperScale": "standardized logit" } } ] }
# Define settings for matching on propensity score.
matchOnPsArgsList <- tibble(
  label = c("1-to-1 Matching on PS"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # "propensity score" | "standardized" | "standardized logit"
) 

# Propensity Score settings - stratify by PS
# The specification does not include stratification, so this data frame is empty.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c(), # "all" | "target" | "comparator"
) 

# Build a single PS configuration list from the data frames above.
# This loop structure allows for easy expansion to multiple PS adjustment strategies.
psConfigList <- list()

# Convert each row in matchOnPsArgsList to a configuration object
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

# Convert each row in stratifyByPsArgsList to a configuration object
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


# Iterate through all analysis setting combinations (study periods, TARs, PS methods)
# to create a list of CohortMethod analyses to run.
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Create arguments for the specified propensity score adjustment method
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

      # Use default covariate settings from FeatureExtraction package
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create a list of all outcomes, including the primary outcomes and negative controls
      outcomeList <- append(
        # Primary outcomes of interest
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA
          )
        }),
        # Negative control outcomes
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )
      
      # Define the target-comparator-outcomes combinations
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude concepts from covariate construction. Based on empty spec, this is empty.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Settings for fetching data from the database from <Analysis Specifications>
      # "getDbCohortMethodDataArgs": { "maxCohortSize": 0, "studyPeriods": ... }
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        # Restricting covariates to a common period of observation across databases
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        # maxCohortSize = 0 means no limit on cohort size
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Settings for creating the propensity score model from <Analysis Specifications>
      # "propensityScoreAdjustment": { "createPsArgs": { ... } }
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # Let Strategus continue even if one PS model fails
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace", 
          exclude = c(0), # Do not regularize the intercept
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", 
          cvType = "auto", 
          seed = 1, # for reproducibility
          resetCoefficients = TRUE, 
          tolerance = 2e-07, 
          cvRepetitions = 10, 
          startingVariance = 0.01
        )
      )

      # Settings for computing covariate balance
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Settings for fitting the outcome model from <Analysis Specifications>
      # "fitOutcomeModelArgs": { ... }
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        # Note: 'stratified = FALSE' as per spec, differing from common practice
        # where the Cox model is stratified by matched sets.
        stratified = FALSE,
        useCovariates = FALSE,
        inversePtWeighting = FALSE,
        prior = Cyclops::createPrior(
          priorType = "laplace", 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          cvType = "auto", 
          seed = 1, # for reproducibility
          resetCoefficients = TRUE,
          startingVariance = 0.01, 
          tolerance = 2e-07, 
          cvRepetitions = 10, 
          noiseLevel = "quiet"
        )
      )
      
      # Settings for creating the study population from <Analysis Specifications>
      # "createStudyPopArgs": { ... }
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = TRUE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,
        # maxDaysAtRisk = 99999 means no maximum days at risk
        maxDaysAtRisk = 99999
      )


      # Append the full analysis settings to the list
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study Period: %s-%s; TAR: %s; PS Adjustment: %s",
          ifelse(studyStartDate == "", "None", studyStartDate),
          ifelse(studyEndDate == "", "None", studyEndDate),
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

# Create the module specifications for CohortMethod
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the analysis specifications ------------------------------------------
# This combines all the shared resources and module specifications into a single
# JSON object that can be executed by Strategus.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications as a JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "iudehre", "iudehreAnalysisSpecification.json")
)