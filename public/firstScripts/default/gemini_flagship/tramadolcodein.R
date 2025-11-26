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

# This script creates the analysis specifications for a Strategus study.
# It is based on the settings provided in the <Analysis Specifications> section.

# Shared Resources -------------------------------------------------------------
# Analysis Specifications setting:
# "cohortDefinitions": {
#   "targetCohort": { "id": 1794126, "name": "target1" },
#   "comparatorCohort": { "id": 1794132, "name": "comparator1" },
#   "outcomeCohort": [ { "id": 1794131, "name": "outcome1" } ]
# }
# "negativeControlConceptSet": { "id": 1888110, "name": "negative" }

# Base URL for the WebAPI instance
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Retrieve cohort definitions from WebAPI
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohort IDs for use within the Strategus framework
# This ensures consistency and avoids potential ID conflicts.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1 # Target
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2 # Comparator
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3 # Outcome

# Retrieve negative control outcome concepts from the specified concept set ID.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # from "negativeControlConceptSet"
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
  # Assign unique cohort IDs to negative controls, starting from 101
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)


# Verify that there are no duplicate cohort IDs between the main cohorts and negative controls.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold cohort information for different analysis modules ---------------
# Outcomes: A list of primary outcomes of interest for the study.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Filter for the re-numbered outcome cohort
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Standard 365-day clean window

# Target and Comparator for the CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1", # from "targetCohort" name
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1" # from "comparatorCohort" name
)

# Analysis Specifications setting:
# "covariateSelection": {
#   "conceptsToInclude": [ { "id": null, "name": "" } ],
#   "conceptsToExclude": [ { "id": null, "name": "" } ]
# }
# Since the specification for concepts to exclude is empty, we create an empty dataframe.
# Note: The target and comparator drugs are automatically excluded from covariates by CohortMethod.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# This module is responsible for generating the cohort instances on the CDM.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Define the main cohort definitions as a shared resource
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Define the negative control outcome cohorts as a shared resource
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
# Create the specific module settings for CohortGenerator
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# This module computes diagnostics on the generated cohorts.
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

# Analysis Specifications setting:
# "getDbCohortMethodDataArgs": { "studyPeriods": [ { "studyStartDate": "", "studyEndDate": "" } ] }
# An empty start and end date implies no date restriction (i.e., use the full observation period).
# We create a tibble with one row of empty strings to ensure the analysis loop runs once for this setting.
studyPeriods <- tibble(
  studyStartDate = c(""), #YYYYMMDD
  studyEndDate   = c("") #YYYYMMDD
)

# Analysis Specifications setting:
# "createStudyPopArgs": { "timeAtRisks": [ ... ] }
# Time-at-risks (TARs) are defined based on the specifications.
timeAtRisks <- tibble::tribble(
  ~label,                ~riskWindowStart, ~startAnchor,   ~riskWindowEnd, ~endAnchor,     ~minDaysAtRisk,
  "On Treatment",        1,                "cohort start", 0,              "cohort end",   1,
  "Intent to Treat",     1,                "cohort start", 99999,          "cohort start", 1
)

# Analysis Specifications setting:
# "propensityScoreAdjustment": { "psSettings": [ { "matchOnPsArgs": { ... } } ] }
# Define propensity score matching settings.
matchOnPsArgsList <- tibble::tribble(
  ~label,                ~maxRatio, ~caliper, ~caliperScale,
  "1-to-1 Matching",     1,         0.2,      "standardized logit"
)

# Since "stratifyByPsArgs" is null in the specifications, we create an empty tibble.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c(), # "all" | "target" | "comparator"
)

# Build a single PS configuration list to iterate over.
psConfigList <- list()

# Convert the "match on PS" tibble into a list format for the analysis loop.
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

# Convert the "stratify by PS" tibble into a list format for the analysis loop.
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
      
      # Set up PS adjustment arguments based on the method (match or stratify).
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

      # Use default covariate settings. Exclusions are handled in targetComparatorOutcomesList.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create a list of all outcomes, including primary outcomes and negative controls.
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
      
      # Define the target-comparator-outcomes combinations.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Analysis Specifications setting: "getDbCohortMethodDataArgs"
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )
      
      # Analysis Specifications setting: "propensityScoreAdjustment": { "createPsArgs": { ... } }
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace", 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", 
          cvType = "auto", 
          seed = 1, 
          resetCoefficients = TRUE, 
          tolerance = 2e-07, 
          cvRepetitions = 10,
          fold = 10,
          startingVariance = 0.01
        )
      )

      # Standard settings for covariate balance computation.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Analysis Specifications setting: "fitOutcomeModelArgs"
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = FALSE,
        useCovariates = FALSE,
        inversePtWeighting = FALSE,
        prior = Cyclops::createPrior(
          priorType = "laplace", 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          noiseLevel = "quiet",
          cvType = "auto", 
          seed = 1, 
          resetCoefficients = TRUE,
          tolerance = 2e-07, 
          cvRepetitions = 10,
          fold = 10,
          startingVariance = 0.01
        )
      )
      
      # Analysis Specifications setting: "createStudyPopArgs"
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = FALSE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t]
      )

      # Append the fully specified analysis settings to the list.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "TAR: %s; PS: %s",
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

# Create the module specification for CohortMethod using the list of analyses.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the final analysis specifications object --------------------------------
# This object combines all shared resources and module specifications.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# Analysis Specifications setting: "name": "tramadolcodein"
studyName <- "tramadolcodein"
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", studyName, paste0(studyName, "AnalysisSpecification.json"))
)