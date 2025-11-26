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
# Get the list of cohorts
# This baseUrl should be the base URL for the WebApi instance containing the cohort definitions
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# According to the Analysis Specifications, we need the following cohorts:
# - Target Cohort: id 1794126, name "target1"
# - Comparator Cohort: id 1794132, name "comparator1"
# - Outcome Cohort: id 1794131, name "outcome1"
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal consistency within the Strategus study.
# It is a good practice to use simple, sequential IDs (e.g., 1, 2, 3)
# for the main cohorts to make referencing them easier in the analysis code.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes
# From Analysis Specifications: "negativeControlConceptSet": { "id": 1888110, "name": "negative" }
# We retrieve the concept set, resolve it to a list of concepts, and format it
# as a cohort set for Strategus.
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
  # Assign unique cohort IDs starting from 101 to avoid collision with main cohorts
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)

# A safety check to ensure there are no overlapping cohort IDs between the main
# cohorts and the negative control outcome cohorts.
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Define the list of outcomes of interest for the study.
# Here, we filter for the outcome cohort we defined (ID 3 after re-numbering).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # cleanWindow is not specified, but typically matches priorOutcomeLookback for consistency
  mutate(cleanWindow = 365) 

# Target and Comparator for the CohortMethod analysis.
# We create a data frame that explicitly defines the pairs of target and comparator cohorts.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# From Analysis Specifications: "covariateSelection": "conceptsToExclude" is empty.
# We create an empty data frame for excluded concepts.
# Note: The template often suggests excluding the T & C drug concepts here, but
# this is handled automatically inside the CohortMethod module by default.
# An empty data frame ensures no *additional* concepts are excluded.
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
# This module is responsible for generating all the cohort instances needed for the study.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Define the main cohort definitions as a shared resource for Strategus
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Define the negative control outcome cohorts as a shared resource.
# These are generated "on the fly" from the source CDM tables based on the concept set.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
# Specify the settings for the CohortGenerator module itself.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Generate inclusion rule statistics for the cohorts
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# This module runs a standard set of diagnostics on the generated cohorts.
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

# From Analysis Specifications: "getDbCohortMethodDataArgs": "studyPeriods" has null start/end dates.
# This indicates the analysis is not restricted to a specific calendar time period.
# We represent this with empty strings.
studyPeriods <- tibble(
  studyStartDate = c(""), # YYYYMMDD
  studyEndDate   = c("")  # YYYYMMDD
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# From Analysis Specifications: "createStudyPopArgs": "timeAtRisks"
# - riskWindowStart: 1, startAnchor: "cohort start"
# - riskWindowEnd: 0, endAnchor: "cohort end"
timeAtRisks <- tibble(
  label = "On Treatment", # A descriptive label for this TAR
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
) 

# Propensity Score settings - match on PS.
# From Analysis Specifications: "propensityScoreAdjustment": "psSettings": "matchOnPsArgs"
# - maxRatio: 10
# - caliper: 0.2
# - caliperScale: "standardized logit"
matchOnPsArgsList <- tibble(
  label = "1-to-10 Matching", # A descriptive label for this matching strategy
  maxRatio  = c(10),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
) 

# Propensity Score settings - stratify by PS.
# From Analysis Specifications: "stratifyByPsArgs": null.
# We create an empty tibble to indicate no stratification analyses are requested.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c(), # "all" | "target" | "comparator"
) 

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


# Iterate through all analysis setting combinations
cmAnalysisList <- list()
analysisId <- 1

# The loops create a Cartesian product of all settings.
# Here, we have 1 study period, 1 TAR, and 1 PS setting, so only one analysis will be created.
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Based on the method ("match" or "stratify"), create the appropriate arguments object.
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

      # Use the default set of covariates from FeatureExtraction.
      # The spec does not specify custom covariates.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Ensures that if a concept is excluded, its descendants are too.
      )

      # Combine the outcomes of interest and the negative control outcomes into a single list.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA # True effect size is unknown for the outcome of interest
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # True effect size is assumed to be 1 for negative controls
          )
        })
      )
      
      # Define the Target-Comparator-Outcomes list. This links the T-C pairs to the outcomes to be tested.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude the specified concepts from the covariates. Here, it is an empty set.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Settings for retrieving data from the database.
      # From Analysis Specifications: "getDbCohortMethodDataArgs"
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # 0 means no limit on cohort size. Matches spec.
        covariateSettings = covariateSettings
      )

      # Settings for creating the propensity score model.
      # From Analysis Specifications: "propensityScoreAdjustment": "createPsArgs"
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # Allows Strategus to continue if a single PS model fails to fit
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
          startingVariance = 0.01,
          fold = 10
        )
      )

      # Settings for computing covariate balance before PS adjustment
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      
      # Settings for computing covariate balance after PS adjustment (for table 1)
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Settings for the outcome model.
      # From Analysis Specifications: "fitOutcomeModelArgs"
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = TRUE, # Stratify by matched sets/strata
        useCovariates = FALSE, # Do not include covariates in the outcome model
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
          noiseLevel = "quiet",
          fold = 10
        )
      )

      # Settings for creating the final study population.
      # From Analysis Specifications: "createStudyPopArgs"
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "remove all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 365,
        # TAR settings are taken from the `timeAtRisks` tibble defined earlier
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1, # from spec
        maxDaysAtRisk = 99999 # a large number to indicate no max
      )


      # Append the settings to the Analysis List. Each element in this list represents
      # one unique combination of analysis choices.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "T: %s, C: %s, TAR: %s, PS: %s",
          cmTcList$targetCohortName[1],
          cmTcList$comparatorCohortName[1],
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

# Create the final CohortMethod module specifications, bundling all the defined analyses.
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
# This brings all the module specifications together into a single JSON object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Add shared resources (available to all modules)
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  # Add module specifications
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the specifications to a JSON file.
# This file will be used as input for the Strategus::execute command.
# From Analysis Specifications: "name": "ceeamos"
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "ceeamos", "ceeamosAnalysisSpecification.json")
)