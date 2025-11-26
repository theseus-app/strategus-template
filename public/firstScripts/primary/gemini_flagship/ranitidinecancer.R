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

# This script generates a Strategus analysis specification JSON file.
# The settings are derived from the <Analysis Specifications> section provided
# in the prompt.

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from a WebAPI instance
# Using the OHDSI demo ATLAS instance
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# The following cohort IDs are sourced from the "cohortDefinitions" section
# of the Analysis Specifications.
# - targetCohort: id = 1794126, name = "target1"
# - comparatorCohort: id = 1794132, name = "comparator1"
# - outcomeCohort: id = 1794131, name = "outcome1"
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
# This is a common practice to simplify referencing cohorts in the analysis.
# Target cohort becomes ID 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# Comparator cohort becomes ID 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# Outcome cohort becomes ID 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes are sourced from the "negativeControlConceptSet"
# section of the Analysis Specifications.
# - id = 1888110, name = "negative"
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
  # Assign new cohort IDs starting from 101 to avoid conflicts with study cohorts
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)


if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts we'll use in the analysis ---------------

# Outcome(s) of interest list based on the re-numbered cohortDefinitionSet
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Filtering for outcome cohort (ID 3)
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # The cleanWindow corresponds to priorOutcomeLookBack in createStudyPopArgs
  mutate(cleanWindow = 365)

# Target and Comparator list for the CohortMethod analysis.
# Names are from the "cohortDefinitions" section of the Analysis Specifications.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# The "covariateSelection" -> "conceptsToExclude" in the Analysis Specifications is empty.
# An empty data frame is created. CohortMethod will automatically exclude the
# target and comparator drug concepts from covariates.
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
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# This module runs diagnostics on the generated cohorts. Settings are based on
# the template as none are provided in the Analysis Specifications.
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

# From "getDbCohortMethodDataArgs" -> "studyPeriods": studyStartDate and studyEndDate are null.
# An empty string indicates no date restriction.
studyPeriods <- tibble(
  studyStartDate = c(""), #YYYYMMDD
  studyEndDate   = c("") #YYYYMMDD
)

# Time-at-risks (TARs) from "createStudyPopArgs" -> "timeAtRisks".
# One TAR is specified.
timeAtRisks <- tibble(
  label = c("On treatment (365-99999)"),
  riskWindowStart  = c(365), # riskWindowStart: 365
  startAnchor = c("cohort start"), # startAnchor: "cohort start"
  riskWindowEnd  = c(99999), # riskWindowEnd: 99999
  endAnchor = c("cohort start") # endAnchor: "cohort start"
) 

# Propensity Score settings from "propensityScoreAdjustment" -> "psSettings".
# "matchOnPsArgs" is specified.
matchOnPsArgsList <- tibble(
  label = c("1-to-1 matching"),
  maxRatio  = c(1), # maxRatio: 1
  caliper = c(0.2), # caliper: 0.2
  caliperScale  = c("standardized logit") # caliperScale: "standardized logit"
) 

# Propensity Score settings from "propensityScoreAdjustment" -> "psSettings".
# "stratifyByPsArgs" is null, so an empty tibble is created.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c(), # "all" | "target" | "comparator"
) 

# Build a single PS configuration list to iterate over.
psConfigList <- list()
# This section converts the tibbles above into a list of PS configurations.
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


# Iterate through all analysis setting combinations to create a list of CM analyses.
cmAnalysisList <- list()
analysisId <- 1

# Loop over study periods (only one in this case: all time)
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  # Loop over time-at-risk windows (only one specified)
  for (t in seq_len(nrow(timeAtRisks))) {

    # Loop over propensity score adjustment methods (only matching specified)
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Create arguments for matching or stratification based on the PS configuration
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

      # Covariate settings are not specified, so default settings are used.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create a list of all outcome cohorts, including the primary outcome and negative controls.
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
      
      # Define the target-comparator-outcomes combinations for the analysis.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }
      
      # Settings from "getDbCohortMethodDataArgs"
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # maxCohortSize: 0
        covariateSettings = covariateSettings
      )

      # Settings from "propensityScoreAdjustment" -> "createPsArgs"
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # maxCohortSizeForFitting: 250000
        errorOnHighCorrelation = TRUE, # errorOnHighCorrelation: true
        stopOnError = FALSE,
        estimator = "att",
        # prior settings from "propensityScoreAdjustment" -> "createPsArgs" -> "prior"
        prior = Cyclops::createPrior(
          priorType = "laplace", # priorType: "laplace"
          useCrossValidation = TRUE # useCrossValidation: true
        ),
        # control settings from "propensityScoreAdjustment" -> "createPsArgs" -> "control"
        control = Cyclops::createControl(
          noiseLevel = "silent", # noiseLevel: "silent"
          cvType = "auto", # cvType: "auto"
          seed = 1, 
          resetCoefficients = TRUE, # resetCoefficients: true
          tolerance = 2e-07, # tolerance: 2e-7
          cvRepetitions = 10, # cvRepetitions: 10
          startingVariance = 0.01 # startingVariance: 0.01
        )
      )

      # Default settings for covariate balance computation
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Settings from "fitOutcomeModelArgs"
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # modelType: "cox"
        stratified = FALSE, # stratified: false
        useCovariates = FALSE, # useCovariates: false
        inversePtWeighting = FALSE, # inversePtWeighting: false
        # prior settings from "fitOutcomeModelArgs" -> "prior"
        prior = Cyclops::createPrior(
          priorType = "laplace", # priorType: "laplace"
          useCrossValidation = TRUE # useCrossValidation: true
        ),
        # control settings from "fitOutcomeModelArgs" -> "control"
        control = Cyclops::createControl(
          cvType = "auto", # cvType: "auto"
          seed = 1, 
          resetCoefficients = TRUE, # resetCoefficients: true
          startingVariance = 0.01, # startingVariance: 0.01
          tolerance = 2e-07, # tolerance: 2e-7
          cvRepetitions = 10, # cvRepetitions: 10
          noiseLevel = "quiet" # noiseLevel: "quiet"
        )
      )
      
      # Settings from "createStudyPopArgs"
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # restrictToCommonPeriod: false
        firstExposureOnly = FALSE, # firstExposureOnly: false
        washoutPeriod = 365, # washoutPeriod: 365
        removeDuplicateSubjects = "keep all", # removeDuplicateSubjects: "keep all"
        censorAtNewRiskWindow = FALSE, # censorAtNewRiskWindow: false
        removeSubjectsWithPriorOutcome = TRUE, # removeSubjectsWithPriorOutcome: true
        priorOutcomeLookback = 365, # priorOutcomeLookBack: 365
        # Time-at-risk settings are applied here from the loop variable
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1 # minDaysAtRisk: 1
      )

      # Append the complete analysis settings to the cmAnalysisList
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study Dates: %s-%s; TAR: %s; PS: %s",
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

# Create the final CohortMethod module specifications
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
# object that Strategus can execute.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the specifications to a JSON file.
# The name "ranitidinecancer" is from the "name" field in the Analysis Specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("ranitidinecancerAnalysisSpecification.json")
)