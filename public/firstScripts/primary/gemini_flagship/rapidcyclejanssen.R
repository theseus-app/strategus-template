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
# This baseUrl is a placeholder and should be replaced with the actual WebAPI URL
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# The cohort IDs are sourced from the "cohortDefinitions" section of the Analysis Specifications.
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

# Re-number cohorts for internal consistency within the Strategus analysis.
# This simplifies referencing them later in the script.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# The concept set ID is sourced from the "negativeControlConceptSet" section.
# - id: 1888110, name: "negative"
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
  # Assign unique cohort IDs to negative controls, starting from 101 to avoid conflicts.
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)


if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Sourced from "outcomeCohort" in the Analysis Specifications.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not specified in JSON.

# Target and Comparator for the CohortMethod analysis 
# Sourced from "targetCohort" and "comparatorCohort" in the Analysis Specifications.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# The "covariateSelection" section in the Analysis Specifications is empty for both
# "conceptsToInclude" and "conceptsToExclude". Therefore, we create an empty data frame.
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

# CohortDiagnoticsModule Settings ---------------------------------------------
# These are default settings for cohort diagnostics, not specified in the JSON.
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

# Study periods are defined in the "getDbCohortMethodDataArgs" section.
# - studyStartDate: 20210101
# - studyEndDate: null (represented as an empty string)
studyPeriods <- tibble(
  studyStartDate = c("20210101"),
  studyEndDate   = c("") 
)

# Time-at-risks (TARs) are defined in the "createStudyPopArgs:timeAtRisks" section.
# - riskWindowStart: 1, startAnchor: "cohort start"
# - riskWindowEnd: 14, endAnchor: "cohort start"
timeAtRisks <- tibble(
  label = c("1-14d from cohort start"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(14),
  endAnchor = c("cohort start")
) 

# Propensity Score settings - match on PS
# Defined in "propensityScoreAdjustment:psSettings:matchOnPsArgs".
# - maxRatio: 100
# - caliper: 0.2
# - caliperScale: "standardized logit"
matchOnPsArgsList <- tibble(
  label = c("1-to-100 matching"),
  maxRatio  = c(100),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
) 

# Propensity Score settings - stratify by PS
# "stratifyByPsArgs" is null in the Analysis Specifications, so this is empty.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c(),
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

      # Use default covariate settings as none are specified in the JSON.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Combine the outcome of interest with the negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            # This setting is from "createStudyPopArgs:priorOutcomeLookBack"
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
      
      # Define the target-comparator-outcomes list.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # The "covariateSelection" section was empty, so we only pass the empty data frame.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Settings from "getDbCohortMethodDataArgs" section.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        # - maxCohortSize: 0
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Settings from "propensityScoreAdjustment:createPsArgs" section.
      createPsArgs = CohortMethod::createCreatePsArgs(
        # - maxCohortSizeForFitting: 250000
        maxCohortSizeForFitting = 250000,
        # - errorOnHighCorrelation: true
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations
        estimator = "att",
        # Settings from "createPsArgs:prior"
        prior = Cyclops::createPrior(
          # - priorType: "laplace"
          priorType = "laplace", 
          exclude = c(0), 
          # - useCrossValidation: true
          useCrossValidation = TRUE
        ),
        # Settings from "createPsArgs:control"
        control = Cyclops::createControl(
          # - noiseLevel: "silent"
          noiseLevel = "silent", 
          # - cvType: "auto"
          cvType = "auto", 
          seed = 1, 
          # - resetCoefficients: true
          resetCoefficients = TRUE, 
          # - tolerance: 2e-7
          tolerance = 2e-07, 
          # - cvRepetitions: 10, fold: 10
          cvRepetitions = 10,
          # - startingVariance: 0.01
          startingVariance = 0.01
        )
      )

      # Default settings for covariate balance computation.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Settings from "fitOutcomeModelArgs" section.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        # - modelType: "cox"
        modelType = "cox",
        # - stratified: true
        stratified = TRUE,
        # - useCovariates: false
        useCovariates = FALSE,
        # - inversePtWeighting: false
        inversePtWeighting = FALSE,
        # Settings from "fitOutcomeModelArgs:prior"
        prior = Cyclops::createPrior(
          # - priorType: "laplace"
          priorType = "laplace", 
          # - useCrossValidation: true
          useCrossValidation = TRUE
        ),
        # Settings from "fitOutcomeModelArgs:control"
        control = Cyclops::createControl(
          # - cvType: "auto"
          cvType = "auto", 
          seed = 1, 
          # - resetCoefficients: true
          resetCoefficients = TRUE,
          # - startingVariance: 0.01
          startingVariance = 0.01, 
          # - tolerance: 2e-7
          tolerance = 2e-07, 
          # - cvRepetitions: 10, fold: 10
          cvRepetitions = 10, 
          # - noiseLevel: "quiet"
          noiseLevel = "quiet"
        )
      )
      
      # Settings from "createStudyPopArgs" section.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        # - restrictToCommonPeriod: false
        restrictToCommonPeriod = FALSE,
        # - firstExposureOnly: true
        firstExposureOnly = TRUE,
        # - washoutPeriod: 365
        washoutPeriod = 365,
        # - removeDuplicateSubjects: "remove all"
        removeDuplicateSubjects = "remove all",
        # - censorAtNewRiskWindow: false
        censorAtNewRiskWindow = FALSE,
        # - removeSubjectsWithPriorOutcome: true
        removeSubjectsWithPriorOutcome = TRUE,
        # - priorOutcomeLookBack: 99999
        priorOutcomeLookback = 99999,
        # Settings from "createStudyPopArgs:timeAtRisks"
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        # - minDaysAtRisk: 1
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
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
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications JSON file.
# The file path uses the "name" from the Analysis Specifications: "rapidcyclejanssen".
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "rapidcyclejanssen", "rapidcyclejanssenAnalysisSpecification.json")
)