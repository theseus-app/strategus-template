################################################################################
# CreateStrategusAnalysisSpecification.R
# Analysis Specification for iudehre study
# 
# This script creates analysis specifications for the OHDSI Strategus package
# based on the provided analysis settings.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
################################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from Atlas
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions - using exact IDs from analysis specifications
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal use in Strategus
# Target becomes cohortId 1, Comparator becomes 2, Outcome becomes 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes from concept set
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Negative control concept set from specifications
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
  mutate(cohortId = row_number() + 100) %>% # Negative controls start from 101
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: outcome1
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Standard clean window for outcomes

# Target and Comparator for the CohortMethod analysis 
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# No specific concepts to exclude based on empty covariateSelection in specifications
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(0),
  conceptName = character(0)
)

# No specific concepts to include based on empty covariateSelection in specifications
# includedCovariateConcepts <- data.frame(
#   conceptId = numeric(0),
#   conceptName = character(0)
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

# Study periods from getDbCohortMethodDataArgs
studyPeriods <- tibble(
  studyStartDate = "20030101", # From studyStartDate in specifications
  studyEndDate   = NA_character_ # Null in specifications becomes NA
)

# Time-at-risks (TARs) from createStudyPopArgs in specifications
timeAtRisks <- tibble(
  label = c("TAR1", "TAR2"), # Labels for the two time-at-risk windows
  riskWindowStart  = c(30, 365), # From riskWindowStart in specifications
  startAnchor = c("cohort start", "cohort start"), # From startAnchor in specifications
  riskWindowEnd  = c(5475, 5475), # From riskWindowEnd in specifications
  endAnchor = c("cohort start", "cohort start"), # From endAnchor in specifications
  minDaysAtRisk = c(1, 1) # From minDaysAtRisk in specifications
) 

# Propensity Score settings - match on PS (first PS method from specifications)
matchOnPsArgsList <- tibble(
  label = c("PS Match"), # Label for matching method
  maxRatio  = c(1), # From maxRatio in specifications
  caliper = c(0.2), # From caliper in specifications
  caliperScale  = c("standardized logit") # From caliperScale in specifications
) 

# Propensity Score settings - stratify by PS (second PS method from specifications)
stratifyByPsArgsList <- tibble(
  label = c("PS Stratify"), # Label for stratification method
  numberOfStrata  = c(5), # From numberOfStrata in specifications
  baseSelection = c("all") # From baseSelection in specifications
) 

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Add match on PS configuration
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

# Add stratify by PS configuration
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

      # Default covariate settings - no specific inclusion/exclusion per specifications
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome list including both primary outcome and negative controls
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999 # From priorOutcomeLookBack in specifications
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
      
      # Create target comparator outcomes list
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # GetDbCohortMethodDataArgs from specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE, # From restrictToCommonPeriod in specifications
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From maxCohortSize in specifications
        firstExposureOnly = TRUE, # From firstExposureOnly in specifications
        washoutPeriod = 365, # From washoutPeriod in specifications
        removeDuplicateSubjects = "remove all", # From removeDuplicateSubjects in specifications
        covariateSettings = covariateSettings
      )

      # CreatePsArgs from propensityScoreAdjustment in specifications
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From maxCohortSizeForFitting in specifications
        errorOnHighCorrelation = TRUE, # From errorOnHighCorrelation in specifications
        stopOnError = FALSE, # Allow Strategus to complete all CM operations
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace", # From priorType in specifications
          exclude = c(0), 
          useCrossValidation = TRUE # From useCrossValidation in specifications
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", # From noiseLevel in specifications
          cvType = "auto", # From cvType in specifications
          seed = 1, 
          resetCoefficients = TRUE, # From resetCoefficients in specifications
          tolerance = 2e-07, # From tolerance in specifications
          cvRepetitions = 10, # From cvRepetitions in specifications
          fold = 10, # From fold in specifications
          startingVariance = 0.01 # From startingVariance in specifications
        )
      )

      # Covariate balance computation
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # FitOutcomeModelArgs from specifications
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From modelType in specifications
        stratified = TRUE, # From stratified in specifications
        useCovariates = FALSE, # From useCovariates in specifications
        inversePtWeighting = FALSE, # From inversePtWeighting in specifications
        prior = Cyclops::createPrior(
          priorType = "laplace", # From priorType in specifications
          useCrossValidation = TRUE # From useCrossValidation in specifications
        ),
        control = Cyclops::createControl(
          cvType = "auto", # From cvType in specifications
          seed = 1, 
          resetCoefficients = TRUE, # From resetCoefficients in specifications
          startingVariance = 0.01, # From startingVariance in specifications
          tolerance = 2e-07, # From tolerance in specifications
          cvRepetitions = 10, # From cvRepetitions in specifications
          fold = 10, # From fold in specifications
          noiseLevel = "quiet" # From noiseLevel in specifications
        )
      )
      
      # CreateStudyPopArgs from specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From restrictToCommonPeriod in specifications
        firstExposureOnly = FALSE, # From firstExposureOnly in specifications
        washoutPeriod = 0, # From washoutPeriod in specifications
        removeDuplicateSubjects = "keep all", # From removeDuplicateSubjects in specifications
        censorAtNewRiskWindow = FALSE, # From censorAtNewRiskWindow in specifications
        removeSubjectsWithPriorOutcome = FALSE, # From removeSubjectsWithPriorOutcome in specifications
        priorOutcomeLookback = 99999, # From priorOutcomeLookback in specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999
      )

      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(is.na(studyStartDate), "All", studyStartDate),
          ifelse(is.na(studyEndDate), "All", studyEndDate),
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

# Save the analysis specifications
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "iudehre", "iudehreAnalysisSpecification.json")
)