################################################################################
# CreateStrategusAnalysisSpecification.R
# Analysis specification for corazon study
# 
# This script creates a Strategus analysis specification for the corazon study
# using OHDSI CohortMethod package for comparative effectiveness research.
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

# Re-number cohorts for internal use (maintains original Atlas IDs in metadata)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes from concept set
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,  # Negative control concept set ID
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

# Create data frames for analysis configurations -------------------------------

# Outcomes configuration
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%  # Outcome cohort
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)  # Standard clean window for outcomes

# Target and Comparator pairs for CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2, 
  comparatorCohortName = "comparator1"
)

# No specific covariate concepts to exclude (empty per specifications)
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(),
  conceptName = character()
)

# No specific covariate concepts to include (empty per specifications)
includedCovariateConcepts <- data.frame(
  conceptId = numeric(),
  conceptName = character()
)

# Study periods from analysis specifications
studyPeriods <- tibble(
  studyStartDate = c("20100101", "20120101"),  # Two study periods
  studyEndDate   = c("20191231", "20191231")   # Same end date for both
)

# Time-at-risks (TARs) from analysis specifications
timeAtRisks <- tibble(
  label = c("cohort_start_to_cohort_end", "cohort_start_to_99999_days"),
  riskWindowStart  = c(1, 1),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd  = c(0, 99999),
  endAnchor = c("cohort end", "cohort start"),
  minDaysAtRisk = c(1, 1)
)

# Propensity Score settings - stratify by PS (first configuration)
stratifyByPsArgsList <- tibble(
  label = c("Stratify by PS - 5 strata"),
  numberOfStrata  = c(5),
  baseSelection = c("all")  # Base selection from specifications
)

# Propensity Score settings - match on PS (second configuration)
matchOnPsArgsList <- tibble(
  label = c("Match on PS - caliper 0.2"),
  maxRatio  = c(0),  # 0 indicates 1:1 matching
  caliper = c(0.2),
  caliperScale  = c("standardized logit")  # Caliper scale from specifications
)

# Build PS configuration list combining both methods
psConfigList <- list()

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

# CohortDiagnosticsModule Settings ---------------------------------------------
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

# Iterate through all analysis setting combinations
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Create PS adjustment arguments based on method
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

      # Covariate settings - using default settings as no specific inclusions/exclusions
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome list including both primary outcomes and negative controls
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999  # From specifications
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
      
      # Create target-comparator-outcomes combinations
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = c()  # No specific exclusions
        )
      }

      # GetDbCohortMethodData arguments from specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,  # From specifications
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,  # From specifications (0 = no limit)
        firstExposureOnly = FALSE,  # From specifications
        washoutPeriod = 0,  # From specifications
        removeDuplicateSubjects = "keep all",  # From specifications
        covariateSettings = covariateSettings
      )

      # Create PS arguments from specifications
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # From specifications
        errorOnHighCorrelation = TRUE,  # From specifications
        stopOnError = FALSE,  # Allow Strategus to complete all operations
        estimator = "att",
        prior = Cyclops::createPrior(  # Prior settings from specifications
          priorType = "laplace", 
          exclude = c(0), 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(  # Control settings from specifications
          noiseLevel = "silent", 
          cvType = "auto", 
          seed = 1, 
          resetCoefficients = TRUE, 
          tolerance = 2e-07, 
          cvRepetitions = 10,  # From specifications
          startingVariance = 0.01
        )
      )

      # Covariate balance computation arguments
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit outcome model arguments from specifications
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",  # From specifications
        stratified = TRUE,  # From specifications
        useCovariates = FALSE,  # From specifications
        inversePtWeighting = FALSE,  # From specifications
        prior = Cyclops::createPrior(  # Prior settings from specifications
          priorType = "laplace", 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(  # Control settings from specifications
          cvType = "auto", 
          seed = 1, 
          resetCoefficients = TRUE,
          startingVariance = 0.01, 
          tolerance = 2e-07, 
          cvRepetitions = 10,  # From specifications
          noiseLevel = "quiet"  # From specifications
        )
      )
      
      # Create study population arguments from specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,  # From specifications
        firstExposureOnly = FALSE,  # From specifications
        washoutPeriod = 0,  # From specifications
        removeDuplicateSubjects = "keep all",  # From specifications
        censorAtNewRiskWindow = FALSE,  # From specifications
        removeSubjectsWithPriorOutcome = TRUE,  # From specifications
        priorOutcomeLookback = 99999,  # From specifications
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

# Create CohortMethod module specifications
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

# Save the analysis specifications to JSON
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "corazon", "corazonAnalysisSpecification.json")
)