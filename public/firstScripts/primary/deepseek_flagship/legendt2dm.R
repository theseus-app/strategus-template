################################################################################
# CreateStrategusAnalysisSpecification.R
# Analysis specification for legendt2dm study using OHDSI Strategus
# 
# This script creates a Strategus analysis specification JSON file based on
# the provided settings. It includes three modules:
# 1. CohortGeneratorModule: Creates cohorts for target, comparator, and outcomes
# 2. CohortDiagnosticsModule: Generates diagnostics for all cohorts
# 3. CohortMethodModule: Performs comparative effectiveness analysis
#
# Note: Replace the baseUrl with your Atlas WebAPI URL and ensure all
#       concept/cohort IDs exist in your environment.
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from Atlas WebAPI
# IMPORTANT: Update baseUrl to your Atlas instance
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions for target, comparator, and outcome
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to avoid conflicts with negative control IDs
# Strategus requires unique cohort IDs across all cohort definitions
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1  # Target becomes ID 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2  # Comparator becomes ID 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3  # Outcome becomes ID 3

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
  mutate(cohortId = row_number() + 100) %>% # Start negative controls at ID 101
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames for outcomes and target-comparator pairs
# Outcomes: outcome of interest plus negative controls
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%  # Outcome cohort
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)  # Default clean window for outcomes

# Target and Comparator for CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate exclusion list - empty based on specifications
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# Covariate inclusion list - empty based on specifications
includedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

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

# Study periods from specifications: 1992-01-01 to 2021-12-31
studyPeriods <- tibble(
  studyStartDate = c("19920101"),
  studyEndDate   = c("20211231")
)

# Time-at-risks (TARs) from specifications
# Risk window: start 1 day after cohort start, end at cohort end
timeAtRisks <- tibble(
  label = c("1d start to 0d end"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end"),
  minDaysAtRisk = c(1)
)

# Propensity Score settings - only stratify by PS (5 strata) based on specifications
stratifyByPsArgsList <- tibble(
  label = c("Stratify by PS (5 strata)"),
  numberOfStrata  = c(5),
  baseSelection = c("all")
)

# Build PS configuration list
psConfigList <- list()

# Add stratify by PS configurations
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

      # Create covariate settings
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )
      
      # Add included concepts if specified
      if (nrow(includedCovariateConcepts) > 0) {
        covariateSettings <- FeatureExtraction::createCovariateSettings(
          useDemographicsGender = TRUE,
          useDemographicsAge = TRUE,
          useDemographicsAgeGroup = TRUE,
          useDemographicsRace = TRUE,
          useDemographicsEthnicity = TRUE,
          useDemographicsIndexYear = TRUE,
          useDemographicsIndexMonth = TRUE,
          useDemographicsPriorObservationTime = TRUE,
          useDemographicsPostObservationTime = TRUE,
          useDemographicsTimeInCohort = TRUE,
          useDemographicsIndexYearMonth = TRUE,
          useConditionOccurrenceAnyTimePrior = TRUE,
          useConditionOccurrenceLongTerm = TRUE,
          useConditionOccurrenceMediumTerm = TRUE,
          useConditionOccurrenceShortTerm = TRUE,
          useConditionEraAnyTimePrior = TRUE,
          useConditionEraLongTerm = TRUE,
          useConditionEraMediumTerm = TRUE,
          useConditionEraShortTerm = TRUE,
          useConditionGroupEraAnyTimePrior = TRUE,
          useConditionGroupEraLongTerm = TRUE,
          useConditionGroupEraMediumTerm = TRUE,
          useConditionGroupEraShortTerm = TRUE,
          useDrugExposureAnyTimePrior = TRUE,
          useDrugExposureLongTerm = TRUE,
          useDrugExposureMediumTerm = TRUE,
          useDrugExposureShortTerm = TRUE,
          useDrugEraAnyTimePrior = TRUE,
          useDrugEraLongTerm = TRUE,
          useDrugEraMediumTerm = TRUE,
          useDrugEraShortTerm = TRUE,
          useDrugGroupEraAnyTimePrior = TRUE,
          useDrugGroupEraLongTerm = TRUE,
          useDrugGroupEraMediumTerm = TRUE,
          useDrugGroupEraShortTerm = TRUE,
          useProcedureOccurrenceAnyTimePrior = TRUE,
          useProcedureOccurrenceLongTerm = TRUE,
          useProcedureOccurrenceMediumTerm = TRUE,
          useProcedureOccurrenceShortTerm = TRUE,
          useDeviceExposureAnyTimePrior = TRUE,
          useDeviceExposureLongTerm = TRUE,
          useDeviceExposureMediumTerm = TRUE,
          useDeviceExposureShortTerm = TRUE,
          useMeasurementAnyTimePrior = TRUE,
          useMeasurementLongTerm = TRUE,
          useMeasurementMediumTerm = TRUE,
          useMeasurementShortTerm = TRUE,
          useMeasurementValueAnyTimePrior = TRUE,
          useMeasurementValueLongTerm = TRUE,
          useMeasurementValueMediumTerm = TRUE,
          useMeasurementValueShortTerm = TRUE,
          useMeasurementRangeGroupAnyTimePrior = TRUE,
          useMeasurementRangeGroupLongTerm = TRUE,
          useMeasurementRangeGroupMediumTerm = TRUE,
          useMeasurementRangeGroupShortTerm = TRUE,
          useObservationAnyTimePrior = TRUE,
          useObservationLongTerm = TRUE,
          useObservationMediumTerm = TRUE,
          useObservationShortTerm = TRUE,
          useCharlsonIndex = TRUE,
          useDcsi = TRUE,
          useChads2 = TRUE,
          useChads2Vasc = TRUE,
          useHfrs = TRUE,
          useDistinctConditionCountLongTerm = TRUE,
          useDistinctConditionCountMediumTerm = TRUE,
          useDistinctConditionCountShortTerm = TRUE,
          useDistinctIngredientCountLongTerm = TRUE,
          useDistinctIngredientCountMediumTerm = TRUE,
          useDistinctIngredientCountShortTerm = TRUE,
          useDistinctProcedureCountLongTerm = TRUE,
          useDistinctProcedureCountMediumTerm = TRUE,
          useDistinctProcedureCountShortTerm = TRUE,
          useDistinctMeasurementCountLongTerm = TRUE,
          useDistinctMeasurementCountMediumTerm = TRUE,
          useDistinctMeasurementCountShortTerm = TRUE,
          useDistinctObservationCountLongTerm = TRUE,
          useDistinctObservationCountMediumTerm = TRUE,
          useDistinctObservationCountShortTerm = TRUE,
          useVisitCountLongTerm = TRUE,
          useVisitCountMediumTerm = TRUE,
          useVisitCountShortTerm = TRUE,
          useVisitConceptCountLongTerm = TRUE,
          useVisitConceptCountMediumTerm = TRUE,
          useVisitConceptCountShortTerm = TRUE,
          longTermStartDays = -365,
          mediumTermStartDays = -180,
          shortTermStartDays = -30,
          endDays = 0,
          includedCovariateConceptIds = includedCovariateConcepts$conceptId,
          addDescendantsToInclude = FALSE,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId,
          addDescendantsToExclude = TRUE
        )
      }

      # Create outcome list: outcome of interest + negative controls
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
      
      # Create target-comparator-outcomes list
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Create getDbCohortMethodDataArgs from specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,  # From specifications
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,  # No limit from specifications
        covariateSettings = covariateSettings
      )

      # Create propensity score arguments from specifications
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # From specifications
        errorOnHighCorrelation = TRUE,  # From specifications
        stopOnError = FALSE,  # Allow Strategus to complete all operations
        estimator = "att",
        prior = Cyclops::createPrior(  # From specifications
          priorType = "laplace", 
          exclude = c(0), 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(  # From specifications
          noiseLevel = "silent", 
          cvType = "auto", 
          seed = 1, 
          resetCoefficients = TRUE, 
          tolerance = 2e-07, 
          cvRepetitions = 10,  # From specifications
          startingVariance = 0.01,
          fold = 10  # From specifications
        )
      )

      # Covariate balance arguments
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model arguments from specifications
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",  # From specifications
        stratified = TRUE,  # From specifications
        useCovariates = FALSE,  # From specifications
        inversePtWeighting = FALSE,  # From specifications
        prior = Cyclops::createPrior(  # From specifications
          priorType = "laplace", 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(  # From specifications
          cvType = "auto", 
          seed = 1, 
          resetCoefficients = TRUE,
          startingVariance = 0.01, 
          tolerance = 2e-07, 
          cvRepetitions = 10,  # From specifications
          noiseLevel = "quiet",  # From specifications
          fold = 10  # From specifications
        )
      )
      
      # Create study population arguments from specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,  # From specifications
        firstExposureOnly = TRUE,  # From specifications
        washoutPeriod = 365,  # From specifications
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

# Save the analysis specification to JSON
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "legendt2dm", "legendt2dmAnalysisSpecification.json")
)