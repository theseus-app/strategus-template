################################################################################
# Create analysis specifications for LEGEND-T2DM study using Strategus
# 
# This script creates analysis specifications for a comparative effectiveness
# study of type 2 diabetes medications using the OHDSI Strategus framework.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
################################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
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

# Re-number cohorts to simpler IDs for analysis
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes - using exact concept set ID from analysis specifications
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
  mutate(cohortId = row_number() + 100) %>% # target/comparator cohort ids start with 1, 2, 3... negativeControl -> 101, 102, 103...
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: 
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis 
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Note: No specific covariate concepts to include or exclude based on analysis specifications
# excludedCovariateConcepts and includedCovariateConcepts are not defined as per specifications

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

# Study periods from analysis specifications
studyPeriods <- tibble(
  studyStartDate = c("19920101"), # Using exact date from specifications: 1992-01-01
  studyEndDate   = c("20211231")  # Using exact date from specifications: 2021-12-31
)

# Time-at-risks (TARs) from analysis specifications - only one TAR defined
timeAtRisks <- tibble(
  label = c("Main TAR"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"), # Using exact anchor from specifications
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end"), # Using exact anchor from specifications
  minDaysAtRisk = c(1) # Using exact value from specifications
) 

# Propensity Score settings - match on PS (first method from specifications)
matchOnPsArgsList <- tibble(
  label = c("Matching"),
  maxRatio  = c(100), # Using exact value from specifications
  caliper = c(0.2), # Using exact value from specifications
  caliperScale  = c("standardized logit") # Using exact scale from specifications
) 

# Propensity Score settings - stratify by PS (second method from specifications)
stratifyByPsArgsList <- tibble(
  label = c("Stratification"),
  numberOfStrata  = c(5), # Using exact value from specifications
  baseSelection = c("all") # Using exact value from specifications
) 

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Convert "match on PS" configurations
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

# Convert "stratify by PS" configurations
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

      # Create covariate settings - using default settings as no specific
      # include/exclude concepts are specified
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
            priorOutcomeLookback = 99999  # Using exact value from specifications
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
          excludedCovariateConceptIds = c()  # No specific concepts to exclude per specifications
        )
      }

      # GetDbCohortMethodDataArgs using specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,  # Using exact value from specifications
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,  # Using exact value from specifications (0 = no limit)
        firstExposureOnly = FALSE,  # Using exact value from specifications
        washoutPeriod = 0,  # Using exact value from specifications
        removeDuplicateSubjects = "keep all",  # Using exact value from specifications
        covariateSettings = covariateSettings
      )

      # CreatePsArgs using specifications
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # Using exact value from specifications
        errorOnHighCorrelation = TRUE,  # Using exact value from specifications
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations
        estimator = "att",
        prior = Cyclops::createPrior(  # Using prior settings from specifications
          priorType = "laplace",  # Using exact prior type from specifications
          exclude = c(0), 
          useCrossValidation = TRUE  # Using exact value from specifications
        ),
        control = Cyclops::createControl(  # Using control settings from specifications
          noiseLevel = "silent",  # Using exact value from specifications
          cvType = "auto",  # Using exact value from specifications
          seed = 1, 
          resetCoefficients = TRUE,  # Using exact value from specifications
          tolerance = 2e-07,  # Using exact value from specifications
          cvRepetitions = 10,  # Using exact value from specifications
          fold = 10,  # Using exact value from specifications
          startingVariance = 0.01  # Using exact value from specifications
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

      # FitOutcomeModelArgs using specifications
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",  # Using exact model type from specifications
        stratified = TRUE,  # Using exact value from specifications
        useCovariates = FALSE,  # Using exact value from specifications
        inversePtWeighting = FALSE,  # Using exact value from specifications
        prior = Cyclops::createPrior(  # Using prior settings from specifications
          priorType = "laplace",  # Using exact prior type from specifications
          useCrossValidation = TRUE  # Using exact value from specifications
        ),
        control = Cyclops::createControl(  # Using control settings from specifications
          cvType = "auto",  # Using exact value from specifications
          seed = 1, 
          resetCoefficients = TRUE,  # Using exact value from specifications
          startingVariance = 0.01,  # Using exact value from specifications
          tolerance = 2e-07,  # Using exact value from specifications
          cvRepetitions = 10,  # Using exact value from specifications
          fold = 10,  # Using exact value from specifications
          noiseLevel = "quiet"  # Using exact value from specifications
        )
      )
      
      # CreateStudyPopArgs using specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,  # Using exact value from specifications
        firstExposureOnly = FALSE,  # Using exact value from specifications
        washoutPeriod = 0,  # Using exact value from specifications
        removeDuplicateSubjects = "keep all",  # Using exact value from specifications
        censorAtNewRiskWindow = FALSE,  # Using exact value from specifications
        removeSubjectsWithPriorOutcome = TRUE,  # Using exact value from specifications
        priorOutcomeLookback = 99999,  # Using exact value from specifications
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

# Save the analysis specifications
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "legendt2dm", "legendt2dmAnalysisSpecification.json")
)