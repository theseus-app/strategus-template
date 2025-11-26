################################################################################
# CreateStrategusAnalysisSpecification.R
# Analysis specification for COVID-19 Famotidine study
# 
# This script creates analysis specifications for the Strategus framework
# to study the effect of famotidine on COVID-19 outcomes.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from ATLAS
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
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes - using concept set ID from specifications
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
# Outcomes: outcome1
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

# No specific concepts to include or exclude based on specifications
# excludedCovariateConcepts <- data.frame(
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

# Study period from specifications: February 1, 2020 to May 30, 2020
studyPeriods <- tibble(
  studyStartDate = c("20200201"), # YYYYMMDD format from specifications
  studyEndDate   = c("20200530")  # YYYYMMDD format from specifications
)

# Time-at-risks (TARs) from specifications: 1-30 days from cohort start
timeAtRisks <- tibble(
  label = c("1-30 days"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"), # From specifications
  riskWindowEnd  = c(30),
  endAnchor = c("cohort start"), # From specifications
  minDaysAtRisk = c(1) # From specifications
) 

# Propensity Score settings - stratify by PS (5 strata, all subjects)
# From specifications: using stratification with 5 strata, baseSelection = "all"
stratifyByPsArgsList <- tibble(
  label = c("Stratify by PS (5 strata)"),
  numberOfStrata  = c(5), # From specifications
  baseSelection = c("all"), # From specifications
) 

# Build PS configuration list
psConfigList <- list()

# Add stratification configuration
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

# Iterate through all analysis setting combinations
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Create covariate settings using default settings
      # No specific concepts to include or exclude based on specifications
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
            priorOutcomeLookback = 30 # From specifications
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
          outcomes = outcomeList
          # No excluded covariate concepts specified
        )
      }

      # GetDbCohortMethodDataArgs from specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE, # From specifications
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From specifications: 0 means no limit
        covariateSettings = covariateSettings
      )

      # CreatePsArgs from specifications with detailed prior and control settings
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From specifications
        errorOnHighCorrelation = TRUE, # From specifications
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations
        estimator = "att",
        prior = Cyclops::createPrior( # Laplace prior from specifications
          priorType = "laplace", # From specifications
          exclude = c(0), 
          useCrossValidation = TRUE # From specifications
        ),
        control = Cyclops::createControl( # Control settings from specifications
          noiseLevel = "silent", # From specifications
          cvType = "auto", # From specifications  
          seed = 1, 
          resetCoefficients = TRUE, # From specifications
          tolerance = 2e-07, # From specifications
          cvRepetitions = 10, # From specifications
          startingVariance = 0.01, # From specifications
          fold = 10 # From specifications
        )
      )

      # Covariate balance computation settings
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # FitOutcomeModelArgs from specifications with Cox model and detailed settings
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From specifications
        stratified = TRUE, # From specifications
        useCovariates = FALSE, # From specifications
        inversePtWeighting = FALSE, # From specifications
        prior = Cyclops::createPrior( # Laplace prior from specifications
          priorType = "laplace", # From specifications
          useCrossValidation = TRUE # From specifications
        ),
        control = Cyclops::createControl( # Control settings from specifications
          cvType = "auto", # From specifications
          seed = 1, 
          resetCoefficients = TRUE, # From specifications
          startingVariance = 0.01, # From specifications
          tolerance = 2e-07, # From specifications
          cvRepetitions = 10, # From specifications
          noiseLevel = "quiet", # From specifications
          fold = 10 # From specifications
        )
      )
      
      # CreateStudyPopArgs from specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From specifications
        firstExposureOnly = FALSE, # From specifications
        washoutPeriod = 0, # From specifications
        removeDuplicateSubjects = "keep all", # From specifications
        censorAtNewRiskWindow = FALSE, # From specifications
        removeSubjectsWithPriorOutcome = TRUE, # From specifications
        priorOutcomeLookback = 30, # From specifications
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

# Save the analysis specifications to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "covid19famotidine", "covid19famotidineAnalysisSpecification.json")
)