################################################################################
# CreateStrategusAnalysisSpecification.R
# Analysis specification for alzheimerdonepezil study
# 
# This script creates Strategus analysis specifications for comparative effectiveness
# research comparing target vs comparator for alzheimerdonepezil study.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from ATLAS
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions - using IDs from analysis specifications
# Note: In Strategus, we need to renumber cohorts starting from 1
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal Strategus use
# Strategus requires consecutive cohort IDs starting from 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes - using negative control concept set from specifications
# Negative controls help detect bias and confounding in observational studies
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,  # From analysis specifications
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
  mutate(cohortId = row_number() + 100) %>% # Start negative control IDs from 101
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs (should not happen with this setup)
if (any(duplicated(co(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Includes both primary outcome and negative controls
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%  # Outcome cohort (ID 3 after renumbering)
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)  # Standard clean window for outcomes

# Target and Comparator for the CohortMethod analysis 
# Mapping the original names to internal IDs
cmTcList <- data.frame(
  targetCohortId = 1,  # Renumbered target cohort
  targetCohortName = "target1",
  comparatorCohortId = 2,  # Renumbered comparator cohort
  comparatorCohortName = "comparator1"
)

# Note: The analysis specifications have empty arrays for conceptsToInclude and conceptsToExclude
# Therefore, we won't create includedCovariateConcepts or excludedCovariateConcepts data frames
# All covariates will be included by default, with no specific exclusions

# Study Periods - from analysis specifications (empty strings mean no restriction)
# If you are not restricting your study to a specific time window, 
# please make these strings empty as per specifications
studyPeriods <- tibble(
  studyStartDate = c(""),  # Empty string = no start date restriction
  studyEndDate   = c("")   # Empty string = no end date restriction
)

# Time-at-risks (TARs) for the outcomes - from analysis specifications
# Risk window: 1 to 180 days after cohort start, with minimum 1 day at risk
timeAtRisks <- tibble(
  label = c("1-180 days"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),  # From specifications
  riskWindowEnd  = c(180),
  endAnchor = c("cohort start"),    # From specifications
  minDaysAtRisk = c(1)              # From specifications
) 

# Propensity Score settings - match on PS
# Two PS matching strategies from analysis specifications:
# 1. 1:1 matching with caliper 0.2
# 2. Variable ratio matching (1:3) with caliper 0.2
matchOnPsArgsList <- tibble(
  label = c("1-to-1 matching", "Variable ratio matching (1:3)"),
  maxRatio  = c(1, 3),  # From analysis specifications
  caliper = c(0.2, 0.2),  # From analysis specifications
  caliperScale  = c("standardized logit", "standardized logit")  # From analysis specifications
) 

# Note: No stratification by PS in this analysis (stratifyByPsArgs is null in specifications)
# Therefore, we don't create stratifyByPsArgsList

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# If a data frame for "match on PS" exists and has rows, convert each row to a config
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    # Append a new element at the end of psConfigList
    psConfigList[[length(psConfigList) + 1]] <- list(
      # Identify the PS adjustment method for this config
      method = "match",
      # Human-readable label to carry through into descriptions
      label  = matchOnPsArgsList$label[i],
      # Parameter bundle passed to createMatchOnPsArgs later
      params = list(
        maxRatio     = matchOnPsArgsList$maxRatio[i],
        caliper      = matchOnPsArgsList$caliper[i],
        caliperScale = matchOnPsArgsList$caliperScale[i]
      )
    )
  }
}

# Note: No stratification configurations since stratifyByPsArgs is null in specifications

# CohortGeneratorModule --------------------------------------------------------
# Creates the cohorts for analysis
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
# Runs cohort diagnostics to understand cohort characteristics
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
# Main comparative effectiveness analysis module

# Iterate through all analysis setting combinations
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Create PS adjustment arguments based on configuration
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

      # Covariate settings - using default settings since no specific inclusions/exclusions
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome list including both primary outcome and negative controls
      outcomeList <- append(
        # Primary outcome
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999  # From analysis specifications
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
      
      # Create target-comparator-outcomes combinations
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = c()  # No specific exclusions from specifications
        )
      }

      # GetDbCohortMethodDataArgs - data extraction settings
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,  # From analysis specifications
        studyStartDate = ifelse(studyStartDate == "", NA, studyStartDate),
        studyEndDate = ifelse(studyEndDate == "", NA, studyEndDate),
        maxCohortSize = 0,  # From analysis specifications (0 = no limit)
        covariateSettings = covariateSettings,
        firstExposureOnly = FALSE,  # From analysis specifications
        washoutPeriod = 0,  # From analysis specifications
        removeDuplicateSubjects = "keep all"  # From analysis specifications
      )

      # CreatePsArgs - propensity score model fitting settings
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # From analysis specifications
        errorOnHighCorrelation = TRUE,  # From analysis specifications
        stopOnError = FALSE,  # Setting to FALSE to allow Strategus complete all CM operations
        estimator = "att",
        prior = Cyclops::createPrior(  # From analysis specifications
          priorType = "laplace", 
          exclude = c(0), 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(  # From analysis specifications
          noiseLevel = "silent",  # From analysis specifications
          cvType = "auto",  # From analysis specifications
          seed = 1, 
          resetCoefficients = TRUE,  # From analysis specifications
          tolerance = 2e-07,  # From analysis specifications
          cvRepetitions = 10,  # From analysis specifications
          startingVariance = 0.01,  # From analysis specifications
          fold = 10  # From analysis specifications
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

      # FitOutcomeModelArgs - outcome model settings
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "logistic",  # From analysis specifications
        stratified = TRUE,  # From analysis specifications
        useCovariates = FALSE,  # From analysis specifications
        inversePtWeighting = FALSE,  # From analysis specifications
        prior = Cyclops::createPrior(  # From analysis specifications
          priorType = "laplace", 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(  # From analysis specifications
          cvType = "auto",  # From analysis specifications
          seed = 1, 
          resetCoefficients = TRUE,  # From analysis specifications
          startingVariance = 0.01,  # From analysis specifications
          tolerance = 2e-07,  # From analysis specifications
          cvRepetitions = 10,  # From analysis specifications
          noiseLevel = "quiet",  # From analysis specifications
          fold = 10  # From analysis specifications
        )
      )
      
      # CreateStudyPopArgs - study population creation settings
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,  # From analysis specifications
        firstExposureOnly = FALSE,  # From analysis specifications
        washoutPeriod = 0,  # From analysis specifications
        removeDuplicateSubjects = "keep all",  # From analysis specifications
        censorAtNewRiskWindow = FALSE,  # From analysis specifications
        removeSubjectsWithPriorOutcome = TRUE,  # From analysis specifications
        priorOutcomeLookback = 99999,  # From analysis specifications
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
          "alzheimerdonepezil study; TAR: %s; PS: %s",
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

# Save the analysis specifications to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "alzheimerdonepezil", "alzheimerdonepezilAnalysisSpecification.json")
)