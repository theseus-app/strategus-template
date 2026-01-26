################################################################################
# CreateStrategusAnalysisSpecification.R
# This script creates a Strategus analysis specification for the study:
# doacsandwarfarin
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
################################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from Atlas
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions from analysis specifications
# Using EXACT cohort IDs from analysis specifications
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal use (target=1, comparator=2, outcome=3)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes from analysis specifications
# Using EXACT negative control concept set ID: 1888110 with name "negative"
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
  mutate(cohortId = row_number() + 100) %>% # Start negative control IDs from 101
  select(cohortId, cohortName, outcomeConceptId)

# Verify no duplicate cohort IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames for analysis configurations --------------------------------
# Outcomes: Main outcome from analysis specifications (cohortId = 3 after re-numbering)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, 
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window

# Target and Comparator pairs from analysis specifications
cmTcList <- data.frame(
  targetCohortId = 1,              # Re-numbered target cohort
  targetCohortName = "target1",
  comparatorCohortId = 2,          # Re-numbered comparator cohort
  comparatorCohortName = "comparator1"
)

# No covariates to include or exclude (empty from analysis specifications)
# includedCovariateConcepts <- data.frame() # Not used as per specifications
# excludedCovariateConcepts <- data.frame() # Not used as per specifications

# CohortGeneratorModule ---------------------------------------------------------
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

# CohortDiagnosticsModule Settings ----------------------------------------------
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

# CohortMethodModule ------------------------------------------------------------
# Study periods from analysis specifications (one period: 20101019 to 20181231)
studyPeriods <- tibble(
  studyStartDate = c("20101019"), # From getDbCohortMethodDataArgs
  studyEndDate   = c("20181231")  # From getDbCohortMethodDataArgs
)

# Time-at-risks (TARs) from createStudyPopArgs in analysis specifications
# Three TARs specified with their respective parameters
timeAtRisks <- tibble(
  label = c("TAR1", "TAR2", "TAR3"), # Labels for reference
  riskWindowStart  = c(1, 1, 1),     # All start at day 1 after anchor
  startAnchor = c("cohort start", "cohort start", "cohort start"), # All anchored to cohort start
  riskWindowEnd  = c(5, 0, 99999),   # End days as specified
  endAnchor = c("cohort end", "cohort end", "cohort start"), # End anchors as specified
  minDaysAtRisk = c(1, 1, 1)         # Minimum days at risk for all TARs
) 

# Propensity Score settings - match on PS from propensityScoreAdjustment
# Two PS configurations: 1:1 matching and variable ratio matching
matchOnPsArgsList <- tibble(
  label = c("1-to-1 match", "variable ratio match"), # Descriptive labels
  maxRatio  = c(1, 100),       # From psSettings: 1:1 and 1:100 max ratios
  caliper = c(0.2, 0.2),       # Both use 0.2 caliper
  caliperScale  = c("standardized logit", "standardized logit") # Both use standardized logit scale
) 

# No stratify by PS settings (null in analysis specifications)
# stratifyByPsArgsList not created as per specifications

# Build PS configuration list from matchOnPsArgsList only
psConfigList <- list()

# Convert matchOnPsArgsList rows to PS configurations
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "match",                     # Using match method only
      label  = matchOnPsArgsList$label[i],  # Label from list
      params = list(
        maxRatio     = matchOnPsArgsList$maxRatio[i],
        caliper      = matchOnPsArgsList$caliper[i],
        caliperScale = matchOnPsArgsList$caliperScale[i]
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
      
      # Create PS arguments based on configuration
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else {
        matchOnPsArgs <- NULL
        # Note: No stratify configurations in this analysis
      }
      
      # Covariate settings (default, no specific inclusions/exclusions)
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )
      
      # Outcome list including main outcome and negative controls
      outcomeList <- append(
        # Main outcome from analysis specifications
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999  # From createStudyPopArgs
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
      
      # Target-comparator-outcomes list (one pair in this analysis)
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = c()  # No excluded concepts from specifications
        )
      }
      
      # GetDbCohortMethodDataArgs from analysis specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,           # From getDbCohortMethodDataArgs
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,                        # 0 = no restriction
        firstExposureOnly = TRUE,                 # From getDbCohortMethodDataArgs
        washoutPeriod = 0,                        # From getDbCohortMethodDataArgs
        removeDuplicateSubjects = "remove all",   # From getDbCohortMethodDataArgs
        covariateSettings = covariateSettings
      )
      
      # CreatePsArgs from propensityScoreAdjustment in analysis specifications
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,         # From createPsArgs
        errorOnHighCorrelation = TRUE,            # From createPsArgs
        stopOnError = FALSE,                      # Allow Strategus to complete all operations
        estimator = "att",
        prior = Cyclops::createPrior(             # From createPsArgs prior settings
          priorType = "laplace",                  # Laplace prior as specified
          exclude = c(0), 
          useCrossValidation = TRUE               # Use CV as specified
        ),
        control = Cyclops::createControl(         # From createPsArgs control settings
          noiseLevel = "silent",                  # From control
          cvType = "auto",                        # From control
          seed = 1, 
          resetCoefficients = TRUE,               # From control
          tolerance = 2e-07,                      # From control
          cvRepetitions = 10,                     # From control (10 repetitions)
          startingVariance = 0.01,                # From control
          fold = 10                               # From control (10 folds)
        )
      )
      
      # Covariate balance computation arguments (default settings)
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )
      
      # FitOutcomeModelArgs from analysis specifications
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",                        # From fitOutcomeModelArgs
        stratified = TRUE,                        # From fitOutcomeModelArgs
        useCovariates = FALSE,                    # From fitOutcomeModelArgs
        inversePtWeighting = FALSE,               # From fitOutcomeModelArgs
        prior = Cyclops::createPrior(             # From fitOutcomeModelArgs prior settings
          priorType = "laplace",                  # Laplace prior as specified
          useCrossValidation = TRUE               # Use CV as specified
        ),
        control = Cyclops::createControl(         # From fitOutcomeModelArgs control settings
          cvType = "auto",                        # From control
          seed = 1, 
          resetCoefficients = TRUE,               # From control
          startingVariance = 0.01,                # From control
          tolerance = 2e-07,                      # From control
          cvRepetitions = 10,                     # From control (10 repetitions)
          noiseLevel = "quiet",                   # From control
          fold = 10                               # From control (10 folds)
        )
      )
      
      # CreateStudyPopArgs from analysis specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,           # From createStudyPopArgs
        firstExposureOnly = FALSE,                # From createStudyPopArgs
        washoutPeriod = 0,                        # From createStudyPopArgs
        removeDuplicateSubjects = "keep all",     # From createStudyPopArgs
        censorAtNewRiskWindow = FALSE,            # From createStudyPopArgs
        removeSubjectsWithPriorOutcome = FALSE,   # From createStudyPopArgs
        priorOutcomeLookback = 99999,             # From createStudyPopArgs
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
  file.path("inst", "doacsandwarfarin", "doacsandwarfarinAnalysisSpecification.json")
)