################################################################################
# Create analysis specifications for semaglutideandnaion study
# This script creates Strategus analysis specifications for a comparative 
# effectiveness study using CohortMethod module
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from ATLAS
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions - fetch from ATLAS using provided cohort IDs
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal use (starting from 1)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes - fetch concept set from ATLAS
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
  mutate(cohortId = row_number() + 100) %>% # Negative controls start at 101
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data structures for analysis ------------------------------------------
# Outcomes: outcome1 from cohort definitions
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Outcome cohort ID after re-numbering
  mutate(outcomeCohortId = cohortId, 
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window of 365 days

# Target and Comparator for the CohortMethod analysis 
cmTcList <- data.frame(
  targetCohortId = 1,           # Re-numbered target cohort ID
  targetCohortName = "target1",
  comparatorCohortId = 2,       # Re-numbered comparator cohort ID
  comparatorCohortName = "comparator1"
)

# Covariate exclusion: No specific concepts to exclude based on specifications
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# Covariate inclusion: No specific concepts to include based on specifications
# includedCovariateConcepts <- data.frame(
#   conceptId = integer(),
#   conceptName = character()
# )

# Study periods from analysis specifications
studyPeriods <- tibble(
  studyStartDate = c(20171201),  # Format: YYYYMMDD
  studyEndDate   = c(20231231)   # Format: YYYYMMDD
)

# Time-at-risks (TARs) from analysis specifications
timeAtRisks <- tibble(
  label = c("Tar1"),
  riskWindowStart  = c(1),          # Days from anchor
  startAnchor = c("cohort start"),  # Anchor point: "cohort start" or "cohort end"
  riskWindowEnd  = c(0),            # Days from anchor (0 = same day as anchor)
  endAnchor = c("cohort end")       # Anchor point: "cohort start" or "cohort end"
  # Note: minDaysAtRisk = 1 is specified in createStudyPopArgs
) 

# Propensity Score settings - match on PS (from first PS setting)
matchOnPsArgsList <- tibble(
  label = c("Match"),
  maxRatio  = c(1),                     # 1:1 matching
  caliper = c(0.2),                     # Caliper width
  caliperScale  = c("standardized logit") # Caliper scale
) 

# Propensity Score settings - stratify by PS (from second PS setting)
stratifyByPsArgsList <- tibble(
  label = c("Stratify"),
  numberOfStrata  = c(5),          # 5 strata
  baseSelection = c("all")         # Base selection method
) 

# Build PS configuration list --------------------------------------------------
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

# Create CohortMethod analysis list --------------------------------------------
cmAnalysisList <- list()
analysisId <- 1

# Iterate through all analysis setting combinations
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
      
      # Covariate settings - using default settings
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )
      
      # Create outcome list including both primary and negative control outcomes
      outcomeList <- append(
        # Primary outcome
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999  # From createStudyPopArgs settings
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
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }
      
      # Create getDbCohortMethodDataArgs from specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,          # From getDbCohortMethodDataArgs
        studyStartDate = studyStartDate,        # From studyPeriods
        studyEndDate = studyEndDate,            # From studyPeriods
        maxCohortSize = 0,                      # From getDbCohortMethodDataArgs
        covariateSettings = covariateSettings,
        firstExposureOnly = FALSE,              # From getDbCohortMethodDataArgs
        washoutPeriod = 0,                      # From getDbCohortMethodDataArgs
        removeDuplicateSubjects = "keep all"    # From getDbCohortMethodDataArgs
      )
      
      # Create propensity score arguments from specifications
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,       # From propensityScoreAdjustment
        errorOnHighCorrelation = TRUE,          # From propensityScoreAdjustment
        stopOnError = FALSE,                    # Allow Strategus to complete all operations
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",                # From propensityScoreAdjustment
          exclude = c(0),
          useCrossValidation = TRUE             # From propensityScoreAdjustment
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",                # From propensityScoreAdjustment
          cvType = "auto",                      # From propensityScoreAdjustment
          seed = 1,
          resetCoefficients = TRUE,             # From propensityScoreAdjustment
          tolerance = 2e-07,                    # From propensityScoreAdjustment
          cvRepetitions = 10,                   # From propensityScoreAdjustment
          fold = 10,                            # From propensityScoreAdjustment
          startingVariance = 0.01               # From propensityScoreAdjustment
        )
      )
      
      # Create covariate balance arguments
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )
      
      # Create outcome model arguments from specifications
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",                      # From fitOutcomeModelArgs
        stratified = TRUE,                      # From fitOutcomeModelArgs
        useCovariates = FALSE,                  # From fitOutcomeModelArgs
        inversePtWeighting = FALSE,             # From fitOutcomeModelArgs
        prior = Cyclops::createPrior(
          priorType = "laplace",                # From fitOutcomeModelArgs
          useCrossValidation = TRUE             # From fitOutcomeModelArgs
        ),
        control = Cyclops::createControl(
          cvType = "auto",                      # From fitOutcomeModelArgs
          seed = 1,
          resetCoefficients = TRUE,             # From fitOutcomeModelArgs
          startingVariance = 0.01,              # From fitOutcomeModelArgs
          tolerance = 2e-07,                    # From fitOutcomeModelArgs
          cvRepetitions = 10,                   # From fitOutcomeModelArgs
          fold = 10,                            # From fitOutcomeModelArgs
          noiseLevel = "quiet"                  # From fitOutcomeModelArgs
        )
      )
      
      # Create study population arguments from specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,         # From createStudyPopArgs
        firstExposureOnly = FALSE,              # From createStudyPopArgs
        washoutPeriod = 0,                      # From createStudyPopArgs
        removeDuplicateSubjects = "keep all",   # From createStudyPopArgs
        censorAtNewRiskWindow = TRUE,           # From createStudyPopArgs
        removeSubjectsWithPriorOutcome = TRUE,  # From createStudyPopArgs
        priorOutcomeLookback = 99999,           # From createStudyPopArgs
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,                      # From createStudyPopArgs
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

# Create module specifications -------------------------------------------------

# CohortGeneratorModule
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

# CohortDiagnosticsModule Settings
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

# CohortMethodModule
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

# Save specifications to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "semaglutideandnaion", "semaglutideandnaionAnalysisSpecification.json")
)