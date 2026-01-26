################################################################################
# CreateStrategusAnalysisSpecification.R
# Analysis specification for: covid19ppiandh2ra
# 
# This script creates an OHDSI Strategus analysis specification for a 
# comparative effectiveness study of COVID-19 outcomes comparing target 
# versus comparator cohorts using CohortMethod.
#
# The specification includes:
# 1. Cohort Generation (CohortGeneratorModule)
# 2. Cohort Diagnostics (CohortDiagnosticsModule)  
# 3. Cohort Method analysis (CohortMethodModule)
#
# Settings are based on the provided analysis specifications document.
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Cohort Definitions from ATLAS
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
# Target becomes cohortId 1, Comparator 2, Outcome 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes from concept set ID 1888110
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

# Verify no duplicate cohort IDs between main and negative control cohorts
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames for analysis specifications --------------------------------
# Outcomes: outcome1 (cohortId 3)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Standard 365-day clean window for outcomes

# Target and Comparator for CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2, 
  comparatorCohortName = "comparator1"
)

# From analysis specifications: covariateSelection has empty conceptsToInclude
# and conceptsToExclude, so we don't define any included/excluded covariate concepts
# excludedCovariateConcepts <- data.frame() # Empty as per specifications

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
# Study periods from analysis specifications: 20200101 to 20200515
studyPeriods <- tibble(
  studyStartDate = c("20200101"), # From getDbCohortMethodDataArgs.studyPeriods[0]
  studyEndDate   = c("20200515")  # From getDbCohortMethodDataArgs.studyPeriods[0]
)

# Time-at-risks from analysis specifications: 
# Single TAR with riskWindowStart=1, riskWindowEnd=99999, minDaysAtRisk=1
timeAtRisks <- tibble(
  label = c("Tar1"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"), # From createStudyPopArgs.timeAtRisks[0]
  riskWindowEnd  = c(99999),
  endAnchor = c("cohort start"), # From createStudyPopArgs.timeAtRisks[0]
  minDaysAtRisk = c(1) # From createStudyPopArgs.timeAtRisks[0]
) 

# Propensity Score settings from analysis specifications: three configurations
# 1. No adjustment (matchOnPsArgs = NULL, stratifyByPsArgs = NULL)
# 2. Match on PS with maxRatio=4, caliper=0.2, caliperScale="standardized logit"  
# 3. Stratify by PS with numberOfStrata=5, baseSelection="all"

matchOnPsArgsList <- tibble(
  label = c("MatchPs"),
  maxRatio  = c(4), # From propensityScoreAdjustment.psSettings[1]
  caliper = c(0.2), # From propensityScoreAdjustment.psSettings[1]
  caliperScale  = c("standardized logit") # From propensityScoreAdjustment.psSettings[1]
) 

stratifyByPsArgsList <- tibble(
  label = c("StratifyPs"),
  numberOfStrata  = c(5), # From propensityScoreAdjustment.psSettings[2]
  baseSelection = c("all") # From propensityScoreAdjustment.psSettings[2]
) 

# Build PS configuration list including unadjusted analysis
psConfigList <- list()

# First: unadjusted analysis (no PS adjustment)
psConfigList[[1]] <- list(
  method = "none",
  label  = "Unadjusted",
  params = list()
)

# Add match on PS configuration
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

# Add stratify by PS configuration  
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
      
      # Set PS adjustment args based on configuration
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
      } else { # "none" - unadjusted
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- NULL
      }

      # Covariate settings - using default since specifications have empty include/exclude lists
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome list: main outcome + negative controls
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999 # From createStudyPopArgs.priorOutcomeLookBack
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
      
      # Target-comparator-outcomes list
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = c() # Empty as per covariateSelection.conceptsToExclude
        )
      }

      # getDbCohortMethodDataArgs from analysis specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE, # From getDbCohortMethodDataArgs.restrictToCommonPeriod
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From getDbCohortMethodDataArgs.maxCohortSize
        firstExposureOnly = TRUE, # From getDbCohortMethodDataArgs.firstExposureOnly
        washoutPeriod = 180, # From getDbCohortMethodDataArgs.washoutPeriod
        removeDuplicateSubjects = "keep first", # From getDbCohortMethodDataArgs.removeDuplicateSubjects
        covariateSettings = covariateSettings
      )

      # createPsArgs from propensityScoreAdjustment.createPsArgs
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Allow Strategus to complete all operations
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace", # From propensityScoreAdjustment.createPsArgs.prior.priorType
          exclude = c(0), 
          useCrossValidation = TRUE # From propensityScoreAdjustment.createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", # From propensityScoreAdjustment.createPsArgs.control.noiseLevel
          cvType = "auto", # From propensityScoreAdjustment.createPsArgs.control.cvType
          seed = 1, 
          resetCoefficients = TRUE, # From propensityScoreAdjustment.createPsArgs.control.resetCoefficients
          tolerance = 2e-07, # From propensityScoreAdjustment.createPsArgs.control.tolerance
          cvRepetitions = 10, # From propensityScoreAdjustment.createPsArgs.control.cvRepetitions
          startingVariance = 0.01, # From propensityScoreAdjustment.createPsArgs.control.startingVariance
          fold = 10 # From propensityScoreAdjustment.createPsArgs.control.fold
        )
      )

      # Covariate balance computation args
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs from analysis specifications
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From fitOutcomeModelArgs.modelType
        stratified = TRUE, # From fitOutcomeModelArgs.stratified
        useCovariates = FALSE, # From fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE, # From fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior(
          priorType = "laplace", # From fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE # From fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          cvType = "auto", # From fitOutcomeModelArgs.control.cvType
          seed = 1, 
          resetCoefficients = TRUE, # From fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01, # From fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07, # From fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10, # From fitOutcomeModelArgs.control.cvRepetitions
          noiseLevel = "quiet", # From fitOutcomeModelArgs.control.noiseLevel
          fold = 10 # From fitOutcomeModelArgs.control.fold
        )
      )
      
      # createStudyPopArgs from analysis specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE, # From createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0, # From createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # From createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # From createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = FALSE, # From createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # From createStudyPopArgs.priorOutcomeLookBack
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

# Save specifications to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "covid19ppiandh2ra", "covid19ppiandh2raAnalysisSpecification.json")
)