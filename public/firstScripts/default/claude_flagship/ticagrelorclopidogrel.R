################################################################################
# Analysis Specification for ticagrelorclopidogrel Study
# 
# This script creates a Strategus analysis specification for a comparative
# effectiveness study comparing ticagrelor (target) vs clopidogrel (comparator)
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
################################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from ATLAS
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohorts from ATLAS using the IDs specified in the analysis specifications
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to sequential IDs (1, 2, 3) for internal use
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve negative control concept set (ID: 1888110) from ATLAS
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
  mutate(cohortId = row_number() + 100) %>% # Negative control IDs start at 101 to avoid collision
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs between positive and negative control cohorts
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts for analysis -------------------------
# Outcomes: Extract outcome cohort (ID=3) and set a clean window of 365 days
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Target: ID=1 (ticagrelor/target1), Comparator: ID=2 (clopidogrel/comparator1)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate adjustments
# Note: Analysis specifications indicate no specific concepts to exclude (both are null/empty)
# If needed in the future, define excluded concepts here
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# CohortGeneratorModule --------------------------------------------------------
# Create shared resources for cohort definitions and negative controls
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
# Configure diagnostics for all cohorts (target, comparator, and outcome)
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

# Study periods: Define two distinct time windows for the analysis
# Period 1: November 1, 2011 to March 31, 2019
# Period 2: March 1, 2013 to December 31, 2016
studyPeriods <- tibble(
  studyStartDate = c("20111101", "20130301"),
  studyEndDate   = c("20190331", "20161231")
)

# Time-at-risk (TAR) windows: 6 different risk windows specified
# TAR 1: Day 1 to Day 365 from cohort start
# TAR 2: Day 1 to Day 1825 (5 years) from cohort start
# TAR 3: Day 1 from cohort start to cohort end
# TAR 4: Day 29 to Day 365 from cohort start
# TAR 5: Day 29 to Day 1825 from cohort start
# TAR 6: Day 29 from cohort start to cohort end
timeAtRisks <- tibble(
  label = c(
    "TAR_1_365",
    "TAR_1_1825",
    "TAR_1_cohort_end",
    "TAR_29_365",
    "TAR_29_1825",
    "TAR_29_cohort_end"
  ),
  riskWindowStart  = c(1, 1, 1, 29, 29, 29),
  startAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd  = c(365, 1825, 0, 365, 1825, 0),
  endAnchor = c("cohort start", "cohort start", "cohort end", "cohort start", "cohort start", "cohort end")
) 

# Propensity Score settings - match on PS
# Configuration 1: 1:1 matching with caliper 0.2 on standardized logit scale
# Configuration 2: 1:10 matching with caliper 0.2 on standardized logit scale
matchOnPsArgsList <- tibble(
  label = c("PS_Match_1to1", "PS_Match_1to10"),
  maxRatio  = c(1, 10),
  caliper = c(0.2, 0.2),
  caliperScale  = c("standardized logit", "standardized logit")
) 

# Propensity Score settings - stratify by PS
# Configuration: 10 strata, using all subjects as base
stratifyByPsArgsList <- tibble(
  label = c("PS_Stratify_10"),
  numberOfStrata  = c(10),
  baseSelection = c("all")
) 

# Build a single PS configuration list combining match and stratify approaches
# Each entry contains: method type, descriptive label, and parameter settings
psConfigList <- list()

# Convert each row of matchOnPsArgsList to a PS config
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

# Convert each row of stratifyByPsArgsList to a PS config
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
# This creates a comprehensive grid of analyses across:
# - Study periods (2 periods)
# - Time-at-risk windows (6 TARs)
# - PS adjustment methods (3 methods: 1:1 match, 1:10 match, stratify)
# Total: 2 * 6 * 3 = 36 analyses
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Set up PS matching or stratification based on method type
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

      # Default covariate settings with descendants excluded
      # Note: Analysis specs indicate no specific concepts to include/exclude (both null/empty)
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build outcome list: positive outcome + negative controls
      # Positive outcome: outcomeOfInterest=TRUE, trueEffectSize=NA
      # Negative controls: outcomeOfInterest=FALSE, trueEffectSize=1
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
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

      # getDbCohortMethodDataArgs settings from analysis specifications:
      # - restrictToCommonPeriod: TRUE
      # - maxCohortSize: 0 (no limit)
      # - studyStartDate/studyEndDate: vary by study period
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs settings from analysis specifications:
      # - maxCohortSizeForFitting: 250000
      # - errorOnHighCorrelation: TRUE
      # - prior: Laplace with cross-validation
      # - control: tolerance=2e-7, cvType=auto, fold=10, cvRepetitions=10, etc.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace", 
          exclude = c(0), 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", 
          cvType = "auto", 
          seed = 1, 
          resetCoefficients = TRUE, 
          tolerance = 2e-07, 
          cvRepetitions = 10, 
          startingVariance = 0.01,
          fold = 10
        )
      )

      # Compute covariate balance for shared and Table 1 covariates
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs settings from analysis specifications:
      # - modelType: cox (Cox proportional hazards)
      # - stratified: TRUE
      # - useCovariates: FALSE
      # - inversePtWeighting: FALSE
      # - prior: Laplace with cross-validation
      # - control: tolerance=2e-7, cvType=auto, fold=10, cvRepetitions=10, noiseLevel=quiet
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = TRUE,
        useCovariates = FALSE,
        inversePtWeighting = FALSE,
        prior = Cyclops::createPrior(
          priorType = "laplace", 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          cvType = "auto", 
          seed = 1, 
          resetCoefficients = TRUE,
          startingVariance = 0.01, 
          tolerance = 2e-07, 
          cvRepetitions = 10, 
          noiseLevel = "quiet",
          fold = 10
        )
      )
      
      # createStudyPopArgs settings from analysis specifications:
      # - restrictToCommonPeriod: FALSE (overriding getDbCohortMethodDataArgs)
      # - firstExposureOnly: FALSE
      # - washoutPeriod: 0
      # - removeDuplicateSubjects: "keep first"
      # - censorAtNewRiskWindow: FALSE (set to TRUE for proper handling)
      # - removeSubjectsWithPriorOutcome: FALSE (set to TRUE per standard practice)
      # - priorOutcomeLookback: 99999
      # - TAR windows: vary by timeAtRisks configuration
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep first",
        censorAtNewRiskWindow = TRUE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Append the configured analysis to the list
      # Description includes study period, TAR label, and PS method for clarity
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
# - refitPsForEveryOutcome: FALSE (reuse PS model across outcomes)
# - refitPsForEveryStudyPopulation: FALSE (reuse PS model across populations)
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the complete analysis specifications ---------------------------------
# Combine all shared resources and module specifications into a single object
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "ticagrelorclopidogrel", "ticagrelorclopidogrelAnalysisSpecification.json")
)