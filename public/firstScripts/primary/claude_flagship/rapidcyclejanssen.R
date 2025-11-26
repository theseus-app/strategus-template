################################################################################
# Strategus Analysis Specification for rapidcyclejanssen Study
# 
# This script creates analysis specifications using the OHDSI Strategus package
# for a comparative cohort study with propensity score matching.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Configuration: ATLAS WebAPI base URL for cohort and concept set retrieval
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export cohort definitions from ATLAS for:
# - Target cohort (ID: 1794126 - target1)
# - Comparator cohort (ID: 1794132 - comparator1)  
# - Outcome cohort (ID: 1794131 - outcome1)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to sequential IDs (1, 2, 3) for internal processing
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative Control Outcomes ----------------------------------------------------
# Retrieve negative control concept set (ID: 1888110 - negative) from ATLAS
# Negative controls are used to calibrate effect estimates and assess residual bias
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
  mutate(cohortId = row_number() + 100) %>% # Negative control IDs start at 101 to avoid conflicts
  select(cohortId, cohortName, outcomeConceptId)

# Validation: Check for duplicate cohort IDs between primary cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Analysis Configuration Data Frames -------------------------------------------
# Outcomes: Define outcome cohort with clean window (washout period between outcomes)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator: Pair for CohortMethod comparative analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate Exclusions: Define concepts to exclude from covariate construction
# Note: No specific concepts to exclude based on specifications (id: null)
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(0),
  conceptName = character(0)
)

# Optional: Define concepts to include for covariate construction
# Note: No specific concepts to include based on specifications (id: null)
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# Module for generating cohorts in the CDM database
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Register primary cohorts (target, comparator, outcome) as shared resources
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Register negative control outcomes as shared resources
# occurrenceType = "first": Only first occurrence of outcome per person
# detectOnDescendants = TRUE: Include descendant concepts in outcome detection
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications with statistics generation enabled
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule ------------------------------------------------------
# Module for comprehensive cohort diagnostics and quality checks
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

# Study Period Configuration
# studyStartDate: 20210101 (January 1, 2021)
# studyEndDate: null (no end date restriction - study runs to end of data)
studyPeriods <- tibble(
  studyStartDate = c("20210101"),
  studyEndDate = c("")
)

# Time-At-Risk (TAR) Configuration
# Defines the risk window for outcome occurrence relative to cohort entry
# riskWindowStart: 1 day after cohort start
# startAnchor: "cohort start"
# riskWindowEnd: 14 days after cohort start
# endAnchor: "cohort start"
# This creates a 14-day risk window starting the day after cohort entry
timeAtRisks <- tibble(
  label = c("TAR_1_14_days_from_start"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(14),
  endAnchor = c("cohort start")
) 

# Propensity Score Settings - Match on PS
# maxRatio: 100 (up to 100 comparators per target)
# caliper: 0.2 (maximum PS difference allowed for matching)
# caliperScale: "standardized logit" (scale for caliper measurement)
matchOnPsArgsList <- tibble(
  label = c("PS_Match_1to100_cal0.2_stdLogit"),
  maxRatio  = c(100),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
) 

# Propensity Score Settings - Stratify by PS
# Not used in this study (stratifyByPsArgs = null in specifications)
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = numeric(0),
  baseSelection = character(0)
) 

# Build Unified PS Configuration List
# Consolidates all PS adjustment methods (matching and/or stratification)
psConfigList <- list()

# Convert match on PS settings to configuration list
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

# Convert stratify by PS settings to configuration list
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

# Iterate Through All Analysis Combinations ------------------------------------
# Creates analysis specifications for all combinations of:
# - Study periods
# - Time-at-risk windows
# - Propensity score adjustment methods
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment method based on config
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

      # Covariate Settings: Use default covariates with descendant inclusion for excluded concepts
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcome List: Combine outcomes of interest with negative controls
      # Outcomes of interest: outcomeOfInterest = TRUE, trueEffectSize = NA
      # Negative controls: outcomeOfInterest = FALSE, trueEffectSize = 1 (null effect)
      # priorOutcomeLookBack: 99999 (effectively unlimited lookback for prior outcomes)
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
      
      # Target-Comparator-Outcomes List: Define all analysis pairs
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # GetDbCohortMethodData Arguments
      # restrictToCommonPeriod: FALSE (use full follow-up for each cohort)
      # studyStartDate: 20210101 (restrict cohort entry to on/after this date)
      # studyEndDate: "" (no end date restriction)
      # maxCohortSize: 0 (no size limit)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # CreatePs Arguments: Propensity score model settings
      # maxCohortSizeForFitting: 250000 (subsample if cohort larger)
      # errorOnHighCorrelation: TRUE (error if covariates highly correlated)
      # stopOnError: FALSE (continue analysis even if PS model fails)
      # estimator: "att" (average treatment effect in the treated)
      # prior: Laplace (L1) regularization with cross-validation
      # control: Cyclops optimization parameters
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

      # Compute Shared Covariate Balance Arguments
      # Used for all covariates (no filtering)
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      
      # Compute Covariate Balance Arguments
      # Used for Table 1 covariates only (filtered)
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # FitOutcomeModel Arguments: Outcome model settings
      # modelType: "cox" (Cox proportional hazards model)
      # stratified: TRUE (stratified on matched sets)
      # useCovariates: FALSE (don't include covariates in outcome model)
      # inversePtWeighting: FALSE (don't use inverse probability weighting)
      # prior: Laplace (L1) regularization with cross-validation
      # control: Cyclops optimization parameters
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
      
      # CreateStudyPopulation Arguments: Study population inclusion criteria
      # restrictToCommonPeriod: FALSE (no restriction to common observation period)
      # firstExposureOnly: TRUE (only first exposure per person)
      # washoutPeriod: 365 (require 365 days prior observation)
      # removeDuplicateSubjects: "remove all" (remove subjects in both target and comparator)
      # censorAtNewRiskWindow: FALSE (don't censor at new exposure)
      # removeSubjectsWithPriorOutcome: TRUE (exclude subjects with prior outcome)
      # priorOutcomeLookBack: 99999 (unlimited lookback for prior outcome)
      # riskWindowStart: 1 (start risk window 1 day after cohort start)
      # startAnchor: "cohort start"
      # riskWindowEnd: 14 (end risk window 14 days after cohort start)
      # endAnchor: "cohort start"
      # minDaysAtRisk: 1 (require at least 1 day at risk)
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = TRUE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "remove all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Create CmAnalysis: Bundle all settings into a single analysis specification
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

# CohortMethod Module Specifications -------------------------------------------
# Create module specifications with:
# - All analysis configurations
# - Target-comparator-outcome combinations
# - No analyses excluded
# - No PS refitting (use same PS for all outcomes and populations)
# - Default diagnostic thresholds
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create Final Analysis Specifications -----------------------------------------
# Assemble all modules into complete Strategus analysis specification
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save Analysis Specifications to JSON -----------------------------------------
# Output: inst/rapidcyclejanssen/rapidcyclejanssenAnalysisSpecification.json
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "rapidcyclejanssen", "rapidcyclejanssenAnalysisSpecification.json")
)