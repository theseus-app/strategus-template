################################################################################
# COVID-19 Famotidine Analysis Specification
# Created using OHDSI Strategus package
# 
# This script creates a complete analysis specification for a comparative 
# effectiveness study of famotidine in COVID-19 patients using cohort method
# with propensity score adjustment.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from WebAPI
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions for:
# - Target cohort (ID: 1794126, "target1")
# - Comparator cohort (ID: 1794132, "comparator1")
# - Outcome cohort (ID: 1794131, "outcome1")
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to sequential IDs for internal processing
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Export negative control concepts from the concept set (ID: 1888110, "negative")
# These will be used to detect bias and validate model assumptions
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
  # Assign cohort IDs starting from 101 to avoid conflicts with target/comparator/outcome IDs
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Validate no duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames for outcomes of interest
# Clean window = 365 days for excluding subjects with prior outcome history
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator cohort pairs for the CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# CohortGeneratorModule --------------------------------------------------------
# Initialize the CohortGeneratorModule to define cohort specifications
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for main cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Create shared resource specifications for negative control outcome cohorts
# occurrence type: "first" - use first occurrence of each negative control outcome
# detectOnDescendants: TRUE - include descendant concepts in the concept set
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create the CohortGenerator module specifications
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# Initialize the CohortDiagnosticsModule for cohort quality diagnostics
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Create module specifications for comprehensive cohort diagnostics
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
# Define study periods from Analysis Specifications
# Study period: 2020-02-01 to 2020-05-30 (COVID-19 pandemic period)
studyPeriods <- tibble(
  studyStartDate = c("20200201"),  # YYYYMMDD format
  studyEndDate   = c("20200530")   # YYYYMMDD format
)

# Define Time-At-Risk (TAR) windows for outcome assessment
# TAR: Day 1 to Day 30 after cohort start
# This defines when outcomes are assessed relative to exposure initiation
timeAtRisks <- tibble(
  label = c("Day 1-30 post-index"),
  riskWindowStart  = c(1),           # Start 1 day after cohort start
  startAnchor = c("cohort start"),   # Anchor point: cohort start date
  riskWindowEnd  = c(30),            # End 30 days after cohort start
  endAnchor = c("cohort start"),     # Anchor point: cohort start date
  minDaysAtRisk = c(1)               # Minimum days at risk = 1 day
)

# Propensity Score settings - stratify by PS
# Configuration 1: Stratify into 5 strata using all subjects
stratifyByPsArgsList <- tibble(
  label = c("PS stratification, 5 strata"),
  numberOfStrata  = c(5),
  baseSelection = c("all")           # Use all subjects as base for stratification
)

# Propensity Score settings - match on PS
# Configuration 2: 1:1 matching with caliper = 0.2 on standardized logit scale
matchOnPsArgsList <- tibble(
  label = c("PS matching, 1:1, caliper=0.2"),
  maxRatio  = c(1),                  # 1:1 matching ratio
  caliper = c(0.2),                  # Caliper = 0.2
  caliperScale  = c("standardized logit")  # Caliper scale: standardized logit
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Convert stratify by PS configurations to config list
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

# Convert match on PS configurations to config list
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

# Iterate through all analysis setting combinations
# Generate separate analyses for each combination of: study period, TAR, and PS adjustment method
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment method-specific arguments
      if (psCfg$method == "match") {
        # Configuration: matching on propensity score
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # Configuration: stratification by propensity score
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings: Use default covariates (all available covariates)
      # addDescendantsToExclude: TRUE - include descendant concepts when excluding covariates
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome list: combine outcomes of interest with negative control outcomes
      outcomeList <- append(
        # Outcomes of interest with true effect sizes unknown (NA)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        # Negative control outcomes with assumed true effect size = 1.0 (null effect)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Create target-comparator-outcomes list for the analysis
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude target and comparator drug concepts from covariates to avoid adjustment paradox
          excludedCovariateConceptIds = c()
        )
      }

      # GetDbCohortMethodData arguments
      # Settings for data extraction from CDM
      # Study period: 2020-02-01 to 2020-05-30
      # maxCohortSize: 0 = no maximum size limit
      # restrictToCommonPeriod: FALSE - not restricted to common period (as per spec)
      # firstExposureOnly: TRUE - only first exposure in study period (as per spec)
      # washoutPeriod: 0 - no washout period required (as per spec)
      # removeDuplicateSubjects: "remove all" - remove duplicate subjects (as per spec)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings,
        firstExposureOnly = TRUE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "remove all"
      )

      # Propensity Score creation arguments
      # maxCohortSizeForFitting: 250000 - maximum cohort size for PS model fitting
      # errorOnHighCorrelation: TRUE - raise error if high correlation detected
      # Prior: Laplace prior with cross-validation enabled
      # Control: Auto CV, 10-fold, 10 repetitions, silent output
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
          fold = 10,
          startingVariance = 0.01
        )
      )

      # Compute shared covariate balance before and after PS adjustment
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      # Compute covariate balance using Table 1 specifications (common covariates)
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit outcome model arguments
      # modelType: cox - Cox proportional hazards model
      # stratified: TRUE - stratified by PS strata (for matched/stratified designs)
      # useCovariates: FALSE - do not include individual covariates in outcome model
      # inversePtWeighting: FALSE - do not use inverse probability weighting
      # Prior: Laplace prior with cross-validation
      # Control: Auto CV, 10-fold, 10 repetitions, quiet output, tolerance 2e-07
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
          fold = 10,
          noiseLevel = "quiet"
        )
      )

      # Create study population arguments
      # restrictToCommonPeriod: FALSE - do not restrict to common follow-up period
      # firstExposureOnly: FALSE - allow multiple exposures (as per spec)
      # washoutPeriod: 0 - no washout period (as per spec)
      # removeDuplicateSubjects: "keep all" - keep all duplicate subjects (as per spec)
      # censorAtNewRiskWindow: FALSE - do not censor at new risk window
      # removeSubjectsWithPriorOutcome: FALSE - keep subjects with prior outcomes
      # priorOutcomeLookback: 99999 - look back 99999 days for prior outcomes
      # TAR: Day 1-30 post-index with minimum 1 day at risk
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = FALSE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999
      )

      # Append the analysis configuration to the analysis list
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
# Combine all module specifications into a single analysis specification
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file
# This JSON file can be used to run the analysis with Strategus
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "covid19famotidine", "covid19famotidineAnalysisSpecification.json")
)