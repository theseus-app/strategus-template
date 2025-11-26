################################################################################
# COVID-19 PPI and H2RA Cohort Method Analysis
# Analysis Specification Script using OHDSI Strategus
#
# This script creates a complete analysis specification for a comparative
# effectiveness study of COVID-19 treatments using propensity score methods.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Configuration for ATLAS WebAPI connection
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions ----------------------------------------------------------
# Export cohort definitions from ATLAS
# Target: PPI users (ID: 1794126)
# Comparator: H2RA users (ID: 1794132)
# Outcome: COVID-19 related outcomes (ID: 1794131)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: PPI users
    1794132, # Comparator: H2RA users
    1794131  # Outcome: COVID-19 outcomes
  ),
  generateStats = TRUE
)

# Renumber cohorts for internal use (1=target, 2=comparator, 3=outcome)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative Control Outcomes ---------------------------------------------------
# Export negative control concept set (ID: 1888110)
# These are concepts used to evaluate systematic bias in the analysis
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
  mutate(cohortId = row_number() + 100) %>% # Negative controls: 101, 102, 103, etc.
  select(cohortId, cohortName, outcomeConceptId)

# Validate no duplicate cohort IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Data Frames for Analysis Structure ------------------------------------------
# Outcomes of interest from cohort definitions
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator combinations for CohortMethod
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Concepts to exclude from covariates (drugs of interest in this study)
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# CohortGeneratorModule Settings -----------------------------------------------
# Initialize CohortGeneratorModule to create cohort definitions and negative controls
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Create shared resource specifications for negative control outcomes
# occurrenceType: "first" - use first occurrence of concept
# detectOnDescendants: TRUE - include descendant concepts in addition to exact matches
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications for cohort generation
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings --------------------------------------------
# Initialize CohortDiagnosticsModule for data quality and cohort diagnostics
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

# CohortMethodModule Settings -------------------------------------------------

# Study Period Definition
# studyStartDate: 20200101 (January 1, 2020)
# studyEndDate: 20200515 (May 15, 2020)
studyPeriods <- tibble(
  studyStartDate = c("20200101"),
  studyEndDate = c("20200515")
)

# Time-at-Risk (TAR) Definition
# Single TAR: from 1 day after cohort start to 99999 days after cohort start
# This captures the entire follow-up period from drug initiation
timeAtRisks <- tibble(
  label = c("1 day to 99999 days after cohort start"),
  riskWindowStart = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(99999),
  endAnchor = c("cohort start"),
  minDaysAtRisk = c(1)
)

# Propensity Score Configuration 1: No matching/stratification
# Used as baseline analysis
ps_config_1 <- list(
  method = "none",
  label = "No PS adjustment",
  params = list()
)

# Propensity Score Configuration 2: Match on PS
# maxRatio: 4 (match up to 4 comparators to each target)
# caliper: 0.2 (maximum PS distance on standardized logit scale)
# caliperScale: "standardized logit" (PS on standardized logit scale)
ps_config_2 <- list(
  method = "match",
  label = "Match on PS (caliper=0.2, maxRatio=4)",
  params = list(
    maxRatio = 4,
    caliper = 0.2,
    caliperScale = "standardized logit"
  )
)

# Propensity Score Configuration 3: Stratify by PS
# numberOfStrata: 5 (quintiles of propensity score)
# baseSelection: "all" (include all subjects in each stratum)
ps_config_3 <- list(
  method = "stratify",
  label = "Stratify by PS (5 strata)",
  params = list(
    numberOfStrata = 5,
    baseSelection = "all"
  )
)

# Combine all propensity score configurations
psConfigList <- list(ps_config_1, ps_config_2, ps_config_3)

# Build Analysis List ---------------------------------------------------------
# Iterate through all combinations of study periods, TARs, and PS configurations
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Create propensity score adjustment arguments based on configuration
      if (psCfg$method == "none") {
        # No matching or stratification
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "match") {
        # Create arguments for matching on propensity score
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # Create arguments for stratification by propensity score
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate Settings
      # Create default covariates (age, sex, comorbidities, prior treatments, etc.)
      # addDescendantsToExclude: TRUE - exclude descendant concepts of excluded covariates
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcome List: Combine true outcomes and negative control outcomes
      # True outcomes: outcomeOfInterest = TRUE, trueEffectSize = NA
      # Negative controls: outcomeOfInterest = FALSE, trueEffectSize = 1 (no effect expected)
      outcomeList <- append(
        # True outcomes of interest
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
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

      # Target-Comparator-Outcomes List
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = c(
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # GetDbCohortMethodData Arguments
      # Retrieves patient-level data for the analysis
      # restrictToCommonPeriod: FALSE - allow target/comparator to have different follow-up periods
      # firstExposureOnly: TRUE - include only patients' first exposure to drug
      # washoutPeriod: 180 - require 180 days with no prior exposure
      # removeDuplicateSubjects: "keep first" - if patient in both cohorts, keep first occurrence
      # maxCohortSize: 0 - no limit on cohort size
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        firstExposureOnly = TRUE,
        washoutPeriod = 180,
        removeDuplicateSubjects = "keep first",
        covariateSettings = covariateSettings
      )

      # Create Study Population Arguments
      # Defines the eligible population for analysis after exposure
      # riskWindowStart: 1 (start TAR 1 day after exposure)
      # riskWindowEnd: 99999 (follow-up for up to 99999 days)
      # removeSubjectsWithPriorOutcome: FALSE - include patients with prior outcomes
      # censorAtNewRiskWindow: FALSE - do not censor at new exposure windows
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
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t]
      )

      # Create Propensity Score Arguments
      # Fits logistic regression model to estimate propensity score
      # prior: Laplace (L1) regularization with cross-validation
      # control: LBFGS optimization with 10-fold cross-validation
      createPsArgs <- CohortMethod::createCreatePsArgs(
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

      # Compute Covariate Balance Arguments (before PS adjustment)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      # Compute Covariate Balance Arguments (after PS adjustment)
      # Uses Table 1 standard covariates for reporting
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit Outcome Model Arguments
      # Cox proportional hazards model with PS-based stratification
      # stratified: TRUE - separate baseline hazard for each stratum
      # useCovariates: FALSE - do not adjust for additional covariates
      # inversePtWeighting: FALSE - use original sample weights
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
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

      # Create the CohortMethod Analysis
      # Combines all arguments into a single analysis definition
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study Period: %s-%s; TAR: %s; PS Method: %s",
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

# CohortMethod Module Specifications ------------------------------------------
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create Final Analysis Specifications ----------------------------------------
# Combine all modules (CohortGenerator, CohortDiagnostics, CohortMethod)
# with shared resources (cohort definitions, negative controls)
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save Analysis Specifications to JSON
# This file can be executed by the Strategus execution engine
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "covid19ppiandh2ra", "covid19ppiandh2raAnalysisSpecification.json")
)

################################################################################
# End of Analysis Specification Script
################################################################################