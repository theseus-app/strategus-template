################################################################################
# STRATEGUS ANALYSIS SPECIFICATION: Semaglutide and NAION Study
################################################################################
# This script creates analysis specifications for a comparative cohort study
# examining the relationship between semaglutide exposure and non-arteritic 
# anterior ischemic optic neuropathy (NAION).
#
# Study Design:
# - Target: Semaglutide users (cohort ID: 1794126)
# - Comparator: Comparator drug users (cohort ID: 1794132)
# - Outcome: NAION (cohort ID: 1794131)
# - Study Period: December 1, 2017 to December 31, 2023
# - Methods: Propensity score matching and stratification with Cox regression
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from ATLAS WebAPI
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions from ATLAS using their original cohort IDs
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1 (Semaglutide)
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1 (NAION)
  ),
  generateStats = TRUE
)

# Re-number cohorts to sequential IDs for internal use
# This standardizes cohort IDs to 1, 2, 3 for easier referencing
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# These are outcomes with no known causal relationship to the exposures
# Used to empirically assess residual bias in the study design
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
  mutate(cohortId = row_number() + 100) %>% # Start negative control IDs at 101 to avoid conflicts
  select(cohortId, cohortName, outcomeConceptId)

# Verify no duplicate cohort IDs exist between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames for analysis configuration --------------------------------

# Outcomes: Define the outcome of interest (NAION)
# cleanWindow: not used in this analysis specification but included for completeness
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Defines the exposure comparison: Target (ID=1) vs Comparator (ID=2)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# For the CohortMethod large-scale propensity score (LSPS) analysis,
# we exclude the drugs of interest to prevent them from being used as covariates
# Note: covariateSelection.conceptsToExclude.id is null in specifications,
# so we create an empty data frame as a placeholder
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(0),
  conceptName = character(0)
)

# Note: covariateSelection.conceptsToInclude.id is also null,
# indicating we will use all available covariates (default behavior)

# CohortGeneratorModule --------------------------------------------------------
# This module generates the cohorts defined in ATLAS
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource for main cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource for negative control outcome cohorts
# occurrenceType: "first" means we only consider the first occurrence of the outcome
# detectOnDescendants: TRUE means we include descendant concepts in the outcome definition
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications with statistics generation enabled
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# This module runs diagnostics on cohorts to assess their quality and characteristics
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
# This module performs the comparative cohort study analysis

# Study Period Configuration
# Study restricted to: December 1, 2017 - December 31, 2023
# Format: YYYYMMDD
studyPeriods <- tibble(
  studyStartDate = c("20171201"),
  studyEndDate   = c("20231231")
)

# Time-at-risk (TAR) Configuration
# Defines when outcomes are attributed to the exposure
# riskWindowStart = 1: Risk starts 1 day after cohort start
# startAnchor = "cohort start": Risk window anchored to exposure start
# riskWindowEnd = 0: Risk ends at cohort end (on treatment analysis)
# endAnchor = "cohort end": Risk window ends with cohort end
# minDaysAtRisk = 1: Subjects must have at least 1 day of follow-up
timeAtRisks <- tibble(
  label = c("OnTreatment"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
) 

# Propensity Score Settings - Match on PS
# maxRatio = 1: 1:1 matching between target and comparator
# caliper = 0.2: Maximum allowable difference in PS (0.2 standard deviations on logit scale)
# caliperScale = "standardized logit": Caliper measured on standardized logit PS scale
matchOnPsArgsList <- tibble(
  label = c("PS_Match_1to1"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
) 

# Propensity Score Settings - Stratify by PS
# numberOfStrata = 5: Divide matched population into 5 PS quintiles
# baseSelection = "all": Use all subjects for stratification
stratifyByPsArgsList <- tibble(
  label = c("PS_Stratify_5"),
  numberOfStrata  = c(5),
  baseSelection = c("all")
) 

# Build unified PS configuration list
# This combines both matching and stratification approaches into a single list
# Each configuration will result in a separate analysis
psConfigList <- list()

# Add PS matching configurations
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

# Add PS stratification configurations
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

# Generate all analysis combinations -------------------------------------------
# Iterate through study periods, time-at-risks, and PS methods to create
# all possible analysis configurations
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment method based on configuration type
      if (psCfg$method == "match") {
        # PS Matching configuration
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # PS Stratification configuration
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate Settings
      # Creates default covariates (demographics, conditions, drugs, procedures, etc.)
      # addDescendantsToExclude = TRUE: Excludes descendant concepts of excluded concept IDs
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcome List Configuration
      # Includes both outcomes of interest and negative control outcomes
      outcomeList <- append(
        # Outcomes of interest (NAION)
        # outcomeOfInterest = TRUE: This is the primary outcome
        # trueEffectSize = NA: True effect size unknown
        # priorOutcomeLookback = 99999: Look back effectively unlimited to exclude prior outcomes
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        # Negative control outcomes
        # outcomeOfInterest = FALSE: These are controls, not primary outcomes
        # trueEffectSize = 1: Assume null effect (hazard ratio = 1)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )
      
      # Create Target-Comparator-Outcomes combinations
      # Links each target-comparator pair with all outcomes
      # Excludes exposure concepts from covariates to prevent confounding
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Data Extraction Configuration
      # restrictToCommonPeriod = TRUE: Only include time when both cohorts are observed
      # studyStartDate/studyEndDate: Restrict data to specified study period
      # maxCohortSize = 0: No limit on cohort size
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Propensity Score Model Configuration
      # maxCohortSizeForFitting = 250000: Limit PS model fitting to 250K subjects
      # errorOnHighCorrelation = TRUE: Stop if high covariate correlation detected
      # stopOnError = FALSE: Continue analysis even if PS model fails
      # estimator = "att": Estimate average treatment effect in the treated
      # prior: Laplace prior with cross-validation for regularization
      # control: Cyclops convergence control parameters
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

      # Covariate Balance Computation - Shared (all covariates)
      # Computes standardized mean differences for all covariates
      # maxCohortSize = 250000: Limit computation to 250K subjects
      # covariateFilter = NULL: Include all covariates
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      
      # Covariate Balance Computation - Table 1 (selected covariates)
      # Computes balance for commonly reported Table 1 covariates
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome Model Configuration
      # modelType = "cox": Cox proportional hazards regression
      # stratified = TRUE: Stratify on matched pairs or PS strata
      # useCovariates = FALSE: Do not include covariates in outcome model (PS adjustment only)
      # inversePtWeighting = FALSE: Do not use inverse probability of treatment weighting
      # prior: Laplace prior with cross-validation for regularization
      # control: Cyclops convergence control parameters
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
      
      # Study Population Configuration
      # restrictToCommonPeriod = FALSE: Already restricted in getDbCohortMethodDataArgs
      # firstExposureOnly = FALSE: Include all exposures (per specifications)
      # washoutPeriod = 0: No washout period required (per specifications)
      # removeDuplicateSubjects = "keep all": Keep all exposures (per specifications)
      # censorAtNewRiskWindow = TRUE: Censor at start of new exposure episode
      # removeSubjectsWithPriorOutcome = TRUE: Exclude subjects with outcome before exposure
      # priorOutcomeLookback = 99999: Look back indefinitely for prior outcomes
      # riskWindowStart, startAnchor, riskWindowEnd, endAnchor: Time-at-risk definition
      # minDaysAtRisk = 1: Require at least 1 day of follow-up
      # maxDaysAtRisk = 99999: No maximum follow-up limit
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
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

      # Create complete analysis specification
      # Combines all configuration elements into a single analysis definition
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

# Create CohortMethod Module Specifications
# refitPsForEveryOutcome = FALSE: Reuse same PS model for all outcomes
# refitPsForEveryStudyPopulation = FALSE: Reuse same PS model for all study populations
# analysesToExclude = NULL: Run all analyses
# cmDiagnosticThresholds: Default thresholds for flagging diagnostic issues
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create Complete Analysis Specifications -------------------------------------
# Combines all module specifications and shared resources into a single
# analysis specification object that can be executed by Strategus
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save Analysis Specifications to JSON file
# This file can be used to execute the study across multiple data sources
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "semaglutideandnaion", "semaglutideandnaionAnalysisSpecification.json")
)