################################################################################
# OHDSI Strategus Analysis Specification: uveitissafety
# 
# This script creates analysis specifications for a comparative cohort study
# using the OHDSI Strategus framework with the following modules:
# - CohortGeneratorModule: Generate target, comparator, and outcome cohorts
# - CohortDiagnosticsModule: Run cohort diagnostics
# - CohortMethodModule: Perform propensity score matched analysis
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Define the WebAPI base URL for cohort definitions
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export cohort definitions from ATLAS
# - Target cohort: 1794126 (target1)
# - Comparator cohort: 1794132 (comparator1)
# - Outcome cohort: 1794131 (outcome1)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal consistency
# Target cohort (1794126) -> 1
# Comparator cohort (1794132) -> 2
# Outcome cohort (1794131) -> 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative Control Outcomes ----------------------------------------------------
# Retrieve negative control concept set (ID: 1888110, name: negative)
# These are used for empirical calibration of effect estimates
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
  mutate(cohortId = row_number() + 100) %>% # Negative control cohort IDs start at 101
  select(cohortId, cohortName, outcomeConceptId)

# Verify no duplicate cohort IDs exist
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Define Analysis Cohorts ------------------------------------------------------
# Outcomes: Define the outcome of interest with a clean window
# cleanWindow = 365 days used to remove subjects with outcome during this period
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for CohortMethod analysis
# Target: cohort ID 1 (target1)
# Comparator: cohort ID 2 (comparator1)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate Concepts to Exclude ------------------------------------------------
# Note: Analysis specifications indicate no specific concepts to exclude
# (conceptsToExclude has null id and empty name)
# This data frame is kept empty but can be populated if needed
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(0),
  conceptName = character(0)
)

# CohortGeneratorModule --------------------------------------------------------
# Module for generating cohorts from cohort definitions
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource for negative control outcome cohorts
# occurrenceType = "first": Only first occurrence of negative control outcome
# detectOnDescendants = TRUE: Include descendant concepts
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications for cohort generation
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule ------------------------------------------------------
# Module for running comprehensive cohort diagnostics
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
# Module for propensity score-based comparative cohort analysis

# Study Period Settings --------------------------------------------------------
# Analysis specifications indicate no specific study period restriction
# (studyStartDate and studyEndDate are both null)
studyPeriods <- tibble(
  studyStartDate = c(""),
  studyEndDate = c("")
)

# Time-at-Risk (TAR) Settings --------------------------------------------------
# Define the risk window for outcome ascertainment
# Per analysis specifications:
# - riskWindowStart = 1 (day after cohort start)
# - startAnchor = "cohort start"
# - riskWindowEnd = 0 (at cohort end)
# - endAnchor = "cohort end"
# - minDaysAtRisk = 1
timeAtRisks <- tibble(
  label = c("OnTreatment"),
  riskWindowStart = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(0),
  endAnchor = c("cohort end")
)

# Propensity Score Settings - Match on PS --------------------------------------
# Per analysis specifications:
# - maxRatio = 10 (up to 10 comparators per target)
# - caliper = 0.2
# - caliperScale = "standardized logit"
matchOnPsArgsList <- tibble(
  label = c("MatchPS_1to10_Cal0.2"),
  maxRatio = c(10),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# Propensity Score Settings - Stratify by PS ----------------------------------
# Analysis specifications indicate stratifyByPsArgs = null
# This data frame is kept empty (no stratification analysis)
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata = numeric(0),
  baseSelection = character(0)
)

# Build PS Configuration List --------------------------------------------------
# Combine all PS adjustment methods into a single configuration list
psConfigList <- list()

# Add "match on PS" configurations
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "match",
      label = matchOnPsArgsList$label[i],
      params = list(
        maxRatio = matchOnPsArgsList$maxRatio[i],
        caliper = matchOnPsArgsList$caliper[i],
        caliperScale = matchOnPsArgsList$caliperScale[i]
      )
    )
  }
}

# Add "stratify by PS" configurations
if (exists("stratifyByPsArgsList") && nrow(stratifyByPsArgsList) > 0) {
  for (i in seq_len(nrow(stratifyByPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "stratify",
      label = stratifyByPsArgsList$label[i],
      params = list(
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}

# Build CohortMethod Analysis List ---------------------------------------------
# Iterate through all combinations of study periods, time-at-risks, and PS methods
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  # Extract study period dates (empty strings if no restriction)
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment method based on configuration
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

      # Covariate Settings -------------------------------------------------------
      # Use default covariate settings (LSPS: Large Scale Propensity Score)
      # addDescendantsToExclude = TRUE: Exclude descendant concepts of excluded concepts
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Define Outcomes ----------------------------------------------------------
      # Combine outcomes of interest and negative control outcomes
      outcomeList <- append(
        # Outcomes of interest
        # priorOutcomeLookBack = 99999: Per analysis specifications
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        # Negative control outcomes
        # outcomeOfInterest = FALSE: Not primary outcomes
        # trueEffectSize = 1: Assumed true effect (no effect)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Target-Comparator-Outcomes Combinations ----------------------------------
      # Define all target-comparator-outcomes combinations for analysis
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # GetDbCohortMethodData Arguments ------------------------------------------
      # Per analysis specifications:
      # - restrictToCommonPeriod = true
      # - studyStartDate = null (no restriction)
      # - studyEndDate = null (no restriction)
      # - maxCohortSize = 0 (no limit)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # CreatePs Arguments -------------------------------------------------------
      # Propensity score model settings per analysis specifications:
      # - maxCohortSizeForFitting = 250000
      # - errorOnHighCorrelation = true
      # - prior: Laplace with cross-validation
      # - control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      #   noiseLevel = "silent", resetCoefficients = true, startingVariance = 0.01
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

      # ComputeCovariateBalance Arguments ----------------------------------------
      # Shared covariate balance: all covariates, maxCohortSize = 250000
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      # Table 1 covariate balance: only Table 1 covariates
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # FitOutcomeModel Arguments ------------------------------------------------
      # Outcome model settings per analysis specifications:
      # - modelType = "cox"
      # - stratified = true
      # - useCovariates = false
      # - inversePtWeighting = false
      # - prior: Laplace with cross-validation
      # - control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      #   noiseLevel = "quiet", resetCoefficients = true, startingVariance = 0.01
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

      # CreateStudyPopulation Arguments ------------------------------------------
      # Study population settings per analysis specifications:
      # - restrictToCommonPeriod = true
      # - firstExposureOnly = true
      # - washoutPeriod = 365 days
      # - removeDuplicateSubjects = "keep all"
      # - censorAtNewRiskWindow = true
      # - removeSubjectsWithPriorOutcome = true
      # - priorOutcomeLookBack = 99999 (all available history)
      # - Time-at-risk from timeAtRisks configuration
      # - minDaysAtRisk = 1
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE,
        firstExposureOnly = TRUE,
        washoutPeriod = 365,
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

      # Create CohortMethod Analysis ---------------------------------------------
      # Combine all settings into a single analysis specification
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(studyStartDate == "", "No restriction", studyStartDate),
          ifelse(studyEndDate == "", "No restriction", studyEndDate),
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

# Create CohortMethod Module Specifications ------------------------------------
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create Analysis Specifications -----------------------------------------------
# Combine all modules into a single analysis specification
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save Analysis Specifications to JSON -----------------------------------------
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "uveitissafety", "uveitissafetyAnalysisSpecification.json")
)