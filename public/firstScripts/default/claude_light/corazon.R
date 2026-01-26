################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates a Strategus analysis specification for the "corazon" study
# using the OHDSI HADES modules: CohortGenerator, CohortDiagnostics, and CohortMethod.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources: Cohort Definitions ----------------------------------------
# These cohort definitions are manually created based on the analysis specifications
# In a real scenario, these would be exported from ATLAS using ROhdsiWebApi

# Target Cohort: target1 (ID: 1794126)
targetCohort <- data.frame(
  cohortId = 1,
  cohortName = "target1",
  sql = "-- SQL for target cohort would go here"
)

# Comparator Cohort: comparator1 (ID: 1794132)
comparatorCohort <- data.frame(
  cohortId = 2,
  cohortName = "comparator1",
  sql = "-- SQL for comparator cohort would go here"
)

# Outcome Cohort: outcome1 (ID: 1794131)
outcomeCohort <- data.frame(
  cohortId = 3,
  cohortName = "outcome1",
  sql = "-- SQL for outcome cohort would go here"
)

# Combine all cohort definitions
cohortDefinitionSet <- bind_rows(targetCohort, comparatorCohort, outcomeCohort)

# Negative Control Outcomes from Concept Set (ID: 1888110, name: "negative")
# These would typically be resolved from a concept set in ATLAS
negativeControlOutcomeCohortSet <- data.frame(
  cohortId = integer(),
  cohortName = character(),
  outcomeConceptId = integer()
)
# Note: In practice, this would be populated from the concept set definition

# Create data frames to hold cohorts used in each analysis ----------------------

# Outcomes: Extract outcome cohorts from cohortDefinitionSet
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Maps target cohort (ID: 1) to comparator cohort (ID: 2)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate exclusion: Empty in this specification (conceptsToInclude and conceptsToExclude are empty)
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# CohortGeneratorModule -------------------------------------------------------
# Configures cohort generation with statistics enabled

cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

# Create shared resource specifications for negative control outcomes
# Note: negativeControlOutcomeCohortSet would be populated from concept set 1888110
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications for CohortGenerator
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings -------------------------------------------
# Configures comprehensive cohort diagnostics

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

# CohortMethodModule ----------------------------------------------------------
# Configures comparative effectiveness analysis with propensity score adjustment

# Study Periods: Two distinct time windows from analysis specifications
# Period 1: 2010-01-01 to 2019-12-31
# Period 2: 2012-01-01 to 2019-12-31
studyPeriods <- tibble(
  studyStartDate = c("20100101", "20120101"),
  studyEndDate = c("20191231", "20191231")
)

# Time-at-Risk (TAR) Definitions: Two different risk windows
# TAR 1: From cohort start (day 1) to cohort end (day 0)
# TAR 2: From cohort start (day 1) to 99999 days after cohort start
timeAtRisks <- tibble(
  label = c("TAR_cohort_start_to_end", "TAR_cohort_start_99999days"),
  riskWindowStart = c(1, 1),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd = c(0, 99999),
  endAnchor = c("cohort end", "cohort start"),
  minDaysAtRisk = c(1, 1)
)

# Propensity Score Configuration 1: Stratification by PS
# Stratifies into 5 strata using all subjects as base selection
stratifyByPsArgsList <- tibble(
  label = c("PS_stratify_5strata"),
  numberOfStrata = c(5),
  baseSelection = c("all")
)

# Propensity Score Configuration 2: Matching on PS
# Matches with caliper of 0.2 on standardized logit scale, max ratio 0 (1:1 matching)
matchOnPsArgsList <- tibble(
  label = c("PS_match_caliper0.2"),
  maxRatio = c(0),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# Build propensity score configuration list
# Each configuration specifies a PS adjustment method with its parameters
psConfigList <- list()

# Add stratification configurations
if (nrow(stratifyByPsArgsList) > 0) {
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

# Add matching configurations
if (nrow(matchOnPsArgsList) > 0) {
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

# Build CohortMethod analysis list by iterating through all combinations
# of study periods, time-at-risks, and propensity score configurations
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure propensity score adjustment method
      if (psCfg$method == "match") {
        # Matching on propensity score with specified caliper
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # Stratification by propensity score
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate Settings: Use default covariates with descendant exclusion
      # covariateSelection specifies conceptsToInclude and conceptsToExclude (both empty in this spec)
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome list combining positive outcomes and negative controls
      outcomeList <- append(
        # Positive outcomes of interest
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999  # From priorOutcomeLookBack in createStudyPopArgs
          )
        }),
        # Negative control outcomes (if any exist)
        if (nrow(negativeControlOutcomeCohortSet) > 0) {
          lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
            CohortMethod::createOutcome(
              outcomeId = i,
              outcomeOfInterest = FALSE,
              trueEffectSize = 1
            )
          })
        } else {
          list()
        }
      )

      # Create target-comparator-outcomes combinations
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

      # Get Database Cohort Method Data Arguments
      # Applies study period restrictions and covariate settings
      # maxCohortSize = 0 means no size restriction
      # restrictToCommonPeriod = FALSE: do not restrict to common exposure period
      # firstExposureOnly = FALSE: include all exposures
      # washoutPeriod = 0: no washout period required
      # removeDuplicateSubjects = "keep all": retain all subject records
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Create Propensity Score Arguments
      # Uses Laplace prior with cross-validation
      # maxCohortSizeForFitting = 250000: limit fitting to 250k subjects
      # errorOnHighCorrelation = TRUE: raise error if high correlation detected
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
          fold = 10,
          startingVariance = 0.01
        )
      )

      # Compute shared covariate balance (before matching/stratification)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      # Compute covariate balance (after matching/stratification)
      # Uses Table 1 specifications for balance assessment
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit Outcome Model Arguments
      # Cox proportional hazards model, stratified by matching/stratification
      # useCovariates = FALSE: do not adjust for covariates in outcome model
      # inversePtWeighting = FALSE: do not use inverse probability weighting
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
          fold = 10,
          noiseLevel = "quiet"
        )
      )

      # Create Study Population Arguments
      # Defines the at-risk population and outcome observation period
      # restrictToCommonPeriod = FALSE: do not restrict to common period
      # firstExposureOnly = FALSE: include all exposures
      # washoutPeriod = 0: no washout period
      # removeDuplicateSubjects = "keep all": retain all records
      # censorAtNewRiskWindow = FALSE: do not censor at new risk window
      # removeSubjectsWithPriorOutcome = TRUE: exclude subjects with prior outcome
      # priorOutcomeLookBack = 99999: look back 99999 days for prior outcomes
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999
      )

      # Create CohortMethod analysis combining all settings
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
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the complete analysis specifications --------------------------------
# Combines all module specifications and shared resources into a single specification
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save analysis specifications to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "corazon", "corazonAnalysisSpecification.json")
)