################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates analysis specifications for the legendt2dm study using the
# OHDSI Strategus package. The analysis compares a target cohort against a 
# comparator cohort to evaluate outcomes of interest, with propensity score
# stratification and Cox regression outcome modeling.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
# ##############################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from ATLAS
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Exports target cohort (1794126), comparator cohort (1794132), and outcome cohort (1794131)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to sequential IDs for internal reference
# Target = 1, Comparator = 2, Outcome = 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Exports concept set (1888110) containing negative control concepts
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
  # Assign negative control cohort IDs starting from 101
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Validate that no cohort ID conflicts exist
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts used in each analysis

# Outcomes data frame: Contains outcome cohort information
# minDaysAtRisk = 1 means participants must have at least 1 day of follow-up
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Pairs target cohort with comparator cohort
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Excluded covariate concepts: Currently empty as per specifications
# These would exclude specific drug concepts from covariate adjustment
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# CohortGeneratorModule --------------------------------------------------------
# Configures cohort generation settings
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Create shared resource specifications for negative control outcomes
# occurrenceType = "first" means we use the first occurrence of each negative control concept
# detectOnDescendants = TRUE means we include descendant concepts in the concept set
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications for cohort generation
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# Configures cohort diagnostics to assess data quality and cohort characteristics
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
# Configures comparative effectiveness analysis using CohortMethod

# Study periods: Defines the time window for the analysis
# Study dates: January 1, 1992 to December 31, 2021
studyPeriods <- tibble(
  studyStartDate = "19920101",
  studyEndDate = "20211231"
)

# Time-at-risks (TARs): Defines the period during which outcomes are measured
# TAR: Starts 1 day after cohort start, ends on cohort end date
# minDaysAtRisk = 1 ensures participants have at least 1 day of follow-up
timeAtRisks <- tibble(
  label = "TAR_1_to_0",
  riskWindowStart = 1,
  startAnchor = "cohort start",
  riskWindowEnd = 0,
  endAnchor = "cohort end",
  minDaysAtRisk = 1
)

# Propensity Score settings: Stratify by PS
# numberOfStrata = 5: Creates 5 PS strata
# baseSelection = "all": Forms strata from all subjects (not just target or comparator)
stratifyByPsArgsList <- tibble(
  label = "stratify_5_strata",
  numberOfStrata = 5,
  baseSelection = "all"
)

# Build PS configuration list
# Each entry specifies a PS adjustment method and its parameters
psConfigList <- list()

# Add stratify by PS configuration
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

# Iterate through all analysis setting combinations
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure propensity score adjustment method
      # matchOnPsArgs = NULL because we are using stratification, not matching
      matchOnPsArgs <- NULL
      
      stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
        numberOfStrata = psCfg$params$numberOfStrata,
        stratificationColumns = c(),
        baseSelection = psCfg$params$baseSelection
      )

      # Create default covariate settings
      # addDescendantsToExclude = TRUE ensures that descendant concepts of excluded concepts are also excluded
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome list: Combines outcomes of interest and negative control outcomes
      # Outcomes of interest have trueEffectSize = NA (unknown)
      # Negative controls have trueEffectSize = 1 (no effect expected)
      outcomeList <- append(
        # Positive outcomes (outcomes of interest)
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

      # Create target-comparator-outcomes list
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

      # Get database cohort method data arguments
      # restrictToCommonPeriod = FALSE: No requirement for target and comparator overlap
      # maxCohortSize = 0: No maximum cohort size limit
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Propensity score creation arguments
      # maxCohortSizeForFitting = 250000: Maximum subjects used to fit PS model
      # errorOnHighCorrelation = TRUE: Stop if high correlation detected
      # Prior: Laplace (L1) regularization with cross-validation
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
          startingVariance = 0.01
        )
      )

      # Compute shared covariate balance arguments
      # Evaluates covariate balance across all covariates
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      # Compute covariate balance arguments
      # Uses default Table 1 specifications for standard covariates to report
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model fitting arguments
      # modelType = "cox": Uses Cox proportional hazards model
      # stratified = TRUE: Stratified by PS strata (accounts for PS-based matching/stratification)
      # useCovariates = FALSE: Does not adjust for additional covariates in outcome model
      # inversePtWeighting = FALSE: Not using inverse probability of treatment weighting
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
          noiseLevel = "quiet"
        )
      )

      # Create study population arguments
      # firstExposureOnly = TRUE: Only includes first exposure for each subject
      # washoutPeriod = 365: Requires 365 days of prior observation
      # removeDuplicateSubjects = "keep all": Retains all subject records
      # removeSubjectsWithPriorOutcome = TRUE: Excludes subjects with outcome before cohort start
      # priorOutcomeLookBack = 99999: Looks back unlimited days for prior outcomes
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = TRUE,
        washoutPeriod = 365,
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

      # Append the settings to analysis list
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

# Create the analysis specifications -------------------------------------------
# Combines all module specifications into a single analysis specification object
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save analysis specifications to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "legendt2dm", "legendt2dmAnalysisSpecification.json")
)