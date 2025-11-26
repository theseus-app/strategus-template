################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates a Strategus analysis specification for the uveitissafety study.
# It defines cohort definitions, negative control outcomes, and CohortMethod analyses
# with propensity score adjustment and outcome modeling.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
# ##############################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Configuration for accessing ATLAS cohort and concept set definitions
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions ----------------------------------------------------------
# Export cohort definitions from ATLAS
# targetCohort (1794126): target1
# comparatorCohort (1794132): comparator1
# outcomeCohort (1794131): outcome1
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal analysis reference
# Target cohort -> 1, Comparator cohort -> 2, Outcome cohort -> 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative Control Outcomes ---------------------------------------------------
# Export negative control concept set (1888110: negative) and resolve concepts
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
  # Assign cohort IDs starting at 101 to avoid conflicts with main cohorts (1, 2, 3)
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Verify no duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create outcome list for CohortMethod analysis --------------------------------
# Main outcomes of interest (outcome1)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName)

# Target and Comparator pairing for CohortMethod --------------------------------
# Defines the comparison: target1 vs comparator1
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# CohortGeneratorModule -------------------------------------------------------
# Generates cohorts and negative control outcomes from definitions
cgModuleSettingsCreator <- CohortGeneratorModule$new()

cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings -------------------------------------------
# Runs diagnostics on all cohorts to assess data quality
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

# CohortMethodModule Settings ------------------------------------------------

# Study periods: empty string indicates no study period restriction
# Format: YYYYMMDD
studyPeriods <- tibble(
  studyStartDate = c(""),
  studyEndDate   = c("")
)

# Time-at-Risk (TAR) configurations from specification
# TAR 1: From cohort start (day 1) to cohort end (day 0)
# TAR 2: From cohort start (day 1) to 99999 days after cohort start
timeAtRisks <- tibble(
  label = c(
    "TAR: cohort start to cohort end",
    "TAR: cohort start to 99999 days after cohort start"
  ),
  riskWindowStart  = c(1, 1),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd  = c(0, 99999),
  endAnchor = c("cohort end", "cohort start"),
  minDaysAtRisk = c(1, 1)
)

# Propensity Score (PS) matching configurations
# Configuration 1: Match with maxRatio=10, caliper=0.2
# Configuration 2: Match with maxRatio=1, caliper=0.2
matchOnPsArgsList <- tibble(
  label = c(
    "PS Match: maxRatio=10, caliper=0.2",
    "PS Match: maxRatio=1, caliper=0.2"
  ),
  maxRatio  = c(10, 1),
  caliper = c(0.2, 0.2),
  caliperScale  = c("standardized logit", "standardized logit")
)

# Build PS configuration list from matchOnPsArgsList
psConfigList <- list()

if (nrow(matchOnPsArgsList) > 0) {
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

# Generate CohortMethod analysis list by iterating through all setting combinations
cmAnalysisList <- list()
analysisId <- 1

# Iterate through study periods
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  # Iterate through time-at-risk windows
  for (t in seq_len(nrow(timeAtRisks))) {

    # Iterate through propensity score configurations
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Create PS matching arguments
      matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
        maxRatio = psCfg$params$maxRatio,
        caliper = psCfg$params$caliper,
        caliperScale = psCfg$params$caliperScale,
        allowReverseMatch = FALSE,
        stratificationColumns = c()
      )
      stratifyByPsArgs <- NULL

      # Create default covariate settings
      # addDescendantsToExclude: TRUE includes descendant concepts when excluding
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build outcome list: combine outcomes of interest with negative controls
      outcomeList <- append(
        # Main outcomes of interest (outcome1) with priorOutcomeLookback=99999
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        # Negative control outcomes with trueEffectSize=1 (null effect expected)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Create target-comparator-outcome combinations
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # excludedCovariateConceptIds: empty per covariateSelection specification
          excludedCovariateConceptIds = c()
        )
      }

      # Get database cohort method data arguments
      # restrictToCommonPeriod: TRUE per getDbCohortMethodDataArgs specification
      # maxCohortSize: 0 (no limit) per specification
      # washoutPeriod: 0 per specification
      # firstExposureOnly: FALSE per specification
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Create propensity score fitting arguments
      # Uses Laplace prior with cross-validation
      # maxCohortSizeForFitting: 250000 per specification
      # errorOnHighCorrelation: TRUE per specification
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
          startingVariance = 0.01
        )
      )

      # Compute shared covariate balance (before PS adjustment)
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      # Compute covariate balance after PS adjustment using Table 1 specifications
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit outcome model arguments
      # modelType: cox per specification
      # stratified: TRUE per specification
      # useCovariates: FALSE per specification
      # inversePtWeighting: FALSE per specification
      # Uses Laplace prior with cross-validation
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
          noiseLevel = "quiet"
        )
      )

      # Create study population arguments
      # restrictToCommonPeriod: FALSE per specification
      # firstExposureOnly: FALSE per specification
      # washoutPeriod: 0 per specification
      # removeDuplicateSubjects: "keep all" per specification
      # removeSubjectsWithPriorOutcome: TRUE per specification
      # priorOutcomeLookback: 99999 per specification
      # censorAtNewRiskWindow: FALSE per specification
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
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t]
      )

      # Create CohortMethod analysis combining all arguments
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study Period: %s-%s; TAR: %s; PS: %s",
          ifelse(studyStartDate == "", "unrestricted", studyStartDate),
          ifelse(studyEndDate == "", "unrestricted", studyEndDate),
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

# Create the complete analysis specifications -----------------------------------
# Combines all module specifications into a single Strategus specification object
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save analysis specifications to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "uveitissafety", "uveitissafetyAnalysisSpecification.json")
)

# Confirmation message
cat("Analysis specification created successfully for study: uveitissafety\n")
cat("Study includes:\n")
cat("  - Target cohort: target1 (1794126)\n")
cat("  - Comparator cohort: comparator1 (1794132)\n")
cat("  - Outcome cohort: outcome1 (1794131)\n")
cat("  - Negative controls: negative (1888110)\n")
cat("  - Time-at-risk windows: 2\n")
cat("  - PS configurations: 2 (match-based)\n")
cat(sprintf("  - Total CM analyses: %d\n", length(cmAnalysisList)))