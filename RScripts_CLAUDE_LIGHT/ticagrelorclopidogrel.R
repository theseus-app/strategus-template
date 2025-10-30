################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates a Strategus analysis specification for a comparative
# effectiveness study of ticagrelor vs clopidogrel using the CohortMethod module.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
# ##############################################################################

library(dplyr)
library(Strategus)

# Shared Resources: Cohort Definitions ----------------------------------------
# ATLAS WebAPI base URL for retrieving cohort definitions and concept sets
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Export cohort definitions from ATLAS
# Target Cohort ID: 1794126 (target1)
# Comparator Cohort ID: 1794132 (comparator1)
# Outcome Cohort ID: 1794131 (outcome1)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal use: target=1, comparator=2, outcome=3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Shared Resources: Negative Control Outcomes --------------------------------
# Concept Set ID: 1888110 (negative)
# Retrieves negative control outcomes from the specified concept set
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
  dplyr::rename(
    outcomeConceptId = "conceptId",
    cohortName = "conceptName"
  ) %>%
  dplyr::mutate(cohortId = dplyr::row_number() + 100) %>%
  dplyr::select(cohortId, cohortName, outcomeConceptId)

# Validation: Ensure no duplicate cohort IDs between main and negative control cohorts
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Prepare Outcome Cohorts for CohortMethod Analysis ---------------------------
# Extract the outcome cohort (ID=3 from re-numbered cohorts)
# Set cleanWindow to 365 days for prior outcome lookback
oList <- cohortDefinitionSet %>%
  dplyr::filter(.data$cohortId == 3) %>%
  dplyr::mutate(
    outcomeCohortId = cohortId,
    outcomeCohortName = cohortName
  ) %>%
  dplyr::select(outcomeCohortId, outcomeCohortName) %>%
  dplyr::mutate(cleanWindow = 365)

# Prepare Target and Comparator Cohorts for CohortMethod Analysis -----------
# Pair target (ID=1) with comparator (ID=2)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Concepts to exclude from covariates: none specified in analysis specifications
# When not specified, default covariate settings will be used
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# CohortGeneratorModule Settings -----------------------------------------------
# Initialize the CohortGeneratorModule to create cohort specifications
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Create shared resource specifications for negative control outcomes
# First occurrence detected on concept and descendants
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications for cohort generation with statistics
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings --------------------------------------------
# Initialize the CohortDiagnosticsModule for cohort diagnostics
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Create comprehensive diagnostic specifications for all cohorts
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

# Define study periods from analysis specifications
# studyStartDate and studyEndDate in YYYYMMDD format
studyPeriods <- tibble::tibble(
  studyStartDate = c("20111101", "20130301"),
  studyEndDate = c("20190331", "20161231")
)

# Define Time-At-Risk (TAR) windows for outcome observation
# TAR 1: 1 to 365 days from cohort start
# TAR 2: 1 to 1825 days from cohort start
# TAR 3: 1 day from cohort start to cohort end
timeAtRisks <- tibble::tibble(
  label = c("TAR_1yr", "TAR_5yr", "TAR_cohortEnd"),
  riskWindowStart = c(1, 1, 1),
  startAnchor = c("cohort start", "cohort start", "cohort start"),
  riskWindowEnd = c(365, 1825, 0),
  endAnchor = c("cohort start", "cohort start", "cohort end"),
  minDaysAtRisk = c(1, 1, 1)
)

# Define Propensity Score Matching configurations
# PS Matching Config 1: 1:1 matching with 0.2 caliper on standardized logit
# PS Matching Config 2: 1:10 matching with 0.2 caliper on standardized logit
matchOnPsArgsList <- tibble::tibble(
  label = c("PS_Match_1to1", "PS_Match_1to10"),
  maxRatio = c(1, 10),
  caliper = c(0.2, 0.2),
  caliperScale = c("standardized logit", "standardized logit")
)

# Define Propensity Score Stratification configuration
# PS Stratification: 10 strata using all subjects for base selection
stratifyByPsArgsList <- tibble::tibble(
  label = c("PS_Stratify_10"),
  numberOfStrata = c(10),
  baseSelection = c("all")
)

# Build composite Propensity Score configuration list
# Each configuration includes method type, label, and parameters
psConfigList <- list()

# Convert PS Matching configurations to config list
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

# Convert PS Stratification configurations to config list
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

# Build CohortMethod Analysis List by iterating through all combinations
# Combinations: study periods × time-at-risk windows × PS adjustment methods
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Configure PS adjustment method: matching or stratification
      if (psCfg$method == "match") {
        # Propensity Score Matching arguments
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # Propensity Score Stratification arguments
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Define default covariate settings
      # Includes all standard covariates with descendants
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build outcome list combining outcomes of interest and negative controls
      outcomeList <- append(
        # Outcomes of interest (from oList)
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

      # GetDbCohortMethodData arguments
      # Restrict to common period: FALSE per specifications
      # Study period and max cohort size per specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Propensity Score estimation arguments
      # Uses Laplace prior with 10-fold cross-validation
      # Max cohort size for fitting: 250000
      # Error on high correlation: TRUE
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

      # Compute shared covariate balance before matching/stratification
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      # Compute covariate balance after matching/stratification
      # Using Table 1 specifications for main results
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model fitting arguments
      # Model type: Cox proportional hazards
      # Stratified: FALSE per specifications
      # Use covariates: FALSE (PS adjustment only)
      # Inverse probability weighting: FALSE (not used)
      # Laplace prior with 10-fold cross-validation
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = FALSE,
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
      # Washout period: 365 days
      # Remove duplicate subjects: keep all per specifications
      # Censor at new risk window: FALSE
      # Remove subjects with prior outcome: TRUE with 99999 day lookback
      # Risk window defined by time-at-risk specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
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

      # Create individual CohortMethod analysis specification
      # Each analysis combines one study period + time-at-risk + PS method
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s to %s; TAR: %s; PS: %s",
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

# Create CohortMethodModule Specifications ------------------------------------
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
# Combine all module specifications into single specification object
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() %>%
  Strategus::addSharedResources(cohortDefinitionShared) %>%
  Strategus::addSharedResources(negativeControlsShared) %>%
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save Analysis Specifications ------------------------------------------------
# Export to JSON format for execution by Strategus
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ticagrelorclopidogrel", "ticagrelorclopidogrelAnalysisSpecification.json")
)

message("Analysis specification created successfully!")
message(sprintf("Total CohortMethod analyses: %d", length(cmAnalysisList)))