################################################################################
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# further details.
# ##############################################################################
library(dplyr)
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(ParallelLogger)

# Shared Resources -------------------------------------------------------------
# Base URL for the OHDSI WebAPI (Atlas instance)
# This is not specified in the Analysis Specifications, using a common demo URL.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Retrieve cohort definitions from WebAPI based on IDs provided in Analysis Specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: tramadolcodein.cohortDefinitions.targetCohort.id
    1794132, # Comparator: tramadolcodein.cohortDefinitions.comparatorCohort.id
    1794131  # Outcome: tramadolcodein.cohortDefinitions.outcomeCohort[0].id
  ),
  generateStats = TRUE
)

# Re-number cohorts to a simplified scheme (1 for target, 2 for comparator, 3 for outcome)
# This re-numbering is for internal use within the Strategus analysis.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Target cohort ID
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Comparator cohort ID
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Outcome cohort ID

# Negative control outcomes
# Retrieve negative control concept set definition from WebAPI.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # tramadolcodein.negativeControlConceptSet.id
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
  # Assign unique cohort IDs for negative controls, starting from 101 to avoid conflicts with T/C/O.
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs to prevent conflicts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts used in each analysis ---------------
# Outcomes: Filter for the outcome cohort (re-numbered to 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # cleanWindow is not specified in Analysis Specifications, using template default.
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered target cohort ID
  targetCohortName = "target1", # tramadolcodein.cohortDefinitions.targetCohort.name
  comparatorCohortId = 2, # Re-numbered comparator cohort ID
  comparatorCohortName = "comparator1" # tramadolcodein.cohortDefinitions.comparatorCohort.name
)

# For the CohortMethod LSPS we'll need to exclude specific concepts.
# Based on tramadolcodein.covariateSelection.conceptsToExclude, which is empty.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# Based on tramadolcodein.covariateSelection.conceptsToInclude, which is empty.
# includedCovariateConcepts <- data.frame(
#   conceptId = integer(0),
#   conceptName = character(0)
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Not specified in Analysis Specifications, using template default.
  detectOnDescendants = TRUE # Not specified in Analysis Specifications, using template default.
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Not specified in Analysis Specifications, using template default.
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE, # Not specified in Analysis Specifications, using template default.
  runIncludedSourceConcepts = TRUE, # Not specified in Analysis Specifications, using template default.
  runOrphanConcepts = TRUE, # Not specified in Analysis Specifications, using template default.
  runTimeSeries = FALSE, # Not specified in Analysis Specifications, using template default.
  runVisitContext = TRUE, # Not specified in Analysis Specifications, using template default.
  runBreakdownIndexEvents = TRUE, # Not specified in Analysis Specifications, using template default.
  runIncidenceRate = TRUE, # Not specified in Analysis Specifications, using template default.
  runCohortRelationship = TRUE, # Not specified in Analysis Specifications, using template default.
  runTemporalCohortCharacterization = TRUE, # Not specified in Analysis Specifications, using template default.
  minCharacterizationMean = 0.01 # Not specified in Analysis Specifications, using template default.
)

# CohortMethodModule -----------------------------------------------------------

# Study periods for restricting the analysis.
# tramadolcodein.getDbCohortMethodDataArgs.studyPeriods has null start/end dates,
# indicating no restriction. We use NA and convert to NULL later.
studyPeriods <- tibble(
  studyStartDate = as.character(NA), # YYYYMMDD
  studyEndDate   = as.character(NA)  # YYYYMMDD
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# Based on tramadolcodein.createStudyPopArgs.timeAtRisks.
timeAtRisks <- tibble(
  label = "TAR_1_0_cohort_start_cohort_end", # Descriptive label for this TAR
  riskWindowStart  = 1, # tramadolcodein.createStudyPopArgs.timeAtRisks[0].riskWindowStart
  startAnchor = "cohort start", # tramadolcodein.createStudyPopArgs.timeAtRisks[0].startAnchor
  riskWindowEnd  = 0, # tramadolcodein.createStudyPopArgs.timeAtRisks[0].riskWindowEnd
  endAnchor = "cohort end", # tramadolcodein.createStudyPopArgs.timeAtRisks[0].endAnchor
  minDaysAtRisk = 1 # tramadolcodein.createStudyPopArgs.timeAtRisks[0].minDaysAtRisk
)

# Propensity Score settings - match on PS
# Based on tramadolcodein.propensityScoreAdjustment.psSettings[0].matchOnPsArgs.
matchOnPsArgsList <- tibble(
  label = "Match_maxRatio1_caliper0.2_stdLogit", # Descriptive label
  maxRatio  = 1, # tramadolcodein.propensityScoreAdjustment.psSettings[0].matchOnPsArgs.maxRatio
  caliper = 0.2, # tramadolcodein.propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliper
  caliperScale  = "standardized logit" # tramadolcodein.propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliperScale
)

# Propensity Score settings - stratify by PS
# Based on tramadolcodein.propensityScoreAdjustment.psSettings[0].stratifyByPsArgs, which is null.
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata = integer(0),
  baseSelection = character(0)
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# If a data frame for "match on PS" exists and has rows, convert each row to a config
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    # Append a new element at the end of psConfigList
    psConfigList[[length(psConfigList) + 1]] <- list(
      # Identify the PS adjustment method for this config
      method = "match",
      # Human-readable label to carry through into descriptions
      label  = matchOnPsArgsList$label[i],
      # Parameter bundle passed to createMatchOnPsArgs later
      params = list(
        maxRatio     = matchOnPsArgsList$maxRatio[i],
        caliper      = matchOnPsArgsList$caliper[i],
        caliperScale = matchOnPsArgsList$caliperScale[i]
      )
    )
  }
}

# If a data frame for "stratify by PS" exists and has rows, convert each row to a config
if (exists("stratifyByPsArgsList") && nrow(stratifyByPsArgsList) > 0) {
  for (i in seq_len(nrow(stratifyByPsArgsList))) {
    # Append a new element at the end of psConfigList
    psConfigList[[length(psConfigList) + 1]] <- list(
      # Identify the PS adjustment method for this config
      method = "stratify",
      # Human-readable label to carry through into descriptions
      label  = stratifyByPsArgsList$label[i],
      # Parameter bundle passed to createStratifyByPsArgs later
      params = list(
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection  = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}


# Iterate through all analysis setting combinations
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  # Convert NA study dates to NULL for CohortMethod functions
  currentStudyStartDate <- if (is.na(studyPeriods$studyStartDate[s])) NULL else studyPeriods$studyStartDate[s]
  currentStudyEndDate <- if (is.na(studyPeriods$studyEndDate[s])) NULL else studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Configure PS adjustment arguments based on the method
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio, # tramadolcodein.propensityScoreAdjustment.psSettings[0].matchOnPsArgs.maxRatio
          caliper = psCfg$params$caliper, # tramadolcodein.propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliper
          caliperScale = psCfg$params$caliperScale, # tramadolcodein.propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliperScale
          allowReverseMatch = FALSE, # Not specified in Analysis Specifications, using template default.
          stratificationColumns = c() # Not specified in Analysis Specifications, using template default.
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata, # From stratifyByPsArgsList
          stratificationColumns = c(), # Not specified in Analysis Specifications, using template default.
          baseSelection = psCfg$params$baseSelection # From stratifyByPsArgsList
        )
      }

      # Default covariate settings for feature extraction
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Not specified in Analysis Specifications, using template default.
      )

      # Define outcomes for the CohortMethod analysis
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE, # Main outcome
            trueEffectSize = NA, # Not applicable for main outcome
            priorOutcomeLookback = 99999 # Not specified in Analysis Specifications, using template default.
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE, # Negative control outcome
            trueEffectSize = 1 # Assumed true effect size for negative controls
          )
        })
      )

      # Define target-comparator-outcome combinations
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude concepts from covariates.
          # tramadolcodein.covariateSelection.conceptsToExclude is empty.
          # The template's targetConceptId/comparatorConceptId are not drug concepts, so removed.
          excludedCovariateConceptIds = c(
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # Arguments for fetching data from the database
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Not specified in Analysis Specifications, using template default.
        studyStartDate = currentStudyStartDate, # tramadolcodein.getDbCohortMethodDataArgs.studyPeriods[0].studyStartDate (converted from NA to NULL)
        studyEndDate = currentStudyEndDate, # tramadolcodein.getDbCohortMethodDataArgs.studyPeriods[0].studyEndDate (converted from NA to NULL)
        maxCohortSize = 0, # tramadolcodein.getDbCohortMethodDataArgs.maxCohortSize
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # tramadolcodein.propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # tramadolcodein.propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Not specified in Analysis Specifications, using template default.
        estimator = "att", # Not specified in Analysis Specifications, using template default.
        prior = Cyclops::createPrior( # tramadolcodein.propensityScoreAdjustment.createPsArgs.prior
          priorType = "laplace", # tramadolcodein.propensityScoreAdjustment.createPsArgs.prior.priorType
          exclude = c(0), # Not specified in Analysis Specifications, using template default.
          useCrossValidation = TRUE # tramadolcodein.propensityScoreAdjustment.createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # tramadolcodein.propensityScoreAdjustment.createPsArgs.control
          noiseLevel = "silent", # tramadolcodein.propensityScoreAdjustment.createPsArgs.control.noiseLevel
          cvType = "auto", # tramadolcodein.propensityScoreAdjustment.createPsArgs.control.cvType
          seed = 1, # Not specified in Analysis Specifications, using template default.
          resetCoefficients = TRUE, # tramadolcodein.propensityScoreAdjustment.createPsArgs.control.resetCoefficients
          tolerance = 2e-07, # tramadolcodein.propensityScoreAdjustment.createPsArgs.control.tolerance
          cvRepetitions = 10, # tramadolcodein.propensityScoreAdjustment.createPsArgs.control.cvRepetitions
          startingVariance = 0.01, # tramadolcodein.propensityScoreAdjustment.createPsArgs.control.startingVariance
          fold = 10 # tramadolcodein.propensityScoreAdjustment.createPsArgs.control.fold
        )
      )

      # Arguments for computing shared covariate balance (e.g., before PS adjustment)
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Not specified in Analysis Specifications, using template default.
        covariateFilter = NULL # Not specified in Analysis Specifications, using template default.
      )
      # Arguments for computing covariate balance (e.g., after PS adjustment)
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Not specified in Analysis Specifications, using template default.
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Not specified in Analysis Specifications, using template default.
      )

      # Arguments for fitting the outcome model
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # tramadolcodein.fitOutcomeModelArgs.modelType
        stratified = FALSE, # tramadolcodein.fitOutcomeModelArgs.stratified
        useCovariates = FALSE, # tramadolcodein.fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE, # tramadolcodein.fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior( # tramadolcodein.fitOutcomeModelArgs.prior
          priorType = "laplace", # tramadolcodein.fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE # tramadolcodein.fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # tramadolcodein.fitOutcomeModelArgs.control
          cvType = "auto", # tramadolcodein.fitOutcomeModelArgs.control.cvType
          seed = 1, # Not specified in Analysis Specifications, using template default.
          resetCoefficients = TRUE, # tramadolcodein.fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01, # tramadolcodein.fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07, # tramadolcodein.fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10, # tramadolcodein.fitOutcomeModelArgs.control.cvRepetitions
          noiseLevel = "quiet", # tramadolcodein.fitOutcomeModelArgs.control.noiseLevel
          fold = 10 # tramadolcodein.fitOutcomeModelArgs.control.fold
        )
      )

      # Arguments for creating the study population
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # tramadolcodein.createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE, # tramadolcodein.createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0, # tramadolcodein.createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # tramadolcodein.createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # tramadolcodein.createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # tramadolcodein.createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 365, # tramadolcodein.createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t], # tramadolcodein.createStudyPopArgs.timeAtRisks[0].riskWindowStart
        startAnchor = timeAtRisks$startAnchor[t], # tramadolcodein.createStudyPopArgs.timeAtRisks[0].startAnchor
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # tramadolcodein.createStudyPopArgs.timeAtRisks[0].riskWindowEnd
        endAnchor = timeAtRisks$endAnchor[t], # tramadolcodein.createStudyPopArgs.timeAtRisks[0].endAnchor
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # tramadolcodein.createStudyPopArgs.timeAtRisks[0].minDaysAtRisk
        maxDaysAtRisk = 99999 # Not specified in Analysis Specifications, using template default.
      )

      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          if (is.null(currentStudyStartDate)) "NoStartDate" else currentStudyStartDate,
          if (is.null(currentStudyEndDate)) "NoEndDate" else currentStudyEndDate,
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

cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Not specified in Analysis Specifications, using template default.
  refitPsForEveryOutcome = FALSE, # Not specified in Analysis Specifications, using template default.
  refitPsForEveryStudyPopulation = FALSE, # Not specified in Analysis Specifications, using template default.
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Not specified in Analysis Specifications, using template default.
)

# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file
# The file path is constructed using the name from Analysis Specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "tramadolcodein", "tramadolcodeinAnalysisSpecification.json")
)