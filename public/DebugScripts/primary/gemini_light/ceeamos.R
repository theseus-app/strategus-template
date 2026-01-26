library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Base URL for the OHDSI WebAPI, used to retrieve cohort definitions and concept sets.
# This value is not specified in the Analysis Specifications, using a common demo URL.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Retrieve cohort definitions from WebAPI based on IDs provided in Analysis Specifications.
# targetCohort: id = 1794126, name = "target1"
# comparatorCohort: id = 1794132, name = "comparator1"
# outcomeCohort: id = 1794131, name = "outcome1"
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to a simpler scheme (1, 2, 3) for internal use in the study.
# This helps in consistently referring to target, comparator, and outcome.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Target cohort re-numbered to 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Comparator cohort re-numbered to 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Outcome cohort re-numbered to 3

# Negative control outcomes
# Retrieve negative control concept set definition from WebAPI.
# negativeControlConceptSet: id = 1888110, name = "negative"
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # From Analysis Specifications: negativeControlConceptSet.id
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
  # Assign cohort IDs starting from 101 to avoid collision with target/comparator/outcome cohorts (1, 2, 3).
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs to prevent conflicts.
# The original script's use of c() is correct for checking duplicates across two vectors.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts used in each analysis ---------------
# Outcomes: Filter for the outcome cohort (re-numbered to 3).
# cleanWindow is set to 365 days, a common default for outcome clean-up.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Outcome cohort (re-numbered ID 3)
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Uses the re-numbered target (1) and comparator (2) cohort IDs and names.
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered target cohort ID
  targetCohortName = "target1", # From Analysis Specifications: targetCohort.name
  comparatorCohortId = 2, # Re-numbered comparator cohort ID
  comparatorCohortName = "comparator1" # From Analysis Specifications: comparatorCohort.name
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study.
# Analysis Specifications has conceptsToExclude as null/empty, so this list will be empty.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all
# Analysis Specifications has conceptsToInclude as null/empty, so this is not used.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default setting
  detectOnDescendants = TRUE # Default setting
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Default setting
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE, # Default setting
  runIncludedSourceConcepts = TRUE, # Default setting
  runOrphanConcepts = TRUE, # Default setting
  runTimeSeries = FALSE, # Default setting
  runVisitContext = TRUE, # Default setting
  runBreakdownIndexEvents = TRUE, # Default setting
  runIncidenceRate = TRUE, # Default setting
  runCohortRelationship = TRUE, # Default setting
  runTemporalCohortCharacterization = TRUE, # Default setting
  minCharacterizationMean = 0.01 # Default setting
)

# CohortMethodModule -----------------------------------------------------------

# Study periods for restricting the analysis.
# From Analysis Specifications: getDbCohortMethodDataArgs.studyPeriods are null, so no restriction.
studyPeriods <- tibble(
  studyStartDate = c(), # YYYYMMDD
  studyEndDate   = c()  # YYYYMMDD
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# From Analysis Specifications: createStudyPopArgs.timeAtRisks
timeAtRisks <- tibble(
  label = c("TAR1"), # A descriptive label for this TAR
  riskWindowStart  = c(1), # From Analysis Specifications: timeAtRisks.riskWindowStart
  startAnchor = c("cohort start"), # From Analysis Specifications: timeAtRisks.startAnchor
  riskWindowEnd  = c(0), # From Analysis Specifications: timeAtRisks.riskWindowEnd
  endAnchor = c("cohort end"), # From Analysis Specifications: timeAtRisks.endAnchor
  minDaysAtRisk = c(1) # Added to support timeAtRisks$minDaysAtRisk[t] in createStudyPopArgs
)

# Propensity Score settings - match on PS
# From Analysis Specifications: propensityScoreAdjustment.psSettings.matchOnPsArgs
matchOnPsArgsList <- tibble(
  label = c("Match_10_0.2_SL"), # A descriptive label for this PS matching setting
  maxRatio  = c(10), # From Analysis Specifications: matchOnPsArgs.maxRatio
  caliper = c(0.2), # From Analysis Specifications: matchOnPsArgs.caliper
  caliperScale  = c("standardized logit") # From Analysis Specifications: matchOnPsArgs.caliperScale
)

# Propensity Score settings - stratify by PS
# From Analysis Specifications: propensityScoreAdjustment.psSettings.stratifyByPsArgs is null, so this list is empty.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c() # "all" | "target" | "comparator"
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
# FIX: Changed 'stratifyByPsList' to 'stratifyByPsArgsList' to match variable name.
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

# If no study periods are defined, create a dummy entry to ensure the loop runs once.
if (nrow(studyPeriods) == 0) {
  studyPeriods <- tibble(studyStartDate = NA_character_, studyEndDate = NA_character_)
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

      # Create matching or stratification arguments based on PS configuration
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper, # FIX: Removed extra ')' from template
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default setting
          stratificationColumns = c() # Default setting
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default setting
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings for feature extraction. Using default settings.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default setting
      )

      # Define outcomes for the CohortMethod analysis.
      # Includes the primary outcome and all negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not applicable for observational studies
            priorOutcomeLookback = 99999 # Default setting
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # For negative controls, true effect size is assumed to be 1 (no effect)
          )
        })
      )

      # Define target-comparator-outcome combinations.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # excludedCovariateConceptIds: From Analysis Specifications, covariateSelection.conceptsToExclude is empty.
          # cmTcList does not contain target/comparator concept IDs, so these are removed.
          excludedCovariateConceptIds = c(excludedCovariateConcepts$conceptId)
        )
      }

      # Arguments for fetching cohort method data from the database.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Default setting
        studyStartDate = studyStartDate, # From Analysis Specifications: getDbCohortMethodDataArgs.studyPeriods (null, so no restriction)
        studyEndDate = studyEndDate, # From Analysis Specifications: getDbCohortMethodDataArgs.studyPeriods (null, so no restriction)
        maxCohortSize = 0, # From Analysis Specifications: getDbCohortMethodDataArgs.maxCohortSize
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # From Analysis Specifications: propensityScoreAdjustment.createPsArgs
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications: createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From Analysis Specifications: createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att", # Default setting
        prior = Cyclops::createPrior( # From Analysis Specifications: createPsArgs.prior
          priorType = "laplace", # From Analysis Specifications: createPsArgs.prior.priorType
          exclude = c(0), # Default setting for intercept
          useCrossValidation = TRUE # From Analysis Specifications: createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # From Analysis Specifications: createPsArgs.control
          noiseLevel = "silent", # From Analysis Specifications: createPsArgs.control.noiseLevel
          cvType = "auto", # From Analysis Specifications: createPsArgs.control.cvType
          seed = 1, # Default setting for reproducibility
          resetCoefficients = TRUE, # From Analysis Specifications: createPsArgs.control.resetCoefficients
          tolerance = 2e-07, # From Analysis Specifications: createPsArgs.control.tolerance
          cvRepetitions = 10, # From Analysis Specifications: createPsArgs.control.cvRepetitions (template was 1, updated to 10)
          startingVariance = 0.01 # From Analysis Specifications: createPsArgs.control.startingVariance
        )
      )

      # Arguments for computing shared covariate balance.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default setting
        covariateFilter = NULL # Default setting
      )
      # Arguments for computing covariate balance.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default setting
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default setting
      )

      # Arguments for fitting the outcome model.
      # From Analysis Specifications: fitOutcomeModelArgs
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From Analysis Specifications: fitOutcomeModelArgs.modelType
        stratified = TRUE, # From Analysis Specifications: fitOutcomeModelArgs.stratified
        useCovariates = FALSE, # From Analysis Specifications: fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE, # From Analysis Specifications: fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior( # From Analysis Specifications: fitOutcomeModelArgs.prior
          priorType = "laplace", # From Analysis Specifications: fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE # From Analysis Specifications: fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # From Analysis Specifications: fitOutcomeModelArgs.control
          cvType = "auto", # From Analysis Specifications: fitOutcomeModelArgs.control.cvType
          seed = 1, # Default setting for reproducibility
          resetCoefficients = TRUE, # From Analysis Specifications: fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01, # From Analysis Specifications: fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07, # From Analysis Specifications: fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10, # From Analysis Specifications: fitOutcomeModelArgs.control.cvRepetitions (template was 1, updated to 10)
          noiseLevel = "quiet" # From Analysis Specifications: fitOutcomeModelArgs.control.noiseLevel
        )
      )

      # Arguments for creating the study population.
      # From Analysis Specifications: createStudyPopArgs
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications: createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE, # From Analysis Specifications: createStudyPopArgs.firstExposureOnly
        washoutPeriod = 365, # From Analysis Specifications: createStudyPopArgs.washoutPeriod (template was 0, updated to 365)
        removeDuplicateSubjects = "remove all", # From Analysis Specifications: createStudyPopArgs.removeDuplicateSubjects (template was "keep first", updated to "remove all")
        censorAtNewRiskWindow = FALSE, # From Analysis Specifications: createStudyPopArgs.censorAtNewRiskWindow (template was TRUE, updated to FALSE)
        removeSubjectsWithPriorOutcome = TRUE, # From Analysis Specifications: createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 365, # From Analysis Specifications: createStudyPopArgs.priorOutcomeLookBack (template was 99999, updated to 365)
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From Analysis Specifications: createStudyPopArgs.timeAtRisks.riskWindowStart
        startAnchor = timeAtRisks$startAnchor[t], # From Analysis Specifications: createStudyPopArgs.timeAtRisks.startAnchor
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From Analysis Specifications: createStudyPopArgs.timeAtRisks.riskWindowEnd
        endAnchor = timeAtRisks$endAnchor[t], # From Analysis Specifications: createStudyPopArgs.timeAtRisks.endAnchor
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # Now valid as minDaysAtRisk is added to timeAtRisks tibble
        maxDaysAtRisk = 99999 # Default setting, not in Analysis Specifications
      )


      # Append the settings to Analysis List
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

cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Default setting
  refitPsForEveryOutcome = FALSE, # Default setting
  refitPsForEveryStudyPopulation = FALSE, # Default setting
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default setting
)

# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path uses the 'name' from Analysis Specifications: "ceeamos".
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ceeamos", "ceeamosAnalysisSpecification.json")
)