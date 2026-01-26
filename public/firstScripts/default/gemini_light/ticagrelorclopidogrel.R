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

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
# Base URL for the WebAPI instance to retrieve cohort definitions.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Retrieve cohort definitions from WebAPI based on IDs provided in Analysis Specifications.
# The cohort IDs are re-numbered internally for consistent use across the study.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1 (from Analysis Specifications)
    1794132, # Comparator: comparator1 (from Analysis Specifications)
    1794131  # Outcome: outcome1 (from Analysis Specifications)
  ),
  generateStats = TRUE
)

# Re-number cohorts to a simplified scheme (1 for target, 2 for comparator, 3 for outcome).
# This makes it easier to reference them in the analysis settings.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve the concept set definition for negative controls from WebAPI.
# The concept set ID is from Analysis Specifications -> negativeControlConceptSet.id.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # From Analysis Specifications -> negativeControlConceptSet.id
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
  # Assign unique cohort IDs for negative controls, starting from 101 to avoid collision
  # with target/comparator/outcome cohorts (1, 2, 3).
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs to prevent errors in Strategus.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Filter for the main outcome cohort (ID 3 after re-numbering).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Outcome cohort ID is 3 after re-numbering
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # cleanWindow is not specified in Analysis Specifications, using template default.
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis.
# Uses the re-numbered cohort IDs and original names from Analysis Specifications.
cmTcList <- data.frame(
  targetCohortId = 1, # Renumbered target cohort ID
  targetCohortName = "target1", # Original target cohort name from Analysis Specifications
  comparatorCohortId = 2, # Renumbered comparator cohort ID
  comparatorCohortName = "comparator1" # Original comparator cohort name from Analysis Specifications
)

# For the CohortMethod LSPS we'll need to exclude specific concepts.
# Based on Analysis Specifications -> covariateSelection.conceptsToExclude.
# Since the JSON specifies an empty list for conceptsToExclude, this data frame will be empty.
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(0),
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all.
# Based on Analysis Specifications -> covariateSelection.conceptsToInclude.
# Since the JSON specifies an empty list for conceptsToInclude, this data frame will be empty.
includedCovariateConcepts <- data.frame(
  conceptId = numeric(0),
  conceptName = character(0)
)

# CohortGeneratorModule --------------------------------------------------------
# Creates shared resources and module specifications for CohortGenerator.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default from template
  detectOnDescendants = TRUE # Default from template
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Default from template
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# Creates module specifications for CohortDiagnostics.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # All defined cohorts
  runInclusionStatistics = TRUE, # Default from template
  runIncludedSourceConcepts = TRUE, # Default from template
  runOrphanConcepts = TRUE, # Default from template
  runTimeSeries = FALSE, # Default from template
  runVisitContext = TRUE, # Default from template
  runBreakdownIndexEvents = TRUE, # Default from template
  runIncidenceRate = TRUE, # Default from template
  runCohortRelationship = TRUE, # Default from template
  runTemporalCohortCharacterization = TRUE, # Default from template
  minCharacterizationMean = 0.01 # Default from template
)

# CohortMethodModule -----------------------------------------------------------

# Study periods for data extraction.
# Populated from Analysis Specifications -> getDbCohortMethodDataArgs.studyPeriods.
studyPeriods <- tibble(
  studyStartDate = c("20111101", "20130301"), # YYYYMMDD
  studyEndDate   = c("20190331", "20161231")  # YYYYMMDD
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# Populated from Analysis Specifications -> createStudyPopArgs.timeAtRisks.
timeAtRisks <- tibble(
  label = c(
    "TAR 1-365 days from cohort start",
    "TAR 1-1825 days from cohort start",
    "TAR 1-cohort end days from cohort start",
    "TAR 29-365 days from cohort start",
    "TAR 29-1825 days from cohort start",
    "TAR 29-cohort end days from cohort start"
  ),
  riskWindowStart  = c(1, 1, 1, 29, 29, 29), # From createStudyPopArgs.timeAtRisks
  startAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start", "cohort start"), # From createStudyPopArgs.timeAtRisks
  riskWindowEnd  = c(365, 1825, 0, 365, 1825, 0), # From createStudyPopArgs.timeAtRisks
  endAnchor = c("cohort start", "cohort start", "cohort end", "cohort start", "cohort start", "cohort end"), # From createStudyPopArgs.timeAtRisks
  minDaysAtRisk = c(1, 1, 1, 1, 1, 1) # From createStudyPopArgs.timeAtRisks
)

# Propensity Score settings - match on PS.
# Populated from Analysis Specifications -> propensityScoreAdjustment.psSettings where matchOnPsArgs is not null.
matchOnPsArgsList <- tibble(
  label = c("Match on PS (maxRatio 1)", "Match on PS (maxRatio 10)"),
  maxRatio  = c(1, 10), # From propensityScoreAdjustment.psSettings[i].matchOnPsArgs.maxRatio
  caliper = c(0.2, 0.2), # From propensityScoreAdjustment.psSettings[i].matchOnPsArgs.caliper
  caliperScale  = c("standardized logit", "standardized logit") # From propensityScoreAdjustment.psSettings[i].matchOnPsArgs.caliperScale
)

# Propensity Score settings - stratify by PS.
# Populated from Analysis Specifications -> propensityScoreAdjustment.psSettings where stratifyByPsArgs is not null.
stratifyByPsArgsList <- tibble(
  label = c("Stratify by PS (10 strata)"),
  numberOfStrata  = c(10), # From propensityScoreAdjustment.psSettings[i].stratifyByPsArgs.numberOfStrata
  baseSelection = c("all") # From propensityScoreAdjustment.psSettings[i].stratifyByPsArgs.baseSelection
)

# Build a single PS configuration list (each entry has: method, label, params).
# This structure allows iterating through different PS adjustment methods.
psConfigList <- list()

# If a data frame for "match on PS" exists and has rows, convert each row to a config.
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

# If a data frame for "stratify by PS" exists and has rows, convert each row to a config.
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

# Iterate through all analysis setting combinations (study periods, time-at-risks, PS adjustments).
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Configure PS adjustment arguments based on the current PS configuration.
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default from template
          stratificationColumns = c() # Default from template
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default from template
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings for feature extraction.
      # Uses default settings, with explicit inclusion/exclusion from Analysis Specifications.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE, # Default from template
        excludedCovariateConceptIds = excludedCovariateConcepts$conceptId, # From Analysis Specifications -> covariateSelection.conceptsToExclude (empty)
        includedCovariateConceptIds = includedCovariateConcepts$conceptId # From Analysis Specifications -> covariateSelection.conceptsToInclude (empty)
      )

      # Define outcomes for the CohortMethod analysis.
      # Includes the main outcome and all negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not applicable for observed outcomes
            priorOutcomeLookback = 99999 # Default from template
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
          # Excluded covariate concept IDs.
          # Only uses concepts explicitly listed in Analysis Specifications -> covariateSelection.conceptsToExclude.
          # The template's placeholders for target/comparator concept IDs are removed as they are not defined in JSON.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Arguments for retrieving cohort method data from the database.
      # Populated from Analysis Specifications -> getDbCohortMethodDataArgs.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # From Analysis Specifications -> getDbCohortMethodDataArgs.restrictToCommonPeriod
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From Analysis Specifications -> getDbCohortMethodDataArgs.maxCohortSize
        covariateSettings = covariateSettings
        # firstExposureOnly, washoutPeriod, removeDuplicateSubjects are not arguments for createGetDbCohortMethodDataArgs.
        # They are handled in createStudyPopulationArgs.
      )

      # Arguments for creating propensity scores.
      # Populated from Analysis Specifications -> propensityScoreAdjustment.createPsArgs.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications -> propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From Analysis Specifications -> propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail (default from template)
        estimator = "att", # Default from template
        prior = Cyclops::createPrior( # Prior settings for regularization
          priorType = "laplace", # From Analysis Specifications -> propensityScoreAdjustment.createPsArgs.prior.priorType
          exclude = c(0), # Default from template
          useCrossValidation = TRUE # From Analysis Specifications -> propensityScoreAdjustment.createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings for Cyclops solver
          noiseLevel = "silent", # From Analysis Specifications -> propensityScoreAdjustment.createPsArgs.control.noiseLevel
          cvType = "auto", # From Analysis Specifications -> propensityScoreAdjustment.createPsArgs.control.cvType
          seed = 1, # Default from template
          resetCoefficients = TRUE, # From Analysis Specifications -> propensityScoreAdjustment.createPsArgs.control.resetCoefficients
          tolerance = 2e-07, # From Analysis Specifications -> propensityScoreAdjustment.createPsArgs.control.tolerance
          cvRepetitions = 10, # From Analysis Specifications -> propensityScoreAdjustment.createPsArgs.control.cvRepetitions
          startingVariance = 0.01 # From Analysis Specifications -> propensityScoreAdjustment.createPsArgs.control.startingVariance
        )
      )

      # Arguments for computing shared covariate balance.
      # Using template defaults as no specific settings in Analysis Specifications.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default from template
        covariateFilter = NULL # Default from template
      )
      # Arguments for computing covariate balance.
      # Using template defaults as no specific settings in Analysis Specifications.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default from template
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default from template
      )

      # Arguments for fitting the outcome model.
      # Populated from Analysis Specifications -> fitOutcomeModelArgs.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From Analysis Specifications -> fitOutcomeModelArgs.modelType
        stratified = TRUE, # From Analysis Specifications -> fitOutcomeModelArgs.stratified
        useCovariates = FALSE, # From Analysis Specifications -> fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE, # From Analysis Specifications -> fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior( # Prior settings for regularization
          priorType = "laplace", # From Analysis Specifications -> fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE # From Analysis Specifications -> fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings for Cyclops solver
          cvType = "auto", # From Analysis Specifications -> fitOutcomeModelArgs.control.cvType
          seed = 1, # Default from template
          resetCoefficients = TRUE, # From Analysis Specifications -> fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01, # From Analysis Specifications -> fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07, # From Analysis Specifications -> fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10, # From Analysis Specifications -> fitOutcomeModelArgs.control.cvRepetitions
          noiseLevel = "quiet" # From Analysis Specifications -> fitOutcomeModelArgs.control.noiseLevel
        )
      )

      # Arguments for creating the study population.
      # Populated from Analysis Specifications -> createStudyPopArgs and current time-at-risk.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications -> createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE, # From Analysis Specifications -> createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0, # From Analysis Specifications -> createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # From Analysis Specifications -> createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # From Analysis Specifications -> createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = FALSE, # From Analysis Specifications -> createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # From Analysis Specifications -> createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999 # Default from template, not in JSON
      )

      # Append the settings to Analysis List.
      # Each entry in cmAnalysisList represents a unique CohortMethod analysis.
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

# Create module specifications for CohortMethod.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Default from template
  refitPsForEveryOutcome = FALSE, # Default from template
  refitPsForEveryStudyPopulation = FALSE, # Default from template
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default from template
)

# Create the analysis specifications ------------------------------------------
# Combine all shared resources and module specifications into a single Strategus analysis specification.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path uses the study name from Analysis Specifications -> name.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ticagrelorclopidogrel", "ticagrelorclopigrelAnalysisSpecification.json")
)