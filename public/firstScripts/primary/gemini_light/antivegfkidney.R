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
library(Cyclops) # Cyclops is a dependency for CohortMethod for priors and controls

# Shared Resources -------------------------------------------------------------
# Base URL for the WebAPI where cohort definitions are stored.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export target, comparator, and outcome cohort definitions from WebAPI.
# IDs are taken directly from the "cohortDefinitions" section of Analysis Specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: antivegfkidney.cohortDefinitions.targetCohort.id
    1794132, # Comparator: antivegfkidney.cohortDefinitions.comparatorCohort.id
    1794131  # Outcome: antivegfkidney.cohortDefinitions.outcomeCohort[0].id
  ),
  generateStats = TRUE
)

# Re-number cohorts to match the internal numbering scheme (1, 2, 3...)
# This is a common practice in Strategus templates for consistent mapping.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Target cohort
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Comparator cohort
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Outcome cohort

# Negative control outcomes
# Retrieve negative control concept set definition from WebAPI and resolve concepts.
# The ID is taken from the "negativeControlConceptSet" section of Analysis Specifications.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # antivegfkidney.negativeControlConceptSet.id
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
  mutate(cohortId = row_number() + 100) %>% # Assign unique IDs starting from 101 for negative controls to avoid overlap
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs to prevent conflicts during analysis execution.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts we'll use in each analysis -----------
# Outcomes: Filter for the main outcome cohort (ID 3) and prepare for use.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # A default clean window; often used in outcome definitions.

# Target and Comparator for the CohortMethod analysis.
# Uses the re-numbered cohort IDs and their corresponding names.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = cohortDefinitionSet[cohortDefinitionSet$cohortId == 1,]$cohortName,
  comparatorCohortId = 2,
  comparatorCohortName = cohortDefinitionSet[cohortDefinitionSet$cohortId == 2,]$cohortName
)

# Covariate exclusion/inclusion lists.
# From "covariateSelection" in Analysis Specifications, both conceptsToInclude
# and conceptsToExclude are empty. Thus, these data frames will be empty.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0), # No specific concepts provided for exclusion in covariates
  conceptName = character(0)
)
# includedCovariateConcepts <- data.frame(
#   conceptId = integer(0), # No specific concepts provided for inclusion in covariates
#   conceptName = character(0)
# )


# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Define shared resources for cohort definitions, used by CohortGenerator and other modules.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Define shared resources for negative control outcomes.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Defaulting to first occurrence
  detectOnDescendants = TRUE # Defaulting to detect on descendants
)
# Create module specifications for CohortGenerator, enabling statistics generation.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Generate cohort statistics
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Create module specifications for CohortDiagnostics.
# Cohort IDs to run diagnostics on are all defined cohorts.
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE, # Run inclusion rule statistics
  runIncludedSourceConcepts = TRUE, # Run included source concept analysis
  runOrphanConcepts = TRUE, # Run orphan concept analysis
  runTimeSeries = FALSE, # Do not run time series analysis (as per template default)
  runVisitContext = TRUE, # Run visit context analysis
  runBreakdownIndexEvents = TRUE, # Run breakdown index events analysis
  runIncidenceRate = TRUE, # Run incidence rate analysis
  runCohortRelationship = TRUE, # Run cohort relationship analysis
  runTemporalCohortCharacterization = TRUE, # Run temporal cohort characterization
  minCharacterizationMean = 0.01 # Minimum mean for characterization features
)

# CohortMethodModule -----------------------------------------------------------

# Define study periods.
# The "studyPeriods" in Analysis Specifications is [{ "studyStartDate": null, "studyEndDate": null }].
# This means no specific date restriction. We handle this by creating a single
# row with empty strings for start/end dates, allowing the loop to run once for an unrestricted period.
studyPeriods <- tibble(
  studyStartDate = "", # If null in specs, no start date restriction
  studyEndDate = ""    # If null in specs, no end date restriction
)
# Annotation: This ensures that if study periods are not explicitly defined or are null
# in the specifications, the analysis will proceed without date restrictions.

# Time-at-risks (TARs) for the outcomes of interest in your study.
# Definitions are extracted from "createStudyPopArgs.timeAtRisks" in Analysis Specifications.
timeAtRisks <- tibble(
  label = c("main TAR"), # A descriptive label for the TAR
  riskWindowStart  = c(1),            # antivegfkidney.createStudyPopArgs.timeAtRisks[0].riskWindowStart
  startAnchor = c("cohort start"),   # antivegfkidney.createStudyPopArgs.timeAtRisks[0].startAnchor
  riskWindowEnd  = c(0),             # antivegfkidney.createStudyPopArgs.timeAtRisks[0].riskWindowEnd
  endAnchor = c("cohort end")        # antivegfkidney.createStudyPopArgs.timeAtRisks[0].endAnchor
)
# Annotation: This table defines the temporal aspects of the risk window for outcomes.
# The 'minDaysAtRisk' setting is applied later within createStudyPopArgs.

# Propensity Score settings - match on PS.
# Based on "propensityScoreAdjustment.psSettings.matchOnPsArgs" in Analysis Specifications.
matchOnPsArgsList <- tibble(
  label = c("match on PS"), # Descriptive label for this PS setting
  maxRatio  = c(1),                               # antivegfkidney.propensityScoreAdjustment.psSettings[0].matchOnPsArgs.maxRatio
  caliper = c(0.2),                               # antivegfkidney.propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliper
  caliperScale  = c("standardized logit")         # antivegfkidney.propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliperScale
)

# Propensity Score settings - stratify by PS.
# In the Analysis Specifications, "stratifyByPsArgs" is null, so this list remains empty.
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0)
)

# Build a single PS configuration list.
# This list will contain one entry for "match on PS" based on the specifications.
psConfigList <- list()

# If 'matchOnPsArgsList' has rows, convert each row to a PS configuration entry.
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
# Annotation: This block processes the 'matchOnPsArgs' settings from the specifications.

# If 'stratifyByPsArgsList' has rows, convert each row to a PS configuration entry.
# This block will not add any entries in this specific case, as 'stratifyByPsArgs' is null in specs.
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
# Annotation: This block processes 'stratifyByPsArgs' settings. As per specs, this remains empty.


# Iterate through all analysis setting combinations to create individual CmAnalyses.
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # PS adjustment arguments (match or stratify): Only one will be non-NULL based on psCfg$method.
      matchOnPsArgs <- NULL
      stratifyByPsArgs <- NULL
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default setting
          stratificationColumns = c() # No stratification columns specified
        )
      } else if (psCfg$method == "stratify") {
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # No stratification columns specified
          baseSelection = psCfg$params$baseSelection
        )
      }
      # Annotation: Dynamically creates either matching or stratification arguments for propensity scores.

      # Covariate settings: Using default settings as no specific concepts to include/exclude are provided
      # in "covariateSelection" of Analysis Specifications.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default behavior for feature extraction
      )
      # Annotation: 'covariateSelection.conceptsToInclude' and 'conceptsToExclude' are empty in specs,
      # so standard default covariate settings are applied.

      # Define outcomes for the analysis (main outcome + negative controls).
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # For true outcomes, true effect size is unknown
            priorOutcomeLookback = 99999 # antivegfkidney.createStudyPopArgs.priorOutcomeLookBack is for the study population.
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # For negative controls, assumed true effect size of 1 for calibration
          )
        })
      )
      # Annotation: Combines the primary outcome (ID 3) with all resolved negative control outcomes.

      # Define Target-Comparator-Outcome combinations.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # excludedCovariateConceptIds: No specific concepts to exclude from covariates other than
          # what's handled by default covariate settings. "excludedCovariateConcepts" is empty from specs.
          excludedCovariateConceptIds = c(excludedCovariateConcepts$conceptId)
        )
      }
      # Annotation: Defines the T-C pair (cohort IDs 1 and 2) and associates them with all defined outcomes.
      # 'excludedCovariateConceptIds' is an empty vector here, as no additional concepts were specified for exclusion.

      # Arguments for fetching cohort method data from the database.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE, # antivegfkidney.createStudyPopArgs.restrictToCommonPeriod
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # antivegfkidney.getDbCohortMethodDataArgs.maxCohortSize (0 means no limit)
        covariateSettings = covariateSettings # Uses the covariate settings defined above
      )
      # Annotation: Configures how exposure cohorts and covariates are retrieved from the database.

      # Arguments for creating propensity scores.
      # Settings are taken from "propensityScoreAdjustment.createPsArgs" in Analysis Specifications.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # antivegfkidney.propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE,    # antivegfkidney.propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att", # Default: Average Treatment Effect on the Treated
        prior = Cyclops::createPrior( # antivegfkidney.propensityScoreAdjustment.createPsArgs.prior
          priorType = "laplace",
          exclude = c(0), # Exclude intercept from regularization
          useCrossValidation = TRUE # antivegfkidney.propensityScoreAdjustment.createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # antivegfkidney.propensityScoreAdjustment.createPsArgs.control
          noiseLevel = "silent",        # antivegfkidney.propensityScoreAdjustment.createPsArgs.control.noiseLevel
          cvType = "auto",              # antivegfkidney.propensityScoreAdjustment.createPsArgs.control.cvType
          seed = 1, # Fixed seed for reproducibility
          resetCoefficients = TRUE,     # antivegfkidney.propensityScoreAdjustment.createPsArgs.control.resetCoefficients
          tolerance = 2e-07,            # antivegfkidney.propensityScoreAdjustment.createPsArgs.control.tolerance
          cvRepetitions = 10,           # antivegfkidney.propensityScoreAdjustment.createPsArgs.control.cvRepetitions
          startingVariance = 0.01       # antivegfkidney.propensityScoreAdjustment.createPsArgs.control.startingVariance
        )
      )
      # Annotation: Sets up the parameters for fitting the propensity score model using Cyclops.

      # Arguments for computing covariate balance before and after PS adjustment.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default max cohort size for balance calculation
        covariateFilter = NULL # No specific filter for shared covariates balance
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default max cohort size for balance calculation
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Use standard Table 1 features
      )
      # Annotation: Configures how covariate balance is calculated for diagnostics.

      # Arguments for fitting the outcome model.
      # Settings are taken from "fitOutcomeModelArgs" in Analysis Specifications.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # antivegfkidney.fitOutcomeModelArgs.modelType (Cox proportional hazards)
        stratified = FALSE, # antivegfkidney.fitOutcomeModelArgs.stratified
        useCovariates = FALSE, # antivegfkidney.fitOutcomeModelArgs.useCovariates (not using covariates in outcome model after PS adjustment)
        inversePtWeighting = FALSE, # antivegfkidney.fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior( # antivegfkidney.fitOutcomeModelArgs.prior
          priorType = "laplace",
          useCrossValidation = TRUE # antivegfkidney.fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # antivegfkidney.fitOutcomeModelArgs.control
          cvType = "auto",              # antivegfkidney.fitOutcomeModelArgs.control.cvType
          seed = 1, # Fixed seed for reproducibility
          resetCoefficients = TRUE,     # antivegfkidney.fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01,      # antivegfkidney.fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07,            # antivegfkidney.fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10,           # antivegfkidney.fitOutcomeModelArgs.control.cvRepetitions
          noiseLevel = "quiet"          # antivegfkidney.fitOutcomeModelArgs.control.noiseLevel
        )
      )
      # Annotation: Sets up parameters for the outcome model using Cyclops.

      # Arguments for creating the study population.
      # Settings are taken from "createStudyPopArgs" in Analysis Specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,      # antivegfkidney.createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = TRUE,            # antivegfkidney.createStudyPopArgs.firstExposureOnly
        washoutPeriod = 365,                 # antivegfkidney.createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all",# antivegfkidney.createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE,       # antivegfkidney.createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # antivegfkidney.createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999,        # antivegfkidney.createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From current TAR setting
        startAnchor = timeAtRisks$startAnchor[t],     # From current TAR setting
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],   # From current TAR setting
        endAnchor = timeAtRisks$endAnchor[t],         # From current TAR setting
        minDaysAtRisk = 1, # antivegfkidney.createStudyPopArgs.timeAtRisks[0].minDaysAtRisk
        maxDaysAtRisk = 99999 # Default max days at risk if not specified
      )
      # Annotation: Configures the criteria for defining the study population and risk window for analysis.

      # Append the combined settings to the CohortMethod analysis list.
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
        matchOnPsArgs = matchOnPsArgs,       # Will be non-NULL for matching, NULL for stratification
        stratifyByPsArgs = stratifyByPsArgs, # Will be non-NULL for stratification, NULL for matching
        computeSharedCovariateBalanceArgs = computeSharedCovariateBalanceArgs,
        computeCovariateBalanceArgs = computeCovariateBalanceArgs,
        fitOutcomeModelArgs = fitOutcomeModelArgs
      )
      analysisId <- analysisId + 1
    }
  }
}
# Annotation: This nested loop generates a distinct CohortMethod analysis object for each
# combination of study period, time-at-risk, and propensity score adjustment method.

cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # No specific analyses to exclude
  refitPsForEveryOutcome = FALSE, # Default: fit PS once per T-C pair
  refitPsForEveryStudyPopulation = FALSE, # Default: fit PS once per T-C pair
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Uses default CM diagnostic thresholds
)
# Annotation: Consolidates all generated CohortMethod analyses and associated settings
# into the module specifications for the CohortMethod module.

# Create the analysis specifications ------------------------------------------
# Initialize an empty Strategus analysis specification and add all shared resources and module specifications.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)
# Annotation: The final Strategus analysis specification combines all defined components.

# Save the complete analysis specifications to a JSON file.
# The file path is structured for Strategus project organization, using the study name
# "antivegfkidney" from the Analysis Specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "antivegfkidney", "antivegfkidneyAnalysisSpecification.json")
)
# Annotation: The entire analysis specification is saved in JSON format, which can
# then be executed by the Strategus platform.