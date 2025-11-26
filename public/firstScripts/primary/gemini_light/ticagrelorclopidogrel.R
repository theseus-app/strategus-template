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
library(ROhdsiWebApi) # Required for fetching cohort definitions and concept sets
library(CohortMethod) # Required for CohortMethod specific arguments
library(FeatureExtraction) # Required for covariate settings
library(Cyclops) # Required for prior and control settings in Cyclops models

# Shared Resources -------------------------------------------------------------
# Define the base URL for the WebAPI to retrieve cohort definitions and concept sets.
# This should point to your ATLAS/WebAPI instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Fetch cohort definitions based on IDs specified in Analysis Specifications.
# - Target: 1794126 (target1)
# - Comparator: 1794132 (comparator1)
# - Outcome: 1794131 (outcome1)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to a standard scheme (1 for target, 2 for comparator, 3 for outcome).
# This simplifies referencing them within the study.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Renumber target1 to 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Renumber comparator1 to 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Renumber outcome1 to 3

# Negative control outcomes
# Fetch the concept set definition for negative controls as specified.
# - Negative Control Concept Set ID: 1888110 (negative)
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
  # Assign unique cohort IDs for negative controls, starting after the main cohorts (1, 2, 3).
  mutate(cohortId = row_number() + 100) %>% # target/comparator/outcome cohort IDs start with 1, 2, 3... negativeControl -> 101, 102, 103...
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs to prevent conflicts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts used in each analysis ---------------
# Outcomes: Filter for the primary outcome, which has been re-numbered to 3.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Filter for the re-numbered outcome cohort (id = 3)
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # Add a default cleanWindow (not specified in Analysis Specifications, using template default)
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Uses the re-numbered target (1) and comparator (2) cohort IDs and names.
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered target cohort ID
  targetCohortName = cohortDefinitionSet[cohortDefinitionSet$cohortId == 1,]$cohortName, # Get name for ID 1
  comparatorCohortId = 2, # Re-numbered comparator cohort ID
  comparatorCohortName = cohortDefinitionSet[cohortDefinitionSet$cohortId == 2,]$cohortName # Get name for ID 2
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study.
# From Analysis Specifications: covariateSelection.conceptsToExclude is empty/null,
# so create an empty data frame.
excludedCovariateConcepts <- data.frame(conceptId = numeric(0), conceptName = character(0))

# Optional: If you want to define covariates to include instead of including them all
# From Analysis Specifications: covariateSelection.conceptsToInclude is empty/null,
# so create an empty data frame.
includedCovariateConcepts <- data.frame(conceptId = numeric(0), conceptName = character(0))

# CohortGeneratorModule --------------------------------------------------------
# This module is used to generate the cohorts defined in the shared resources.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for cohort definitions.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource specifications for negative control outcomes.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Using default from template, not specified in Analysis Specifications
  detectOnDescendants = TRUE # Using default from template, not specified in Analysis Specifications
)

# Create module specifications for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Generate cohort statistics as specified in template
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# This module runs various diagnostics on the generated cohorts.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Create module specifications for CohortDiagnostics.
# Using default values from template as no specific settings are provided in Analysis Specifications.
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # Run diagnostics for all primary cohorts
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

# Define study periods for data collection.
# From Analysis Specifications: getDbCohortMethodDataArgs.studyPeriods
studyPeriods <- tibble(
  studyStartDate = c("20111101"), # YYYYMMDD
  studyEndDate   = c("20190331")  # YYYYMMDD
)

# Define time-at-risks (TARs) for the outcomes.
# From Analysis Specifications: createStudyPopArgs.timeAtRisks
timeAtRisks <- tibble(
  label = c("1-365 days from cohort start"), # Descriptive label for the TAR
  riskWindowStart  = c(1), # From Analysis Specifications: riskWindowStart
  startAnchor = c("cohort start"), # From Analysis Specifications: startAnchor
  riskWindowEnd  = c(365), # From Analysis Specifications: riskWindowEnd
  endAnchor = c("cohort start") # From Analysis Specifications: endAnchor
)

# Propensity Score settings - match on PS
# From Analysis Specifications: propensityScoreAdjustment.psSettings.matchOnPsArgs
# (only one setting provided in this spec)
matchOnPsArgsList <- tibble(
  label = c("Match 1:1, Caliper 0.2 SL"), # Descriptive label for PS matching
  maxRatio  = c(1), # From Analysis Specifications: maxRatio
  caliper = c(0.2), # From Analysis Specifications: caliper
  caliperScale  = c("standardized logit") # From Analysis Specifications: caliperScale
)

# Propensity Score settings - stratify by PS
# From Analysis Specifications: propensityScoreAdjustment.psSettings.stratifyByPsArgs is null,
# so create an empty tibble.
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = numeric(0),
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

# Iterate through all analysis setting combinations to create CohortMethod analysis specifications
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Define PS matching or stratification arguments based on the current PS configuration
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Template default
          stratificationColumns = c() # Template default
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Template default
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Define covariate settings.
      # Since `covariateSelection.conceptsToInclude` and `conceptsToExclude` are empty in spec,
      # use default covariate settings.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Template default
      )

      # Create a list of outcomes for this analysis, including primary outcomes and negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not applicable for real outcomes
            priorOutcomeLookback = 99999 # From Analysis Specifications: createStudyPopArgs.priorOutcomeLookBack
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # Expected effect size for negative controls
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
          # Exclude specific covariate concepts.
          # The template included `targetConceptId[i]` and `comparatorConceptId[i]`
          # but these are not present in cmTcList or Analysis Specifications.
          # Only using `excludedCovariateConcepts$conceptId` which is an empty vector here.
          excludedCovariateConceptIds = c(excludedCovariateConcepts$conceptId)
        )
      }

      # Define arguments for retrieving cohort method data from the database.
      # From Analysis Specifications: getDbCohortMethodDataArgs
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Restrict to common observation period for target/comparator within study dates
        studyStartDate = studyStartDate, # From current studyPeriods iteration
        studyEndDate = studyEndDate, # From current studyPeriods iteration
        maxCohortSize = 0, # From Analysis Specifications: maxCohortSize (0 means no restriction)
        covariateSettings = covariateSettings
      )

      # Define arguments for creating propensity scores.
      # From Analysis Specifications: propensityScoreAdjustment.createPsArgs
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications: maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From Analysis Specifications: errorOnHighCorrelation
        stopOnError = FALSE, # Template default: set to FALSE to allow Strategus to complete
        estimator = "att", # Template default
        prior = Cyclops::createPrior( # From Analysis Specifications: prior (laplace, useCrossValidation)
          priorType = "laplace",
          # exclude = c(0), # Not in Analysis Specifications, removed from template default
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl( # From Analysis Specifications: control (tolerance, cvType, fold, cvRepetitions, noiseLevel, resetCoefficients, startingVariance)
          noiseLevel = "silent", # From Analysis Specifications: noiseLevel
          cvType = "auto", # From Analysis Specifications: cvType
          # seed = 1, # Not in Analysis Specifications, removed from template default
          resetCoefficients = TRUE, # From Analysis Specifications: resetCoefficients
          tolerance = 2e-07, # From Analysis Specifications: tolerance
          cvRepetitions = 10, # From Analysis Specifications: cvRepetitions (template was 1, spec is 10)
          startingVariance = 0.01, # From Analysis Specifications: startingVariance
          fold = 10 # From Analysis Specifications: fold
        )
      )

      # Arguments for computing covariate balance for shared covariates.
      # Using template defaults, not specified in Analysis Specifications.
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      # Arguments for computing covariate balance.
      # Using template defaults, not specified in Analysis Specifications.
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Define arguments for fitting the outcome model.
      # From Analysis Specifications: fitOutcomeModelArgs
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From Analysis Specifications: modelType
        stratified = FALSE, # From Analysis Specifications: stratified
        useCovariates = FALSE, # From Analysis Specifications: useCovariates
        inversePtWeighting = FALSE, # From Analysis Specifications: inversePtWeighting
        prior = Cyclops::createPrior( # From Analysis Specifications: prior (laplace, useCrossValidation)
          priorType = "laplace",
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl( # From Analysis Specifications: control (tolerance, cvType, fold, cvRepetitions, noiseLevel, resetCoefficients, startingVariance)
          cvType = "auto", # From Analysis Specifications: cvType
          # seed = 1, # Not in Analysis Specifications, removed from template default
          resetCoefficients = TRUE, # From Analysis Specifications: resetCoefficients
          startingVariance = 0.01, # From Analysis Specifications: startingVariance
          tolerance = 2e-07, # From Analysis Specifications: tolerance
          cvRepetitions = 10, # From Analysis Specifications: cvRepetitions (template was 1, spec is 10)
          noiseLevel = "quiet", # From Analysis Specifications: noiseLevel
          fold = 10 # From Analysis Specifications: fold
        )
      )

      # Define arguments for creating the study population.
      # From Analysis Specifications: createStudyPopArgs and current timeAtRisks iteration.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications: restrictToCommonPeriod
        firstExposureOnly = FALSE, # From Analysis Specifications: firstExposureOnly
        washoutPeriod = 365, # From Analysis Specifications: washoutPeriod
        removeDuplicateSubjects = "keep all", # From Analysis Specifications: removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # From Analysis Specifications: censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # From Analysis Specifications: removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # From Analysis Specifications: priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From current timeAtRisks iteration
        startAnchor = timeAtRisks$startAnchor[t], # From current timeAtRisks iteration
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From current timeAtRisks iteration
        endAnchor = timeAtRisks$endAnchor[t], # From current timeAtRisks iteration
        minDaysAtRisk = 1, # From Analysis Specifications: minDaysAtRisk
        maxDaysAtRisk = 99999 # Template default, not in Analysis Specifications
      )

      # Append the combined settings to the Cohort Method Analysis List
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
        matchOnPsArgs = matchOnPsArgs, # Will be NULL if stratify method is used
        stratifyByPsArgs = stratifyByPsArgs, # Will be NULL if match method is used
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
  analysesToExclude = NULL, # Template default
  refitPsForEveryOutcome = FALSE, # Template default
  refitPsForEveryStudyPopulation = FALSE, # Template default
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Template default
)

# Create the overall analysis specifications ------------------------------------------
# Combine all shared resources and module specifications into a single analysis object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The filename uses the "name" from Analysis Specifications: "ticagrelorclopidogrel".
studyName <- "ticagrelorclopidogrel"
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", studyName, paste0(studyName, "AnalysisSpecification.json"))
)