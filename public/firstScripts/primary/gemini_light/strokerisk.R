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
library(ROhdsiWebApi) # Required for WebAPI calls to retrieve cohort definitions and concept sets
library(CohortMethod) # Required for CohortMethod package functions
library(FeatureExtraction) # Required for FeatureExtraction package functions (e.g., covariate settings)
library(Cyclops) # Required for Cyclops package functions (e.g., prior and control settings)
library(ParallelLogger) # Required for saving settings to JSON

# Shared Resources -------------------------------------------------------------
# Base URL for the OHDSI WebAPI (e.g., Atlas)
# This URL is used to retrieve cohort definitions and concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Extract cohort IDs from the analysis specifications for Target, Comparator, and Outcome.
targetCohortId <- 1794126
comparatorCohortId <- 1794132
outcomeCohortId <- 1794131

# Export cohort definitions from the WebAPI.
# 'generateStats = TRUE' ensures that cohort statistics are generated during cohort generation.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    targetCohortId,    # Target Cohort ID from analysis specifications
    comparatorCohortId, # Comparator Cohort ID from analysis specifications
    outcomeCohortId     # Outcome Cohort ID from analysis specifications
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal use within Strategus modules.
# This re-numbering provides consistent, small integer IDs for easier management.
# Target cohort will be assigned ID 1.
cohortDefinitionSet[cohortDefinitionSet$cohortId == targetCohortId,]$cohortId <- 1
# Comparator cohort will be assigned ID 2.
cohortDefinitionSet[cohortDefinitionSet$cohortId == comparatorCohortId,]$cohortId <- 2
# Outcome cohort will be assigned ID 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == outcomeCohortId,]$cohortId <- 3

# Negative control outcomes
# Extract the concept set ID for negative controls from the analysis specifications.
negativeControlConceptSetId <- 1888110

# Retrieve and process negative control outcome concepts from the WebAPI.
# These concepts are resolved to their constituent concepts and then formatted
# into a cohort set structure.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = negativeControlConceptSetId,
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
  # Assign unique cohort IDs starting from 101 to avoid collision with
  # Target (1), Comparator (2), and Outcome (3) cohort IDs.
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs between study cohorts and negative controls.
# This is a critical check to ensure unique identifiers across all cohorts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes:
# Filter for the outcome cohort (re-numbered to ID 3) and prepare it for analysis.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # A default clean window of 365 days is used, as not specified in input.
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Use the re-numbered target (1) and comparator (2) cohort IDs and their names.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = cohortDefinitionSet$cohortName[cohortDefinitionSet$cohortId == 1],
  comparatorCohortId = 2,
  comparatorCohortName = cohortDefinitionSet$cohortName[cohortDefinitionSet$cohortId == 2]
)

# For the CohortMethod LSPS, we'll need to exclude specific covariates.
# The analysis specifications indicate empty lists for conceptsToInclude and conceptsToExclude.
# Therefore, 'excludedCovariateConcepts' will be an empty data frame.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# The analysis specifications indicate an empty list for conceptsToInclude.
# includedCovariateConcepts <- data.frame(
#   conceptId = integer(0),
#   conceptName = character(0)
# )

# CohortGeneratorModule --------------------------------------------------------
# Initialize the CohortGeneratorModule settings creator.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for cohort definitions.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource specifications for negative control outcome cohorts.
# 'occurrenceType = "first"' means the first occurrence of the outcome is considered.
# 'detectOnDescendants = TRUE' means descendants of the outcome concepts are also detected.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications for the CohortGeneratorModule.
# 'generateStats = TRUE' ensures that cohort generation statistics are computed.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# Initialize the CohortDiagnosticsModule settings creator.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Create module specifications for the CohortDiagnosticsModule.
# The 'cohortIds' parameter includes all study cohorts (T, C, O) for diagnostics.
# Various diagnostic reports are enabled based on common study practices.
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE,
  runIncludedSourceConcepts = TRUE,
  runOrphanConcepts = TRUE,
  runTimeSeries = FALSE, # Not typically run for every study, set to FALSE
  runVisitContext = TRUE,
  runBreakdownIndexEvents = TRUE,
  runIncidenceRate = TRUE,
  runCohortRelationship = TRUE,
  runTemporalCohortCharacterization = TRUE,
  minCharacterizationMean = 0.01 # Minimum mean for characterization covariates to be included
)

# CohortMethodModule -----------------------------------------------------------

# Study periods from analysis specifications.
# These define the overall date range for data extraction.
studyPeriods <- tibble(
  studyStartDate = c("20010101"), # YYYYMMDD
  studyEndDate   = c("20171231")  # YYYYMMDD
)

# Time-at-risks (TARs) for the outcomes of interest in your study from analysis specifications.
# This defines the risk window relative to the cohort start/end.
timeAtRisks <- tibble(
  label = c("Default TAR"), # A descriptive label for this TAR
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"), # Anchor for the start of the risk window
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end") # Anchor for the end of the risk window
)

# Propensity Score settings - match on PS from analysis specifications.
# This tibble defines parameters for propensity score matching.
matchOnPsArgsList <- tibble(
  label = c("Match on PS"), # A descriptive label for this PS matching setting
  maxRatio  = c(10), # Maximum number of comparators to match to each target
  caliper = c(0.2), # Caliper for matching
  caliperScale  = c("standardized logit") # Scale of the caliper
)

# Propensity Score settings - stratify by PS.
# This is empty as 'stratifyByPsArgs' was null in the analysis specifications.
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0) # "all" | "target" | "comparator"
)

# Build a single PS configuration list (each entry has: method, label, params)
# This loop processes the defined PS matching and stratification settings into a unified list.
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
# This block will not execute as stratifyByPsArgsList is empty based on the input.
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
# This nested loop creates a CohortMethod analysis for each combination of
# study period, time-at-risk, and propensity score adjustment method.
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Configure PS adjustment arguments based on the method (match or stratify).
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

      # Define covariate settings.
      # 'addDescendantsToExclude = TRUE' is a default from the template.
      # Since 'covariateSelection.conceptsToInclude' and 'conceptsToExclude'
      # are empty in the analysis specifications, no specific concept IDs are passed here.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Prepare the list of outcomes, including both study outcomes and negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # True effect size is unknown for study outcomes
            priorOutcomeLookback = 99999 # From analysis specifications
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # True effect size for negative controls is assumed to be 1 (no effect)
          )
        })
      )

      # Create target-comparator-outcomes list for each T/C pair.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # 'excludedCovariateConceptIds' is set to an empty vector because
          # 'covariateSelection.conceptsToExclude' in the analysis specifications is empty,
          # and no specific target/comparator drug concepts were provided for exclusion.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Arguments for retrieving cohort method data from the database.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Default from template
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From analysis specifications
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From analysis specifications
        errorOnHighCorrelation = TRUE, # From analysis specifications
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att", # Default from template
        prior = Cyclops::createPrior( # Prior settings for the PS model
          priorType = "laplace", # From analysis specifications
          exclude = c(0), # Default from template
          useCrossValidation = TRUE # From analysis specifications
        ),
        control = Cyclops::createControl( # Control settings for the PS model
          noiseLevel = "silent", # From analysis specifications
          cvType = "auto", # From analysis specifications
          seed = 1, # Default from template
          resetCoefficients = TRUE, # From analysis specifications
          tolerance = 2e-07, # From analysis specifications
          cvRepetitions = 10, # From analysis specifications (template had 1, spec has 10)
          startingVariance = 0.01 # From analysis specifications
        )
      )

      # Arguments for computing shared covariate balance.
      # These are default settings from the template, as not specified in input.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      # Arguments for computing covariate balance.
      # These are default settings from the template, as not specified in input.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting the outcome model.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From analysis specifications
        stratified = TRUE, # From analysis specifications
        useCovariates = FALSE, # From analysis specifications
        inversePtWeighting = FALSE, # From analysis specifications
        prior = Cyclops::createPrior( # Prior settings for the outcome model
          priorType = "laplace", # From analysis specifications
          useCrossValidation = TRUE # From analysis specifications
        ),
        control = Cyclops::createControl( # Control settings for the outcome model
          cvType = "auto", # From analysis specifications
          seed = 1, # Default from template
          resetCoefficients = TRUE, # From analysis specifications
          startingVariance = 0.01, # From analysis specifications
          tolerance = 2e-07, # From analysis specifications
          cvRepetitions = 10, # From analysis specifications (template had 1, spec has 10)
          noiseLevel = "quiet" # From analysis specifications
        )
      )

      # Arguments for creating the study population.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From analysis specifications
        firstExposureOnly = FALSE, # From analysis specifications
        washoutPeriod = 0, # From analysis specifications
        removeDuplicateSubjects = "keep all", # From analysis specifications (template had "keep first")
        censorAtNewRiskWindow = FALSE, # From analysis specifications (template had TRUE)
        removeSubjectsWithPriorOutcome = TRUE, # From analysis specifications
        priorOutcomeLookback = 99999, # From analysis specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1, # From analysis specifications
        maxDaysAtRisk = 99999 # Default, not specified in input
      )

      # Append the settings to Analysis List
      # Each entry in cmAnalysisList represents a complete CohortMethod analysis configuration.
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

# Initialize the CohortMethodModule settings creator.
cmModuleSettingsCreator <- CohortMethodModule$new()

# Create module specifications for the CohortMethodModule.
# This bundles all defined CM analyses and T/C/O pairs.
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # No specific analyses are excluded
  refitPsForEveryOutcome = FALSE, # Default from template
  refitPsForEveryStudyPopulation = FALSE, # Default from template
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default diagnostic thresholds
)

# Create the analysis specifications ------------------------------------------
# Combine all shared resources and module specifications into a single
# Strategus analysis specification object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path is constructed using the study name from the analysis specifications.
studyName <- "strokerisk" # From analysis specifications
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", studyName, paste0(studyName, "AnalysisSpecification.json"))
)