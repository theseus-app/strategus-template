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
library(CohortGenerator)
library(CohortDiagnostics)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(ParallelLogger)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
# Base URL for the WebAPI instance where cohort definitions are stored.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions from WebAPI based on provided IDs.
# These IDs correspond to target, comparator, and outcome cohorts from Analysis Specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to a simplified scheme (1, 2, 3) for internal use in the study.
# This makes it easier to refer to target, comparator, and outcome consistently.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve the concept set definition for negative controls from WebAPI.
# The concept set ID is specified in Analysis Specifications.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # ID for the negative control concept set
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
  # Assign unique cohort IDs for negative controls, starting from 101 to avoid
  # collision with target/comparator/outcome (1, 2, 3).
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs between study cohorts and negative controls.
# This is a critical check to ensure unique identifiers across all cohorts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes:
# Filter for the primary outcome cohort (re-numbered to ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # Default cleanWindow, not specified in Analysis Specifications.
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Use the re-numbered target (ID 1) and comparator (ID 2) cohorts.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study. Based on Analysis Specifications, `covariateSelection.conceptsToExclude`
# is empty, so this data frame will be empty.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all
# Based on Analysis Specifications, `covariateSelection.conceptsToInclude` is empty.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# Initialize CohortGeneratorModule settings creator.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for cohort definitions.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource specifications for negative control outcome cohorts.
# `occurrenceType` and `detectOnDescendants` are default settings.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications for CohortGenerator.
# `generateStats` is set to TRUE as per template.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# Initialize CohortDiagnosticsModule settings creator.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Create module specifications for CohortDiagnostics.
# `cohortIds` includes all study cohorts (target, comparator, outcome).
# Various diagnostic options are enabled as per template defaults.
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE,
  runIncludedSourceConcepts = TRUE,
  runOrphanConcepts = TRUE,
  runTimeSeries = FALSE, # Set to FALSE as per template default
  runVisitContext = TRUE,
  runBreakdownIndexEvents = TRUE,
  runIncidenceRate = TRUE,
  runCohortRelationship = TRUE,
  runTemporalCohortCharacterization = TRUE,
  minCharacterizationMean = 0.01
)

# CohortMethodModule -----------------------------------------------------------

# Study periods for restricting the analysis.
# Based on Analysis Specifications, `studyStartDate` and `studyEndDate` are empty strings.
# This means no specific date restriction is applied, and the analysis will run
# for the entire available data period.
studyPeriods <- tibble(
  studyStartDate = c(""), # YYYYMMDD
  studyEndDate   = c("")  # YYYYMMDD
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# These are extracted directly from `createStudyPopArgs.timeAtRisks` in Analysis Specifications.
timeAtRisks <- tibble(
  label = c("TAR1_1_0_cohort_end", "TAR2_1_99999_cohort_start"), # Descriptive labels for each TAR
  riskWindowStart  = c(1, 1),
  startAnchor = c("cohort start", "cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd  = c(0, 99999),
  endAnchor = c("cohort end", "cohort start"), # "cohort start" | "cohort end"
  minDaysAtRisk = c(1, 1)
)

# Propensity Score settings - match on PS
# Extracted from `propensityScoreAdjustment.psSettings.matchOnPsArgs` in Analysis Specifications.
matchOnPsArgsList <- tibble(
  label = c("MatchOnPs_Caliper0.2"), # Descriptive label for this PS matching setting
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # "propensity score" | "standardized" | "standardized logit"
)

# Propensity Score settings - stratify by PS
# Based on Analysis Specifications, `propensityScoreAdjustment.psSettings.stratifyByPsArgs` is null,
# so this tibble will be empty.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c() # "all" | "target" | "comparator"
)

# Build a single PS configuration list (each entry has: method, label, params)
# This list will contain all PS adjustment methods (matching, stratification) to be tested.
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
# This block will not execute as `stratifyByPsArgsList` is empty based on Analysis Specifications.
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
# This loop generates a list of CohortMethod analyses, each representing a unique
# combination of study period, time-at-risk, and propensity score adjustment method.
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
          allowReverseMatch = FALSE, # Default
          stratificationColumns = c() # Default
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings for FeatureExtraction.
      # Based on Analysis Specifications, `covariateSelection.conceptsToInclude`
      # and `conceptsToExclude` are empty, so default settings are used.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default
      )

      # List of outcomes for the analysis, including primary outcomes and negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not applicable for primary outcomes
            priorOutcomeLookback = 99999 # Default
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

      # Target-comparator-outcomes list.
      # This defines which target-comparator pairs are analyzed for which outcomes.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # `excludedCovariateConceptIds` is empty as per Analysis Specifications
          # `covariateSelection.conceptsToExclude` being empty.
          excludedCovariateConceptIds = c()
        )
      }

      # Arguments for fetching cohort method data from the database.
      # Values are extracted from `getDbCohortMethodDataArgs` in Analysis Specifications.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # From Analysis Specifications
        studyStartDate = studyStartDate, # From loop variable (empty string)
        studyEndDate = studyEndDate,     # From loop variable (empty string)
        maxCohortSize = 0,               # From Analysis Specifications
        covariateSettings = covariateSettings,
        firstExposureOnly = FALSE,       # From Analysis Specifications
        washoutPeriod = 0,               # From Analysis Specifications
        removeDuplicateSubjects = "keep all" # From Analysis Specifications
      )

      # Arguments for creating propensity scores.
      # Values are extracted from `propensityScoreAdjustment.createPsArgs` in Analysis Specifications.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications
        errorOnHighCorrelation = TRUE,    # From Analysis Specifications
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail (template default)
        estimator = "att",   # Template default
        prior = Cyclops::createPrior( # Prior settings for regularization, from Analysis Specifications
          priorType = "laplace",
          exclude = c(0), # Default
          useCrossValidation = TRUE # From Analysis Specifications
        ),
        control = Cyclops::createControl( # Control settings for Cyclops, from Analysis Specifications
          noiseLevel = "silent", # From Analysis Specifications
          cvType = "auto",       # From Analysis Specifications
          seed = 1,              # Template default
          resetCoefficients = TRUE, # From Analysis Specifications
          tolerance = 2e-07,     # From Analysis Specifications
          cvRepetitions = 10,    # From Analysis Specifications
          startingVariance = 0.01, # From Analysis Specifications
          fold = 10              # From Analysis Specifications
        )
      )

      # Arguments for computing shared covariate balance (e.g., for equipoise diagnostics).
      # Not specified in Analysis Specifications, using template defaults.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      # Arguments for computing covariate balance after PS adjustment.
      # Not specified in Analysis Specifications, using template defaults.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting the outcome model.
      # Values are extracted from `fitOutcomeModelArgs` in Analysis Specifications.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",               # From Analysis Specifications
        stratified = FALSE,              # From Analysis Specifications
        useCovariates = FALSE,           # From Analysis Specifications
        inversePtWeighting = FALSE,      # From Analysis Specifications
        prior = Cyclops::createPrior(    # Prior settings for regularization, from Analysis Specifications
          priorType = "laplace",
          useCrossValidation = TRUE      # From Analysis Specifications
        ),
        control = Cyclops::createControl( # Control settings for Cyclops, from Analysis Specifications
          cvType = "auto",               # From Analysis Specifications
          seed = 1,                      # Template default
          resetCoefficients = TRUE,      # From Analysis Specifications
          startingVariance = 0.01,       # From Analysis Specifications
          tolerance = 2e-07,             # From Analysis Specifications
          cvRepetitions = 10,            # From Analysis Specifications
          noiseLevel = "quiet",          # From Analysis Specifications
          fold = 10                      # From Analysis Specifications
        )
      )

      # Arguments for creating the study population.
      # Values are extracted from `createStudyPopArgs` in Analysis Specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,  # From Analysis Specifications
        firstExposureOnly = FALSE,       # From Analysis Specifications
        washoutPeriod = 0,               # From Analysis Specifications
        removeDuplicateSubjects = "keep all", # From Analysis Specifications
        censorAtNewRiskWindow = FALSE,   # From Analysis Specifications
        removeSubjectsWithPriorOutcome = TRUE, # From Analysis Specifications
        priorOutcomeLookback = 99999,    # From Analysis Specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From loop variable
        startAnchor = timeAtRisks$startAnchor[t],         # From loop variable
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],     # From loop variable
        endAnchor = timeAtRisks$endAnchor[t],             # From loop variable
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],     # From loop variable
        maxDaysAtRisk = 99999            # Default
      )

      # Append the settings to Analysis List
      # Each entry in cmAnalysisList represents a complete CohortMethod analysis.
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

# Initialize CohortMethodModule settings creator.
cmModuleSettingsCreator <- CohortMethodModule$new()

# Create module specifications for CohortMethod.
# `cmAnalysisList` contains all defined analyses.
# `targetComparatorOutcomesList` defines the T/C/O combinations.
# Other parameters are template defaults.
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Default
  refitPsForEveryOutcome = FALSE, # Default
  refitPsForEveryStudyPopulation = FALSE, # Default
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default
)

# Create the analysis specifications ------------------------------------------
# Assemble all shared resources and module specifications into a single
# Strategus analysis specification object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the complete analysis specifications to a JSON file.
# The file path is constructed using the study name "antivegfkidney".
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "antivegfkidney", "antivegfkidneyAnalysisSpecification.json")
)