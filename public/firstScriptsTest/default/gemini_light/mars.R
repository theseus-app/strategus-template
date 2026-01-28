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
library(tibble) # For tibble creation

# Shared Resources -------------------------------------------------------------
# Base URL for the WebAPI instance to retrieve cohort definitions and concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Retrieve cohort definitions from WebAPI based on IDs provided in Analysis Specifications.
# The IDs are re-numbered locally for internal Strategus use (1 for target, 2 for comparator, 3 for outcome).
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to standard IDs for Strategus modules.
# Target cohort (ID 1794126) is mapped to local ID 1.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# Comparator cohort (ID 1794132) is mapped to local ID 2.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# Outcome cohort (ID 1794131) is mapped to local ID 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve the concept set definition for negative controls from WebAPI.
# The concept set ID is 1888110 ("negative") from Analysis Specifications.
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
  # Assign unique cohort IDs for negative controls, starting from 101 to avoid conflicts
  # with target/comparator/outcome cohorts (1, 2, 3).
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs between study cohorts and negative controls.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Filter for the outcome cohort (local ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not specified in analysis specs.

# Target and Comparator for the CohortMethod analysis
# Use the re-numbered local IDs for target (1) and comparator (2).
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = cohortDefinitionSet[cohortDefinitionSet$cohortId == 1,]$cohortName,
  comparatorCohortId = 2,
  comparatorCohortName = cohortDefinitionSet[cohortDefinitionSet$cohortId == 2,]$cohortName
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study. Based on Analysis Specifications, `conceptsToExclude` is empty.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# Based on Analysis Specifications, `conceptsToInclude` is empty.
includedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Shared resource for study cohorts (target, comparator, outcome).
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Shared resource for negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default, not specified in analysis specs.
  detectOnDescendants = TRUE # Default, not specified in analysis specs.
)
# Module specifications for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Generate cohort statistics.
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Module specifications for CohortDiagnostics.
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # Run diagnostics for all defined cohorts.
  runInclusionStatistics = TRUE,
  runIncludedSourceConcepts = TRUE,
  runOrphanConcepts = TRUE,
  runTimeSeries = FALSE, # Default, not specified in analysis specs.
  runVisitContext = TRUE,
  runBreakdownIndexEvents = TRUE,
  runIncidenceRate = TRUE,
  runCohortRelationship = TRUE,
  runTemporalCohortCharacterization = TRUE,
  minCharacterizationMean = 0.01 # Default, not specified in analysis specs.
)

# CohortMethodModule -----------------------------------------------------------

# Study periods from Analysis Specifications -> getDbCohortMethodDataArgs -> studyPeriods.
studyPeriods <- tibble(
  studyStartDate = c("20110101"), # YYYYMMDD
  studyEndDate   = c("20131231")  # YYYYMMDD
)

# Time-at-risks (TARs) for the outcomes of interest in your study
# From Analysis Specifications -> createStudyPopArgs -> timeAtRisks.
timeAtRisks <- tibble(
  label = c("TAR_3_90"), # A descriptive label for this TAR.
  riskWindowStart  = c(3),
  startAnchor = c("cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd  = c(90),
  endAnchor = c("cohort start") # "cohort start" | "cohort end"
)

# Propensity Score settings - match on PS
# From Analysis Specifications -> propensityScoreAdjustment -> psSettings -> matchOnPsArgs.
matchOnPsArgsList <- tibble(
  label = c("Match_Caliper0.2_Ratio1"), # A descriptive label for this PS matching setting.
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # "propensity score" | "standardized" | "standardized logit"
)

# Propensity Score settings - stratify by PS
# From Analysis Specifications, stratifyByPsArgs is null, so this list will be empty.
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
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
          allowReverseMatch = FALSE, # Default, not specified in analysis specs.
          stratificationColumns = c() # Default, not specified in analysis specs.
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default, not specified in analysis specs.
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings for feature extraction.
      # Analysis Specifications do not provide specific covariate settings beyond include/exclude concepts.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default, not specified in analysis specs.
      )

      # Combine study outcomes and negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE, # Study outcome is of interest.
            trueEffectSize = NA, # Not applicable for study outcomes.
            priorOutcomeLookback = 99999 # From Analysis Specifications -> createStudyPopArgs.
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE, # Negative controls are not of interest.
            trueEffectSize = 1 # Assumed true effect size of 1 for negative controls.
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
          # Exclude covariates based on `excludedCovariateConcepts` from Analysis Specifications.
          # Note: The template included target/comparator concept IDs here, but the analysis spec
          # only provides `conceptsToExclude` which is empty.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Arguments for retrieving cohort method data.
      # Settings from Analysis Specifications -> getDbCohortMethodDataArgs.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # From Analysis Specifications.
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From Analysis Specifications.
        firstExposureOnly = FALSE, # From Analysis Specifications.
        washoutPeriod = 0, # From Analysis Specifications.
        removeDuplicateSubjects = "keep all", # From Analysis Specifications.
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # Settings from Analysis Specifications -> propensityScoreAdjustment -> createPsArgs.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications.
        errorOnHighCorrelation = TRUE, # From Analysis Specifications.
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att", # Default, not specified in analysis specs.
        prior = Cyclops::createPrior( # Prior settings from Analysis Specifications -> prior.
          priorType = "laplace",
          exclude = c(0), # Default, not specified in analysis specs.
          useCrossValidation = TRUE # From Analysis Specifications.
        ),
        control = Cyclops::createControl( # Control settings from Analysis Specifications -> control.
          noiseLevel = "silent", # From Analysis Specifications.
          cvType = "auto", # From Analysis Specifications.
          seed = 1, # Default, not specified in analysis specs.
          resetCoefficients = TRUE, # From Analysis Specifications.
          tolerance = 2e-07, # From Analysis Specifications.
          cvRepetitions = 10, # From Analysis Specifications.
          startingVariance = 0.01 # From Analysis Specifications.
        )
      )

      # Arguments for computing shared covariate balance.
      # Not explicitly specified in Analysis Specifications, using template defaults.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      # Arguments for computing covariate balance.
      # Not explicitly specified in Analysis Specifications, using template defaults.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting the outcome model.
      # Settings from Analysis Specifications -> fitOutcomeModelArgs.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From Analysis Specifications.
        stratified = FALSE, # From Analysis Specifications.
        useCovariates = FALSE, # From Analysis Specifications.
        inversePtWeighting = FALSE, # From Analysis Specifications.
        prior = Cyclops::createPrior( # Prior settings from Analysis Specifications -> prior.
          priorType = "laplace",
          useCrossValidation = TRUE # From Analysis Specifications.
        ),
        control = Cyclops::createControl( # Control settings from Analysis Specifications -> control.
          cvType = "auto", # From Analysis Specifications.
          seed = 1, # Default, not specified in analysis specs.
          resetCoefficients = TRUE, # From Analysis Specifications.
          startingVariance = 0.01, # From Analysis Specifications.
          tolerance = 2e-07, # From Analysis Specifications.
          cvRepetitions = 10, # From Analysis Specifications.
          noiseLevel = "quiet" # From Analysis Specifications.
        )
      )

      # Arguments for creating the study population.
      # Settings from Analysis Specifications -> createStudyPopArgs.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications.
        firstExposureOnly = FALSE, # From Analysis Specifications.
        washoutPeriod = 0, # From Analysis Specifications.
        removeDuplicateSubjects = "keep all", # From Analysis Specifications.
        censorAtNewRiskWindow = FALSE, # From Analysis Specifications.
        removeSubjectsWithPriorOutcome = TRUE, # From Analysis Specifications.
        priorOutcomeLookback = 99999, # From Analysis Specifications.
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1, # From Analysis Specifications.
        maxDaysAtRisk = 99999 # Default, not specified in analysis specs.
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
# Module specifications for CohortMethod.
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Default, not specified in analysis specs.
  refitPsForEveryOutcome = FALSE, # Default, not specified in analysis specs.
  refitPsForEveryStudyPopulation = FALSE, # Default, not specified in analysis specs.
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default thresholds.
)

# Create the analysis specifications ------------------------------------------
# Initialize empty analysis specifications.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Add shared resources for cohort definitions and negative controls.
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  # Add module specifications for CohortGenerator, CohortDiagnostics, and CohortMethod.
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path uses the study name "mars" from Analysis Specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "mars", "marsAnalysisSpecification.json")
)