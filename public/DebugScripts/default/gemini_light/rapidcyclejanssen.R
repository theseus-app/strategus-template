# Load necessary libraries
# Strategus for creating analysis specifications
library(Strategus)
# dplyr for data manipulation (e.g., tibble, mutate, filter)
library(dplyr)
# ROhdsiWebApi for interacting with Atlas/WebAPI to retrieve cohort definitions and concept sets
library(ROhdsiWebApi)
# CohortMethod for population-level effect estimation module functions
library(CohortMethod)
# FeatureExtraction for covariate extraction settings
library(FeatureExtraction)
# Cyclops for prior and control settings in statistical models (used by CohortMethod)
library(Cyclops)
# tibble for creating structured data frames easily
library(tibble)

# Base URL for OHDSI WebAPI
# This URL is used to retrieve cohort definitions and concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# The analysis specifications define the target, comparator, and outcome cohorts by their Atlas IDs.
# These cohorts are retrieved from the specified WebAPI instance.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target Cohort ID from Analysis Specifications
    1794132, # Comparator Cohort ID from Analysis Specifications
    1794131  # Outcome Cohort ID from Analysis Specifications
  ),
  generateStats = TRUE # Request generation of cohort statistics
)

# Re-numbering cohorts to internal study IDs for consistency and simplicity.
# Target (original ID 1794126) is re-numbered to 1
# Comparator (original ID 1794132) is re-numbered to 2
# Outcome (original ID 1794131) is re-numbered to 3
cohortDefinitionSet <- cohortDefinitionSet %>%
  mutate(
    cohortId = case_when(
      cohortId == 1794126 ~ 1,
      cohortId == 1794132 ~ 2,
      cohortId == 1794131 ~ 3,
      TRUE ~ cohortId # Keep other cohort IDs as is if any (though not expected here)
    )
  )

# Negative Control Outcomes ----------------------------------------------------
# The analysis specifications include a concept set for negative control outcomes.
# These are used to calibrate effect estimates and detect systematic bias.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Negative Control Concept Set ID from Analysis Specifications
  baseUrl = baseUrl
) %>%
  # FIX: Removed 'vocabularyIds' argument, which caused the "unused argument" error.
  # The ROhdsiWebApi::resolveConceptSet function, as used by Strategus, does not
  # expect this argument or it has been deprecated.
  ROhdsiWebApi::resolveConceptSet(
    baseUrl = baseUrl
  ) %>%
  ROhdsiWebApi::getConcepts(
    baseUrl = baseUrl
  ) %>%
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  # Assign unique cohort IDs to negative controls, starting from 101 to avoid
  # collision with target/comparator/outcome (which are 1, 2, 3).
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs to ensure all cohorts (target, comparator, outcome, negative controls)
# have unique identifiers before proceeding.
# Retained correct usage of 'c()' for duplicated function from Original Script.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found in cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# Create data frames to hold specific cohort lists used in the analysis ---------

# Outcomes: Filter for the main outcome cohort using its re-numbered ID (3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Re-numbered outcome cohort ID
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window for outcomes

# Target and Comparator list for the CohortMethod analysis.
# Uses the re-numbered target (1) and comparator (2) cohort IDs and names
# as specified in the Analysis Specifications.
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered target cohort ID
  targetCohortName = "target1", # Target cohort name from Analysis Specifications
  comparatorCohortId = 2, # Re-numbered comparator cohort ID
  comparatorCohortName = "comparator1" # Comparator cohort name from Analysis Specifications
)

# Covariate Selection for CohortMethod.
# The analysis specifications define empty lists for `conceptsToInclude` and `conceptsToExclude`.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)
includedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# CohortGeneratorModule Settings -----------------------------------------------
# This module is responsible for generating cohorts in the CDM and computing basic statistics.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource for cohort definitions, includes all cohorts defined in cohortDefinitionSet.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Shared resource for negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Detect on the first occurrence of the negative control
  detectOnDescendants = TRUE # Include descendants of the concepts in the concept set definition
)

# Module specifications for CohortGenerator, enabling statistics generation.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Generate cohort inclusion rules statistics, etc.
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# This module performs a series of diagnostics on the generated cohorts.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # Run diagnostics for all defined cohorts
  runInclusionStatistics = TRUE,
  runIncludedSourceConcepts = TRUE,
  runOrphanConcepts = TRUE,
  runTimeSeries = FALSE,
  runVisitContext = TRUE,
  runBreakdownIndexEvents = TRUE,
  runIncidenceRate = TRUE,
  runCohortRelationship = TRUE,
  runTemporalCohortCharacterization = TRUE,
  minCharacterizationMean = 0.01 # Minimum mean frequency for characterization covariates
)

# CohortMethodModule Settings --------------------------------------------------
# This module performs population-level effect estimation using the CohortMethod package.

# Study Periods: Defines the date ranges for restricting the study population.
studyPeriods <- tibble(
  studyStartDate = c("20210101"),
  studyEndDate   = c("") # Empty string indicates no end date restriction
)

# Time-at-Risks (TARs): Defines the risk windows for outcome ascertainment.
timeAtRisks <- tibble(
  label = c("TAR1_14_start", "TAR1_28_start", "TAR1_42_start", "TAR1_90_start", "TAR0_2_start"),
  riskWindowStart  = c(1, 1, 1, 1, 0),
  startAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd  = c(14, 28, 42, 90, 2),
  endAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start")
)

# Propensity Score (PS) settings - Matching on PS
matchOnPsArgsList <- tibble(
  label = c("PSMatch_Ratio100_Caliper0.2StdLogit"),
  maxRatio  = c(100),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# Propensity Score settings - Stratify by PS
# The analysis specifications indicate `stratifyByPsArgs: null`, meaning no stratification
# by propensity score is performed in this analysis. Thus, this list is empty.
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0)
)

# Build a single PS configuration list combining matching and stratification settings.
psConfigList <- list()

# Add match on PS configurations from matchOnPsArgsList
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "match", # Specify matching method
      label  = matchOnPsArgsList$label[i], # Human-readable label for the configuration
      params = list( # Parameters for createMatchOnPsArgs
        maxRatio     = matchOnPsArgsList$maxRatio[i],
        caliper      = matchOnPsArgsList$caliper[i],
        caliperScale = matchOnPsArgsList$caliperScale[i]
      )
    )
  }
}

# Add stratify by PS configurations (this block will be skipped as stratifyByPsArgsList is empty)
if (exists("stratifyByPsArgsList") && nrow(stratifyByPsArgsList) > 0) {
  for (i in seq_len(nrow(stratifyByPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "stratify", # Specify stratification method
      label  = stratifyByPsArgsList$label[i], # Human-readable label
      params = list( # Parameters for createStratifyByPsArgs
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection  = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}

# Iterate through all analysis setting combinations (study periods, time-at-risks, PS methods)
# to create individual CohortMethod analysis specifications.
cmAnalysisList <- list()
analysisId <- 1 # Initialize a counter for unique analysis IDs

for (s in seq_len(nrow(studyPeriods))) {
  currentStudyStartDate <- studyPeriods$studyStartDate[s]
  currentStudyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Determine the Propensity Score adjustment method (matching or stratification)
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          # Retained correct syntax from Original Script (template had extra parenthesis)
          caliper = psCfg$params$caliper, 
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings for feature extraction.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE, # Include descendants when excluding concepts
        excludedCovariateConceptIds = excludedCovariateConcepts$conceptId, # Concepts to exclude from FeatureExtraction (empty here)
        includedCovariateConceptIds = includedCovariateConcepts$conceptId # Concepts to specifically include (empty here)
      )
      
      # Prepare a list of outcomes for the CohortMethod module.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )
      
      # Define target-comparator-outcome (TCO) pairs for the analysis.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude the target and comparator cohort IDs from covariate analysis.
          # The Original Script correctly uses targetCohortId and comparatorCohortId.
          excludedCovariateConceptIds = c(
            cmTcList$targetCohortId[i], # Exclude the target exposure cohort ID
            cmTcList$comparatorCohortId[i], # Exclude the comparator exposure cohort ID
            excludedCovariateConcepts$conceptId # Any additional concepts to exclude (empty here)
          )
        )
      }

      # Arguments for retrieving data for CohortMethod (from the CDM).
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = currentStudyStartDate,
        studyEndDate = currentStudyEndDate,
        restrictToCommonPeriod = TRUE,
        maxCohortSize = 0,
        covariateSettings = covariateSettings,
        firstExposureOnly = TRUE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "remove all"
      )

      # Arguments for creating propensity scores (PS).
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
          # Retained 10 from original script, as it's indicated to be from Analysis Specifications.
          cvRepetitions = 10, 
          startingVariance = 0.01
        )
      )

      # Arguments for computing covariate balance BEFORE matching/stratification (shared covariates).
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      
      # Arguments for computing covariate balance AFTER matching/stratification.
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting the outcome model.
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
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
          # Retained 10 from original script, as it's indicated to be from Analysis Specifications.
          cvRepetitions = 10,
          noiseLevel = "quiet"
        )
      )
      
      # Arguments for creating the study population from the cohort data.
      # Original script correctly included the comma after restrictToCommonPeriod.
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
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Append the current combination of settings to the CohortMethod analysis list.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          currentStudyStartDate,
          currentStudyEndDate,
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
      analysisId <- analysisId + 1 # Increment analysis ID for the next combination
    }
  }
}

# CohortMethodModule Specifications
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the final analysis specifications object ------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path uses the specific study name "rapidcyclejanssen" as in the original script.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "rapidcyclejanssen", "rapidcyclejanssenAnalysisSpecification.json")
)