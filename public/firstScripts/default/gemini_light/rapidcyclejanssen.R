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
# As per Analysis Specifications, this is not explicitly provided, so using the template default.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# The analysis specifications define the target, comparator, and outcome cohorts by their Atlas IDs.
# These cohorts are retrieved from the specified WebAPI instance.
#
# Analysis Specifications:
# - targetCohort:    id = 1794126, name = "target1"
# - comparatorCohort: id = 1794132, name = "comparator1"
# - outcomeCohort:   id = 1794131, name = "outcome1"

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
# This maps the original Atlas IDs to sequential integers (1, 2, 3...) for use within Strategus.
# - Target (original ID 1794126) is re-numbered to 1
# - Comparator (original ID 1794132) is re-numbered to 2
# - Outcome (original ID 1794131) is re-numbered to 3
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
# The concepts within the set are resolved and assigned unique cohort IDs.
#
# Analysis Specifications:
# - negativeControlConceptSet: id = 1888110, name = "negative"

negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Negative Control Concept Set ID from Analysis Specifications
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(
    baseUrl = baseUrl,
    # Standard vocabularies for resolving concepts, adjust if needed
    vocabularyIds = c("SNOMED", "RxNorm", "ATC", "ICD9CM", "ICD10CM", "MedDRA")
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
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found in cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# Create data frames to hold specific cohort lists used in the analysis ---------

# Outcomes: Filter for the main outcome cohort using its re-numbered ID (3).
# 'cleanWindow' is a default CohortMethod parameter, not explicitly in analysis specifications.
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
#
# Analysis Specifications:
# - covariateSelection.conceptsToInclude: [ { id: null, name: "" } ]
# - covariateSelection.conceptsToExclude: [ { id: null, name: "" } ]
#
# Thus, these data frames will be empty, resulting in no specific concepts being
# included or excluded beyond the default covariate settings.
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
# The analysis specifications do not provide specific settings for this module,
# so the template defaults are used.

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
#
# Analysis Specifications:
# - getDbCohortMethodDataArgs.studyPeriods: [ { studyStartDate: 20210101, studyEndDate: null } ]
# 'null' for studyEndDate is represented as an empty string in the tibble for CohortMethod.
studyPeriods <- tibble(
  studyStartDate = c("20210101"),
  studyEndDate   = c("") # Empty string indicates no end date restriction
)

# Time-at-Risks (TARs): Defines the risk windows for outcome ascertainment.
#
# Analysis Specifications:
# - createStudyPopArgs.timeAtRisks:
#   - Each entry defines a risk window with start, end, and anchor points.
timeAtRisks <- tibble(
  label = c("TAR1_14_start", "TAR1_28_start", "TAR1_42_start", "TAR1_90_start", "TAR0_2_start"),
  riskWindowStart  = c(1, 1, 1, 1, 0),
  startAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd  = c(14, 28, 42, 90, 2),
  endAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start")
)

# Propensity Score (PS) settings - Matching on PS
# The analysis specifications define one PS matching setting.
#
# Analysis Specifications:
# - propensityScoreAdjustment.psSettings:
#   - matchOnPsArgs: { maxRatio: 100, caliper: 0.2, caliperScale: "standardized logit" }
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
# In this case, only matching settings will be added.
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
      # and set the corresponding arguments, nulling out the other method's arguments.
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Do not allow reverse matching
          stratificationColumns = c() # No specific columns for stratification within matching
        )
        stratifyByPsArgs <- NULL # Stratification args are NULL if matching is used
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL # Matching args are NULL if stratification is used
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # No specific columns for stratification
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings for feature extraction.
      #
      # Analysis Specifications:
      # - covariateSelection.conceptsToInclude: [ { id: null, name: "" } ]
      # - covariateSelection.conceptsToExclude: [ { id: null, name: "" } ]
      # Given these are empty, no specific concepts are included or excluded from the
      # default covariate set.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE, # Include descendants when excluding concepts
        excludedCovariateConceptIds = excludedCovariateConcepts$conceptId, # Concepts to exclude from FeatureExtraction
        includedCovariateConceptIds = includedCovariateConcepts$conceptId # Concepts to specifically include in FeatureExtraction
      )
      
      # Prepare a list of outcomes for the CohortMethod module.
      # This combines the main outcome and the resolved negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE, # Mark as the primary outcome of interest
            trueEffectSize = NA, # Not a negative control, true effect size unknown
            priorOutcomeLookback = 99999 # Look back indefinitely for prior occurrences of this outcome
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE, # Mark as a negative control outcome
            trueEffectSize = 1 # True effect size for negative controls is assumed to be 1 (null effect)
          )
        })
      )
      
      # Define target-comparator-outcome (TCO) pairs for the analysis.
      # In this study, there is one TCO pair, which will be analyzed against all outcomes.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude the target and comparator cohort IDs from covariate analysis.
          # These are the exposures under study and should not be treated as covariates.
          # Any additional concepts specified in `excludedCovariateConcepts` (from specs)
          # would also be added here, but it is empty in this case.
          excludedCovariateConceptIds = c(
            cmTcList$targetCohortId[i], # Exclude the target exposure concept
            cmTcList$comparatorCohortId[i], # Exclude the comparator exposure concept
            excludedCovariateConcepts$conceptId # Any additional concepts to exclude (empty here)
          )
        )
      }

      # Arguments for retrieving data for CohortMethod (from the CDM).
      #
      # Analysis Specifications:
      # - getDbCohortMethodDataArgs:
      #   - studyPeriods: (handled by loop variables currentStudyStartDate, currentStudyEndDate)
      #   - maxCohortSize: 0
      #   - restrictToCommonPeriod: true
      #   - firstExposureOnly: true
      #   - washoutPeriod: 365
      #   - removeDuplicateSubjects: "remove all"
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = currentStudyStartDate,
        studyEndDate = currentStudyEndDate,
        restrictToCommonPeriod = TRUE, # Restrict to the period when both cohorts are observed
        maxCohortSize = 0, # 0 means no restriction on the maximum cohort size for data retrieval
        covariateSettings = covariateSettings, # Use the defined covariate settings
        firstExposureOnly = TRUE, # Only include a subject's first exposure in the cohort
        washoutPeriod = 365, # Minimum days between a prior exposure and the current exposure
        removeDuplicateSubjects = "remove all" # If a subject has multiple exposures, remove all of them
      )

      # Arguments for creating propensity scores (PS).
      #
      # Analysis Specifications:
      # - propensityScoreAdjustment.createPsArgs:
      #   - maxCohortSizeForFitting: 250000
      #   - errorOnHighCorrelation: true
      #   - prior: { priorType: "laplace", useCrossValidation: true }
      #   - control: { tolerance: 2e-7, cvType: "auto", fold: 10, cvRepetitions: 10, noiseLevel: "silent", resetCoefficients: true, startingVariance: 0.01 }
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # Max cohort size for PS model fitting to manage memory
        errorOnHighCorrelation = TRUE, # Stop if high correlation between covariates is found
        stopOnError = FALSE, # Allow Strategus to continue even if PS model fitting fails (diagnostics will catch it)
        estimator = "att", # Estimate Average Treatment Effect on the Treated
        prior = Cyclops::createPrior( # Prior distribution for regularization
          priorType = "laplace", # Laplace prior (L1 regularization)
          exclude = c(0), # Exclude the intercept term from regularization
          useCrossValidation = TRUE # Use cross-validation to tune the prior
        ),
        control = Cyclops::createControl( # Control settings for the Cyclops optimizer
          noiseLevel = "silent", # Suppress verbose output
          cvType = "auto", # Automatic cross-validation type selection
          seed = 1, # Seed for reproducibility of cross-validation
          resetCoefficients = TRUE, # Reset coefficients at start of each optimization
          tolerance = 2e-07, # Tolerance for convergence
          cvRepetitions = 10, # Number of cross-validation repetitions (from Analysis Specifications)
          startingVariance = 0.01 # Starting variance for optimization
        )
      )

      # Arguments for computing covariate balance BEFORE matching/stratification (shared covariates).
      # These are default settings from the template.
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL # No specific filter for shared covariates
      )
      
      # Arguments for computing covariate balance AFTER matching/stratification.
      # These are default settings from the template.
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Use default table 1 specs for balance reporting
      )

      # Arguments for fitting the outcome model.
      #
      # Analysis Specifications:
      # - fitOutcomeModelArgs:
      #   - modelType: "cox"
      #   - stratified: true
      #   - useCovariates: false
      #   - inversePtWeighting: false
      #   - prior: { priorType: "laplace", useCrossValidation: true }
      #   - control: { tolerance: 2e-7, cvType: "auto", fold: 10, cvRepetitions: 10, noiseLevel: "quiet", resetCoefficients: true, startingVariance: 0.01 }
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # Cox proportional hazards model for time-to-event outcomes
        stratified = TRUE, # Stratify by PS stratum or matched set ID
        useCovariates = FALSE, # Do not include additional covariates in the outcome model
        inversePtWeighting = FALSE, # Do not use inverse propensity score weighting
        prior = Cyclops::createPrior( # Prior distribution for regularization
          priorType = "laplace", # Laplace prior (L1 regularization)
          useCrossValidation = TRUE # Use cross-validation to tune the prior
        ),
        control = Cyclops::createControl( # Control settings for the Cyclops optimizer
          cvType = "auto", # Automatic cross-validation type selection
          seed = 1, # Seed for reproducibility of cross-validation
          resetCoefficients = TRUE, # Reset coefficients at start of each optimization
          startingVariance = 0.01, # Starting variance for optimization
          tolerance = 2e-07, # Tolerance for convergence
          cvRepetitions = 10, # Number of cross-validation repetitions (from Analysis Specifications)
          noiseLevel = "quiet" # Suppress some output, but less than "silent"
        )
      )
      
      # Arguments for creating the study population from the cohort data.
      #
      # Analysis Specifications:
      # - createStudyPopArgs:
      #   - restrictToCommonPeriod: false
      #   - firstExposureOnly: false
      #   - washoutPeriod: 0
      #   - removeDuplicateSubjects: "keep all"
      #   - censorAtNewRiskWindow: false
      #   - removeSubjectsWithPriorOutcome: true
      #   - priorOutcomeLookback: 99999
      #   - timeAtRisks (individual fields from loop variable t)
      #   - minDaysAtRisk: 1
      #   - maxDaysAtRisk: 99999 (template default)
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # Do not restrict study population to common observation period
        firstExposureOnly = FALSE, # Do not restrict to first exposure only within study population
        washoutPeriod = 0, # No washout period required before risk window for study population
        removeDuplicateSubjects = "keep all", # Keep all duplicate subjects in study population
        censorAtNewRiskWindow = FALSE, # Do not censor at a new risk window
        removeSubjectsWithPriorOutcome = TRUE, # Remove subjects who had the outcome prior to risk start
        priorOutcomeLookback = 99999, # Look back indefinitely for prior outcomes
        riskWindowStart = timeAtRisks$riskWindowStart[t], # Risk window start (from loop)
        startAnchor = timeAtRisks$startAnchor[t], # Anchor for risk window start (from loop)
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # Risk window end (from loop)
        endAnchor = timeAtRisks$endAnchor[t], # Anchor for risk window end (from loop)
        minDaysAtRisk = 1, # Minimum days a subject must be at risk to be included
        maxDaysAtRisk = 99999 # Maximum days a subject can be at risk (default, no upper limit)
      )

      # Append the current combination of settings to the CohortMethod analysis list.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId, # Unique ID for this specific analysis
        description = sprintf( # Descriptive string for this analysis
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
# This defines the overall CohortMethod module settings, including all TCOs and the list of analyses.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList, # The list of all CohortMethod analyses to perform
  targetComparatorOutcomesList = targetComparatorOutcomesList, # The list of TCO pairs
  analysesToExclude = NULL, # No specific analyses are excluded
  refitPsForEveryOutcome = FALSE, # Do not refit the PS model for each outcome
  refitPsForEveryStudyPopulation = FALSE, # Do not refit the PS model for each study population (i.e., each TAR)
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Use default diagnostic thresholds for CohortMethod
)

# Create the final analysis specifications object ------------------------------
# This object combines all shared resources and module specifications into a complete
# Strategus analysis plan.
#
# Analysis Specifications:
# - name: "rapidcyclejanssen" (used for file naming)
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path is structured as "inst/studyName/studyNameAnalysisSpecification.json".
# The studyName "rapidcyclejanssen" is taken from the Analysis Specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "rapidcyclejanssen", "rapidcyclejanssenAnalysisSpecification.json")
)