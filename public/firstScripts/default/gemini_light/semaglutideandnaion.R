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
# Get the list of cohorts
# Base URL for OHDSI WebAPI, using a demo instance as default.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Define the cohort IDs from the analysis specifications.
# These IDs correspond to the target, comparator, and outcome cohorts in Atlas.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1 (from analysis specifications)
    1794132, # Comparator: comparator1 (from analysis specifications)
    1794131  # Outcome: outcome1 (from analysis specifications)
  ),
  generateStats = TRUE # Set to TRUE to generate cohort statistics.
)

# Re-number cohorts to a simplified scheme (1, 2, 3...) for internal use in the study.
# This makes it easier to refer to target, comparator, and outcome consistently.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Renumber target cohort
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Renumber comparator cohort
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Renumber outcome cohort

# Negative control outcomes
# Retrieve the concept set definition for negative controls from Atlas.
# These are used to evaluate potential unmeasured confounding.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Negative control concept set ID from analysis specifications
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet( # Resolve the concept set to individual concepts
    baseUrl = baseUrl
  ) %>%
  ROhdsiWebApi::getConcepts( # Get details for each concept
    baseUrl = baseUrl
  ) %>%
  rename(outcomeConceptId = "conceptId", # Rename 'conceptId' to 'outcomeConceptId' for clarity
         cohortName = "conceptName") %>% # Rename 'conceptName' to 'cohortName'
  mutate(cohortId = row_number() + 100) %>% # Assign unique cohort IDs starting from 101 for negative controls
  select(cohortId, cohortName, outcomeConceptId) # Select relevant columns

# Check for duplicate cohort IDs to prevent conflicts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes:
# Filter for the primary outcome cohort and prepare its details.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Filter for the re-numbered outcome cohort (ID 3)
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window for outcomes, not specified in analysis spec.

# Target and Comparator for the CohortMethod analysis
# Define the target and comparator cohorts using their re-numbered IDs and names.
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered target cohort ID
  targetCohortName = "target1", # Target cohort name from analysis specifications
  comparatorCohortId = 2, # Re-numbered comparator cohort ID
  comparatorCohortName = "comparator1" # Comparator cohort name from analysis specifications
)

# For the CohortMethod LSPS (Large Scale Propensity Score) we'll need to exclude
# specific concepts from covariate generation.
# Based on analysis specifications, no specific concepts are provided for exclusion,
# so this list will be empty.
excludedCovariateConcepts <- data.frame(
  conceptId = c(), # No concepts to exclude specified in analysis specifications
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all
# Based on analysis specifications, no specific concepts are provided for inclusion,
# so this section remains commented out.
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
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default: detect first occurrence of negative control
  detectOnDescendants = TRUE # Default: detect on descendants of negative control concepts
)
# Create module specifications for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Generate cohort statistics.
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# Initialize CohortDiagnosticsModule settings creator.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Create module specifications for CohortDiagnostics.
# This module runs various diagnostics on the generated cohorts.
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # Run diagnostics for all defined cohorts
  runInclusionStatistics = TRUE, # Run inclusion rule statistics
  runIncludedSourceConcepts = TRUE, # Run included source concepts analysis
  runOrphanConcepts = TRUE, # Run orphan concepts analysis
  runTimeSeries = FALSE, # Do not run time series analysis (default in template)
  runVisitContext = TRUE, # Run visit context analysis
  runBreakdownIndexEvents = TRUE, # Run breakdown index events analysis
  runIncidenceRate = TRUE, # Run incidence rate analysis
  runCohortRelationship = TRUE, # Run cohort relationship analysis
  runTemporalCohortCharacterization = TRUE, # Run temporal cohort characterization
  minCharacterizationMean = 0.01 # Minimum mean for characterization covariates
)

# CohortMethodModule -----------------------------------------------------------

# Study periods from analysis specifications.
# Defines the start and end dates for the study.
studyPeriods <- tibble(
  studyStartDate = c(20171201), # YYYYMMDD from analysis specifications
  studyEndDate   = c(20231231)  # YYYYMMDD from analysis specifications
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# Defines the risk window for outcome observation.
timeAtRisks <- tibble(
  label = c("TAR1"), # A descriptive label for this time-at-risk setting
  riskWindowStart  = c(1), # From analysis specifications
  startAnchor = c("cohort start"), # From analysis specifications
  riskWindowEnd  = c(0), # From analysis specifications
  endAnchor = c("cohort end") # From analysis specifications
)

# Propensity Score settings - match on PS
# Defines parameters for propensity score matching.
matchOnPsArgsList <- tibble(
  label = c("MatchOnPs1"), # A descriptive label for this matching setting
  maxRatio  = c(1), # From analysis specifications
  caliper = c(0.2), # From analysis specifications
  caliperScale  = c("standardized logit") # From analysis specifications
)

# Propensity Score settings - stratify by PS
# Defines parameters for propensity score stratification.
stratifyByPsArgsList <- tibble(
  label = c("StratifyByPs1"), # A descriptive label for this stratification setting
  numberOfStrata  = c(5), # From analysis specifications
  baseSelection = c("all") # From analysis specifications
)

# Build a single PS configuration list (each entry has: method, label, params)
# This list will combine both matching and stratification settings.
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

# Loop through each defined study period.
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  # Loop through each defined time-at-risk.
  for (t in seq_len(nrow(timeAtRisks))) {

    # Loop through each defined propensity score adjustment method (match or stratify).
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Configure matching or stratification arguments based on the current PS setting.
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio, # From analysis specifications
          caliper = psCfg$params$caliper, # From analysis specifications
          caliperScale = psCfg$params$caliperScale, # From analysis specifications
          allowReverseMatch = FALSE, # Template default
          stratificationColumns = c() # Template default
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata, # From analysis specifications
          stratificationColumns = c(), # Template default
          baseSelection = psCfg$params$baseSelection # From analysis specifications
        )
      }

      # Define covariate settings.
      # Using default settings, with descendants included for exclusion.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Template default
      )

      # Prepare the list of outcomes, including primary outcomes and negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE, # Mark as outcome of interest
            trueEffectSize = NA, # Not applicable for observed outcomes
            priorOutcomeLookback = 99999 # From analysis specifications
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE, # Mark as negative control
            trueEffectSize = 1 # Expected true effect size for negative controls
          )
        })
      )

      # Define target-comparator-outcome combinations.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i], # Target cohort ID
          comparatorId = cmTcList$comparatorCohortId[i], # Comparator cohort ID
          outcomes = outcomeList, # List of all outcomes (primary + negative controls)
          # Exclude specific covariate concepts.
          # The analysis specifications do not provide target/comparator concept IDs for exclusion,
          # so only the general excludedCovariateConcepts are used.
          excludedCovariateConceptIds = c(
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # Arguments for retrieving cohort method data from the database.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # From analysis specifications
        studyStartDate = studyStartDate, # From current study period iteration
        studyEndDate = studyEndDate, # From current study period iteration
        maxCohortSize = 0, # From analysis specifications (0 means no restriction)
        firstExposureOnly = FALSE, # From analysis specifications
        washoutPeriod = 0, # From analysis specifications
        removeDuplicateSubjects = "keep all", # From analysis specifications
        covariateSettings = covariateSettings # Defined above
      )

      # Arguments for creating propensity scores.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From analysis specifications
        errorOnHighCorrelation = TRUE, # From analysis specifications
        stopOnError = FALSE, # Template default: allow Strategus to complete even if PS model fails
        estimator = "att", # Template default: Average Treatment effect on the Treated
        prior = Cyclops::createPrior( # Prior settings for regularization
          priorType = "laplace", # From analysis specifications
          exclude = c(0), # Template default: exclude intercept from regularization
          useCrossValidation = TRUE # From analysis specifications
        ),
        control = Cyclops::createControl( # Control settings for Cyclops solver
          noiseLevel = "silent", # From analysis specifications
          cvType = "auto", # From analysis specifications
          seed = 1, # Template default: for reproducibility
          resetCoefficients = TRUE, # From analysis specifications
          tolerance = 2e-07, # From analysis specifications
          cvRepetitions = 10, # From analysis specifications (fold: 10, cvRepetitions: 10)
          startingVariance = 0.01 # From analysis specifications
        )
      )

      # Arguments for computing shared covariate balance (before PS adjustment).
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Template default
        covariateFilter = NULL # Template default: no specific filter
      )
      # Arguments for computing covariate balance (after PS adjustment).
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Template default
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Template default: use Table 1 specifications
      )

      # Arguments for fitting the outcome model.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From analysis specifications (Cox proportional hazards model)
        stratified = TRUE, # From analysis specifications (stratified by PS strata/matched sets)
        useCovariates = FALSE, # From analysis specifications (do not include covariates in outcome model)
        inversePtWeighting = FALSE, # From analysis specifications (do not use inverse probability of treatment weighting)
        prior = Cyclops::createPrior( # Prior settings for regularization
          priorType = "laplace", # From analysis specifications
          useCrossValidation = TRUE # From analysis specifications
        ),
        control = Cyclops::createControl( # Control settings for Cyclops solver
          cvType = "auto", # From analysis specifications
          seed = 1, # Template default: for reproducibility
          resetCoefficients = TRUE, # From analysis specifications
          startingVariance = 0.01, # From analysis specifications
          tolerance = 2e-07, # From analysis specifications
          cvRepetitions = 10, # From analysis specifications (fold: 10, cvRepetitions: 10)
          noiseLevel = "quiet" # From analysis specifications
        )
      )

      # Arguments for creating the study population.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From analysis specifications
        firstExposureOnly = FALSE, # From analysis specifications
        washoutPeriod = 0, # From analysis specifications
        removeDuplicateSubjects = "keep all", # From analysis specifications
        censorAtNewRiskWindow = TRUE, # From analysis specifications
        removeSubjectsWithPriorOutcome = TRUE, # From analysis specifications
        priorOutcomeLookback = 99999, # From analysis specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From current time-at-risk iteration
        startAnchor = timeAtRisks$startAnchor[t], # From current time-at-risk iteration
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From current time-at-risk iteration
        endAnchor = timeAtRisks$endAnchor[t], # From current time-at-risk iteration
        minDaysAtRisk = 1, # From analysis specifications
        maxDaysAtRisk = 99999 # Template default: no maximum days at risk
      )


      # Append the settings to Analysis List
      # Each entry in cmAnalysisList represents a complete CohortMethod analysis.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId, # Unique ID for this analysis
        description = sprintf( # Descriptive string for the analysis
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
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList, # List of all CohortMethod analyses to run
  targetComparatorOutcomesList = targetComparatorOutcomesList, # List of TCO combinations
  analysesToExclude = NULL, # No analyses to exclude
  refitPsForEveryOutcome = FALSE, # From analysis specifications (template default)
  refitPsForEveryStudyPopulation = FALSE, # From analysis specifications (template default)
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default diagnostic thresholds
)

# Create the analysis specifications ------------------------------------------
# Combine all module specifications and shared resources into a single analysis specification object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the complete analysis specifications to a JSON file.
# The file path is constructed using the study name from analysis specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "semaglutideandnaion", "semaglutideandnaionAnalysisSpecification.json") # Study name from analysis specifications
)