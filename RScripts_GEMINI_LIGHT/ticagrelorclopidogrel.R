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
library(ROhdsiWebApi) # Required for WebAPI calls
library(CohortMethod) # Required for CohortMethod specific args creation
library(FeatureExtraction) # Required for covariate settings
library(Cyclops) # Required for prior and control objects

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
# Base URL for the OHDSI WebAPI, used to fetch cohort definitions and concept sets.
# Note: A placeholder URL is used as it's not provided in Analysis Specifications.
# Users should replace this with their actual WebAPI URL.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions from Analysis Specifications
# These IDs correspond to the target, comparator, and outcome cohorts defined in the JSON.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: "target1"
    1794132, # Comparator: "comparator1"
    1794131  # Outcome: "outcome1"
  ),
  generateStats = TRUE # Generates inclusion rule statistics for cohorts.
)

# Re-number cohorts to a simplified scheme (1, 2, 3) for easier referencing in the study
# This mapping ensures consistency with the study design within the script.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Map target1 to ID 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Map comparator1 to ID 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Map outcome1 to ID 3

# Negative control outcomes from Analysis Specifications
# Fetches the concept set for negative controls, resolves it to individual concepts,
# and converts them into a cohort set structure for use as outcomes.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # ID for "negative" concept set from Analysis Specifications
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
  # conflicts with target/comparator/outcome cohort IDs (1, 2, 3).
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs to prevent issues in the analysis.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Filter for the main outcome cohort (ID 3 after re-numbering).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default cleanWindow as not specified in Analysis Specifications

# Target and Comparator for the CohortMethod analysis 
# Uses the re-numbered target (ID 1) and comparator (ID 2) cohorts.
cmTcList <- data.frame(
  targetCohortId = 1, # Mapped from 1794126
  targetCohortName = cohortDefinitionSet[cohortDefinitionSet$cohortId == 1,]$cohortName,
  comparatorCohortId = 2, # Mapped from 1794132
  comparatorCohortName = cohortDefinitionSet[cohortDefinitionSet$cohortId == 2,]$cohortName
)

# For the CohortMethod LSPS we'll need to exclude specific drugs/concepts.
# Based on Analysis Specifications, `conceptsToExclude` is empty, so this will be an empty data frame.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0), # No concepts specified for exclusion in Analysis Specifications
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# Based on Analysis Specifications, `conceptsToInclude` is empty, so this section remains commented out.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Defines shared resources for cohort definitions (target, comparator, outcome).
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Defines shared resources for negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default value, not specified in Analysis Specifications
  detectOnDescendants = TRUE # Default value, not specified in Analysis Specifications
)
# Specifies settings for the CohortGenerator module.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Generate statistics for cohort generation.
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Specifies settings for the CohortDiagnostics module.
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId), # All cohort IDs for diagnostics
  runInclusionStatistics = TRUE, # Default, not specified in Analysis Specifications
  runIncludedSourceConcepts = TRUE, # Default, not specified in Analysis Specifications
  runOrphanConcepts = TRUE, # Default, not specified in Analysis Specifications
  runTimeSeries = FALSE, # Default, not specified in Analysis Specifications
  runVisitContext = TRUE, # Default, not specified in Analysis Specifications
  runBreakdownIndexEvents = TRUE, # Default, not specified in Analysis Specifications
  runIncidenceRate = TRUE, # Default, not specified in Analysis Specifications
  runCohortRelationship = TRUE, # Default, not specified in Analysis Specifications
  runTemporalCohortCharacterization = TRUE, # Default, not specified in Analysis Specifications
  minCharacterizationMean = 0.01 # Default, not specified in Analysis Specifications
)

# CohortMethodModule -----------------------------------------------------------

# Study periods from Analysis Specifications -> getDbCohortMethodDataArgs
studyPeriods <- tibble(
  # studyStartDate and studyEndDate are in YYYYMMDD format
  studyStartDate = c("20111101", "20130301"), 
  studyEndDate   = c("20190331", "20161231")
)

# Time-at-risks (TARs) for the outcomes of interest in your study
# From Analysis Specifications -> createStudyPopArgs
timeAtRisks <- tibble(
  label = c("TAR1", "TAR2", "TAR3"), # Descriptive labels for each TAR
  riskWindowStart  = c(1, 1, 1),
  startAnchor = c("cohort start", "cohort start", "cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd  = c(365, 1825, 0),
  endAnchor = c("cohort start", "cohort start", "cohort end"), # "cohort start" | "cohort end"
  minDaysAtRisk = c(1, 1, 1) # Minimum days at risk from Analysis Specifications
) 

# Propensity Score settings from Analysis Specifications -> propensityScoreAdjustment
# For 'match on PS' configurations
matchOnPsArgsList <- bind_rows(
  tibble(
    label = "Match1", # Label for the first matching setting
    maxRatio  = 1,
    caliper = 0.2,
    caliperScale  = "standardized logit" # "propensity score" | "standardized" | "standardized logit"
  ),
  tibble(
    label = "Match2", # Label for the second matching setting
    maxRatio  = 10,
    caliper = 0.2,
    caliperScale  = "standardized logit"
  )
)

# For 'stratify by PS' configurations
stratifyByPsArgsList <- tibble(
  label = "Stratify1", # Label for the stratification setting
  numberOfStrata  = 10,
  baseSelection = "all" # "all" | "target" | "comparator"
) 

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Convert each row of 'matchOnPsArgsList' to a PS configuration
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

# Convert each row of 'stratifyByPsArgsList' to a PS configuration
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

# Iterate through all analysis setting combinations
cmAnalysisList <- list()
analysisId <- 1

# Loop through each defined study period
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  # Loop through each defined time-at-risk
  for (t in seq_len(nrow(timeAtRisks))) {

    # Loop through each defined propensity score adjustment configuration
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Determine PS adjustment method (match or stratify) and create corresponding arguments
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default, not specified in Analysis Specifications
          stratificationColumns = c() # Default, not specified in Analysis Specifications
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default, not specified in Analysis Specifications
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings: Uses default settings as per Analysis Specifications,
      # with `conceptsToInclude` and `conceptsToExclude` being empty.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default, not specified in Analysis Specifications
      )

      # Create a list of all outcomes (main outcome + negative controls)
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE, # Main outcome is of interest
            trueEffectSize = NA, # No true effect size for main outcomes
            priorOutcomeLookback = 99999 # From Analysis Specifications -> createStudyPopArgs
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE, # Negative controls are not of primary interest
            trueEffectSize = 1, # Expected true effect size for negative controls is 1 (no effect)
            priorOutcomeLookback = 99999 # From Analysis Specifications -> createStudyPopArgs
          )
        })
      )
      
      # Define target-comparator-outcome combinations for the analysis
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Excluded covariate concepts: Based on Analysis Specifications -> covariateSelection,
          # no specific concepts are excluded beyond default, so `excludedCovariateConcepts` is empty.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId 
        )
      }

      # Arguments for retrieving cohort method data from the database
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Default, not specified in Analysis Specifications for this function
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From Analysis Specifications -> getDbCohortMethodDataArgs
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs
        errorOnHighCorrelation = TRUE, # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs
        stopOnError = FALSE, # Default value, allows Strategus to complete all CM operations
        estimator = "att", # Default value, not specified in Analysis Specifications
        prior = Cyclops::createPrior( # Prior settings from Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> prior
          priorType = "laplace", 
          exclude = c(0), # Default, not specified in Analysis Specifications
          useCrossValidation = TRUE # From Analysis Specifications
        ),
        control = Cyclops::createControl( # Control settings from Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> control
          noiseLevel = "silent", 
          cvType = "auto", 
          seed = 1, # Default, not specified in Analysis Specifications
          resetCoefficients = TRUE, 
          tolerance = 2e-07, 
          cvRepetitions = 10, # From Analysis Specifications (Note: template uses 1, spec uses 10)
          startingVariance = 0.01
        )
      )

      # Arguments for computing shared covariate balance (e.g., before PS adjustment)
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default, not specified in Analysis Specifications
        covariateFilter = NULL # Default, not specified in Analysis Specifications
      )
      # Arguments for computing covariate balance (e.g., after PS adjustment)
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default, not specified in Analysis Specifications
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default, not specified in Analysis Specifications
      )

      # Arguments for fitting the outcome model
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From Analysis Specifications -> fitOutcomeModelArgs
        stratified = FALSE, # From Analysis Specifications -> fitOutcomeModelArgs
        useCovariates = FALSE, # From Analysis Specifications -> fitOutcomeModelArgs
        inversePtWeighting = FALSE, # From Analysis Specifications -> fitOutcomeModelArgs
        prior = Cyclops::createPrior( # Prior settings from Analysis Specifications -> fitOutcomeModelArgs -> prior
          priorType = "laplace", 
          useCrossValidation = TRUE # From Analysis Specifications
        ),
        control = Cyclops::createControl( # Control settings from Analysis Specifications -> fitOutcomeModelArgs -> control
          cvType = "auto", 
          seed = 1, # Default, not specified in Analysis Specifications
          resetCoefficients = TRUE,
          startingVariance = 0.01, 
          tolerance = 2e-07, 
          cvRepetitions = 10, # From Analysis Specifications (Note: template uses 1, spec uses 10)
          noiseLevel = "quiet"
        )
      )
      
      # Arguments for creating the study population
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications -> createStudyPopArgs
        firstExposureOnly = FALSE, # From Analysis Specifications -> createStudyPopArgs
        washoutPeriod = 365, # From Analysis Specifications -> createStudyPopArgs
        removeDuplicateSubjects = "keep all", # From Analysis Specifications -> createStudyPopArgs
        censorAtNewRiskWindow = FALSE, # From Analysis Specifications -> createStudyPopArgs
        removeSubjectsWithPriorOutcome = TRUE, # From Analysis Specifications -> createStudyPopArgs
        priorOutcomeLookback = 99999, # From Analysis Specifications -> createStudyPopArgs
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999 # Default, not specified in Analysis Specifications
      )

      # Append the settings to Analysis List for this specific combination of study period, TAR, and PS adjustment
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

# Specifies settings for the CohortMethod module
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Default, not specified in Analysis Specifications
  refitPsForEveryOutcome = FALSE, # Default, not specified in Analysis Specifications
  refitPsForEveryStudyPopulation = FALSE, # Default, not specified in Analysis Specifications
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default, not specified in Analysis Specifications
)

# Create the analysis specifications ------------------------------------------
# Initialize empty analysis specifications
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Add shared resources (cohort definitions, negative controls)
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  # Add module specifications for CohortGenerator, CohortDiagnostics, and CohortMethod
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the complete analysis specifications to a JSON file
# The filename uses the study name from Analysis Specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "ticagrelorclopidogrel", "ticagrelorclopidogrelAnalysisSpecification.json")
)