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
# Base URL for the ATLAS/WebAPI instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions from WebAPI based on their IDs.
# These IDs correspond to the target, comparator, and outcome cohorts specified in Analysis Specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: glp1radepression - target1
    1794132, # Comparator: glp1radepression - comparator1
    1794131  # Outcome: glp1radepression - outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to a simpler 1, 2, 3 scheme for internal use in the study.
# This maps the original WebAPI IDs to sequential study-specific IDs.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Target cohort ID
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Comparator cohort ID
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Outcome cohort ID

# Negative control outcomes
# Retrieve the concept set definition for negative controls from WebAPI.
# The conceptSetId is specified in Analysis Specifications under negativeControlConceptSet.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Negative control concept set ID from Analysis Specifications
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
  mutate(cohortId = row_number() + 100) %>% # Assign unique cohort IDs starting from 101 for negative controls
  select(cohortId, cohortName, outcomeConceptId)


if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Filter for the outcome cohort (ID 3 after re-numbering).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Outcome cohort ID (outcome1)
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not specified in JSON

# Target and Comparator for the CohortMethod analysis 
# Assign the re-numbered target (1) and comparator (2) cohort IDs and names.
cmTcList <- data.frame(
  targetCohortId = 1, # Target cohort ID (target1)
  targetCohortName = "target1", # Target cohort name
  comparatorCohortId = 2, # Comparator cohort ID (comparator1)
  comparatorCohortName = "comparator1" # Comparator cohort name
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study. The Analysis Specifications indicate no specific concepts to exclude
# beyond the target/comparator concepts themselves.
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(0), # No specific concepts to exclude from Analysis Specifications
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# The Analysis Specifications indicate no specific concepts to include.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Shared resource for cohort definitions.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Shared resource for negative control outcome cohort definitions.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default, not specified in JSON
  detectOnDescendants = TRUE # Default, not specified in JSON
)
# Module specifications for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Generate cohort statistics
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Module specifications for CohortDiagnostics.
# No specific settings provided in Analysis Specifications, using template defaults.
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
  minCharacterizationMean = 0.01
)

# CohortMethodModule -----------------------------------------------------------

# Study periods: Defined in Analysis Specifications under getDbCohortMethodDataArgs.studyPeriods.
studyPeriods <- tibble(
  studyStartDate = c("20130101"), # YYYYMMDD format
  studyEndDate   = c("20201231")  # YYYYMMDD format
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# Defined in Analysis Specifications under createStudyPopArgs.timeAtRisks.
timeAtRisks <- tibble(
  label = c("TAR_1_730"), # Label for this time-at-risk window
  riskWindowStart  = c(1), # Risk window start day relative to anchor
  startAnchor = c("cohort start"), # Anchor point for risk window start
  riskWindowEnd  = c(730), # Risk window end day relative to anchor
  endAnchor = c("cohort start") # Anchor point for risk window end
) 

# Propensity Score settings - match on PS
# Defined in Analysis Specifications under propensityScoreAdjustment.psSettings.matchOnPsArgs.
matchOnPsArgsList <- tibble(
  label = c("MatchOnPs_1"), # Label for this PS matching setting
  maxRatio  = c(1), # Maximum number of comparators to match to each target
  caliper = c(0.05), # Caliper for matching
  caliperScale  = c("standardized logit") # Scale of the caliper
) 

# Propensity Score settings - stratify by PS
# Defined as null in Analysis Specifications, so this list remains empty.
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


# Iterate through all analysis setting combinations
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment arguments based on the method (match or stratify)
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio, # Max ratio for matching from Analysis Specifications
          caliper = psCfg$params$caliper, # Caliper for matching from Analysis Specifications
          caliperScale = psCfg$params$caliperScale, # Caliper scale from Analysis Specifications
          allowReverseMatch = FALSE, # Default, not specified in JSON
          stratificationColumns = c() # Default, not specified in JSON
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata, # Number of strata from Analysis Specifications
          stratificationColumns = c(), # Default, not specified in JSON
          baseSelection = psCfg$params$baseSelection # Base selection for stratification from Analysis Specifications
        )
      }

      # Covariate settings: Using default settings.
      # Analysis Specifications has empty conceptsToInclude/Exclude, so no specific concepts are passed here.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default, not specified in JSON
      )

      # Define outcomes for the analysis, including true outcomes and negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i], # Outcome cohort ID
            outcomeOfInterest = TRUE, # This is a true outcome
            trueEffectSize = NA, # Not applicable for true outcomes
            priorOutcomeLookback = 99999 # Prior outcome lookback from Analysis Specifications
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i, # Negative control cohort ID
            outcomeOfInterest = FALSE, # This is a negative control
            trueEffectSize = 1 # True effect size for negative controls (null effect)
          )
        })
      )
      
      # Define target-comparator-outcome combinations.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i], # Target cohort ID
          comparatorId = cmTcList$comparatorCohortId[i], # Comparator cohort ID
          outcomes = outcomeList, # List of outcomes for this T-C pair
          # Exclude target and comparator cohort IDs from covariates, plus any additional excluded concepts.
          excludedCovariateConceptIds = c(
            cmTcList$targetCohortId[i], # Exclude target cohort ID as a covariate
            cmTcList$comparatorCohortId[i], # Exclude comparator cohort ID as a covariate
            excludedCovariateConcepts$conceptId # Additional excluded concepts from Analysis Specifications
          )
        )
      }

      # Arguments for fetching cohort method data from the database.
      # Settings from Analysis Specifications under getDbCohortMethodDataArgs.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Restrict to common observation period from Analysis Specifications
        studyStartDate = studyStartDate, # Study start date from current loop iteration
        studyEndDate = studyEndDate, # Study end date from current loop iteration
        maxCohortSize = 0, # Max cohort size (0 means no restriction) from Analysis Specifications
        covariateSettings = covariateSettings # Covariate settings defined above
      )

      # Arguments for creating propensity scores.
      # Settings from Analysis Specifications under propensityScoreAdjustment.createPsArgs.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # Max cohort size for fitting PS model from Analysis Specifications
        errorOnHighCorrelation = TRUE, # Error on high correlation from Analysis Specifications
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att", # Default, not specified in JSON
        prior = Cyclops::createPrior( # Prior settings for PS model
          priorType = "laplace", # Prior type from Analysis Specifications
          exclude = c(0), # Default, not specified in JSON
          useCrossValidation = TRUE # Use cross-validation from Analysis Specifications
        ),
        control = Cyclops::createControl( # Control settings for PS model
          noiseLevel = "silent", # Noise level from Analysis Specifications
          cvType = "auto", # Cross-validation type from Analysis Specifications
          seed = 1, # Default, not specified in JSON
          resetCoefficients = TRUE, # Reset coefficients from Analysis Specifications
          tolerance = 2e-07, # Tolerance from Analysis Specifications
          cvRepetitions = 10, # Cross-validation repetitions from Analysis Specifications
          startingVariance = 0.01, # Starting variance from Analysis Specifications
          fold = 10 # Number of folds for cross-validation from Analysis Specifications
        )
      )

      # Arguments for computing shared covariate balance.
      # No specific settings in Analysis Specifications, using template defaults.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default, not specified in JSON
        covariateFilter = NULL # Default, not specified in JSON
      )
      # Arguments for computing covariate balance.
      # No specific settings in Analysis Specifications, using template defaults.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default, not specified in JSON
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default, not specified in JSON
      )

      # Arguments for fitting the outcome model.
      # Settings from Analysis Specifications under fitOutcomeModelArgs.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # Model type from Analysis Specifications
        stratified = TRUE, # Stratified analysis from Analysis Specifications
        useCovariates = FALSE, # Use covariates in outcome model from Analysis Specifications
        inversePtWeighting = FALSE, # Inverse probability of treatment weighting from Analysis Specifications
        prior = Cyclops::createPrior( # Prior settings for outcome model
          priorType = "laplace", # Prior type from Analysis Specifications
          useCrossValidation = TRUE # Use cross-validation from Analysis Specifications
        ),
        control = Cyclops::createControl( # Control settings for outcome model
          cvType = "auto", # Cross-validation type from Analysis Specifications
          seed = 1, # Default, not specified in JSON
          resetCoefficients = TRUE, # Reset coefficients from Analysis Specifications
          startingVariance = 0.01, # Starting variance from Analysis Specifications
          tolerance = 2e-07, # Tolerance from Analysis Specifications
          cvRepetitions = 10, # Cross-validation repetitions from Analysis Specifications
          noiseLevel = "quiet", # Noise level from Analysis Specifications
          fold = 10 # Number of folds for cross-validation from Analysis Specifications
        )
      )
      
      # Arguments for creating the study population.
      # Settings from Analysis Specifications under createStudyPopArgs.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE, # Restrict to common observation period from Analysis Specifications
        firstExposureOnly = FALSE, # First exposure only from Analysis Specifications
        washoutPeriod = 0, # Washout period from Analysis Specifications
        removeDuplicateSubjects = "keep all", # How to handle duplicate subjects from Analysis Specifications
        censorAtNewRiskWindow = FALSE, # Censor at new risk window from Analysis Specifications
        removeSubjectsWithPriorOutcome = TRUE, # Remove subjects with prior outcome from Analysis Specifications
        priorOutcomeLookback = 99999, # Prior outcome lookback from Analysis Specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t], # Risk window start from current loop iteration
        startAnchor = timeAtRisks$startAnchor[t], # Start anchor from current loop iteration
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # Risk window end from current loop iteration
        endAnchor = timeAtRisks$endAnchor[t], # End anchor from current loop iteration
        minDaysAtRisk = 1, # Minimum days at risk from Analysis Specifications
        maxDaysAtRisk = 99999 # Default, not specified in JSON
      )


      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId, # Unique ID for this analysis
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
# No specific settings for refitPsForEveryOutcome, refitPsForEveryStudyPopulation,
# or cmDiagnosticThresholds in Analysis Specifications, using template defaults.
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList, # List of CohortMethod analyses
  targetComparatorOutcomesList = targetComparatorOutcomesList, # List of T-C-O combinations
  analysesToExclude = NULL, # Default, not specified in JSON
  refitPsForEveryOutcome = FALSE, # Default, not specified in JSON
  refitPsForEveryStudyPopulation = FALSE, # Default, not specified in JSON
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default, not specified in JSON
)

# Create the analysis specifications ------------------------------------------
# Initialize empty analysis specifications and add shared resources and module specifications.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> # Add cohort definitions as a shared resource
  Strategus::addSharedResources(negativeControlsShared) |> # Add negative controls as a shared resource
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |> # Add CohortGenerator module
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |> # Add CohortDiagnostics module
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications) # Add CohortMethod module

# Save the complete analysis specifications to a JSON file.
# The file path is constructed based on the study name "glp1radepression".
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "glp1radepression", "glp1radepressionAnalysisSpecification.json")
)