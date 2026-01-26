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
# Base URL for the OHDSI WebAPI (e.g., Atlas instance)
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions from WebAPI using the specified cohort IDs.
# These IDs correspond to the target, comparator, and outcome cohorts
# defined in the Analysis Specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1 (from Analysis Specifications: cohortDefinitions.targetCohort.id)
    1794132, # Comparator: comparator1 (from Analysis Specifications: cohortDefinitions.comparatorCohort.id)
    1794131  # Outcome: outcome1 (from Analysis Specifications: cohortDefinitions.outcomeCohort[0].id)
  ),
  generateStats = TRUE # Generate cohort statistics during export
)

# Re-number cohorts for internal Strategus use.
# The original cohort IDs are mapped to simpler, sequential IDs (1, 2, 3)
# for easier reference within the study analysis.
# Target cohort (ID 1794126) is re-numbered to 1.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# Comparator cohort (ID 1794132) is re-numbered to 2.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# Outcome cohort (ID 1794131) is re-numbered to 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve concept set definition for negative controls from WebAPI.
# The conceptSetId is specified in the Analysis Specifications.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # From Analysis Specifications: negativeControlConceptSet.id
  baseUrl = baseUrl
) %>%
  # Resolve the concept set to its constituent concepts.
  ROhdsiWebApi::resolveConceptSet(
    baseUrl = baseUrl
  ) %>%
  # Get detailed information for the resolved concepts.
  ROhdsiWebApi::getConcepts(
    baseUrl = baseUrl
  ) %>%
  # Rename columns to match expected format for outcome cohorts.
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  # Assign unique cohort IDs for negative controls, starting from 101
  # to avoid collision with target/comparator/outcome cohorts (1, 2, 3).
  mutate(cohortId = row_number() + 100) %>%
  # Select relevant columns.
  select(cohortId, cohortName, outcomeConceptId)


# Check for duplicate cohort IDs across all defined cohorts to prevent errors.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes:
# Filter for the re-numbered outcome cohort (ID 3) from the cohortDefinitionSet.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not specified in Analysis Specifications.

# Target and Comparator for the CohortMethod analysis
# Use the re-numbered target (1) and comparator (2) cohort IDs and names.
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered target cohort ID
  targetCohortName = "target1", # From Analysis Specifications: cohortDefinitions.targetCohort.name
  comparatorCohortId = 2, # Re-numbered comparator cohort ID
  comparatorCohortName = "comparator1" # From Analysis Specifications: cohortDefinitions.comparatorCohort.name
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study.
# From Analysis Specifications: covariateSelection.conceptsToExclude.
# In this specification, conceptsToExclude is empty, so an empty data frame is created.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all
# From Analysis Specifications: covariateSelection.conceptsToInclude.
# In this specification, conceptsToInclude is empty, so this section is commented out.
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
  occurrenceType = "first", # Default, not specified in Analysis Specifications.
  detectOnDescendants = TRUE # Default, not specified in Analysis Specifications.
)
# Create module specifications for the CohortGenerator module.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Default, not specified in Analysis Specifications.
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# Initialize CohortDiagnosticsModule settings creator.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Create module specifications for the CohortDiagnostics module.
# It will run diagnostics on all re-numbered cohorts (target, comparator, outcome, negative controls).
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # All re-numbered cohorts
  runInclusionStatistics = TRUE, # Default, not specified in Analysis Specifications.
  runIncludedSourceConcepts = TRUE, # Default, not specified in Analysis Specifications.
  runOrphanConcepts = TRUE, # Default, not specified in Analysis Specifications.
  runTimeSeries = FALSE, # Default, not specified in Analysis Specifications.
  runVisitContext = TRUE, # Default, not specified in Analysis Specifications.
  runBreakdownIndexEvents = TRUE, # Default, not specified in Analysis Specifications.
  runIncidenceRate = TRUE, # Default, not specified in Analysis Specifications.
  runCohortRelationship = TRUE, # Default, not specified in Analysis Specifications.
  runTemporalCohortCharacterization = TRUE, # Default, not specified in Analysis Specifications.
  minCharacterizationMean = 0.01 # Default, not specified in Analysis Specifications.
)

# CohortMethodModule -----------------------------------------------------------

# Study periods for restricting data retrieval.
# From Analysis Specifications: getDbCohortMethodDataArgs.studyPeriods.
studyPeriods <- tibble(
  studyStartDate = c("20101019"), # YYYYMMDD format
  studyEndDate   = c("20181231")  # YYYYMMDD format
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# From Analysis Specifications: createStudyPopArgs.timeAtRisks.
timeAtRisks <- tibble(
  label = c("TAR_1_5_CE", "TAR_1_0_CE", "TAR_1_99999_CS"), # Descriptive labels for each TAR
  riskWindowStart  = c(1, 1, 1),
  startAnchor = c("cohort start", "cohort start", "cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd  = c(5, 0, 99999),
  endAnchor = c("cohort end", "cohort end", "cohort start"), # "cohort start" | "cohort end"
  minDaysAtRisk = c(1, 1, 1) # From Analysis Specifications: createStudyPopArgs.timeAtRisks.minDaysAtRisk
)

# Propensity Score settings - match on PS.
# From Analysis Specifications: propensityScoreAdjustment.psSettings where matchOnPsArgs is not null.
matchOnPsArgsList <- tibble(
  label = c("Match_Ratio1_Caliper0.2", "Match_Ratio100_Caliper0.2"), # Descriptive labels for each matching setting
  maxRatio  = c(1, 100), # From Analysis Specifications: psSettings.matchOnPsArgs.maxRatio
  caliper = c(0.2, 0.2), # From Analysis Specifications: psSettings.matchOnPsArgs.caliper
  caliperScale  = c("standardized logit", "standardized logit") # From Analysis Specifications: psSettings.matchOnPsArgs.caliperScale
)

# Propensity Score settings - stratify by PS.
# From Analysis Specifications: propensityScoreAdjustment.psSettings where stratifyByPsArgs is not null.
# In this specification, stratifyByPsArgs is always null, so this tibble remains empty.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c() # "all" | "target" | "comparator"
)

# Build a single PS configuration list (each entry has: method, label, params)
# This list will combine all matching and stratification settings.
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
          maxRatio = psCfg$params$maxRatio, # From psConfigList (derived from matchOnPsArgsList)
          caliper = psCfg$params$caliper, # From psConfigList (derived from matchOnPsArgsList)
          caliperScale = psCfg$params$caliperScale, # From psConfigList (derived from matchOnPsArgsList)
          allowReverseMatch = FALSE, # Default, not specified in Analysis Specifications.
          stratificationColumns = c() # Default, not specified in Analysis Specifications.
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata, # From psConfigList (derived from stratifyByPsArgsList)
          stratificationColumns = c(), # Default, not specified in Analysis Specifications.
          baseSelection = psCfg$params$baseSelection # From psConfigList (derived from stratifyByPsArgsList)
        )
      }

      # Covariate settings for feature extraction.
      # From Analysis Specifications: covariateSelection.conceptsToInclude and conceptsToExclude.
      # Since both are empty in the spec, default covariate settings are used.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default, not specified in Analysis Specifications.
      )

      # Define outcome cohorts for the CohortMethod analysis.
      # Includes the primary outcome and all negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i], # Re-numbered outcome cohort ID
            outcomeOfInterest = TRUE, # Mark as primary outcome
            trueEffectSize = NA, # Not applicable for observed outcomes
            priorOutcomeLookback = 99999 # Default, not specified in Analysis Specifications.
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i, # Negative control cohort ID
            outcomeOfInterest = FALSE, # Mark as negative control
            trueEffectSize = 1 # Default, not specified in Analysis Specifications.
          )
        })
      )

      # Define target-comparator-outcome combinations.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i], # Re-numbered target cohort ID
          comparatorId = cmTcList$comparatorCohortId[i], # Re-numbered comparator cohort ID
          outcomes = outcomeList, # All defined outcomes
          # Excluded covariate concepts.
          # Based on Analysis Specifications, covariateSelection.conceptsToExclude is empty,
          # so this will be an empty vector.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Arguments for retrieving cohort method data from the database.
      # From Analysis Specifications: getDbCohortMethodDataArgs.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications: getDbCohortMethodDataArgs.restrictToCommonPeriod
        studyStartDate = studyStartDate, # From current loop iteration
        studyEndDate = studyEndDate, # From current loop iteration
        maxCohortSize = 0, # From Analysis Specifications: getDbCohortMethodDataArgs.maxCohortSize (0 means no restriction)
        covariateSettings = covariateSettings, # Defined earlier
        firstExposureOnly = TRUE, # From Analysis Specifications: getDbCohortMethodDataArgs.firstExposureOnly
        washoutPeriod = 0, # From Analysis Specifications: getDbCohortMethodDataArgs.washoutPeriod
        removeDuplicateSubjects = "remove all" # From Analysis Specifications: getDbCohortMethodDataArgs.removeDuplicateSubjects
      )

      # Arguments for creating propensity scores.
      # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications: createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From Analysis Specifications: createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Default, not specified in Analysis Specifications.
        estimator = "att", # Default, not specified in Analysis Specifications.
        prior = Cyclops::createPrior( # Prior settings for regularization
          priorType = "laplace", # From Analysis Specifications: createPsArgs.prior.priorType
          exclude = c(0), # Default, not specified in Analysis Specifications.
          useCrossValidation = TRUE # From Analysis Specifications: createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings for Cyclops solver
          noiseLevel = "silent", # From Analysis Specifications: createPsArgs.control.noiseLevel
          cvType = "auto", # From Analysis Specifications: createPsArgs.control.cvType
          seed = 1, # Default, not specified in Analysis Specifications.
          resetCoefficients = TRUE, # From Analysis Specifications: createPsArgs.control.resetCoefficients
          tolerance = 2e-07, # From Analysis Specifications: createPsArgs.control.tolerance
          cvRepetitions = 10, # From Analysis Specifications: createPsArgs.control.cvRepetitions
          startingVariance = 0.01, # From Analysis Specifications: createPsArgs.control.startingVariance
          fold = 10 # From Analysis Specifications: createPsArgs.control.fold
        )
      )

      # Arguments for computing shared covariate balance.
      # Not explicitly in Analysis Specifications, using template defaults.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      # Arguments for computing covariate balance.
      # Not explicitly in Analysis Specifications, using template defaults.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting the outcome model.
      # From Analysis Specifications: fitOutcomeModelArgs.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From Analysis Specifications: fitOutcomeModelArgs.modelType
        stratified = TRUE, # From Analysis Specifications: fitOutcomeModelArgs.stratified
        useCovariates = FALSE, # From Analysis Specifications: fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE, # From Analysis Specifications: fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior( # Prior settings for regularization
          priorType = "laplace", # From Analysis Specifications: fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE # From Analysis Specifications: fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings for Cyclops solver
          cvType = "auto", # From Analysis Specifications: fitOutcomeModelArgs.control.cvType
          seed = 1, # Default, not specified in Analysis Specifications.
          resetCoefficients = TRUE, # From Analysis Specifications: fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01, # From Analysis Specifications: fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07, # From Analysis Specifications: fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10, # From Analysis Specifications: fitOutcomeModelArgs.control.cvRepetitions
          noiseLevel = "quiet", # From Analysis Specifications: fitOutcomeModelArgs.control.noiseLevel
          fold = 10 # From Analysis Specifications: fitOutcomeModelArgs.control.fold
        )
      )

      # Arguments for creating the study population.
      # From Analysis Specifications: createStudyPopArgs.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications: createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE, # From Analysis Specifications: createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0, # From Analysis Specifications: createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # From Analysis Specifications: createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # From Analysis Specifications: createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = FALSE, # From Analysis Specifications: createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # From Analysis Specifications: createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From current loop iteration (timeAtRisks)
        startAnchor = timeAtRisks$startAnchor[t], # From current loop iteration (timeAtRisks)
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From current loop iteration (timeAtRisks)
        endAnchor = timeAtRisks$endAnchor[t], # From current loop iteration (timeAtRisks)
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From current loop iteration (timeAtRisks)
        maxDaysAtRisk = 99999 # Default, not specified in Analysis Specifications.
      )


      # Append the settings to Analysis List
      # Each unique combination of settings forms a CohortMethod analysis.
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
# Create module specifications for the CohortMethod module.
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList, # List of all CohortMethod analyses
  targetComparatorOutcomesList = targetComparatorOutcomesList, # List of TCO combinations
  analysesToExclude = NULL, # Default, not specified in Analysis Specifications.
  refitPsForEveryOutcome = FALSE, # Default, not specified in Analysis Specifications.
  refitPsForEveryStudyPopulation = FALSE, # Default, not specified in Analysis Specifications.
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default, not specified in Analysis Specifications.
)

# Create the analysis specifications ------------------------------------------
# Initialize an empty Strategus analysis specifications object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Add shared resources (cohort definitions, negative controls).
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  # Add module specifications for each module.
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the complete analysis specifications to a JSON file.
# The file path uses the study name from Analysis Specifications.name.
studyName <- "doacsandwarfarin" # From Analysis Specifications: name
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", studyName, paste0(studyName, "AnalysisSpecification.json"))
)