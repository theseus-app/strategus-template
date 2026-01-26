# Add necessary libraries
library(dplyr)
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(tibble) # Ensure tibble is loaded for tibble() function

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
# Base URL for the ATLAS/WebAPI instance where cohort definitions are stored.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions from WebAPI based on their IDs.
# These IDs correspond to the target, comparator, and outcome cohorts specified in the analysis.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to internal study IDs for consistency and simplicity.
# Target cohort (ID 1794126) is re-numbered to 1.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# Comparator cohort (ID 1794132) is re-numbered to 2.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# Outcome cohort (ID 1794131) is re-numbered to 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve the concept set definition for negative controls from WebAPI.
# The concept set ID 1888110 corresponds to "negative" in the analysis specifications.
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
  # Assign unique cohort IDs for negative controls, starting after the main cohorts.
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs between main cohorts and negative controls to prevent conflicts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts used in each analysis ----------------
# Outcomes: Filter for the re-numbered outcome cohort (ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window for outcomes

# Target and Comparator for the CohortMethod analysis
# Use the re-numbered target (ID 1) and comparator (ID 2) cohorts.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# For the CohortMethod analysis, define covariates to exclude.
# The analysis specifications indicate no specific concepts to exclude (null/empty).
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(0), # No specific concepts to exclude as per analysis specifications
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# The analysis specifications indicate no specific concepts to include (null/empty), so this is commented out.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Create shared resource specifications for cohort definitions.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Create shared resource specifications for negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default from template
  detectOnDescendants = TRUE # Default from template
)
# Create module specifications for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Generate cohort statistics
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Create module specifications for CohortDiagnostics.
# Run diagnostics for all defined cohorts.
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE, # Default from template
  runIncludedSourceConcepts = TRUE, # Default from template
  runOrphanConcepts = TRUE, # Default from template
  runTimeSeries = FALSE, # Default from template
  runVisitContext = TRUE, # Default from template
  runBreakdownIndexEvents = TRUE, # Default from template
  runIncidenceRate = TRUE, # Default from template
  runCohortRelationship = TRUE, # Default from template
  runTemporalCohortCharacterization = TRUE, # Default from template
  minCharacterizationMean = 0.01 # Default from template
)

# CohortMethodModule -----------------------------------------------------------

# Study periods: From analysis specifications, studyStartDate and studyEndDate are null,
# indicating no restriction. Represented as NA_character_ in tibble.
studyPeriods <- tibble(
  studyStartDate = c(NA_character_), # YYYYMMDD or NA_character_ for unrestricted
  studyEndDate   = c(NA_character_)  # YYYYMMDD or NA_character_ for unrestricted
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# Settings are from analysis specifications. A label is added for description.
timeAtRisks <- tibble(
  label = c("TAR_1_0_cohort_end"), # Descriptive label for this TAR
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end"),
  minDaysAtRisk = c(1) # From analysis specifications
)

# Propensity Score settings - match on PS
# Settings are from analysis specifications. A label is added for description.
matchOnPsArgsList <- tibble(
  label = c("MatchOnPs_1_0.2_stdLogit"), # Descriptive label for this PS matching
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # From analysis specifications
)

# Propensity Score settings - stratify by PS
# Analysis specifications indicate stratifyByPsArgs is null, so this list is empty.
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
# This section will not execute as stratifyByPsArgsList is empty based on analysis specifications.
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
  # Convert NA_character_ to NULL for createGetDbCohortMethodDataArgs if study period is unrestricted.
  currentStudyStartDate <- if (is.na(studyPeriods$studyStartDate[s])) NULL else studyPeriods$studyStartDate[s]
  currentStudyEndDate <- if (is.na(studyPeriods$studyEndDate[s])) NULL else studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      if (psCfg$method == "match") {
        # Create matching on PS arguments based on analysis specifications.
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default from template
          stratificationColumns = c() # Default from template
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # This branch will not be executed as stratifyByPsArgsList is empty.
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings: Use default settings as per analysis specifications (no specific includes/excludes).
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default from template
      )

      # Define outcomes for the CohortMethod analysis.
      # Includes both outcomes of interest and negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not a simulated outcome
            priorOutcomeLookback = 99999 # Default from template
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
      
      # Define target-comparator-outcome combinations.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude specific covariate concepts.
          # Based on analysis specifications, excludedCovariateConcepts is empty.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Arguments for fetching cohort method data from the database.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Default from template
        studyStartDate = currentStudyStartDate, # From analysis specifications (can be NULL)
        studyEndDate = currentStudyEndDate,     # From analysis specifications (can be NULL)
        maxCohortSize = 0, # From analysis specifications
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # Settings are from analysis specifications.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From analysis specifications
        errorOnHighCorrelation = TRUE, # From analysis specifications
        stopOnError = FALSE, # Default from template (allow Strategus to complete)
        estimator = "att", # Default from template
        prior = Cyclops::createPrior( # Prior settings from analysis specifications
          priorType = "laplace", 
          exclude = c(0), # Default from template
          useCrossValidation = TRUE # From analysis specifications
        ),
        control = Cyclops::createControl( # Control settings from analysis specifications
          noiseLevel = "silent", # From analysis specifications
          cvType = "auto", # From analysis specifications
          seed = 1, # Default from template
          resetCoefficients = TRUE, # From analysis specifications
          tolerance = 2e-07, # From analysis specifications
          cvRepetitions = 10, # From analysis specifications (template was 1)
          startingVariance = 0.01 # From analysis specifications
        )
      )

      # Arguments for computing shared covariate balance.
      # Default settings from template.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default from template
        covariateFilter = NULL # Default from template
      )
      # Arguments for computing covariate balance.
      # Default settings from template.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default from template
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default from template
      )

      # Arguments for fitting the outcome model.
      # Settings are from analysis specifications.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From analysis specifications
        stratified = FALSE, # From analysis specifications (template was TRUE)
        useCovariates = FALSE, # From analysis specifications
        inversePtWeighting = FALSE, # From analysis specifications
        prior = Cyclops::createPrior( # Prior settings from analysis specifications
          priorType = "laplace", 
          useCrossValidation = TRUE # From analysis specifications
        ),
        control = Cyclops::createControl( # Control settings from analysis specifications
          cvType = "auto", # From analysis specifications
          seed = 1, # Default from template
          resetCoefficients = TRUE, # From analysis specifications
          startingVariance = 0.01, # From analysis specifications
          tolerance = 2e-07, # From analysis specifications
          cvRepetitions = 10, # From analysis specifications (template was 1)
          noiseLevel = "quiet" # From analysis specifications
        )
      )
      
      # Arguments for creating the study population.
      # Settings are from analysis specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From analysis specifications
        firstExposureOnly = TRUE, # From analysis specifications
        washoutPeriod = 365, # From analysis specifications
        removeDuplicateSubjects = "keep all", # From analysis specifications (template was "keep first")
        censorAtNewRiskWindow = FALSE, # From analysis specifications (template was TRUE)
        removeSubjectsWithPriorOutcome = TRUE, # From analysis specifications
        priorOutcomeLookback = 99999, # From analysis specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From analysis specifications
        startAnchor = timeAtRisks$startAnchor[t], # From analysis specifications
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From analysis specifications
        endAnchor = timeAtRisks$endAnchor[t], # From analysis specifications
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From analysis specifications (template was 1)
        maxDaysAtRisk = 99999 # Default from template
      )

      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        # Generate a descriptive label for the analysis.
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          if (is.null(currentStudyStartDate)) "Unrestricted" else currentStudyStartDate,
          if (is.null(currentStudyEndDate)) "Unrestricted" else currentStudyEndDate,
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
# Create module specifications for CohortMethod.
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Default from template
  refitPsForEveryOutcome = FALSE, # Default from template
  refitPsForEveryStudyPopulation = FALSE, # Default from template
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default from template
)

# Create the analysis specifications ------------------------------------------
# Initialize empty analysis specifications and add shared resources and module specifications.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path uses the study name "antivegfkidney" from the analysis specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "antivegfkidney", "antivegfkidneyAnalysisSpecification.json")
)