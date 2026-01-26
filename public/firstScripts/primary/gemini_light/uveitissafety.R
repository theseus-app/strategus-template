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
# Base URL for OHDSI WebAPI. This is a placeholder and should be updated
# if a different WebAPI instance is used.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Extracting cohort IDs and names from the <Analysis Specifications> section.
# Target Cohort: ID 1794126, Name "target1"
# Comparator Cohort: ID 1794132, Name "comparator1"
# Outcome Cohort: ID 1794131, Name "outcome1"
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE # Generate cohort statistics during generation
)

# Re-number cohorts to a simplified scheme (1, 2, 3) for internal use in the study.
# This makes it easier to refer to target, comparator, and outcome consistently
# within the Strategus analysis.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Update cohort names for the re-numbered cohorts for clarity.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1,]$cohortName <- "target1"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 2,]$cohortName <- "comparator1"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 3,]$cohortName <- "outcome1"

# Negative control outcomes
# Extracting negative control concept set ID and name from <Analysis Specifications>.
# Negative Control Concept Set: ID 1888110, Name "negative"
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
  # Assign unique cohort IDs starting from 101 to avoid collision with
  # target/comparator/outcome cohort IDs (1, 2, 3).
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs to prevent errors in Strategus.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes:
# Filtering for the outcome cohort (re-numbered ID 3) from the cohortDefinitionSet.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # cleanWindow is not explicitly specified in <Analysis Specifications>,
  # using a default value of 365 days.
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Populating cmTcList with the re-numbered target (ID 1) and comparator (ID 2) cohorts.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# For the CohortMethod LSPS we'll need to exclude specific concepts from covariates.
# <Analysis Specifications> -> covariateSelection -> conceptsToExclude is empty (null ID, empty name).
# Therefore, this data frame will be empty, meaning no additional concepts are
# explicitly excluded from covariate building beyond the default settings.
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(0),
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# <Analysis Specifications> -> covariateSelection -> conceptsToInclude is empty.
# This section remains commented out as no specific concepts are defined for inclusion.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Create shared resource specifications for cohort definitions.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Create shared resource specifications for negative control outcomes.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default from template: detect the first occurrence of the negative control outcome.
  detectOnDescendants = TRUE # Default from template: detect on descendants of the negative control concepts.
)
# Create module specifications for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Default from template: generate cohort statistics.
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Create module specifications for CohortDiagnostics.
# Using default settings from the template, as no specific CohortDiagnostics
# settings are provided in <Analysis Specifications>.
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
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

# Study Periods:
# From <Analysis Specifications> -> getDbCohortMethodDataArgs -> studyPeriods.
# Since studyStartDate and studyEndDate are null in the specifications,
# this tibble will be empty, indicating no restriction on study period.
studyPeriods <- tibble(
  studyStartDate = character(0), # YYYYMMDD
  studyEndDate   = character(0)  # YYYYMMDD
)

# Time-at-risks (TARs) for the outcomes of interest in your study
# From <Analysis Specifications> -> createStudyPopArgs -> timeAtRisks.
# Populating with the single entry from the specifications.
timeAtRisks <- tibble(
  label = c("TAR 1-0"), # Custom label for this time-at-risk window for description.
  riskWindowStart  = c(1), # From <Analysis Specifications> -> riskWindowStart
  startAnchor = c("cohort start"), # From <Analysis Specifications> -> startAnchor
  riskWindowEnd  = c(0), # From <Analysis Specifications> -> riskWindowEnd
  endAnchor = c("cohort end"), # From <Analysis Specifications> -> endAnchor
  minDaysAtRisk = c(1) # From <Analysis Specifications> -> minDaysAtRisk
)

# Propensity Score settings - match on PS
# From <Analysis Specifications> -> propensityScoreAdjustment -> psSettings -> matchOnPsArgs.
# Populating with the single entry from the specifications.
matchOnPsArgsList <- tibble(
  label = c("Match 10:1 Caliper 0.2 SL"), # Custom label for this PS matching setting.
  maxRatio  = c(10), # From <Analysis Specifications> -> maxRatio
  caliper = c(0.2), # From <Analysis Specifications> -> caliper
  caliperScale  = c("standardized logit") # From <Analysis Specifications> -> caliperScale
)

# Propensity Score settings - stratify by PS
# From <Analysis Specifications> -> propensityScoreAdjustment -> psSettings -> stratifyByPsArgs.
# Since stratifyByPsArgs is null in the specifications, this tibble will be empty.
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = numeric(0),
  baseSelection = character(0) # "all" | "target" | "comparator"
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# If a data frame for "match on PS" exists and has rows, convert each row to a config.
# This block will execute as matchOnPsArgsList is populated.
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
# This block will not execute as stratifyByPsArgsList is empty based on <Analysis Specifications>.
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

# Loop through study periods. If studyPeriods is empty, the loop runs once
# with empty studyStartDate/studyEndDate.
for (s in seq_len(max(1, nrow(studyPeriods)))) {
  studyStartDate <- if (nrow(studyPeriods) > 0) studyPeriods$studyStartDate[s] else ""
  studyEndDate <- if (nrow(studyPeriods) > 0) studyPeriods$studyEndDate[s] else ""

  # Loop through time-at-risk definitions.
  for (t in seq_len(nrow(timeAtRisks))) {

    # Loop through propensity score adjustment configurations.
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

      # Covariate settings: Using default settings as per template.
      # <Analysis Specifications> has empty covariateSelection, so no custom
      # covariate settings are applied here.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default from template
      )

      # Combine outcome cohorts and negative control outcomes into a single list.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not specified in <Analysis Specifications>, using NA.
            priorOutcomeLookback = 99999 # Default from template
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # Default for negative controls
          )
        })
      )

      # Create target-comparator-outcomes list for the CohortMethod analysis.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Excluded covariate concept IDs:
          # <Analysis Specifications> -> covariateSelection -> conceptsToExclude is empty.
          # The template included cmTcList$targetConceptId[i] and cmTcList$comparatorConceptId[i],
          # but these are not defined in cmTcList based on <Analysis Specifications>.
          # Therefore, only excludedCovariateConcepts$conceptId (which is an empty vector) is used.
          excludedCovariateConceptIds = c(
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # getDbCohortMethodDataArgs: Populated from <Analysis Specifications> -> getDbCohortMethodDataArgs.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Default from template
        studyStartDate = studyStartDate, # From loop (empty string if no study periods specified)
        studyEndDate = studyEndDate,     # From loop (empty string if no study periods specified)
        maxCohortSize = 0, # From <Analysis Specifications> -> getDbCohortMethodDataArgs -> maxCohortSize (0 means no restriction)
        covariateSettings = covariateSettings
      )

      # createPsArgs: Populated from <Analysis Specifications> -> propensityScoreAdjustment -> createPsArgs.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From <Analysis Specifications> -> maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From <Analysis Specifications> -> errorOnHighCorrelation
        stopOnError = FALSE, # Default from template: allows Strategus to complete all CM operations even if a model cannot be fitted.
        estimator = "att", # Default from template: Average Treatment effect on the Treated.
        prior = Cyclops::createPrior( # From <Analysis Specifications> -> prior (for PS model)
          priorType = "laplace", # From <Analysis Specifications> -> priorType
          exclude = c(0), # Default from template: Exclude intercept from regularization.
          useCrossValidation = TRUE # From <Analysis Specifications> -> useCrossValidation
        ),
        control = Cyclops::createControl( # From <Analysis Specifications> -> control (for PS model)
          noiseLevel = "silent", # From <Analysis Specifications> -> noiseLevel
          cvType = "auto", # From <Analysis Specifications> -> cvType
          seed = 1, # Default from template: Random seed for reproducibility.
          resetCoefficients = TRUE, # From <Analysis Specifications> -> resetCoefficients
          tolerance = 2e-07, # From <Analysis Specifications> -> tolerance
          cvRepetitions = 10, # From <Analysis Specifications> -> cvRepetitions (template was 1, spec is 10)
          startingVariance = 0.01 # From <Analysis Specifications> -> startingVariance
        )
      )

      # computeSharedCovariateBalanceArgs and computeCovariateBalanceArgs:
      # Using default settings from template. maxCohortSize here refers to the
      # maximum number of subjects to sample for balance computation, not the
      # initial cohort size restriction.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs: Populated from <Analysis Specifications> -> fitOutcomeModelArgs.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From <Analysis Specifications> -> modelType
        stratified = TRUE, # From <Analysis Specifications> -> stratified
        useCovariates = FALSE, # From <Analysis Specifications> -> useCovariates
        inversePtWeighting = FALSE, # From <Analysis Specifications> -> inversePtWeighting
        prior = Cyclops::createPrior( # From <Analysis Specifications> -> prior (for outcome model)
          priorType = "laplace", # From <Analysis Specifications> -> priorType
          useCrossValidation = TRUE # From <Analysis Specifications> -> useCrossValidation
        ),
        control = Cyclops::createControl( # From <Analysis Specifications> -> control (for outcome model)
          cvType = "auto", # From <Analysis Specifications> -> cvType
          seed = 1, # Default from template: Random seed for reproducibility.
          resetCoefficients = TRUE, # From <Analysis Specifications> -> resetCoefficients
          startingVariance = 0.01, # From <Analysis Specifications> -> startingVariance
          tolerance = 2e-07, # From <Analysis Specifications> -> tolerance
          cvRepetitions = 10, # From <Analysis Specifications> -> cvRepetitions (template was 1, spec is 10)
          noiseLevel = "quiet" # From <Analysis Specifications> -> noiseLevel
        )
      )

      # createStudyPopArgs: Populated from <Analysis Specifications> -> createStudyPopArgs.
      # Risk window parameters (riskWindowStart, startAnchor, etc.) come from the timeAtRisks loop.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE, # From <Analysis Specifications> -> restrictToCommonPeriod
        firstExposureOnly = TRUE, # From <Analysis Specifications> -> firstExposureOnly
        washoutPeriod = 365, # From <Analysis Specifications> -> washoutPeriod
        removeDuplicateSubjects = "keep all", # From <Analysis Specifications> -> removeDuplicateSubjects
        censorAtNewRiskWindow = TRUE, # From <Analysis Specifications> -> censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # From <Analysis Specifications> -> removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # From <Analysis Specifications> -> priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From timeAtRisks loop
        startAnchor = timeAtRisks$startAnchor[t], # From timeAtRisks loop
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From timeAtRisks loop
        endAnchor = timeAtRisks$endAnchor[t], # From timeAtRisks loop
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From timeAtRisks loop
        maxDaysAtRisk = 99999 # Default from template: Maximum days at risk.
      )


      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          # If studyPeriods is empty, studyStartDate/EndDate will be empty strings,
          # so replace with "Any" for a more readable description.
          ifelse(studyStartDate == "", "Any", studyStartDate),
          ifelse(studyEndDate == "", "Any", studyEndDate),
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
# Using default settings for analysesToExclude, refitPsForEveryOutcome,
# refitPsForEveryStudyPopulation, and cmDiagnosticThresholds.
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the analysis specifications ------------------------------------------
# Initialize an empty analysis specifications object and add shared resources
# and module specifications.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path is updated to reflect the study name "uveitissafety"
# from <Analysis Specifications> -> name.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "uveitissafety", "uveitissafetyAnalysisSpecification.json")
)