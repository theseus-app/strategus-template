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
# Base URL for the WebAPI instance to retrieve cohort definitions and concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions from WebAPI based on the IDs provided in the analysis specifications.
# These are the original cohort IDs from the WebAPI.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal use within the study package.
# This maps the original WebAPI cohort IDs to simpler, sequential IDs (1, 2, 3)
# for easier reference in the Strategus analysis specification.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Target cohort re-numbered to 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Comparator cohort re-numbered to 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Outcome cohort re-numbered to 3

# Negative control outcomes
# Negative controls are derived from a concept set specified in the analysis specifications.
# The concept set is resolved to individual concepts, which are then treated as outcomes.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # ID for the negative control concept set from analysis specifications
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

# Check for duplicate cohort IDs between study cohorts and negative controls to prevent conflicts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Filter for the re-numbered outcome cohort (ID 3) from the analysis specifications.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window for outcomes, not specified in analysis spec.

# Target and Comparator for the CohortMethod analysis.
# Uses the re-numbered cohort IDs and names from the analysis specifications.
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered target cohort ID
  targetCohortName = "target1", # Name from analysis specifications
  comparatorCohortId = 2, # Re-numbered comparator cohort ID
  comparatorCohortName = "comparator1" # Name from analysis specifications
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study. Based on analysis specifications, no specific concepts are excluded
# in the 'covariateSelection.conceptsToExclude' section (it's null).
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0), # Empty as per analysis specifications
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# Based on analysis specifications, no specific concepts are included
# in the 'covariateSelection.conceptsToInclude' section (it's null).
includedCovariateConcepts <- data.frame(
  conceptId = integer(0), # Empty as per analysis specifications
  conceptName = character(0)
)

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Shared resource for cohort definitions, including target, comparator, and outcome cohorts.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Shared resource for negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default from template
  detectOnDescendants = TRUE # Default from template
)
# Module specifications for CohortGenerator, set to generate statistics.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Module specifications for CohortDiagnostics.
# Runs various diagnostics for all defined cohorts.
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # All re-numbered study cohorts
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

# Study periods are defined in getDbCohortMethodDataArgs in the analysis specifications.
studyPeriods <- tibble(
  studyStartDate = c("20130101"), # YYYYMMDD from analysis specifications
  studyEndDate   = c("20201231")  # YYYYMMDD from analysis specifications
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# Defined in createStudyPopArgs in the analysis specifications.
timeAtRisks <- tibble(
  label = c("TAR_1_730_CS_CS"), # Descriptive label for the time-at-risk window
  riskWindowStart  = c(1), # From analysis specifications
  startAnchor = c("cohort start"), # From analysis specifications
  riskWindowEnd  = c(730), # From analysis specifications
  endAnchor = c("cohort start") # From analysis specifications
)

# Propensity Score settings - match on PS.
# Defined in propensityScoreAdjustment.psSettings in the analysis specifications.
matchOnPsArgsList <- tibble(
  label = c("Match_MaxRatio1_Caliper0.05_StdLogit"), # Descriptive label for PS matching
  maxRatio  = c(1), # From analysis specifications
  caliper = c(0.05), # From analysis specifications
  caliperScale  = c("standardized logit") # From analysis specifications
)

# Propensity Score settings - stratify by PS.
# Based on analysis specifications, 'stratifyByPsArgs' is null, so this list is empty.
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0) # "all" | "target" | "comparator"
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

      # Define PS adjustment arguments based on the current PS configuration.
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

      # Covariate settings for FeatureExtraction.
      # Based on analysis specifications, no specific concepts are included or excluded
      # beyond the default settings.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE, # Default from template
        excludedCovariateConceptIds = excludedCovariateConcepts$conceptId, # Empty vector from analysis spec
        includedCovariateConceptIds = includedCovariateConcepts$conceptId  # Empty vector from analysis spec
      )

      # Combine study outcomes and negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Default from template
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

      # Define target-comparator-outcome combinations.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # No specific concepts to exclude from covariates for T/C,
          # as per analysis specifications.
          # The template's placeholder for target/comparator concept IDs (cmTcList$targetConceptId[i], cmTcList$comparatorConceptId[i])
          # is removed as these are internal cohort IDs, not concept IDs for covariate exclusion.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId # This is an empty vector based on analysis spec
        )
      }

      # Arguments for retrieving cohort method data from the database.
      # Settings are from 'getDbCohortMethodDataArgs' in the analysis specifications.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,   # From analysis specifications
        studyStartDate = studyStartDate, # From studyPeriods loop
        studyEndDate = studyEndDate,     # From studyPeriods loop
        maxCohortSize = 0,               # From analysis specifications
        firstExposureOnly = FALSE,       # From analysis specifications
        washoutPeriod = 0,               # From analysis specifications
        removeDuplicateSubjects = "keep all", # From analysis specifications
        covariateSettings = covariateSettings # Defined above based on covariateSelection
      )

      # Arguments for creating propensity scores.
      # Settings are from 'propensityScoreAdjustment.createPsArgs' in the analysis specifications.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From analysis specifications
        errorOnHighCorrelation = TRUE,    # From analysis specifications
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att", # Default from template
        prior = Cyclops::createPrior(
          priorType = "laplace",          # From analysis specifications (propensityScoreAdjustment.createPsArgs.prior.priorType)
          exclude = c(0),                 # Default from template
          useCrossValidation = TRUE       # From analysis specifications (propensityScoreAdjustment.createPsArgs.prior.useCrossValidation)
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",          # From analysis specifications (propensityScoreAdjustment.createPsArgs.control.noiseLevel)
          cvType = "auto",                # From analysis specifications (propensityScoreAdjustment.createPsArgs.control.cvType)
          seed = 1,                       # Default from template, not specified in analysis spec
          resetCoefficients = TRUE,       # From analysis specifications (propensityScoreAdjustment.createPsArgs.control.resetCoefficients)
          tolerance = 2e-07,              # From analysis specifications (propensityScoreAdjustment.createPsArgs.control.tolerance)
          cvRepetitions = 10,             # From analysis specifications (propensityScoreAdjustment.createPsArgs.control.cvRepetitions)
          startingVariance = 0.01         # From analysis specifications (propensityScoreAdjustment.createPsArgs.control.startingVariance)
        )
      )

      # Arguments for computing shared covariate balance.
      # Default settings from template, not specified in analysis spec.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      # Arguments for computing covariate balance.
      # Default settings from template, not specified in analysis spec.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting the outcome model.
      # Settings are from 'fitOutcomeModelArgs' in the analysis specifications.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",              # From analysis specifications
        stratified = TRUE,              # From analysis specifications
        useCovariates = FALSE,          # From analysis specifications
        inversePtWeighting = FALSE,     # From analysis specifications
        prior = Cyclops::createPrior(
          priorType = "laplace",          # From analysis specifications (fitOutcomeModelArgs.prior.priorType)
          useCrossValidation = TRUE       # From analysis specifications (fitOutcomeModelArgs.prior.useCrossValidation)
        ),
        control = Cyclops::createControl(
          cvType = "auto",                # From analysis specifications (fitOutcomeModelArgs.control.cvType)
          seed = 1,                       # Default from template, not specified in analysis spec
          resetCoefficients = TRUE,       # From analysis specifications (fitOutcomeModelArgs.control.resetCoefficients)
          startingVariance = 0.01,        # From analysis specifications (fitOutcomeModelArgs.control.startingVariance)
          tolerance = 2e-07,              # From analysis specifications (fitOutcomeModelArgs.control.tolerance)
          cvRepetitions = 10,             # From analysis specifications (fitOutcomeModelArgs.control.cvRepetitions)
          noiseLevel = "quiet"            # From analysis specifications (fitOutcomeModelArgs.control.noiseLevel)
        )
      )

      # Arguments for creating the study population.
      # Settings are from 'createStudyPopArgs' in the analysis specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,       # From analysis specifications
        firstExposureOnly = FALSE,            # From analysis specifications
        washoutPeriod = 0,                    # From analysis specifications
        removeDuplicateSubjects = "keep all", # From analysis specifications
        censorAtNewRiskWindow = FALSE,        # From analysis specifications
        removeSubjectsWithPriorOutcome = TRUE, # From analysis specifications
        priorOutcomeLookback = 99999,         # From analysis specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From timeAtRisks loop
        startAnchor = timeAtRisks$startAnchor[t],         # From timeAtRisks loop
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],     # From timeAtRisks loop
        endAnchor = timeAtRisks$endAnchor[t],             # From timeAtRisks loop
        minDaysAtRisk = 1,                    # From analysis specifications (timeAtRisks.minDaysAtRisk)
        maxDaysAtRisk = 99999                 # Default from template, not specified in analysis spec
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
# Module specifications for CohortMethod, including all defined analyses and TCOs.
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Default from template
  refitPsForEveryOutcome = FALSE, # Default from template
  refitPsForEveryStudyPopulation = FALSE, # Default from template
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default from template
)

# Create the analysis specifications ------------------------------------------
# Combine all shared resources and module specifications into a single analysis specification object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the complete analysis specifications to a JSON file.
# The file path uses the study name "glp1radepression" from the analysis specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "glp1radepression", "glp1radepressionAnalysisSpecification.json")
)