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

# --- Analysis Specifications from JSON ---
# These values are extracted from the provided JSON analysis specifications
# to configure the Strategus modules.

# General study name
studyName <- "uveitissafety"

# Cohort definitions from JSON
targetCohortId_json <- 1794126
targetCohortName_json <- "target1"
comparatorCohortId_json <- 1794132
comparatorCohortName_json <- "comparator1"
outcomeCohortId_json <- 1794131
outcomeCohortName_json <- "outcome1"

# Negative control concept set from JSON
negativeControlConceptSetId_json <- 1888110
negativeControlConceptSetName_json <- "negative"

# Covariate selection from JSON (Note: conceptsToInclude/Exclude are empty in spec)
conceptsToExclude_json <- data.frame(id = NULL, name = character(0)) # Empty data frame as per spec
conceptsToInclude_json <- data.frame(id = NULL, name = character(0)) # Empty data frame as per spec

# getDbCohortMethodDataArgs from JSON
getDbCohortMethodDataArgs_json <- list(
  studyPeriods = list(
    list(studyStartDate = "", studyEndDate = "")
  ),
  maxCohortSize = 0,
  restrictToCommonPeriod = TRUE,
  firstExposureOnly = FALSE,
  washoutPeriod = 0,
  removeDuplicateSubjects = "keep all"
)

# createStudyPopArgs from JSON
createStudyPopArgs_json <- list(
  restrictToCommonPeriod = FALSE,
  firstExposureOnly = FALSE,
  washoutPeriod = 0,
  removeDuplicateSubjects = "keep all",
  censorAtNewRiskWindow = FALSE,
  removeSubjectsWithPriorOutcome = TRUE,
  priorOutcomeLookBack = 99999,
  timeAtRisks = list(
    list(riskWindowStart = 1, startAnchor = "cohort start", riskWindowEnd = 0, endAnchor = "cohort end", minDaysAtRisk = 1),
    list(riskWindowStart = 1, startAnchor = "cohort start", riskWindowEnd = 99999, endAnchor = "cohort start", minDaysAtRisk = 1)
  )
)

# propensityScoreAdjustment from JSON
propensityScoreAdjustment_json <- list(
  psSettings = list(
    list(matchOnPsArgs = list(maxRatio = 10, caliper = 0.2, caliperScale = "standardized logit"), stratifyByPsArgs = NULL),
    list(matchOnPsArgs = list(maxRatio = 1, caliper = 0.2, caliperScale = "standardized logit"), stratifyByPsArgs = NULL)
  ),
  createPsArgs = list(
    maxCohortSizeForFitting = 250000,
    errorOnHighCorrelation = TRUE,
    prior = list(priorType = "laplace", useCrossValidation = TRUE),
    control = list(tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10, noiseLevel = "silent", resetCoefficients = TRUE, startingVariance = 0.01)
  )
)

# fitOutcomeModelArgs from JSON
fitOutcomeModelArgs_json <- list(
  modelType = "cox",
  stratified = TRUE,
  useCovariates = FALSE,
  inversePtWeighting = FALSE,
  prior = list(priorType = "laplace", useCrossValidation = TRUE),
  control = list(tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10, noiseLevel = "quiet", resetCoefficients = TRUE, startingVariance = 0.01)
)

# --- End of Analysis Specifications from JSON ---


# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
# Base URL for OHDSI WebAPI to retrieve cohort definitions
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions for Target, Comparator, and Outcome
# These are fetched from WebAPI using their original IDs from the JSON specification.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    targetCohortId_json,   # Target Cohort ID
    comparatorCohortId_json, # Comparator Cohort ID
    outcomeCohortId_json     # Outcome Cohort ID
  ),
  generateStats = TRUE
)

# Re-number cohorts to standard internal Strategus IDs (1 for Target, 2 for Comparator, 3 for Outcome)
# This mapping ensures consistent referencing within the analysis.
cohortDefinitionSet[cohortDefinitionSet$cohortId == targetCohortId_json,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == targetCohortId_json,]$cohortName <- targetCohortName_json
cohortDefinitionSet[cohortDefinitionSet$cohortId == comparatorCohortId_json,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == comparatorCohortId_json,]$cohortName <- comparatorCohortName_json
cohortDefinitionSet[cohortDefinitionSet$cohortId == outcomeCohortId_json,]$cohortId <- 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == outcomeCohortId_json,]$cohortName <- outcomeCohortName_json

# Negative control outcomes
# Resolve the negative control concept set from WebAPI into a set of concepts.
# These concepts will be used to generate negative control outcome cohorts.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = negativeControlConceptSetId_json, # Negative control concept set ID from JSON
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
  # Assign unique cohort IDs for negative controls, starting from 101 to avoid conflict with T/C/O (1, 2, 3)
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs between the main cohorts and negative controls
# This prevents potential errors during analysis execution.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts for specific analysis types ---------------
# Outcomes: Filtering for the primary outcome (re-numbered ID 3)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not specified in JSON

# Target and Comparator for the CohortMethod analysis
# Uses the re-numbered internal IDs (1 and 2)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = targetCohortName_json,
  comparatorCohortId = 2,
  comparatorCohortName = comparatorCohortName_json
)

# For the CohortMethod LSPS, we might need to exclude specific drugs/concepts.
# The JSON 'conceptsToExclude' and 'conceptsToInclude' are empty.
# If specific concept IDs were provided, they would be populated here.
excludedCovariateConcepts <- data.frame(
  conceptId = as.numeric(c()), # Empty as per JSON 'conceptsToExclude'
  conceptName = character(0)
)

# CohortGeneratorModule --------------------------------------------------------
# Initializes the CohortGeneratorModule to create shared resources and module specifications.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Defines shared resource for primary cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Defines shared resource for negative control outcome cohorts
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default from template
  detectOnDescendants = TRUE # Default from template
)
# Specifies the CohortGenerator module, including generating cohort statistics.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# Initializes the CohortDiagnosticsModule.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Specifies the CohortDiagnostics module to run various diagnostics for all defined cohorts.
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # Run diagnostics for all primary cohorts
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

# Study Periods for getDbCohortMethodDataArgs
# Populated from JSON 'getDbCohortMethodDataArgs.studyPeriods'.
# An empty string for start/end date indicates no restriction on the study period.
studyPeriods <- tibble(
  studyStartDate = as.character(sapply(getDbCohortMethodDataArgs_json$studyPeriods, `[[`, "studyStartDate")),
  studyEndDate   = as.character(sapply(getDbCohortMethodDataArgs_json$studyPeriods, `[[`, "studyEndDate"))
)
# Ensure at least one row if periods are empty but still need to iterate for a single unrestricted analysis
if (nrow(studyPeriods) == 0 && length(getDbCohortMethodDataArgs_json$studyPeriods) > 0) {
    studyPeriods <- tibble(studyStartDate = "", studyEndDate = "")
}


# Time-at-risks (TARs) for the outcomes of interest
# Populated from JSON 'createStudyPopArgs.timeAtRisks'.
timeAtRisks <- tibble(
  label = paste0(
    "RW_S", sapply(createStudyPopArgs_json$timeAtRisks, `[[`, "riskWindowStart"),
    "_SA", sapply(createStudyPopArgs_json$timeAtRisks, `[[`, "startAnchor"),
    "_E", sapply(createStudyPopArgs_json$timeAtRisks, `[[`, "riskWindowEnd"),
    "_EA", sapply(createStudyPopArgs_json$timeAtRisks, `[[`, "endAnchor")
  ),
  riskWindowStart  = as.numeric(sapply(createStudyPopArgs_json$timeAtRisks, `[[`, "riskWindowStart")),
  startAnchor = as.character(sapply(createStudyPopArgs_json$timeAtRisks, `[[`, "startAnchor")),
  riskWindowEnd  = as.numeric(sapply(createStudyPopArgs_json$timeAtRisks, `[[`, "riskWindowEnd")),
  endAnchor = as.character(sapply(createStudyPopArgs_json$timeAtRisks, `[[`, "endAnchor")),
  minDaysAtRisk = as.numeric(sapply(createStudyPopArgs_json$timeAtRisks, `[[`, "minDaysAtRisk"))
)

# Propensity Score settings - match on PS
# Populated from JSON 'propensityScoreAdjustment.psSettings' for 'matchOnPsArgs'.
matchOnPsArgsList <- bind_rows(lapply(propensityScoreAdjustment_json$psSettings, function(ps_setting) {
  if (!is.null(ps_setting$matchOnPsArgs)) {
    data.frame(
      label = paste0("Match_Ratio", ps_setting$matchOnPsArgs$maxRatio, "_Cal", ps_setting$matchOnPsArgs$caliper),
      maxRatio = ps_setting$matchOnPsArgs$maxRatio,
      caliper = ps_setting$matchOnPsArgs$caliper,
      caliperScale = ps_setting$matchOnPsArgs$caliperScale
    )
  } else {
    NULL
  }
}))

# Propensity Score settings - stratify by PS
# Populated from JSON 'propensityScoreAdjustment.psSettings' for 'stratifyByPsArgs'.
# In the provided JSON, stratifyByPsArgs is always null, so this will be empty.
stratifyByPsArgsList <- bind_rows(lapply(propensityScoreAdjustment_json$psSettings, function(ps_setting) {
  if (!is.null(ps_setting$stratifyByPsArgs)) {
    data.frame(
      label = paste0("Strat_N", ps_setting$stratifyByPsArgs$numberOfStrata, "_Base", ps_setting$stratifyByPsArgs$baseSelection),
      numberOfStrata = ps_setting$stratifyByPsArgs$numberOfStrata,
      baseSelection = ps_setting$stratifyByPsArgs$baseSelection
    )
  } else {
    NULL
  }
}))


# Build a single PS configuration list (each entry has: method, label, params)
# This structure allows iterating through different PS adjustment methods (matching or stratification).
psConfigList <- list()

# If 'match on PS' settings exist, convert each row to a config entry
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

# If 'stratify by PS' settings exist, convert each row to a config entry
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

# Iterate through all analysis setting combinations to create a list of CohortMethod analyses
cmAnalysisList <- list()
analysisId <- 1

# Loop through each defined study period (from getDbCohortMethodDataArgs)
for (s_idx in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s_idx]
  studyEndDate <- studyPeriods$studyEndDate[s_idx]

  # Loop through each defined Time-At-Risk (TAR) window (from createStudyPopArgs)
  for (t_idx in seq_len(nrow(timeAtRisks))) {

    # Loop through each defined Propensity Score (PS) adjustment configuration
    for (p_idx in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p_idx]]
      
      # Determine PS adjustment method (matching or stratification)
      matchOnPsArgs <- NULL
      stratifyByPsArgs <- NULL
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default from template
          stratificationColumns = c() # Default from template
        )
      } else if (psCfg$method == "stratify") {
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default from template
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Define covariate settings using default FeatureExtraction settings.
      # The JSON did not specify custom conceptsToInclude/Exclude for this.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default from template
      )

      # Create a list of outcomes, including outcomes of interest and negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # For real outcomes, true effect size is unknown
            priorOutcomeLookback = createStudyPopArgs_json$priorOutcomeLookBack # From JSON
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

      # Create target-comparator-outcome combinations for the analysis.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude specific concept IDs. If target/comparator are defined by specific drugs,
          # their concept IDs would typically be excluded here to prevent confounding.
          # As per JSON 'covariateSelection.conceptsToExclude' is empty,
          # we only pass the `excludedCovariateConcepts$conceptId` which is an empty vector.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Arguments for retrieving data from the CDM database.
      # Populated from JSON 'getDbCohortMethodDataArgs_json'.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = getDbCohortMethodDataArgs_json$restrictToCommonPeriod, # From JSON
        studyStartDate = studyStartDate, # From current loop iteration
        studyEndDate = studyEndDate, # From current loop iteration
        maxCohortSize = getDbCohortMethodDataArgs_json$maxCohortSize, # From JSON
        firstExposureOnly = getDbCohortMethodDataArgs_json$firstExposureOnly, # From JSON
        washoutPeriod = getDbCohortMethodDataArgs_json$washoutPeriod, # From JSON
        removeDuplicateSubjects = getDbCohortMethodDataArgs_json$removeDuplicateSubjects, # From JSON
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # Populated from JSON 'propensityScoreAdjustment.createPsArgs'.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = propensityScoreAdjustment_json$createPsArgs$maxCohortSizeForFitting, # From JSON
        errorOnHighCorrelation = propensityScoreAdjustment_json$createPsArgs$errorOnHighCorrelation, # From JSON
        stopOnError = FALSE, # Default from template; allows Strategus to complete other operations if PS model fails
        estimator = "att", # Default from template
        prior = Cyclops::createPrior( # Cyclops prior settings from JSON
          priorType = propensityScoreAdjustment_json$createPsArgs$prior$priorType,
          exclude = c(0), # Default from template
          useCrossValidation = propensityScoreAdjustment_json$createPsArgs$prior$useCrossValidation
        ),
        control = Cyclops::createControl( # Cyclops control settings from JSON
          noiseLevel = propensityScoreAdjustment_json$createPsArgs$control$noiseLevel,
          cvType = propensityScoreAdjustment_json$createPsArgs$control$cvType,
          seed = 1, # Default from template
          resetCoefficients = propensityScoreAdjustment_json$createPsArgs$control$resetCoefficients,
          tolerance = propensityScoreAdjustment_json$createPsArgs$control$tolerance,
          cvRepetitions = propensityScoreAdjustment_json$createPsArgs$control$cvRepetitions,
          startingVariance = propensityScoreAdjustment_json$createPsArgs$control$startingVariance
        )
      )

      # Arguments for computing shared covariate balance (e.g., for equipoise diagnostics).
      # Not specified in JSON, using template defaults.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default from template
        covariateFilter = NULL # Default from template
      )
      # Arguments for computing overall covariate balance.
      # Not specified in JSON, using template defaults.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default from template
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default from template
      )

      # Arguments for fitting the outcome model.
      # Populated from JSON 'fitOutcomeModelArgs_json'.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = fitOutcomeModelArgs_json$modelType, # From JSON
        stratified = fitOutcomeModelArgs_json$stratified, # From JSON
        useCovariates = fitOutcomeModelArgs_json$useCovariates, # From JSON
        inversePtWeighting = fitOutcomeModelArgs_json$inversePtWeighting, # From JSON
        prior = Cyclops::createPrior( # Cyclops prior settings from JSON
          priorType = fitOutcomeModelArgs_json$prior$priorType,
          useCrossValidation = fitOutcomeModelArgs_json$prior$useCrossValidation
        ),
        control = Cyclops::createControl( # Cyclops control settings from JSON
          cvType = fitOutcomeModelArgs_json$control$cvType,
          seed = 1, # Default from template
          resetCoefficients = fitOutcomeModelArgs_json$control$resetCoefficients,
          startingVariance = fitOutcomeModelArgs_json$control$startingVariance,
          tolerance = fitOutcomeModelArgs_json$control$tolerance,
          cvRepetitions = fitOutcomeModelArgs_json$control$cvRepetitions,
          noiseLevel = fitOutcomeModelArgs_json$control$noiseLevel
        )
      )

      # Arguments for creating the study population.
      # Populated from JSON 'createStudyPopArgs_json' and current TAR iteration.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = createStudyPopArgs_json$restrictToCommonPeriod, # From JSON
        firstExposureOnly = createStudyPopArgs_json$firstExposureOnly, # From JSON
        washoutPeriod = createStudyPopArgs_json$washoutPeriod, # From JSON
        removeDuplicateSubjects = createStudyPopArgs_json$removeDuplicateSubjects, # From JSON
        censorAtNewRiskWindow = createStudyPopArgs_json$censorAtNewRiskWindow, # From JSON
        removeSubjectsWithPriorOutcome = createStudyPopArgs_json$removeSubjectsWithPriorOutcome, # From JSON
        priorOutcomeLookback = createStudyPopArgs_json$priorOutcomeLookBack, # From JSON
        riskWindowStart = timeAtRisks$riskWindowStart[t_idx], # From current TAR iteration
        startAnchor = timeAtRisks$startAnchor[t_idx], # From current TAR iteration
        riskWindowEnd = timeAtRisks$riskWindowEnd[t_idx], # From current TAR iteration
        endAnchor = timeAtRisks$endAnchor[t_idx], # From current TAR iteration
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t_idx], # From current TAR iteration
        maxDaysAtRisk = 99999 # Default from template, not in JSON
      )

      # Append the current CohortMethod analysis settings to the list.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(studyStartDate == "", "unrestricted", studyStartDate), # Handle empty start date
          ifelse(studyEndDate == "", "unrestricted", studyEndDate),   # Handle empty end date
          timeAtRisks$label[t_idx],
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

# Initializes the CohortMethodModule.
cmModuleSettingsCreator <- CohortMethodModule$new()
# Creates module specifications for CohortMethod, including all defined analyses and TCO combinations.
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Not specified in JSON
  refitPsForEveryOutcome = FALSE, # Default from template
  refitPsForEveryStudyPopulation = FALSE, # Default from template
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default from template
)

# Create the overall analysis specifications ------------------------------------------
# Combine all shared resources and module specifications into a single Strategus analysis specification.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the complete analysis specifications to a JSON file.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", studyName, paste0(studyName, "AnalysisSpecification.json"))
)