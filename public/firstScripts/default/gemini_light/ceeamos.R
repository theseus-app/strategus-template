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

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
# Base URL for the WebAPI, using a common OHDSI Atlas instance as a placeholder.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Cohort IDs are extracted from Analysis Specifications: cohortDefinitions
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1 (from Analysis Specifications: cohortDefinitions.targetCohort.id)
    1794132, # Comparator: comparator1 (from Analysis Specifications: cohortDefinitions.comparatorCohort.id)
    1794131  # Outcome: outcome1 (from Analysis Specifications: cohortDefinitions.outcomeCohort[0].id)
  ),
  generateStats = TRUE
)

# Re-number cohorts to internal study IDs for consistency and to avoid conflicts
# Target cohort re-numbered to 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortName <- "target1" # From Analysis Specifications: cohortDefinitions.targetCohort.name
# Comparator cohort re-numbered to 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortName <- "comparator1" # From Analysis Specifications: cohortDefinitions.comparatorCohort.name
# Outcome cohort re-numbered to 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortName <- "outcome1" # From Analysis Specifications: cohortDefinitions.outcomeCohort[0].name

# Negative control outcomes
# Concept set ID for negative controls from Analysis Specifications: negativeControlConceptSet.id
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # From Analysis Specifications: negativeControlConceptSet.id
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
  # Negative control cohort IDs start from 101 to avoid conflicts with T/C/O cohorts
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)


if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes:
# Filter for the outcome cohort (re-numbered to 3)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not specified in analysis specifications

# Target and Comparator for the CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered target cohort ID
  targetCohortName = "target1", # From Analysis Specifications: cohortDefinitions.targetCohort.name
  comparatorCohortId = 2, # Re-numbered comparator cohort ID
  comparatorCohortName = "comparator1" # From Analysis Specifications: cohortDefinitions.comparatorCohort.name
)

# For the CohortMethod LSPS we'll need to exclude specific concepts.
# The analysis specifications (covariateSelection.conceptsToExclude) indicate no specific concepts to exclude.
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(0),
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# The analysis specifications (covariateSelection.conceptsToInclude) indicate no specific concepts to include.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default, not specified in analysis specifications
  detectOnDescendants = TRUE # Default, not specified in analysis specifications
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Default, not specified in analysis specifications
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE, # Default, not specified in analysis specifications
  runIncludedSourceConcepts = TRUE, # Default, not specified in analysis specifications
  runOrphanConcepts = TRUE, # Default, not specified in analysis specifications
  runTimeSeries = FALSE, # Default, not specified in analysis specifications
  runVisitContext = TRUE, # Default, not specified in analysis specifications
  runBreakdownIndexEvents = TRUE, # Default, not specified in analysis specifications
  runIncidenceRate = TRUE, # Default, not specified in analysis specifications
  runCohortRelationship = TRUE, # Default, not specified in analysis specifications
  runTemporalCohortCharacterization = TRUE, # Default, not specified in analysis specifications
  minCharacterizationMean = 0.01 # Default, not specified in analysis specifications
)

# CohortMethodModule -----------------------------------------------------------

# If you are not restricting your study to a specific time window,
# please make these strings empty.
# From Analysis Specifications: getDbCohortMethodDataArgs.studyPeriods
studyPeriods <- tibble(
  studyStartDate = c(""), # YYYYMMDD (empty string means no restriction)
  studyEndDate   = c("")  # YYYYMMDD (empty string means no restriction)
)

# Time-at-risks (TARs) for the outcomes of interest in your study
# From Analysis Specifications: createStudyPopArgs.timeAtRisks
timeAtRisks <- tibble(
  label = c("TAR_1_CS_0_CE", "TAR_1_CS_99999_CS"), # Custom labels for clarity
  riskWindowStart  = c(1, 1), # From Analysis Specifications: createStudyPopArgs.timeAtRisks[].riskWindowStart
  startAnchor = c("cohort start", "cohort start"), # From Analysis Specifications: createStudyPopArgs.timeAtRisks[].startAnchor
  riskWindowEnd  = c(0, 99999), # From Analysis Specifications: createStudyPopArgs.timeAtRisks[].riskWindowEnd
  endAnchor = c("cohort end", "cohort start"), # From Analysis Specifications: createStudyPopArgs.timeAtRisks[].endAnchor
  minDaysAtRisk = c(1, 1) # From Analysis Specifications: createStudyPopArgs.timeAtRisks[].minDaysAtRisk
)

# Propensity Score settings - match on PS
# From Analysis Specifications: propensityScoreAdjustment.psSettings[0].matchOnPsArgs
matchOnPsArgsList <- tibble(
  label = c("Match_MaxRatio10_Caliper0.2_StdLogit"), # Custom label for clarity
  maxRatio  = c(10), # From Analysis Specifications: propensityScoreAdjustment.psSettings[0].matchOnPsArgs.maxRatio
  caliper = c(0.2), # From Analysis Specifications: propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliper
  caliperScale  = c("standardized logit") # From Analysis Specifications: propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliperScale
)

# Propensity Score settings - stratify by PS
# From Analysis Specifications: propensityScoreAdjustment.psSettings[0].stratifyByPsArgs (which is null)
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
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

      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default, not specified in analysis specifications
          stratificationColumns = c() # Default, not specified in analysis specifications
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default, not specified in analysis specifications
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings for feature extraction
      # From Analysis Specifications: covariateSelection (empty, so using default)
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default, not specified in analysis specifications
      )

      # Combine outcome cohorts and negative control outcomes
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not applicable for observed outcomes
            priorOutcomeLookback = 99999 # Default, not specified in analysis specifications
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

      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # No specific concepts to exclude were provided in the analysis specifications
          # (covariateSelection.conceptsToExclude is empty).
          # The default covariate settings (createDefaultCovariateSettings) handle
          # excluding descendants of the target/comparator cohorts if addDescendantsToExclude is TRUE.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        # From Analysis Specifications: getDbCohortMethodDataArgs.restrictToCommonPeriod
        restrictToCommonPeriod = FALSE,
        # From current studyPeriods iteration
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        # From Analysis Specifications: getDbCohortMethodDataArgs.maxCohortSize
        maxCohortSize = 0,
        # From Analysis Specifications: getDbCohortMethodDataArgs.firstExposureOnly
        firstExposureOnly = FALSE,
        # From Analysis Specifications: getDbCohortMethodDataArgs.washoutPeriod
        washoutPeriod = 0,
        # From Analysis Specifications: getDbCohortMethodDataArgs.removeDuplicateSubjects
        removeDuplicateSubjects = "keep first",
        # Covariate settings are defined globally for the CM module
        covariateSettings = covariateSettings
      )

      createPsArgs = CohortMethod::createCreatePsArgs(
        # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        maxCohortSizeForFitting = 250000,
        # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        errorOnHighCorrelation = TRUE,
        # Setting to FALSE to allow Strategus complete all CM operations;
        # when we cannot fit a model, the equipoise diagnostic should fail
        stopOnError = FALSE,
        # Default in template, not explicitly in analysis specifications
        estimator = "att",
        # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.prior
        prior = Cyclops::createPrior(
          priorType = "laplace", # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.prior.priorType
          exclude = c(0), # Default in template, not explicitly in analysis specifications
          useCrossValidation = TRUE # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.prior.useCrossValidation
        ),
        # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control
        control = Cyclops::createControl(
          noiseLevel = "silent", # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.noiseLevel
          cvType = "auto", # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.cvType
          seed = 1, # Default in template, not explicitly in analysis specifications
          resetCoefficients = TRUE, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.resetCoefficients
          tolerance = 2e-07, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.tolerance
          cvRepetitions = 10, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.cvRepetitions
          startingVariance = 0.01, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.startingVariance
          fold = 10 # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.fold
        )
      )

      # Default settings for covariate balance computation, not specified in analysis specifications
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        # From Analysis Specifications: fitOutcomeModelArgs.modelType
        modelType = "cox",
        # From Analysis Specifications: fitOutcomeModelArgs.stratified
        stratified = TRUE,
        # From Analysis Specifications: fitOutcomeModelArgs.useCovariates
        useCovariates = FALSE,
        # From Analysis Specifications: fitOutcomeModelArgs.inversePtWeighting
        inversePtWeighting = FALSE,
        # From Analysis Specifications: fitOutcomeModelArgs.prior
        prior = Cyclops::createPrior(
          priorType = "laplace", # From Analysis Specifications: fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE # From Analysis Specifications: fitOutcomeModelArgs.prior.useCrossValidation
        ),
        # From Analysis Specifications: fitOutcomeModelArgs.control
        control = Cyclops::createControl(
          cvType = "auto", # From Analysis Specifications: fitOutcomeModelArgs.control.cvType
          seed = 1, # Default in template, not explicitly in analysis specifications
          resetCoefficients = TRUE, # From Analysis Specifications: fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01, # From Analysis Specifications: fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07, # From Analysis Specifications: fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10, # From Analysis Specifications: fitOutcomeModelArgs.control.cvRepetitions
          noiseLevel = "quiet", # From Analysis Specifications: fitOutcomeModelArgs.control.noiseLevel
          fold = 10 # From Analysis Specifications: fitOutcomeModelArgs.control.fold
        )
      )

      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        # From Analysis Specifications: createStudyPopArgs.restrictToCommonPeriod
        restrictToCommonPeriod = FALSE,
        # From Analysis Specifications: createStudyPopArgs.firstExposureOnly
        firstExposureOnly = FALSE,
        # From Analysis Specifications: createStudyPopArgs.washoutPeriod
        washoutPeriod = 0,
        # From Analysis Specifications: createStudyPopArgs.removeDuplicateSubjects
        removeDuplicateSubjects = "keep all",
        # From Analysis Specifications: createStudyPopArgs.censorAtNewRiskWindow
        censorAtNewRiskWindow = FALSE,
        # From Analysis Specifications: createStudyPopArgs.removeSubjectsWithPriorOutcome
        removeSubjectsWithPriorOutcome = TRUE,
        # From Analysis Specifications: createStudyPopArgs.priorOutcomeLookBack
        priorOutcomeLookback = 99999,
        # From current timeAtRisks iteration
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        # Not specified in analysis specifications, using default
        maxDaysAtRisk = 99999
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
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Not specified in analysis specifications
  refitPsForEveryOutcome = FALSE, # Default, not specified in analysis specifications
  refitPsForEveryStudyPopulation = FALSE, # Default, not specified in analysis specifications
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default, not specified in analysis specifications
)

# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file
# The file path uses the study name from Analysis Specifications: name
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ceeamos", "ceeamosAnalysisSpecification.json")
)