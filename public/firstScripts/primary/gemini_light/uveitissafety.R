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
# Base URL for OHDSI WebAPI, kept as per template.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Extracting cohort IDs from Analysis Specifications -> cohortDefinitions
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: Analysis Specifications -> cohortDefinitions -> targetCohort -> id
    1794132, # Comparator: Analysis Specifications -> cohortDefinitions -> comparatorCohort -> id
    1794131  # Outcome: Analysis Specifications -> cohortDefinitions -> outcomeCohort[0] -> id
  ),
  generateStats = TRUE
)

# Re-number cohorts to generic IDs (1 for Target, 2 for Comparator, 3 for Outcome)
# This mapping helps in standardizing references within the script.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Target cohort ID re-numbered to 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Comparator cohort ID re-numbered to 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Outcome cohort ID re-numbered to 3

# Negative control outcomes
# Extracting negative control concept set ID from Analysis Specifications -> negativeControlConceptSet -> id
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Analysis Specifications -> negativeControlConceptSet -> id
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
  mutate(cohortId = row_number() + 100) %>% # Assigning unique IDs starting from 101 for negative controls
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs to prevent conflicts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes:
# Filtering for the re-numbered outcome cohort (ID 3)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Corresponds to the re-numbered outcome cohort
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window as per template

# Target and Comparator for the CohortMethod analysis
# Using the re-numbered target (ID 1) and comparator (ID 2) cohorts
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered target cohort ID
  targetCohortName = "target1", # Analysis Specifications -> cohortDefinitions -> targetCohort -> name
  comparatorCohortId = 2, # Re-numbered comparator cohort ID
  comparatorCohortName = "comparator1" # Analysis Specifications -> cohortDefinitions -> comparatorCohort -> name
)

# For the CohortMethod LSPS, if specific concepts need to be excluded as covariates.
# Based on Analysis Specifications -> covariateSelection -> conceptsToExclude.
# Since the specification has `null` for ID and empty string for name, this list is empty.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all.
# Based on Analysis Specifications -> covariateSelection -> conceptsToInclude.
# Since the specification has `null` for ID and empty string for name, this list is empty.
# If populated, a custom FeatureExtraction::createCovariateSettings would be needed.
# includedCovariateConcepts <- data.frame(
#   conceptId = integer(0),
#   conceptName = character(0)
# )


# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default from template
  detectOnDescendants = TRUE # Default from template
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Default from template
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # All cohort IDs (target, comparator, outcome)
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

# Study periods from Analysis Specifications -> getDbCohortMethodDataArgs -> studyPeriods
# If studyStartDate and studyEndDate are null in the spec, the tibble is empty, indicating no restriction.
studyPeriods <- tibble(
  studyStartDate = c(), # YYYYMMDD, from Analysis Specifications -> getDbCohortMethodDataArgs -> studyPeriods[0] -> studyStartDate (null, so empty)
  studyEndDate   = c()  # YYYYMMDD, from Analysis Specifications -> getDbCohortMethodDataArgs -> studyPeriods[0] -> studyEndDate (null, so empty)
)

# Time-at-risks (TARs) for the outcomes of interest in your study
# From Analysis Specifications -> createStudyPopArgs -> timeAtRisks
timeAtRisks <- tibble(
  label = c("TAR1"), # A generic label for the time-at-risk window
  riskWindowStart  = c(1), # Analysis Specifications -> createStudyPopArgs -> timeAtRisks[0] -> riskWindowStart
  startAnchor = c("cohort start"), # Analysis Specifications -> createStudyPopArgs -> timeAtRisks[0] -> startAnchor
  riskWindowEnd  = c(0), # Analysis Specifications -> createStudyPopArgs -> timeAtRisks[0] -> riskWindowEnd
  endAnchor = c("cohort end") # Analysis Specifications -> createStudyPopArgs -> timeAtRisks[0] -> endAnchor
)

# Propensity Score settings - match on PS
# From Analysis Specifications -> propensityScoreAdjustment -> psSettings[0] -> matchOnPsArgs
matchOnPsArgsList <- tibble(
  label = c("PSMatch1"), # A generic label for the PS matching strategy
  maxRatio  = c(10), # Analysis Specifications -> propensityScoreAdjustment -> psSettings[0] -> matchOnPsArgs -> maxRatio
  caliper = c(0.2), # Analysis Specifications -> propensityScoreAdjustment -> psSettings[0] -> matchOnPsArgs -> caliper
  caliperScale  = c("standardized logit") # Analysis Specifications -> propensityScoreAdjustment -> psSettings[0] -> matchOnPsArgs -> caliperScale
)

# Propensity Score settings - stratify by PS
# From Analysis Specifications -> propensityScoreAdjustment -> psSettings[0] -> stratifyByPsArgs
# Since stratifyByPsArgs is null in the spec, this list is empty.
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

# If studyPeriods is empty, the loop will run once with empty studyStartDate/EndDate
for (s in seq_len(max(1, nrow(studyPeriods)))) {
  studyStartDate <- if (nrow(studyPeriods) > 0) studyPeriods$studyStartDate[s] else NULL
  studyEndDate <- if (nrow(studyPeriods) > 0) studyPeriods$studyEndDate[s] else NULL

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

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

      # Default covariate settings with descendant exclusion
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default from template
      )

      # Construct outcome list including both primary and negative control outcomes
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # For primary outcomes, true effect size is unknown
            priorOutcomeLookback = 99999 # From Analysis Specifications -> createStudyPopArgs -> priorOutcomeLookBack
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # For negative controls, assumed true effect size is 1 (null effect)
          )
        })
      )
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude specific covariate concepts (from Analysis Specifications -> covariateSelection -> conceptsToExclude)
          # Note: cmTcList$targetConceptId and cmTcList$comparatorConceptId are not provided in spec,
          # so they are removed from the default template behavior here.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # getDbCohortMethodDataArgs from Analysis Specifications -> getDbCohortMethodDataArgs
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # From Analysis Specifications -> createStudyPopArgs -> restrictToCommonPeriod
        studyStartDate = studyStartDate, # From studyPeriods tibble (can be NULL)
        studyEndDate = studyEndDate, # From studyPeriods tibble (can be NULL)
        maxCohortSize = 0, # From Analysis Specifications -> getDbCohortMethodDataArgs -> maxCohortSize
        covariateSettings = covariateSettings
      )

      # createPsArgs from Analysis Specifications -> propensityScoreAdjustment -> createPsArgs
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> errorOnHighCorrelation
        stopOnError = FALSE, # Default from template, allows Strategus to complete all CM operations
        estimator = "att", # Default from template
        prior = Cyclops::createPrior( # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> prior
          priorType = "laplace", # Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> prior -> priorType
          useCrossValidation = TRUE # Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> prior -> useCrossValidation
        ),
        control = Cyclops::createControl( # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> control
          noiseLevel = "silent", # Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> control -> noiseLevel
          cvType = "auto", # Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> control -> cvType
          seed = 1, # Default from template
          resetCoefficients = TRUE, # Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> control -> resetCoefficients
          tolerance = 2e-07, # Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> control -> tolerance
          cvRepetitions = 10, # Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> control -> cvRepetitions
          startingVariance = 0.01 # Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> control -> startingVariance
        )
      )

      # computeSharedCovariateBalanceArgs - no specific parameters in Analysis Specifications, use template defaults
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default from template
        covariateFilter = NULL # Default from template
      )
      # computeCovariateBalanceArgs - no specific parameters in Analysis Specifications, use template defaults
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default from template
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default from template
      )

      # fitOutcomeModelArgs from Analysis Specifications -> fitOutcomeModelArgs
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # Analysis Specifications -> fitOutcomeModelArgs -> modelType
        stratified = TRUE, # Analysis Specifications -> fitOutcomeModelArgs -> stratified
        useCovariates = FALSE, # Analysis Specifications -> fitOutcomeModelArgs -> useCovariates
        inversePtWeighting = FALSE, # Analysis Specifications -> fitOutcomeModelArgs -> inversePtWeighting
        prior = Cyclops::createPrior( # From Analysis Specifications -> fitOutcomeModelArgs -> prior
          priorType = "laplace", # Analysis Specifications -> fitOutcomeModelArgs -> prior -> priorType
          useCrossValidation = TRUE # Analysis Specifications -> fitOutcomeModelArgs -> prior -> useCrossValidation
        ),
        control = Cyclops::createControl( # From Analysis Specifications -> fitOutcomeModelArgs -> control
          cvType = "auto", # Analysis Specifications -> fitOutcomeModelArgs -> control -> cvType
          seed = 1, # Default from template
          resetCoefficients = TRUE, # Analysis Specifications -> fitOutcomeModelArgs -> control -> resetCoefficients
          startingVariance = 0.01, # Analysis Specifications -> fitOutcomeModelArgs -> control -> startingVariance
          tolerance = 2e-07, # Analysis Specifications -> fitOutcomeModelArgs -> control -> tolerance
          cvRepetitions = 10, # Analysis Specifications -> fitOutcomeModelArgs -> control -> cvRepetitions
          noiseLevel = "quiet" # Analysis Specifications -> fitOutcomeModelArgs -> control -> noiseLevel
        )
      )

      # createStudyPopArgs from Analysis Specifications -> createStudyPopArgs
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE, # From Analysis Specifications -> createStudyPopArgs -> restrictToCommonPeriod
        firstExposureOnly = TRUE, # From Analysis Specifications -> createStudyPopArgs -> firstExposureOnly
        washoutPeriod = 365, # From Analysis Specifications -> createStudyPopArgs -> washoutPeriod
        removeDuplicateSubjects = "keep all", # From Analysis Specifications -> createStudyPopArgs -> removeDuplicateSubjects
        censorAtNewRiskWindow = TRUE, # From Analysis Specifications -> createStudyPopArgs -> censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # From Analysis Specifications -> createStudyPopArgs -> removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # From Analysis Specifications -> createStudyPopArgs -> priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From timeAtRisks tibble
        startAnchor = timeAtRisks$startAnchor[t], # From timeAtRisks tibble
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From timeAtRisks tibble
        endAnchor = timeAtRisks$endAnchor[t], # From timeAtRisks tibble
        minDaysAtRisk = 1, # From Analysis Specifications -> createStudyPopArgs -> timeAtRisks[0] -> minDaysAtRisk
        maxDaysAtRisk = 99999 # Default from template, not specified in Analysis Specifications
      )


      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study Period: %s-%s; TAR: %s; PS: %s",
          if (is.null(studyStartDate)) "Any" else studyStartDate, # Description includes study period
          if (is.null(studyEndDate)) "Any" else studyEndDate, # Description includes study period
          timeAtRisks$label[t], # Description includes Time-at-Risk label
          psCfg$label # Description includes PS configuration label
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
  analysesToExclude = NULL, # Default from template
  refitPsForEveryOutcome = FALSE, # Default from template
  refitPsForEveryStudyPopulation = FALSE, # Default from template
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default from template
)

# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file
# The folder structure and filename are derived from Analysis Specifications -> name
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "uveitissafety", "uveitissafetyAnalysisSpecification.json") # Using "uveitissafety" from Analysis Specifications -> name
)