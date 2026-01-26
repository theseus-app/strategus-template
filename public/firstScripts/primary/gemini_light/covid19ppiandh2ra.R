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
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# From Analysis Specifications: cohortDefinitions.targetCohort, cohortDefinitions.comparatorCohort, cohortDefinitions.outcomeCohort
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to internal IDs for Strategus modules
# Target cohort ID 1794126 becomes 1
# Comparator cohort ID 1794132 becomes 2
# Outcome cohort ID 1794131 becomes 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# From Analysis Specifications: negativeControlConceptSet.id
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
  mutate(cohortId = row_number() + 100) %>% # target/comparator cohort ids start with 1, 2, 3... negativeControl -> 101, 102, 103...
  select(cohortId, cohortName, outcomeConceptId)


if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes:
# Filters for the outcome cohort (ID 3 after re-numbering)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Outcome cohort ID is 3 after re-numbering
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default from template, not specified in analysis spec

# Target and Comparator for the CohortMethod analysis
# Uses the re-numbered internal IDs and names
cmTcList <- data.frame(
  targetCohortId = 1, # Internal ID for target1
  targetCohortName = "target1", # From Analysis Specifications: cohortDefinitions.targetCohort.name
  comparatorCohortId = 2, # Internal ID for comparator1
  comparatorCohortName = "comparator1" # From Analysis Specifications: cohortDefinitions.comparatorCohort.name
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study. Based on Analysis Specifications: covariateSelection.conceptsToExclude,
# which is empty, so this data frame will be empty.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all
# Based on Analysis Specifications: covariateSelection.conceptsToInclude, which is empty.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
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
  cohortIds = cohortDefinitionSet$cohortId, # All cohorts defined
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

# Study periods for restricting data acquisition
# From Analysis Specifications: getDbCohortMethodDataArgs.studyPeriods
studyPeriods <- tibble(
  studyStartDate = c("20200101"), # YYYYMMDD
  studyEndDate   = c("20200515")  # YYYYMMDD
)

# Time-at-risks (TARs) for the outcomes of interest in your study
# From Analysis Specifications: createStudyPopArgs.timeAtRisks
timeAtRisks <- tibble(
  label = c("TAR1"), # Added a label for description
  riskWindowStart  = c(1), # From Analysis Specifications: createStudyPopArgs.timeAtRisks.riskWindowStart
  startAnchor = c("cohort start"), # From Analysis Specifications: createStudyPopArgs.timeAtRisks.startAnchor
  riskWindowEnd  = c(99999), # From Analysis Specifications: createStudyPopArgs.timeAtRisks.riskWindowEnd
  endAnchor = c("cohort start"), # From Analysis Specifications: createStudyPopArgs.timeAtRisks.endAnchor
  minDaysAtRisk = c(1) # From Analysis Specifications: createStudyPopArgs.timeAtRisks.minDaysAtRisk
)

# Propensity Score settings - match on PS
# From Analysis Specifications: propensityScoreAdjustment.psSettings.matchOnPsArgs (which is null)
matchOnPsArgsList <- tibble(
  label = c(),
  maxRatio  = c(),
  caliper = c(),
  caliperScale  = c() # "propensity score" | "standardized" | "standardized logit"
)

# Propensity Score settings - stratify by PS
# From Analysis Specifications: propensityScoreAdjustment.psSettings.stratifyByPsArgs
stratifyByPsArgsList <- tibble(
  label = c("Stratify5All"), # Added a label for description
  numberOfStrata  = c(5), # From Analysis Specifications: propensityScoreAdjustment.psSettings.stratifyByPsArgs.numberOfStrata
  baseSelection = c("all") # From Analysis Specifications: propensityScoreAdjustment.psSettings.stratifyByPsArgs.baseSelection
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

      # Covariate settings for feature extraction
      # Based on Analysis Specifications: covariateSelection.conceptsToInclude and conceptsToExclude (both empty)
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default from template
      )

      # List of outcomes including study outcomes and negative controls
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

      # Target-comparator-outcome combinations
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Based on Analysis Specifications, covariateSelection.conceptsToExclude is empty.
          # The template's cmTcList$targetConceptId[i] and cmTcList$comparatorConceptId[i] are not defined.
          # Therefore, no specific concepts are excluded beyond default covariate settings.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Arguments for fetching cohort method data
      # From Analysis Specifications: getDbCohortMethodDataArgs
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Not specified in analysis spec, keeping template default
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From Analysis Specifications: getDbCohortMethodDataArgs.maxCohortSize
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores
      # From Analysis Specifications: propensityScoreAdjustment.createPsArgs
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att", # Default from template, not specified in analysis spec
        prior = Cyclops::createPrior( # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.prior
          priorType = "laplace", # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.prior.priorType
          exclude = c(0), # Default from template
          useCrossValidation = TRUE # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control
          noiseLevel = "silent", # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.noiseLevel
          cvType = "auto", # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.cvType
          seed = 1, # Default from template
          resetCoefficients = TRUE, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.resetCoefficients
          tolerance = 2e-07, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.tolerance
          cvRepetitions = 10, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.cvRepetitions
          startingVariance = 0.01, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.startingVariance
          fold = 10 # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.fold
        )
      )

      # Arguments for computing shared covariate balance (default from template)
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      # Arguments for computing covariate balance (default from template)
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting the outcome model
      # From Analysis Specifications: fitOutcomeModelArgs
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From Analysis Specifications: fitOutcomeModelArgs.modelType
        stratified = TRUE, # From Analysis Specifications: fitOutcomeModelArgs.stratified
        useCovariates = FALSE, # From Analysis Specifications: fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE, # From Analysis Specifications: fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior( # From Analysis Specifications: fitOutcomeModelArgs.prior
          priorType = "laplace", # From Analysis Specifications: fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE # From Analysis Specifications: fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # From Analysis Specifications: fitOutcomeModelArgs.control
          cvType = "auto", # From Analysis Specifications: fitOutcomeModelArgs.control.cvType
          seed = 1, # Default from template
          resetCoefficients = TRUE, # From Analysis Specifications: fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01, # From Analysis Specifications: fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07, # From Analysis Specifications: fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10, # From Analysis Specifications: fitOutcomeModelArgs.control.cvRepetitions
          noiseLevel = "quiet", # From Analysis Specifications: fitOutcomeModelArgs.control.noiseLevel
          fold = 10 # From Analysis Specifications: fitOutcomeModelArgs.control.fold
        )
      )

      # Arguments for creating the study population
      # From Analysis Specifications: createStudyPopArgs
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications: createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = TRUE, # From Analysis Specifications: createStudyPopArgs.firstExposureOnly
        washoutPeriod = 365, # From Analysis Specifications: createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # From Analysis Specifications: createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # From Analysis Specifications: createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # From Analysis Specifications: createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # From Analysis Specifications: createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From Analysis Specifications: createStudyPopArgs.timeAtRisks
        startAnchor = timeAtRisks$startAnchor[t], # From Analysis Specifications: createStudyPopArgs.timeAtRisks
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From Analysis Specifications: createStudyPopArgs.timeAtRisks
        endAnchor = timeAtRisks$endAnchor[t], # From Analysis Specifications: createStudyPopArgs.timeAtRisks
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From Analysis Specifications: createStudyPopArgs.timeAtRisks
        maxDaysAtRisk = 99999 # Not specified in analysis spec, keeping template default
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
# The study name is "covid19ppiandh2ra" from Analysis Specifications: name
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "covid19ppiandh2ra", "covid19ppiandh2raAnalysisSpecification.json")
)