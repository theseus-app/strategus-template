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
# Base URL for the WebAPI instance to retrieve cohort definitions and concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions from WebAPI based on IDs provided in Analysis Specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1 (from Analysis Specifications)
    1794132, # Comparator: comparator1 (from Analysis Specifications)
    1794131  # Outcome: outcome1 (from Analysis Specifications)
  ),
  generateStats = TRUE
)

# Re-number cohorts to standard IDs (1 for target, 2 for comparator, 3 for outcome)
# This simplifies referencing them within the Strategus modules.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve the concept set definition for negative controls from WebAPI.
# The conceptSetId is taken from "negativeControlConceptSet.id" in Analysis Specifications.
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
  mutate(cohortId = row_number() + 100) %>% # Target/comparator cohort IDs start with 1, 2, 3... negativeControl -> 101, 102, 103...
  select(cohortId, cohortName, outcomeConceptId)


if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes:
# Filters the cohortDefinitionSet to get the outcome cohort (re-numbered to ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not specified in Analysis Specifications

# Target and Comparator for the CohortMethod analysis
# Defines the target and comparator cohorts using their re-numbered IDs and names.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1", # From Analysis Specifications: cohortDefinitions.targetCohort.name
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1" # From Analysis Specifications: cohortDefinitions.comparatorCohort.name
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study. Based on Analysis Specifications, "covariateSelection.conceptsToExclude" is empty.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all
# Based on Analysis Specifications, "covariateSelection.conceptsToInclude" is empty.
includedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Creates shared resource specifications for cohort definitions.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Creates shared resource specifications for negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default, not specified in Analysis Specifications
  detectOnDescendants = TRUE # Default, not specified in Analysis Specifications
)
# Creates module specifications for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # From template, not explicitly in Analysis Specifications
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Creates module specifications for CohortDiagnostics.
# All run flags are set to TRUE/FALSE based on common practice or template defaults.
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

# Study periods for restricting the analysis.
# Taken from "getDbCohortMethodDataArgs.studyPeriods" in Analysis Specifications.
studyPeriods <- tibble(
  studyStartDate = c("20110101"), # YYYYMMDD from Analysis Specifications
  studyEndDate   = c("20131231")  # YYYYMMDD from Analysis Specifications
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# Taken from "createStudyPopArgs.timeAtRisks" in Analysis Specifications.
timeAtRisks <- tibble(
  label = c("TAR_3_90"), # Custom label for description
  riskWindowStart  = c(3), # From Analysis Specifications
  startAnchor = c("cohort start"), # From Analysis Specifications
  riskWindowEnd  = c(90), # From Analysis Specifications
  endAnchor = c("cohort start"), # From Analysis Specifications
  minDaysAtRisk = c(1) # From Analysis Specifications
)

# Propensity Score settings - match on PS
# Taken from "propensityScoreAdjustment.psSettings.matchOnPsArgs" in Analysis Specifications.
matchOnPsArgsList <- tibble(
  label = c("Match_1_0.2_StdLogit"), # Custom label for description
  maxRatio  = c(1), # From Analysis Specifications
  caliper = c(0.2), # From Analysis Specifications
  caliperScale  = c("standardized logit") # From Analysis Specifications
)

# Propensity Score settings - stratify by PS
# "propensityScoreAdjustment.psSettings.stratifyByPsArgs" is null in Analysis Specifications, so this list is empty.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c() # "all" | "target" | "comparator"
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
      
      # Configure PS adjustment arguments based on the current PS configuration
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

      # Covariate settings for feature extraction.
      # Since "covariateSelection.conceptsToInclude" and "conceptsToExclude" are empty in Analysis Specifications,
      # default covariate settings are used.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default, not specified in Analysis Specifications
      )

      # List of outcomes for the analysis, including true outcomes and negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Default, not specified in Analysis Specifications
            priorOutcomeLookback = 99999 # From Analysis Specifications: createStudyPopArgs.priorOutcomeLookBack
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # Default for negative controls, not specified in Analysis Specifications
          )
        })
      )
      
      # Target-comparator-outcome combinations.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Excluded covariate concept IDs.
          # Based on Analysis Specifications, "covariateSelection.conceptsToExclude" is empty.
          excludedCovariateConceptIds = c(
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # Arguments for retrieving cohort method data from the database.
      # Settings are taken from "getDbCohortMethodDataArgs" in Analysis Specifications.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # From template, not explicitly in Analysis Specifications
        studyStartDate = studyStartDate, # From studyPeriods loop
        studyEndDate = studyEndDate, # From studyPeriods loop
        maxCohortSize = 0, # From Analysis Specifications: getDbCohortMethodDataArgs.maxCohortSize
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # Settings are taken from "propensityScoreAdjustment.createPsArgs" in Analysis Specifications.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications
        errorOnHighCorrelation = TRUE, # From Analysis Specifications
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att", # Default, not specified in Analysis Specifications
        prior = Cyclops::createPrior( # Prior settings for PS model.
          priorType = "laplace", # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.prior.priorType
          exclude = c(0), # Default, not specified in Analysis Specifications
          useCrossValidation = TRUE # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings for PS model.
          noiseLevel = "silent", # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.noiseLevel
          cvType = "auto", # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.cvType
          seed = 1, # Default, not specified in Analysis Specifications
          resetCoefficients = TRUE, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.resetCoefficients
          tolerance = 2e-07, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.tolerance
          cvRepetitions = 10, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.cvRepetitions
          startingVariance = 0.01 # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.startingVariance
        )
      )

      # Arguments for computing shared covariate balance.
      # Default settings are used as no specific settings are provided in Analysis Specifications.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      # Arguments for computing covariate balance.
      # Default settings are used as no specific settings are provided in Analysis Specifications.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting the outcome model.
      # Settings are taken from "fitOutcomeModelArgs" in Analysis Specifications.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From Analysis Specifications: fitOutcomeModelArgs.modelType
        stratified = FALSE, # From Analysis Specifications: fitOutcomeModelArgs.stratified
        useCovariates = FALSE, # From Analysis Specifications: fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE, # From Analysis Specifications: fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior( # Prior settings for outcome model.
          priorType = "laplace", # From Analysis Specifications: fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE # From Analysis Specifications: fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings for outcome model.
          cvType = "auto", # From Analysis Specifications: fitOutcomeModelArgs.control.cvType
          seed = 1, # Default, not specified in Analysis Specifications
          resetCoefficients = TRUE, # From Analysis Specifications: fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01, # From Analysis Specifications: fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07, # From Analysis Specifications: fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10, # From Analysis Specifications: fitOutcomeModelArgs.control.cvRepetitions
          noiseLevel = "quiet" # From Analysis Specifications: fitOutcomeModelArgs.control.noiseLevel
        )
      )
      
      # Arguments for creating the study population.
      # Settings are taken from "createStudyPopArgs" in Analysis Specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE, # From Analysis Specifications
        firstExposureOnly = FALSE, # From Analysis Specifications
        washoutPeriod = 0, # From Analysis Specifications
        removeDuplicateSubjects = "keep all", # From Analysis Specifications
        censorAtNewRiskWindow = FALSE, # From Analysis Specifications
        removeSubjectsWithPriorOutcome = TRUE, # From Analysis Specifications
        priorOutcomeLookback = 99999, # From Analysis Specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From timeAtRisks loop
        startAnchor = timeAtRisks$startAnchor[t], # From timeAtRisks loop
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From timeAtRisks loop
        endAnchor = timeAtRisks$endAnchor[t], # From timeAtRisks loop
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From timeAtRisks loop
        maxDaysAtRisk = 99999 # Default, not specified in Analysis Specifications
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
# Creates module specifications for CohortMethod.
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Default, not specified in Analysis Specifications
  refitPsForEveryOutcome = FALSE, # Default, not specified in Analysis Specifications
  refitPsForEveryStudyPopulation = FALSE, # Default, not specified in Analysis Specifications
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default, not specified in Analysis Specifications
)

# Create the analysis specifications ------------------------------------------
# Initializes an empty analysis specifications object and adds shared resources and module specifications.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path uses the study name "mars" from Analysis Specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "mars", "marsAnalysisSpecification.json")
)