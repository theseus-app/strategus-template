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
library(ROhdsiWebApi) # Required for fetching cohort definitions
library(CohortMethod) # Required for CohortMethod functions
library(FeatureExtraction) # Required for covariate settings
library(Cyclops) # Required for prior and control settings in PS and Outcome models

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
# Base URL for the WebAPI instance to retrieve cohort definitions
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI" # Default URL from template

# Cohort Definitions from Analysis Specifications
# targetCohort: id=1794126, name="target1"
# comparatorCohort: id=1794132, name="comparator1"
# outcomeCohort: id=1794131, name="outcome1"
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to a standard internal mapping (1=target, 2=comparator, 3=outcome)
# This mapping simplifies the logic downstream in the analysis specification.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Map target1 (1794126) to 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Map comparator1 (1794132) to 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Map outcome1 (1794131) to 3

# Negative control outcomes from Analysis Specifications
# negativeControlConceptSet: id=1888110, name="negative"
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Specified negativeControlConceptSet ID
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
  mutate(cohortId = row_number() + 100) %>% # Target/comparator cohort IDs start with 1, 2, 3... negativeControl outcomes start from 101.
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs between study cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Extract the outcome cohort and assign its re-mapped ID (3)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Filter for the re-mapped outcome cohort ID
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # cleanWindow is not specified in JSON, keeping template default

# Target and Comparator for the CohortMethod analysis
# Uses the re-mapped cohort IDs and names from the Analysis Specifications
cmTcList <- data.frame(
  targetCohortId = 1, # Re-mapped target cohort ID
  targetCohortName = "target1", # Name from Analysis Specifications
  comparatorCohortId = 2, # Re-mapped comparator cohort ID
  comparatorCohortName = "comparator1" # Name from Analysis Specifications
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study. The Analysis Specifications provide "conceptsToExclude" as empty.
excludedCovariateConcepts <- data.frame(
  conceptId = c(), # Empty as per Analysis Specifications (id=null, name="")
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all
# The Analysis Specifications provide "conceptsToInclude" as empty.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Not specified in JSON, keeping template default
  detectOnDescendants = TRUE # Not specified in JSON, keeping template default
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Not specified in JSON, keeping template default
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # All defined cohorts for diagnostics
  runInclusionStatistics = TRUE, # Not specified in JSON, keeping template default
  runIncludedSourceConcepts = TRUE, # Not specified in JSON, keeping template default
  runOrphanConcepts = TRUE, # Not specified in JSON, keeping template default
  runTimeSeries = FALSE, # Not specified in JSON, keeping template default
  runVisitContext = TRUE, # Not specified in JSON, keeping template default
  runBreakdownIndexEvents = TRUE, # Not specified in JSON, keeping template default
  runIncidenceRate = TRUE, # Not specified in JSON, keeping template default
  runCohortRelationship = TRUE, # Not specified in JSON, keeping template default
  runTemporalCohortCharacterization = TRUE, # Not specified in JSON, keeping template default
  minCharacterizationMean = 0.01 # Not specified in JSON, keeping template default
)

# CohortMethodModule -----------------------------------------------------------

# Study periods from Analysis Specifications -> getDbCohortMethodDataArgs -> studyPeriods
# The studyPeriods are empty in the JSON, so this tibble will be empty, leading to no period restriction.
studyPeriods <- tibble(
  studyStartDate = c(), # YYYYMMDD
  studyEndDate   = c()  # YYYYMMDD
)

# Time-at-risks (TARs) for the outcomes of interest in your study
# From Analysis Specifications -> createStudyPopArgs -> timeAtRisks
timeAtRisks <- tibble(
  label = c("TAR1", "TAR2"), # Descriptive labels for each time-at-risk window
  riskWindowStart  = c(1, 1), # From JSON
  startAnchor = c("cohort start", "cohort start"), # From JSON
  riskWindowEnd  = c(0, 99999), # From JSON
  endAnchor = c("cohort end", "cohort start") # From JSON
)

# Propensity Score settings - match on PS
# From Analysis Specifications -> propensityScoreAdjustment -> psSettings -> matchOnPsArgs
matchOnPsArgsList <- tibble(
  label = c("Match_1_0.2"), # Descriptive label for the matching strategy
  maxRatio  = c(1), # From JSON
  caliper = c(0.2), # From JSON
  caliperScale  = c("standardized logit") # From JSON
)

# Propensity Score settings - stratify by PS
# From Analysis Specifications -> propensityScoreAdjustment -> psSettings -> stratifyByPsArgs
# This is null in the JSON, so this tibble will be empty
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c()
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

# If studyPeriods is empty, ensure the loop runs at least once with empty strings
if (nrow(studyPeriods) == 0) {
  studyPeriods <- tibble(studyStartDate = "", studyEndDate = "")
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

      matchOnPsArgs <- NULL # Initialize to NULL
      stratifyByPsArgs <- NULL # Initialize to NULL

      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Not specified in JSON, keeping template default
          stratificationColumns = c() # Not specified in JSON, keeping template default
        )
      } else if (psCfg$method == "stratify") {
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Not specified in JSON, keeping template default
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings: The JSON has conceptsToInclude/Exclude as empty,
      # so we use the default settings for covariate generation.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Not specified in JSON, keeping template default
      )

      # Outcome list includes the main outcome and all negative controls
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not specified in JSON, keeping template default
            priorOutcomeLookback = 99999 # Not specified in JSON, keeping template default
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # Not specified in JSON, keeping template default (for negative controls)
          )
        })
      )
      
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Excluded covariate concepts: The JSON has "conceptsToExclude" as empty.
          # The template included placeholders for target/comparator concept IDs,
          # but these are not provided in the current Analysis Specifications,
          # so they are removed.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # getDbCohortMethodDataArgs from Analysis Specifications -> getDbCohortMethodDataArgs
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE, # From JSON
        studyStartDate = studyStartDate, # From loop, will be "" if no periods specified
        studyEndDate = studyEndDate, # From loop, will be "" if no periods specified
        maxCohortSize = 0, # From JSON
        covariateSettings = covariateSettings,
        firstExposureOnly = FALSE, # From JSON
        washoutPeriod = 0, # From JSON
        removeDuplicateSubjects = "keep all" # From JSON
      )

      # createPsArgs from Analysis Specifications -> propensityScoreAdjustment -> createPsArgs
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From JSON
        errorOnHighCorrelation = TRUE, # From JSON
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail (template default)
        estimator = "att", # Not specified in JSON, keeping template default
        prior = Cyclops::createPrior(
          priorType = "laplace", # From JSON
          exclude = c(0), # Not specified in JSON, keeping template default
          useCrossValidation = TRUE # From JSON
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", # From JSON
          cvType = "auto", # From JSON
          fold = 10, # From JSON
          seed = 1, # Not specified in JSON, keeping template default
          resetCoefficients = TRUE, # From JSON
          tolerance = 2e-07, # From JSON
          cvRepetitions = 10, # From JSON
          startingVariance = 0.01 # From JSON
        )
      )

      # computeSharedCovariateBalanceArgs and computeCovariateBalanceArgs
      # Not specified in JSON, keeping template defaults
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs from Analysis Specifications -> fitOutcomeModelArgs
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From JSON
        stratified = FALSE, # From JSON
        useCovariates = FALSE, # From JSON
        inversePtWeighting = FALSE, # From JSON
        prior = Cyclops::createPrior(
          priorType = "laplace", # From JSON
          useCrossValidation = TRUE # From JSON
        ),
        control = Cyclops::createControl(
          cvType = "auto", # From JSON
          fold = 10, # From JSON
          seed = 1, # Not specified in JSON, keeping template default
          resetCoefficients = TRUE, # From JSON
          startingVariance = 0.01, # From JSON
          tolerance = 2e-07, # From JSON
          cvRepetitions = 10, # From JSON
          noiseLevel = "quiet" # From JSON
        )
      )

      # createStudyPopArgs from Analysis Specifications -> createStudyPopArgs
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From JSON
        firstExposureOnly = FALSE, # From JSON
        washoutPeriod = 0, # From JSON
        removeDuplicateSubjects = "keep all", # From JSON (template had "keep first")
        censorAtNewRiskWindow = FALSE, # From JSON (template had TRUE)
        removeSubjectsWithPriorOutcome = FALSE, # From JSON (template had TRUE)
        priorOutcomeLookback = 99999, # From JSON
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From timeAtRisks tibble
        startAnchor = timeAtRisks$startAnchor[t], # From timeAtRisks tibble
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From timeAtRisks tibble
        endAnchor = timeAtRisks$endAnchor[t], # From timeAtRisks tibble
        minDaysAtRisk = 1, # From JSON
        maxDaysAtRisk = 99999 # Not specified in JSON, keeping template default
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
  analysesToExclude = NULL, # Not specified in JSON, keeping template default
  refitPsForEveryOutcome = FALSE, # Not specified in JSON, keeping template default
  refitPsForEveryStudyPopulation = FALSE, # Not specified in JSON, keeping template default
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Not specified in JSON, keeping template default
)

# Create the analysis specifications ------------------------------------------
# Use the "name" from Analysis Specifications for the file name: "tramadolcodein"
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "tramadolcodein", "tramadolcodeinAnalysisSpecification.json") # Using the specified study name
)