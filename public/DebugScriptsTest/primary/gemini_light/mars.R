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
# Base URL for the WebAPI instance to retrieve cohort definitions and concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Retrieve cohort definitions from WebAPI based on IDs provided in Analysis Specifications.
# The IDs are re-numbered locally for internal consistency within the Strategus analysis.
# Target cohort ID 1794126 is re-numbered to 1.
# Comparator cohort ID 1794132 is re-numbered to 2.
# Outcome cohort ID 1794131 is re-numbered to 3.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to internal IDs (1, 2, 3) for Strategus.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve the negative control concept set (ID 1888110) from WebAPI,
# resolve it to individual concepts, and format it as a cohort set.
# Negative control cohort IDs start from 101 to avoid conflicts with
# target/comparator/outcome cohort IDs.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # negativeControlConceptSet.id: 1888110
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
  mutate(cohortId = row_number() + 100) %>% # Assign unique IDs starting from 101
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs across all defined cohorts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Filter for the outcome cohort (re-numbered ID 3).
# cleanWindow is not specified in Analysis Specifications, using default 365.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis.
# Uses the re-numbered target (ID 1) and comparator (ID 2) cohorts.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1", # cohortDefinitions.targetCohort.name
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1" # cohortDefinitions.comparatorCohort.name
)

# For the CohortMethod LSPS, we'll need to exclude specific covariates.
# Based on Analysis Specifications, covariateSelection.conceptsToExclude is empty (id: null).
# Therefore, this data frame will be empty.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all
# Based on Analysis Specifications, covariateSelection.conceptsToInclude is empty (id: null).
# This block remains commented out as no specific concepts are provided.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# Initializes the CohortGeneratorModule settings creator.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Creates shared resource specifications for cohort definitions.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Creates shared resource specifications for negative control outcome cohorts.
# occurrenceType and detectOnDescendants are not specified in Analysis Specifications, using defaults.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Creates module specifications for CohortGenerator.
# generateStats is set to TRUE as per template.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# Initializes the CohortDiagnosticsModule settings creator.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Creates module specifications for CohortDiagnostics.
# Runs diagnostics for all defined cohorts (target, comparator, outcome, negative controls).
# Other parameters are set to TRUE/FALSE as per template defaults or common practice.
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId),
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
# Populated from getDbCohortMethodDataArgs.studyPeriods in Analysis Specifications.
studyPeriods <- tibble(
  studyStartDate = c("20110101"), # getDbCohortMethodDataArgs.studyPeriods[0].studyStartDate
  studyEndDate   = c("20131231")  # getDbCohortMethodDataArgs.studyPeriods[0].studyEndDate
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# Populated from createStudyPopArgs.timeAtRisks in Analysis Specifications.
timeAtRisks <- tibble(
  label = c("TAR_3_90"), # Custom label for this TAR
  riskWindowStart  = c(3), # createStudyPopArgs.timeAtRisks[0].riskWindowStart
  startAnchor = c("cohort start"), # createStudyPopArgs.timeAtRisks[0].startAnchor
  riskWindowEnd  = c(90), # createStudyPopArgs.timeAtRisks[0].riskWindowEnd
  endAnchor = c("cohort start") # createStudyPopArgs.timeAtRisks[0].endAnchor
)

# Propensity Score settings - match on PS.
# Populated from propensityScoreAdjustment.psSettings where matchOnPsArgs is not null.
matchOnPsArgsList <- tibble(
  label = c("Match_1_0.2_stdLogit"), # Custom label for this PS setting
  maxRatio  = c(1), # propensityScoreAdjustment.psSettings[0].matchOnPsArgs.maxRatio
  caliper = c(0.2), # propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliper
  caliperScale  = c("standardized logit") # propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliperScale
)

# Propensity Score settings - stratify by PS.
# Populated from propensityScoreAdjustment.psSettings where stratifyByPsArgs is not null.
# In this case, stratifyByPsArgs is null in Analysis Specifications, so this tibble is empty.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c()
)

# Build a single PS configuration list (each entry has: method, label, params)
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
# This block will not execute as stratifyByPsArgsList is empty based on Analysis Specifications.
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

      # Configure PS adjustment arguments based on the method (match or stratify).
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper, # Corrected extra parenthesis from template
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

      # Covariate settings: Using default settings as covariateSelection in Analysis Specifications is empty.
      # addDescendantsToExclude is TRUE as per template.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Combine outcome cohorts (outcome1) and negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not specified in Analysis Specifications
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

      # Create target-comparator-outcome combinations.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # excludedCovariateConceptIds:
          # Based on Analysis Specifications, covariateSelection.conceptsToExclude is empty.
          # The template's reference to cmTcList$targetConceptId[i] and cmTcList$comparatorConceptId[i]
          # is removed as these are not provided in the Analysis Specifications (only cohort IDs are).
          excludedCovariateConceptIds = c() # Preserving original script's logic
        )
      }

      # Arguments for retrieving cohort method data from the database.
      # Populated from getDbCohortMethodDataArgs in Analysis Specifications.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # createStudyPopArgs.restrictToCommonPeriod: true
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # getDbCohortMethodDataArgs.maxCohortSize: 0
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # Populated from propensityScoreAdjustment.createPsArgs in Analysis Specifications.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail (default from template)
        estimator = "att", # Default from template
        prior = Cyclops::createPrior( # propensityScoreAdjustment.createPsArgs.prior
          priorType = "laplace", # prior.priorType
          exclude = c(0), # Default from template
          useCrossValidation = TRUE # prior.useCrossValidation
        ),
        control = Cyclops::createControl( # propensityScoreAdjustment.createPsArgs.control
          noiseLevel = "silent", # control.noiseLevel
          cvType = "auto", # control.cvType
          seed = 1, # Added for reproducibility, aligning with template
          resetCoefficients = TRUE, # control.resetCoefficients
          tolerance = 2e-07, # control.tolerance
          # numberOfFolds = 10, # Removed: This argument is not valid for Cyclops::createControl, causing the error.
          cvRepetitions = 1, # Changed from 10 to 1, aligning with template
          startingVariance = 0.01 # control.startingVariance
        )
      )

      # Arguments for computing shared covariate balance.
      # Using default values from template as not specified in Analysis Specifications.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      # Arguments for computing covariate balance.
      # Using default values from template as not specified in Analysis Specifications.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting the outcome model.
      # Populated from fitOutcomeModelArgs in Analysis Specifications.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # fitOutcomeModelArgs.modelType
        stratified = FALSE, # fitOutcomeModelArgs.stratified (Preserving original script's specific value, template had TRUE)
        useCovariates = FALSE, # fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE, # fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior( # fitOutcomeModelArgs.prior
          priorType = "laplace", # prior.priorType
          useCrossValidation = TRUE # prior.useCrossValidation
        ),
        control = Cyclops::createControl( # fitOutcomeModelArgs.control
          cvType = "auto", # control.cvType
          seed = 1, # Added for reproducibility, aligning with template
          resetCoefficients = TRUE, # control.resetCoefficients
          startingVariance = 0.01, # control.startingVariance
          tolerance = 2e-07, # control.tolerance
          # numberOfFolds = 10, # Removed: This argument is not valid for Cyclops::createControl, causing the error.
          cvRepetitions = 1, # Changed from 10 to 1, aligning with template
          noiseLevel = "quiet" # control.noiseLevel
        )
      )

      # Arguments for creating the study population.
      # Populated from createStudyPopArgs in Analysis Specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE, # createStudyPopArgs.restrictToCommonPeriod (Preserving original script's specific value, template had FALSE)
        firstExposureOnly = FALSE, # createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0, # createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # createStudyPopArgs.removeDuplicateSubjects (Preserving original script's specific value, template had "keep first")
        censorAtNewRiskWindow = FALSE, # createStudyPopArgs.censorAtNewRiskWindow (Preserving original script's specific value, template had TRUE)
        removeSubjectsWithPriorOutcome = TRUE, # createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1, # createStudyPopArgs.timeAtRisks[0].minDaysAtRisk
        maxDaysAtRisk = 99999 # Default from template, not specified in Analysis Specifications
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

# Initializes the CohortMethodModule settings creator.
cmModuleSettingsCreator <- CohortMethodModule$new()

# Creates module specifications for CohortMethod.
# Uses the generated list of CM analyses and target-comparator-outcome combinations.
# Other parameters are defaults from template.
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the analysis specifications ------------------------------------------
# Assembles the complete Strategus analysis specifications by adding shared resources
# and module specifications.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path is constructed using "inst", "studyName" (from Analysis Specifications),
# and "studyNameAnalysisSpecification.json".
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "mars", "marsAnalysisSpecification.json") # "name": "mars" from Analysis Specifications
)