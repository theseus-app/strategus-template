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
# Export cohort definitions from WebAPI based on IDs provided in Analysis Specifications.
# The IDs are re-numbered to 1, 2, 3 for internal consistency within the study.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for use in the study.
# Target cohort (ID 1794126) becomes internal ID 1.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# Comparator cohort (ID 1794132) becomes internal ID 2.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# Outcome cohort (ID 1794131) becomes internal ID 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve negative control concept set definition from WebAPI.
# The concept set ID 1888110 is specified in Analysis Specifications.
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
  # Assign unique cohort IDs for negative controls, starting from 101 to avoid conflicts with T/C/O.
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs between study cohorts and negative controls.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes:
# Filter for the outcome cohort (internal ID 3, which is 1794131 from Analysis Specifications).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not specified in Analysis Specifications.

# Target and Comparator for the CohortMethod analysis
# Populate with internal IDs and names from Analysis Specifications.
cmTcList <- data.frame(
  targetCohortId = 1, # Internal ID for 1794126 (target1)
  targetCohortName = "target1",
  comparatorCohortId = 2, # Internal ID for 1794132 (comparator1)
  comparatorCohortName = "comparator1"
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study.
# From Analysis Specifications: covariateSelection.conceptsToExclude is empty.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all
# From Analysis Specifications: covariateSelection.conceptsToInclude is empty.
# This block is commented out as no concepts are specified for inclusion.
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
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default, not specified in Analysis Specifications.
  detectOnDescendants = TRUE # Default, not specified in Analysis Specifications.
)
# Creates module specifications for CohortGenerator, enabling statistics generation.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# Initializes the CohortDiagnosticsModule settings creator.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Creates module specifications for CohortDiagnostics.
# All run flags are set to TRUE/FALSE as per template defaults, as no specific settings
# are provided in Analysis Specifications for CohortDiagnostics.
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

# Study periods for restricting data extraction.
# From Analysis Specifications: getDbCohortMethodDataArgs.studyPeriods.
studyPeriods <- tibble(
  studyStartDate = c("20130101"), # YYYYMMDD
  studyEndDate   = c("20201231")  # YYYYMMDD
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# From Analysis Specifications: createStudyPopArgs.timeAtRisks.
timeAtRisks <- tibble(
  label = c("TAR_1_730_CS"), # A descriptive label for this TAR.
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd  = c(730),
  endAnchor = c("cohort start") # "cohort start" | "cohort end"
)

# Propensity Score settings - match on PS
# From Analysis Specifications: propensityScoreAdjustment.psSettings.matchOnPsArgs.
matchOnPsArgsList <- tibble(
  label = c("MatchOnPs_1_0.05"), # A descriptive label for this PS matching setting.
  maxRatio  = c(1),
  caliper = c(0.05),
  caliperScale  = c("propensity score") # "propensity score" | "standardized" | "standardized logit"
)

# Propensity Score settings - stratify by PS
# From Analysis Specifications: propensityScoreAdjustment.psSettings.stratifyByPsArgs is null.
# Therefore, this tibble remains empty.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c(), # "all" | "target" | "comparator"
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
# This block will not execute as stratifyByPsArgsList is empty.
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

      # Configure PS adjustment arguments based on the method.
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default, not specified in Analysis Specifications.
          stratificationColumns = c() # Default, not specified in Analysis Specifications.
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default, not specified in Analysis Specifications.
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings for feature extraction.
      # From Analysis Specifications: covariateSelection.conceptsToInclude/Exclude are empty.
      # Using default settings.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default, not specified in Analysis Specifications.
      )

      # Define outcomes for the study, including true outcomes and negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not applicable for observational studies.
            priorOutcomeLookback = 99999 # From Analysis Specifications: createStudyPopArgs.priorOutcomeLookBack.
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # Assumed true effect size for negative controls.
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
          # Excluded covariate concept IDs.
          # From Analysis Specifications: covariateSelection.conceptsToExclude is empty.
          # The template's `cmTcList$targetConceptId[i]` and `cmTcList$comparatorConceptId[i]`
          # are not present in `cmTcList` and are removed.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Arguments for retrieving cohort method data from the database.
      # From Analysis Specifications: getDbCohortMethodDataArgs.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Default in template, not explicitly in JSON for this section.
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From Analysis Specifications: getDbCohortMethodDataArgs.maxCohortSize.
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications: createPsArgs.maxCohortSizeForFitting.
        errorOnHighCorrelation = TRUE, # From Analysis Specifications: createPsArgs.errorOnHighCorrelation.
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail.
        estimator = "att", # Default, not specified in Analysis Specifications.
        prior = Cyclops::createPrior( # From Analysis Specifications: createPsArgs.prior.
          priorType = "laplace",
          exclude = c(0), # Default, not specified in Analysis Specifications.
          useCrossValidation = TRUE # From Analysis Specifications: createPsArgs.prior.useCrossValidation.
        ),
        control = Cyclops::createControl( # From Analysis Specifications: createPsArgs.control.
          noiseLevel = "silent", # From Analysis Specifications: createPsArgs.control.noiseLevel.
          cvType = "auto", # From Analysis Specifications: createPsArgs.control.cvType.
          seed = 1, # Default, not specified in Analysis Specifications.
          resetCoefficients = TRUE, # From Analysis Specifications: createPsArgs.control.resetCoefficients.
          tolerance = 2e-07, # From Analysis Specifications: createPsArgs.control.tolerance.
          cvRepetitions = 10, # From Analysis Specifications: createPsArgs.control.cvRepetitions.
          startingVariance = 0.01 # From Analysis Specifications: createPsArgs.control.startingVariance.
        )
      )

      # Arguments for computing shared covariate balance.
      # Using template defaults as not specified in Analysis Specifications.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      # Arguments for computing covariate balance.
      # Using template defaults as not specified in Analysis Specifications.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting the outcome model.
      # From Analysis Specifications: fitOutcomeModelArgs.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From Analysis Specifications: fitOutcomeModelArgs.modelType.
        stratified = TRUE, # From Analysis Specifications: fitOutcomeModelArgs.stratified.
        useCovariates = FALSE, # From Analysis Specifications: fitOutcomeModelArgs.useCovariates.
        inversePtWeighting = FALSE, # From Analysis Specifications: fitOutcomeModelArgs.inversePtWeighting.
        prior = Cyclops::createPrior( # From Analysis Specifications: fitOutcomeModelArgs.prior.
          priorType = "laplace",
          useCrossValidation = TRUE # From Analysis Specifications: fitOutcomeModelArgs.prior.useCrossValidation.
        ),
        control = Cyclops::createControl( # From Analysis Specifications: fitOutcomeModelArgs.control.
          cvType = "auto", # From Analysis Specifications: fitOutcomeModelArgs.control.cvType.
          seed = 1, # Default, not specified in Analysis Specifications.
          resetCoefficients = TRUE, # From Analysis Specifications: fitOutcomeModelArgs.control.resetCoefficients.
          startingVariance = 0.01, # From Analysis Specifications: fitOutcomeModelArgs.control.startingVariance.
          tolerance = 2e-07, # From Analysis Specifications: fitOutcomeModelArgs.control.tolerance.
          cvRepetitions = 10, # From Analysis Specifications: fitOutcomeModelArgs.control.cvRepetitions.
          noiseLevel = "quiet" # From Analysis Specifications: fitOutcomeModelArgs.control.noiseLevel.
        )
      )

      # Arguments for creating the study population.
      # From Analysis Specifications: createStudyPopArgs.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE, # From Analysis Specifications: createStudyPopArgs.restrictToCommonPeriod.
        firstExposureOnly = FALSE, # From Analysis Specifications: createStudyPopArgs.firstExposureOnly.
        washoutPeriod = 0, # From Analysis Specifications: createStudyPopArgs.washoutPeriod.
        removeDuplicateSubjects = "keep all", # From Analysis Specifications: createStudyPopArgs.removeDuplicateSubjects.
        censorAtNewRiskWindow = FALSE, # From Analysis Specifications: createStudyPopArgs.censorAtNewRiskWindow.
        removeSubjectsWithPriorOutcome = TRUE, # From Analysis Specifications: createStudyPopArgs.removeSubjectsWithPriorOutcome.
        priorOutcomeLookback = 99999, # From Analysis Specifications: createStudyPopArgs.priorOutcomeLookBack.
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From Analysis Specifications: createStudyPopArgs.timeAtRisks.minDaysAtRisk.
        maxDaysAtRisk = 99999 # Default, not specified in Analysis Specifications.
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
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # No analyses to exclude, not specified in Analysis Specifications.
  refitPsForEveryOutcome = FALSE, # Default, not specified in Analysis Specifications.
  refitPsForEveryStudyPopulation = FALSE, # Default, not specified in Analysis Specifications.
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Using default diagnostic thresholds.
)

# Create the analysis specifications ------------------------------------------
# Initializes an empty analysis specifications object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Adds shared resources for cohort definitions.
  Strategus::addSharedResources(cohortDefinitionShared) |>
  # Adds shared resources for negative control outcomes.
  Strategus::addSharedResources(negativeControlsShared) |>
  # Adds module specifications for CohortGenerator.
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  # Adds module specifications for CohortDiagnostics.
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  # Adds module specifications for CohortMethod.
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Saves the complete analysis specifications to a JSON file.
# The file path is constructed using the study name "glp1radepression" from Analysis Specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "glp1radepression", "glp1radepressionAnalysisSpecification.json")
)