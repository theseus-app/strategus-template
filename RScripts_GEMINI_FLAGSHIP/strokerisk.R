################################################################################
# This script creates the Strategus analysis specifications for the "strokerisk"
# study, based on the provided JSON settings. It defines the cohorts,
# analysis settings for CohortMethod, and configures Strategus modules
# including CohortGenerator, CohortDiagnostics, and CohortMethod.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
################################################################################
library(dplyr)
library(Strategus)

# ============== Shared Resources ==============
# This section defines resources shared across multiple modules, such as
# cohort definitions.

# The baseUrl for the WebAPI instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# --- Cohort Definitions ---
# Fetching cohort definitions from ATLAS using their IDs specified in the JSON.
# "cohortDefinitions": { "targetCohort": {"id": 1794126}, "comparatorCohort": {"id": 1794132}, "outcomeCohort": [{"id": 1794131}] }
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-numbering cohorts to a local, consistent ID scheme (1, 2, 3) for use within Strategus.
# This makes referencing them in the analysis settings easier.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# --- Negative Control Outcomes ---
# Fetching the negative control concept set from ATLAS and preparing it for use in CohortMethod.
# "negativeControlConceptSet": { "id": 1888110 }
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,
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
  # Assign unique cohort IDs starting from 101 to avoid collisions with main cohorts.
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Sanity check to ensure no cohort IDs overlap.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}


# --- Data frames for analysis settings ---
# These data frames and lists structure the analysis parameters for easy iteration.

# A data frame for the outcome(s) of interest.
# "cohortDefinitions": { "outcomeCohort": [{"id": 1794131, "name": "outcome1"}] }
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName)

# A data frame defining the target and comparator cohorts for the CohortMethod analysis.
# "cohortDefinitions": { "targetCohort": {"name": "target1"}, "comparatorCohort": {"name": "comparator1"} }
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# --- Covariate Selection ---
# The JSON specifies empty arrays for conceptsToInclude and conceptsToExclude.
# CohortMethod by default excludes the target and comparator concepts from the covariates,
# so no additional specification is needed here.

# ============== Module Settings ==============

# --- CohortGeneratorModule Settings ---
# This module is responsible for generating the cohorts defined above.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# --- CohortDiagnosticsModule Settings ---
# This module runs a standard set of diagnostics on the generated cohorts.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
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
  minCharacterizationMean = 0.01 # A common setting to reduce noise.
)

# --- CohortMethodModule Settings ---
# This section defines the settings for the comparative cohort analysis.

# Defining study periods from the analysis specifications.
# "getDbCohortMethodDataArgs": { "studyPeriods": [...] }
studyPeriods <- tibble::tibble(
  studyStartDate = c("20010101", "20010101"),
  studyEndDate   = c("20171231", "20151130")
)

# Defining Time-at-risks (TARs) for the outcomes.
# "createStudyPopArgs": { "timeAtRisks": [...] }
timeAtRisks <- tibble::tibble(
  label = c("On Treatment"), # A descriptive label for the TAR.
  riskWindowStart  = c(1),            # From timeAtRisks.riskWindowStart
  startAnchor = c("cohort start"),    # From timeAtRisks.startAnchor
  riskWindowEnd  = c(0),              # From timeAtRisks.riskWindowEnd
  endAnchor = c("cohort end")         # From timeAtRisks.endAnchor
)

# Defining propensity score adjustment strategies.
# "propensityScoreAdjustment": { "psSettings": [...] }
psConfigList <- list()
# Setting 1: No propensity score adjustment.
psConfigList[[1]] <- list(
  method = "none",
  label = "No PS adjustment",
  params = list()
)
# Setting 2: Matching on propensity score.
psConfigList[[2]] <- list(
  method = "match",
  label = "1:1 matching on PS",
  params = list(
    maxRatio = 1,                 # From psSettings[1].matchOnPsArgs.maxRatio
    caliper = 0.05,               # From psSettings[1].matchOnPsArgs.caliper
    caliperScale = "propensity score"  # From psSettings[1].matchOnPsArgs.caliperScale
  )
)
# Setting 3: Matching on standardized logit of the propensity score.
psConfigList[[3]] <- list(
  method = "match",
  label = "1:10 matching on standardized logit",
  params = list(
    maxRatio = 10,                # From psSettings[2].matchOnPsArgs.maxRatio
    caliper = 0.2,                # From psSettings[2].matchOnPsArgs.caliper
    caliperScale = "standardized logit" # From psSettings[2].matchOnPsArgs.caliperScale
  )
)

# --- Iteration and Analysis List Creation ---
# This loop iterates through all combinations of study periods, TARs, and PS settings
# to create a comprehensive list of analyses to be executed.
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Set up propensity score adjustment arguments based on the current configuration.
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # This block is included for completeness, though no stratification is specified in this study.
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      } else { # This handles the "none" method
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- NULL
      }

      # Use default covariate settings, as no specific inclusions/exclusions were specified.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Combine the primary outcome of interest with the negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            # This is also specified in createStudyPopArgs, defining the lookback window for prior outcomes.
            priorOutcomeLookback = 99999
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # By definition, negative controls have a true effect size of 1.
          )
        })
      )
      
      # Define the target-comparator-outcomes list for CohortMethod.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # No additional concepts to exclude beyond the default T & C cohorts.
          excludedCovariateConceptIds = c()
        )
      }

      # Settings for fetching data from the database.
      # "getDbCohortMethodDataArgs": {...}
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From getDbCohortMethodDataArgs.maxCohortSize, 0 means no limit.
        covariateSettings = covariateSettings,
        restrictToCommonPeriod = FALSE # This setting is primarily used in createStudyPopArgs.
      )
      
      # Settings for creating the study population.
      # "createStudyPopArgs": {...}
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1, # From timeAtRisks.minDaysAtRisk
        restrictToCommonPeriod = FALSE, # From createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE, # From createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0, # From createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # From createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # From createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # From createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999 # From createStudyPopArgs.priorOutcomeLookBack
      )
      
      # Settings for creating the propensity score model.
      # "propensityScoreAdjustment": { "createPsArgs": {...} }
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Setting to FALSE allows Strategus to complete all operations.
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace", # From createPsArgs.prior.priorType
          useCrossValidation = TRUE # From createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          tolerance = 2e-7, # From createPsArgs.control.tolerance
          cvType = "auto", # From createPsArgs.control.cvType
          fold = 10, # From createPsArgs.control.fold
          cvRepetitions = 10, # From createPsArgs.control.cvRepetitions
          noiseLevel = "quiet", # From createPsArgs.control.noiseLevel
          resetCoefficients = TRUE, # From createPsArgs.control.resetCoefficients
          startingVariance = 0.01 # From createPsArgs.control.startingVariance
        )
      )

      # Settings for computing covariate balance. These are standard settings.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )
      
      # Settings for fitting the outcome model.
      # "fitOutcomeModelArgs": {...}
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From fitOutcomeModelArgs.modelType
        stratified = TRUE, # From fitOutcomeModelArgs.stratified
        useCovariates = FALSE, # From fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE, # From fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior(
          priorType = "laplace", # From fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE # From fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          tolerance = 2e-7, # From fitOutcomeModelArgs.control.tolerance
          cvType = "auto", # From fitOutcomeModelArgs.control.cvType
          fold = 10, # From fitOutcomeModelArgs.control.fold
          cvRepetitions = 10, # From fitOutcomeModelArgs.control.cvRepetitions
          noiseLevel = "quiet", # From fitOutcomeModelArgs.control.noiseLevel
          resetCoefficients = TRUE, # From fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01 # From fitOutcomeModelArgs.control.startingVariance
        )
      )

      # Append the fully specified analysis to the list of analyses.
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

# Create the final CohortMethod module specifications.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# ============== Analysis Specification Assembly ==============
# Combine all module specifications into a single Strategus analysis specification object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The study name "strokerisk" is taken from the analysis specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "strokerisk", "strokeriskAnalysisSpecification.json")
)