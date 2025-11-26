################################################################################
# This script uses the OHDSI Strategus package to create a complete analysis
# specification for a comparative cohort study. The settings are derived from
# a provided JSON configuration.
#
# See the "Create analysis specifications" section of the Strategus documentation
# for more details on the functions used here.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# provides further details on the function arguments.
################################################################################
library(dplyr)
library(Strategus)

# ===========廣===========
# Analysis Specification Name
#
# The 'name' from the JSON specification is used to define the study name, which
# will be used for the output folder and file names.
# ===========狹===========
analysisName <- "covid19famotidine"

# ===========廣===========
# Shared Resources
#
# This section defines the resources that are shared across different modules
# of the analysis, such as cohort definitions and concept sets.
# ===========狹===========

# --- Cohort Definitions ---
# The cohort IDs for target, comparator, and outcome cohorts are specified here.
# These IDs are used to retrieve the cohort definitions from a WebAPI instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI" # Using a public WebAPI instance for demonstration

# Retrieve cohort definitions from WebAPI using the IDs from the JSON specification.
# "targetCohort": { "id": 1794126 }
# "comparatorCohort": { "id": 1794132 }
# "outcomeCohort": { "id": 1794131 }
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target
    1794132, # Comparator
    1794131  # Outcome
  ),
  generateStats = TRUE
)

# Re-number cohorts to a simple, consistent scheme (1, 2, 3) for use within Strategus.
# This avoids potential conflicts and simplifies referencing them later.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# --- Negative Control Concept Set ---
# Negative controls are outcomes not believed to be caused by the exposure.
# They are used for empirical calibration of p-values.
# The concept set ID is taken from the "negativeControlConceptSet" section of the JSON.
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
  # Assign unique cohort IDs to negative controls, starting from 101 to avoid collision with main cohorts.
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Sanity check to ensure no cohort IDs are duplicated across the main cohorts and negative controls.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# ===========廣===========
# Analysis Settings Data Frames
#
# These data frames organize the various components of the analysis, such as
# target-comparator pairs, outcomes, and covariate settings.
# ===========狹===========

# --- Outcomes List ---
# Defines the primary outcomes for the study based on the retrieved cohort definitions.
# "outcomeCohort": [{ "id": 1794131, "name": "outcome1" }]
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # cleanWindow is for Characterization module, not used in CM here.

# --- Target and Comparator List ---
# Defines the target-comparator pairs for the CohortMethod analysis.
# The names are taken directly from the JSON specification.
# "targetCohort": { "name": "target1" }
# "comparatorCohort": { "name": "comparator1" }
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# --- Covariate Selection ---
# Based on the "covariateSelection" section of the JSON.
# In this case, both conceptsToInclude and conceptsToExclude are empty,
# so we will use the default covariates from FeatureExtraction and define an
# empty data frame for any additional exclusions.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# Optional: If you want to define covariates to include instead of including them all
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )


# ===========廣===========
# Module Specifications
#
# This section defines the settings for each Strategus module that will be
# executed in the analysis pipeline (e.g., CohortGenerator, CohortDiagnostics,
# and CohortMethod).
# ===========狹===========

# --- CohortGeneratorModule ---
# This module is responsible for generating the cohort instances from their definitions.
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

# --- CohortDiagnosticsModule ---
# This module runs a set of diagnostic checks on the generated cohorts.
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
  minCharacterizationMean = 0.01
)

# --- CohortMethodModule ---
# This is the core module for the comparative cohort study. The following sections
# configure all aspects of the analysis, from data extraction to outcome modeling.

# --- Study Period Settings ---
# Defined in the "getDbCohortMethodDataArgs.studyPeriods" section of the JSON.
# "studyStartDate": "20200201", "studyEndDate": "20200530"
studyPeriods <- tibble(
  studyStartDate = c("20200201"),
  studyEndDate   = c("20200530")
)

# --- Time-at-Risk (TAR) Settings ---
# Defined in the "createStudyPopArgs.timeAtRisks" section of the JSON.
# "riskWindowStart": 1, "startAnchor": "cohort start"
# "riskWindowEnd": 30, "endAnchor": "cohort start"
timeAtRisks <- tibble(
  label = c("1 to 30 days after cohort start"), # A descriptive label for this TAR
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(30),
  endAnchor = c("cohort start")
)

# --- Propensity Score (PS) Matching Settings ---
# Defined in the "propensityScoreAdjustment.psSettings" array (entry with "matchOnPsArgs").
# "maxRatio": 1, "caliper": 0.2, "caliperScale": "standardized logit"
matchOnPsArgsList <- tibble(
  label = c("1-to-1 Matching, 0.2 SDM Caliper"), # A descriptive label
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# --- Propensity Score (PS) Stratification Settings ---
# Defined in the "propensityScoreAdjustment.psSettings" array (entry with "stratifyByPsArgs").
# "numberOfStrata": 5, "baseSelection": "all"
stratifyByPsArgsList <- tibble(
  label = c("5 PS Strata"), # A descriptive label
  numberOfStrata  = c(5),
  baseSelection = c("all")
)

# Build a single list of all PS configurations to iterate over.
# This structure allows combining different PS adjustment methods in one study.
psConfigList <- list()

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


# --- Build Full CohortMethod Analysis List ---
# Iterate through all combinations of study period, TAR, and PS settings
# to create a list of CohortMethod analysis specifications.
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Configure either matching or stratification based on the PS configuration
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
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Standard covariate settings from FeatureExtraction package
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Combine primary outcomes and negative control outcomes into a single list
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )
      
      # Create the list of Target-Comparator-Outcomes
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Use the empty list of excluded concepts defined earlier
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Settings for initial data extraction from the database.
      # Based on the "getDbCohortMethodDataArgs" section of the JSON.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        maxCohortSize = 0, # "maxCohortSize": 0
        restrictToCommonPeriod = FALSE, # "restrictToCommonPeriod": false
        firstExposureOnly = TRUE, # "firstExposureOnly": true
        washoutPeriod = 0, # "washoutPeriod": 0
        removeDuplicateSubjects = "remove all", # "removeDuplicateSubjects": "remove all"
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        covariateSettings = covariateSettings
      )
      
      # Settings for creating the propensity score model.
      # Based on the "propensityScoreAdjustment.createPsArgs" section of the JSON.
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # "maxCohortSizeForFitting": 250000
        errorOnHighCorrelation = TRUE, # "errorOnHighCorrelation": true
        stopOnError = FALSE, # Setting to FALSE allows Strategus to complete all CM operations
        prior = Cyclops::createPrior(
          priorType = "laplace", # "prior.priorType": "laplace"
          useCrossValidation = TRUE # "prior.useCrossValidation": true
        ),
        control = Cyclops::createControl(
          tolerance = 2e-7, # "control.tolerance": 2e-7
          cvType = "auto", # "control.cvType": "auto"
          fold = 10, # "control.fold": 10
          cvRepetitions = 10, # "control.cvRepetitions": 10
          noiseLevel = "silent", # "control.noiseLevel": "silent"
          resetCoefficients = TRUE, # "control.resetCoefficients": true
          startingVariance = 0.01 # "control.startingVariance": 0.01
        )
      )

      # Settings for calculating covariate balance after PS adjustment
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(maxCohortSize = 250000)
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(maxCohortSize = 250000)

      # Settings for fitting the final outcome model.
      # Based on the "fitOutcomeModelArgs" section of the JSON.
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # "modelType": "cox"
        stratified = TRUE, # "stratified": true
        useCovariates = FALSE, # "useCovariates": false
        inversePtWeighting = FALSE, # "inversePtWeighting": false
        prior = Cyclops::createPrior(
          priorType = "laplace", # "prior.priorType": "laplace"
          useCrossValidation = TRUE # "prior.useCrossValidation": true
        ),
        control = Cyclops::createControl(
          tolerance = 2e-7, # "control.tolerance": 2e-7
          cvType = "auto", # "control.cvType": "auto"
          fold = 10, # "control.fold": 10
          cvRepetitions = 10, # "control.cvRepetitions": 10
          noiseLevel = "quiet", # "control.noiseLevel": "quiet"
          resetCoefficients = TRUE, # "control.resetCoefficients": true
          startingVariance = 0.01 # "control.startingVariance": 0.01
        )
      )
      
      # Settings for creating the final study population after data extraction.
      # Based on the "createStudyPopArgs" section of the JSON.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # "restrictToCommonPeriod": false
        firstExposureOnly = FALSE, # "firstExposureOnly": false
        washoutPeriod = 0, # "washoutPeriod": 0
        removeDuplicateSubjects = "keep all", # "removeDuplicateSubjects": "keep all"
        censorAtNewRiskWindow = FALSE, # "censorAtNewRiskWindow": false
        removeSubjectsWithPriorOutcome = FALSE, # "removeSubjectsWithPriorOutcome": false
        priorOutcomeLookback = 99999, # "priorOutcomeLookBack": 99999
        minDaysAtRisk = 1, # "timeAtRisks.minDaysAtRisk": 1
        # TAR settings from the loop variable
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t]
      )

      # Append the complete settings for this analysis variant to the list.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study period: %s-%s; TAR: %s; PS: %s",
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

# Create the final CohortMethod module specification using the list of analyses.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# ===========廣===========
# Create and Save Analysis Specifications
#
# This final section assembles all the module specifications into a single
# analysis specification object and saves it as a JSON file.
# ===========狹===========
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() %>%
  Strategus::addSharedResources(cohortDefinitionShared) %>%
  Strategus::addSharedResources(negativeControlsShared) %>%
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the specifications to a JSON file. The path uses the 'name' from the
# original JSON specification.
outputFolder <- file.path("inst", analysisName)
if (!dir.exists(outputFolder)) {
  dir.create(outputFolder, recursive = TRUE)
}
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path(outputFolder, paste0(analysisName, "AnalysisSpecification.json"))
)