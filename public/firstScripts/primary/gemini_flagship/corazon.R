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

# ===========-===================================================================
#
# --- Shared Resources ---
#
# This section defines the cohorts and concept sets that will be used
# throughout the analysis. These are defined once and then referenced
# by the analysis modules.
#
# ===============================================================================

# --- WebAPI Connections ---
# Point to the OHDSI Atlas instance from which to retrieve cohorts and concept sets
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# --- Cohort Definitions ---
# Retrieve cohort definitions from Atlas
# Per "cohortDefinitions" in Analysis Specifications
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: "target1" from Analysis Specifications
    1794132, # Comparator: "comparator1" from Analysis Specifications
    1794131  # Outcome: "outcome1" from Analysis Specifications
  ),
  generateStats = TRUE
)

# --- Cohort Re-numbering ---
# Strategus works best with simple integer IDs (e.g., 1, 2, 3)
# We re-map the Atlas cohort IDs to these simple IDs and will use the
# simple IDs throughout the rest of the script.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# --- Negative Control Outcomes ---
# Per "negativeControlConceptSet" in Analysis Specifications
# Retrieve the concept set for negative controls
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # id: 1888110, name: "negative"
  baseUrl = baseUrl
) %>%
  # Resolve the concept set into a list of concepts
  ROhdsiWebApi::resolveConceptSet(
    baseUrl = baseUrl
  ) %>%
  # Get the details of the concepts
  ROhdsiWebApi::getConcepts(
    baseUrl = baseUrl
  ) %>%
  # Rename columns for consistency
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  # Assign unique cohort IDs for each negative control, starting from 101
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)


# --- Data Frames for Analysis Settings ---
# Create data frames to hold the various cohorts and settings we'll use in the analysis modules.

# Outcomes of interest list
# Using the re-numbered outcome cohort (ID=3)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # A 365 day clean window is standard

# Target and Comparator list for the CohortMethod analysis
# Using re-numbered cohort IDs and names from "cohortDefinitions" in Analysis Specifications
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Per "covariateSelection" in Analysis Specifications, both conceptsToInclude
# and conceptsToExclude are empty. We create empty dataframes to reflect this.
# These could be populated to include/exclude specific concepts from the default
# covariate settings used in propensity score model building.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)
includedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)


# ===============================================================================
#
# --- Module Settings ---
#
# This section defines the settings for each of the HADES modules that will be
# executed as part of the analysis.
#
# ===============================================================================

# --- CohortGeneratorModule Settings ---
# This module is responsible for instantiating the cohorts on the CDM.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Define the cohort definitions as a shared resource
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Define the negative control outcome cohorts as a shared resource
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
# Define the module specifications, including whether to generate cohort statistics
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# --- CohortDiagnosticsModule Settings ---
# This module runs a standard set of diagnostics on the instantiated cohorts.
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

# --- CohortMethodModule Settings ---
# This is the core module for the comparative cohort analysis.

# Define study periods per "getDbCohortMethodDataArgs:studyPeriods" in Analysis Specifications
studyPeriods <- tibble(
  studyStartDate = c("20100101"),
  studyEndDate   = c("20191231")
)

# Define time-at-risk (TAR) windows per "createStudyPopArgs:timeAtRisks" in Analysis Specifications
timeAtRisks <- tibble(
  label = c("1 day after start to cohort end"), # A descriptive label for this TAR
  riskWindowStart  = c(1),                     # riskWindowStart: 1
  startAnchor = c("cohort start"),             # startAnchor: "cohort start"
  riskWindowEnd  = c(0),                       # riskWindowEnd: 0
  endAnchor = c("cohort end")                  # endAnchor: "cohort end"
)

# Per "propensityScoreAdjustment:psSettings", matchOnPsArgs is null, so this tibble is empty.
matchOnPsArgsList <- tibble(
  label = c(),
  maxRatio  = c(),
  caliper = c(),
  caliperScale  = c()
)

# Define propensity score stratification settings per "propensityScoreAdjustment:psSettings:stratifyByPsArgs"
stratifyByPsArgsList <- tibble(
  label = c("5 strata"),        # A descriptive label for this PS adjustment strategy
  numberOfStrata  = c(5),       # numberOfStrata: 5
  baseSelection = c("all"),     # baseSelection: "all"
)

# --- PS Configuration List Builder ---
# This logic converts the tibbles above into a list of PS configurations that the main loop can iterate through.
psConfigList <- list()
# The matchOnPsArgsList is empty, so this block will not run.
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
# Convert the stratifyByPsArgsList tibble into a configuration list item.
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


# --- Main Analysis Loop ---
# Iterate through all combinations of study periods, TARs, and PS settings to create a list of analyses.
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Based on the method in psConfigList, create the appropriate PS adjustment arguments.
      # In this case, it will be "stratify".
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

      # Use default covariate settings, as "covariateSelection" in the spec is empty.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create a list of outcomes, combining the main outcome of interest and all negative controls.
      outcomeList <- append(
        # Main outcome of interest
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA
          )
        }),
        # Negative control outcomes
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Define the target-comparator-outcomes list.
      # This specifies which T-C pairs are to be analyzed with which outcomes.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude concepts from being used as covariates.
          # The list is empty per "covariateSelection:conceptsToExclude" in the spec.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Settings for retrieving data from the CDM.
      # Per "getDbCohortMethodDataArgs" in Analysis Specifications.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # maxCohortSize: 0 (means no limit)
        covariateSettings = covariateSettings
      )

      # Settings for creating the propensity score model.
      # Per "propensityScoreAdjustment:createPsArgs" in Analysis Specifications.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,   # maxCohortSizeForFitting: 250000
        errorOnHighCorrelation = TRUE,      # errorOnHighCorrelation: true
        prior = Cyclops::createPrior(       # prior settings
          priorType = "laplace",            # priorType: "laplace"
          useCrossValidation = TRUE         # useCrossValidation: true
        ),
        control = Cyclops::createControl(   # control settings
          tolerance = 2e-7,                 # tolerance: 2e-7
          cvType = "auto",                  # cvType: "auto"
          fold = 10,                        # fold: 10
          cvRepetitions = 10,               # cvRepetitions: 10
          noiseLevel = "silent",            # noiseLevel: "silent"
          resetCoefficients = TRUE,         # resetCoefficients: true
          startingVariance = 0.01           # startingVariance: 0.01
        )
      )

      # Settings for calculating covariate balance after PS adjustment.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Settings for fitting the outcome model.
      # Per "fitOutcomeModelArgs" in Analysis Specifications.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",                  # modelType: "cox"
        stratified = TRUE,                  # stratified: true
        useCovariates = FALSE,              # useCovariates: false
        inversePtWeighting = FALSE,         # inversePtWeighting: false
        prior = Cyclops::createPrior(       # prior settings
          priorType = "laplace",            # priorType: "laplace"
          useCrossValidation = TRUE         # useCrossValidation: true
        ),
        control = Cyclops::createControl(   # control settings
          tolerance = 2e-7,                 # tolerance: 2e-7
          cvType = "auto",                  # cvType: "auto"
          fold = 10,                        # fold: 10
          cvRepetitions = 10,               # cvRepetitions: 10
          noiseLevel = "quiet",             # noiseLevel: "quiet"
          resetCoefficients = TRUE,         # resetCoefficients: true
          startingVariance = 0.01           # startingVariance: 0.01
        )
      )
      
      # Settings for creating the final study population.
      # Per "createStudyPopArgs" in Analysis Specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,        # restrictToCommonPeriod: false
        firstExposureOnly = FALSE,             # firstExposureOnly: false
        washoutPeriod = 0,                     # washoutPeriod: 0
        removeDuplicateSubjects = "keep all",  # removeDuplicateSubjects: "keep all"
        censorAtNewRiskWindow = FALSE,         # censorAtNewRiskWindow: false
        removeSubjectsWithPriorOutcome = TRUE, # removeSubjectsWithPriorOutcome: true
        priorOutcomeLookback = 99999,          # priorOutcomeLookBack: 99999
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1                      # minDaysAtRisk: 1
      )


      # --- Assemble the Analysis Settings ---
      # Each unique combination of settings becomes a single analysis with a unique ID.
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

# --- Create the CohortMethod Module Specifications ---
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)


# ===============================================================================
#
# --- Create and Save Analysis Specifications ---
#
# This final section combines all the module specifications into a single
# analysis specification object and saves it as a JSON file.
#
# ===============================================================================
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Add shared resources
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  # Add module specifications
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications object to a JSON file.
# The file name is based on the study name "corazon" from the Analysis Specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "corazon", "corazonAnalysisSpecification.json")
)