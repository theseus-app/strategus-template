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

# ===========-==================================================================
# --- Shared Resources Section ---
# This section defines resources that are shared across different modules,
# such as cohort definitions and concept sets.
# ==============================================================================

# --- Cohort Definitions ---
# Get the list of cohorts from the ATLAS-DEMO WebAPI
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Define the cohort IDs to be imported from ATLAS.
# These IDs are sourced from the 'cohortDefinitions' section in the Analysis Specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1 from cohortDefinitions.targetCohort
    1794132, # Comparator: comparator1 from cohortDefinitions.comparatorCohort
    1794131  # Outcome: outcome1 from cohortDefinitions.outcomeCohort
  ),
  generateStats = TRUE
)

# Re-numbering cohort IDs for internal consistency within the Strategus framework.
# It is recommended to use simple, sequential IDs (e.g., 1, 2, 3) for clarity.
# The mapping is based on the imported cohort definitions.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Target mapped to ID 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Comparator mapped to ID 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Outcome mapped to ID 3

# --- Negative Control Outcomes ---
# Importing the negative control concept set from ATLAS.
# Sourced from: negativeControlConceptSet section in Analysis Specifications.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # ID for "negative" concept set
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
  # Assigning cohort IDs starting from 101 to avoid conflicts with study cohorts (1, 2, 3).
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)

# Sanity check to ensure no cohort IDs are duplicated between the main cohorts and negative controls.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}


# ==============================================================================
# --- Helper Data Frames Section ---
# This section creates data frames to organize cohorts and analysis parameters
# for easier processing in the main analysis loop.
# ==============================================================================

# Create a data frame for the outcome(s) of interest.
# This list will be used to generate outcome cohorts in the CohortMethod analysis.
# Sourced from: cohortDefinitions.outcomeCohort in Analysis Specifications.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Filter for the re-numbered outcome cohort ID
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName)

# Create a data frame defining the Target-Comparator pairs for the CohortMethod analysis.
# Sourced from: cohortDefinitions.targetCohort and cohortDefinitions.comparatorCohort.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Define concepts to exclude from covariates. 
# The Analysis Specifications for covariateSelection.conceptsToExclude is empty.
# By default, CohortMethod will exclude concepts related to the target and comparator exposures.
# This data frame is intentionally left empty as per the specifications.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)


# ==============================================================================
# --- Module Specifications Section ---
# This section defines the settings for each Strategus module that will be
# executed in the analysis pipeline.
# ==============================================================================

# --- CohortGeneratorModule Settings ---
# This module is responsible for generating the cohort instances from the definitions.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Shared resource for cohort definitions, making them available to other modules.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Shared resource for negative control outcome definitions.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
# Module specifications for cohort generation. generateStats = TRUE enables summary statistics.
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
  minCharacterizationMean = 0.01
)

# --- CohortMethodModule Settings ---
# This section defines all the parameters for the comparative cohort analysis.

# Define study periods. An empty string for start/end date indicates no date restriction.
# Sourced from: getDbCohortMethodDataArgs.studyPeriods in Analysis Specifications (which is null).
studyPeriods <- tibble(
  studyStartDate = c(""), # YYYYMMDD format, empty for no restriction
  studyEndDate   = c("")  # YYYYMMDD format, empty for no restriction
)

# Define Time-at-Risks (TARs). These specify the period during which outcomes are counted.
# Sourced from: createStudyPopArgs.timeAtRisks in Analysis Specifications.
timeAtRisks <- tibble(
  label = c("On Treatment", "Intent to Treat"),
  # TAR 1: Starts at cohort start, ends at cohort end.
  riskWindowStart  = c(0, 0),
  startAnchor = c("cohort start", "cohort start"), 
  # TAR 2: Starts at cohort start, follows for a fixed duration (9999 days).
  riskWindowEnd  = c(0, 9999),
  endAnchor = c("cohort end", "cohort start"),
  minDaysAtRisk = c(1, 1) # Minimum number of days at risk.
) 

# Define Propensity Score matching settings.
# Sourced from: propensityScoreAdjustment.psSettings.matchOnPsArgs in Analysis Specifications.
matchOnPsArgsList <- tibble(
  label = c("1-to-1 Matching"),
  maxRatio  = c(1),                   # maxRatio = 1
  caliper = c(0.2),                 # caliper = 0.2
  caliperScale  = c("standardized logit") # caliperScale = "standardized logit"
) 

# Define Propensity Score stratification settings. This is null in the specifications.
# An empty tibble is created to ensure the processing loop handles this correctly.
stratifyByPsArgsList <- tibble()

# Build a single PS configuration list from the settings defined above.
# This allows the analysis loop to iterate through all desired PS adjustment methods.
psConfigList <- list()

# Convert the matchOnPsArgsList tibble into the psConfigList format.
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

# Convert the stratifyByPsArgsList tibble into the psConfigList format.
# This block will not execute as stratifyByPsArgsList is empty, as per specifications.
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


# --- Analysis Definition Loop ---
# This section iterates through all combinations of settings to create a list of
# analyses to be executed by the CohortMethod module.

cmAnalysisList <- list()
analysisId <- 1

# Loop over each study period defined.
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  # Loop over each Time-at-Risk defined.
  for (t in seq_len(nrow(timeAtRisks))) {
    
    # Loop over each Propensity Score configuration.
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Create PS adjustment arguments based on the method ('match' or 'stratify').
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

      # Define covariate settings using defaults. Exclusions are handled later.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(addDescendantsToExclude = TRUE)
      
      # Combine the user-defined outcomes with the negative control outcomes.
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
      
      # Define the list of Target-Comparator-Outcomes analyses to run.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # --- Define CohortMethod Function Arguments based on Analysis Specifications ---
      
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # Sourced from getDbCohortMethodDataArgs.maxCohortSize, 0 = no limit
        covariateSettings = covariateSettings
      )

      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,        # Sourced from createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE,             # Sourced from createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0,                     # Sourced from createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all",  # Sourced from createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE,         # Sourced from createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # Sourced from createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 365,            # Sourced from createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999 # Default value, not in specifications
      )

      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # Sourced from propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE,    # Sourced from propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE,
        estimator = "att",
        prior = Cyclops::createPrior(      # Sourced from propensityScoreAdjustment.createPsArgs.prior
          priorType = "laplace", 
          exclude = c(0),
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(  # Sourced from propensityScoreAdjustment.createPsArgs.control
          noiseLevel = "silent", 
          cvType = "auto", 
          seed = 1,
          resetCoefficients = TRUE, 
          tolerance = 2e-07, 
          cvRepetitions = 10,
          fold = 10,
          startingVariance = 0.01
        )
      )

      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(maxCohortSize = 250000)
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(maxCohortSize = 250000)

      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",            # Sourced from fitOutcomeModelArgs.modelType
        stratified = FALSE,           # Sourced from fitOutcomeModelArgs.stratified
        useCovariates = FALSE,        # Sourced from fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE,   # Sourced from fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior( # Sourced from fitOutcomeModelArgs.prior
          priorType = "laplace", 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl( # Sourced from fitOutcomeModelArgs.control
          cvType = "auto", 
          seed = 1,
          resetCoefficients = TRUE,
          startingVariance = 0.01, 
          tolerance = 2e-07, 
          cvRepetitions = 10, 
          fold = 10,
          noiseLevel = "quiet"
        )
      )

      # --- Assemble and Store the Analysis Settings ---
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "T-C: %s-%s; TAR: %s; PS: %s",
          cmTcList$targetCohortName[1],
          cmTcList$comparatorCohortName[1],
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

# Create the final CohortMethod module specifications, packaging up all defined analyses.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# ==============================================================================
# --- Analysis Specifications Creation ---
# This section combines all module specifications into a single JSON file
# that can be executed by Strategus.
# ==============================================================================
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Add shared resources (available to all modules)
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  # Add module specifications
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the specifications to a JSON file.
# The file path uses the study 'name' from the Analysis Specifications ("tramadolcodein").
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "tramadolcodein", "tramadolcodeinAnalysisSpecification.json")
)