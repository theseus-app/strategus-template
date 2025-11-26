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

# =========== Define analysis settings from JSON specification ===========
# This section translates the settings from the <Analysis Specifications> JSON
# into R objects that will be used to build the Strategus analysis specification.
# Each setting is annotated with a reference to the corresponding JSON key.

# Analysis Name
# Corresponds to: "name"
analysisName <- "semaglutideandnaion"

# Cohort Definition IDs from ATLAS
# Corresponds to: "cohortDefinitions"
targetCohortId <- 1794126
comparatorCohortId <- 1794132
outcomeCohortId <- 1794131

# Negative Control Concept Set ID from ATLAS
# Corresponds to: "negativeControlConceptSet"
negativeControlConceptSetId <- 1888110

# Endpoint for ATLAS WebAPI
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"
# ========================================================================


# Shared Resources -------------------------------------------------------------
# This section defines resources that are shared across different analysis modules,
# such as cohort definitions and concept sets.

# --- Cohort Definitions ---
# Retrieve cohort definitions from ATLAS using the specified IDs.
# These will be used by CohortGenerator to instantiate the cohorts.
# Corresponds to: "cohortDefinitions"
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    targetCohortId,     # targetCohort: target1
    comparatorCohortId, # comparatorCohort: comparator1
    outcomeCohortId     # outcomeCohort: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohort IDs for use within the Strategus study.
# This makes it easier to reference them in the analysis settings.
# Target cohort is always ID 1, Comparator is ID 2, Outcomes start from ID 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == targetCohortId, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == comparatorCohortId, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == outcomeCohortId, ]$cohortId <- 3

# --- Negative Control Outcomes ---
# Retrieve the concept set for negative controls, resolve it to a list of concepts,
# and format it as a negative control outcome cohort set.
# Corresponds to: "negativeControlConceptSet"
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = negativeControlConceptSetId,
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
  # Assign unique cohort IDs for negative controls, starting from 101
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)


# Sanity check to ensure there are no duplicate cohort IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts we'll use in the CohortMethod analysis
# --- Outcomes of Interest ---
# Defines the primary outcomes for the study.
# Corresponds to: "cohortDefinitions.outcomeCohort"
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Filter for the re-numbered outcome cohort ID
  mutate(outcomeCohortId = cohortId, 
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Standard clean window for outcome definition

# --- Target and Comparator Pairs ---
# Defines the Target-Comparator pairs for the comparative cohort analysis.
# Corresponds to: "cohortDefinitions.targetCohort", "cohortDefinitions.comparatorCohort"
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = cohortDefinitionSet$cohortName[cohortDefinitionSet$cohortId == 1],
  comparatorCohortId = 2,
  comparatorCohortName = cohortDefinitionSet$cohortName[cohortDefinitionSet$cohortId == 2]
)

# --- Covariate Selection ---
# The JSON specifies empty arrays for conceptsToInclude and conceptsToExclude,
# so we will use the default covariate settings from FeatureExtraction.
# This means most standard covariate domains will be included, and CohortMethod
# will automatically exclude the target and comparator drug concepts.
# Corresponds to: "covariateSelection"


# CohortGeneratorModule --------------------------------------------------------
# This module is responsible for instantiating all cohorts.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Define the main cohort definitions as a shared resource
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Define the negative control outcome cohorts as a shared resource
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
# Create the module specifications for CohortGenerator
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule -----------------------------------------------------
# This module runs diagnostics on the instantiated cohorts.
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

# CohortMethodModule -----------------------------------------------------------
# This module performs the comparative cohort analysis.

# --- Study Period ---
# Defines the overall period for the study.
# Corresponds to: "getDbCohortMethodDataArgs.studyPeriods"
studyPeriods <- tibble(
  studyStartDate = c("20171201"), # YYYYMMDD format
  studyEndDate   = c("20231231")  # YYYYMMDD format
)

# --- Time-at-Risk (TAR) ---
# Defines the time window where outcomes are counted relative to cohort entry.
# Corresponds to: "createStudyPopArgs.timeAtRisks"
timeAtRisks <- tibble(
  label = "On Treatment", # A descriptive label for this TAR
  riskWindowStart  = 1,
  startAnchor = "cohort start",
  riskWindowEnd  = 0,
  endAnchor = "cohort end"
) 

# --- Propensity Score (PS) Adjustment Settings ---
# Defines the different PS adjustment strategies to be executed.
# Corresponds to: "propensityScoreAdjustment.psSettings"

# PS Method 1: Matching
# Corresponds to: "psSettings[0].matchOnPsArgs"
matchOnPsArgsList <- tibble(
  label = "1-to-1 Matching", # Descriptive label
  maxRatio  = 1,             # 1-to-1 matching
  caliper = 0.2,             # Caliper of 0.2 standard deviations of the logit PS
  caliperScale  = "standardized logit"
) 

# PS Method 2: Stratification
# Corresponds to: "psSettings[1].stratifyByPsArgs"
stratifyByPsArgsList <- tibble(
  label = "5 PS Strata",   # Descriptive label
  numberOfStrata  = 5,     # 5 strata based on PS
  baseSelection = "all"
) 

# --- Combine PS configurations ---
# Build a single list of PS configurations to iterate over.
# This loop structure allows for easily adding more PS adjustment methods.
psConfigList <- list()

# Add matching configurations
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

# Add stratification configurations
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

# --- Iterate through all analysis setting combinations ---
# This set of nested loops creates a full factorial design of all specified
# settings (study periods, TARs, PS methods). Each combination will be a
# separate CohortMethod analysis.
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment arguments based on the current iteration
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

      # --- Define arguments for each step of the CohortMethod pipeline ---
      
      # Define arguments for retrieving data from the database.
      # Corresponds to: "getDbCohortMethodDataArgs"
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        restrictToCommonPeriod = TRUE,
        firstExposureOnly = FALSE,
        removeDuplicateSubjects = "keep all",
        washoutPeriod = 0,
        maxCohortSize = 0, # 0 = no maximum size
        covariateSettings = FeatureExtraction::createDefaultCovariateSettings(
          addDescendantsToExclude = TRUE # Auto-exclude T and C concepts and their descendants
        )
      )
      
      # Define arguments for creating the study population.
      # Corresponds to: "createStudyPopArgs"
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999, # Look back entire history for prior outcomes
        restrictToCommonPeriod = FALSE, # Common period was already applied in getDbCohortMethodDataArgs
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = TRUE
      )
      
      # Define arguments for creating the propensity score model.
      # Corresponds to: "propensityScoreAdjustment.createPsArgs"
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace", 
          useCrossValidation = TRUE,
          exclude = c(0) # Do not regularize the intercept
        ),
        control = Cyclops::createControl(
          tolerance = 2e-07, 
          cvType = "auto", 
          cvRepetitions = 10,
          noiseLevel = "silent", 
          resetCoefficients = TRUE, 
          startingVariance = 0.01
        )
      )
      
      # Define arguments for fitting the outcome model.
      # Corresponds to: "fitOutcomeModelArgs"
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = TRUE, # Stratify by matched/stratified sets
        useCovariates = FALSE, # Do not include covariates in the outcome model
        inversePtWeighting = FALSE,
        prior = Cyclops::createPrior(
          priorType = "laplace", 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          tolerance = 2e-07,
          cvType = "auto",
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # --- Define T-C-O combinations ---
      # Create the list of outcomes, combining primary outcomes and negative controls.
      outcomeList <- append(
        # Primary outcomes of interest
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
      
      # Create the list of Target, Comparator, and Outcome combinations.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList
        )
      }

      # --- Assemble the analysis settings ---
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "T: %s, C: %s, Study Period: %s-%s; TAR: %s; PS: %s",
          cmTcList$targetCohortName[1],
          cmTcList$comparatorCohortName[1],
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
        computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
          maxCohortSize = 250000
        ),
        computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
          maxCohortSize = 250000
        ),
        fitOutcomeModelArgs = fitOutcomeModelArgs
      )
      analysisId <- analysisId + 1
    }
  }
}

# Create the module specifications for CohortMethod, including all defined analyses.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the analysis specifications ------------------------------------------
# This combines all the module specifications into a single JSON object
# that can be executed by Strategus.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Add shared resources
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  # Add module specifications
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# This file will be used as input for the Strategus::execute() function.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", analysisName, paste0(analysisName, "AnalysisSpecification.json"))
)