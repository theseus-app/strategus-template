################################################################################
# This script uses the OHDSI Strategus framework to create a JSON file 
# defining a comparative cohort study.
#
# The settings are derived from the provided <Analysis Specifications>.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
################################################################################
library(dplyr)
library(Strategus)

# =========== General Settings =================================================
# The 'name' of the analysis, used for naming the output file.
# From <Analysis Specifications> -> "name"
analysisName <- "antivegfkidney"

# =========== Shared Resources =================================================
# Shared resources are components used across different Strategus modules.
# This includes cohort definitions and concept sets.

# --- Cohort Definitions -------------------------------------------------------
# We start by retrieving the cohort definitions from a WebAPI instance.
# The baseUrl can be pointed to any instance, here we use the OHDSI demo Atlas.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Define the cohort IDs based on <Analysis Specifications> -> "cohortDefinitions"
targetCohortId <- 1794126      # "targetCohort"
comparatorCohortId <- 1794132  # "comparatorCohort"
outcomeCohortId <- 1794131     # "outcomeCohort"

# Export the cohort definitions from WebAPI into a data frame.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    targetCohortId,
    comparatorCohortId,
    outcomeCohortId
  ),
  generateStats = TRUE
)

# It's a good practice in Strategus to use small, sequential integer IDs (e.g., 1, 2, 3)
# for cohorts within the analysis specification. We map the WebAPI IDs to these new IDs.
cohortDefinitionSet <- cohortDefinitionSet %>%
  mutate(cohortId = case_when(
    cohortId == targetCohortId ~ 1,
    cohortId == comparatorCohortId ~ 2,
    cohortId == outcomeCohortId ~ 3,
    TRUE ~ cohortId
  ))

# --- Negative Control Outcomes ------------------------------------------------
# Negative controls are concepts believed not to be caused by the exposure.
# They are used for empirical calibration. We retrieve them from a concept set.
# From <Analysis Specifications> -> "negativeControlConceptSet"
negativeControlConceptSetId <- 1888110 # "id"

# Retrieve the concept set, resolve it to standard concepts, and format it
# for use in Strategus. New cohort IDs are assigned starting from 101.
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
  # Assign cohort IDs starting from 101 to avoid collision with main cohorts
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)

# Safety check for duplicate cohort IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# =========== Analysis-Specific Settings =======================================
# Here we define data frames that hold the specific settings for the
# CohortMethod analysis, such as target-comparator pairs, outcomes, and TARs.

# --- Outcomes of Interest -----------------------------------------------------
# Create a data frame for the primary outcomes of interest.
# From <Analysis Specifications> -> "cohortDefinitions" -> "outcomeCohort"
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Corresponds to the re-numbered outcome cohort
  mutate(outcomeCohortId = cohortId, 
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName)

# --- Target-Comparator List ---------------------------------------------------
# Define the target and comparator cohort pairs for the analysis.
# From <Analysis Specifications> -> "cohortDefinitions"
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1", # From "targetCohort" -> "name"
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1" # From "comparatorCohort" -> "name"
)

# --- Covariate Selection ------------------------------------------------------
# Define concepts to include or exclude from the covariate construction.
# In this case, both lists are empty as specified in the JSON.
# From <Analysis Specifications> -> "covariateSelection"
excludedCovariateConcepts <- data.frame(
  conceptId = character(),
  conceptName = character()
)

# includedCovariateConcepts is not used as per the specifications
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )


# =========== Strategus Module Specifications ==================================
# Now we define the settings for each Strategus module that will be executed.

# --- CohortGeneratorModule ----------------------------------------------------
# This module is responsible for generating the cohort instances based on the
# definitions provided.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for the main cohorts.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

# Create shared resource specifications for the negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Define the module specifications for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# --- CohortDiagnosticsModule --------------------------------------------------
# This module runs a set of diagnostics on the generated cohorts to assess their
# quality and characteristics. Settings are standard defaults as none were specified.
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

# --- CohortMethodModule -------------------------------------------------------
# This is the core module for the comparative cohort study.

# --- Study Window -------------------------------------------------------------
# If studyStartDate and studyEndDate are null, we use empty strings, which
# tells CohortMethod to use the full available observation period.
# From <Analysis Specifications> -> "getDbCohortMethodDataArgs" -> "studyPeriods"
studyPeriods <- tibble(
  studyStartDate = c(""), # NULL corresponds to empty string
  studyEndDate   = c("")  # NULL corresponds to empty string
)

# --- Time-at-Risk (TAR) Definitions -------------------------------------------
# Define the time-at-risk windows for the analysis.
# From <Analysis Specifications> -> "createStudyPopArgs" -> "timeAtRisks"
timeAtRisks <- tibble(
  label = c("On Treatment"), # A descriptive label for this TAR
  riskWindowStart  = c(0),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
) 

# --- Propensity Score (PS) Adjustment Strategy Definitions --------------------
# From <Analysis Specifications> -> "propensityScoreAdjustment" -> "psSettings"

# Matching on Propensity Score settings
matchOnPsArgsList <- tibble(
  label = c("1-to-1 Matching"), # A descriptive label
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
) 

# Stratification on Propensity Score settings (not used in this analysis)
stratifyByPsArgsList <- tibble() # Empty as per specification

# --- Combine PS settings into a single configuration list ---------------------
# This loop structure allows for defining multiple PS strategies (e.g., both
# matching and stratification) to be run as part of the analysis.
psConfigList <- list()

# Convert the "match on PS" data frame into the required list format.
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

# Convert the "stratify by PS" data frame (if any) into the list format.
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

# --- Main Analysis Loop to Create CmAnalysis Objects --------------------------
# This set of nested loops iterates through all combinations of study periods,
# TARs, and PS strategies to create a list of analysis settings (`cmAnalysisList`).
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Based on the method ("match" or "stratify"), create the appropriate arguments object.
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

      # --- Define function arguments for CohortMethod -------------------------
      
      # Covariate settings: Using default settings as none were specified.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE,
        excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
      )

      # Create a list of all outcome cohorts (primary outcomes + negative controls).
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
      
      # Combine target, comparator, and outcomes into a single list structure.
      # Also specifies concepts to exclude from covariates for this T-C pair.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Note: The target and comparator drug concepts themselves are usually excluded
          # from the covariates. Since the concept IDs were not specified, we only
          # use the concepts from the `excludedCovariateConcepts` dataframe.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Settings for extracting data from the database.
      # From <Analysis Specifications> -> "getDbCohortMethodDataArgs"
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # 0 means no maximum size
        covariateSettings = covariateSettings
      )

      # Settings for creating the study population.
      # From <Analysis Specifications> -> "createStudyPopArgs"
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = TRUE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        # TAR settings are taken from the `timeAtRisks` data frame defined earlier
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1, # From "timeAtRisks" -> "minDaysAtRisk"
        maxDaysAtRisk = 99999 # Default: no maximum
      )

      # Settings for creating the propensity score model.
      # From <Analysis Specifications> -> "propensityScoreAdjustment" -> "createPsArgs"
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # Let Strategus continue if one PS model fails
        prior = Cyclops::createPrior(
          priorType = "laplace", # "prior" -> "priorType"
          useCrossValidation = TRUE  # "prior" -> "useCrossValidation"
        ),
        control = Cyclops::createControl(
          tolerance = 2e-7,       # "control" -> "tolerance"
          cvType = "auto",        # "control" -> "cvType"
          fold = 10,              # "control" -> "fold"
          cvRepetitions = 10,     # "control" -> "cvRepetitions"
          noiseLevel = "silent",  # "control" -> "noiseLevel"
          resetCoefficients = TRUE, # "control" -> "resetCoefficients"
          startingVariance = 0.01, # "control" -> "startingVariance"
          seed = 1 # for reproducibility
        )
      )

      # Settings for computing covariate balance (not specified, using defaults).
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )
      
      # Settings for fitting the outcome model.
      # From <Analysis Specifications> -> "fitOutcomeModelArgs"
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = FALSE, # This is FALSE for matching, TRUE for stratification
        useCovariates = FALSE,
        inversePtWeighting = FALSE,
        prior = Cyclops::createPrior(
          priorType = "laplace",      # "prior" -> "priorType"
          useCrossValidation = TRUE   # "prior" -> "useCrossValidation"
        ),
        control = Cyclops::createControl(
          tolerance = 2e-7,         # "control" -> "tolerance"
          cvType = "auto",          # "control" -> "cvType"
          fold = 10,                # "control" -> "fold"
          cvRepetitions = 10,       # "control" -> "cvRepetitions"
          noiseLevel = "quiet",     # "control" -> "noiseLevel"
          resetCoefficients = TRUE, # "control" -> "resetCoefficients"
          startingVariance = 0.01,  # "control" -> "startingVariance"
          seed = 1 # for reproducibility
        )
      )

      # --- Assemble the Analysis Specification --------------------------------
      # Each unique combination of settings is packaged into a `cmAnalysis` object.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(studyStartDate == "", "Full", studyStartDate),
          ifelse(studyEndDate == "", "History", studyEndDate),
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

# --- Create the CohortMethod Module Specification -----------------------------
# All the defined `cmAnalysis` objects are passed to the CohortMethod module.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# =========== Create and Save the Full Analysis Specifications =================
# All module specifications and shared resources are combined into a single
# analysis specification object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the specifications to a JSON file. This file can then be executed by Strategus.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", analysisName, paste0(analysisName, "AnalysisSpecification.json"))
)