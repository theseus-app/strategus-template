# This script generates a Strategus analysis specification JSON file
# based on the provided Analysis Specifications.
# It leverages the OHDSI Strategus and CohortMethod packages.

# Load required libraries
library(dplyr)
library(Strategus)
library(ROhdsiWebApi)
library(CohortGenerator)
library(CohortDiagnostics)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(jsonlite) # For parsing the analysis specifications JSON

# --- START Analysis Specifications from JSON ---
# This section parses the analysis specifications provided in JSON format
# and makes them accessible as R objects.
analysis_spec_json <- jsonlite::fromJSON(
  '{
    "name": "semaglutideandnaion",
    "cohortDefinitions": {
      "targetCohort": {
        "id": 1794126,
        "name": "target1"
      },
      "comparatorCohort": {
        "id": 1794132,
        "name": "comparator1"
      },
      "outcomeCohort": [
        {
          "id": 1794131,
          "name": "outcome1"
        }
      ]
    },
    "negativeControlConceptSet": {
      "id": 1888110,
      "name": "negative"
    },
    "covariateSelection": {
      "conceptsToInclude": [
        {
          "id": null,
          "name": ""
        }
      ],
      "conceptsToExclude": [
        {
          "id": null,
          "name": ""
        }
      ]
    },
    "getDbCohortMethodDataArgs": {
      "studyPeriods": [
        {
          "studyStartDate": 20171201,
          "studyEndDate": 20231231
        }
      ],
      "maxCohortSize": 0
    },
    "createStudyPopArgs": {
      "restrictToCommonPeriod": false,
      "firstExposureOnly": false,
      "washoutPeriod": 365,
      "removeDuplicateSubjects": "keep all",
      "censorAtNewRiskWindow": false,
      "removeSubjectsWithPriorOutcome": true,
      "priorOutcomeLookBack": 99999,
      "timeAtRisks": [
        {
          "riskWindowStart": 1,
          "startAnchor": "cohort start",
          "riskWindowEnd": 0,
          "endAnchor": "cohort end",
          "minDaysAtRisk": 1
        }
      ]
    },
    "propensityScoreAdjustment": {
      "psSettings": [
        {
          "matchOnPsArgs": {
            "maxRatio": 1,
            "caliper": 0.2,
            "caliperScale": "standardized logit"
          },
          "stratifyByPsArgs": null
        }
      ],
      "createPsArgs": {
        "maxCohortSizeForFitting": 250000,
        "errorOnHighCorrelation": true,
        "prior": {
          "priorType": "laplace",
          "useCrossValidation": true
        },
        "control": {
          "tolerance": 2e-7,
          "cvType": "auto",
          "fold": 10,
          "cvRepetitions": 10,
          "noiseLevel": "silent",
          "resetCoefficients": true,
          "startingVariance": 0.01
        }
      }
    },
    "fitOutcomeModelArgs": {
      "modelType": "cox",
      "stratified": false,
      "useCovariates": false,
      "inversePtWeighting": false,
      "prior": {
        "priorType": "laplace",
        "useCrossValidation": true
      },
      "control": {
        "tolerance": 2e-7,
        "cvType": "auto",
        "fold": 10,
        "cvRepetitions": 10,
        "noiseLevel": "quiet",
        "resetCoefficients": true,
        "startingVariance": 0.01
      }
    }
  }'
)

# Extract specific sections for easier reference
targetCohortSpec <- analysis_spec_json$cohortDefinitions$targetCohort
comparatorCohortSpec <- analysis_spec_json$cohortDefinitions$comparatorCohort
outcomeCohortSpec <- analysis_spec_json$cohortDefinitions$outcomeCohort[[1]] # Outcome is an array, take the first one
negativeControlConceptSetSpec <- analysis_spec_json$negativeControlConceptSet
covariateSelectionSpec <- analysis_spec_json$covariateSelection
getDbCohortMethodDataArgsSpec <- analysis_spec_json$getDbCohortMethodDataArgs
createStudyPopArgsSpec <- analysis_spec_json$createStudyPopArgs
propensityScoreAdjustmentSpec <- analysis_spec_json$propensityScoreAdjustment
fitOutcomeModelArgsSpec <- analysis_spec_json$fitOutcomeModelArgs

# --- END Analysis Specifications from JSON ---


# Shared Resources -------------------------------------------------------------
# Define the base URL for the WebAPI to retrieve cohort and concept set definitions.
# This value is taken directly from the template.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Fetch the target, comparator, and outcome cohort definitions from WebAPI.
# The IDs are extracted from the analysis_spec_json.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    targetCohortSpec$id,     # Target Cohort ID from analysis specifications
    comparatorCohortSpec$id, # Comparator Cohort ID from analysis specifications
    outcomeCohortSpec$id     # Outcome Cohort ID from analysis specifications
  ),
  generateStats = TRUE # Generate cohort statistics as part of the export.
)

# Re-number cohorts for internal consistency, mapping them to 1 (target), 2 (comparator), 3 (outcome).
# This simplifies referencing them consistently in the study.
cohortDefinitionSet[cohortDefinitionSet$cohortId == targetCohortSpec$id,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == comparatorCohortSpec$id,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == outcomeCohortSpec$id,]$cohortId <- 3

# Update cohort names to reflect their role (Target, Comparator, Outcome) and original name from spec.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1,]$cohortName <- paste("Target:", targetCohortSpec$name)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 2,]$cohortName <- paste("Comparator:", comparatorCohortSpec$name)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 3,]$cohortName <- paste("Outcome:", outcomeCohortSpec$name)


# Negative control outcomes
# Retrieve negative control concept set definition from WebAPI using the ID from analysis specifications.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = negativeControlConceptSetSpec$id, # Negative Control Concept Set ID from analysis specifications
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet( # Resolve concepts within the concept set
    baseUrl = baseUrl
  ) %>%
  ROhdsiWebApi::getConcepts( # Get detailed concept information
    baseUrl = baseUrl
  ) %>%
  rename(outcomeConceptId = "conceptId", # Rename conceptId to outcomeConceptId for clarity
         cohortName = "conceptName") %>%
  mutate(cohortId = row_number() + 100) %>% # Assign unique cohort IDs for negative controls (starting from 101)
  select(cohortId, cohortName, outcomeConceptId)


# Check for duplicate cohort IDs, which would cause issues in Strategus.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found in cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# Create data frames to hold the cohorts for specific analysis types ---------------

# Outcomes:
# Filters the re-numbered cohort definitions to get the main outcome cohort (ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not specified in analysis_spec.

# Target and Comparator for the CohortMethod analysis
# Uses the re-numbered IDs for target (1) and comparator (2).
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = cohortDefinitionSet[cohortDefinitionSet$cohortId == 1,]$cohortName,
  comparatorCohortId = 2,
  comparatorCohortName = cohortDefinitionSet[cohortDefinitionSet$cohortId == 2,]$cohortName
)

# For the CohortMethod LSPS, we define covariates to exclude.
# Based on Analysis Specifications: covariateSelection.conceptsToExclude.
# If 'id' is null or NA, it means no specific concepts are provided to exclude.
if (is.null(covariateSelectionSpec$conceptsToExclude) || all(is.na(covariateSelectionSpec$conceptsToExclude$id))) {
  excludedCovariateConcepts <- data.frame(conceptId = numeric(), conceptName = character())
} else {
  excludedCovariateConcepts <- data.frame(
    conceptId = as.numeric(covariateSelectionSpec$conceptsToExclude$id),
    conceptName = covariateSelectionSpec$conceptsToExclude$name
  ) %>% filter(!is.na(conceptId)) # Ensure only valid IDs are included.
}


# Optional: If you want to define covariates to include instead of including them all
# Based on Analysis Specifications: covariateSelection.conceptsToInclude.
# If 'id' is null or NA, it means no specific concepts are provided to include.
if (is.null(covariateSelectionSpec$conceptsToInclude) || all(is.na(covariateSelectionSpec$conceptsToInclude$id))) {
  includedCovariateConcepts <- data.frame(conceptId = numeric(), conceptName = character())
} else {
  includedCovariateConcepts <- data.frame(
    conceptId = as.numeric(covariateSelectionSpec$conceptsToInclude$id),
    conceptName = covariateSelectionSpec$conceptsToInclude$name
  ) %>% filter(!is.na(conceptId)) # Ensure only valid IDs are included.
}


# CohortGeneratorModule --------------------------------------------------------
# Creates shared resource specifications for cohort definitions and negative controls,
# and then generates module specifications for the CohortGenerator module.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for the main cohort definitions (T, C, O).
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource specifications for negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Detect the first occurrence of the negative control outcome.
  detectOnDescendants = TRUE # Include descendants of the concepts when detecting negative controls.
)

# Create module specifications for CohortGenerator, indicating to generate cohort stats.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)


# CohortDiagnoticsModule Settings ---------------------------------------------
# Creates module specifications for the CohortDiagnostics module.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Runs various diagnostics for all defined cohorts (target, comparator, outcome, negative controls).
# Settings for diagnostics are based on template defaults, as not explicitly overridden in spec.
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # All cohort IDs (T, C, O)
  runInclusionStatistics = TRUE,
  runIncludedSourceConcepts = TRUE,
  runOrphanConcepts = TRUE,
  runTimeSeries = FALSE,
  runVisitContext = TRUE,
  runBreakdownIndexEvents = TRUE,
  runIncidenceRate = TRUE,
  runCohortRelationship = TRUE,
  runTemporalCohortCharacterization = TRUE,
  minCharacterizationMean = 0.01 # Minimum mean frequency for characterization.
)


# CohortMethodModule -----------------------------------------------------------

# Study periods for restricting the study data retrieval.
# Taken from getDbCohortMethodDataArgs.studyPeriods in analysis specifications.
studyPeriods <- tibble::tibble(
  studyStartDate = getDbCohortMethodDataArgsSpec$studyPeriods$studyStartDate,
  studyEndDate   = getDbCohortMethodDataArgsSpec$studyPeriods$studyEndDate
)

# Time-at-risks (TARs) for the outcomes of interest.
# Taken from createStudyPopArgs.timeAtRisks in analysis specifications.
# A label is added for descriptive purposes in the analysis description.
timeAtRisks <- tibble::tibble(
  label = c("Primary_TAR"), # Descriptive label for the time-at-risk window.
  riskWindowStart  = createStudyPopArgsSpec$timeAtRisks$riskWindowStart,
  startAnchor = createStudyPopArgsSpec$timeAtRisks$startAnchor,
  riskWindowEnd  = createStudyPopArgsSpec$timeAtRisks$riskWindowEnd,
  endAnchor = createStudyPopArgsSpec$timeAtRisks$endAnchor,
  minDaysAtRisk = createStudyPopArgsSpec$timeAtRisks$minDaysAtRisk
)


# Propensity Score settings - match on PS
# Extracts settings for propensity score matching from propensityScoreAdjustment.psSettings.
# If matchOnPsArgs is null, an empty tibble is created.
if (!is.null(propensityScoreAdjustmentSpec$psSettings[[1]]$matchOnPsArgs)) {
  matchOnPsArgsList <- tibble::tibble(
    label = c("1:1 Matching on Std. Logit PS"), # Descriptive label for PS matching.
    maxRatio  = propensityScoreAdjustmentSpec$psSettings[[1]]$matchOnPsArgs$maxRatio,
    caliper = propensityScoreAdjustmentSpec$psSettings[[1]]$matchOnPsArgs$caliper,
    caliperScale  = propensityScoreAdjustmentSpec$psSettings[[1]]$matchOnPsArgs$caliperScale
  )
} else {
  matchOnPsArgsList <- tibble::tibble(
    label = character(),
    maxRatio  = numeric(),
    caliper = numeric(),
    caliperScale  = character()
  )
}


# Propensity Score settings - stratify by PS
# Extracts settings for propensity score stratification from propensityScoreAdjustment.psSettings.
# If stratifyByPsArgs is null, an empty tibble is created.
if (!is.null(propensityScoreAdjustmentSpec$psSettings[[1]]$stratifyByPsArgs)) {
  stratifyByPsArgsList <- tibble::tibble(
    label = c("Stratification by PS"), # Descriptive label for PS stratification.
    numberOfStrata  = propensityScoreAdjustmentSpec$psSettings[[1]]$stratifyByPsArgs$numberOfStrata,
    baseSelection = propensityScoreAdjustmentSpec$psSettings[[1]]$stratifyByPsArgs$baseSelection
  )
} else {
  stratifyByPsArgsList <- tibble::tibble(
    label = character(),
    numberOfStrata  = numeric(),
    baseSelection = character()
  )
}


# Build a single PS configuration list (each entry has: method, label, params)
# This loop converts the tibbles into a list of standardized PS configurations.
psConfigList <- list()

# If a data frame for "match on PS" exists and has rows, convert each row to a config.
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

# If a data frame for "stratify by PS" exists and has rows, convert each row to a config.
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


# Iterate through all analysis setting combinations
# This section constructs all `CohortMethod::createCmAnalysis` objects.
cmAnalysisList <- list()
analysisId <- 1

# Loop over study periods (if multiple defined)
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  # Loop over time-at-risk windows (if multiple defined)
  for (t in seq_len(nrow(timeAtRisks))) {

    # Loop over propensity score adjustment settings (matching/stratification)
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Configure PS adjustment arguments based on the method
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Template default
          stratificationColumns = c() # Template default
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Template default
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Define covariate settings using FeatureExtraction.
      # Excluded/included concepts from analysis_spec are incorporated.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE, # Template default
        excludeCovariateIds = excludedCovariateConcepts$conceptId, # Concepts to exclude from analysis_spec
        includeCovariateIds = includedCovariateConcepts$conceptId  # Concepts to include from analysis_spec
      )

      # Create a list of outcomes, combining the main outcome and negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE, # Mark as primary outcome
            trueEffectSize = NA, # No true effect size for primary outcomes
            priorOutcomeLookback = 99999 # Template default
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE, # Mark as negative control
            trueEffectSize = 1 # True effect size for negative controls is 1 (no effect)
          )
        })
      )

      # Define target-comparator-outcome combinations for the study.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude covariate concepts. The template had placeholders for T/C drug concepts
          # not available in analysis_spec, so only explicit excluded concepts are used.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Arguments for retrieving cohort method data from the database.
      # Settings are primarily from getDbCohortMethodDataArgs in analysis_spec.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Template default
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = getDbCohortMethodDataArgsSpec$maxCohortSize, # From analysis_spec
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # Settings are from propensityScoreAdjustment.createPsArgs in analysis_spec.
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = propensityScoreAdjustmentSpec$createPsArgs$maxCohortSizeForFitting,
        errorOnHighCorrelation = propensityScoreAdjustmentSpec$createPsArgs$errorOnHighCorrelation,
        stopOnError = FALSE, # Template default, allows Strategus to complete if PS model fails
        estimator = "att", # Template default for average treatment effect on treated
        prior = Cyclops::createPrior( # Prior distribution for regularization
          priorType = propensityScoreAdjustmentSpec$createPsArgs$prior$priorType,
          exclude = c(0), # Template default (exclude intercept from regularization)
          useCrossValidation = propensityScoreAdjustmentSpec$createPsArgs$prior$useCrossValidation
        ),
        control = Cyclops::createControl( # Control parameters for Cyclops optimization
          noiseLevel = propensityScoreAdjustmentSpec$createPsArgs$control$noiseLevel,
          cvType = propensityScoreAdjustmentSpec$createPsArgs$control$cvType,
          seed = 1, # Template default for reproducibility, not in spec
          resetCoefficients = propensityScoreAdjustmentSpec$createPsArgs$control$resetCoefficients,
          tolerance = propensityScoreAdjustmentSpec$createPsArgs$control$tolerance,
          cvRepetitions = propensityScoreAdjustmentSpec$createPsArgs$control$cvRepetitions, # From analysis_spec
          fold = propensityScoreAdjustmentSpec$createPsArgs$control$fold, # From analysis_spec
          startingVariance = propensityScoreAdjustmentSpec$createPsArgs$control$startingVariance
        )
      )

      # Arguments for computing covariate balance for shared and full covariates.
      # Using template defaults as not specified in analysis_spec.
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting the outcome model.
      # Settings are from fitOutcomeModelArgs in analysis_spec.
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = fitOutcomeModelArgsSpec$modelType,
        stratified = fitOutcomeModelArgsSpec$stratified, # From analysis_spec
        useCovariates = fitOutcomeModelArgsSpec$useCovariates,
        inversePtWeighting = fitOutcomeModelArgsSpec$inversePtWeighting,
        prior = Cyclops::createPrior( # Prior distribution for regularization
          priorType = fitOutcomeModelArgsSpec$prior$priorType,
          useCrossValidation = fitOutcomeModelArgsSpec$prior$useCrossValidation
        ),
        control = Cyclops::createControl( # Control parameters for Cyclops optimization
          cvType = fitOutcomeModelArgsSpec$control$cvType,
          seed = 1, # Template default for reproducibility, not in spec
          resetCoefficients = fitOutcomeModelArgsSpec$control$resetCoefficients,
          startingVariance = fitOutcomeModelArgsSpec$control$startingVariance,
          tolerance = fitOutcomeModelArgsSpec$control$tolerance,
          cvRepetitions = fitOutcomeModelArgsSpec$control$cvRepetitions, # From analysis_spec
          fold = fitOutcomeModelArgsSpec$control$fold, # From analysis_spec
          noiseLevel = fitOutcomeModelArgsSpec$control$noiseLevel
        )
      )

      # Arguments for creating the study population.
      # Settings are from createStudyPopArgs in analysis_spec, and timeAtRisks for TAR-specific details.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = createStudyPopArgsSpec$restrictToCommonPeriod,
        firstExposureOnly = createStudyPopArgsSpec$firstExposureOnly,
        washoutPeriod = createStudyPopArgsSpec$washoutPeriod,
        removeDuplicateSubjects = createStudyPopArgsSpec$removeDuplicateSubjects,
        censorAtNewRiskWindow = createStudyPopArgsSpec$censorAtNewRiskWindow,
        removeSubjectsWithPriorOutcome = createStudyPopArgsSpec$removeSubjectsWithPriorOutcome,
        priorOutcomeLookback = createStudyPopArgsSpec$priorOutcomeLookBack,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999 # Template default, not specified in analysis_spec
      )

      # Append the combined settings to the list of CohortMethod analyses.
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

# Create module specifications for the CohortMethod module.
# This wraps all individual CM analyses and other global CM settings.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # No specific analyses to exclude from the list
  refitPsForEveryOutcome = FALSE, # Do not refit PS model for each outcome
  refitPsForEveryStudyPopulation = FALSE, # Do not refit PS model for each study population
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Use default diagnostic thresholds
)


# Create the analysis specifications ------------------------------------------
# Combine all shared resources and module specifications into a single
# Strategus analysis specifications object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Define the output directory and file name based on the study name from analysis_spec_json.
outputFolder <- file.path("inst", analysis_spec_json$name)
if (!dir.exists(outputFolder)) {
  dir.create(outputFolder, recursive = TRUE)
}
outputFileName <- file.path(outputFolder, paste0(analysis_spec_json$name, "AnalysisSpecification.json"))

# Save the complete analysis specifications object to a JSON file.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  outputFileName
)