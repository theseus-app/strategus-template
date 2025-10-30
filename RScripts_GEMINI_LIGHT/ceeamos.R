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
library(jsonlite)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)

# --- Analysis Specifications Placeholder (In a real script, load from a file) ---
# This 'specifications' object simulates loading your analysis_spec.json
# For the purpose of this script, we'll hardcode the JSON string.
# In a real Strategus run, this would be loaded from a file like:
# specifications <- jsonlite::fromJSON(
#   file.path("path", "to", "your", "studyNameAnalysisSpecification.json"),
#   simplifyVector = FALSE
# )
analysis_spec_json <- '{
  "name": "ceeamos",
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
        "studyStartDate": null,
        "studyEndDate": null
      }
    ],
    "maxCohortSize": 0
  },
  "createStudyPopArgs": {
    "restrictToCommonPeriod": false,
    "firstExposureOnly": false,
    "washoutPeriod": 365,
    "removeDuplicateSubjects": "remove all",
    "censorAtNewRiskWindow": false,
    "removeSubjectsWithPriorOutcome": true,
    "priorOutcomeLookBack": 365,
    "timeAtRisks": [
      {
        "riskWindowStart": 0,
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
          "maxRatio": 10,
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
  },
  "maxCohortSize": 0
}'
specifications <- jsonlite::fromJSON(analysis_spec_json, simplifyVector = FALSE)
# --- End Analysis Specifications Placeholder ---


# Shared Resources -------------------------------------------------------------
# Base URL for OHDSI WebAPI (Atlas) - usually provided by the user
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Extract cohort IDs from specifications
targetCohortId <- specifications$cohortDefinitions$targetCohort$id
comparatorCohortId <- specifications$cohortDefinitions$comparatorCohort$id
outcomeCohortId <- specifications$cohortDefinitions$outcomeCohort[[1]]$id # Assuming one outcome for now

# Cohort Definitions
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    targetCohortId, # Target cohort ID from specifications
    comparatorCohortId, # Comparator cohort ID from specifications
    outcomeCohortId # Outcome cohort ID from specifications
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal Strategus use (1=target, 2=comparator, 3=outcome)
# This mapping ensures consistent internal IDs regardless of original Atlas IDs.
cohortDefinitionSet[cohortDefinitionSet$cohortId == targetCohortId,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == comparatorCohortId,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == outcomeCohortId,]$cohortId <- 3

# Negative control outcomes
# Retrieve negative control concept set definition from WebAPI using the ID from specifications
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = specifications$negativeControlConceptSet$id, # Negative control concept set ID from specifications
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(
    baseUrl = baseUrl
  ) %>%
  ROhdsiWebApi::getConcepts(
    baseUrl = baseUrl
  ) %>%
  dplyr::rename(outcomeConceptId = "conceptId",
                cohortName = "conceptName") %>%
  dplyr::mutate(cohortId = dplyr::row_number() + 100) %>% # Assigning unique IDs starting from 101 for negative controls
  dplyr::select(cohortId, cohortName, outcomeConceptId)


if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Filter for the re-numbered outcome cohort (ID 3)
oList <- cohortDefinitionSet %>%
  dplyr::filter(.data$cohortId == 3) %>%
  dplyr::mutate(outcomeCohortId = .data$cohortId, outcomeCohortName = .data$cohortName) %>%
  dplyr::select(outcomeCohortId, outcomeCohortName) %>%
  dplyr::mutate(cleanWindow = 365) # Default clean window, not specified in current JSON

# Target and Comparator for the CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered target cohort ID
  targetCohortName = specifications$cohortDefinitions$targetCohort$name, # Target cohort name from specifications
  comparatorCohortId = 2, # Re-numbered comparator cohort ID
  comparatorCohortName = specifications$cohortDefinitions$comparatorCohort$name # Comparator cohort name from specifications
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study. Populated from covariateSelection.conceptsToExclude.
excludedCovariateConcepts <- data.frame(conceptId = integer(0), conceptName = character(0))
if (length(specifications$covariateSelection$conceptsToExclude) > 0 &&
    !is.null(specifications$covariateSelection$conceptsToExclude[[1]]$id)) {
  excludedCovariateConcepts <- data.frame(
    conceptId = sapply(specifications$covariateSelection$conceptsToExclude, `[[`, "id"),
    conceptName = sapply(specifications$covariateSelection$conceptsToExclude, `[[`, "name")
  )
}

# Optional: If you want to define covariates to include instead of including them all
includedCovariateConcepts <- data.frame(conceptId = integer(0), conceptName = character(0))
if (length(specifications$covariateSelection$conceptsToInclude) > 0 &&
    !is.null(specifications$covariateSelection$conceptsToInclude[[1]]$id)) {
  includedCovariateConcepts <- data.frame(
    conceptId = sapply(specifications$covariateSelection$conceptsToInclude, `[[`, "id"),
    conceptName = sapply(specifications$covariateSelection$conceptsToInclude, `[[`, "name")
  )
}

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default, not specified in JSON
  detectOnDescendants = TRUE # Default, not specified in JSON
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Default, not specified in JSON
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # Use all defined cohort IDs
  runInclusionStatistics = TRUE, # Default, not specified in JSON
  runIncludedSourceConcepts = TRUE, # Default, not specified in JSON
  runOrphanConcepts = TRUE, # Default, not specified in JSON
  runTimeSeries = FALSE, # Default, not specified in JSON
  runVisitContext = TRUE, # Default, not specified in JSON
  runBreakdownIndexEvents = TRUE, # Default, not specified in JSON
  runIncidenceRate = TRUE, # Default, not specified in JSON
  runCohortRelationship = TRUE, # Default, not specified in JSON
  runTemporalCohortCharacterization = TRUE, # Default, not specified in JSON
  minCharacterizationMean = 0.01 # Default, not specified in JSON
)

# CohortMethodModule -----------------------------------------------------------

# Study periods from specifications. If null/empty, represent as NA to allow one loop iteration.
studyPeriods <- tibble(
  studyStartDate = character(0),
  studyEndDate = character(0)
)
if (length(specifications$getDbCohortMethodDataArgs$studyPeriods) > 0 &&
    !is.null(specifications$getDbCohortMethodDataArgs$studyPeriods[[1]]$studyStartDate)) {
  studyPeriods <- tibble(
    studyStartDate = sapply(specifications$getDbCohortMethodDataArgs$studyPeriods, `[[`, "studyStartDate"),
    studyEndDate = sapply(specifications$getDbCohortMethodDataArgs$studyPeriods, `[[`, "studyEndDate")
  )
} else {
  # If no specific study periods are defined, use a single entry with NA values
  # to ensure the loop runs once for unrestricted periods.
  studyPeriods <- tibble(
    studyStartDate = NA_character_,
    studyEndDate = NA_character_
  )
}

# Time-at-risks (TARs) for the outcomes of interest in your study from specifications
timeAtRisks <- tibble(
  label = character(0),
  riskWindowStart  = integer(0),
  startAnchor = character(0),
  riskWindowEnd  = integer(0),
  endAnchor = character(0)
)
if (length(specifications$createStudyPopArgs$timeAtRisks) > 0) {
  timeAtRisks <- tibble(
    label = paste0("TAR ", seq_along(specifications$createStudyPopArgs$timeAtRisks)),
    riskWindowStart = sapply(specifications$createStudyPopArgs$timeAtRisks, `[[`, "riskWindowStart"),
    startAnchor = sapply(specifications$createStudyPopArgs$timeAtRisks, `[[`, "startAnchor"),
    riskWindowEnd = sapply(specifications$createStudyPopArgs$timeAtRisks, `[[`, "riskWindowEnd"),
    endAnchor = sapply(specifications$createStudyPopArgs$timeAtRisks, `[[`, "endAnchor")
  )
}

# Propensity Score settings - match on PS from specifications
matchOnPsArgsList <- tibble(
  label = character(0),
  maxRatio  = numeric(0),
  caliper = numeric(0),
  caliperScale  = character(0)
)
# Check for matchOnPsArgs in psSettings
psSettings <- specifications$propensityScoreAdjustment$psSettings
if (length(psSettings) > 0) {
  for (i in seq_along(psSettings)) {
    if (!is.null(psSettings[[i]]$matchOnPsArgs)) {
      matchOnPsArgsList <- matchOnPsArgsList %>%
        add_row(
          label = paste0("MatchOnPs ", i), # Assign a label for description
          maxRatio = psSettings[[i]]$matchOnPsArgs$maxRatio,
          caliper = psSettings[[i]]$matchOnPsArgs$caliper,
          caliperScale = psSettings[[i]]$matchOnPsArgs$caliperScale
        )
    }
  }
}

# Propensity Score settings - stratify by PS (empty in current specifications)
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0)
)
# No stratifyByPsArgs in current specifications, so this will remain empty.

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
      # Note: 'baseSelection' is not in the JSON but is in the template. If needed,
      # ensure it's handled or has a default.
      params = list(
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection  = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}

# Handle case where no PS adjustment is specified - run an unadjusted analysis
if (length(psConfigList) == 0) {
  psConfigList[[1]] <- list(method = "none", label = "No PS Adjustment", params = list())
}


# Iterate through all analysis setting combinations
cmAnalysisList <- list()
analysisId <- 1

# Loop through study periods. If `studyPeriods` has NA, the loop will run once for unrestricted periods.
for (s in seq_len(nrow(studyPeriods))) {
  currentStudyStartDate <- studyPeriods$studyStartDate[s]
  currentStudyEndDate <- studyPeriods$studyEndDate[s]

  # Set to NULL if NA to correctly pass to CohortMethod functions
  studyStartDateValue <- if (is.na(currentStudyStartDate)) NULL else currentStudyStartDate
  studyEndDateValue <- if (is.na(currentStudyEndDate)) NULL else currentStudyEndDate
  
  # Loop through time-at-risk definitions
  for (t in seq_len(nrow(timeAtRisks))) {

    # Loop through PS adjustment configurations (matching, stratifying, or none)
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Initialize PS adjustment arguments based on the current configuration
      matchOnPsArgs <- NULL
      stratifyByPsArgs <- NULL

      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default, not in JSON
          stratificationColumns = c() # Default, not in JSON
        )
      } else if (psCfg$method == "stratify") {
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default, not in JSON
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings
      # Included and excluded covariate concept IDs from specifications
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE, # Default, not in JSON
        excludedCovariateConceptIds = excludedCovariateConcepts$conceptId, # Excluded concepts from specifications
        includedCovariateConceptIds = includedCovariateConcepts$conceptId # Included concepts from specifications
      )

      # Create outcome list including both outcomes of interest and negative controls
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not specified in JSON
            priorOutcomeLookback = 99999 # Not specified in JSON for createOutcome, but for study population
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
      
      # Define target-comparator-outcomes list for each T-C pair
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Only use specified excludedCovariateConcepts.
          # The template had targetConceptId/comparatorConceptId, which are not in specs for cmTcList.
          excludedCovariateConceptIds = c(excludedCovariateConcepts$conceptId)
        )
      }

      # Arguments for fetching cohort method data
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Default, not in JSON
        studyStartDate = studyStartDateValue, # From current loop iteration (can be NULL)
        studyEndDate = studyEndDateValue, # From current loop iteration (can be NULL)
        maxCohortSize = specifications$getDbCohortMethodDataArgs$maxCohortSize, # From specifications
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = specifications$propensityScoreAdjustment$createPsArgs$maxCohortSizeForFitting, # From specifications
        errorOnHighCorrelation = specifications$propensityScoreAdjustment$createPsArgs$errorOnHighCorrelation, # From specifications
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att", # Default, not specified in JSON
        prior = Cyclops::createPrior( # Prior settings from specifications
          priorType = specifications$propensityScoreAdjustment$createPsArgs$prior$priorType,
          useCrossValidation = specifications$propensityScoreAdjustment$createPsArgs$prior$useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings from specifications
          tolerance = specifications$propensityScoreAdjustment$createPsArgs$control$tolerance,
          cvType = specifications$propensityScoreAdjustment$createPsArgs$control$cvType,
          fold = specifications$propensityScoreAdjustment$createPsArgs$control$fold,
          cvRepetitions = specifications$propensityScoreAdjustment$createPsArgs$control$cvRepetitions,
          noiseLevel = specifications$propensityScoreAdjustment$createPsArgs$control$noiseLevel,
          resetCoefficients = specifications$propensityScoreAdjustment$createPsArgs$control$resetCoefficients,
          startingVariance = specifications$propensityScoreAdjustment$createPsArgs$control$startingVariance
          # seed = 1 # Not specified in JSON, so remove from template
        )
      )

      # Arguments for computing shared covariate balance (default values from template)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default, not in JSON
        covariateFilter = NULL # Default, not in JSON
      )
      
      # Arguments for computing covariate balance (default values from template)
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default, not in JSON
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default, not in JSON
      )

      # Arguments for fitting outcome model
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = specifications$fitOutcomeModelArgs$modelType, # From specifications
        stratified = specifications$fitOutcomeModelArgs$stratified, # From specifications
        useCovariates = specifications$fitOutcomeModelArgs$useCovariates, # From specifications
        inversePtWeighting = specifications$fitOutcomeModelArgs$inversePtWeighting, # From specifications
        prior = Cyclops::createPrior( # Prior settings from specifications
          priorType = specifications$fitOutcomeModelArgs$prior$priorType,
          useCrossValidation = specifications$fitOutcomeModelArgs$prior$useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings from specifications
          tolerance = specifications$fitOutcomeModelArgs$control$tolerance,
          cvType = specifications$fitOutcomeModelArgs$control$cvType,
          fold = specifications$fitOutcomeModelArgs$control$fold,
          cvRepetitions = specifications$fitOutcomeModelArgs$control$cvRepetitions,
          noiseLevel = specifications$fitOutcomeModelArgs$control$noiseLevel,
          resetCoefficients = specifications$fitOutcomeModelArgs$control$resetCoefficients,
          startingVariance = specifications$fitOutcomeModelArgs$control$startingVariance
          # seed = 1 # Not specified in JSON, so remove from template
        )
      )
      
      # Arguments for creating study population
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = specifications$createStudyPopArgs$restrictToCommonPeriod, # From specifications
        firstExposureOnly = specifications$createStudyPopArgs$firstExposureOnly, # From specifications
        washoutPeriod = specifications$createStudyPopArgs$washoutPeriod, # From specifications
        removeDuplicateSubjects = specifications$createStudyPopArgs$removeDuplicateSubjects, # From specifications
        censorAtNewRiskWindow = specifications$createStudyPopArgs$censorAtNewRiskWindow, # From specifications
        removeSubjectsWithPriorOutcome = specifications$createStudyPopArgs$removeSubjectsWithPriorOutcome, # From specifications
        priorOutcomeLookback = specifications$createStudyPopArgs$priorOutcomeLookBack, # From specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From current loop iteration
        startAnchor = timeAtRisks$startAnchor[t], # From current loop iteration
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From current loop iteration
        endAnchor = timeAtRisks$endAnchor[t], # From current loop iteration
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From current loop iteration
        maxDaysAtRisk = 99999 # Default, not in JSON
      )


      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(is.null(studyStartDateValue), "Unrestricted", studyStartDateValue),
          ifelse(is.null(studyEndDateValue), "Unrestricted", studyEndDateValue),
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
  analysesToExclude = NULL, # Not specified in JSON
  refitPsForEveryOutcome = FALSE, # Not specified in JSON
  refitPsForEveryStudyPopulation = FALSE, # Not specified in JSON
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default thresholds
)

# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file
# The file path should be adjusted based on your study's directory structure.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", specifications$name, paste0(specifications$name, "AnalysisSpecification.json"))
)