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
library(jsonlite) # Required for parsing JSON if running this script directly from the JSON spec

# --- Internal script setup: Parsing Analysis Specifications from JSON ---
# This section simulates loading the analysis specifications from the JSON.
# In a real Strategus run, these values would be directly embedded or passed.
# For generating the script, we parse it once to use its values.
analysis_spec_json <- '{
  "name": "corazon",
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
        "studyStartDate": "20100101",
        "studyEndDate": "20191231"
      },
      {
        "studyStartDate": "20120101",
        "studyEndDate": "20191231"
      }
    ],
    "maxCohortSize": 0,
    "restrictToCommonPeriod": false,
    "firstExposureOnly": false,
    "washoutPeriod": 0,
    "removeDuplicateSubjects": "keep all"
  },
  "createStudyPopArgs": {
    "restrictToCommonPeriod": false,
    "firstExposureOnly": false,
    "washoutPeriod": 0,
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
      },
      {
        "riskWindowStart": 1,
        "startAnchor": "cohort start",
        "riskWindowEnd": 99999,
        "endAnchor": "cohort start",
        "minDaysAtRisk": 1
      }
    ]
  },
  "propensityScoreAdjustment": {
    "psSettings": [
      {
        "matchOnPsArgs": null,
        "stratifyByPsArgs": {
          "numberOfStrata": 5,
          "baseSelection": "all"
        }
      },
      {
        "matchOnPsArgs": {
          "maxRatio": 0,
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
    "stratified": true,
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
analysis_spec <- jsonlite::fromJSON(analysis_spec_json, simplifyVector = FALSE)
# --- End of internal script setup ---


# Shared Resources -------------------------------------------------------------
# Define the base URL for the OHDSI WebAPI to retrieve cohort definitions and concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Retrieve cohort definitions for Target, Comparator, and Outcome cohorts.
# The IDs are taken directly from the "cohortDefinitions" section of the analysis specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    analysis_spec$cohortDefinitions$targetCohort$id,    # Target cohort ID
    analysis_spec$cohortDefinitions$comparatorCohort$id, # Comparator cohort ID
    analysis_spec$cohortDefinitions$outcomeCohort[[1]]$id # Outcome cohort ID (assuming first in list)
  ),
  generateStats = TRUE
)

# Store original cohort IDs before re-numbering for mapping purposes.
originalTargetId <- analysis_spec$cohortDefinitions$targetCohort$id
originalComparatorId <- analysis_spec$cohortDefinitions$comparatorCohort$id
originalOutcomeId <- analysis_spec$cohortDefinitions$outcomeCohort[[1]]$id

# Re-number cohorts to internal, sequential IDs (1, 2, 3) as expected by the template structure.
# This makes it easier to reference Target, Comparator, and Outcome consistently within the script.
cohortDefinitionSet <- cohortDefinitionSet %>%
  mutate(originalCohortId = cohortId) %>% # Keep original cohortId for reference
  mutate(
    # Assign new internal cohort IDs based on the original IDs
    cohortId = case_when(
      originalCohortId == originalTargetId    ~ 1, # Target cohort gets internal ID 1
      originalCohortId == originalComparatorId ~ 2, # Comparator cohort gets internal ID 2
      originalCohortId == originalOutcomeId    ~ 3, # Outcome cohort gets internal ID 3
      TRUE ~ originalCohortId # Keep other cohort IDs as they are if they exist
    )
  )

# Retrieve negative control outcome cohort set.
# The conceptSetId is taken from the "negativeControlConceptSet" section of the analysis specifications.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = analysis_spec$negativeControlConceptSet$id, # Negative Control Concept Set ID
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
  mutate(cohortId = row_number() + 100) %>% # Assign unique IDs starting from 101 for negative controls
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs between study cohorts and negative controls.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create a data frame for the outcome cohorts.
# It filters for the re-numbered outcome cohort (internal ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Outcome cohort (re-numbered to 3)
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window for outcomes

# Create a data frame for Target and Comparator cohorts for CohortMethod analysis.
# Uses the re-numbered internal IDs (1 and 2) and their corresponding names.
cmTcList <- data.frame(
  targetCohortId = cohortDefinitionSet %>% filter(cohortId == 1) %>% pull(cohortId),
  targetCohortName = cohortDefinitionSet %>% filter(cohortId == 1) %>% pull(cohortName),
  comparatorCohortId = cohortDefinitionSet %>% filter(cohortId == 2) %>% pull(cohortId),
  comparatorCohortName = cohortDefinitionSet %>% filter(cohortId == 2) %>% pull(cohortName)
)

# Define concepts to exclude from covariates, based on "covariateSelection.conceptsToExclude".
# Handles cases where the list might be empty or contain null/empty entries.
if (length(analysis_spec$covariateSelection$conceptsToExclude) > 0 &&
    !is.null(analysis_spec$covariateSelection$conceptsToExclude[[1]]$id)) {
  excludedCovariateConcepts <- do.call(rbind, lapply(analysis_spec$covariateSelection$conceptsToExclude, as.data.frame))
} else {
  excludedCovariateConcepts <- data.frame(conceptId = integer(0), conceptName = character(0))
}

# Define concepts to include for covariates, based on "covariateSelection.conceptsToInclude".
# Handles cases where the list might be empty or contain null/empty entries.
# (This list is currently empty in the provided specifications).
if (length(analysis_spec$covariateSelection$conceptsToInclude) > 0 &&
    !is.null(analysis_spec$covariateSelection$conceptsToInclude[[1]]$id)) {
  includedCovariateConcepts <- do.call(rbind, lapply(analysis_spec$covariateSelection$conceptsToInclude, as.data.frame))
} else {
  includedCovariateConcepts <- data.frame(conceptId = integer(0), conceptName = character(0))
}


# CohortGeneratorModule --------------------------------------------------------
# Module to generate study cohorts.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Shared resource for cohort definitions (Target, Comparator, Outcome).
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Shared resource for negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default from template
  detectOnDescendants = TRUE # Default from template
)
# Module specifications for cohort generation, generating statistics.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# Module to run various cohort diagnostics.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Module specifications for cohort diagnostics, running various checks.
# Cohort IDs for diagnostics include all re-numbered study cohorts.
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

# Define study periods from "getDbCohortMethodDataArgs.studyPeriods" in analysis specifications.
studyPeriods <- tibble(
  studyStartDate = unlist(lapply(analysis_spec$getDbCohortMethodDataArgs$studyPeriods, `[[`, "studyStartDate")),
  studyEndDate   = unlist(lapply(analysis_spec$getDbCohortMethodDataArgs$studyPeriods, `[[`, "studyEndDate"))
)

# Define time-at-risks (TARs) from "createStudyPopArgs.timeAtRisks" in analysis specifications.
timeAtRisks <- tibble(
  label = c(
    "1d-to-cohortEnd", # Label for the first TAR
    "1d-to-99999d-from-cohortStart" # Label for the second TAR
  ),
  riskWindowStart  = unlist(lapply(analysis_spec$createStudyPopArgs$timeAtRisks, `[[`, "riskWindowStart")),
  startAnchor = unlist(lapply(analysis_spec$createStudyPopArgs$timeAtRisks, `[[`, "startAnchor")),
  riskWindowEnd  = unlist(lapply(analysis_spec$createStudyPopArgs$timeAtRisks, `[[`, "riskWindowEnd")),
  endAnchor = unlist(lapply(analysis_spec$createStudyPopArgs$timeAtRisks, `[[`, "endAnchor")),
  minDaysAtRisk = unlist(lapply(analysis_spec$createStudyPopArgs$timeAtRisks, `[[`, "minDaysAtRisk")) # Included from analysis spec
)

# Initialize data frames to hold propensity score settings for matching and stratification.
matchOnPsArgsList <- tibble(
  label = character(),
  maxRatio = numeric(),
  caliper = numeric(),
  caliperScale = character()
)

stratifyByPsArgsList <- tibble(
  label = character(),
  numberOfStrata = numeric(),
  baseSelection = character()
)

# Populate PS settings lists from "propensityScoreAdjustment.psSettings".
psSettings_spec <- analysis_spec$propensityScoreAdjustment$psSettings
for (i in seq_along(psSettings_spec)) {
  setting <- psSettings_spec[[i]]
  if (!is.null(setting$matchOnPsArgs)) {
    matchOnPsArgsList <- matchOnPsArgsList %>%
      add_row(
        label = paste0("Match_maxRatio", setting$matchOnPsArgs$maxRatio, "_caliper", setting$matchOnPsArgs$caliper),
        maxRatio = setting$matchOnPsArgs$maxRatio,
        caliper = setting$matchOnPsArgs$caliper,
        caliperScale = setting$matchOnPsArgs$caliperScale
      )
  }
  if (!is.null(setting$stratifyByPsArgs)) {
    stratifyByPsArgsList <- stratifyByPsArgsList %>%
      add_row(
        label = paste0("Stratify_strata", setting$stratifyByPsArgs$numberOfStrata, "_base", setting$stratifyByPsArgs$baseSelection),
        numberOfStrata = setting$stratifyByPsArgs$numberOfStrata,
        baseSelection = setting$stratifyByPsArgs$baseSelection
      )
  }
}

# Build a single PS configuration list, combining matching and stratification settings.
psConfigList <- list()

if (nrow(matchOnPsArgsList) > 0) {
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

if (nrow(stratifyByPsArgsList) > 0) {
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

# Iterate through all analysis setting combinations to create CohortMethod analyses.
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Define matchOnPsArgs or stratifyByPsArgs based on the current PS configuration.
      matchOnPsArgs <- NULL
      stratifyByPsArgs <- NULL
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Template default
          stratificationColumns = c() # Template default
        )
      } else if (psCfg$method == "stratify") {
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Template default
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Define covariate settings.
      # Since conceptsToInclude and conceptsToExclude are empty in the spec,
      # createDefaultCovariateSettings is used with only addDescendantsToExclude.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Template default
      )

      # Create a list of outcomes, including true outcomes and negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # For true outcomes, trueEffectSize is NA
            priorOutcomeLookback = 99999 # Template default
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # For negative controls, trueEffectSize is 1
          )
        })
      )

      # Determine which covariate concepts to exclude from the analysis.
      # This uses the `excludedCovariateConcepts` list from the "covariateSelection" section.
      currentExcludedConcepts <- if (nrow(excludedCovariateConcepts) > 0) {
        excludedCovariateConcepts$conceptId
      } else {
        c()
      }

      # Create a list of Target-Comparator-Outcome combinations.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = currentExcludedConcepts # Use the list of excluded concepts
        )
      }

      # Define arguments for retrieving DbCohortMethodData.
      # Settings are taken from "getDbCohortMethodDataArgs" in analysis specifications.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        restrictToCommonPeriod = analysis_spec$getDbCohortMethodDataArgs$restrictToCommonPeriod,
        maxCohortSize = analysis_spec$getDbCohortMethodDataArgs$maxCohortSize,
        firstExposureOnly = analysis_spec$getDbCohortMethodDataArgs$firstExposureOnly,
        washoutPeriod = analysis_spec$getDbCohortMethodDataArgs$washoutPeriod,
        removeDuplicateSubjects = analysis_spec$getDbCohortMethodDataArgs$removeDuplicateSubjects,
        covariateSettings = covariateSettings
      )

      # Define arguments for creating propensity scores.
      # Settings are taken from "propensityScoreAdjustment.createPsArgs" in analysis specifications.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = analysis_spec$propensityScoreAdjustment$createPsArgs$maxCohortSizeForFitting,
        errorOnHighCorrelation = analysis_spec$propensityScoreAdjustment$createPsArgs$errorOnHighCorrelation,
        stopOnError = FALSE, # Template default (prevents stopping the entire Strategus run)
        estimator = "att",   # Template default
        prior = Cyclops::createPrior( # Prior settings for regularization
          priorType = analysis_spec$propensityScoreAdjustment$createPsArgs$prior$priorType,
          exclude = c(0), # Template default
          useCrossValidation = analysis_spec$propensityScoreAdjustment$createPsArgs$prior$useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings for Cyclops optimizer
          noiseLevel = analysis_spec$propensityScoreAdjustment$createPsArgs$control$noiseLevel,
          cvType = analysis_spec$propensityScoreAdjustment$createPsArgs$control$cvType,
          seed = 1,          # Template default
          resetCoefficients = analysis_spec$propensityScoreAdjustment$createPsArgs$control$resetCoefficients,
          tolerance = analysis_spec$propensityScoreAdjustment$createPsArgs$control$tolerance,
          cvRepetitions = analysis_spec$propensityScoreAdjustment$createPsArgs$control$cvRepetitions,
          startingVariance = analysis_spec$propensityScoreAdjustment$createPsArgs$control$startingVariance,
          fold = analysis_spec$propensityScoreAdjustment$createPsArgs$control$fold # From analysis spec
        )
      )

      # Define arguments for computing shared covariate balance.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Template default
        covariateFilter = NULL # Template default
      )
      # Define arguments for computing covariate balance for study population.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Template default
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Template default
      )

      # Define arguments for fitting the outcome model.
      # Settings are taken from "fitOutcomeModelArgs" in analysis specifications.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = analysis_spec$fitOutcomeModelArgs$modelType,
        stratified = analysis_spec$fitOutcomeModelArgs$stratified,
        useCovariates = analysis_spec$fitOutcomeModelArgs$useCovariates,
        inversePtWeighting = analysis_spec$fitOutcomeModelArgs$inversePtWeighting,
        prior = Cyclops::createPrior( # Prior settings for regularization
          priorType = analysis_spec$fitOutcomeModelArgs$prior$priorType,
          useCrossValidation = analysis_spec$fitOutcomeModelArgs$prior$useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings for Cyclops optimizer
          cvType = analysis_spec$fitOutcomeModelArgs$control$cvType,
          seed = 1, # Template default
          resetCoefficients = analysis_spec$fitOutcomeModelArgs$control$resetCoefficients,
          startingVariance = analysis_spec$fitOutcomeModelArgs$control$startingVariance,
          tolerance = analysis_spec$fitOutcomeModelArgs$control$tolerance,
          cvRepetitions = analysis_spec$fitOutcomeModelArgs$control$cvRepetitions,
          noiseLevel = analysis_spec$fitOutcomeModelArgs$control$noiseLevel,
          fold = analysis_spec$fitOutcomeModelArgs$control$fold # From analysis spec
        )
      )

      # Define arguments for creating the study population.
      # Settings are taken from "createStudyPopArgs" in analysis specifications and current timeAtRisks.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = analysis_spec$createStudyPopArgs$restrictToCommonPeriod,
        firstExposureOnly = analysis_spec$createStudyPopArgs$firstExposureOnly,
        washoutPeriod = analysis_spec$createStudyPopArgs$washoutPeriod,
        removeDuplicateSubjects = analysis_spec$createStudyPopArgs$removeDuplicateSubjects,
        censorAtNewRiskWindow = analysis_spec$createStudyPopArgs$censorAtNewRiskWindow,
        removeSubjectsWithPriorOutcome = analysis_spec$createStudyPopArgs$removeSubjectsWithPriorOutcome,
        priorOutcomeLookback = analysis_spec$createStudyPopArgs$priorOutcomeLookBack, # Note: using `priorOutcomeLookBack` from spec
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999 # Template default, not specified in analysis spec
      )

      # Append the combined settings for a single CohortMethod analysis to the list.
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

# CohortMethod Module specifications.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Template default thresholds
)

# Create the overall analysis specifications object for Strategus ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> # Add cohort definitions shared resource
  Strategus::addSharedResources(negativeControlsShared) |> # Add negative controls shared resource
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |> # Add CohortGenerator module
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |> # Add CohortDiagnostics module
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications) # Add CohortMethod module

# Save the complete analysis specifications to a JSON file.
# The file path uses the study name from the analysis specifications ("corazon").
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", analysis_spec$name, paste0(analysis_spec$name, "AnalysisSpecification.json"))
)