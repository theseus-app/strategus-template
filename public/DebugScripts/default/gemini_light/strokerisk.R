library(dplyr)
library(Strategus)
library(jsonlite)
library(tibble)
library(ROhdsiWebApi)
library(CohortGenerator)
library(CohortDiagnostics)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(ParallelLogger) # Added to explicitly use saveSettingsToJson

# --- Start of Analysis Specifications Parsing ---
# The analysis specifications are provided as a JSON string.
# This section parses the JSON into an R list for easy access.
analysisSpecsJson <- '{
  "name": "strokerisk",
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
        "studyStartDate": "20010101",
        "studyEndDate": "20171231"
      },
      {
        "studyStartDate": "20010101",
        "studyEndDate": "20150930"
      }
    ],
    "maxCohortSize": 0,
    "restrictToCommonPeriod": false,
    "firstExposureOnly": true,
    "washoutPeriod": 183,
    "removeDuplicateSubjects": "keep first"
  },
  "createStudyPopArgs": {
    "restrictToCommonPeriod": false,
    "firstExposureOnly": false,
    "washoutPeriod": 0,
    "removeDuplicateSubjects": "keep all",
    "censorAtNewRiskWindow": false,
    "removeSubjectsWithPriorOutcome": false,
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
        "matchOnPsArgs": null,
        "stratifyByPsArgs": null
      },
      {
        "matchOnPsArgs": {
          "maxRatio": 1,
          "caliper": 0.05,
          "caliperScale": "propensity score"
        },
        "stratifyByPsArgs": null
      },
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
# Storing the parsed JSON in 'inputAnalysisSpecs' to avoid conflict with the final 'analysisSpecifications' object
# which is built using Strategus functions, as per the template.
inputAnalysisSpecs <- jsonlite::fromJSON(analysisSpecsJson, simplifyVector = FALSE)
# --- End of Analysis Specifications Parsing ---


# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
# baseUrl is not provided in Analysis Specifications, using a common OHDSI demo WebAPI.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Extracting cohort IDs and names for Target, Comparator, and Outcome from the parsed specifications.
targetCohortId <- inputAnalysisSpecs$cohortDefinitions$targetCohort$id
targetCohortName <- inputAnalysisSpecs$cohortDefinitions$targetCohort$name
comparatorCohortId <- inputAnalysisSpecs$cohortDefinitions$comparatorCohort$id
comparatorCohortName <- inputAnalysisSpecs$cohortDefinitions$comparatorCohort$name
# Assuming a single outcome cohort based on the provided JSON structure.
outcomeCohortId <- inputAnalysisSpecs$cohortDefinitions$outcomeCohort[[1]]$id
outcomeCohortName <- inputAnalysisSpecs$cohortDefinitions$outcomeCohort[[1]]$name

# Fetch cohort definitions using ROhdsiWebApi
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    targetCohortId,
    comparatorCohortId,
    outcomeCohortId
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal consistency within Strategus modules.
# Assigning IDs 1, 2, 3 to Target, Comparator, Outcome respectively as per template convention.
cohortDefinitionSet[cohortDefinitionSet$cohortId == targetCohortId,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == comparatorCohortId,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == outcomeCohortId,]$cohortId <- 3

# Negative control outcomes
# Extracting negative control concept set ID and name from Analysis Specifications.
negativeControlConceptSetId <- inputAnalysisSpecs$negativeControlConceptSet$id
negativeControlConceptSetName <- inputAnalysisSpecs$negativeControlConceptSet$name

# Resolve the concept set to get individual concept IDs which will serve as negative control outcomes.
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
  dplyr::rename(outcomeConceptId = "conceptId",
                cohortName = "conceptName") %>%
  # Assigning cohort IDs starting from 100+ to avoid collision with primary cohorts (1, 2, 3)
  dplyr::mutate(cohortId = dplyr::row_number() + 100) %>%
  dplyr::select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs which would cause issues in Strategus
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes:
# Filtering for the outcome cohort (re-numbered to 3) and preparing it for oList.
oList <- cohortDefinitionSet %>%
  dplyr::filter(.data$cohortId == 3) %>% # Assuming outcome cohort was re-numbered to 3
  dplyr::mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  dplyr::select(outcomeCohortId, outcomeCohortName) %>%
  # 'cleanWindow' is a default value from the template, not specified in JSON.
  dplyr::mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis.
# Using the re-numbered cohort IDs for target (1) and comparator (2).
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered target cohort ID
  targetCohortName = targetCohortName, # Original target cohort name
  comparatorCohortId = 2, # Re-numbered comparator cohort ID
  comparatorCohortName = comparatorCohortName # Original comparator cohort name
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this study.
# Extracting concepts to exclude from covariateSelection in inputAnalysisSpecs.
# The JSON provides `conceptsToExclude` as an array with `{ "id": null, "name": "" }`,
# which means it's effectively empty.
if (length(inputAnalysisSpecs$covariateSelection$conceptsToExclude) > 0 &&
    !is.null(inputAnalysisSpecs$covariateSelection$conceptsToExclude[[1]]$id)) {
  excludedCovariateConcepts <- data.frame(
    conceptId = sapply(inputAnalysisSpecs$covariateSelection$conceptsToExclude, `[[`, "id"),
    conceptName = sapply(inputAnalysisSpecs$covariateSelection$conceptsToExclude, `[[`, "name")
  )
} else {
  excludedCovariateConcepts <- data.frame(conceptId = numeric(0), conceptName = character(0))
}

# Optional: If you want to define covariates to include instead of including them all.
# The JSON provides `conceptsToInclude` as an array with `{ "id": null, "name": "" }`,
# which means it's effectively empty.
if (length(inputAnalysisSpecs$covariateSelection$conceptsToInclude) > 0 &&
    !is.null(inputAnalysisSpecs$covariateSelection$conceptsToInclude[[1]]$id)) {
  includedCovariateConcepts <- data.frame(
    conceptId = sapply(inputAnalysisSpecs$covariateSelection$conceptsToInclude, `[[`, "id"),
    conceptName = sapply(inputAnalysisSpecs$covariateSelection$conceptsToInclude, `[[`, "name")
  )
} else {
  includedCovariateConcepts <- data.frame(conceptId = numeric(0), conceptName = character(0))
}


# CohortGeneratorModule --------------------------------------------------------
# This module generates the cohorts specified in shared resources.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Defines shared resource for the primary cohorts (target, comparator, outcome).
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Defines shared resource for negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Not specified in JSON, using template default
  detectOnDescendants = TRUE # Not specified in JSON, using template default
)
# Module specifications for CohortGenerator, enabling statistics generation.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Not specified in JSON, using template default
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# This module runs diagnostics on the generated cohorts.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # Run diagnostics for all primary cohorts
  runInclusionStatistics = TRUE, # Not specified in JSON, using template default
  runIncludedSourceConcepts = TRUE, # Not specified in JSON, using template default
  runOrphanConcepts = TRUE, # Not specified in JSON, using template default
  runTimeSeries = FALSE, # Not specified in JSON, using template default
  runVisitContext = TRUE, # Not specified in JSON, using template default
  runBreakdownIndexEvents = TRUE, # Not specified in JSON, using template default
  runIncidenceRate = TRUE, # Not specified in JSON, using template default
  runCohortRelationship = TRUE, # Not specified in JSON, using template default
  runTemporalCohortCharacterization = TRUE, # Not specified in JSON, using template default
  minCharacterizationMean = 0.01 # Not specified in JSON, using template default
)

# CohortMethodModule -----------------------------------------------------------
# This module performs comparative effectiveness analysis using CohortMethod.

# Extracting study periods from getDbCohortMethodDataArgs in the JSON.
studyPeriods <- tibble::tibble(
  studyStartDate = sapply(inputAnalysisSpecs$getDbCohortMethodDataArgs$studyPeriods, `[[`, "studyStartDate"),
  studyEndDate   = sapply(inputAnalysisSpecs$getDbCohortMethodDataArgs$studyPeriods, `[[`, "studyEndDate")
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# Extracting time-at-risk settings from createStudyPopArgs in the JSON.
# A label is generated for descriptive purposes in the analysis ID.
timeAtRisks <- tibble::tibble(
  label = paste0("RW: ",
                 sapply(inputAnalysisSpecs$createStudyPopArgs$timeAtRisks, `[[`, "startAnchor"), " + ",
                 sapply(inputAnalysisSpecs$createStudyPopArgs$timeAtRisks, `[[`, "riskWindowStart"), "d to ",
                 sapply(inputAnalysisSpecs$createStudyPopArgs$timeAtRisks, `[[`, "endAnchor"), " + ",
                 sapply(inputAnalysisSpecs$createStudyPopArgs$timeAtRisks, `[[`, "riskWindowEnd"), "d"),
  riskWindowStart  = sapply(inputAnalysisSpecs$createStudyPopArgs$timeAtRisks, `[[`, "riskWindowStart"),
  startAnchor = sapply(inputAnalysisSpecs$createStudyPopArgs$timeAtRisks, `[[`, "startAnchor"),
  riskWindowEnd  = sapply(inputAnalysisSpecs$createStudyPopArgs$timeAtRisks, `[[`, "riskWindowEnd"),
  endAnchor = sapply(inputAnalysisSpecs$createStudyPopArgs$timeAtRisks, `[[`, "endAnchor"),
  minDaysAtRisk = sapply(inputAnalysisSpecs$createStudyPopArgs$timeAtRisks, `[[`, "minDaysAtRisk") # Added this from JSON
)

# Propensity Score (PS) settings - match on PS or stratify by PS.
# Extracting PS adjustment settings from propensityScoreAdjustment.psSettings in the JSON.
# The `psSettings` array can contain configurations for matching or stratification.
matchOnPsArgsList <- tibble::tibble(
  label = character(),
  maxRatio  = numeric(),
  caliper = numeric(),
  caliperScale  = character()
)

stratifyByPsArgsList <- tibble::tibble(
  label = character(),
  numberOfStrata  = numeric(),
  baseSelection = character()
)

# Loop through each PS setting specified in the JSON and populate the respective lists.
for (psSetting in inputAnalysisSpecs$propensityScoreAdjustment$psSettings) {
  if (!is.null(psSetting$matchOnPsArgs)) {
    label <- paste0("Match (", psSetting$matchOnPsArgs$maxRatio, ":1, caliper=",
                    psSetting$matchOnPsArgs$caliper, " ", psSetting$matchOnPsArgs$caliperScale, ")")
    matchOnPsArgsList <- dplyr::add_row(matchOnPsArgsList,
                                        label = label,
                                        maxRatio = psSetting$matchOnPsArgs$maxRatio,
                                        caliper = psSetting$matchOnPsArgs$caliper,
                                        caliperScale = psSetting$matchOnPsArgs$caliperScale)
  }
  if (!is.null(psSetting$stratifyByPsArgs)) {
    # No stratify settings in the provided JSON, but keeping the logic for completeness.
    # Note: `stratifyByPsArgs` in the provided JSON is always null, so this block won't be entered.
    label <- paste0("Stratify (", psSetting$stratifyByPsArgs$numberOfStrata, " strata, base=",
                    psSetting$stratifyByPsArgs$baseSelection, ")")
    stratifyByPsArgsList <- dplyr::add_row(stratifyByPsArgsList,
                                           label = label,
                                           numberOfStrata = psSetting$stratifyByPsArgs$numberOfStrata,
                                           baseSelection = psSetting$stratifyByPsArgs$baseSelection)
  }
}

# Consolidate match and stratify settings into a single configuration list.
psConfigList <- list()

# Add matching configurations to psConfigList.
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

# Add stratification configurations to psConfigList.
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


# Iterate through all analysis setting combinations (study periods, time-at-risks, PS adjustments)
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      matchOnPsArgs <- NULL
      stratifyByPsArgs <- NULL

      # Create specific PS adjustment arguments based on the current configuration.
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Not specified in JSON, using template default
          stratificationColumns = c() # Not specified in JSON, using template default
        )
      } else if (psCfg$method == "stratify") {
        # This branch is not hit with the provided JSON, as `stratifyByPsArgs` is null in the JSON.
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Not specified in JSON, using template default
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings definition.
      # The JSON's `covariateSelection.conceptsToInclude` and `conceptsToExclude` are empty.
      # `FeatureExtraction::createDefaultCovariateSettings` is used, and it supports passing
      # included/excluded concept IDs if they were present.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE, # Not specified in JSON, using template default
        includedCovariateConceptIds = if (nrow(includedCovariateConcepts) > 0) includedCovariateConcepts$conceptId else c(),
        excludedCovariateConceptIds = if (nrow(excludedCovariateConcepts) > 0) excludedCovariateConcepts$conceptId else c()
      )

      # Combining primary outcome cohorts and negative control outcome cohorts.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not specified in JSON, using template default
            priorOutcomeLookback = 99999 # Not specified in JSON, using template default
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # Not specified in JSON, using template default
          )
        })
      )

      # Create Target-Comparator-Outcome list for CohortMethod.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        # `excludedCovariateConceptIds` refers to concept IDs to exclude from covariate generation.
        # Original template had erroneous references to cmTcList$targetConceptId, etc.
        # The corrected logic uses only the `excludedCovariateConcepts` dataframe.
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = if (nrow(excludedCovariateConcepts) > 0) excludedCovariateConcepts$conceptId else c()
        )
      }

      # getDbCohortMethodDataArgs: arguments for fetching data from the CDM.
      # Mapping directly from JSON, with current study period.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = inputAnalysisSpecs$getDbCohortMethodDataArgs$maxCohortSize,
        restrictToCommonPeriod = inputAnalysisSpecs$getDbCohortMethodDataArgs$restrictToCommonPeriod,
        firstExposureOnly = inputAnalysisSpecs$getDbCohortMethodDataArgs$firstExposureOnly,
        washoutPeriod = inputAnalysisSpecs$getDbCohortMethodDataArgs$washoutPeriod,
        removeDuplicateSubjects = inputAnalysisSpecs$getDbCohortMethodDataArgs$removeDuplicateSubjects,
        covariateSettings = covariateSettings # Using the dynamically created covariate settings
      )

      # createPsArgs: arguments for propensity score model fitting.
      # Mapping directly from JSON for prior and control settings.
      createPsArgs_json <- inputAnalysisSpecs$propensityScoreAdjustment$createPsArgs
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = createPsArgs_json$maxCohortSizeForFitting,
        errorOnHighCorrelation = createPsArgs_json$errorOnHighCorrelation,
        stopOnError = FALSE, # Template default, not specified in JSON
        estimator = "att", # Template default, not specified in JSON
        prior = Cyclops::createPrior(
          priorType = createPsArgs_json$prior$priorType,
          exclude = c(0), # Template default, not specified in JSON
          useCrossValidation = createPsArgs_json$prior$useCrossValidation
        ),
        control = Cyclops::createControl(
          noiseLevel = createPsArgs_json$control$noiseLevel,
          cvType = createPsArgs_json$control$cvType,
          seed = 1, # Template default, not specified in JSON
          resetCoefficients = createPsArgs_json$control$resetCoefficients,
          tolerance = createPsArgs_json$control$tolerance,
          cvRepetitions = createPsArgs_json$control$cvRepetitions,
          startingVariance = createPsArgs_json$control$startingVariance,
          fold = createPsArgs_json$control$fold # Using 'fold' from JSON
        )
      )

      # computeSharedCovariateBalanceArgs: arguments for computing covariate balance for shared covariates.
      # These are default values from the template, not specified in JSON.
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      # computeCovariateBalanceArgs: arguments for computing covariate balance, potentially with a filter.
      # These are default values from the template, not specified in JSON.
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs: arguments for fitting the outcome model.
      # Mapping directly from JSON for model type, stratified, useCovariates, inversePtWeighting, prior, and control.
      fitOutcomeModelArgs_json <- inputAnalysisSpecs$fitOutcomeModelArgs
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = fitOutcomeModelArgs_json$modelType,
        stratified = fitOutcomeModelArgs_json$stratified,
        useCovariates = fitOutcomeModelArgs_json$useCovariates,
        inversePtWeighting = fitOutcomeModelArgs_json$inversePtWeighting,
        prior = Cyclops::createPrior(
          priorType = fitOutcomeModelArgs_json$prior$priorType,
          useCrossValidation = fitOutcomeModelArgs_json$prior$useCrossValidation
        ),
        control = Cyclops::createControl(
          cvType = fitOutcomeModelArgs_json$control$cvType,
          seed = 1, # Template default, not specified in JSON
          resetCoefficients = fitOutcomeModelArgs_json$control$resetCoefficients,
          startingVariance = fitOutcomeModelArgs_json$control$startingVariance,
          tolerance = fitOutcomeModelArgs_json$control$tolerance,
          cvRepetitions = fitOutcomeModelArgs_json$control$cvRepetitions,
          noiseLevel = fitOutcomeModelArgs_json$control$noiseLevel,
          fold = fitOutcomeModelArgs_json$control$fold # Using 'fold' from JSON
        )
      )

      # createStudyPopArgs: arguments for defining the study population and risk window.
      # Mapping directly from JSON and current timeAtRisks iteration.
      createStudyPopArgs_json <- inputAnalysisSpecs$createStudyPopArgs
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = createStudyPopArgs_json$restrictToCommonPeriod,
        firstExposureOnly = createStudyPopArgs_json$firstExposureOnly,
        washoutPeriod = createStudyPopArgs_json$washoutPeriod,
        removeDuplicateSubjects = createStudyPopArgs_json$removeDuplicateSubjects,
        censorAtNewRiskWindow = createStudyPopArgs_json$censorAtNewRiskWindow,
        removeSubjectsWithPriorOutcome = createStudyPopArgs_json$removeSubjectsWithPriorOutcome,
        priorOutcomeLookback = createStudyPopArgs_json$priorOutcomeLookBack, # Note: JSON field name is 'priorOutcomeLookBack'
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # Using minDaysAtRisk from current timeAtRisks
        maxDaysAtRisk = 99999 # Template default, not specified in JSON
      )

      # Append the settings to the CohortMethod Analysis List.
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

# Create the CohortMethod module specifications using the generated analysis list.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Not specified in JSON, using template default
  refitPsForEveryOutcome = FALSE, # Not specified in JSON, using template default
  refitPsForEveryStudyPopulation = FALSE, # Not specified in JSON, using template default
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Not specified in JSON, using template default
)

# Create the overall Strategus analysis specifications ------------------------------------------
# Combine all shared resources and module specifications into a single Strategus object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the complete analysis specifications to a JSON file.
# The file name is constructed using the 'name' field from the input Analysis Specifications.
studyName <- inputAnalysisSpecs$name
outputFolderPath <- file.path("inst", studyName)

# FIX: Ensure the output directory exists before writing the file.
# The `saveSettingsToJson` function (which internally uses `jsonlite::write_json`)
# does not create directories recursively by default, leading to "argument is of length zero"
# error if the directory does not exist.
if (!dir.exists(outputFolderPath)) {
  dir.create(outputFolderPath, recursive = TRUE)
}

ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path(outputFolderPath, paste0(studyName, "AnalysisSpecification.json"))
)