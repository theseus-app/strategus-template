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
library(ROhdsiWebApi) # Required for WebAPI calls
library(CohortMethod) # Required for CohortMethod module functions
library(FeatureExtraction) # Required for CovariateSettings
library(Cyclops) # Required for Prior and Control settings
library(tibble) # Used for tibble data structures
library(jsonlite) # Used for parsing JSON input

# Parse the analysis specifications from JSON
# This allows dynamic configuration of the study from a JSON string.
# In a real scenario, this JSON would likely be loaded from a file.
analysis_spec <- jsonlite::fromJSON(
  '{
    "name": "ranitidinecancer",
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
          "studyStartDate": "",
          "studyEndDate": ""
        }
      ],
      "maxCohortSize": 0,
      "restrictToCommonPeriod": false
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
          "riskWindowEnd": 99999,
          "endAnchor": "cohort start",
          "minDaysAtRisk": 1
        },
        {
          "riskWindowStart": 365,
          "startAnchor": "cohort start",
          "riskWindowEnd": 99999,
          "endAnchor": "cohort start",
          "minDaysAtRisk": 1
        },
        {
          "riskWindowStart": 1,
          "startAnchor": "cohort start",
          "riskWindowEnd": 0,
          "endAnchor": "cohort end",
          "minDaysAtRisk": 1
        },
        {
          "riskWindowStart": 365,
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
        },
        {
          "matchOnPsArgs": {
            "maxRatio": 10,
            "caliper": 0.2,
            "caliperScale": "standardized logit"
          },
          "stratifyByPsArgs": null
        },
        {
          "matchOnPsArgs": null,
          "stratifyByPsArgs": {
            "numberOfStrata": 10,
            "baseSelection": "all"
          }
        },
        {
          "matchOnPsArgs": null,
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
)

# Shared Resources -------------------------------------------------------------
# Placeholder for Atlas/WebAPI base URL - update if needed
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Fetch cohort definitions from WebAPI based on IDs specified in analysis_spec.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    analysis_spec$cohortDefinitions$targetCohort$id,
    analysis_spec$cohortDefinitions$comparatorCohort$id,
    analysis_spec$cohortDefinitions$outcomeCohort$id[1] # Fix: Access 'id' column directly, then the first element
  ),
  generateStats = TRUE
)

# Re-number cohorts for consistent internal use (target=1, comparator=2, outcome=3)
cohortDefinitionSet[cohortDefinitionSet$cohortId == analysis_spec$cohortDefinitions$targetCohort$id,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == analysis_spec$cohortDefinitions$comparatorCohort$id,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == analysis_spec$cohortDefinitions$outcomeCohort$id[1],]$cohortId <- 3 # Fix: Access 'id' column directly, then the first element

# Negative control outcomes
# Resolve the negative control concept set ID from analysis_spec to a set of outcomes.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = analysis_spec$negativeControlConceptSet$id,
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
  mutate(cohortId = row_number() + 100) %>% # Assign cohort IDs starting from 101 to avoid collision with T/C/O
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs across definitions
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts used in each analysis ---------------
# Outcomes: Filter the re-numbered cohort definitions for the outcome cohort (ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window for outcome

# Target and Comparator for the CohortMethod analysis 
# Use the re-numbered target (ID 1) and comparator (ID 2) cohort information.
cmTcList <- data.frame(
  targetCohortId = 1, # Remapped target cohort ID
  targetCohortName = analysis_spec$cohortDefinitions$targetCohort$name, # Target cohort name from analysis_spec
  comparatorCohortId = 2, # Remapped comparator cohort ID
  comparatorCohortName = analysis_spec$cohortDefinitions$comparatorCohort$name # Comparator cohort name from analysis_spec
)

# For the CohortMethod LSPS we'll need to exclude specific concepts.
# Check if conceptsToExclude are provided and valid in analysis_spec.
excludedCovariateConcepts <- data.frame(conceptId = integer(0), conceptName = character(0))
if (!is.null(analysis_spec$covariateSelection$conceptsToExclude) && 
    length(analysis_spec$covariateSelection$conceptsToExclude) > 0 &&
    !is.null(analysis_spec$covariateSelection$conceptsToExclude[[1]]$id)) {
  # Filter out rows where 'id' is NULL if present from JSON parsing
  valid_excluded_concepts <- analysis_spec$covariateSelection$conceptsToExclude[!sapply(analysis_spec$covariateSelection$conceptsToExclude, function(x) is.null(x$id))]
  if (length(valid_excluded_concepts) > 0) {
    excludedCovariateConcepts <- bind_rows(lapply(valid_excluded_concepts, as.data.frame)) %>%
      rename(conceptId = id, conceptName = name)
  }
}

# Optional: If you want to define covariates to include instead of including them all
# Check if conceptsToInclude are provided and valid in analysis_spec.
includedCovariateConcepts <- data.frame(conceptId = integer(0), conceptName = character(0))
if (!is.null(analysis_spec$covariateSelection$conceptsToInclude) && 
    length(analysis_spec$covariateSelection$conceptsToInclude) > 0 &&
    !is.null(analysis_spec$covariateSelection$conceptsToInclude[[1]]$id)) {
  # Filter out rows where 'id' is NULL if present from JSON parsing
  valid_included_concepts <- analysis_spec$covariateSelection$conceptsToInclude[!sapply(analysis_spec$covariateSelection$conceptsToInclude, function(x) is.null(x$id))]
  if (length(valid_included_concepts) > 0) {
    includedCovariateConcepts <- bind_rows(lapply(valid_included_concepts, as.data.frame)) %>%
      rename(conceptId = id, conceptName = name)
  }
}


# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Shared resource for cohort definitions, including T, C, O
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Shared resource for negative control outcomes
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default from template
  detectOnDescendants = TRUE # Default from template
)
# Module specifications for CohortGenerator, generating stats for all cohorts
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Module specifications for CohortDiagnostics, running various diagnostics for all defined cohorts
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # All cohort IDs (T, C, O)
  runInclusionStatistics = TRUE, # Default from template
  runIncludedSourceConcepts = TRUE, # Default from template
  runOrphanConcepts = TRUE, # Default from template
  runTimeSeries = FALSE, # Default from template
  runVisitContext = TRUE, # Default from template
  runBreakdownIndexEvents = TRUE, # Default from template
  runIncidenceRate = TRUE, # Default from template
  runCohortRelationship = TRUE, # Default from template
  runTemporalCohortCharacterization = TRUE, # Default from template
  minCharacterizationMean = 0.01 # Default from template
)

# CohortMethodModule -----------------------------------------------------------

# Study periods from analysis_spec (if empty strings, means no restriction)
studyPeriodsJson <- analysis_spec$getDbCohortMethodDataArgs$studyPeriods[[1]]
studyPeriods <- tibble(
  studyStartDate = if(studyPeriodsJson$studyStartDate == "") character(0) else studyPeriodsJson$studyStartDate,
  studyEndDate   = if(studyPeriodsJson$studyEndDate == "") character(0) else studyPeriodsJson$studyEndDate
)
# If no specific dates, represent as a single unrestricted period
if (nrow(studyPeriods) == 0 || (length(studyPeriods$studyStartDate) == 0 && length(studyPeriods$studyEndDate) == 0)) { 
  studyPeriods <- tibble(studyStartDate = "", studyEndDate = "")
}

# Time-at-risks (TARs) for the outcomes of interest in your study, from analysis_spec
timeAtRisks <- tibble(
  label = character(0),
  riskWindowStart  = integer(0),
  startAnchor = character(0),
  riskWindowEnd  = integer(0),
  endAnchor = character(0),
  minDaysAtRisk = integer(0)
) 
for (tar_spec in analysis_spec$createStudyPopArgs$timeAtRisks) {
  label <- sprintf("TAR_S%s%s_E%s%s", 
                   tar_spec$riskWindowStart, 
                   substr(tar_spec$startAnchor, 1, 1), 
                   tar_spec$riskWindowEnd, 
                   substr(tar_spec$endAnchor, 1, 1))
  timeAtRisks <- bind_rows(timeAtRisks, tibble(
    label = label,
    riskWindowStart = tar_spec$riskWindowStart,
    startAnchor = tar_spec$startAnchor,
    riskWindowEnd = tar_spec$riskWindowEnd,
    endAnchor = tar_spec$endAnchor,
    minDaysAtRisk = tar_spec$minDaysAtRisk
  ))
}

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()
psSettingsJson <- analysis_spec$propensityScoreAdjustment$psSettings
for (i in seq_along(psSettingsJson)) {
  setting <- psSettingsJson[[i]]
  if (!is.null(setting$matchOnPsArgs)) {
    matchArgs <- setting$matchOnPsArgs
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "match",
      label  = sprintf("Match_MaxRatio%s_Caliper%s_%s", matchArgs$maxRatio, matchArgs$caliper, matchArgs$caliperScale),
      params = list(
        maxRatio     = matchArgs$maxRatio,
        caliper      = matchArgs$caliper,
        caliperScale = matchArgs$caliperScale
      )
    )
  } else if (!is.null(setting$stratifyByPsArgs)) {
    stratifyArgs <- setting$stratifyByPsArgs
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "stratify",
      label  = sprintf("Stratify_Strata%s_Base%s", stratifyArgs$numberOfStrata, stratifyArgs$baseSelection),
      params = list(
        numberOfStrata = stratifyArgs$numberOfStrata,
        baseSelection  = stratifyArgs$baseSelection
      )
    )
  } else { # Both matchOnPsArgs and stratifyByPsArgs are null, indicating no PS adjustment
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "none",
      label  = "NoPsAdjustment",
      params = list()
    )
  }
}

# Define outcomes: combine the specific outcome with negative controls.
# This part is independent of study periods, TAR, or PS settings, so it's defined once.
outcomeList <- append(
  lapply(seq_len(nrow(oList)), function(i) {
    CohortMethod::createOutcome(
      outcomeId = oList$outcomeCohortId[i],
      outcomeOfInterest = TRUE,
      trueEffectSize = NA,
      priorOutcomeLookback = analysis_spec$createStudyPopArgs$priorOutcomeLookBack
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

# Define target-comparator-outcome combinations.
# This part is also independent of loop variables and should be defined once.
targetComparatorOutcomesList <- list()
for (i in seq_len(nrow(cmTcList))) {
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    excludedCovariateConceptIds = excludedCovariateConcepts$conceptId # Fixed: Only include existing excluded concepts
  )
}

# Iterate through all analysis setting combinations
cmAnalysisList <- list()
analysisId <- 1

for (s_idx in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s_idx]
  studyEndDate <- studyPeriods$studyEndDate[s_idx]

  for (t_idx in seq_len(nrow(timeAtRisks))) {
    
    # Define covariate settings. Use default and potentially add specific included concepts.
    covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
      addDescendantsToExclude = TRUE # Default from template
    )
    if (nrow(includedCovariateConcepts) > 0) {
      # If specific concepts to include are provided, create additional settings for them.
      # These example settings are not configurable via the current JSON.
      # Users may need to extend the JSON or hardcode if more granularity is needed.
      specificCovariateSettings <- FeatureExtraction::createCovariateSettings(
        # Example settings for specific covariates (adjust as needed if not defined in JSON)
        useDemographicsGender = FALSE, 
        useDemographicsAge = FALSE,
        useDemographicsRace = FALSE,
        useDemographicsEthnicity = FALSE,
        useDemographicsIndexYear = FALSE,
        useDemographicsIndexMonth = FALSE,
        useDemographicsPriorObservationTime = FALSE,
        useDemographicsPostObservationTime = FALSE,
        useDemographicsTimeInCohort = FALSE,
        useDemographicsDateOfFirstObservation = FALSE,
        useDemographicsMaritalStatus = FALSE,
        useDemographicsEmploymentStatus = FALSE,
        useDemographicsLivingSituation = FALSE,
        useConditionOccurrence = FALSE,
        useConditionOccurrenceLongTerm = FALSE,
        useConditionOccurrenceShortTerm = FALSE,
        useConditionOccurrenceInPriorDays = FALSE,
        useConditionGroupEra = FALSE,
        useConditionGroupEraLongTerm = FALSE,
        useConditionGroupEraShortTerm = FALSE,
        useConditionGroupEraOverlapping = FALSE,
        useDrugExposure = FALSE,
        useDrugExposureLongTerm = FALSE,
        useDrugExposureShortTerm = FALSE,
        useDrugExposureInPriorDays = FALSE,
        useDrugEra = FALSE,
        useDrugEraLongTerm = FALSE,
        useDrugEraShortTerm = FALSE,
        useDrugEraOverlapping = FALSE,
        useProcedureOccurrence = FALSE,
        useProcedureOccurrenceLongTerm = FALSE,
        useProcedureOccurrenceShortTerm = FALSE,
        useProcedureOccurrenceInPriorDays = FALSE,
        useDeviceExposure = FALSE,
        useDeviceExposureLongTerm = FALSE,
        useDeviceExposureShortTerm = FALSE,
        useDeviceExposureInPriorDays = FALSE,
        useMeasurement = FALSE,
        useMeasurementLongTerm = FALSE,
        useMeasurementShortTerm = FALSE,
        useMeasurementInPriorDays = FALSE,
        useMeasurementValue = FALSE,
        useMeasurementRangeGroup = FALSE,
        useObservation = FALSE,
        useObservationLongTerm = FALSE,
        useObservationShortTerm = FALSE,
        useObservationInPriorDays = FALSE,
        useVisitOccurrence = FALSE,
        useVisitOccurrenceLongTerm = FALSE,
        useVisitOccurrenceShortTerm = FALSE,
        useVisitOccurrenceInPriorDays = FALSE,
        useCharlsonIndex = FALSE,
        useDcsi = FALSE,
        useChads2 = FALSE,
        useChads2Vasc = FALSE,
        useHfrs = FALSE,
        useDistinctIngredientCount = FALSE,
        useDistinctATC3CodeCount = FALSE,
        useDistinctATC4CodeCount = FALSE,
        useVisitCount = FALSE,
        useVisitCountLongTerm = FALSE,
        useVisitCountShortTerm = FALSE,
        useVisitConceptCount = FALSE,
        useRiskScores = FALSE,
        addSpecificAggregatedCovariates = TRUE,
        specificCovariateSettings = list(
          FeatureExtraction::createSpecificAggregateCovariateSettings(
            inclusionConceptIds = includedCovariateConcepts$conceptId,
            covariateName = "CustomIncludedConcept",
            overwriteWindow = FALSE,
            windowStart = -30, # Example, not in JSON
            windowEnd = 0,     # Example, not in JSON
            analysisId = 999
          )
        )
      )
      # Combine default and specific covariate settings
      covariateSettings <- FeatureExtraction::createDetailedCovariateSettings(
        settings = list(covariateSettings, specificCovariateSettings)
      )
    }

    for (p_idx in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p_idx]]
      
      matchOnPsArgs <- NULL
      stratifyByPsArgs <- NULL
      if (psCfg$method == "match") {
        # Create MatchOnPsArgs based on analysis_spec
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default from template
          stratificationColumns = c() # Default from template
        )
      } else if (psCfg$method == "stratify") {
        # Create StratifyByPsArgs based on analysis_spec
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default from template
          baseSelection = psCfg$params$baseSelection
        )
      } # If psCfg$method == "none", both remain NULL, indicating no PS adjustment

      # Arguments for retrieving cohort method data from the database
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = analysis_spec$getDbCohortMethodDataArgs$restrictToCommonPeriod,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = analysis_spec$getDbCohortMethodDataArgs$maxCohortSize,
        # Fix: Removed firstExposureOnly, washoutPeriod, removeDuplicateSubjects
        # as they belong to createStudyPopArgs.
        covariateSettings = covariateSettings
      )

      # Arguments for propensity score model creation
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = analysis_spec$propensityScoreAdjustment$createPsArgs$maxCohortSizeForFitting,
        errorOnHighCorrelation = analysis_spec$propensityScoreAdjustment$createPsArgs$errorOnHighCorrelation,
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att", # Default from template
        prior = Cyclops::createPrior( # Prior settings for PS model, from analysis_spec
          priorType = analysis_spec$propensityScoreAdjustment$createPsArgs$prior$priorType,
          exclude = c(0), # Default from template (for intercept)
          useCrossValidation = analysis_spec$propensityScoreAdjustment$createPsArgs$prior$useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings for PS model, from analysis_spec
          noiseLevel = analysis_spec$propensityScoreAdjustment$createPsArgs$control$noiseLevel,
          cvType = analysis_spec$propensityScoreAdjustment$createPsArgs$control$cvType,
          seed = 1, # Default from template
          resetCoefficients = analysis_spec$propensityScoreAdjustment$createPsArgs$control$resetCoefficients,
          tolerance = analysis_spec$propensityScoreAdjustment$createPsArgs$control$tolerance,
          cvRepetitions = analysis_spec$propensityScoreAdjustment$createPsArgs$control$cvRepetitions,
          startingVariance = analysis_spec$propensityScoreAdjustment$createPsArgs$control$startingVariance
        )
      )

      # Arguments for computing shared covariate balance (e.g., for equipoise diagnostics)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default from template
        covariateFilter = NULL # Default from template
      )
      
      # Arguments for computing covariate balance for table 1
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default from template
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default from template
      )

      # Arguments for fitting the outcome model
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = analysis_spec$fitOutcomeModelArgs$modelType,
        stratified = analysis_spec$fitOutcomeModelArgs$stratified,
        useCovariates = analysis_spec$fitOutcomeModelArgs$useCovariates,
        inversePtWeighting = analysis_spec$fitOutcomeModelArgs$inversePtWeighting,
        prior = Cyclops::createPrior( # Prior settings for outcome model, from analysis_spec
          priorType = analysis_spec$fitOutcomeModelArgs$prior$priorType,
          useCrossValidation = analysis_spec$fitOutcomeModelArgs$prior$useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings for outcome model, from analysis_spec
          cvType = analysis_spec$fitOutcomeModelArgs$control$cvType,
          seed = 1, # Default from template
          resetCoefficients = analysis_spec$fitOutcomeModelArgs$control$resetCoefficients,
          startingVariance = analysis_spec$fitOutcomeModelArgs$control$startingVariance,
          tolerance = analysis_spec$fitOutcomeModelArgs$control$tolerance,
          cvRepetitions = analysis_spec$fitOutcomeModelArgs$control$cvRepetitions,
          noiseLevel = analysis_spec$fitOutcomeModelArgs$control$noiseLevel
        )
      )
      
      # Arguments for creating the study population
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = analysis_spec$createStudyPopArgs$restrictToCommonPeriod,
        firstExposureOnly = analysis_spec$createStudyPopArgs$firstExposureOnly,
        washoutPeriod = analysis_spec$createStudyPopArgs$washoutPeriod,
        removeDuplicateSubjects = analysis_spec$createStudyPopArgs$removeDuplicateSubjects,
        censorAtNewRiskWindow = analysis_spec$createStudyPopArgs$censorAtNewRiskWindow,
        removeSubjectsWithPriorOutcome = analysis_spec$createStudyPopArgs$removeSubjectsWithPriorOutcome,
        priorOutcomeLookback = analysis_spec$createStudyPopArgs$priorOutcomeLookBack,
        riskWindowStart = timeAtRisks$riskWindowStart[t_idx],
        startAnchor = timeAtRisks$startAnchor[t_idx],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t_idx],
        endAnchor = timeAtRisks$endAnchor[t_idx],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t_idx],
        maxDaysAtRisk = 99999 # Default from template (not specified in JSON)
      )

      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "StudyDates:%s-%s; TAR:%s; PS:%s",
          ifelse(studyStartDate == "", "Unrestricted", studyStartDate), # Description for study period
          ifelse(studyEndDate == "", "Unrestricted", studyEndDate), # Description for study period
          timeAtRisks$label[t_idx], # Description for time-at-risk
          psCfg$label # Description for PS adjustment method
        ),
        getDbCohortMethodDataArgs = getDbCohortMethodDataArgs,
        createStudyPopArgs = createStudyPopArgs,
        createPsArgs = createPsArgs,
        matchOnPsArgs = matchOnPsArgs, # PS matching arguments (NULL if stratify or none)
        stratifyByPsArgs = stratifyByPsArgs, # PS stratification arguments (NULL if match or none)
        computeSharedCovariateBalanceArgs = computeSharedCovariateBalanceArgs,
        computeCovariateBalanceArgs = computeCovariateBalanceArgs,
        fitOutcomeModelArgs = fitOutcomeModelArgs
      )
      analysisId <- analysisId + 1
    }
  }
}

# CohortMethodModule Specifications
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList, # List of all CM analyses to run
  targetComparatorOutcomesList = targetComparatorOutcomesList, # List of TCO combinations (defined once above loops)
  analysesToExclude = NULL, # No analyses to exclude by default
  refitPsForEveryOutcome = FALSE, # Default from template
  refitPsForEveryStudyPopulation = FALSE, # Default from template 
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default CM diagnostic thresholds
)

# Create the analysis specifications ------------------------------------------
# Combine all module specifications and shared resources into a single analysis specification object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", analysis_spec$name, paste0(analysis_spec$name, "AnalysisSpecification.json"))
)