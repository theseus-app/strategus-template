# This script creates an OHDSI Strategus analysis specification for a comparative
# cohort study, based on the provided JSON analysis specifications.
# It uses the Strategus R package along with HADES packages like CohortMethod,
# FeatureExtraction, ROhdsiWebApi, and Cyclops.

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
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(tibble)
library(jsonlite)

# Parse the analysis specifications from JSON
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
      "restrictToCommonPeriod": false,
      "firstExposureOnly": false,
      "washoutPeriod": 365,
      "removeDuplicateSubjects": "keep first"
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
# The `cohortIds` are remapped to 1, 2, 3 for internal consistency.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    analysis_spec$cohortDefinitions$targetCohort$id,      # Target cohort ID from analysis_spec
    analysis_spec$cohortDefinitions$comparatorCohort$id,  # Comparator cohort ID from analysis_spec
    analysis_spec$cohortDefinitions$outcomeCohort[[1]]$id # Outcome cohort ID from analysis_spec
  ),
  generateStats = TRUE
)

# Re-number cohorts for consistent internal use (target=1, comparator=2, outcome=3)
cohortDefinitionSet[cohortDefinitionSet$cohortId == analysis_spec$cohortDefinitions$targetCohort$id,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == analysis_spec$cohortDefinitions$comparatorCohort$id,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == analysis_spec$cohortDefinitions$outcomeCohort[[1]]$id,]$cohortId <- 3

# Negative control outcomes
# Resolve the negative control concept set ID from analysis_spec to a set of outcomes.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = analysis_spec$negativeControlConceptSet$id, # Negative control concept set ID from analysis_spec
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
if (!is.null(analysis_spec$covariateSelection$conceptsToExclude) && 
    length(analysis_spec$covariateSelection$conceptsToExclude) > 0 &&
    !is.null(analysis_spec$covariateSelection$conceptsToExclude[[1]]$id)) {
  excludedCovariateConcepts <- bind_rows(lapply(analysis_spec$covariateSelection$conceptsToExclude, as.data.frame)) %>%
    rename(conceptId = id, conceptName = name)
} else {
  excludedCovariateConcepts <- data.frame(conceptId = integer(0), conceptName = character(0))
}

# Optional: If you want to define covariates to include instead of including them all
# Check if conceptsToInclude are provided and valid in analysis_spec.
if (!is.null(analysis_spec$covariateSelection$conceptsToInclude) && 
    length(analysis_spec$covariateSelection$conceptsToInclude) > 0 &&
    !is.null(analysis_spec$covariateSelection$conceptsToInclude[[1]]$id)) {
  includedCovariateConcepts <- bind_rows(lapply(analysis_spec$covariateSelection$conceptsToInclude, as.data.frame)) %>%
    rename(conceptId = id, conceptName = name)
} else {
  includedCovariateConcepts <- data.frame(conceptId = integer(0), conceptName = character(0))
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
if (nrow(studyPeriods) == 0) { # If no specific dates, represent as a single unrestricted period
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
      specificCovariateSettings <- FeatureExtraction::createCovariateSettings(
        useDemographicsGender = FALSE, # Example, adjust as needed
        # ... other general settings you want to keep false ...
        addSpecificAggregatedCovariates = TRUE,
        specificCovariateSettings = list(
          FeatureExtraction::createSpecificAggregateCovariateSettings(
            inclusionConceptIds = includedCovariateConcepts$conceptId,
            # Adjust window and other settings as per specific requirements
            # Not specified in JSON, so using defaults/examples
            covariateName = "CustomIncludedConcept",
            overwriteWindow = FALSE,
            windowStart = -30,
            windowEnd = 0,
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
          maxRatio = psCfg$params$maxRatio, # maxRatio from analysis_spec
          caliper = psCfg$params$caliper, # caliper from analysis_spec
          caliperScale = psCfg$params$caliperScale, # caliperScale from analysis_spec
          allowReverseMatch = FALSE, # Default from template
          stratificationColumns = c() # Default from template
        )
      } else if (psCfg$method == "stratify") {
        # Create StratifyByPsArgs based on analysis_spec
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata, # numberOfStrata from analysis_spec
          stratificationColumns = c(), # Default from template
          baseSelection = psCfg$params$baseSelection # baseSelection from analysis_spec
        )
      } # If psCfg$method == "none", both remain NULL, indicating no PS adjustment

      # Define outcomes: combine the specific outcome with negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i], # Outcome cohort ID
            outcomeOfInterest = TRUE, # This is the primary outcome
            trueEffectSize = NA, # Unknown true effect size for primary outcome
            priorOutcomeLookback = analysis_spec$createStudyPopArgs$priorOutcomeLookBack # priorOutcomeLookback from analysis_spec
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i, # Negative control outcome ID
            outcomeOfInterest = FALSE, # These are negative controls
            trueEffectSize = 1 # Assumed true effect size for negative controls
          )
        })
      )
      
      # Define target-comparator-outcome combinations
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i], # Remapped target cohort ID
          comparatorId = cmTcList$comparatorCohortId[i], # Remapped comparator cohort ID
          outcomes = outcomeList, # Combined primary and negative control outcomes
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId # Concepts to exclude from covariates, from analysis_spec
          # Note: target and comparator drug concept IDs are not provided in analysis_spec for auto-exclusion
        )
      }

      # Arguments for retrieving cohort method data from the database
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = analysis_spec$getDbCohortMethodDataArgs$restrictToCommonPeriod, # restrictToCommonPeriod from analysis_spec
        studyStartDate = studyStartDate, # Current study start date from loop
        studyEndDate = studyEndDate, # Current study end date from loop
        maxCohortSize = analysis_spec$getDbCohortMethodDataArgs$maxCohortSize, # maxCohortSize from analysis_spec
        firstExposureOnly = analysis_spec$getDbCohortMethodDataArgs$firstExposureOnly, # firstExposureOnly from analysis_spec
        washoutPeriod = analysis_spec$getDbCohortMethodDataArgs$washoutPeriod, # washoutPeriod from analysis_spec
        removeDuplicateSubjects = analysis_spec$getDbCohortMethodDataArgs$removeDuplicateSubjects, # removeDuplicateSubjects from analysis_spec
        covariateSettings = covariateSettings # Defined covariate settings
      )

      # Arguments for propensity score model creation
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = analysis_spec$propensityScoreAdjustment$createPsArgs$maxCohortSizeForFitting, # maxCohortSizeForFitting from analysis_spec
        errorOnHighCorrelation = analysis_spec$propensityScoreAdjustment$createPsArgs$errorOnHighCorrelation, # errorOnHighCorrelation from analysis_spec
        stopOnError = FALSE, # Default from template to allow Strategus to complete
        estimator = "att", # Default from template
        prior = Cyclops::createPrior( # Prior settings for PS model, from analysis_spec
          priorType = analysis_spec$propensityScoreAdjustment$createPsArgs$prior$priorType, # priorType from analysis_spec
          exclude = c(0), # Default from template (for intercept)
          useCrossValidation = analysis_spec$propensityScoreAdjustment$createPsArgs$prior$useCrossValidation # useCrossValidation from analysis_spec
        ),
        control = Cyclops::createControl( # Control settings for PS model, from analysis_spec
          noiseLevel = analysis_spec$propensityScoreAdjustment$createPsArgs$control$noiseLevel, # noiseLevel from analysis_spec
          cvType = analysis_spec$propensityScoreAdjustment$createPsArgs$control$cvType, # cvType from analysis_spec
          seed = 1, # Default from template
          resetCoefficients = analysis_spec$propensityScoreAdjustment$createPsArgs$control$resetCoefficients, # resetCoefficients from analysis_spec
          tolerance = analysis_spec$propensityScoreAdjustment$createPsArgs$control$tolerance, # tolerance from analysis_spec
          cvRepetitions = analysis_spec$propensityScoreAdjustment$createPsArgs$control$cvRepetitions, # cvRepetitions from analysis_spec
          startingVariance = analysis_spec$propensityScoreAdjustment$createPsArgs$control$startingVariance # startingVariance from analysis_spec
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
        modelType = analysis_spec$fitOutcomeModelArgs$modelType, # modelType from analysis_spec
        stratified = analysis_spec$fitOutcomeModelArgs$stratified, # stratified from analysis_spec
        useCovariates = analysis_spec$fitOutcomeModelArgs$useCovariates, # useCovariates from analysis_spec
        inversePtWeighting = analysis_spec$fitOutcomeModelArgs$inversePtWeighting, # inversePtWeighting from analysis_spec
        prior = Cyclops::createPrior( # Prior settings for outcome model, from analysis_spec
          priorType = analysis_spec$fitOutcomeModelArgs$prior$priorType, # priorType from analysis_spec
          useCrossValidation = analysis_spec$fitOutcomeModelArgs$prior$useCrossValidation # useCrossValidation from analysis_spec
        ),
        control = Cyclops::createControl( # Control settings for outcome model, from analysis_spec
          cvType = analysis_spec$fitOutcomeModelArgs$control$cvType, # cvType from analysis_spec
          seed = 1, # Default from template
          resetCoefficients = analysis_spec$fitOutcomeModelArgs$control$resetCoefficients, # resetCoefficients from analysis_spec
          startingVariance = analysis_spec$fitOutcomeModelArgs$control$startingVariance, # startingVariance from analysis_spec
          tolerance = analysis_spec$fitOutcomeModelArgs$control$tolerance, # tolerance from analysis_spec
          cvRepetitions = analysis_spec$fitOutcomeModelArgs$control$cvRepetitions, # cvRepetitions from analysis_spec
          noiseLevel = analysis_spec$fitOutcomeModelArgs$control$noiseLevel # noiseLevel from analysis_spec
        )
      )
      
      # Arguments for creating the study population
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = analysis_spec$createStudyPopArgs$restrictToCommonPeriod, # restrictToCommonPeriod from analysis_spec
        firstExposureOnly = analysis_spec$createStudyPopArgs$firstExposureOnly, # firstExposureOnly from analysis_spec
        washoutPeriod = analysis_spec$createStudyPopArgs$washoutPeriod, # washoutPeriod from analysis_spec
        removeDuplicateSubjects = analysis_spec$createStudyPopArgs$removeDuplicateSubjects, # removeDuplicateSubjects from analysis_spec
        censorAtNewRiskWindow = analysis_spec$createStudyPopArgs$censorAtNewRiskWindow, # censorAtNewRiskWindow from analysis_spec
        removeSubjectsWithPriorOutcome = analysis_spec$createStudyPopArgs$removeSubjectsWithPriorOutcome, # removeSubjectsWithPriorOutcome from analysis_spec
        priorOutcomeLookback = analysis_spec$createStudyPopArgs$priorOutcomeLookBack, # priorOutcomeLookBack from analysis_spec
        riskWindowStart = timeAtRisks$riskWindowStart[t_idx], # Current riskWindowStart from loop
        startAnchor = timeAtRisks$startAnchor[t_idx], # Current startAnchor from loop
        riskWindowEnd = timeAtRisks$riskWindowEnd[t_idx], # Current riskWindowEnd from loop
        endAnchor = timeAtRisks$endAnchor[t_idx], # Current endAnchor from loop
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t_idx], # Current minDaysAtRisk from loop
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
  targetComparatorOutcomesList = targetComparatorOutcomesList, # List of TCO combinations
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