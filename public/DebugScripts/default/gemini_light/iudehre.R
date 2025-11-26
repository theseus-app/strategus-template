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
library(jsonlite) # For reading the analysis specification JSON
library(ROhdsiWebApi) # For WebAPI calls
library(CohortGenerator) # For CohortGeneratorModule
library(CohortDiagnostics) # For CohortDiagnosticsModule
library(CohortMethod) # For CohortMethodModule
library(FeatureExtraction) # For covariate settings
library(Cyclops) # For prior and control in CohortMethod
library(ParallelLogger) # For saving settings

# --- Start of Analysis Specifications Loading ---
# This section loads the analysis settings from the provided JSON specification.
# In a real scenario, you might read this from a file.
# For this script, it's embedded as a string for direct use.
spec_json <- '{
  "name": "iudehre",
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
        "studyStartDate": "20030101",
        "studyEndDate": null
      }
    ],
    "maxCohortSize": 0,
    "restrictToCommonPeriod": false
  },
  "createStudyPopArgs": {
    "restrictToCommonPeriod": false,
    "firstExposureOnly": true,
    "washoutPeriod": 365,
    "removeDuplicateSubjects": "remove all",
    "censorAtNewRiskWindow": false,
    "removeSubjectsWithPriorOutcome": false,
    "priorOutcomeLookBack": 99999,
    "timeAtRisks": [
      {
        "riskWindowStart": 30,
        "startAnchor": "cohort start",
        "riskWindowEnd": 5475,
        "endAnchor": "cohort start",
        "minDaysAtRisk": 1
      },
      {
        "riskWindowStart": 365,
        "startAnchor": "cohort start",
        "riskWindowEnd": 5475,
        "endAnchor": "cohort start",
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
        "matchOnPsArgs": null,
        "stratifyByPsArgs": {
          "numberOfStrata": 5,
          "baseSelection": "all"
        }
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
spec <- jsonlite::fromJSON(spec_json)
# --- End of Analysis Specifications Loading ---

# Shared Resources -------------------------------------------------------------
# Base URL for the OHDSI WebAPI. Users should configure this for their environment.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI" 

# Cohort Definitions from Analysis Specifications
# This section defines the target, comparator, and outcome cohorts using IDs 
# provided in the analysis specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    spec$cohortDefinitions$targetCohort$id,    # Target cohort ID
    spec$cohortDefinitions$comparatorCohort$id, # Comparator cohort ID
    # FIX: Correctly access the outcome cohort ID. 'outcomeCohort' is a data.frame.
    spec$cohortDefinitions$outcomeCohort$id[1]  # Outcome cohort ID (first row)
  ),
  generateStats = TRUE
)

# Re-number cohorts to standard IDs used in Strategus modules (1, 2, 3...)
# This ensures a consistent mapping for internal use within the analysis.
# Target is assigned ID 1, Comparator ID 2, and Outcome ID 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == spec$cohortDefinitions$targetCohort$id,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == spec$cohortDefinitions$comparatorCohort$id,]$cohortId <- 2
# FIX: Correctly access the outcome cohort ID for re-numbering.
cohortDefinitionSet[cohortDefinitionSet$cohortId == spec$cohortDefinitions$outcomeCohort$id[1],]$cohortId <- 3

# Negative control outcomes from Analysis Specifications
# This section retrieves and formats the negative control concept set into a cohort set.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = spec$negativeControlConceptSet$id, # Negative control concept set ID
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
  # Negative control cohort IDs are offset to avoid clashes with target/comparator/outcome
  mutate(cohortId = row_number() + 100) %>% # Target/comparator/outcome IDs start 1, 2, 3... negative controls start from 101.
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs to prevent conflicts in analysis.
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis -------
# Outcomes: Extract the main outcome cohort details, re-numbered to ID 3.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Outcome cohort (re-numbered to 3)
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not specified in analysis specifications

# Target and Comparator for the CohortMethod analysis, using re-numbered IDs.
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered target cohort ID
  targetCohortName = spec$cohortDefinitions$targetCohort$name, # Name from Analysis Specifications
  comparatorCohortId = 2, # Re-numbered comparator cohort ID
  comparatorCohortName = spec$cohortDefinitions$comparatorCohort$name # Name from Analysis Specifications
)

# Excluded covariate concepts: From Analysis Specifications' covariateSelection.conceptsToExclude.
# If 'id' is null or an empty array, this list will be empty.
excludedCovariateConcepts <- spec$covariateSelection$conceptsToExclude %>%
  filter(!is.null(id)) %>% # Filter out entries where ID is null
  select(conceptId = id, conceptName = name)

# Optional: If you want to define covariates to include instead of including them all
includedCovariateConcepts <- spec$covariateSelection$conceptsToInclude %>%
  filter(!is.null(id)) %>% # Filter out entries where ID is null
  select(conceptId = id, conceptName = name)

# CohortGeneratorModule --------------------------------------------------------
# Defines how cohorts are generated.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Shared resource for cohort definitions, used across modules.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Shared resource for negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default, not specified in analysis specifications
  detectOnDescendants = TRUE # Default, not specified in analysis specifications
)
# Module specifications for cohort generation.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Generate cohort statistics
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# Defines how cohort diagnostics are performed.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # All cohort IDs to run diagnostics on
  runInclusionStatistics = TRUE, # Default, not specified in analysis specifications
  runIncludedSourceConcepts = TRUE, # Default, not specified in analysis specifications
  runOrphanConcepts = TRUE, # Default, not specified in analysis specifications
  runTimeSeries = FALSE, # Default, not specified in analysis specifications
  runVisitContext = TRUE, # Default, not specified in analysis specifications
  runBreakdownIndexEvents = TRUE, # Default, not specified in analysis specifications
  runIncidenceRate = TRUE, # Default, not specified in analysis specifications
  runCohortRelationship = TRUE, # Default, not specified in analysis specifications
  runTemporalCohortCharacterization = TRUE, # Default, not specified in analysis specifications
  minCharacterizationMean = 0.01 # Default, not specified in analysis specifications
)

# CohortMethodModule -----------------------------------------------------------

# Study periods from getDbCohortMethodDataArgs.studyPeriods in Analysis Specifications
# Using NA_character_ for null studyEndDate as it's a character field.
studyPeriods <- tibble(
  studyStartDate = unlist(lapply(spec$getDbCohortMethodDataArgs$studyPeriods, function(x) x$studyStartDate)), # YYYYMMDD
  studyEndDate   = unlist(lapply(spec$getDbCohortMethodDataArgs$studyPeriods, function(x) ifelse(is.null(x$studyEndDate), NA_character_, x$studyEndDate))) # YYYYMMDD
)

# Time-at-risks (TARs) for the outcomes of interest, from createStudyPopArgs.timeAtRisks in Analysis Specifications
timeAtRisks <- tibble(
  # Dynamically create descriptive labels for each TAR configuration
  label = paste0("TAR_S", 
                 unlist(lapply(spec$createStudyPopArgs$timeAtRisks, function(x) x$riskWindowStart)), 
                 "_", 
                 unlist(lapply(spec$createStudyPopArgs$timeAtRisks, function(x) x$startAnchor)), 
                 "_E", 
                 unlist(lapply(spec$createStudyPopArgs$timeAtRisks, function(x) x$riskWindowEnd)), 
                 "_", 
                 unlist(lapply(spec$createStudyPopArgs$timeAtRisks, function(x) x$endAnchor))), 
  riskWindowStart  = unlist(lapply(spec$createStudyPopArgs$timeAtRisks, function(x) x$riskWindowStart)),
  startAnchor = unlist(lapply(spec$createStudyPopArgs$timeAtRisks, function(x) x$startAnchor)), # "cohort start" | "cohort end"
  riskWindowEnd  = unlist(lapply(spec$createStudyPopArgs$timeAtRisks, function(x) x$riskWindowEnd)),
  endAnchor = unlist(lapply(spec$createStudyPopArgs$timeAtRisks, function(x) x$endAnchor)), # "cohort start" | "cohort end"
  minDaysAtRisk = unlist(lapply(spec$createStudyPopArgs$timeAtRisks, function(x) x$minDaysAtRisk)) # From spec
) 

# Propensity Score settings - match on PS, from propensityScoreAdjustment.psSettings in Analysis Specifications
matchOnPsArgsList <- tibble(
  label = character(),
  maxRatio  = numeric(),
  caliper = numeric(),
  caliperScale  = character()
) 

# Propensity Score settings - stratify by PS, from propensityScoreAdjustment.psSettings in Analysis Specifications
stratifyByPsArgsList <- tibble(
  label = character(),
  numberOfStrata  = numeric(),
  baseSelection = character()
) 

# Populate matchOnPsArgsList and stratifyByPsArgsList from the spec
for (psSetting in spec$propensityScoreAdjustment$psSettings) {
  if (!is.null(psSetting$matchOnPsArgs)) {
    matchOnPsArgsList <- matchOnPsArgsList %>%
      add_row(
        label = paste0("Match_MaxR", psSetting$matchOnPsArgs$maxRatio, "_Cal", psSetting$matchOnPsArgs$caliper, "_", psSetting$matchOnPsArgs$caliperScale),
        maxRatio = psSetting$matchOnPsArgs$maxRatio,
        caliper = psSetting$matchOnPsArgs$caliper,
        caliperScale = psSetting$matchOnPsArgs$caliperScale
      )
  } else if (!is.null(psSetting$stratifyByPsArgs)) {
    stratifyByPsArgsList <- stratifyByPsArgsList %>%
      add_row(
        label = paste0("Stratify_NStrata", psSetting$stratifyByPsArgs$numberOfStrata, "_", psSetting$stratifyByPsArgs$baseSelection),
        numberOfStrata = psSetting$stratifyByPsArgs$numberOfStrata,
        baseSelection = psSetting$stratifyByPsArgs$baseSelection
      )
  }
}

# Build a single PS configuration list (each entry has: method, label, params)
# This loop consolidates different PS adjustment methods into a unified list.
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
      params = list(
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection  = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}

# Iterate through all analysis setting combinations
# This nested loop generates a comprehensive list of CohortMethod analyses
# by combining different study periods, time-at-risks, and PS adjustment methods.
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Propensity score adjustment arguments based on the method
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default, not in spec
          stratificationColumns = c() # Default, not in spec
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default, not in spec
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings
      # Included/excluded covariate concepts from Analysis Specifications.
      # If 'id' is null, the list will be empty.
      includedConcepts <- if (nrow(includedCovariateConcepts) > 0) includedCovariateConcepts$conceptId else c()
      excludedConcepts <- if (nrow(excludedCovariateConcepts) > 0) excludedCovariateConcepts$conceptId else c()
      
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE, # Default, not in spec
        includedCovariateConceptIds = includedConcepts,
        excludedCovariateConceptIds = excludedConcepts
      )

      # Combine outcome cohorts and negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not applicable for observed outcomes
            priorOutcomeLookback = 99999 # Default, not in spec
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # For negative controls, true effect size is assumed 1 (no effect)
          )
        })
      )
      
      # Define target-comparator-outcome combinations for the analysis.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # The spec doesn't provide specific concept IDs for the target/comparator 
          # drugs themselves, only cohort IDs. Using c() as per original script.
          excludedCovariateConceptIds = c() 
        )
      }

      # Arguments for fetching cohort method data.
      # Parameters from getDbCohortMethodDataArgs in Analysis Specifications.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = spec$getDbCohortMethodDataArgs$maxCohortSize, # 0
        restrictToCommonPeriod = spec$getDbCohortMethodDataArgs$restrictToCommonPeriod, # false
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # Parameters from propensityScoreAdjustment.createPsArgs in Analysis Specifications.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = spec$propensityScoreAdjustment$createPsArgs$maxCohortSizeForFitting, # 250000
        errorOnHighCorrelation = spec$propensityScoreAdjustment$createPsArgs$errorOnHighCorrelation, # true
        stopOnError = FALSE, # Default, not in spec. Set to FALSE to allow Strategus to complete all CM operations.
        estimator = "att", # Default, not in spec. Average Treatment Effect on the Treated.
        prior = Cyclops::createPrior( # Prior distribution for regularization.
          priorType = spec$propensityScoreAdjustment$createPsArgs$prior$priorType, # laplace
          exclude = c(0), # Added: Common default to exclude intercept from regularization.
          useCrossValidation = spec$propensityScoreAdjustment$createPsArgs$prior$useCrossValidation # true
        ),
        control = Cyclops::createControl( # Control settings for Cyclops optimizer.
          noiseLevel = spec$propensityScoreAdjustment$createPsArgs$control$noiseLevel, # silent
          cvType = spec$propensityScoreAdjustment$createPsArgs$control$cvType, # auto
          fold = spec$propensityScoreAdjustment$createPsArgs$control$fold, # 10 (from spec)
          seed = 1, # Default, not in spec. For reproducibility.
          resetCoefficients = spec$propensityScoreAdjustment$createPsArgs$control$resetCoefficients, # true
          tolerance = spec$propensityScoreAdjustment$createPsArgs$control$tolerance, # 2e-07
          cvRepetitions = spec$propensityScoreAdjustment$createPsArgs$control$cvRepetitions, # 10 (from spec)
          startingVariance = spec$propensityScoreAdjustment$createPsArgs$control$startingVariance # 0.01
        )
      )

      # Arguments for computing shared covariate balance.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default, not in spec
        covariateFilter = NULL # Default, not in spec
      )
      # Arguments for computing covariate balance.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default, not in spec
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default, not in spec
      )

      # Arguments for fitting the outcome model.
      # Parameters from fitOutcomeModelArgs in Analysis Specifications.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = spec$fitOutcomeModelArgs$modelType, # cox
        stratified = spec$fitOutcomeModelArgs$stratified, # true
        useCovariates = spec$fitOutcomeModelArgs$useCovariates, # false
        inversePtWeighting = spec$fitOutcomeModelArgs$inversePtWeighting, # false
        prior = Cyclops::createPrior( # Prior distribution for regularization.
          priorType = spec$fitOutcomeModelArgs$prior$priorType, # laplace
          useCrossValidation = spec$fitOutcomeModelArgs$prior$useCrossValidation # true
        ),
        control = Cyclops::createControl( # Control settings for Cyclops optimizer.
          cvType = spec$fitOutcomeModelArgs$control$cvType, # auto
          fold = spec$fitOutcomeModelArgs$control$fold, # 10 (from spec)
          seed = 1, # Default, not in spec. For reproducibility.
          resetCoefficients = spec$fitOutcomeModelArgs$control$resetCoefficients, # true
          startingVariance = spec$fitOutcomeModelArgs$control$startingVariance, # 0.01
          tolerance = spec$fitOutcomeModelArgs$control$tolerance, # 2e-07
          cvRepetitions = spec$fitOutcomeModelArgs$control$cvRepetitions, # 10 (from spec)
          noiseLevel = spec$fitOutcomeModelArgs$control$noiseLevel # quiet
        )
      )
      
      # Arguments for creating the study population.
      # Parameters from createStudyPopArgs in Analysis Specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = spec$createStudyPopArgs$restrictToCommonPeriod, # false
        firstExposureOnly = spec$createStudyPopArgs$firstExposureOnly, # true (Moved from getDbCohortMethodDataArgs)
        washoutPeriod = spec$createStudyPopArgs$washoutPeriod, # 365 (Moved from getDbCohortMethodDataArgs)
        removeDuplicateSubjects = spec$createStudyPopArgs$removeDuplicateSubjects, # "remove all" (Moved from getDbCohortMethodDataArgs)
        censorAtNewRiskWindow = spec$createStudyPopArgs$censorAtNewRiskWindow, # false
        removeSubjectsWithPriorOutcome = spec$createStudyPopArgs$removeSubjectsWithPriorOutcome, # false
        priorOutcomeLookback = spec$createStudyPopArgs$priorOutcomeLookBack, # FIX: Corrected typo from 'priorOutcomeLookBack' to 'priorOutcomeLookback'
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # 1 (from spec's timeAtRisks)
        maxDaysAtRisk = 99999 # Default, not in spec
      )

      # Append the settings to Analysis List
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

# CohortMethodModule specifications
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # No analyses specified for exclusion
  refitPsForEveryOutcome = FALSE, # Default, not in spec
  refitPsForEveryStudyPopulation = FALSE, # Default, not in spec 
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default thresholds for diagnostics
)

# Create the analysis specifications ------------------------------------------
# This section assembles all shared resources and module specifications 
# into a complete Strategus analysis specification.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path is constructed using the study name from the analysis specifications.
outputFolder <- file.path("inst", spec$name)
if (!dir.exists(outputFolder)) {
  dir.create(outputFolder, recursive = TRUE)
}
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path(outputFolder, paste0(spec$name, "AnalysisSpecification.json"))
)