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

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
# Base URL for OHDSI WebAPI, used to fetch cohort definitions and concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Fetch cohort definitions based on IDs from Analysis Specifications.
# The IDs are mapped as follows:
# Target Cohort ID: 1794126 (from Analysis Specifications -> cohortDefinitions -> targetCohort -> id)
# Comparator Cohort ID: 1794132 (from Analysis Specifications -> cohortDefinitions -> comparatorCohort -> id)
# Outcome Cohort ID: 1794131 (from Analysis Specifications -> cohortDefinitions -> outcomeCohort[0] -> id)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to a simplified internal ID scheme for the study:
# Target Cohort (1794126) -> ID 1
# Comparator Cohort (1794132) -> ID 2
# Outcome Cohort (1794131) -> ID 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Fetches a concept set for negative controls based on its ID from Analysis Specifications.
# Negative Control Concept Set ID: 1888110 (from Analysis Specifications -> negativeControlConceptSet -> id)
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,
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
  mutate(cohortId = row_number() + 100) %>% # Assign cohort IDs starting from 101 for negative controls
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs between study cohorts and negative controls.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: 
# Selects the re-numbered outcome cohort (ID 3) for the study.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window of 365 days

# Target and Comparator for the CohortMethod analysis 
# Uses the re-numbered target (ID 1) and comparator (ID 2) cohorts.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = cohortDefinitionSet[cohortDefinitionSet$cohortId == 1,]$cohortName,
  comparatorCohortId = 2,
  comparatorCohortName = cohortDefinitionSet[cohortDefinitionSet$cohortId == 2,]$cohortName
)

# Excluded Covariate Concepts:
# Based on Analysis Specifications -> covariateSelection -> conceptsToExclude.
# In this specification, it's an empty list, so this will be an empty data frame.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# Included Covariate Concepts:
# Based on Analysis Specifications -> covariateSelection -> conceptsToInclude.
# In this specification, it's an empty list, so this will be an empty data frame.
includedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)


# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Defines shared resource specifications for study cohorts.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Defines shared resource specifications for negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default occurrence type
  detectOnDescendants = TRUE # Default to detect on descendants
)
# Creates module specifications for CohortGenerator, enabling cohort generation stats.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Creates module specifications for CohortDiagnostics.
# Cohort IDs to diagnose are all the ones defined in `cohortDefinitionSet`.
# Runs various diagnostic checks as enabled by the parameters.
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE,
  runIncludedSourceConcepts = TRUE,
  runOrphanConcepts = TRUE,
  runTimeSeries = FALSE, # Default
  runVisitContext = TRUE,
  runBreakdownIndexEvents = TRUE,
  runIncidenceRate = TRUE,
  runCohortRelationship = TRUE,
  runTemporalCohortCharacterization = TRUE,
  minCharacterizationMean = 0.01 # Default
)

# CohortMethodModule -----------------------------------------------------------

# Study periods: Defined in Analysis Specifications -> getDbCohortMethodDataArgs -> studyPeriods.
# Here, a single study period from "20200101" to "20200515".
studyPeriods <- tibble(
  studyStartDate = c("20200101"), #YYYYMMDD
  studyEndDate   = c("20200515") #YYYYMMDD
)

# Time-at-risks (TARs): Defined in Analysis Specifications -> createStudyPopArgs -> timeAtRisks.
# Here, a single TAR from day 1 (cohort start) to day 99999 (cohort start).
timeAtRisks <- tibble(
  label = c("TAR1_1_99999"), # A descriptive label for this TAR.
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd  = c(99999),
  endAnchor = c("cohort start") # "cohort start" | "cohort end"
) 

# Propensity Score settings - match on PS
# Settings for PS matching from Analysis Specifications -> propensityScoreAdjustment -> psSettings.
# Filters for entries where 'matchOnPsArgs' is not null.
matchOnPsArgsList <- tibble(
  label = c("MatchOnPs1"), # Label for the first matching strategy
  maxRatio  = c(4), # From Analysis Specifications -> psSettings[1] -> matchOnPsArgs -> maxRatio
  caliper = c(0.2), # From Analysis Specifications -> psSettings[1] -> matchOnPsArgs -> caliper
  caliperScale  = c("standardized logit") # From Analysis Specifications -> psSettings[1] -> matchOnPsArgs -> caliperScale
) 

# Propensity Score settings - stratify by PS
# Settings for PS stratification from Analysis Specifications -> propensityScoreAdjustment -> psSettings.
# Filters for entries where 'stratifyByPsArgs' is not null.
stratifyByPsArgsList <- tibble(
  label = c("StratifyByPs1"), # Label for the first stratification strategy
  numberOfStrata  = c(5), # From Analysis Specifications -> psSettings[2] -> stratifyByPsArgs -> numberOfStrata
  baseSelection = c("all") # From Analysis Specifications -> psSettings[2] -> stratifyByPsArgs -> baseSelection
) 

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
      params = list(
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection  = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}


# Iterate through all analysis setting combinations
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment arguments based on the current PS configuration.
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default
          stratificationColumns = c() # Default
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Define covariate settings.
      # Since Analysis Specifications -> covariateSelection -> conceptsToInclude/Exclude are empty,
      # we use default covariate settings.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default
      )

      # List of outcomes for the CohortMethod analysis.
      # Includes the primary outcome from oList and all negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # For true effect size when not a negative control
            priorOutcomeLookback = 99999 # Default
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # For true effect size of negative controls
          )
        })
      )

      # Target-Comparator-Outcomes (TCO) list.
      # For each T/C pair, associates the defined outcomes and any specific covariate exclusions.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        # Collect covariate concept IDs to exclude from Analysis Specifications if any.
        # Currently, Analysis Specifications -> covariateSelection -> conceptsToExclude is empty.
        excludedConceptIdsFromSpec <- as.integer(excludedCovariateConcepts$conceptId)
        
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # excludedCovariateConceptIds: Combines any specific exclusions from Analysis Specifications.
          excludedCovariateConceptIds = excludedConceptIdsFromSpec
        )
      }

      # Arguments for fetching cohort method data from the database.
      # Parameters mapped from Analysis Specifications -> getDbCohortMethodDataArgs.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications -> restrictToCommonPeriod
        studyStartDate = studyStartDate, # From current study period loop
        studyEndDate = studyEndDate,   # From current study period loop
        maxCohortSize = 0, # From Analysis Specifications -> maxCohortSize (0 means no restriction)
        covariateSettings = covariateSettings, # Defined above
        firstExposureOnly = TRUE, # From Analysis Specifications -> firstExposureOnly
        washoutPeriod = 180, # From Analysis Specifications -> washoutPeriod
        removeDuplicateSubjects = "keep first" # From Analysis Specifications -> removeDuplicateSubjects
      )

      # Arguments for creating propensity scores.
      # Parameters mapped from Analysis Specifications -> propensityScoreAdjustment -> createPsArgs.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications -> maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From Analysis Specifications -> errorOnHighCorrelation
        stopOnError = FALSE, # Default: Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att", # Default
        # Prior settings for regularization, mapped from Analysis Specifications -> prior.
        prior = Cyclops::createPrior( 
          priorType = "laplace", # From Analysis Specifications -> prior -> priorType
          exclude = c(0), # Default
          useCrossValidation = TRUE # From Analysis Specifications -> prior -> useCrossValidation
        ),
        # Control settings for the Cyclops solver, mapped from Analysis Specifications -> control.
        control = Cyclops::createControl( 
          noiseLevel = "silent", # From Analysis Specifications -> control -> noiseLevel
          cvType = "auto", # From Analysis Specifications -> control -> cvType
          seed = 1, # Default
          resetCoefficients = TRUE, # From Analysis Specifications -> control -> resetCoefficients
          tolerance = 2e-07, # From Analysis Specifications -> control -> tolerance
          cvRepetitions = 10, # From Analysis Specifications -> control -> cvRepetitions
          startingVariance = 0.01 # From Analysis Specifications -> control -> startingVariance
        )
      )

      # Arguments for computing shared covariate balance (e.g., balance before PS adjustment).
      # Using default settings as no specific parameters in Analysis Specifications.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default
        covariateFilter = NULL # Default
      )
      # Arguments for computing covariate balance (e.g., balance after PS adjustment).
      # Using default settings as no specific parameters in Analysis Specifications.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default
      )

      # Arguments for fitting the outcome model.
      # Parameters mapped from Analysis Specifications -> fitOutcomeModelArgs.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From Analysis Specifications -> modelType
        stratified = TRUE, # From Analysis Specifications -> stratified
        useCovariates = FALSE, # From Analysis Specifications -> useCovariates
        inversePtWeighting = FALSE, # From Analysis Specifications -> inversePtWeighting
        # Prior settings for regularization, mapped from Analysis Specifications -> prior.
        prior = Cyclops::createPrior( 
          priorType = "laplace", # From Analysis Specifications -> prior -> priorType
          useCrossValidation = TRUE # From Analysis Specifications -> prior -> useCrossValidation
        ),
        # Control settings for the Cyclops solver, mapped from Analysis Specifications -> control.
        control = Cyclops::createControl( 
          cvType = "auto", # From Analysis Specifications -> control -> cvType
          seed = 1, # Default
          resetCoefficients = TRUE, # From Analysis Specifications -> control -> resetCoefficients
          startingVariance = 0.01, # From Analysis Specifications -> control -> startingVariance
          tolerance = 2e-07, # From Analysis Specifications -> control -> tolerance
          cvRepetitions = 10, # From Analysis Specifications -> control -> cvRepetitions
          noiseLevel = "quiet" # From Analysis Specifications -> control -> noiseLevel
        )
      )
      # Arguments for creating the study population.
      # Parameters mapped from Analysis Specifications -> createStudyPopArgs.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications -> restrictToCommonPeriod
        firstExposureOnly = FALSE, # From Analysis Specifications -> firstExposureOnly
        washoutPeriod = 0, # From Analysis Specifications -> washoutPeriod
        removeDuplicateSubjects = "keep all", # From Analysis Specifications -> removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # From Analysis Specifications -> censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = FALSE, # From Analysis Specifications -> removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # From Analysis Specifications -> priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From current time-at-risk loop
        startAnchor = timeAtRisks$startAnchor[t], # From current time-at-risk loop
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From current time-at-risk loop
        endAnchor = timeAtRisks$endAnchor[t], # From current time-at-risk loop
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From current time-at-risk loop
        maxDaysAtRisk = 99999 # Default
      )


      # Append the settings to Analysis List
      # Each entry in cmAnalysisList represents a unique CohortMethod analysis.
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

# Create CohortMethod module specifications.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList, # List of CM analyses defined above
  targetComparatorOutcomesList = targetComparatorOutcomesList, # TCO list defined above
  analysesToExclude = NULL, # No specific analyses to exclude
  refitPsForEveryOutcome = FALSE, # Default
  refitPsForEveryStudyPopulation = FALSE, # Default
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default diagnostic thresholds
)

# Create the analysis specifications ------------------------------------------
# Initialize empty analysis specifications.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Add shared resources for cohort definitions.
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  # Add shared resources for negative control outcome cohorts.
  Strategus::addSharedResources(negativeControlsShared) |>
  # Add module specifications for CohortGenerator.
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  # Add module specifications for CohortDiagnostics.
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  # Add module specifications for CohortMethod.
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the complete analysis specifications to a JSON file.
# The filename uses the study name from Analysis Specifications -> name: "covid19ppiandh2ra".
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "covid19ppiandh2ra", "covid19ppiandh2raAnalysisSpecification.json")
)