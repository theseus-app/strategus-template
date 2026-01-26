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
library(ROhdsiWebApi) # Explicitly load ROhdsiWebApi for clarity
library(CohortMethod) # Explicitly load CohortMethod for clarity
library(FeatureExtraction) # Explicitly load FeatureExtraction for clarity
library(Cyclops) # Explicitly load Cyclops for clarity

# Shared Resources -------------------------------------------------------------
# Base URL for the WebAPI to retrieve cohort definitions and concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Retrieving cohort definitions for Target, Comparator, and Outcome cohorts
# as specified in the Analysis Specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1 (from Analysis Specifications -> cohortDefinitions -> targetCohort -> id)
    1794132, # Comparator: comparator1 (from Analysis Specifications -> cohortDefinitions -> comparatorCohort -> id)
    1794131  # Outcome: outcome1 (from Analysis Specifications -> cohortDefinitions -> outcomeCohort -> id)
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal use within the study package.
# This ensures consistent, small integer IDs for Target (1), Comparator (2), and Outcome (3).
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Target cohort re-numbered to 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Comparator cohort re-numbered to 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Outcome cohort re-numbered to 3

# Negative control outcomes
# Retrieving the concept set for negative controls as specified.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Negative Control Concept Set ID (from Analysis Specifications -> negativeControlConceptSet -> id)
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
  mutate(cohortId = row_number() + 100) %>% # Assign unique IDs for negative controls, starting from 101
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs to prevent conflicts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts used in each analysis ---------------
# Outcomes:
# Filtering for the outcome cohort (re-numbered to 3) and preparing its details.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Outcome cohort ID (re-numbered to 3)
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not specified in analysis specs

# Target and Comparator for the CohortMethod analysis
# Populating target and comparator details based on re-numbered IDs and names.
cmTcList <- data.frame(
  targetCohortId = 1, # Target cohort ID (re-numbered)
  targetCohortName = "target1", # Target cohort name (from Analysis Specifications -> cohortDefinitions -> targetCohort -> name)
  comparatorCohortId = 2, # Comparator cohort ID (re-numbered)
  comparatorCohortName = "comparator1" # Comparator cohort name (from Analysis Specifications -> cohortDefinitions -> comparatorCohort -> name)
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study.
# Based on Analysis Specifications -> covariateSelection -> conceptsToExclude,
# which is empty (id: null), so this data frame will be empty.
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(0),
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# Based on Analysis Specifications -> covariateSelection -> conceptsToInclude,
# which is empty (id: null), so this data frame will be empty.
includedCovariateConcepts <- data.frame(
  conceptId = numeric(0),
  conceptName = character(0)
)

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default, not specified in analysis specs
  detectOnDescendants = TRUE # Default, not specified in analysis specs
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Default, not specified in analysis specs
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # All cohort IDs defined in cohortDefinitionSet
  runInclusionStatistics = TRUE, # Default, not specified in analysis specs
  runIncludedSourceConcepts = TRUE, # Default, not specified in analysis specs
  runOrphanConcepts = TRUE, # Default, not specified in analysis specs
  runTimeSeries = FALSE, # Default, not specified in analysis specs
  runVisitContext = TRUE, # Default, not specified in analysis specs
  runBreakdownIndexEvents = TRUE, # Default, not specified in analysis specs
  runIncidenceRate = TRUE, # Default, not specified in analysis specs
  runCohortRelationship = TRUE, # Default, not specified in analysis specs
  runTemporalCohortCharacterization = TRUE, # Default, not specified in analysis specs
  minCharacterizationMean = 0.01 # Default, not specified in analysis specs
)

# CohortMethodModule -----------------------------------------------------------

# Study periods for analysis (from Analysis Specifications -> getDbCohortMethodDataArgs -> studyPeriods)
studyPeriods <- tibble(
  studyStartDate = c("20100101", "20120101"), # YYYYMMDD format
  studyEndDate   = c("20191231", "20191231")  # YYYYMMDD format
)

# Time-at-risks (TARs) for the outcomes of interest in your study
# (from Analysis Specifications -> createStudyPopArgs -> timeAtRisks)
timeAtRisks <- tibble(
  label = c(
    "TAR 1-0 (cohort start-end, minDaysAtRisk 1)", # Descriptive label for the first TAR
    "TAR 1-99999 (cohort start-start, minDaysAtRisk 1)" # Descriptive label for the second TAR
  ),
  riskWindowStart  = c(1, 1), # From Analysis Specifications -> createStudyPopArgs -> timeAtRisks -> riskWindowStart
  startAnchor = c("cohort start", "cohort start"), # From Analysis Specifications -> createStudyPopArgs -> timeAtRisks -> startAnchor
  riskWindowEnd  = c(0, 99999), # From Analysis Specifications -> createStudyPopArgs -> timeAtRisks -> riskWindowEnd
  endAnchor = c("cohort end", "cohort start"), # From Analysis Specifications -> createStudyPopArgs -> timeAtRisks -> endAnchor
  minDaysAtRisk = c(1, 1) # From Analysis Specifications -> createStudyPopArgs -> timeAtRisks -> minDaysAtRisk
)

# Propensity Score settings - match on PS
# (from Analysis Specifications -> propensityScoreAdjustment -> psSettings where matchOnPsArgs is not null)
matchOnPsArgsList <- tibble(
  label = c("Match on PS (maxRatio 0, caliper 0.2 standardized logit)"), # Descriptive label
  maxRatio  = c(0), # From Analysis Specifications -> propensityScoreAdjustment -> psSettings[2] -> matchOnPsArgs -> maxRatio
  caliper = c(0.2), # From Analysis Specifications -> propensityScoreAdjustment -> psSettings[2] -> matchOnPsArgs -> caliper
  caliperScale  = c("standardized logit") # From Analysis Specifications -> propensityScoreAdjustment -> psSettings[2] -> matchOnPsArgs -> caliperScale
)

# Propensity Score settings - stratify by PS
# (from Analysis Specifications -> propensityScoreAdjustment -> psSettings where stratifyByPsArgs is not null)
stratifyByPsArgsList <- tibble(
  label = c("Stratify by PS (5 strata, base all)"), # Descriptive label
  numberOfStrata  = c(5), # From Analysis Specifications -> propensityScoreAdjustment -> psSettings[1] -> stratifyByPsArgs -> numberOfStrata
  baseSelection = c("all") # From Analysis Specifications -> propensityScoreAdjustment -> psSettings[1] -> stratifyByPsArgs -> baseSelection
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
      
      # Propensity score adjustment method (match or stratify)
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio, # From Analysis Specifications -> propensityScoreAdjustment -> psSettings -> matchOnPsArgs -> maxRatio
          caliper = psCfg$params$caliper, # From Analysis Specifications -> propensityScoreAdjustment -> psSettings -> matchOnPsArgs -> caliper
          caliperScale = psCfg$params$caliperScale, # From Analysis Specifications -> propensityScoreAdjustment -> psSettings -> matchOnPsArgs -> caliperScale
          allowReverseMatch = FALSE, # Default, not specified in analysis specs
          stratificationColumns = c() # Default, not specified in analysis specs
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata, # From Analysis Specifications -> propensityScoreAdjustment -> psSettings -> stratifyByPsArgs -> numberOfStrata
          stratificationColumns = c(), # Default, not specified in analysis specs
          baseSelection = psCfg$params$baseSelection # From Analysis Specifications -> propensityScoreAdjustment -> psSettings -> stratifyByPsArgs -> baseSelection
        )
      }

      # Covariate settings
      # Since covariateSelection.conceptsToInclude and conceptsToExclude are empty in the specs,
      # we use default covariate settings.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default, not specified in analysis specs
      )

      # Outcome list including both outcomes of interest and negative controls
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not applicable for observed outcomes
            priorOutcomeLookback = 99999 # From Analysis Specifications -> createStudyPopArgs -> priorOutcomeLookBack
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # For negative controls, true effect size is assumed to be 1 (null)
          )
        })
      )
      
      # Target-Comparator-Outcome combinations
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i], # Target cohort ID (re-numbered)
          comparatorId = cmTcList$comparatorCohortId[i], # Comparator cohort ID (re-numbered)
          outcomes = outcomeList,
          # Exclude target and comparator cohort IDs from covariates to prevent confounding.
          # Also include any other specified excluded concepts (empty in this case, from Analysis Specifications).
          excludedCovariateConceptIds = c(
            cmTcList$targetCohortId[i], 
            cmTcList$comparatorCohortId[i],
            excludedCovariateConcepts$conceptId # Empty based on Analysis Specifications -> covariateSelection -> conceptsToExclude
          ),
          # Included covariate concepts (empty based on Analysis Specifications -> covariateSelection -> conceptsToInclude)
          includedCovariateConceptIds = includedCovariateConcepts$conceptId 
        )
      }

      # Arguments for fetching cohort method data from the database
      # (from Analysis Specifications -> getDbCohortMethodDataArgs)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications -> getDbCohortMethodDataArgs -> restrictToCommonPeriod
        studyStartDate = studyStartDate, # From current loop iteration
        studyEndDate = studyEndDate, # From current loop iteration
        maxCohortSize = 0, # From Analysis Specifications -> getDbCohortMethodDataArgs -> maxCohortSize
        covariateSettings = covariateSettings,
        firstExposureOnly = FALSE, # From Analysis Specifications -> getDbCohortMethodDataArgs -> firstExposureOnly
        washoutPeriod = 0, # From Analysis Specifications -> getDbCohortMethodDataArgs -> washoutPeriod
        removeDuplicateSubjects = "keep all" # From Analysis Specifications -> getDbCohortMethodDataArgs -> removeDuplicateSubjects
      )

      # Arguments for creating propensity scores
      # (from Analysis Specifications -> propensityScoreAdjustment -> createPsArgs)
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> errorOnHighCorrelation
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail (default in template)
        estimator = "att", # Default, not specified in analysis specs
        prior = Cyclops::createPrior( # Prior settings for PS model
          priorType = "laplace", # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> prior -> priorType
          exclude = c(0), # Default, not specified in analysis specs
          useCrossValidation = TRUE # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> prior -> useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings for PS model
          noiseLevel = "silent", # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> control -> noiseLevel
          cvType = "auto", # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> control -> cvType
          seed = 1, # Default, not specified in analysis specs
          resetCoefficients = TRUE, # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> control -> resetCoefficients
          tolerance = 2e-07, # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> control -> tolerance
          cvRepetitions = 10, # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> control -> cvRepetitions
          startingVariance = 0.01, # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> control -> startingVariance
          fold = 10 # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> control -> fold
        )
      )

      # Arguments for computing shared covariate balance (default in template, not specified in analysis specs)
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      # Arguments for computing covariate balance (default in template, not specified in analysis specs)
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting the outcome model
      # (from Analysis Specifications -> fitOutcomeModelArgs)
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From Analysis Specifications -> fitOutcomeModelArgs -> modelType
        stratified = TRUE, # From Analysis Specifications -> fitOutcomeModelArgs -> stratified
        useCovariates = FALSE, # From Analysis Specifications -> fitOutcomeModelArgs -> useCovariates
        inversePtWeighting = FALSE, # From Analysis Specifications -> fitOutcomeModelArgs -> inversePtWeighting
        prior = Cyclops::createPrior( # Prior settings for outcome model
          priorType = "laplace", # From Analysis Specifications -> fitOutcomeModelArgs -> prior -> priorType
          useCrossValidation = TRUE # From Analysis Specifications -> fitOutcomeModelArgs -> prior -> useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings for outcome model
          cvType = "auto", # From Analysis Specifications -> fitOutcomeModelArgs -> control -> cvType
          seed = 1, # Default, not specified in analysis specs
          resetCoefficients = TRUE, # From Analysis Specifications -> fitOutcomeModelArgs -> control -> resetCoefficients
          startingVariance = 0.01, # From Analysis Specifications -> fitOutcomeModelArgs -> control -> startingVariance
          tolerance = 2e-07, # From Analysis Specifications -> fitOutcomeModelArgs -> control -> tolerance
          cvRepetitions = 10, # From Analysis Specifications -> fitOutcomeModelArgs -> control -> cvRepetitions
          noiseLevel = "quiet", # From Analysis Specifications -> fitOutcomeModelArgs -> control -> noiseLevel
          fold = 10 # From Analysis Specifications -> fitOutcomeModelArgs -> control -> fold
        )
      )
      
      # Arguments for creating the study population
      # (from Analysis Specifications -> createStudyPopArgs and current timeAtRisks loop)
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications -> createStudyPopArgs -> restrictToCommonPeriod
        firstExposureOnly = FALSE, # From Analysis Specifications -> createStudyPopArgs -> firstExposureOnly
        washoutPeriod = 0, # From Analysis Specifications -> createStudyPopArgs -> washoutPeriod
        removeDuplicateSubjects = "keep all", # From Analysis Specifications -> createStudyPopArgs -> removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # From Analysis Specifications -> createStudyPopArgs -> censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # From Analysis Specifications -> createStudyPopArgs -> removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # From Analysis Specifications -> createStudyPopArgs -> priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From current timeAtRisks loop iteration
        startAnchor = timeAtRisks$startAnchor[t], # From current timeAtRisks loop iteration
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From current timeAtRisks loop iteration
        endAnchor = timeAtRisks$endAnchor[t], # From current timeAtRisks loop iteration
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From current timeAtRisks loop iteration
        maxDaysAtRisk = 99999 # Default, not specified in analysis specs
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

cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Default, not specified in analysis specs
  refitPsForEveryOutcome = FALSE, # Default, not specified in analysis specs
  refitPsForEveryStudyPopulation = FALSE, # Default, not specified in analysis specs
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default, not specified in analysis specs
)

# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path uses the study name "corazon" from the Analysis Specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "corazon", "corazonAnalysisSpecification.json")
)