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
library(ParallelLogger)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
# Base URL for the ATLAS/WebAPI instance. This is not specified in the
# analysis specifications, so a default is used.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Extract cohort IDs and names from the analysis specifications.
# Target: 1794126 (target1)
# Comparator: 1794132 (comparator1)
# Outcome: 1794131 (outcome1)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to standard IDs (1 for target, 2 for comparator, 3 for outcome)
# This ensures consistent internal referencing within the Strategus modules.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Extract negative control concept set ID from analysis specifications.
# ID: 1888110 (negative)
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # From analysis specifications: negativeControlConceptSet.id
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
  # Assign cohort IDs starting from 101 to avoid collision with target/comparator/outcome
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs between study cohorts and negative controls.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts for each analysis ---------------
# Outcomes: Filter for the outcome cohort (re-numbered ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window

# Target and Comparator for the CohortMethod analysis 
# Use the re-numbered target (ID 1) and comparator (ID 2) cohorts.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = cohortDefinitionSet[cohortDefinitionSet$cohortId == 1,]$cohortName,
  comparatorCohortId = 2,
  comparatorCohortName = cohortDefinitionSet[cohortDefinitionSet$cohortId == 2,]$cohortName
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study.
# From analysis specifications: covariateSelection.conceptsToExclude
# This is empty in the specifications, so create an empty data frame.
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(0),
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# From analysis specifications: covariateSelection.conceptsToInclude
# This is empty in the specifications, so keep it commented out.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default setting
  detectOnDescendants = TRUE # Default setting
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Default setting
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE, # Default setting
  runIncludedSourceConcepts = TRUE, # Default setting
  runOrphanConcepts = TRUE, # Default setting
  runTimeSeries = FALSE, # Default setting
  runVisitContext = TRUE, # Default setting
  runBreakdownIndexEvents = TRUE, # Default setting
  runIncidenceRate = TRUE, # Default setting
  runCohortRelationship = TRUE, # Default setting
  runTemporalCohortCharacterization = TRUE, # Default setting
  minCharacterizationMean = 0.01 # Default setting
)

# CohortMethodModule -----------------------------------------------------------

# Study periods for restricting data.
# From analysis specifications: getDbCohortMethodDataArgs.studyPeriods
# If studyStartDate and studyEndDate are empty strings, the loop will run once with empty strings.
studyPeriods <- tibble(
  studyStartDate = c(""), # From analysis specifications: getDbCohortMethodDataArgs.studyPeriods[0].studyStartDate
  studyEndDate   = c("")  # From analysis specifications: getDbCohortMethodDataArgs.studyPeriods[0].studyEndDate
)

# Time-at-risks (TARs) for the outcomes of interest in your study
# From analysis specifications: createStudyPopArgs.timeAtRisks
timeAtRisks <- tibble(
  label = c("TAR_1_180"), # Custom label for this TAR
  riskWindowStart  = c(1), # From analysis specifications: createStudyPopArgs.timeAtRisks[0].riskWindowStart
  startAnchor = c("cohort start"), # From analysis specifications: createStudyPopArgs.timeAtRisks[0].startAnchor
  riskWindowEnd  = c(180), # From analysis specifications: createStudyPopArgs.timeAtRisks[0].riskWindowEnd
  endAnchor = c("cohort start") # From analysis specifications: createStudyPopArgs.timeAtRisks[0].endAnchor
) 

# Propensity Score settings - match on PS
# From analysis specifications: propensityScoreAdjustment.psSettings
# Two matching settings are specified.
matchOnPsArgsList <- tibble(
  label = c("Match_MaxRatio1_Caliper0.2", "Match_MaxRatio3_Caliper0.2"), # Custom labels for PS settings
  maxRatio  = c(1, 3), # From analysis specifications: propensityScoreAdjustment.psSettings[0/1].matchOnPsArgs.maxRatio
  caliper = c(0.2, 0.2), # From analysis specifications: propensityScoreAdjustment.psSettings[0/1].matchOnPsArgs.caliper
  caliperScale  = c("standardized logit", "standardized logit") # From analysis specifications: propensityScoreAdjustment.psSettings[0/1].matchOnPsArgs.caliperScale
) 

# Propensity Score settings - stratify by PS
# No stratification settings are specified in the analysis specifications.
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = numeric(0),
  baseSelection = character(0)
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
      
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default setting
          stratificationColumns = c() # Default setting
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default setting
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Default covariate settings for feature extraction.
      # The analysis specifications do not provide specific covariate settings
      # beyond inclusion/exclusion lists (which are empty).
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default setting
      )

      # Combine study outcomes and negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not specified in analysis specifications
            priorOutcomeLookback = 99999 # From analysis specifications: createStudyPopArgs.priorOutcomeLookBack
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
      
      # Define target-comparator-outcome combinations.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Excluded covariate concept IDs.
          # The analysis specifications' covariateSelection.conceptsToExclude is empty.
          # The template's cmTcList$targetConceptId and cmTcList$comparatorConceptId are not
          # concept IDs for exclusion, but cohort IDs. They are removed here.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Settings for fetching data from the database.
      # From analysis specifications: getDbCohortMethodDataArgs
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = as.logical(studyPeriods$studyStartDate[s] != "" || studyPeriods$studyEndDate[s] != ""), # If study periods are specified, restrict to common period.
        studyStartDate = studyStartDate, # From loop (empty string if not specified)
        studyEndDate = studyEndDate, # From loop (empty string if not specified)
        maxCohortSize = 0, # From analysis specifications: getDbCohortMethodDataArgs.maxCohortSize
        covariateSettings = covariateSettings,
        firstExposureOnly = FALSE, # From analysis specifications: getDbCohortMethodDataArgs.firstExposureOnly
        washoutPeriod = 0, # From analysis specifications: getDbCohortMethodDataArgs.washoutPeriod
        removeDuplicateSubjects = "keep all" # From analysis specifications: getDbCohortMethodDataArgs.removeDuplicateSubjects
      )

      # Settings for creating propensity scores.
      # From analysis specifications: propensityScoreAdjustment.createPsArgs
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From analysis specifications: propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From analysis specifications: propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att", # Default setting
        prior = Cyclops::createPrior( # From analysis specifications: propensityScoreAdjustment.createPsArgs.prior
          priorType = "laplace", 
          exclude = c(0), # Default setting
          useCrossValidation = TRUE # From analysis specifications: propensityScoreAdjustment.createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # From analysis specifications: propensityScoreAdjustment.createPsArgs.control
          noiseLevel = "silent", # From analysis specifications: propensityScoreAdjustment.createPsArgs.control.noiseLevel
          cvType = "auto", # From analysis specifications: propensityScoreAdjustment.createPsArgs.control.cvType
          seed = 1, # Default setting
          resetCoefficients = TRUE, # From analysis specifications: propensityScoreAdjustment.createPsArgs.control.resetCoefficients
          tolerance = 2e-07, # From analysis specifications: propensityScoreAdjustment.createPsArgs.control.tolerance
          cvRepetitions = 10, # From analysis specifications: propensityScoreAdjustment.createPsArgs.control.cvRepetitions
          startingVariance = 0.01 # From analysis specifications: propensityScoreAdjustment.createPsArgs.control.startingVariance
        )
      )

      # Settings for computing shared covariate balance.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default setting
        covariateFilter = NULL # Default setting
      )
      
      # Settings for computing covariate balance.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default setting
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default setting
      )

      # Settings for fitting the outcome model.
      # From analysis specifications: fitOutcomeModelArgs
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "logistic", # From analysis specifications: fitOutcomeModelArgs.modelType
        stratified = TRUE, # From analysis specifications: fitOutcomeModelArgs.stratified
        useCovariates = FALSE, # From analysis specifications: fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE, # From analysis specifications: fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior( # From analysis specifications: fitOutcomeModelArgs.prior
          priorType = "laplace", 
          useCrossValidation = TRUE # From analysis specifications: fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # From analysis specifications: fitOutcomeModelArgs.control
          cvType = "auto", # From analysis specifications: fitOutcomeModelArgs.control.cvType
          seed = 1, # Default setting
          resetCoefficients = TRUE, # From analysis specifications: fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01, # From analysis specifications: fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07, # From analysis specifications: fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10, # From analysis specifications: fitOutcomeModelArgs.control.cvRepetitions
          noiseLevel = "quiet" # From analysis specifications: fitOutcomeModelArgs.control.noiseLevel
        )
      )
      
      # Settings for creating the study population.
      # From analysis specifications: createStudyPopArgs
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From analysis specifications: createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE, # From analysis specifications: createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0, # From analysis specifications: createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # From analysis specifications: createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # From analysis specifications: createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # From analysis specifications: createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # From analysis specifications: createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From timeAtRisks loop
        startAnchor = timeAtRisks$startAnchor[t], # From timeAtRisks loop
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From timeAtRisks loop
        endAnchor = timeAtRisks$endAnchor[t], # From timeAtRisks loop
        minDaysAtRisk = 1, # From analysis specifications: createStudyPopArgs.timeAtRisks[0].minDaysAtRisk
        maxDaysAtRisk = 99999 # Default setting, not in analysis specifications
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
  analysesToExclude = NULL, # Default setting
  refitPsForEveryOutcome = FALSE, # Default setting
  refitPsForEveryStudyPopulation = FALSE, # Default setting
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default setting
)

# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The study name is "alzheimerdonepezil" from the analysis specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "alzheimerdonepezil", "alzheimerdonepezilAnalysisSpecification.json")
)