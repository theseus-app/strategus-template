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

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
# Base URL for the WebAPI instance to retrieve cohort definitions and concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions from WebAPI based on IDs provided in analysis specifications.
# The IDs are mapped to internal IDs (1, 2, 3) for target, comparator, and outcome.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1 (from analysis specifications: cohortDefinitions.targetCohort.id)
    1794132, # Comparator: comparator1 (from analysis specifications: cohortDefinitions.comparatorCohort.id)
    1794131  # Outcome: outcome1 (from analysis specifications: cohortDefinitions.outcomeCohort[0].id)
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal use in Strategus modules.
# Target cohort gets ID 1.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# Comparator cohort gets ID 2.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# Outcome cohort gets ID 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve negative control concept set from WebAPI and convert to a cohort set.
# The concept set ID is from analysis specifications: negativeControlConceptSet.id.
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
  mutate(cohortId = row_number() + 100) %>% # Assign unique IDs starting from 101 for negative controls
  select(cohortId, cohortName, outcomeConceptId)


if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Filter for the outcome cohort (internal ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Outcome cohort ID is 3
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not specified in analysis specifications

# Target and Comparator for the CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1, # Internal ID for target cohort
  targetCohortName = "target1", # From analysis specifications: cohortDefinitions.targetCohort.name
  comparatorCohortId = 2, # Internal ID for comparator cohort
  comparatorCohortName = "comparator1" # From analysis specifications: cohortDefinitions.comparatorCohort.name
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study. Based on analysis specifications: covariateSelection.conceptsToExclude.
# Since it's null in the spec, this will be an empty data frame.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0), # No concepts to exclude specified in analysis specifications
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# Based on analysis specifications: covariateSelection.conceptsToInclude.
# Since it's null in the spec, this will be an empty data frame.
includedCovariateConcepts <- data.frame(
  conceptId = integer(0), # No concepts to include specified in analysis specifications
  conceptName = character(0)
)

# CohortGeneratorModule --------------------------------------------------------
# Settings for the CohortGeneratorModule.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default from template
  detectOnDescendants = TRUE # Default from template
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Default from template
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# Settings for the CohortDiagnosticsModule.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
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

# Study periods for the analysis.
# From analysis specifications: getDbCohortMethodDataArgs.studyPeriods.
studyPeriods <- tibble(
  studyStartDate = c("20050101"), # From analysis specifications: getDbCohortMethodDataArgs.studyPeriods[0].studyStartDate
  studyEndDate   = c("20171231")  # From analysis specifications: getDbCohortMethodDataArgs.studyPeriods[0].studyEndDate
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# From analysis specifications: createStudyPopArgs.timeAtRisks.
timeAtRisks <- tibble(
  label = c("TAR1"), # Custom label for this time-at-risk setting
  riskWindowStart  = c(1), # From analysis specifications: createStudyPopArgs.timeAtRisks[0].riskWindowStart
  startAnchor = c("cohort start"), # From analysis specifications: createStudyPopArgs.timeAtRisks[0].startAnchor
  riskWindowEnd  = c(99999), # From analysis specifications: createStudyPopArgs.timeAtRisks[0].riskWindowEnd
  endAnchor = c("cohort start"), # From analysis specifications: createStudyPopArgs.timeAtRisks[0].endAnchor
  minDaysAtRisk = c(1) # From analysis specifications: createStudyPopArgs.timeAtRisks[0].minDaysAtRisk
)

# Propensity Score settings - match on PS.
# From analysis specifications: propensityScoreAdjustment.psSettings where matchOnPsArgs is not null.
matchOnPsArgsList <- tibble(
  label = c("MatchOnPs_3_0.2", "MatchOnPs_1_0.2", "MatchOnPs_2_0.2", "MatchOnPs_4_0.2"), # Custom labels for each PS setting
  maxRatio  = c(3, 1, 2, 4), # From analysis specifications: propensityScoreAdjustment.psSettings[i].matchOnPsArgs.maxRatio
  caliper = c(0.2, 0.2, 0.2, 0.2), # From analysis specifications: propensityScoreAdjustment.psSettings[i].matchOnPsArgs.caliper
  caliperScale  = c("standardized logit", "standardized logit", "standardized logit", "standardized logit") # From analysis specifications: propensityScoreAdjustment.psSettings[i].matchOnPsArgs.caliperScale
)

# Propensity Score settings - stratify by PS.
# From analysis specifications: propensityScoreAdjustment.psSettings where stratifyByPsArgs is not null.
# In this case, it's empty based on the analysis specifications.
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
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

# Determine included and excluded covariate concept IDs from analysis specifications
includedCovariateConceptIds <- if (nrow(includedCovariateConcepts) > 0) includedCovariateConcepts$conceptId else c()
excludedCovariateConceptIds <- if (nrow(excludedCovariateConcepts) > 0) excludedCovariateConcepts$conceptId else c()

# Create covariate settings for FeatureExtraction
# Incorporates both default settings and specific inclusions/exclusions from analysis specifications.
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE, # Default from template
  excludedCovariateConceptIds = excludedCovariateConceptIds, # From analysis specifications: covariateSelection.conceptsToExclude
  includedCovariateConceptIds = includedCovariateConceptIds # From analysis specifications: covariateSelection.conceptsToInclude
)

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      matchOnPsArgs <- NULL
      stratifyByPsArgs <- NULL

      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default from template
          stratificationColumns = c() # Default from template
        )
      } else if (psCfg$method == "stratify") {
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default from template
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Define outcomes for the CohortMethod analysis.
      # Includes the primary outcome and all negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Default from template
            priorOutcomeLookback = 99999 # Default from template
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
          # Exclude specific covariate concepts.
          # Based on analysis specifications: covariateSelection.conceptsToExclude.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Arguments for retrieving cohort method data from the database.
      # Settings from analysis specifications: getDbCohortMethodDataArgs.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # From analysis specifications: getDbCohortMethodDataArgs.restrictToCommonPeriod
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From analysis specifications: getDbCohortMethodDataArgs.maxCohortSize
        firstExposureOnly = FALSE, # From analysis specifications: getDbCohortMethodDataArgs.firstExposureOnly
        washoutPeriod = 0, # From analysis specifications: getDbCohortMethodDataArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # From analysis specifications: getDbCohortMethodDataArgs.removeDuplicateSubjects
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # Settings from analysis specifications: propensityScoreAdjustment.createPsArgs.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From analysis specifications: propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From analysis specifications: propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail (Default from template)
        estimator = "att", # Default from template
        prior = Cyclops::createPrior( # Prior settings for PS model.
          priorType = "laplace", # From analysis specifications: propensityScoreAdjustment.createPsArgs.prior.priorType
          exclude = c(0), # Default from template
          useCrossValidation = TRUE # From analysis specifications: propensityScoreAdjustment.createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings for PS model.
          noiseLevel = "silent", # From analysis specifications: propensityScoreAdjustment.createPsArgs.control.noiseLevel
          cvType = "auto", # From analysis specifications: propensityScoreAdjustment.createPsArgs.control.cvType
          seed = 1, # Default from template
          resetCoefficients = TRUE, # From analysis specifications: propensityScoreAdjustment.createPsArgs.control.resetCoefficients
          tolerance = 2e-07, # From analysis specifications: propensityScoreAdjustment.createPsArgs.control.tolerance
          cvRepetitions = 10, # From analysis specifications: propensityScoreAdjustment.createPsArgs.control.cvRepetitions
          startingVariance = 0.01 # From analysis specifications: propensityScoreAdjustment.createPsArgs.control.startingVariance
        )
      )

      # Arguments for computing shared covariate balance (default from template).
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      # Arguments for computing covariate balance (default from template).
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting the outcome model.
      # Settings from analysis specifications: fitOutcomeModelArgs.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From analysis specifications: fitOutcomeModelArgs.modelType
        stratified = TRUE, # From analysis specifications: fitOutcomeModelArgs.stratified
        useCovariates = TRUE, # From analysis specifications: fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE, # From analysis specifications: fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior( # Prior settings for outcome model.
          priorType = "laplace", # From analysis specifications: fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE # From analysis specifications: fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings for outcome model.
          cvType = "auto", # From analysis specifications: fitOutcomeModelArgs.control.cvType
          seed = 1, # Default from template
          resetCoefficients = TRUE, # From analysis specifications: fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01, # From analysis specifications: fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07, # From analysis specifications: fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10, # From analysis specifications: fitOutcomeModelArgs.control.cvRepetitions
          noiseLevel = "quiet" # From analysis specifications: fitOutcomeModelArgs.control.noiseLevel
        )
      )

      # Arguments for creating the study population.
      # Settings from analysis specifications: createStudyPopArgs.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From analysis specifications: createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE, # From analysis specifications: createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0, # From analysis specifications: createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # From analysis specifications: createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # From analysis specifications: createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # From analysis specifications: createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # From analysis specifications: createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From analysis specifications: createStudyPopArgs.timeAtRisks[0].minDaysAtRisk
        maxDaysAtRisk = 99999 # Default from template, not in analysis specifications
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

# CohortMethodModule specifications.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Default from template
  refitPsForEveryOutcome = FALSE, # Default from template
  refitPsForEveryStudyPopulation = FALSE, # Default from template
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default from template
)

# Create the analysis specifications ------------------------------------------
# Combine all module specifications and shared resources into a single analysis specification object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path uses the study name "cystectomytrimodality" from analysis specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "cystectomytrimodality", "cystectomytrimodalityAnalysisSpecification.json")
)