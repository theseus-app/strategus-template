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
# The baseUrl is a placeholder for the ATLAS WebAPI instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# The cohort IDs are extracted from the 'cohortDefinitions' section of the analysis specifications.
# These IDs correspond to the target, comparator, and outcome cohorts defined in ATLAS.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal consistency within Strategus modules.
# This re-numbering maps the original ATLAS IDs to simpler, sequential IDs (1, 2, 3).
# Target cohort (ID 1794126) is re-numbered to 1.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# Comparator cohort (ID 1794132) is re-numbered to 2.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# Outcome cohort (ID 1794131) is re-numbered to 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# The concept set ID for negative controls is extracted from 'negativeControlConceptSet.id'.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # ID for 'negative' concept set from analysis specifications
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
  mutate(cohortId = row_number() + 100) %>% # Negative control cohort IDs start from 101 to avoid conflict with T/C/O (1, 2, 3).
  select(cohortId, cohortName, outcomeConceptId)


if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes:
# Filters for the outcome cohort, which was re-numbered to 3.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not specified in analysis specifications.

# Target and Comparator for the CohortMethod analysis
# Uses the re-numbered target (1) and comparator (2) cohort IDs and names.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# For the CohortMethod LSPS we'll need to exclude specific concepts as covariates.
# This is derived from 'covariateSelection.conceptsToExclude'.
# Since 'conceptsToExclude' is empty in the specifications, this data frame will be empty.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all.
# This is derived from 'covariateSelection.conceptsToInclude'.
# Since 'conceptsToInclude' is empty in the specifications, this data frame will be empty and commented out.
# includedCovariateConcepts <- data.frame(
#   conceptId = integer(0),
#   conceptName = character(0)
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default, not specified in analysis specifications.
  detectOnDescendants = TRUE # Default, not specified in analysis specifications.
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Default, not specified in analysis specifications.
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE, # Default, not specified in analysis specifications.
  runIncludedSourceConcepts = TRUE, # Default, not specified in analysis specifications.
  runOrphanConcepts = TRUE, # Default, not specified in analysis specifications.
  runTimeSeries = FALSE, # Default, not specified in analysis specifications.
  runVisitContext = TRUE, # Default, not specified in analysis specifications.
  runBreakdownIndexEvents = TRUE, # Default, not specified in analysis specifications.
  runIncidenceRate = TRUE, # Default, not specified in analysis specifications.
  runCohortRelationship = TRUE, # Default, not specified in analysis specifications.
  runTemporalCohortCharacterization = TRUE, # Default, not specified in analysis specifications.
  minCharacterizationMean = 0.01 # Default, not specified in analysis specifications.
)

# CohortMethodModule -----------------------------------------------------------

# Study periods are extracted from 'getDbCohortMethodDataArgs.studyPeriods'.
# If studyStartDate and studyEndDate are empty strings, CohortMethod will not restrict the study period.
studyPeriods <- tibble(
  studyStartDate = c(""), # From 'getDbCohortMethodDataArgs.studyPeriods.studyStartDate'
  studyEndDate   = c("")  # From 'getDbCohortMethodDataArgs.studyPeriods.studyEndDate'
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# These are extracted from 'createStudyPopArgs.timeAtRisks'.
timeAtRisks <- tibble(
  label = c("TAR1", "TAR2"), # Custom labels for description
  riskWindowStart  = c(1, 1), # From 'createStudyPopArgs.timeAtRisks.riskWindowStart'
  startAnchor = c("cohort start", "cohort start"), # From 'createStudyPopArgs.timeAtRisks.startAnchor'
  riskWindowEnd  = c(0, 99999), # From 'createStudyPopArgs.timeAtRisks.riskWindowEnd'
  endAnchor = c("cohort end", "cohort start"), # From 'createStudyPopArgs.timeAtRisks.endAnchor'
  minDaysAtRisk = c(1, 1) # From 'createStudyPopArgs.timeAtRisks.minDaysAtRisk'
)

# Propensity Score settings - match on PS
# Extracted from 'propensityScoreAdjustment.psSettings.matchOnPsArgs'.
matchOnPsArgsList <- tibble(
  label = c("PS_Match_1"), # Custom label for description
  maxRatio  = c(1), # From 'matchOnPsArgs.maxRatio'
  caliper = c(0.2), # From 'matchOnPsArgs.caliper'
  caliperScale  = c("standardized logit") # From 'matchOnPsArgs.caliperScale'
)

# Propensity Score settings - stratify by PS
# Extracted from 'propensityScoreAdjustment.psSettings.stratifyByPsArgs'.
# Since 'stratifyByPsArgs' is null in the specifications, this data frame will be empty.
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0) # "all" | "target" | "comparator"
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

      # Propensity score adjustment settings (match or stratify)
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default, not specified in analysis specifications.
          stratificationColumns = c() # Default, not specified in analysis specifications.
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default, not specified in analysis specifications.
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings for feature extraction.
      # 'addDescendantsToExclude' is TRUE by default in the template.
      # 'excludedCovariateConceptIds' are passed from 'excludedCovariateConcepts'.
      # 'includedCovariateConceptIds' would be passed if 'includedCovariateConcepts' was used.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE,
        excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        # includedCovariateConceptIds = includedCovariateConcepts$conceptId # Uncomment if includedCovariateConcepts is used
      )

      # Define outcomes for the analysis.
      # Includes the main outcome (re-numbered to 3) and negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not specified in analysis specifications.
            priorOutcomeLookback = 99999 # Default, not specified in analysis specifications.
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # Default for negative controls, not specified in analysis specifications.
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
          # Excluded covariate concept IDs are from 'covariateSelection.conceptsToExclude'.
          # The template's original `cmTcList$targetConceptId[i]` and `cmTcList$comparatorConceptId[i]`
          # are removed as they are not defined in the analysis specifications.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Settings for fetching cohort method data from the database.
      # Parameters are extracted from 'getDbCohortMethodDataArgs'.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE, # From 'getDbCohortMethodDataArgs.restrictToCommonPeriod'
        studyStartDate = studyStartDate, # From loop variable, derived from 'getDbCohortMethodDataArgs.studyPeriods'
        studyEndDate = studyEndDate, # From loop variable, derived from 'getDbCohortMethodDataArgs.studyPeriods'
        maxCohortSize = 0, # From 'getDbCohortMethodDataArgs.maxCohortSize'
        covariateSettings = covariateSettings,
        firstExposureOnly = FALSE, # From 'getDbCohortMethodDataArgs.firstExposureOnly'
        washoutPeriod = 0, # From 'getDbCohortMethodDataArgs.washoutPeriod'
        removeDuplicateSubjects = "keep all" # From 'getDbCohortMethodDataArgs.removeDuplicateSubjects'
      )

      # Settings for creating propensity scores.
      # Parameters are extracted from 'propensityScoreAdjustment.createPsArgs'.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From 'propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting'
        errorOnHighCorrelation = TRUE, # From 'propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation'
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att", # Default, not specified in analysis specifications.
        prior = Cyclops::createPrior( # Prior settings from 'propensityScoreAdjustment.createPsArgs.prior'
          priorType = "laplace", # From 'prior.priorType'
          exclude = c(0), # Default, not specified in analysis specifications.
          useCrossValidation = TRUE # From 'prior.useCrossValidation'
        ),
        control = Cyclops::createControl( # Control settings from 'propensityScoreAdjustment.createPsArgs.control'
          noiseLevel = "silent", # From 'control.noiseLevel'
          cvType = "auto", # From 'control.cvType'
          fold = 10, # From 'control.fold'
          seed = 1, # Default, not specified in analysis specifications.
          resetCoefficients = TRUE, # From 'control.resetCoefficients'
          tolerance = 2e-07, # From 'control.tolerance'
          cvRepetitions = 10, # From 'control.cvRepetitions'
          startingVariance = 0.01 # From 'control.startingVariance'
        )
      )

      # Settings for computing shared covariate balance.
      # Default values are used as not specified in analysis specifications.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      # Settings for computing covariate balance.
      # Default values are used as not specified in analysis specifications.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Settings for fitting the outcome model.
      # Parameters are extracted from 'fitOutcomeModelArgs'.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From 'fitOutcomeModelArgs.modelType'
        stratified = FALSE, # From 'fitOutcomeModelArgs.stratified'
        useCovariates = FALSE, # From 'fitOutcomeModelArgs.useCovariates'
        inversePtWeighting = FALSE, # From 'fitOutcomeModelArgs.inversePtWeighting'
        prior = Cyclops::createPrior( # Prior settings from 'fitOutcomeModelArgs.prior'
          priorType = "laplace", # From 'prior.priorType'
          useCrossValidation = TRUE # From 'prior.useCrossValidation'
        ),
        control = Cyclops::createControl( # Control settings from 'fitOutcomeModelArgs.control'
          cvType = "auto", # From 'control.cvType'
          fold = 10, # From 'control.fold'
          seed = 1, # Default, not specified in analysis specifications.
          resetCoefficients = TRUE, # From 'control.resetCoefficients'
          startingVariance = 0.01, # From 'control.startingVariance'
          tolerance = 2e-07, # From 'control.tolerance'
          cvRepetitions = 10, # From 'control.cvRepetitions'
          noiseLevel = "quiet" # From 'control.noiseLevel'
        )
      )

      # Settings for creating the study population.
      # Parameters are extracted from 'createStudyPopArgs' and 'timeAtRisks'.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From 'createStudyPopArgs.restrictToCommonPeriod'
        firstExposureOnly = FALSE, # From 'createStudyPopArgs.firstExposureOnly'
        washoutPeriod = 0, # From 'createStudyPopArgs.washoutPeriod'
        removeDuplicateSubjects = "keep all", # From 'createStudyPopArgs.removeDuplicateSubjects'
        censorAtNewRiskWindow = FALSE, # From 'createStudyPopArgs.censorAtNewRiskWindow'
        removeSubjectsWithPriorOutcome = FALSE, # From 'createStudyPopArgs.removeSubjectsWithPriorOutcome'
        priorOutcomeLookback = 99999, # From 'createStudyPopArgs.priorOutcomeLookBack'
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From loop variable, derived from 'createStudyPopArgs.timeAtRisks'
        startAnchor = timeAtRisks$startAnchor[t], # From loop variable, derived from 'createStudyPopArgs.timeAtRisks'
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From loop variable, derived from 'createStudyPopArgs.timeAtRisks'
        endAnchor = timeAtRisks$endAnchor[t], # From loop variable, derived from 'createStudyPopArgs.timeAtRisks'
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From loop variable, derived from 'createStudyPopArgs.timeAtRisks'
        maxDaysAtRisk = 99999 # Default, not specified in analysis specifications.
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
  analysesToExclude = NULL, # Not specified in analysis specifications.
  refitPsForEveryOutcome = FALSE, # Default, not specified in analysis specifications.
  refitPsForEveryStudyPopulation = FALSE, # Default, not specified in analysis specifications.
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default, not specified in analysis specifications.
)

# Create the analysis specifications ------------------------------------------
# The overall analysis specifications are built by adding shared resources and module specifications.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path uses the 'name' from the analysis specifications, which is "tramadolcodein".
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "tramadolcodein", "tramadolcodeinAnalysisSpecification.json")
)