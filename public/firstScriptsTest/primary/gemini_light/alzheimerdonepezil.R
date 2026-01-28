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
# Base URL for the ATLAS/WebAPI instance to retrieve cohort definitions and concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Retrieve cohort definitions from WebAPI based on IDs specified in Analysis Specifications.
# The cohort IDs are re-mapped to generic IDs (1, 2, 3) for internal consistency within the study.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal study use:
# Target cohort (ID 1794126) is re-mapped to 1.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# Comparator cohort (ID 1794132) is re-mapped to 2.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# Outcome cohort (ID 1794131) is re-mapped to 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve the concept set for negative controls from WebAPI.
# These concepts are resolved to individual concepts and then converted into a cohort set.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Negative control concept set ID from Analysis Specifications
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
  # Assign unique cohort IDs starting from 101 to avoid collision with T/C/O cohorts.
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs to prevent conflicts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts used in each analysis ----------------
# Outcomes: Filter for the re-mapped outcome cohort (ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # Default cleanWindow, not specified in Analysis Specifications.
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1, # Re-mapped target cohort ID
  targetCohortName = "target1", # Target cohort name from Analysis Specifications
  comparatorCohortId = 2, # Re-mapped comparator cohort ID
  comparatorCohortName = "comparator1" # Comparator cohort name from Analysis Specifications
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study. Based on Analysis Specifications, no specific concepts are provided for exclusion
# beyond what default covariate settings might handle.
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(0), # No specific concepts to exclude from Analysis Specifications
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# Based on Analysis Specifications, no specific concepts are provided for inclusion.
includedCovariateConcepts <- data.frame(
  conceptId = numeric(0), # No specific concepts to include from Analysis Specifications
  conceptName = character(0)
)

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Shared resource for cohort definitions.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Shared resource for negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default, not specified in Analysis Specifications
  detectOnDescendants = TRUE # Default, not specified in Analysis Specifications
)
# Module specifications for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Generate cohort statistics
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Module specifications for CohortDiagnostics.
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # Run diagnostics for all defined cohorts
  runInclusionStatistics = TRUE, # Default
  runIncludedSourceConcepts = TRUE, # Default
  runOrphanConcepts = TRUE, # Default
  runTimeSeries = FALSE, # Default
  runVisitContext = TRUE, # Default
  runBreakdownIndexEvents = TRUE, # Default
  runIncidenceRate = TRUE, # Default
  runCohortRelationship = TRUE, # Default
  runTemporalCohortCharacterization = TRUE, # Default
  minCharacterizationMean = 0.01 # Default
)

# CohortMethodModule -----------------------------------------------------------

# Study periods: If studyStartDate/studyEndDate are null in Analysis Specifications,
# an empty tibble indicates no restriction to a specific time window.
studyPeriods <- tibble(
  studyStartDate = c(), # YYYYMMDD, from getDbCohortMethodDataArgs.studyPeriods in Analysis Specifications
  studyEndDate   = c()  # YYYYMMDD, from getDbCohortMethodDataArgs.studyPeriods in Analysis Specifications
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# Populated from createStudyPopArgs.timeAtRisks in Analysis Specifications.
timeAtRisks <- tibble(
  label = c("TAR1"), # A descriptive label for this time-at-risk window
  riskWindowStart  = c(1), # From createStudyPopArgs.timeAtRisks.riskWindowStart
  startAnchor = c("cohort start"), # From createStudyPopArgs.timeAtRisks.startAnchor
  riskWindowEnd  = c(180), # From createStudyPopArgs.timeAtRisks.riskWindowEnd
  endAnchor = c("cohort start") # From createStudyPopArgs.timeAtRisks.endAnchor
)

# Propensity Score settings - match on PS
# Populated from propensityScoreAdjustment.psSettings.matchOnPsArgs in Analysis Specifications.
matchOnPsArgsList <- tibble(
  label = c("Match1"), # A descriptive label for this PS matching setting
  maxRatio  = c(1), # From matchOnPsArgs.maxRatio
  caliper = c(0.2), # From matchOnPsArgs.caliper
  caliperScale  = c("standardized logit") # From matchOnPsArgs.caliperScale
)

# Propensity Score settings - stratify by PS
# From propensityScoreAdjustment.psSettings.stratifyByPsArgs in Analysis Specifications (which is null).
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

# If no study periods are defined, create a dummy entry to ensure the loop runs once.
if (nrow(studyPeriods) == 0) {
  studyPeriods <- tibble(studyStartDate = NA_character_, studyEndDate = NA_character_)
}

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
          allowReverseMatch = FALSE, # Default
          stratificationColumns = c() # Default
        )
      } else if (psCfg$method == "stratify") {
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings for FeatureExtraction.
      # Includes concepts to exclude/include from Analysis Specifications.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE, # Default
        excludedCovariateConceptIds = excludedCovariateConcepts$conceptId, # From Analysis Specifications
        includedCovariateConceptIds = includedCovariateConcepts$conceptId # From Analysis Specifications
      )

      # List of outcomes for the CohortMethod analysis.
      # Includes the main outcome and all negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not applicable for observed outcomes
            priorOutcomeLookback = 99999 # From createStudyPopArgs.priorOutcomeLookBack
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # For negative controls, true effect size is assumed to be 1
          )
        })
      )

      # Target-Comparator-Outcomes list for CohortMethod.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude general covariate concepts specified in Analysis Specifications.
          # Note: The template had placeholders for target/comparator concept IDs which are not in the spec.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Arguments for retrieving cohort method data from the database.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Default in template, not explicitly in getDbCohortMethodDataArgs in spec
        studyStartDate = studyStartDate, # From studyPeriods (can be NA if no restriction)
        studyEndDate = studyEndDate, # From studyPeriods (can be NA if no restriction)
        maxCohortSize = 0, # From getDbCohortMethodDataArgs.maxCohortSize in Analysis Specifications
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att", # Default
        prior = Cyclops::createPrior( # Prior settings from propensityScoreAdjustment.createPsArgs.prior
          priorType = "laplace", # From prior.priorType
          exclude = c(0), # Default
          useCrossValidation = TRUE # From prior.useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings from propensityScoreAdjustment.createPsArgs.control
          noiseLevel = "silent", # From control.noiseLevel
          cvType = "auto", # From control.cvType
          seed = 1, # Default
          resetCoefficients = TRUE, # From control.resetCoefficients
          tolerance = 2e-07, # From control.tolerance
          cvRepetitions = 10, # From control.cvRepetitions
          startingVariance = 0.01 # From control.startingVariance
        )
      )

      # Arguments for computing shared covariate balance.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default
        covariateFilter = NULL # Default
      )
      # Arguments for computing covariate balance.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default
      )

      # Arguments for fitting the outcome model.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "logistic", # From fitOutcomeModelArgs.modelType
        stratified = TRUE, # From fitOutcomeModelArgs.stratified
        useCovariates = FALSE, # From fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE, # From fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior( # Prior settings from fitOutcomeModelArgs.prior
          priorType = "laplace", # From prior.priorType
          useCrossValidation = TRUE # From prior.useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings from fitOutcomeModelArgs.control
          cvType = "auto", # From control.cvType
          seed = 1, # Default
          resetCoefficients = TRUE, # From control.resetCoefficients
          startingVariance = 0.01, # From control.startingVariance
          tolerance = 2e-07, # From control.tolerance
          cvRepetitions = 10, # From control.cvRepetitions
          noiseLevel = "quiet" # From control.noiseLevel
        )
      )

      # Arguments for creating the study population.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE, # From createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE, # From createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0, # From createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # From createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # From createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # From createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # From createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From createStudyPopArgs.timeAtRisks.riskWindowStart
        startAnchor = timeAtRisks$startAnchor[t], # From createStudyPopArgs.timeAtRisks.startAnchor
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From createStudyPopArgs.timeAtRisks.riskWindowEnd
        endAnchor = timeAtRisks$endAnchor[t], # From createStudyPopArgs.timeAtRisks.endAnchor
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From createStudyPopArgs.timeAtRisks.minDaysAtRisk
        maxDaysAtRisk = 99999 # Default, not specified in Analysis Specifications
      )

      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(is.na(studyStartDate), "NoStartDate", studyStartDate), # Handle NA study dates
          ifelse(is.na(studyEndDate), "NoEndDate", studyEndDate), # Handle NA study dates
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
# Module specifications for CohortMethod.
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Default
  refitPsForEveryOutcome = FALSE, # Default
  refitPsForEveryStudyPopulation = FALSE, # Default
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default, not specified in Analysis Specifications
)

# Create the analysis specifications ------------------------------------------
# The overall Strategus analysis specifications.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path uses the study name from Analysis Specifications.
studyName <- "alzheimerdonepezil" # From "name" in Analysis Specifications
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", studyName, paste0(studyName, "AnalysisSpecification.json"))
)