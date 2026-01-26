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
# Base URL for the WebAPI instance to retrieve cohort definitions and concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions from WebAPI based on IDs provided in Analysis Specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1 (from Analysis Specifications -> cohortDefinitions.targetCohort.id)
    1794132, # Comparator: comparator1 (from Analysis Specifications -> cohortDefinitions.comparatorCohort.id)
    1794131  # Outcome: outcome1 (from Analysis Specifications -> cohortDefinitions.outcomeCohort[0].id)
  ),
  generateStats = TRUE
)

# Re-number cohorts to standard IDs (1 for target, 2 for comparator, 3 for outcome)
# This ensures consistent internal referencing within the Strategus modules.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Target
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Comparator
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Outcome

# Negative control outcomes
# Retrieve negative control concept set definition from WebAPI.
# (from Analysis Specifications -> negativeControlConceptSet.id)
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # negative (from Analysis Specifications -> negativeControlConceptSet.id)
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
  # Assign unique cohort IDs for negative controls, starting from 101 to avoid conflicts with T/C/O.
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs to prevent errors in downstream modules.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Filter for the primary outcome cohort (ID 3 after re-numbering).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Outcome cohort ID is 3 after re-numbering
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not specified in Analysis Specifications.

# Target and Comparator for the CohortMethod analysis
# Populate with re-numbered IDs and names from Analysis Specifications.
cmTcList <- data.frame(
  targetCohortId = 1, # Renumbered target cohort ID
  targetCohortName = "target1", # From Analysis Specifications -> cohortDefinitions.targetCohort.name
  comparatorCohortId = 2, # Renumbered comparator cohort ID
  comparatorCohortName = "comparator1" # From Analysis Specifications -> cohortDefinitions.comparatorCohort.name
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study. This list is empty based on Analysis Specifications -> covariateSelection.conceptsToExclude.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all
# This section is commented out as Analysis Specifications -> covariateSelection.conceptsToInclude is empty.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Create shared resource specifications for cohort definitions.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Create shared resource specifications for negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default, not in spec
  detectOnDescendants = TRUE # Default, not in spec
)
# Create module specifications for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Default, not in spec
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Create module specifications for CohortDiagnostics.
# Includes all defined cohort IDs (target, comparator, outcome, and negative controls).
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId),
  runInclusionStatistics = TRUE, # Default, not in spec
  runIncludedSourceConcepts = TRUE, # Default, not in spec
  runOrphanConcepts = TRUE, # Default, not in spec
  runTimeSeries = FALSE, # Default, not in spec
  runVisitContext = TRUE, # Default, not in spec
  runBreakdownIndexEvents = TRUE, # Default, not in spec
  runIncidenceRate = TRUE, # Default, not in spec
  runCohortRelationship = TRUE, # Default, not in spec
  runTemporalCohortCharacterization = TRUE, # Default, not in spec
  minCharacterizationMean = 0.01 # Default, not in spec
)

# CohortMethodModule -----------------------------------------------------------

# Study periods for restricting data retrieval.
# (from Analysis Specifications -> getDbCohortMethodDataArgs.studyPeriods)
studyPeriods <- tibble(
  studyStartDate = c("20030101"), # YYYYMMDD (from Analysis Specifications -> getDbCohortMethodDataArgs.studyPeriods[0].studyStartDate)
  studyEndDate   = c(NA_character_) # YYYYMMDD (from Analysis Specifications -> getDbCohortMethodDataArgs.studyPeriods[0].studyEndDate, which is null)
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# (from Analysis Specifications -> createStudyPopArgs.timeAtRisks)
timeAtRisks <- tibble(
  label = c("TAR 30-5475 days from start", "TAR 365-5475 days from start"), # Custom labels for clarity
  riskWindowStart  = c(30, 365), # From Analysis Specifications -> createStudyPopArgs.timeAtRisks[].riskWindowStart
  startAnchor = c("cohort start", "cohort start"), # From Analysis Specifications -> createStudyPopArgs.timeAtRisks[].startAnchor
  riskWindowEnd  = c(5475, 5475), # From Analysis Specifications -> createStudyPopArgs.timeAtRisks[].riskWindowEnd
  endAnchor = c("cohort start", "cohort start"), # From Analysis Specifications -> createStudyPopArgs.timeAtRisks[].endAnchor
  minDaysAtRisk = c(1, 1) # From Analysis Specifications -> createStudyPopArgs.timeAtRisks[].minDaysAtRisk
)

# Propensity Score settings - match on PS
# (from Analysis Specifications -> propensityScoreAdjustment.psSettings[0].matchOnPsArgs)
matchOnPsArgsList <- tibble(
  label = c("Match on PS (maxRatio 1, caliper 0.2 standardized logit)"), # Custom label
  maxRatio  = c(1), # From Analysis Specifications -> propensityScoreAdjustment.psSettings[0].matchOnPsArgs.maxRatio
  caliper = c(0.2), # From Analysis Specifications -> propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliper
  caliperScale  = c("standardized logit") # From Analysis Specifications -> propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliperScale
)

# Propensity Score settings - stratify by PS
# (from Analysis Specifications -> propensityScoreAdjustment.psSettings[1].stratifyByPsArgs)
stratifyByPsArgsList <- tibble(
  label = c("Stratify by PS (5 strata, base all)"), # Custom label
  numberOfStrata  = c(5), # From Analysis Specifications -> propensityScoreAdjustment.psSettings[1].stratifyByPsArgs.numberOfStrata
  baseSelection = c("all") # From Analysis Specifications -> propensityScoreAdjustment.psSettings[1].stratifyByPsArgs.baseSelection
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

      # Configure PS adjustment arguments based on the current PS configuration
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

      # Default covariate settings. Analysis Specifications -> covariateSelection is empty,
      # so default settings are used.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default, not in spec
      )

      # Combine primary outcome and negative control outcomes for analysis.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Default, not in spec
            priorOutcomeLookback = 99999 # Default, not in spec
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
          # Excluded covariate concepts. Only includes concepts from 'excludedCovariateConcepts'
          # as target/comparator concept IDs are not provided in Analysis Specifications.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Arguments for retrieving cohort method data.
      # (from Analysis Specifications -> getDbCohortMethodDataArgs)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications -> getDbCohortMethodDataArgs.restrictToCommonPeriod
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From Analysis Specifications -> getDbCohortMethodDataArgs.maxCohortSize
        firstExposureOnly = TRUE, # From Analysis Specifications -> getDbCohortMethodDataArgs.firstExposureOnly
        washoutPeriod = 365, # From Analysis Specifications -> getDbCohortMethodDataArgs.washoutPeriod
        removeDuplicateSubjects = "remove all", # From Analysis Specifications -> getDbCohortMethodDataArgs.removeDuplicateSubjects
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # (from Analysis Specifications -> propensityScoreAdjustment.createPsArgs)
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications -> propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From Analysis Specifications -> propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att", # Default, not in spec
        prior = Cyclops::createPrior( # Prior settings for PS model
          priorType = "laplace", # From Analysis Specifications -> propensityScoreAdjustment.createPsArgs.prior.priorType
          exclude = c(0), # Default, not in spec
          useCrossValidation = TRUE # From Analysis Specifications -> propensityScoreAdjustment.createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings for PS model
          noiseLevel = "silent", # From Analysis Specifications -> propensityScoreAdjustment.createPsArgs.control.noiseLevel
          cvType = "auto", # From Analysis Specifications -> propensityScoreAdjustment.createPsArgs.control.cvType
          seed = 1, # Default, not in spec
          resetCoefficients = TRUE, # From Analysis Specifications -> propensityScoreAdjustment.createPsArgs.control.resetCoefficients
          tolerance = 2e-07, # From Analysis Specifications -> propensityScoreAdjustment.createPsArgs.control.tolerance
          cvRepetitions = 10, # From Analysis Specifications -> propensityScoreAdjustment.createPsArgs.control.cvRepetitions
          fold = 10, # From Analysis Specifications -> propensityScoreAdjustment.createPsArgs.control.fold
          startingVariance = 0.01 # From Analysis Specifications -> propensityScoreAdjustment.createPsArgs.control.startingVariance
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
      # (from Analysis Specifications -> fitOutcomeModelArgs)
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From Analysis Specifications -> fitOutcomeModelArgs.modelType
        stratified = TRUE, # From Analysis Specifications -> fitOutcomeModelArgs.stratified
        useCovariates = FALSE, # From Analysis Specifications -> fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE, # From Analysis Specifications -> fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior( # Prior settings for outcome model
          priorType = "laplace", # From Analysis Specifications -> fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE # From Analysis Specifications -> fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings for outcome model
          cvType = "auto", # From Analysis Specifications -> fitOutcomeModelArgs.control.cvType
          seed = 1, # Default, not in spec
          resetCoefficients = TRUE, # From Analysis Specifications -> fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01, # From Analysis Specifications -> fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07, # From Analysis Specifications -> fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10, # From Analysis Specifications -> fitOutcomeModelArgs.control.cvRepetitions
          fold = 10, # From Analysis Specifications -> fitOutcomeModelArgs.control.fold
          noiseLevel = "quiet" # From Analysis Specifications -> fitOutcomeModelArgs.control.noiseLevel
        )
      )

      # Arguments for creating the study population.
      # (from Analysis Specifications -> createStudyPopArgs and current timeAtRisks)
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications -> createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE, # From Analysis Specifications -> createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0, # From Analysis Specifications -> createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # From Analysis Specifications -> createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # From Analysis Specifications -> createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = FALSE, # From Analysis Specifications -> createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # From Analysis Specifications -> createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From Analysis Specifications -> createStudyPopArgs.timeAtRisks[].minDaysAtRisk
        maxDaysAtRisk = 99999 # Default, not in spec
      )

      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          studyStartDate,
          ifelse(is.na(studyEndDate), "End of Data", studyEndDate), # Handle NA studyEndDate for description
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
# Create module specifications for CohortMethod.
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Default, not in spec
  refitPsForEveryOutcome = FALSE, # Default, not in spec
  refitPsForEveryStudyPopulation = FALSE, # Default, not in spec
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default, not in spec
)

# Create the analysis specifications ------------------------------------------
# Combine all shared resources and module specifications into a single analysis specification object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path uses the study name from Analysis Specifications -> name.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "iudehre", "iudehreAnalysisSpecification.json") # From Analysis Specifications -> name
)