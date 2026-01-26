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
library(CohortGenerator)
library(CohortDiagnostics)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(ParallelLogger)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
# Base URL for the WebAPI instance where cohort definitions are stored.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions from WebAPI based on IDs provided in Analysis Specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: sglt2imetformin.cohortDefinitions.targetCohort.id
    1794132, # Comparator: sglt2imetformin.cohortDefinitions.comparatorCohort.id
    1794131  # Outcome: sglt2imetformin.cohortDefinitions.outcomeCohort[0].id
  ),
  generateStats = TRUE
)

# Re-number cohorts to a simplified 1, 2, 3... scheme for internal use.
# This maps the original WebAPI IDs to generic IDs for the study.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Target cohort ID
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Comparator cohort ID
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Outcome cohort ID

# Update cohort names based on Analysis Specifications
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1,]$cohortName <- "target1" # sglt2imetformin.cohortDefinitions.targetCohort.name
cohortDefinitionSet[cohortDefinitionSet$cohortId == 2,]$cohortName <- "comparator1" # sglt2imetformin.cohortDefinitions.comparatorCohort.name
cohortDefinitionSet[cohortDefinitionSet$cohortId == 3,]$cohortName <- "outcome1" # sglt2imetformin.cohortDefinitions.outcomeCohort[0].name

# Negative control outcomes
# Retrieve negative control concept set definition from WebAPI.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # sglt2imetformin.negativeControlConceptSet.id
  baseUrl = baseUrl
) %>%
  # Resolve the concept set to get all included concepts.
  ROhdsiWebApi::resolveConceptSet(
    baseUrl = baseUrl
  ) %>%
  # Get detailed information for each concept.
  ROhdsiWebApi::getConcepts(
    baseUrl = baseUrl
  ) %>%
  # Rename columns to match expected format for negative control outcomes.
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  # Assign unique cohort IDs starting from 101 to avoid collision with study cohorts.
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)


# Check for duplicate cohort IDs to prevent errors.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Filter for the main outcome cohort (ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not specified in analysis spec.

# Target and Comparator for the CohortMethod analysis 
# Use the re-numbered target (1) and comparator (2) cohorts.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1", # sglt2imetformin.cohortDefinitions.targetCohort.name
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1" # sglt2imetformin.cohortDefinitions.comparatorCohort.name
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study. Based on analysis spec, conceptsToExclude is empty.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0), # sglt2imetformin.covariateSelection.conceptsToExclude is empty
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# Based on analysis spec, conceptsToInclude is empty.
includedCovariateConcepts <- data.frame(
  conceptId = integer(0), # sglt2imetformin.covariateSelection.conceptsToInclude is empty
  conceptName = character(0)
)

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Create shared resource specifications for cohort definitions.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Create shared resource specifications for negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default, not specified in analysis spec.
  detectOnDescendants = TRUE # Default, not specified in analysis spec.
)
# Create module specifications for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # As specified in the template.
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Create module specifications for CohortDiagnostics.
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # Run diagnostics for all defined cohorts.
  runInclusionStatistics = TRUE, # Default, not specified in analysis spec.
  runIncludedSourceConcepts = TRUE, # Default, not specified in analysis spec.
  runOrphanConcepts = TRUE, # Default, not specified in analysis spec.
  runTimeSeries = FALSE, # Default, not specified in analysis spec.
  runVisitContext = TRUE, # Default, not specified in analysis spec.
  runBreakdownIndexEvents = TRUE, # Default, not specified in analysis spec.
  runIncidenceRate = TRUE, # Default, not specified in analysis spec.
  runCohortRelationship = TRUE, # Default, not specified in analysis spec.
  runTemporalCohortCharacterization = TRUE, # Default, not specified in analysis spec.
  minCharacterizationMean = 0.01 # Default, not specified in analysis spec.
)

# CohortMethodModule -----------------------------------------------------------

# Study periods for data extraction, from sglt2imetformin.getDbCohortMethodDataArgs.studyPeriods
studyPeriods <- tibble(
  studyStartDate = c("20130401", "20130401"), # YYYYMMDD
  studyEndDate   = c("20200331", "20181231")  # YYYYMMDD
)

# Time-at-risks (TARs) for the outcomes of interest in your study, from sglt2imetformin.createStudyPopArgs.timeAtRisks
timeAtRisks <- tibble(
  label = c("TAR1", "TAR2"), # Custom labels for each time-at-risk window.
  riskWindowStart  = c(1, 1),
  startAnchor = c("cohort start", "cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd  = c(0, 99999),
  endAnchor = c("cohort end", "cohort start"), # "cohort start" | "cohort end"
  minDaysAtRisk = c(1, 1) # From sglt2imetformin.createStudyPopArgs.timeAtRisks.minDaysAtRisk
) 

# Propensity Score settings - match on PS, from sglt2imetformin.propensityScoreAdjustment.psSettings
matchOnPsArgsList <- tibble(
  label = c("MatchOnPs1"), # Custom label for this PS matching setting.
  maxRatio  = c(1), # sglt2imetformin.propensityScoreAdjustment.psSettings[0].matchOnPsArgs.maxRatio
  caliper = c(0.2), # sglt2imetformin.propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliper
  caliperScale  = c("standardized logit") # sglt2imetformin.propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliperScale
) 

# Propensity Score settings - stratify by PS. Not specified in analysis spec.
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

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment arguments based on the method (match or stratify).
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default, not specified in analysis spec.
          stratificationColumns = c() # Default, not specified in analysis spec.
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default, not specified in analysis spec.
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings for feature extraction.
      # If included/excluded concepts are provided in the analysis spec, use them.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE, # Default, not specified in analysis spec.
        excludedCovariateConceptIds = if (nrow(excludedCovariateConcepts) > 0) excludedCovariateConcepts$conceptId else c(),
        includedCovariateConceptIds = if (nrow(includedCovariateConcepts) > 0) includedCovariateConcepts$conceptId else c()
      )

      # Define outcomes for the CohortMethod analysis.
      outcomeList <- append(
        # Main outcome cohort (ID 3) from oList.
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not specified in analysis spec.
            priorOutcomeLookback = 99999 # Default, not specified in analysis spec.
          )
        }),
        # Negative control outcome cohorts.
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # Default for negative controls.
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
          # Note: cmTcList does not contain target/comparator concept IDs, only cohort IDs.
          # The excludedCovariateConcepts data frame is used here.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Arguments for fetching data from the database.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = studyPeriods$restrictToCommonPeriod[s], # sglt2imetformin.getDbCohortMethodDataArgs.restrictToCommonPeriod
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # sglt2imetformin.getDbCohortMethodDataArgs.maxCohortSize
        covariateSettings = covariateSettings,
        firstExposureOnly = FALSE, # sglt2imetformin.getDbCohortMethodDataArgs.firstExposureOnly
        washoutPeriod = 0, # sglt2imetformin.getDbCohortMethodDataArgs.washoutPeriod
        removeDuplicateSubjects = "keep all" # sglt2imetformin.getDbCohortMethodDataArgs.removeDuplicateSubjects
      )

      # Arguments for creating propensity scores.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # sglt2imetformin.propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # sglt2imetformin.propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att", # Default, not specified in analysis spec.
        prior = Cyclops::createPrior( # From sglt2imetformin.propensityScoreAdjustment.createPsArgs.prior
          priorType = "laplace", 
          exclude = c(0), # Default, not specified in analysis spec.
          useCrossValidation = TRUE # sglt2imetformin.propensityScoreAdjustment.createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # From sglt2imetformin.propensityScoreAdjustment.createPsArgs.control
          noiseLevel = "silent", # sglt2imetformin.propensityScoreAdjustment.createPsArgs.control.noiseLevel
          cvType = "auto", # sglt2imetformin.propensityScoreAdjustment.createPsArgs.control.cvType
          seed = 1, # Default, not specified in analysis spec.
          resetCoefficients = TRUE, # sglt2imetformin.propensityScoreAdjustment.createPsArgs.control.resetCoefficients
          tolerance = 2e-07, # sglt2imetformin.propensityScoreAdjustment.createPsArgs.control.tolerance
          cvRepetitions = 10, # sglt2imetformin.propensityScoreAdjustment.createPsArgs.control.cvRepetitions
          startingVariance = 0.01, # sglt2imetformin.propensityScoreAdjustment.createPsArgs.control.startingVariance
          fold = 10 # sglt2imetformin.propensityScoreAdjustment.createPsArgs.control.fold
        )
      )

      # Arguments for computing shared covariate balance.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default, not specified in analysis spec.
        covariateFilter = NULL # Default, not specified in analysis spec.
      )
      # Arguments for computing covariate balance.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default, not specified in analysis spec.
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default, not specified in analysis spec.
      )

      # Arguments for fitting the outcome model.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # sglt2imetformin.fitOutcomeModelArgs.modelType
        stratified = TRUE, # sglt2imetformin.fitOutcomeModelArgs.stratified
        useCovariates = FALSE, # sglt2imetformin.fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE, # sglt2imetformin.fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior( # From sglt2imetformin.fitOutcomeModelArgs.prior
          priorType = "laplace", 
          useCrossValidation = TRUE # sglt2imetformin.fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # From sglt2imetformin.fitOutcomeModelArgs.control
          cvType = "auto", # sglt2imetformin.fitOutcomeModelArgs.control.cvType
          seed = 1, # Default, not specified in analysis spec.
          resetCoefficients = TRUE, # sglt2imetformin.fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01, # sglt2imetformin.fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07, # sglt2imetformin.fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10, # sglt2imetformin.fitOutcomeModelArgs.control.cvRepetitions
          noiseLevel = "quiet", # sglt2imetformin.fitOutcomeModelArgs.control.noiseLevel
          fold = 10 # sglt2imetformin.fitOutcomeModelArgs.control.fold
        )
      )
      
      # Arguments for creating the study population.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # sglt2imetformin.createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE, # sglt2imetformin.createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0, # sglt2imetformin.createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # sglt2imetformin.createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # sglt2imetformin.createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # sglt2imetformin.createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # sglt2imetformin.createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # sglt2imetformin.createStudyPopArgs.timeAtRisks.minDaysAtRisk
        maxDaysAtRisk = 99999 # Default, not specified in analysis spec.
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
  analysesToExclude = NULL, # Default, not specified in analysis spec.
  refitPsForEveryOutcome = FALSE, # Default, not specified in analysis spec.
  refitPsForEveryStudyPopulation = FALSE, # Default, not specified in analysis spec.
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default, not specified in analysis spec.
)

# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "sglt2imetformin", "sglt2imetforminAnalysisSpecification.json")
)