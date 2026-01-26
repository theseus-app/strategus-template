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
# Retrieve cohort definitions from WebAPI based on IDs provided in Analysis Specifications.
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
# This re-numbering simplifies referencing within the Strategus analysis.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Target cohort ID
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Comparator cohort ID
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Outcome cohort ID

# Negative control outcomes
# Retrieve negative control concept set from WebAPI and resolve it into individual concepts.
# The Analysis Specifications provides a single conceptSetId for negative controls.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # From Analysis Specifications: negativeControlConceptSet.id
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
  # Assign unique cohort IDs for negative controls, starting from 101 to avoid collision with T/C/O.
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs to prevent errors in Strategus.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Filter for the primary outcome cohort (re-numbered ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not specified in Analysis Specifications.

# Target and Comparator for the CohortMethod analysis 
# Use the re-numbered target (ID 1) and comparator (ID 2) cohorts.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1", # From Analysis Specifications: cohortDefinitions.targetCohort.name
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1" # From Analysis Specifications: cohortDefinitions.comparatorCohort.name
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study. The Analysis Specifications provides an empty list for conceptsToExclude.
excludedCovariateConcepts <- data.frame(
  conceptId = c(), # From Analysis Specifications: covariateSelection.conceptsToExclude (empty)
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all
# The Analysis Specifications provides an empty list for conceptsToInclude.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Create shared resource for cohort definitions.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Create shared resource for negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default, not specified in Analysis Specifications.
  detectOnDescendants = TRUE # Default, not specified in Analysis Specifications.
)
# Create module specifications for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Default, not specified in Analysis Specifications.
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Create module specifications for CohortDiagnostics.
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # Run diagnostics for all defined cohorts.
  runInclusionStatistics = TRUE, # Default, not specified in Analysis Specifications.
  runIncludedSourceConcepts = TRUE, # Default, not specified in Analysis Specifications.
  runOrphanConcepts = TRUE, # Default, not specified in Analysis Specifications.
  runTimeSeries = FALSE, # Default, not specified in Analysis Specifications.
  runVisitContext = TRUE, # Default, not specified in Analysis Specifications.
  runBreakdownIndexEvents = TRUE, # Default, not specified in Analysis Specifications.
  runIncidenceRate = TRUE, # Default, not specified in Analysis Specifications.
  runCohortRelationship = TRUE, # Default, not specified in Analysis Specifications.
  runTemporalCohortCharacterization = TRUE, # Default, not specified in Analysis Specifications.
  minCharacterizationMean = 0.01 # Default, not specified in Analysis Specifications.
)

# CohortMethodModule -----------------------------------------------------------

# Study periods for data extraction.
# From Analysis Specifications: getDbCohortMethodDataArgs.studyPeriods
studyPeriods <- tibble(
  studyStartDate = c("20010101", "20010101"), # YYYYMMDD
  studyEndDate   = c("20171231", "20150930")  # YYYYMMDD
)

# Time-at-risks (TARs) for the outcomes of interest in your study
# From Analysis Specifications: createStudyPopArgs.timeAtRisks
timeAtRisks <- tibble(
  label = c("TAR_1_0_cohort_start_end"), # Custom label for description
  riskWindowStart  = c(1), # From Analysis Specifications: createStudyPopArgs.timeAtRisks[0].riskWindowStart
  startAnchor = c("cohort start"), # From Analysis Specifications: createStudyPopArgs.timeAtRisks[0].startAnchor
  riskWindowEnd  = c(0), # From Analysis Specifications: createStudyPopArgs.timeAtRisks[0].riskWindowEnd
  endAnchor = c("cohort end") # From Analysis Specifications: createStudyPopArgs.timeAtRisks[0].endAnchor
) 

# Build a single PS configuration list (each entry has: method, label, params)
# This list will contain all propensity score adjustment strategies defined in the Analysis Specifications.
psConfigList <- list()

# From Analysis Specifications: propensityScoreAdjustment.psSettings
# 1. No PS adjustment (matchOnPsArgs = null, stratifyByPsArgs = null)
psConfigList[[length(psConfigList) + 1]] <- list(
  method = "none",
  label  = "No PS adjustment",
  params = list()
)

# 2. Match on PS (maxRatio = 1, caliper = 0.05, caliperScale = "propensity score")
# From Analysis Specifications: propensityScoreAdjustment.psSettings[1].matchOnPsArgs
psConfigList[[length(psConfigList) + 1]] <- list(
  method = "match",
  label  = "Match on PS (ratio 1, caliper 0.05 PS)",
  params = list(
    maxRatio     = 1, # From Analysis Specifications: propensityScoreAdjustment.psSettings[1].matchOnPsArgs.maxRatio
    caliper      = 0.05, # From Analysis Specifications: propensityScoreAdjustment.psSettings[1].matchOnPsArgs.caliper
    caliperScale = "propensity score" # From Analysis Specifications: propensityScoreAdjustment.psSettings[1].matchOnPsArgs.caliperScale
  )
)

# 3. Match on PS (maxRatio = 10, caliper = 0.2, caliperScale = "standardized logit")
# From Analysis Specifications: propensityScoreAdjustment.psSettings[2].matchOnPsArgs
psConfigList[[length(psConfigList) + 1]] <- list(
  method = "match",
  label  = "Match on PS (ratio 10, caliper 0.2 SL)",
  params = list(
    maxRatio     = 10, # From Analysis Specifications: propensityScoreAdjustment.psSettings[2].matchOnPsArgs.maxRatio
    caliper      = 0.2, # From Analysis Specifications: propensityScoreAdjustment.psSettings[2].matchOnPsArgs.caliper
    caliperScale = "standardized logit" # From Analysis Specifications: propensityScoreAdjustment.psSettings[2].matchOnPsArgs.caliperScale
  )
)

# Iterate through all analysis setting combinations
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      matchOnPsArgs <- NULL
      stratifyByPsArgs <- NULL
      
      # Determine if outcome model should be stratified based on PS adjustment method
      # If no PS adjustment ("none"), the outcome model should not be stratified.
      stratifiedOutcomeModel <- (psCfg$method != "none")

      if (psCfg$method == "match") {
        # Create MatchOnPsArgs based on the current PS configuration.
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default, not specified in Analysis Specifications.
          stratificationColumns = c() # Default, not specified in Analysis Specifications.
        )
      } else if (psCfg$method == "stratify") {
        # This branch is not used with the current Analysis Specifications as no stratifyByPsArgs are defined.
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      } else if (psCfg$method == "none") {
        # No PS adjustment, so both args are NULL.
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- NULL
      }

      # Covariate settings for feature extraction.
      # The Analysis Specifications has empty lists for conceptsToInclude/Exclude,
      # so default settings are used, with any explicitly excluded concepts.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE, # Default, not specified in Analysis Specifications.
        excludedCovariateConceptIds = excludedCovariateConcepts$conceptId # From Analysis Specifications: covariateSelection.conceptsToExclude
      )

      # Combine primary outcome and negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not applicable for observational studies.
            priorOutcomeLookback = 99999 # Default, not specified in Analysis Specifications.
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # For negative controls, true effect size is assumed to be 1.
          )
        })
      )
      
      # Create target-comparator-outcome combinations.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude concepts that define the target/comparator exposures themselves,
          # and any other specified excluded covariates.
          excludedCovariateConceptIds = c(
            excludedCovariateConcepts$conceptId # From Analysis Specifications: covariateSelection.conceptsToExclude
          )
        )
      }

      # Arguments for retrieving cohort method data from the database.
      # Settings from Analysis Specifications: getDbCohortMethodDataArgs
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications: getDbCohortMethodDataArgs.restrictToCommonPeriod
        studyStartDate = studyStartDate, # From current loop iteration.
        studyEndDate = studyEndDate, # From current loop iteration.
        maxCohortSize = 0, # From Analysis Specifications: getDbCohortMethodDataArgs.maxCohortSize (0 means no restriction)
        firstExposureOnly = TRUE, # From Analysis Specifications: getDbCohortMethodDataArgs.firstExposureOnly
        washoutPeriod = 183, # From Analysis Specifications: getDbCohortMethodDataArgs.washoutPeriod
        removeDuplicateSubjects = "keep first", # From Analysis Specifications: getDbCohortMethodDataArgs.removeDuplicateSubjects
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # Settings from Analysis Specifications: propensityScoreAdjustment.createPsArgs
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Default, set to FALSE to allow Strategus to complete all CM operations even if PS model fails.
        estimator = "att", # Default, not specified in Analysis Specifications.
        prior = Cyclops::createPrior( # Prior settings for regularization.
          priorType = "laplace", # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.prior.priorType
          exclude = c(0), # Default, not specified in Analysis Specifications.
          useCrossValidation = TRUE # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings for Cyclops solver.
          noiseLevel = "silent", # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.noiseLevel
          cvType = "auto", # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.cvType
          seed = 1, # Default, not specified in Analysis Specifications.
          resetCoefficients = TRUE, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.resetCoefficients
          tolerance = 2e-07, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.tolerance
          cvRepetitions = 10, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.cvRepetitions
          startingVariance = 0.01 # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.startingVariance
        )
      )

      # Arguments for computing covariate balance before PS adjustment.
      # Not specified in Analysis Specifications, using template defaults.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      # Arguments for computing covariate balance after PS adjustment.
      # Not specified in Analysis Specifications, using template defaults.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting the outcome model.
      # Settings from Analysis Specifications: fitOutcomeModelArgs
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From Analysis Specifications: fitOutcomeModelArgs.modelType
        stratified = stratifiedOutcomeModel, # FIX: Set 'stratified' based on whether PS adjustment is applied
        useCovariates = FALSE, # From Analysis Specifications: fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE, # From Analysis Specifications: fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior( # Prior settings for regularization.
          priorType = "laplace", # From Analysis Specifications: fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE # From Analysis Specifications: fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings for Cyclops solver.
          cvType = "auto", # From Analysis Specifications: fitOutcomeModelArgs.control.cvType
          seed = 1, # Default, not specified in Analysis Specifications.
          resetCoefficients = TRUE, # From Analysis Specifications: fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01, # From Analysis Specifications: fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07, # From Analysis Specifications: fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10, # From Analysis Specifications: fitOutcomeModelArgs.control.cvRepetitions
          noiseLevel = "quiet" # From Analysis Specifications: fitOutcomeModelArgs.control.noiseLevel
        )
      )
      
      # Arguments for creating the study population.
      # Settings from Analysis Specifications: createStudyPopArgs
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications: createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE, # From Analysis Specifications: createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0, # From Analysis Specifications: createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # From Analysis Specifications: createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # From Analysis Specifications: createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = FALSE, # From Analysis Specifications: createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # From Analysis Specifications: createStudyPopArgs.priorOutcomeLookback
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From current loop iteration.
        startAnchor = timeAtRisks$startAnchor[t], # From current loop iteration.
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From current loop iteration.
        endAnchor = timeAtRisks$endAnchor[t], # From current loop iteration.
        minDaysAtRisk = 1, # FIX: Use fixed value as per template to resolve warning
        maxDaysAtRisk = 99999 # FIX: Use fixed value as per template to resolve warning
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
  analysesToExclude = NULL, # Default, not specified in Analysis Specifications.
  refitPsForEveryOutcome = FALSE, # Default, not specified in Analysis Specifications.
  refitPsForEveryStudyPopulation = FALSE, # Default, not specified in Analysis Specifications.
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default, not specified in Analysis Specifications.
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
# The file path uses the study name "strokerisk" from Analysis Specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "strokerisk", "strokeriskAnalysisSpecification.json")
)