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
# Base URL for the OHDSI WebAPI, used to fetch cohort definitions and concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Fetch cohort definitions based on IDs from analysis specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target Cohort: "target1", from analysis specifications: cohortDefinitions.targetCohort.id
    1794132, # Comparator Cohort: "comparator1", from analysis specifications: cohortDefinitions.comparatorCohort.id
    1794131  # Outcome Cohort: "outcome1", from analysis specifications: cohortDefinitions.outcomeCohort[0].id
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal consistency within the Strategus module's operations.
# Target is mapped to 1, Comparator to 2, and Outcome to 3. This simplifies referencing in CM analyses.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Mapped Target ID
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Mapped Comparator ID
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Mapped Outcome ID

# Negative control outcomes
# Fetch negative control concept set definition based on ID from analysis specifications.
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
  # Assign unique cohort IDs for negative controls, starting from 101 to avoid conflicts with study cohorts (1, 2, 3).
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs to ensure uniqueness across all defined cohorts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts for analysis ---------------------------
# Outcomes for the study.
# Filters for the outcome cohort using its mapped ID (3) and extracts relevant information.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Filtering for the mapped outcome cohort ID (3)
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default cleanWindow, not specified in JSON analysis specifications

# Target and Comparator cohorts for the CohortMethod analysis.
# Uses the mapped cohort IDs (1 and 2) and names from analysis specifications.
cmTcList <- data.frame(
  targetCohortId = 1, # Mapped target cohort ID
  targetCohortName = "target1", # From analysis specifications: cohortDefinitions.targetCohort.name
  comparatorCohortId = 2, # Mapped comparator cohort ID
  comparatorCohortName = "comparator1" # From analysis specifications: cohortDefinitions.comparatorCohort.name
)

# Concepts to exclude from covariates.
# From analysis specifications: covariateSelection.conceptsToExclude.
# Since the input JSON has an empty array for `conceptsToExclude`, this data frame will be empty.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all.
# From analysis specifications: covariateSelection.conceptsToInclude.
# Since the input JSON has an empty array for `conceptsToInclude`, this is commented out.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule Settings -----------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Defines shared resources for cohort definitions, making them available to other modules.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Defines shared resources for negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default setting, not specified in JSON
  detectOnDescendants = TRUE # Default setting, not specified in JSON
)
# Creates module specifications for CohortGenerator, enabling cohort generation and statistics.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Generates cohort statistics
)

# CohortDiagnosticsModule Settings --------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Creates module specifications for CohortDiagnostics.
# Cohort IDs include all study cohorts (target, comparator, outcome, negative controls).
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId), # All cohort IDs to run diagnostics on
  runInclusionStatistics = TRUE, # Default setting, not specified in JSON
  runIncludedSourceConcepts = TRUE, # Default setting, not specified in JSON
  runOrphanConcepts = TRUE, # Default setting, not specified in JSON
  runTimeSeries = FALSE, # Default setting, not specified in JSON
  runVisitContext = TRUE, # Default setting, not specified in JSON
  runBreakdownIndexEvents = TRUE, # Default setting, not specified in JSON
  runIncidenceRate = TRUE, # Default setting, not specified in JSON
  runCohortRelationship = TRUE, # Default setting, not specified in JSON
  runTemporalCohortCharacterization = TRUE, # Default setting, not specified in JSON
  minCharacterizationMean = 0.01 # Default setting, not specified in JSON
)

# CohortMethodModule Settings --------------------------------------------------

# Study periods to define the overall observation window for cohort method analyses.
# From analysis specifications: getDbCohortMethodDataArgs.studyPeriods.
studyPeriods <- tibble(
  studyStartDate = c("20010101"), # YYYYMMDD, from analysis specifications: getDbCohortMethodDataArgs.studyPeriods[0].studyStartDate
  studyEndDate   = c("20171231")  # YYYYMMDD, from analysis specifications: getDbCohortMethodDataArgs.studyPeriods[0].studyEndDate
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# From analysis specifications: createStudyPopArgs.timeAtRisks.
timeAtRisks <- tibble(
  label = c("Default TAR"), # A descriptive label for this TAR setting
  riskWindowStart  = c(1), # From analysis specifications: createStudyPopArgs.timeAtRisks[0].riskWindowStart
  startAnchor = c("cohort start"), # From analysis specifications: createStudyPopArgs.timeAtRisks[0].startAnchor
  riskWindowEnd  = c(0), # From analysis specifications: createStudyPopArgs.timeAtRisks[0].riskWindowEnd
  endAnchor = c("cohort end") # From analysis specifications: createStudyPopArgs.timeAtRisks[0].endAnchor
) 

# Propensity Score settings - match on PS.
# From analysis specifications: propensityScoreAdjustment.psSettings[0].matchOnPsArgs.
matchOnPsArgsList <- tibble(
  label = c("PS Matching"), # A descriptive label for this PS setting
  maxRatio  = c(10), # From analysis specifications: propensityScoreAdjustment.psSettings[0].matchOnPsArgs.maxRatio
  caliper = c(0.2), # From analysis specifications: propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliper
  caliperScale  = c("standardized logit") # From analysis specifications: propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliperScale
) 

# Propensity Score settings - stratify by PS.
# From analysis specifications: propensityScoreAdjustment.psSettings[0].stratifyByPsArgs.
# Since this is null in the JSON, this list is empty.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c()
) 

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# If a data frame for "match on PS" exists and has rows, convert each row to a config.
# This processes the `matchOnPsArgs` from the JSON.
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

# If a data frame for "stratify by PS" exists and has rows, convert each row to a config.
# This will not run if `stratifyByPsArgsList` is empty (as per JSON).
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

# Iterate through all analysis setting combinations to create a list of CM analyses.
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Determine PS adjustment method based on the configuration.
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio, # From analysis specifications: propensityScoreAdjustment.psSettings[0].matchOnPsArgs.maxRatio
          caliper = psCfg$params$caliper, # From analysis specifications: propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliper
          caliperScale = psCfg$params$caliperScale, # From analysis specifications: propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliperScale
          allowReverseMatch = FALSE, # Default setting, not specified in JSON
          stratificationColumns = c() # Default setting, not specified in JSON
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata, # From analysis specifications: propensityScoreAdjustment.psSettings[0].stratifyByPsArgs.numberOfStrata
          stratificationColumns = c(), # Default setting, not specified in JSON
          baseSelection = psCfg$params$baseSelection # From analysis specifications: propensityScoreAdjustment.psSettings[0].stratifyByPsArgs.baseSelection
        )
      }

      # Covariate settings using default configurations.
      # The JSON's `covariateSelection` is empty, so default settings are appropriate.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default setting, not specified in JSON
      )

      # List of outcomes for the analysis, including both study outcomes and negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # For study outcomes, trueEffectSize is NA
            priorOutcomeLookback = 99999 # Default setting, not specified in JSON
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # For negative controls, trueEffectSize is typically 1 (null effect)
          )
        })
      )

      # Target-Comparator-Outcome (TCO) list.
      # Sets up the target, comparator, and outcomes for each analysis.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        # Excluded covariate concept IDs.
        # From analysis specifications: covariateSelection.conceptsToExclude.
        # Since `covariateSelection.conceptsToExclude` is an empty array in the JSON,
        # and no target/comparator *concept* IDs are provided, this will be an empty vector.
        excludedConceptIdsForTco <- c(excludedCovariateConcepts$conceptId)
        
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i], # Mapped target cohort ID
          comparatorId = cmTcList$comparatorCohortId[i], # Mapped comparator cohort ID
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedConceptIdsForTco # Empty based on JSON
        )
      }

      # Arguments for retrieving cohort method data from the database.
      # Populated using `getDbCohortMethodDataArgs` from analysis specifications.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Not explicitly in JSON's getDbCohortMethodDataArgs, keeping template default.
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From analysis specifications: getDbCohortMethodDataArgs.maxCohortSize
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # Populated using `propensityScoreAdjustment.createPsArgs` from analysis specifications.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From analysis specifications: propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From analysis specifications: propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Template default, set to FALSE to allow Strategus to complete all CM operations even if PS model fitting fails for some T/C/O
        estimator = "att", # Template default, not specified in JSON
        prior = Cyclops::createPrior( # Prior settings for PS model fitting
          priorType = "laplace", # From analysis specifications: propensityScoreAdjustment.createPsArgs.prior.priorType
          exclude = c(0), # Default setting, not specified in JSON
          useCrossValidation = TRUE # From analysis specifications: propensityScoreAdjustment.createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings for PS model fitting
          noiseLevel = "silent", # From analysis specifications: propensityScoreAdjustment.createPsArgs.control.noiseLevel
          cvType = "auto", # From analysis specifications: propensityScoreAdjustment.createPsArgs.control.cvType
          seed = 1, # Default setting, not specified in JSON
          resetCoefficients = TRUE, # From analysis specifications: propensityScoreAdjustment.createPsArgs.control.resetCoefficients
          tolerance = 2e-07, # From analysis specifications: propensityScoreAdjustment.createPsArgs.control.tolerance
          cvRepetitions = 10, # From analysis specifications: propensityScoreAdjustment.createPsArgs.control.cvRepetitions
          startingVariance = 0.01 # From analysis specifications: propensityScoreAdjustment.createPsArgs.control.startingVariance
        )
      )

      # Arguments for computing shared covariate balance (before PS adjustment).
      # Default settings, not specified in JSON.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default setting, not specified in JSON
        covariateFilter = NULL # Default setting, not specified in JSON
      )
      # Arguments for computing covariate balance (after PS adjustment).
      # Default settings, not specified in JSON.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default setting, not specified in JSON
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default setting, not specified in JSON
      )

      # Arguments for fitting the outcome model.
      # Populated using `fitOutcomeModelArgs` from analysis specifications.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From analysis specifications: fitOutcomeModelArgs.modelType
        stratified = TRUE, # From analysis specifications: fitOutcomeModelArgs.stratified
        useCovariates = FALSE, # From analysis specifications: fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE, # From analysis specifications: fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior( # Prior settings for outcome model
          priorType = "laplace", # From analysis specifications: fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE # From analysis specifications: fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings for outcome model
          cvType = "auto", # From analysis specifications: fitOutcomeModelArgs.control.cvType
          seed = 1, # Default setting, not specified in JSON
          resetCoefficients = TRUE, # From analysis specifications: fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01, # From analysis specifications: fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07, # From analysis specifications: fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10, # From analysis specifications: fitOutcomeModelArgs.control.cvRepetitions
          noiseLevel = "quiet" # From analysis specifications: fitOutcomeModelArgs.control.noiseLevel
        )
      )

      # Arguments for creating the study population.
      # Populated using `createStudyPopArgs` from analysis specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From analysis specifications: createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE, # From analysis specifications: createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0, # From analysis specifications: createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # From analysis specifications: createStudyPopArgs.removeDuplicateSubjects (template default was "keep first")
        censorAtNewRiskWindow = FALSE, # From analysis specifications: createStudyPopArgs.censorAtNewRiskWindow (template default was TRUE)
        removeSubjectsWithPriorOutcome = TRUE, # From analysis specifications: createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # From analysis specifications: createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From `timeAtRisks` tibble, derived from JSON `createStudyPopArgs.timeAtRisks[0].riskWindowStart`
        startAnchor = timeAtRisks$startAnchor[t], # From `timeAtRisks` tibble, derived from JSON `createStudyPopArgs.timeAtRisks[0].startAnchor`
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From `timeAtRisks` tibble, derived from JSON `createStudyPopArgs.timeAtRisks[0].riskWindowEnd`
        endAnchor = timeAtRisks$endAnchor[t], # From `timeAtRisks` tibble, derived from JSON `createStudyPopArgs.timeAtRisks[0].endAnchor`
        minDaysAtRisk = 1, # From analysis specifications: createStudyPopArgs.timeAtRisks[0].minDaysAtRisk
        maxDaysAtRisk = 99999 # Default setting, not specified in JSON
      )

      # Append the settings to the CohortMethod Analysis List.
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
# Creates module specifications for CohortMethod, packaging all defined analyses.
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # No specific analyses to exclude, default
  refitPsForEveryOutcome = FALSE, # Default setting, not specified in JSON
  refitPsForEveryStudyPopulation = FALSE, # Default setting, not specified in JSON
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default diagnostic thresholds
)

# Create the overall analysis specifications for Strategus ----------------------
# Initializes an empty analysis specification and adds shared resources and module specifications.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path is constructed using the study name from analysis specifications ("strokerisk").
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "strokerisk", "strokeriskAnalysisSpecification.json") # From analysis specifications: name = "strokerisk"
)