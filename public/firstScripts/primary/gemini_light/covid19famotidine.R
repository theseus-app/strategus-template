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
library(ParallelLogger) # For saving the specifications

# Define the analysis name based on Analysis Specifications.
analysisName <- "covid19famotidine"

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
# Base URL for the WebAPI instance to retrieve cohort definitions and concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Retrieves cohort definitions from the WebAPI based on the provided IDs
# from 'cohortDefinitions' in Analysis Specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target cohort ID from 'cohortDefinitions.targetCohort.id'
    1794132, # Comparator cohort ID from 'cohortDefinitions.comparatorCohort.id'
    1794131  # Outcome cohort ID from 'cohortDefinitions.outcomeCohort.id'
  ),
  generateStats = TRUE # Always generate statistics for the cohorts
)

# Re-number cohorts for consistent internal study IDs
# This re-mapping is crucial for internal identification within the Strategus analysis.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Target cohort maps to ID 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Comparator cohort maps to ID 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Outcome cohort maps to ID 3


# Negative control outcomes
# Retrieves the concept set definition for negative controls using the ID
# from 'negativeControlConceptSet.id' in Analysis Specifications.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # From 'negativeControlConceptSet.id'
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet( # Resolves concept set to individual concepts
    baseUrl = baseUrl
  ) %>%
  ROhdsiWebApi::getConcepts( # Fetches concept details
    baseUrl = baseUrl
  ) %>%
  rename(outcomeConceptId = "conceptId", # Rename for clarity
         cohortName = "conceptName") %>%
  # Assign unique cohort IDs for negative controls, starting from 101
  # to avoid clashes with the re-numbered T/C/O cohorts (1, 2, 3).
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)


# Check for duplicate cohort IDs which would cause errors in analysis.
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts used in analyses ----------------------
# Outcomes: 
# Filters the cohortDefinitionSet for the re-numbered outcome cohort (ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # 'cleanWindow' is not specified in Analysis Specifications; using template's default.
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis 
# Uses re-numbered IDs (1 for Target, 2 for Comparator) and original names from JSON.
# Also stores original concept IDs for target and comparator for covariate exclusion.
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered internal ID for the target cohort
  targetCohortName = "target1", # From 'cohortDefinitions.targetCohort.name'
  comparatorCohortId = 2, # Re-numbered internal ID for the comparator cohort
  comparatorCohortName = "comparator1", # From 'cohortDefinitions.comparatorCohort.name'
  targetConceptId = 1794126, # Original concept ID for target, used for covariate exclusion
  comparatorConceptId = 1794132  # Original concept ID for comparator, used for covariate exclusion
)

# Concepts to exclude as covariates (beyond the target/comparator index concepts).
# Populated from 'covariateSelection.conceptsToExclude' in Analysis Specifications.
# The JSON specifies an empty array for this, so this data frame will be empty.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional: If you want to define specific covariates to include instead of all.
# Populated from 'covariateSelection.conceptsToInclude' in Analysis Specifications.
# The JSON specifies an empty array for this, so this data frame will be empty.
includedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# CohortGeneratorModule --------------------------------------------------------
# This module is responsible for generating the actual cohorts in the CDM and their statistics.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource for the set of all cohorts to be generated.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Shared resource for the negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Not specified in JSON, using template default
  detectOnDescendants = TRUE # Not specified in JSON, using template default
)

# Module-specific settings for the CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Always generate cohort statistics.
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# This module runs a suite of diagnostic analyses on the generated cohorts.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # Run diagnostics for all defined cohorts.
  runInclusionStatistics = TRUE, # Not specified in JSON, using template default
  runIncludedSourceConcepts = TRUE, # Not specified in JSON, using template default
  runOrphanConcepts = TRUE, # Not specified in JSON, using template default
  runTimeSeries = FALSE, # Not specified in JSON, using template default
  runVisitContext = TRUE, # Not specified in JSON, using template default
  runBreakdownIndexEvents = TRUE, # Not specified in JSON, using template default
  runIncidenceRate = TRUE, # Not specified in JSON, using template default
  runCohortRelationship = TRUE, # Not specified in JSON, using template default
  runTemporalCohortCharacterization = TRUE, # Not specified in JSON, using template default
  minCharacterizationMean = 0.01 # Not specified in JSON, using template default
)

# CohortMethodModule -----------------------------------------------------------

# Study periods for restricting the observation window.
# Populated from 'getDbCohortMethodDataArgs.studyPeriods' in Analysis Specifications.
studyPeriods <- tibble(
  studyStartDate = c("20200201"), # YYYYMMDD format
  studyEndDate   = c("20200530")  # YYYYMMDD format
)

# Time-at-risks (TARs) for the outcomes of interest.
# Populated from 'createStudyPopArgs.timeAtRisks' in Analysis Specifications.
timeAtRisks <- tibble(
  label = c("TAR 1-30 days from cohort start"), # Descriptive label for this TAR
  riskWindowStart  = c(1), # Start of the risk window
  startAnchor = c("cohort start"), # Anchor for the start of the risk window
  riskWindowEnd  = c(30), # End of the risk window
  endAnchor = c("cohort start"), # Anchor for the end of the risk window
  minDaysAtRisk = c(1) # Minimum number of days at risk for a subject (from createStudyPopArgs.minDaysAtRisk)
) 

# Propensity Score settings - match on PS
# Based on 'propensityScoreAdjustment.psSettings.matchOnPsArgs' from Analysis Specifications.
# In this case, 'matchOnPsArgs' is null, so this list will be empty.
matchOnPsArgsList <- tibble(
  label = c(),
  maxRatio  = c(),
  caliper = c(),
  caliperScale  = c() # "propensity score" | "standardized" | "standardized logit"
) 

# Propensity Score settings - stratify by PS
# Based on 'propensityScoreAdjustment.psSettings.stratifyByPsArgs' from Analysis Specifications.
# In this case, 'stratifyByPsArgs' is present.
stratifyByPsArgsList <- tibble(
  label = c("Stratify by PS (5 strata, base=all)"), # Descriptive label for PS stratification
  numberOfStrata  = c(5), # From 'propensityScoreAdjustment.psSettings.stratifyByPsArgs.numberOfStrata'
  baseSelection = c("all") # From 'propensityScoreAdjustment.psSettings.stratifyByPsArgs.baseSelection'
) 

# Build a single PS configuration list (each entry has: method, label, params)
# This list will contain all PS adjustment methods to be tested.
psConfigList <- list()

# If 'matchOnPsArgsList' has rows, convert each row to a PS configuration.
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "match", # Identifies the PS adjustment method
      label  = matchOnPsArgsList$label[i], # Human-readable label
      params = list( # Parameters for createMatchOnPsArgs
        maxRatio     = matchOnPsArgsList$maxRatio[i],
        caliper      = matchOnPsArgsList$caliper[i],
        caliperScale = matchOnPsArgsList$caliperScale[i]
      )
    )
  }
}

# If 'stratifyByPsArgsList' has rows, convert each row to a PS configuration.
if (exists("stratifyByPsArgsList") && nrow(stratifyByPsArgsList) > 0) {
  for (i in seq_len(nrow(stratifyByPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "stratify", # Identifies the PS adjustment method
      label  = stratifyByPsArgsList$label[i], # Human-readable label
      params = list( # Parameters for createStratifyByPsArgs
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection  = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}


# Iterate through all analysis setting combinations (study periods, TARs, PS methods)
# to create multiple CmAnalysis objects.
cmAnalysisList <- list()
analysisId <- 1 # Initialize a counter for unique analysis IDs

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Determine PS adjustment method based on the current configuration.
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Not specified in JSON, using template default
          stratificationColumns = c() # Not specified in JSON, using template default
        )
        stratifyByPsArgs <- NULL # Only one PS adjustment method can be active.
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL # Only one PS adjustment method can be active.
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Not specified in JSON, using template default
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Default covariate settings for feature extraction.
      # 'addDescendantsToExclude = TRUE' ensures that descendants of any excluded concepts are also excluded.
      # No specific covariate settings from 'covariateSelection' in JSON besides the target/comparator concepts.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Combine study outcomes and negative control outcomes into a single list.
      outcomeList <- append(
        # Study outcomes (from oList, which contains the re-numbered outcome 3).
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE, # Mark as an outcome of interest
            trueEffectSize = NA, # True effect size is unknown for study outcomes
            priorOutcomeLookback = 99999 # Broad lookback, actual will be set by createStudyPopArgs
          )
        }),
        # Negative control outcomes (from negativeControlOutcomeCohortSet).
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE, # Negative controls are not outcomes of interest
            trueEffectSize = 1 # True effect size is assumed to be 1 (null hypothesis)
          )
        })
      )
      
      # Define target-comparator-outcome combinations.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i], # Re-numbered target cohort ID
          comparatorId = cmTcList$comparatorCohortId[i], # Re-numbered comparator cohort ID
          outcomes = outcomeList, # All outcomes (study + negative controls)
          # Exclude the original concept IDs of the target and comparator drugs
          # from covariate analysis to prevent confounding by indication bias.
          # 'excludedCovariateConcepts$conceptId' is empty based on the JSON.
          excludedCovariateConceptIds = c(
            cmTcList$targetConceptId[i], 
            cmTcList$comparatorConceptId[i],
            excludedCovariateConcepts$conceptId
          )
          # If 'includedCovariateConcepts' were defined, they would be passed here.
        )
      }

      # Settings for fetching cohort method data from the database.
      # Populated from 'getDbCohortMethodDataArgs' in Analysis Specifications.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Set to TRUE because studyStartDate/EndDate are provided.
        studyStartDate = studyStartDate, # From the current study period loop iteration.
        studyEndDate = studyEndDate, # From the current study period loop iteration.
        maxCohortSize = 0, # From 'maxCohortSize'. 0 means no restriction on cohort size.
        covariateSettings = covariateSettings # Uses the default covariate settings.
      )

      # Settings for creating propensity scores.
      # Populated from 'propensityScoreAdjustment.createPsArgs' in Analysis Specifications.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From 'maxCohortSizeForFitting'
        errorOnHighCorrelation = TRUE, # From 'errorOnHighCorrelation'
        stopOnError = FALSE, # Set to FALSE to allow Strategus to complete all CM operations.
                             # If PS model fails to fit, equipoise diagnostic should fail. (Template default)
        estimator = "att", # Not specified in JSON, using template default
        prior = Cyclops::createPrior( # Prior distribution for regularization in PS model.
          priorType = "laplace", # From 'prior.priorType'
          exclude = c(0), # Exclude the intercept from regularization (template default).
          useCrossValidation = TRUE # From 'prior.useCrossValidation'
        ),
        control = Cyclops::createControl( # Control settings for the Cyclops optimizer.
          noiseLevel = "silent", # From 'control.noiseLevel'
          cvType = "auto", # From 'control.cvType'
          seed = 1, # Not specified in JSON, using template default.
          resetCoefficients = TRUE, # From 'control.resetCoefficients'
          tolerance = 2e-07, # From 'control.tolerance'
          cvRepetitions = 10, # From 'control.cvRepetitions' (JSON value)
          startingVariance = 0.01 # From 'control.startingVariance'
        )
      )

      # Settings for computing shared covariate balance.
      # Not specified in JSON; using template defaults.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      # Settings for computing covariate balance.
      # Not specified in JSON; using template defaults.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Settings for fitting the outcome model.
      # Populated from 'fitOutcomeModelArgs' in Analysis Specifications.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From 'modelType' (e.g., "cox", "logistic", "poisson")
        stratified = TRUE, # From 'stratified' (usually TRUE with PS stratification/matching)
        useCovariates = FALSE, # From 'useCovariates' (often FALSE when PS adjustment is used)
        inversePtWeighting = FALSE, # From 'inversePtWeighting'
        prior = Cyclops::createPrior( # Prior distribution for regularization in outcome model.
          priorType = "laplace", # From 'prior.priorType'
          useCrossValidation = TRUE # From 'prior.useCrossValidation'
        ),
        control = Cyclops::createControl( # Control settings for the Cyclops optimizer.
          cvType = "auto", # From 'control.cvType'
          seed = 1, # Not specified in JSON, using template default.
          resetCoefficients = TRUE, # From 'control.resetCoefficients'
          startingVariance = 0.01, # From 'control.startingVariance'
          tolerance = 2e-07, # From 'control.tolerance'
          cvRepetitions = 10, # From 'control.cvRepetitions' (JSON value)
          noiseLevel = "quiet" # From 'control.noiseLevel'
        )
      )
      
      # Settings for creating the study population.
      # Populated from 'createStudyPopArgs' in Analysis Specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From 'restrictToCommonPeriod'
        firstExposureOnly = FALSE, # From 'firstExposureOnly'
        washoutPeriod = 0, # From 'washoutPeriod'
        removeDuplicateSubjects = "keep all", # From 'removeDuplicateSubjects' (JSON value: "keep all")
        censorAtNewRiskWindow = FALSE, # From 'censorAtNewRiskWindow' (JSON value: FALSE)
        removeSubjectsWithPriorOutcome = TRUE, # From 'removeSubjectsWithPriorOutcome'
        priorOutcomeLookback = 30, # From 'priorOutcomeLookBack' (JSON value: 30 days)
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From current TAR iteration
        startAnchor = timeAtRisks$startAnchor[t], # From current TAR iteration
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From current TAR iteration
        endAnchor = timeAtRisks$endAnchor[t], # From current TAR iteration
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From current TAR iteration
        maxDaysAtRisk = 99999 # Not specified in JSON, using template default for unlimited.
      )

      # Append the current set of analysis settings to the list of CohortMethod analyses.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId, # Unique ID for this specific analysis.
        description = sprintf( # Descriptive string for this analysis.
          "Study Dates: %s-%s; TAR: %s; PS Adjustment: %s",
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
      analysisId <- analysisId + 1 # Increment analysis ID for the next iteration.
    }
  }
}

# CohortMethodModule settings for Strategus.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList, # List of all defined CmAnalysis objects.
  targetComparatorOutcomesList = targetComparatorOutcomesList, # List of TCO combinations.
  analysesToExclude = NULL, # Not specified in JSON, using template default.
  refitPsForEveryOutcome = FALSE, # Not specified in JSON, using template default.
  refitPsForEveryStudyPopulation = FALSE, # Not specified in JSON, using template default.
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Not specified in JSON, using template default.
)

# Create the final analysis specifications for Strategus ----------------------
# Initializes an empty analysis specification object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Adds shared resources (cohort definitions and negative controls).
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  # Adds the specifications for each Strategus module.
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path is constructed using the 'analysisName'.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", analysisName, paste0(analysisName, "AnalysisSpecification.json"))
)