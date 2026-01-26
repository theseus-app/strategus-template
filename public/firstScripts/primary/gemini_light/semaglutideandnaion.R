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
# Base URL for the WebAPI instance to retrieve cohort definitions.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Retrieve cohort definitions from WebAPI based on IDs provided in Analysis Specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1 (from Analysis Specifications)
    1794132, # Comparator: comparator1 (from Analysis Specifications)
    1794131  # Outcome: outcome1 (from Analysis Specifications)
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal use within Strategus modules.
# Target, Comparator, and Outcome cohorts are typically re-numbered to 1, 2, and 3 respectively.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Renumber Target cohort
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Renumber Comparator cohort
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Renumber Outcome cohort

# Negative control outcomes
# Retrieve negative control concept set definition from WebAPI.
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
  # Assign unique cohort IDs for negative controls, starting from 101 to avoid conflicts with T/C/O.
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs to prevent errors in Strategus.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes list: Filters for the re-numbered outcome cohort (ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Outcome cohort is re-numbered to 3
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not specified in Analysis Specifications.

# Target and Comparator for the CohortMethod analysis.
# Uses the re-numbered cohort IDs and names from Analysis Specifications.
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered target cohort ID
  targetCohortName = "target1", # From Analysis Specifications: targetCohort.name
  comparatorCohortId = 2, # Re-numbered comparator cohort ID
  comparatorCohortName = "comparator1" # From Analysis Specifications: comparatorCohort.name
)

# For the CohortMethod LSPS, we might need to exclude specific concepts.
# Analysis Specifications has empty `conceptsToExclude`, so this will be an empty data frame.
excludedCovariateConcepts <- data.frame(
  conceptId = c(), # From Analysis Specifications: covariateSelection.conceptsToExclude (empty)
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all
# Analysis Specifications has empty `conceptsToInclude`, so this remains commented out.
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
  cohortIds = c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId), # Include all defined cohorts (T, C, O, and negative controls)
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

# Study periods for restricting data retrieval.
# From Analysis Specifications: getDbCohortMethodDataArgs.studyPeriods
studyPeriods <- tibble(
  studyStartDate = c(20171201), # YYYYMMDD format
  studyEndDate   = c(20231231)  # YYYYMMDD format
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# From Analysis Specifications: createStudyPopArgs.timeAtRisks
timeAtRisks <- tibble(
  label = c("TAR1"), # Custom label for this time-at-risk setting
  riskWindowStart  = c(1), # From Analysis Specifications
  startAnchor = c("cohort start"), # From Analysis Specifications
  riskWindowEnd  = c(0), # From Analysis Specifications
  endAnchor = c("cohort end") # From Analysis Specifications
) 

# Propensity Score settings - match on PS.
# From Analysis Specifications: propensityScoreAdjustment.psSettings[0].matchOnPsArgs
matchOnPsArgsList <- tibble(
  label = c("MatchOnPs1"), # Custom label for this PS matching setting
  maxRatio  = c(1), # From Analysis Specifications
  caliper = c(0.2), # From Analysis Specifications
  caliperScale  = c("standardized logit") # From Analysis Specifications
) 

# Propensity Score settings - stratify by PS.
# Analysis Specifications: propensityScoreAdjustment.psSettings[0].stratifyByPsArgs is null, so this list is empty.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c(),
) 

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# If a data frame for "match on PS" exists and has rows, convert each row to a config.
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
      
      # Configure PS adjustment arguments based on the current PS configuration.
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default, not specified in Analysis Specifications.
          stratificationColumns = c() # Default, not specified in Analysis Specifications.
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default, not specified in Analysis Specifications.
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings for feature extraction.
      # Analysis Specifications has empty `conceptsToInclude` and `conceptsToExclude`,
      # so using default covariate settings.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default, not specified in Analysis Specifications.
      )

      # Create a list of outcomes, including the outcome of interest and negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not a simulated outcome
            priorOutcomeLookback = 99999 # From Analysis Specifications: createStudyPopArgs.priorOutcomeLookBack
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # For negative controls, true effect size is assumed to be 1 (no effect)
          )
        })
      )
      
      # Create target-comparator-outcomes list for CohortMethod.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Excluded covariate concept IDs.
          # The template had placeholders for target/comparator concept IDs which are not in the spec.
          # Only include general `excludedCovariateConcepts` if they exist (which is an empty list in this case).
          excludedCovariateConceptIds = if (nrow(excludedCovariateConcepts) > 0) excludedCovariateConcepts$conceptId else c()
        )
      }

      # Arguments for retrieving cohort method data from the database.
      # From Analysis Specifications: getDbCohortMethodDataArgs
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Default, not specified in Analysis Specifications.
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From Analysis Specifications
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # From Analysis Specifications: propensityScoreAdjustment.createPsArgs
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications
        errorOnHighCorrelation = TRUE, # From Analysis Specifications
        stopOnError = FALSE, # Default, not specified in Analysis Specifications.
        estimator = "att", # Default, not specified in Analysis Specifications.
        prior = Cyclops::createPrior(
          priorType = "laplace", # From Analysis Specifications: prior.priorType
          exclude = c(0), # Default, not specified in Analysis Specifications.
          useCrossValidation = TRUE # From Analysis Specifications: prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", # From Analysis Specifications: control.noiseLevel
          cvType = "auto", # From Analysis Specifications: control.cvType
          seed = 1, # Default, not specified in Analysis Specifications.
          resetCoefficients = TRUE, # From Analysis Specifications: control.resetCoefficients
          tolerance = 2e-07, # From Analysis Specifications: control.tolerance
          cvRepetitions = 10, # From Analysis Specifications: control.cvRepetitions
          startingVariance = 0.01 # From Analysis Specifications: control.startingVariance
        )
      )

      # Arguments for computing shared covariate balance.
      # Defaults used as not specified in Analysis Specifications.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      # Arguments for computing covariate balance.
      # Defaults used as not specified in Analysis Specifications.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting the outcome model.
      # From Analysis Specifications: fitOutcomeModelArgs
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From Analysis Specifications
        stratified = FALSE, # From Analysis Specifications
        useCovariates = FALSE, # From Analysis Specifications
        inversePtWeighting = FALSE, # From Analysis Specifications
        prior = Cyclops::createPrior(
          priorType = "laplace", # From Analysis Specifications: prior.priorType
          useCrossValidation = TRUE # From Analysis Specifications: prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          cvType = "auto", # From Analysis Specifications: control.cvType
          seed = 1, # Default, not specified in Analysis Specifications.
          resetCoefficients = TRUE, # From Analysis Specifications: control.resetCoefficients
          startingVariance = 0.01, # From Analysis Specifications: control.startingVariance
          tolerance = 2e-07, # From Analysis Specifications: control.tolerance
          cvRepetitions = 10, # From Analysis Specifications: control.cvRepetitions
          noiseLevel = "quiet" # From Analysis Specifications: control.noiseLevel
        )
      )
      
      # Arguments for creating the study population.
      # From Analysis Specifications: createStudyPopArgs
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications
        firstExposureOnly = FALSE, # From Analysis Specifications
        washoutPeriod = 365, # From Analysis Specifications
        removeDuplicateSubjects = "keep all", # From Analysis Specifications
        censorAtNewRiskWindow = FALSE, # From Analysis Specifications
        removeSubjectsWithPriorOutcome = TRUE, # From Analysis Specifications
        priorOutcomeLookback = 99999, # From Analysis Specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From current time-at-risk setting
        startAnchor = timeAtRisks$startAnchor[t], # From current time-at-risk setting
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From current time-at-risk setting
        endAnchor = timeAtRisks$endAnchor[t], # From current time-at-risk setting
        minDaysAtRisk = 1, # From Analysis Specifications
        maxDaysAtRisk = 99999 # Default, not specified in Analysis Specifications.
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
# Create module specifications for CohortMethod.
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Default, not specified in Analysis Specifications.
  refitPsForEveryOutcome = FALSE, # Default, not specified in Analysis Specifications.
  refitPsForEveryStudyPopulation = FALSE, # Default, not specified in Analysis Specifications.
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default, not specified in Analysis Specifications.
)

# Create the analysis specifications ------------------------------------------
# Initialize an empty analysis specifications object and add shared resources and module specifications.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path uses the study name from Analysis Specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "semaglutideandnaion", "semaglutideandnaionAnalysisSpecification.json")
)