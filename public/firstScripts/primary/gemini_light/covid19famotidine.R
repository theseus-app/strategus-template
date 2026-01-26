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
# Base URL for the ATLAS/WebAPI instance to retrieve cohort definitions and concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions from WebAPI based on provided IDs.
# These IDs correspond to the target, comparator, and outcome cohorts specified in Analysis Specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to a simplified scheme (1, 2, 3...) for internal use in the study.
# This maps the original WebAPI IDs to generic study IDs.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Target cohort re-numbered to 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Comparator cohort re-numbered to 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Outcome cohort re-numbered to 3

# Negative control outcomes
# Retrieve the concept set definition for negative controls from WebAPI.
# The conceptSetId (1888110) is from the "negativeControlConceptSet" in Analysis Specifications.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Negative control concept set ID
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
  # Assign unique cohort IDs for negative controls, starting after the main cohorts.
  # Main cohorts (target, comparator, outcome) are 1, 2, 3. Negative controls start from 101.
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs to prevent conflicts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Filter for the re-numbered outcome cohort (ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not specified in analysis spec.

# Target and Comparator for the CohortMethod analysis 
# Use the re-numbered target (1) and comparator (2) cohort IDs and names.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study. The 'covariateSelection.conceptsToExclude' in Analysis Specifications is empty.
# Therefore, this data frame will be empty.
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(0),
  conceptName = character(0)
)

# The 'covariateSelection.conceptsToInclude' in Analysis Specifications is empty.
# Therefore, the optional block for includedCovariateConcepts is not needed.

# CohortGeneratorModule --------------------------------------------------------
# Initializes the CohortGeneratorModule settings creator.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Creates shared resource specifications for the defined cohorts.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Creates shared resource specifications for negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default, not specified in analysis spec.
  detectOnDescendants = TRUE # Default, not specified in analysis spec.
)
# Creates module specifications for the CohortGenerator module.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Generate cohort statistics.
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# Initializes the CohortDiagnosticsModule settings creator.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Creates module specifications for the CohortDiagnostics module.
# Runs diagnostics for all cohorts defined in cohortDefinitionSet.
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
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

# Study periods for restricting the data.
# From 'getDbCohortMethodDataArgs.studyPeriods' in Analysis Specifications.
studyPeriods <- tibble(
  studyStartDate = c("20200201"), # YYYYMMDD
  studyEndDate   = c("20200530")  # YYYYMMDD
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# From 'createStudyPopArgs.timeAtRisks' in Analysis Specifications.
timeAtRisks <- tibble(
  label = c("TAR 1-30 days"), # Custom label for description
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd  = c(30),
  endAnchor = c("cohort start") # "cohort start" | "cohort end"
) 

# Propensity Score settings - match on PS
# 'propensityScoreAdjustment.psSettings.matchOnPsArgs' is null in Analysis Specifications.
# Therefore, this tibble will be empty.
matchOnPsArgsList <- tibble(
  label = character(0),
  maxRatio  = numeric(0),
  caliper = numeric(0),
  caliperScale  = character(0) # "propensity score" | "standardized" | "standardized logit"
) 

# Propensity Score settings - stratify by PS
# From 'propensityScoreAdjustment.psSettings.stratifyByPsArgs' in Analysis Specifications.
stratifyByPsArgsList <- tibble(
  label = c("Stratify 5 strata"), # Custom label for description
  numberOfStrata  = c(5),
  baseSelection = c("all") # "all" | "target" | "comparator"
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
      
      # Determine PS adjustment method based on psConfigList
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
      # 'covariateSelection' in Analysis Specifications is empty, so using default settings.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default, not specified in analysis spec.
      )

      # Define outcomes for CohortMethod.
      # Includes the main outcome from oList and all negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not applicable for observed outcomes.
            priorOutcomeLookback = 99999 # Default, used for outcome definition, not study population.
          )
        }),
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
          # Excluded covariate concept IDs.
          # 'covariateSelection.conceptsToExclude' is empty in Analysis Specifications.
          # The template's original `cmTcList$targetConceptId[i]` and `cmTcList$comparatorConceptId[i]`
          # are removed as they refer to drug concept IDs not provided in the spec,
          # and `excludedCovariateConcepts` is empty.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId 
        )
      }

      # Arguments for fetching cohort method data from the database.
      # Settings from 'getDbCohortMethodDataArgs' in Analysis Specifications.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Default, not specified in analysis spec.
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From Analysis Specifications.
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # Settings from 'propensityScoreAdjustment.createPsArgs' in Analysis Specifications.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications.
        errorOnHighCorrelation = TRUE, # From Analysis Specifications.
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att", # Default, not specified in analysis spec.
        prior = Cyclops::createPrior( # Prior settings from Analysis Specifications.
          priorType = "laplace", 
          exclude = c(0), # Default, not specified in analysis spec.
          useCrossValidation = TRUE # From Analysis Specifications.
        ),
        control = Cyclops::createControl( # Control settings from Analysis Specifications.
          noiseLevel = "silent", # From Analysis Specifications.
          cvType = "auto", # From Analysis Specifications.
          seed = 1, # Default, not specified in analysis spec.
          resetCoefficients = TRUE, # From Analysis Specifications.
          tolerance = 2e-07, # From Analysis Specifications.
          cvRepetitions = 10, # From Analysis Specifications (mapped from 'fold' and 'cvRepetitions').
          startingVariance = 0.01 # From Analysis Specifications.
        )
      )

      # Arguments for computing shared covariate balance.
      # Using default settings as not specified in analysis spec.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default, not specified in analysis spec.
        covariateFilter = NULL # Default, not specified in analysis spec.
      )
      # Arguments for computing covariate balance.
      # Using default settings as not specified in analysis spec.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default, not specified in analysis spec.
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default, not specified in analysis spec.
      )

      # Arguments for fitting the outcome model.
      # Settings from 'fitOutcomeModelArgs' in Analysis Specifications.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From Analysis Specifications.
        stratified = TRUE, # From Analysis Specifications.
        useCovariates = FALSE, # From Analysis Specifications.
        inversePtWeighting = FALSE, # From Analysis Specifications.
        prior = Cyclops::createPrior( # Prior settings from Analysis Specifications.
          priorType = "laplace", 
          useCrossValidation = TRUE # From Analysis Specifications.
        ),
        control = Cyclops::createControl( # Control settings from Analysis Specifications.
          cvType = "auto", # From Analysis Specifications.
          seed = 1, # Default, not specified in analysis spec.
          resetCoefficients = TRUE, # From Analysis Specifications.
          startingVariance = 0.01, # From Analysis Specifications.
          tolerance = 2e-07, # From Analysis Specifications.
          cvRepetitions = 10, # From Analysis Specifications (mapped from 'fold' and 'cvRepetitions').
          noiseLevel = "quiet" # From Analysis Specifications.
        )
      )
      
      # Arguments for creating the study population.
      # Settings from 'createStudyPopArgs' in Analysis Specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications.
        firstExposureOnly = FALSE, # From Analysis Specifications.
        washoutPeriod = 0, # From Analysis Specifications.
        removeDuplicateSubjects = "keep all", # From Analysis Specifications.
        censorAtNewRiskWindow = FALSE, # From Analysis Specifications.
        removeSubjectsWithPriorOutcome = TRUE, # From Analysis Specifications.
        priorOutcomeLookback = 30, # From Analysis Specifications.
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1, # From Analysis Specifications.
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

# Initializes the CohortMethodModule settings creator.
cmModuleSettingsCreator <- CohortMethodModule$new()
# Creates module specifications for the CohortMethod module.
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Default, not specified in analysis spec.
  refitPsForEveryOutcome = FALSE, # Default, not specified in analysis spec.
  refitPsForEveryStudyPopulation = FALSE, # Default, not specified in analysis spec.
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default, not specified in analysis spec.
)

# Create the analysis specifications ------------------------------------------
# Initializes an empty analysis specifications object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Adds shared resources (cohort definitions, negative controls) to the specifications.
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  # Adds module specifications for CohortGenerator, CohortDiagnostics, and CohortMethod.
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Saves the complete analysis specifications to a JSON file.
# The file path uses the study name "covid19famotidine" from Analysis Specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "covid19famotidine", "covid19famotidineAnalysisSpecification.json")
)