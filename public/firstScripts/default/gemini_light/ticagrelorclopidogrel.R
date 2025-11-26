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
library(ROhdsiWebApi) # Required for fetching cohort definitions and concept sets
library(CohortMethod) # Required for CohortMethod specific functions
library(FeatureExtraction) # Required for covariate settings
library(Cyclops) # Required for prior and control settings in PS and Outcome models

# Shared Resources -------------------------------------------------------------
# Define the base URL for the WebAPI instance to fetch cohort definitions and concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Fetch cohort definitions based on IDs from Analysis Specifications.
# The template assigns internal IDs 1, 2, 3 to target, comparator, and outcome respectively.
# targetCohort: id = 1794126, name = "target1" -> internal ID 1
# comparatorCohort: id = 1794132, name = "comparator1" -> internal ID 2
# outcomeCohort: id = 1794131, name = "outcome1" -> internal ID 3
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131 # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal use within Strategus modules (1: Target, 2: Comparator, 3: Outcome).
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortName <- "target1" # Ensure names are also correct
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortName <- "comparator1"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortName <- "outcome1"


# Negative control outcomes
# Fetch the concept set definition for negative controls.
# negativeControlConceptSet: id = 1888110, name = "negative"
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # From Analysis Specifications -> negativeControlConceptSet -> id
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
  # Assign unique cohort IDs for negative controls, starting from 101 to avoid
  # clashes with target, comparator, and outcome cohorts (1, 2, 3).
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs across all defined cohorts to prevent errors.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts used in each analysis ----------------
# Outcome cohorts: Filter for the primary outcome (internal ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window from template, not specified in analysis spec.

# Target and Comparator for the CohortMethod analysis.
cmTcList <- data.frame(
  targetCohortId = 1, # Internal ID for target1
  targetCohortName = "target1", # Name from Analysis Specifications
  comparatorCohortId = 2, # Internal ID for comparator1
  comparatorCohortName = "comparator1" # Name from Analysis Specifications
)

# Covariate exclusion: For the CohortMethod analysis, specific covariates can be excluded.
# Analysis Specifications -> covariateSelection -> conceptsToExclude is empty,
# so this data frame will be empty.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all.
# Analysis Specifications -> covariateSelection -> conceptsToInclude is empty,
# so this data frame will also be empty.
includedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# CohortGeneratorModule --------------------------------------------------------
# This module generates the cohorts based on the definitions provided.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource for primary cohorts (target, comparator, outcome).
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Shared resource for negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default from template, not specified in analysis spec.
  detectOnDescendants = TRUE # Default from template, not specified in analysis spec.
)

# Module specifications for CohortGenerator, including option to generate statistics.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # From template, not explicitly overridden in analysis spec.
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# This module runs diagnostics on the generated cohorts.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # Run diagnostics for all primary cohorts.
  runInclusionStatistics = TRUE, # From template.
  runIncludedSourceConcepts = TRUE, # From template.
  runOrphanConcepts = TRUE, # From template.
  runTimeSeries = FALSE, # From template.
  runVisitContext = TRUE, # From template.
  runBreakdownIndexEvents = TRUE, # From template.
  runIncidenceRate = TRUE, # From template.
  runCohortRelationship = TRUE, # From template.
  runTemporalCohortCharacterization = TRUE, # From template.
  minCharacterizationMean = 0.01 # From template.
)

# CohortMethodModule -----------------------------------------------------------
# This module performs the comparative effectiveness analysis using CohortMethod.

# Study Periods: Define the start and end dates for the study from
# Analysis Specifications -> getDbCohortMethodDataArgs -> studyPeriods.
studyPeriods <- tibble(
  studyStartDate = c("20111101", "20130301"), # YYYYMMDD format
  studyEndDate   = c("20190331", "20161231") # YYYYMMDD format
)

# Time-at-risks (TARs) for the outcomes of interest from
# Analysis Specifications -> createStudyPopArgs -> timeAtRisks.
timeAtRisks <- tibble(
  label = c(
    "1-365 days after cohort start",
    "1-1825 days after cohort start",
    "1 day after cohort start - cohort end",
    "29-365 days after cohort start",
    "29-1825 days after cohort start",
    "29 days after cohort start - cohort end"
  ),
  riskWindowStart  = c(1, 1, 1, 29, 29, 29),
  startAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd  = c(365, 1825, 0, 365, 1825, 0),
  endAnchor = c("cohort start", "cohort start", "cohort end", "cohort start", "cohort start", "cohort end"),
  minDaysAtRisk = c(1, 1, 1, 1, 1, 1) # From Analysis Specifications -> createStudyPopArgs -> timeAtRisks
)

# Propensity Score (PS) settings - match on PS.
# From Analysis Specifications -> propensityScoreAdjustment -> psSettings where matchOnPsArgs is not null.
matchOnPsArgsList <- tibble(
  label = c("Match 1:1, Caliper 0.2", "Match 1:10, Caliper 0.2"),
  maxRatio  = c(1, 10),
  caliper = c(0.2, 0.2),
  caliperScale  = c("standardized logit", "standardized logit") # From Analysis Specifications
)

# Propensity Score (PS) settings - stratify by PS.
# From Analysis Specifications -> propensityScoreAdjustment -> psSettings where stratifyByPsArgs is not null.
stratifyByPsArgsList <- tibble(
  label = c("Stratify by 10 strata"),
  numberOfStrata  = c(10),
  baseSelection = c("all") # From Analysis Specifications
)

# Build a single PS configuration list (each entry has: method, label, params).
# This structure allows iterating through different PS adjustment strategies.
psConfigList <- list()

# Convert "match on PS" settings from tibble to list of configurations.
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "match",
      label  = matchOnPsArgsList$label[i],
      params = list(
        maxRatio     = matchOnPsArgsList$maxRatio[i],
        caliper      = matchOnPsArgsList$caliper[i],
        caliperScale = matchOnPsArgsList$caliperScale[i]
      )
    )
  }
}

# Convert "stratify by PS" settings from tibble to list of configurations.
if (exists("stratifyByPsArgsList") && nrow(stratifyByPsArgsList) > 0) {
  for (i in seq_len(nrow(stratifyByPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "stratify",
      label  = stratifyByPsArgsList$label[i],
      params = list(
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection  = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}


# Iterate through all analysis setting combinations:
# Study periods x Time-at-risks x Propensity Score adjustment methods.
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {
    
    # Extract minDaysAtRisk from the current timeAtRisks entry
    currentMinDaysAtRisk <- timeAtRisks$minDaysAtRisk[t]

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Determine PS adjustment method (matching or stratification) and create corresponding args.
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default from template
          stratificationColumns = c() # Default from template
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default from template
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings: Uses default settings for covariate extraction.
      # Analysis Specifications -> covariateSelection is empty, so no specific inclusions/exclusions here.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default from template
      )

      # Combine primary outcome and negative control outcomes into a single list.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # For true outcomes, effect size is unknown.
            priorOutcomeLookback = 99999 # Default from template
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
      
      # Prepare Target-Comparator-Outcome combinations.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        
        # Define specific covariate concepts to exclude from the model.
        # Based on Analysis Specifications -> covariateSelection -> conceptsToExclude, this list is empty.
        # The template also contained placeholders for target and comparator drug concept IDs,
        # which are not provided in the current Analysis Specifications.
        # Therefore, no specific covariate concept IDs will be excluded beyond the default
        # behavior of createDefaultCovariateSettings.
        excludedConceptsForCmAnalysis <- c() 

        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedConceptsForCmAnalysis # This will be an empty vector c()
        )
      }

      # Arguments for fetching data from the CDM database.
      # Settings from Analysis Specifications -> getDbCohortMethodDataArgs.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate, # From current loop iteration
        studyEndDate = studyEndDate, # From current loop iteration
        restrictToCommonPeriod = TRUE, # From Analysis Specifications
        maxCohortSize = 0, # From Analysis Specifications (0 means no restriction)
        firstExposureOnly = FALSE, # From Analysis Specifications
        washoutPeriod = 0, # From Analysis Specifications
        removeDuplicateSubjects = "keep first", # From Analysis Specifications
        covariateSettings = covariateSettings
      )

      # Arguments for creating the Propensity Score model.
      # Settings from Analysis Specifications -> propensityScoreAdjustment -> createPsArgs.
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications
        errorOnHighCorrelation = TRUE, # From Analysis Specifications
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail (from template)
        estimator = "att", # Default from template, not specified in analysis spec.
        prior = Cyclops::createPrior(
          priorType = "laplace", # From Analysis Specifications
          exclude = c(0), # Default from template (for intercept), not in analysis spec.
          useCrossValidation = TRUE # From Analysis Specifications
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", # From Analysis Specifications
          cvType = "auto", # From Analysis Specifications
          seed = 1, # Default from template (for reproducibility), not in analysis spec.
          resetCoefficients = TRUE, # From Analysis Specifications
          tolerance = 2e-07, # From Analysis Specifications
          cvRepetitions = 10, # From Analysis Specifications (template was 1)
          startingVariance = 0.01, # From Analysis Specifications
          fold = 10 # From Analysis Specifications (template didn't specify fold, derived from cvType)
        )
      )

      # Arguments for computing covariate balance before PS adjustment (shared).
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default from template
        covariateFilter = NULL # Default from template
      )
      
      # Arguments for computing covariate balance after PS adjustment.
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default from template
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default from template
      )

      # Arguments for fitting the outcome model.
      # Settings from Analysis Specifications -> fitOutcomeModelArgs.
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From Analysis Specifications
        stratified = TRUE, # From Analysis Specifications
        useCovariates = FALSE, # From Analysis Specifications
        inversePtWeighting = FALSE, # From Analysis Specifications
        prior = Cyclops::createPrior(
          priorType = "laplace", # From Analysis Specifications
          useCrossValidation = TRUE # From Analysis Specifications
        ),
        control = Cyclops::createControl(
          cvType = "auto", # From Analysis Specifications
          seed = 1, # Default from template (for reproducibility), not in analysis spec.
          resetCoefficients = TRUE, # From Analysis Specifications
          startingVariance = 0.01, # From Analysis Specifications
          tolerance = 2e-07, # From Analysis Specifications
          cvRepetitions = 10, # From Analysis Specifications (template was 1)
          noiseLevel = "quiet", # From Analysis Specifications
          fold = 10 # From Analysis Specifications (template didn't specify fold, derived from cvType)
        )
      )
      
      # Arguments for creating the study population.
      # Settings from Analysis Specifications -> createStudyPopArgs.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications
        firstExposureOnly = FALSE, # From Analysis Specifications
        washoutPeriod = 0, # From Analysis Specifications
        removeDuplicateSubjects = "keep all", # From Analysis Specifications
        censorAtNewRiskWindow = FALSE, # From Analysis Specifications
        removeSubjectsWithPriorOutcome = FALSE, # From Analysis Specifications
        priorOutcomeLookback = 99999, # From Analysis Specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From current loop iteration
        startAnchor = timeAtRisks$startAnchor[t], # From current loop iteration
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From current loop iteration
        endAnchor = timeAtRisks$endAnchor[t], # From current loop iteration
        minDaysAtRisk = currentMinDaysAtRisk, # From current loop iteration
        maxDaysAtRisk = 99999 # Default from template, not in analysis spec.
      )


      # Append the specific analysis settings to the CohortMethod analysis list.
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

# CohortMethod module specifications for Strategus.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList, # List of all CohortMethod analyses to perform.
  targetComparatorOutcomesList = targetComparatorOutcomesList, # List of T/C/O combinations.
  analysesToExclude = NULL, # No specific analyses excluded (from template).
  refitPsForEveryOutcome = FALSE, # Default from template, not specified in analysis spec.
  refitPsForEveryStudyPopulation = FALSE, # Default from template, not specified in analysis spec.
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default thresholds for diagnostics.
)

# Create the overall analysis specifications for Strategus -------------------
# This combines all shared resources and module specifications.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the complete analysis specifications to a JSON file.
# The filename uses the "name" from Analysis Specifications ("ticagrelorclopidogrel").
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ticagrelorclopidogrel", "ticagrelorclopidogrelAnalysisSpecification.json")
)