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
library(ParallelLogger) # Required for saveSettingsToJson

# Shared Resources -------------------------------------------------------------
# Base URL for the WebAPI to retrieve cohort definitions and concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Retrieve cohort definitions from WebAPI based on IDs provided in Analysis Specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: sglt2imetformin.target1
    1794132, # Comparator: sglt2imetformin.comparator1
    1794131  # Outcome: sglt2imetformin.outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to standard IDs (1 for target, 2 for comparator, 3 for outcome)
# This simplifies referencing them in the analysis and aligns with the template structure.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Update cohort names for clarity after re-numbering, using names from Analysis Specifications.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1,]$cohortName <- "target1"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 2,]$cohortName <- "comparator1"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 3,]$cohortName <- "outcome1"


# Negative control outcomes
# Retrieve negative control concept set from WebAPI based on ID in Analysis Specifications.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # negativeControlConceptSet.id from Analysis Specifications
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
  # Assign unique cohort IDs for negative controls, starting from 101 to avoid conflicts
  # with target/comparator/outcome cohorts (1, 2, 3).
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)


# Check for duplicate cohort IDs to prevent errors in Strategus.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Filter for the main outcome cohort (re-numbered to ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not specified in analysis specs

# Target and Comparator for the CohortMethod analysis 
# Use the re-numbered target (1) and comparator (2) cohort IDs and names from Analysis Specifications.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# For the CohortMethod LSPS (Large Scale Propensity Score) we'll need to exclude
# specific concepts from covariate generation.
# The analysis specifications (covariateSelection.conceptsToExclude) did not provide
# additional concepts to exclude beyond the target and comparator cohorts themselves.
# We will exclude the target and comparator cohort IDs from covariates in the
# createTargetComparatorOutcomes call directly.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0), # No additional concepts specified in analysis specifications
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# The analysis specifications (covariateSelection.conceptsToInclude) did not provide
# concepts to include, so this section is commented out.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# This module generates the cohorts defined in the shared resources.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Define shared resources for cohort definitions.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Define shared resources for negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default, not specified in analysis specs
  detectOnDescendants = TRUE # Default, not specified in analysis specs
)

# Create module specifications for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Generate cohort statistics as specified in template
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# This module runs diagnostics on the generated cohorts.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # Run diagnostics on all defined cohorts (target, comparator, outcome, negative controls)
  runInclusionStatistics = TRUE, # Default, not specified in analysis specs
  runIncludedSourceConcepts = TRUE, # Default, not specified in analysis specs
  runOrphanConcepts = TRUE, # Default, not specified in analysis specs
  runTimeSeries = FALSE, # Default, not specified in analysis specs
  runVisitContext = TRUE, # Default, not specified in analysis specs
  runBreakdownIndexEvents = TRUE, # Default, not specified in analysis specs
  runIncidenceRate = TRUE, # Default, not specified in analysis specs
  runCohortRelationship = TRUE, # Default, not specified in analysis specs
  runTemporalCohortCharacterization = TRUE, # Default, not specified in analysis specs
  minCharacterizationMean = 0.01 # Default, not specified in analysis specs
)

# CohortMethodModule -----------------------------------------------------------
# This module performs comparative effectiveness analysis using CohortMethod.

# Study periods: Define specific study start and end dates for the analysis.
# Taken from getDbCohortMethodDataArgs.studyPeriods in Analysis Specifications.
studyPeriods <- tibble(
  studyStartDate = c("20130401", "20130401"), # YYYYMMDD
  studyEndDate   = c("20200331", "20181231")  # YYYYMMDD
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# Taken from createStudyPopArgs.timeAtRisks in Analysis Specifications.
timeAtRisks <- tibble(
  label = c("TAR_1_to_cohort_end", "TAR_1_to_99999_from_cohort_start"), # Descriptive labels for each TAR
  riskWindowStart  = c(1, 1), # riskWindowStart from Analysis Specifications
  startAnchor = c("cohort start", "cohort start"), # startAnchor from Analysis Specifications
  riskWindowEnd  = c(0, 99999), # riskWindowEnd from Analysis Specifications
  endAnchor = c("cohort end", "cohort start"), # endAnchor from Analysis Specifications
  minDaysAtRisk = c(1, 1) # minDaysAtRisk from Analysis Specifications
) 

# Propensity Score settings - match on PS
# Taken from propensityScoreAdjustment.psSettings where matchOnPsArgs is not null.
matchOnPsArgsList <- tibble(
  label = c("Match_MaxRatio2_Caliper0.2_StdLogit"),
  maxRatio  = c(2), # maxRatio from Analysis Specifications
  caliper = c(0.2), # caliper from Analysis Specifications
  caliperScale  = c("standardized logit") # caliperScale from Analysis Specifications
) 

# Propensity Score settings - stratify by PS
# The analysis specifications did not include stratifyByPsArgs, so this remains empty.
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0)
) 

# Build a single PS configuration list (each entry has: method, label, params)
# This loop processes both matching and stratification settings.
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


# Iterate through all analysis setting combinations to create a list of CM analyses.
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
          allowReverseMatch = FALSE, # Default, not specified in analysis specs
          stratificationColumns = c() # Default, not specified in analysis specs
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default, not specified in analysis specs
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings: Use default settings for covariate extraction.
      # The analysis specifications (covariateSelection.conceptsToInclude) did not
      # provide specific covariate settings beyond concepts to exclude (handled in
      # targetComparatorOutcomesList).
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default, not specified in analysis specs
      )

      # Define outcomes for the CohortMethod analysis.
      # Includes the main outcome and all negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not applicable for observed outcomes
            priorOutcomeLookback = 99999 # Default, not specified in analysis specs
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # For negative controls, assumed true effect size is 1 (no effect)
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
          # Exclude the target and comparator cohort IDs themselves from covariates.
          # This is a common practice to prevent confounding by the exposure itself.
          # No additional concepts to exclude were specified in the analysis specifications
          # (covariateSelection.conceptsToExclude was empty).
          excludedCovariateConceptIds = c(
            cmTcList$targetCohortId[i],
            cmTcList$comparatorCohortId[i]
          )
        )
      }

      # Arguments for fetching data from the database for CohortMethod.
      # Parameters taken from getDbCohortMethodDataArgs in Analysis Specifications.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # restrictToCommonPeriod from Analysis Specifications
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # maxCohortSize from Analysis Specifications (0 means no restriction)
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # Parameters taken from propensityScoreAdjustment.createPsArgs in Analysis Specifications.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # maxCohortSizeForFitting from Analysis Specifications
        errorOnHighCorrelation = TRUE, # errorOnHighCorrelation from Analysis Specifications
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail (template default)
        estimator = "att", # Average Treatment effect on the Treated (template default)
        prior = Cyclops::createPrior( # prior settings from Analysis Specifications
          priorType = "laplace", # prior.priorType from Analysis Specifications
          exclude = c(0), # Default, not specified in analysis specs
          useCrossValidation = TRUE # prior.useCrossValidation from Analysis Specifications
        ),
        control = Cyclops::createControl( # control settings from Analysis Specifications
          noiseLevel = "silent", # control.noiseLevel from Analysis Specifications
          cvType = "auto", # control.cvType from Analysis Specifications
          fold = 10, # control.fold from Analysis Specifications
          seed = 1, # Default, not specified in analysis specs
          resetCoefficients = TRUE, # control.resetCoefficients from Analysis Specifications
          tolerance = 2e-07, # control.tolerance from Analysis Specifications
          cvRepetitions = 10, # control.cvRepetitions from Analysis Specifications
          startingVariance = 0.01 # control.startingVariance from Analysis Specifications
        )
      )

      # Arguments for computing shared covariate balance.
      # Using template defaults as not specified in Analysis Specifications.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      # Arguments for computing covariate balance.
      # Using template defaults as not specified in Analysis Specifications.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting the outcome model.
      # Parameters taken from fitOutcomeModelArgs in Analysis Specifications.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # modelType from Analysis Specifications
        stratified = TRUE, # stratified from Analysis Specifications
        useCovariates = FALSE, # useCovariates from Analysis Specifications
        inversePtWeighting = FALSE, # inversePtWeighting from Analysis Specifications
        prior = Cyclops::createPrior( # prior settings from Analysis Specifications
          priorType = "laplace", # prior.priorType from Analysis Specifications
          useCrossValidation = TRUE # prior.useCrossValidation from Analysis Specifications
        ),
        control = Cyclops::createControl( # control settings from Analysis Specifications
          cvType = "auto", # control.cvType from Analysis Specifications
          fold = 10, # control.fold from Analysis Specifications
          seed = 1, # Default, not specified in analysis specs
          resetCoefficients = TRUE, # control.resetCoefficients from Analysis Specifications
          startingVariance = 0.01, # control.startingVariance from Analysis Specifications
          tolerance = 2e-07, # control.tolerance from Analysis Specifications
          cvRepetitions = 10, # control.cvRepetitions from Analysis Specifications
          noiseLevel = "quiet" # control.noiseLevel from Analysis Specifications
        )
      )

      # Arguments for creating the study population.
      # Parameters taken from createStudyPopArgs in Analysis Specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # restrictToCommonPeriod from Analysis Specifications
        firstExposureOnly = FALSE, # firstExposureOnly from Analysis Specifications
        washoutPeriod = 0, # washoutPeriod from Analysis Specifications
        removeDuplicateSubjects = "keep all", # removeDuplicateSubjects from Analysis Specifications
        censorAtNewRiskWindow = FALSE, # censorAtNewRiskWindow from Analysis Specifications
        removeSubjectsWithPriorOutcome = TRUE, # removeSubjectsWithPriorOutcome from Analysis Specifications
        priorOutcomeLookback = 99999, # priorOutcomeLookback from Analysis Specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # minDaysAtRisk from Analysis Specifications
        maxDaysAtRisk = 99999 # Default, not specified in analysis specs
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

# Create CohortMethod module specifications.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Default, not specified in analysis specs
  refitPsForEveryOutcome = FALSE, # Default, not specified in analysis specs
  refitPsForEveryStudyPopulation = FALSE, # Default, not specified in analysis specs
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default, not specified in analysis specs
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
# The file path uses the 'name' from the Analysis Specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "sglt2imetformin", "sglt2imetforminAnalysisSpecification.json")
)