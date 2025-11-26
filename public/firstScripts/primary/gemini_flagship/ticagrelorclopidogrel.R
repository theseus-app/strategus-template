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

# ==============
# Shared Resources
# ==============
# This section defines the cohorts and concept sets that will be used throughout the analysis.
# These are declared as "Shared Resources" in Strategus and can be referenced by any module.

# The baseUrl for the WebApi instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Define the cohort IDs from the <Analysis Specifications>.
# These cohorts will be downloaded from the WebApi and used in the analysis.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-numbering cohort IDs to a consistent 1, 2, 3... sequence for Strategus.
# This makes it easier to reference them in the analysis settings.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # target1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # comparator1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # outcome1

# Define the concept set for negative controls from the <Analysis Specifications>.
# These are outcomes not believed to be caused by the exposure and are used for calibration.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # negative
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
  # Assign unique cohort IDs for negative controls, starting from 101.
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)

# Sanity check to ensure there are no duplicate cohort IDs between the main cohorts and negative controls.
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# =================
# Analysis Settings
# =================
# This section defines the parameters for the specific analyses to be run.

# Define the outcomes of interest for the CohortMethod analysis.
# We select the re-numbered outcome cohort (ID=3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName)

# Define the target and comparator cohorts for the CohortMethod analysis.
# These correspond to the re-numbered cohort IDs.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Per the <Analysis Specifications>, the `covariateSelection` section for concepts to exclude is empty.
# Therefore, we create an empty data frame.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional: Per the <Analysis Specifications>, the `covariateSelection` section for concepts to include is empty.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# =========================================================================
# Module Settings: CohortGeneratorModule
# =========================================================================
# This module is responsible for generating the cohort instances on the CDM.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Define the main cohort definitions as a shared resource.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Define the negative control outcome cohorts as a shared resource.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
# Specify the settings for the CohortGenerator module execution.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# =========================================================================
# Module Settings: CohortDiagnosticsModule
# =========================================================================
# This module runs a set of diagnostics on the generated cohorts.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE,
  runIncludedSourceConcepts = TRUE,
  runOrphanConcepts = TRUE,
  runTimeSeries = FALSE,
  runVisitContext = TRUE,
  runBreakdownIndexEvents = TRUE,
  runIncidenceRate = TRUE,
  runCohortRelationship = TRUE,
  runTemporalCohortCharacterization = TRUE,
  minCharacterizationMean = 0.01
)

# =========================================================================
# Module Settings: CohortMethodModule
# =========================================================================
# This section defines the settings for the comparative cohort analysis (CohortMethod).

# Define the overall study period based on getDbCohortMethodDataArgs.studyPeriods.
studyPeriods <- tibble(
  studyStartDate = c("20111101"),
  studyEndDate   = c("20190331")
)

# Define the time-at-risk (TAR) windows based on createStudyPopArgs.timeAtRisks.
timeAtRisks <- tibble(
  label = c("1 to 365d after cohort start"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(365),
  endAnchor = c("cohort start")
) 

# Define propensity score matching settings based on propensityScoreAdjustment.psSettings.matchOnPsArgs.
matchOnPsArgsList <- tibble(
  label = c("1-to-1 matching, 0.2 caliper on standardized logit"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
) 

# The <Analysis Specifications> indicates stratifyByPsArgs is null, so we create an empty tibble.
# This ensures that no stratification analyses are generated.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c(),
) 

# Build a list of propensity score adjustment configurations from the tibbles defined above.
# This structure allows for iterating through different PS methods (e.g., matching, stratification).
psConfigList <- list()

# Convert each row in the matching arguments tibble into a configuration list item.
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

# Convert each row in the stratification arguments tibble into a configuration list item.
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


# =========================================================================
# Analysis Assembly Loop
# =========================================================================
# This loop iterates through all combinations of study periods, TARs, and PS settings
# to create a list of CohortMethod analysis settings (cmAnalysisList).
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure either matching or stratification based on the PS configuration.
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Use default covariate settings as `covariateSelection` is empty in the spec.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create a list of outcomes, combining the primary outcome(s) and negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )
      
      # Create the list of target-comparator-outcomes settings.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Since the spec for `conceptsToExclude` is empty, this will be an empty vector.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Define arguments for getting data from the database.
      # Settings from getDbCohortMethodDataArgs.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # 0 means no limit on cohort size.
        covariateSettings = covariateSettings
      )

      # Define arguments for creating the propensity score model.
      # Settings from propensityScoreAdjustment.createPsArgs.
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace", 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", 
          cvType = "auto", 
          resetCoefficients = TRUE, 
          tolerance = 2e-07, 
          cvRepetitions = 10,
          fold = 10,
          startingVariance = 0.01
        )
      )

      # Arguments for computing covariate balance (shared across adjusted populations).
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      
      # Arguments for computing covariate balance for the final table 1.
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Define arguments for fitting the outcome model.
      # Settings from fitOutcomeModelArgs.
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = FALSE, # Matched sets are not used as strata in the outcome model.
        useCovariates = FALSE, # No covariates are included in the outcome model.
        inversePtWeighting = FALSE,
        prior = Cyclops::createPrior(
          priorType = "laplace", 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          cvType = "auto", 
          resetCoefficients = TRUE,
          startingVariance = 0.01, 
          tolerance = 2e-07, 
          cvRepetitions = 10,
          fold = 10,
          noiseLevel = "quiet"
        )
      )
      
      # Define arguments for creating the study population.
      # Settings from createStudyPopArgs.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Assemble all the defined arguments into a single CohortMethod analysis object.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study Period: %s to %s; TAR: %s; PS Method: %s",
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

# Create the final module specifications for CohortMethod.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# =========================================================================
# Create the complete analysis specifications JSON
# =========================================================================
# This combines all the shared resources and module specifications into a single
# Strategus analysis specification object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Add shared resources (cohorts and negative controls).
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  # Add module specifications.
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications object to a JSON file.
# The file path uses the study name from the <Analysis Specifications>.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "ticagrelorclopidogrel", "ticagrelorclopidogrelAnalysisSpecification.json")
)