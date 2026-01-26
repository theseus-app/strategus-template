################################################################################
# CreateStrategusAnalysisSpecification.R
# Analysis specification for the "corazon" study
# 
# This script creates Strategus analysis specifications for a comparative
# effectiveness study using the OHDSI CohortMethod package.
# 
# Settings are applied according to the provided JSON configuration:
# - Target cohort: ID 1794126, name "target1"
# - Comparator cohort: ID 1794132, name "comparator1"
# - Outcome cohort: ID 1794131, name "outcome1"
# - Negative control concept set: ID 1888110, name "negative"
# - Study period: 2010-01-01 to 2019-12-31
# - Propensity score stratification with 5 strata
# - Cox proportional hazards outcome model
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from ATLAS
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Note: Using exact cohort IDs from the analysis specifications
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal use in Strategus
# Strategus requires sequential cohort IDs starting from 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Using concept set ID 1888110 from analysis specifications
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,
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
  mutate(cohortId = row_number() + 100) %>% # Negative controls start from 101
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts for analysis --------------------------
# Outcomes: outcome1 (cohort ID 3 after re-numbering)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Standard 365-day clean window for outcomes

# Target and Comparator for the CohortMethod analysis
# Using exact names from analysis specifications
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate exclusion: No specific concepts to exclude based on analysis specs
# excludedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# Optional: No specific concepts to include based on analysis specs
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
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

# CohortMethodModule -----------------------------------------------------------

# Study periods from analysis specifications: 2010-01-01 to 2019-12-31
studyPeriods <- tibble(
  studyStartDate = c("20100101"), # YYYYMMDD format as specified
  studyEndDate   = c("20191231")  # YYYYMMDD format as specified
)

# Time-at-risks (TARs) from analysis specifications:
# Risk window: 1 day after cohort start to cohort end, with min 1 day at risk
timeAtRisks <- tibble(
  label = c("Tar1"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"), # From analysis specs
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end"), # From analysis specs
  minDaysAtRisk = c(1) # From analysis specs
)

# Propensity Score settings - match on PS: Not used (null in analysis specs)
# matchOnPsArgsList <- tibble(
#   label = c(),
#   maxRatio  = c(),
#   caliper = c(),
#   caliperScale  = c()
# )

# Propensity Score settings - stratify by PS: Used with 5 strata, baseSelection "all"
stratifyByPsArgsList <- tibble(
  label = c("Stratify5"),
  numberOfStrata  = c(5), # From analysis specs
  baseSelection = c("all") # From analysis specs
)

# Build a single PS configuration list
psConfigList <- list()

# Add stratify by PS configuration (only method specified in analysis specs)
if (exists("stratifyByPsArgsList") && nrow(stratifyByPsArgsList) > 0) {
  for (i in seq_len(nrow(stratifyByPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "stratify", # Only stratification specified in analysis specs
      label  = stratifyByPsArgsList$label[i],
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
      
      # Configure PS adjustment method based on analysis specs
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

      # Covariate settings: using default with no specific inclusions/exclusions
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome list including both primary outcome and negative controls
      outcomeList <- append(
        # Primary outcome from analysis specs
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999 # From analysis specs
          )
        }),
        # Negative control outcomes
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )
      
      # Create target-comparator-outcomes combinations
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = c() # No specific exclusions in analysis specs
        )
      }

      # GetDbCohortMethodDataArgs from analysis specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Default for Strategus
        studyStartDate = studyStartDate, # From analysis specs: 20100101
        studyEndDate = studyEndDate,     # From analysis specs: 20191231
        maxCohortSize = 0,               # From analysis specs: 0 (no limit)
        covariateSettings = covariateSettings
      )

      # CreatePsArgs from analysis specifications
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From analysis specs
        errorOnHighCorrelation = TRUE,    # From analysis specs
        stopOnError = FALSE, # Allow Strategus to complete all operations
        estimator = "att",
        prior = Cyclops::createPrior(     # From analysis specs: laplace prior
          priorType = "laplace", 
          exclude = c(0), 
          useCrossValidation = TRUE       # From analysis specs
        ),
        control = Cyclops::createControl( # From analysis specs
          noiseLevel = "silent",          # From analysis specs
          cvType = "auto",                # From analysis specs
          seed = 1, 
          resetCoefficients = TRUE,       # From analysis specs
          tolerance = 2e-07,              # From analysis specs
          cvRepetitions = 10,             # From analysis specs (note: template had 1)
          startingVariance = 0.01         # From analysis specs
        )
      )

      # Covariate balance computation arguments
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # FitOutcomeModelArgs from analysis specifications
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",                # From analysis specs
        stratified = TRUE,                # From analysis specs
        useCovariates = FALSE,            # From analysis specs
        inversePtWeighting = FALSE,       # From analysis specs
        prior = Cyclops::createPrior(     # From analysis specs: laplace prior
          priorType = "laplace", 
          useCrossValidation = TRUE       # From analysis specs
        ),
        control = Cyclops::createControl( # From analysis specs
          cvType = "auto",                # From analysis specs
          seed = 1, 
          resetCoefficients = TRUE,       # From analysis specs
          startingVariance = 0.01,        # From analysis specs
          tolerance = 2e-07,              # From analysis specs
          cvRepetitions = 10,             # From analysis specs (note: template had 1)
          noiseLevel = "quiet"            # From analysis specs
        )
      )
      
      # CreateStudyPopArgs from analysis specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,   # From analysis specs
        firstExposureOnly = FALSE,        # From analysis specs
        washoutPeriod = 0,                # From analysis specs
        removeDuplicateSubjects = "keep all", # From analysis specs
        censorAtNewRiskWindow = FALSE,    # From analysis specs
        removeSubjectsWithPriorOutcome = TRUE, # From analysis specs
        priorOutcomeLookback = 99999,     # From analysis specs
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From analysis specs
        maxDaysAtRisk = 99999
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
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to JSON
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "corazon", "corazonAnalysisSpecification.json")
)