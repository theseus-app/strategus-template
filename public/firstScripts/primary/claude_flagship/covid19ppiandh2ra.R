################################################################################
# COVID-19 PPI and H2RA Analysis Specifications
# 
# This script creates Strategus analysis specifications for comparing
# PPI vs H2RA exposures in COVID-19 patients.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from Atlas
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Retrieve cohort definitions for target (PPI), comparator (H2RA), and outcome cohorts
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to sequential IDs for internal use
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve negative control concept set (ID: 1888110) and resolve to specific concepts
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
  mutate(cohortId = row_number() + 100) %>% # Start negative control cohort IDs at 101 to avoid conflicts
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts for analysis -------------------------

# Outcomes: Define the outcome of interest (cohort ID 3 = outcome1)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Clean window not actively used but kept for compatibility

# Target and Comparator for the CohortMethod analysis
# Target: cohort ID 1 (target1), Comparator: cohort ID 2 (comparator1)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Excluded covariate concepts: No specific concepts to exclude per specifications
# (conceptsToExclude id and name are null)
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# Optional: Included covariate concepts
# Per specifications, conceptsToInclude has null id and empty name, so no specific inclusions
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# Create cohort generator module to instantiate cohorts in the database
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
# Run cohort diagnostics to assess cohort quality and characteristics
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

# Study periods: Define the time window for the study
# From specifications: studyStartDate = "20200101", studyEndDate = "20200515"
studyPeriods <- tibble(
  studyStartDate = c("20200101"), # Study period start: January 1, 2020
  studyEndDate   = c("20200515")  # Study period end: May 15, 2020
)

# Time-at-risks (TARs) for the outcomes of interest
# From specifications: riskWindowStart = 1 (cohort start), riskWindowEnd = 99999 (cohort start), minDaysAtRisk = 1
timeAtRisks <- tibble(
  label = c("TAR_1_to_99999_days"),
  riskWindowStart  = c(1),          # Start 1 day after cohort start
  startAnchor = c("cohort start"),  # Anchor risk window start to cohort start
  riskWindowEnd  = c(99999),        # End 99999 days after cohort start (essentially unlimited follow-up)
  endAnchor = c("cohort start")     # Anchor risk window end to cohort start
)

# Propensity Score settings - match on PS
# Per specifications: matchOnPsArgs is null, so no matching on PS
matchOnPsArgsList <- tibble(
  label = character(0),
  maxRatio  = numeric(0),
  caliper = numeric(0),
  caliperScale  = character(0)
)

# Propensity Score settings - stratify by PS
# From specifications: stratifyByPsArgs with numberOfStrata = 5, baseSelection = "all"
stratifyByPsArgsList <- tibble(
  label = c("PS_Stratify_5_strata"),
  numberOfStrata  = c(5),          # Stratify into 5 propensity score strata
  baseSelection = c("all")         # Use all subjects as base for stratification
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
      
      # Configure PS adjustment method based on psConfigList entry
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

      # Covariate settings: Use default covariate settings with descendants
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome list: includes outcome of interest and negative controls
      # Outcome of interest: priorOutcomeLookback = 99999 (from specifications)
      # Negative controls: trueEffectSize = 1 (no effect expected)
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999  # From priorOutcomeLookBack in specifications
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
      
      # Create target-comparator-outcomes combinations
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # getDbCohortMethodDataArgs: Define parameters for extracting cohort data
      # From specifications: maxCohortSize = 0 (no limit), restrictToCommonPeriod = false
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,  # From specifications: restrictToCommonPeriod = false
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,               # From specifications: maxCohortSize = 0 (no limit)
        covariateSettings = covariateSettings
      )

      # createPsArgs: Define parameters for propensity score model
      # From specifications: maxCohortSizeForFitting = 250000, errorOnHighCorrelation = true
      # prior: priorType = "laplace", useCrossValidation = true
      # control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10, etc.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # From specifications
        errorOnHighCorrelation = TRUE,      # From specifications
        stopOnError = FALSE,                # Allow Strategus to complete all operations
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",            # From specifications: priorType = "laplace"
          exclude = c(0),
          useCrossValidation = TRUE         # From specifications: useCrossValidation = true
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",            # From specifications: noiseLevel = "silent"
          cvType = "auto",                  # From specifications: cvType = "auto"
          seed = 1,
          resetCoefficients = TRUE,         # From specifications: resetCoefficients = true
          tolerance = 2e-07,                # From specifications: tolerance = 2e-7
          cvRepetitions = 10,               # From specifications: cvRepetitions = 10
          startingVariance = 0.01,          # From specifications: startingVariance = 0.01
          fold = 10                         # From specifications: fold = 10
        )
      )

      # Covariate balance computation for shared covariates (all covariates)
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      
      # Covariate balance computation for table 1 covariates
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs: Define parameters for outcome model
      # From specifications: modelType = "cox", stratified = true, useCovariates = false, inversePtWeighting = false
      # prior: priorType = "laplace", useCrossValidation = true
      # control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10, noiseLevel = "quiet", etc.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",               # From specifications: modelType = "cox"
        stratified = TRUE,               # From specifications: stratified = true
        useCovariates = FALSE,           # From specifications: useCovariates = false
        inversePtWeighting = FALSE,      # From specifications: inversePtWeighting = false
        prior = Cyclops::createPrior(
          priorType = "laplace",         # From specifications: priorType = "laplace"
          useCrossValidation = TRUE      # From specifications: useCrossValidation = true
        ),
        control = Cyclops::createControl(
          cvType = "auto",               # From specifications: cvType = "auto"
          seed = 1,
          resetCoefficients = TRUE,      # From specifications: resetCoefficients = true
          startingVariance = 0.01,       # From specifications: startingVariance = 0.01
          tolerance = 2e-07,             # From specifications: tolerance = 2e-7
          cvRepetitions = 10,            # From specifications: cvRepetitions = 10
          noiseLevel = "quiet",          # From specifications: noiseLevel = "quiet"
          fold = 10                      # From specifications: fold = 10
        )
      )
      
      # createStudyPopArgs: Define parameters for creating study population
      # From specifications: restrictToCommonPeriod = false, firstExposureOnly = true, washoutPeriod = 365
      # removeDuplicateSubjects = "keep all", censorAtNewRiskWindow = false
      # removeSubjectsWithPriorOutcome = true, priorOutcomeLookBack = 99999
      # minDaysAtRisk = 1
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,              # From specifications
        firstExposureOnly = TRUE,                    # From specifications
        washoutPeriod = 365,                         # From specifications: washoutPeriod = 365
        removeDuplicateSubjects = "keep all",        # From specifications: removeDuplicateSubjects = "keep all"
        censorAtNewRiskWindow = FALSE,               # From specifications: censorAtNewRiskWindow = false
        removeSubjectsWithPriorOutcome = TRUE,       # From specifications: removeSubjectsWithPriorOutcome = true
        priorOutcomeLookback = 99999,                # From specifications: priorOutcomeLookBack = 99999
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,                           # From specifications: minDaysAtRisk = 1
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

# Create CohortMethod module specifications
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
# Combine all module specifications into a single analysis specification object
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save analysis specifications to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "covid19ppiandh2ra", "covid19ppiandh2raAnalysisSpecification.json")
)