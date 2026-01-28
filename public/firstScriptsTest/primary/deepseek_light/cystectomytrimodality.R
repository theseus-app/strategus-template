################################################################################
# CreateStrategusAnalysisSpecification.R
# Analysis specification for cystectomytrimodality study
# 
# This script creates Strategus analysis specifications based on the provided
# settings. It includes:
# 1. Cohort definitions (target, comparator, outcome)
# 2. Negative control outcomes
# 3. CohortGenerator module for cohort creation
# 4. CohortDiagnostics module for cohort characterization
# 5. CohortMethod module for comparative effectiveness analysis
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from ATLAS
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions - using exact IDs from analysis specifications
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
# Target becomes cohortId 1, comparator becomes 2, outcome becomes 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes - using concept set ID 1888110 from specifications
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
  mutate(cohortId = row_number() + 100) %>% # Start negative controls at ID 101
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts for analysis --------------------------
# Outcomes: outcome1 (cohortId 3)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)  # Standard 365-day clean window for outcomes

# Target and Comparator for CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# No specific covariate concepts to exclude (empty per specifications)
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# No specific covariate concepts to include (empty per specifications)
# includedCovariateConcepts <- data.frame(
#   conceptId = integer(),
#   conceptName = character()
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

# Study periods from specifications: 2005-01-01 to 2017-12-31
studyPeriods <- tibble(
  studyStartDate = c("20050101"),
  studyEndDate   = c("20171231")
)

# Time-at-risks (TARs) from specifications:
# Risk window: 1 day after cohort start to 99999 days (end of follow-up)
# Minimum days at risk: 1
timeAtRisks <- tibble(
  label = c("Tar1"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(99999),
  endAnchor = c("cohort start")
) 

# Propensity Score settings - match on PS from specifications:
# Max ratio: 3, Caliper: 0.2, Caliper scale: standardized logit
matchOnPsArgsList <- tibble(
  label = c("MatchOnPs"),
  maxRatio  = c(3),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
) 

# No stratification by PS (stratifyByPsArgs is null in specifications)
# stratifyByPsArgsList <- tibble(
#   label = character(),
#   numberOfStrata  = integer(),
#   baseSelection = character()
# ) 

# Build PS configuration list
psConfigList <- list()

# Add match on PS configuration
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

# No stratification configurations to add
# if (exists("stratifyByPsArgsList") && nrow(stratifyByPsArgsList) > 0) {
#   for (i in seq_len(nrow(stratifyByPsArgsList))) {
#     psConfigList[[length(psConfigList) + 1]] <- list(
#       method = "stratify",
#       label  = stratifyByPsArgsList$label[i],
#       params = list(
#         numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
#         baseSelection  = stratifyByPsArgsList$baseSelection[i]
#       )
#     )
#   }
# }

# Iterate through all analysis setting combinations
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
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

      # Default covariate settings (no specific inclusions/exclusions per specs)
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome list including both primary outcome and negative controls
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999  # From specifications
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
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId  # Empty per specs
        )
      }

      # GetDbCohortMethodDataArgs from specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,  # From specifications
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,  # From specifications (0 = no limit)
        covariateSettings = covariateSettings
      )

      # CreatePsArgs from specifications
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # From specifications
        errorOnHighCorrelation = TRUE,     # From specifications
        stopOnError = FALSE,  # Allow Strategus to complete all operations
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",  # From specifications
          exclude = c(0),
          useCrossValidation = TRUE  # From specifications
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",      # From specifications
          cvType = "auto",            # From specifications
          seed = 1,
          resetCoefficients = TRUE,   # From specifications
          tolerance = 2e-07,          # From specifications
          cvRepetitions = 10,         # From specifications (corrected from template)
          startingVariance = 0.01     # From specifications
        )
      )

      # Covariate balance computation
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # FitOutcomeModelArgs from specifications
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",          # From specifications
        stratified = TRUE,          # From specifications
        useCovariates = TRUE,       # From specifications
        inversePtWeighting = FALSE, # From specifications
        prior = Cyclops::createPrior(
          priorType = "laplace",    # From specifications
          useCrossValidation = TRUE # From specifications
        ),
        control = Cyclops::createControl(
          cvType = "auto",            # From specifications
          seed = 1,
          resetCoefficients = TRUE,   # From specifications
          startingVariance = 0.01,    # From specifications
          tolerance = 2e-07,          # From specifications
          cvRepetitions = 10,         # From specifications (corrected from template)
          noiseLevel = "quiet"        # From specifications
        )
      )
      
      # CreateStudyPopArgs from specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE,      # From specifications
        firstExposureOnly = FALSE,          # From specifications
        washoutPeriod = 0,                  # From specifications
        removeDuplicateSubjects = "keep all", # From specifications
        censorAtNewRiskWindow = FALSE,      # From specifications
        removeSubjectsWithPriorOutcome = TRUE,  # From specifications
        priorOutcomeLookback = 99999,       # From specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,                  # From specifications
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
  file.path("inst", "cystectomytrimodality", "cystectomytrimodalityAnalysisSpecification.json")
)