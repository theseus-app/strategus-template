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
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Define cohort IDs from analysis specifications
targetCohortIdSpec <- 1794126
comparatorCohortIdSpec <- 1794132
outcomeCohortIdSpec <- 1794131

# Cohort Definitions
# Export cohort definitions from WebAPI based on the specified IDs.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    targetCohortIdSpec, # Target: target1
    comparatorCohortIdSpec, # Comparator: comparator1
    outcomeCohortIdSpec # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal consistency within the study.
# Target cohort is assigned ID 1.
cohortDefinitionSet[cohortDefinitionSet$cohortId == targetCohortIdSpec,]$cohortId <- 1
# Comparator cohort is assigned ID 2.
cohortDefinitionSet[cohortDefinitionSet$cohortId == comparatorCohortIdSpec,]$cohortId <- 2
# Outcome cohort is assigned ID 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == outcomeCohortIdSpec,]$cohortId <- 3

# Negative control outcomes
# Retrieve negative control concept set definition from WebAPI.
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
  # Assign unique cohort IDs for negative controls, starting from 101.
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs to prevent conflicts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Filter for the main outcome cohort (re-numbered ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # cleanWindow is not specified, using template default.
  mutate(cleanWindow = 365) 

# Target and Comparator for the CohortMethod analysis 
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered target cohort ID
  targetCohortName = cohortDefinitionSet[cohortDefinitionSet$cohortId == 1,]$cohortName, # Target cohort name from spec
  comparatorCohortId = 2, # Re-numbered comparator cohort ID
  comparatorCohortName = cohortDefinitionSet[cohortDefinitionSet$cohortId == 2,]$cohortName # Comparator cohort name from spec
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study.
# Check if conceptsToExclude is provided and not null/empty.
if (!is.null(NULL) && length(NULL) > 0 && !is.null(NULL) && length(NULL) > 0) {
  excludedCovariateConcepts <- data.frame(
    conceptId = c(), # From analysis specifications: covariateSelection.conceptsToExclude.id
    conceptName = c() # From analysis specifications: covariateSelection.conceptsToExclude.name
  )
} else {
  excludedCovariateConcepts <- data.frame(conceptId = numeric(0), conceptName = character(0))
}

# Optional: If you want to define covariates to include instead of including them all
# Check if conceptsToInclude is provided and not null/empty.
if (!is.null(NULL) && length(NULL) > 0 && !is.null(NULL) && length(NULL) > 0) {
  includedCovariateConcepts <- data.frame(
    conceptId = c(), # From analysis specifications: covariateSelection.conceptsToInclude.id
    conceptName = c() # From analysis specifications: covariateSelection.conceptsToInclude.name
  )
} else {
  includedCovariateConcepts <- data.frame(conceptId = numeric(0), conceptName = character(0))
}

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default from template
  detectOnDescendants = TRUE # Default from template
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Default from template
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # All cohorts defined in cohortDefinitionSet
  runInclusionStatistics = TRUE, # Default from template
  runIncludedSourceConcepts = TRUE, # Default from template
  runOrphanConcepts = TRUE, # Default from template
  runTimeSeries = FALSE, # Default from template
  runVisitContext = TRUE, # Default from template
  runBreakdownIndexEvents = TRUE, # Default from template
  runIncidenceRate = TRUE, # Default from template
  runCohortRelationship = TRUE, # Default from template
  runTemporalCohortCharacterization = TRUE, # Default from template
  minCharacterizationMean = 0.01 # Default from template
)

# CohortMethodModule -----------------------------------------------------------

# Study periods from analysis specifications: getDbCohortMethodDataArgs.studyPeriods
studyPeriods <- tibble(
  studyStartDate = c(20210101), # YYYYMMDD, from analysis specifications
  studyEndDate   = c(NA) # YYYYMMDD, NA for null, from analysis specifications
)

# Time-at-risks (TARs) for the outcomes of interest in your study
# From analysis specifications: createStudyPopArgs.timeAtRisks
timeAtRisks <- tibble(
  label = c("TAR 1-14 days from cohort start"), # Descriptive label for the TAR
  riskWindowStart  = c(1), # From analysis specifications
  startAnchor = c("cohort start"), # From analysis specifications
  riskWindowEnd  = c(14), # From analysis specifications
  endAnchor = c("cohort start") # From analysis specifications
) 

# Propensity Score settings - match on PS
# From analysis specifications: propensityScoreAdjustment.psSettings.matchOnPsArgs
matchOnPsArgsList <- tibble(
  label = c("Match on PS (maxRatio 100, caliper 0.2 standardized logit)"), # Descriptive label for PS matching
  maxRatio  = c(100), # From analysis specifications
  caliper = c(0.2), # From analysis specifications
  caliperScale  = c("standardized logit") # From analysis specifications
) 

# Propensity Score settings - stratify by PS
# From analysis specifications: propensityScoreAdjustment.psSettings.stratifyByPsArgs (is null)
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = numeric(0),
  baseSelection = character(0)
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
      
      matchOnPsArgs <- NULL
      stratifyByPsArgs <- NULL

      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio, # From analysis specifications: propensityScoreAdjustment.psSettings.matchOnPsArgs.maxRatio
          caliper = psCfg$params$caliper, # From analysis specifications: propensityScoreAdjustment.psSettings.matchOnPsArgs.caliper
          caliperScale = psCfg$params$caliperScale, # From analysis specifications: propensityScoreAdjustment.psSettings.matchOnPsArgs.caliperScale
          allowReverseMatch = FALSE, # Default from template
          stratificationColumns = c() # Default from template
        )
      } else if (psCfg$method == "stratify") {
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata, # From analysis specifications: propensityScoreAdjustment.psSettings.stratifyByPsArgs.numberOfStrata
          stratificationColumns = c(), # Default from template
          baseSelection = psCfg$params$baseSelection # From analysis specifications: propensityScoreAdjustment.psSettings.stratifyByPsArgs.baseSelection
        )
      }

      # Default covariate settings.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default from template
      )

      # Combine main outcomes and negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999 # From analysis specifications: createStudyPopArgs.priorOutcomeLookBack
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # Default for negative controls
          )
        })
      )
      
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        # Excluded covariate concept IDs:
        # - Target and comparator cohort IDs (assuming these are the index exposures to exclude)
        # - Additional concepts from covariateSelection.conceptsToExclude
        excludedConcepts <- c(
          cmTcList$targetCohortId[i], 
          cmTcList$comparatorCohortId[i],
          excludedCovariateConcepts$conceptId
        )
        
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i], # Re-numbered target cohort ID
          comparatorId = cmTcList$comparatorCohortId[i], # Re-numbered comparator cohort ID
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedConcepts
        )
      }

      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Default from template
        studyStartDate = as.character(studyStartDate), # From analysis specifications: getDbCohortMethodDataArgs.studyPeriods.studyStartDate
        studyEndDate = as.character(studyEndDate), # From analysis specifications: getDbCohortMethodDataArgs.studyPeriods.studyEndDate
        maxCohortSize = 0, # From analysis specifications: getDbCohortMethodDataArgs.maxCohortSize
        covariateSettings = covariateSettings
      )

      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From analysis specifications: propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From analysis specifications: propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att", # Default from template
        prior = Cyclops::createPrior( # From analysis specifications: propensityScoreAdjustment.createPsArgs.prior
          priorType = "laplace", # From analysis specifications
          exclude = c(0), # Default from template
          useCrossValidation = TRUE # From analysis specifications
        ),
        control = Cyclops::createControl( # From analysis specifications: propensityScoreAdjustment.createPsArgs.control
          noiseLevel = "silent", # From analysis specifications
          cvType = "auto", # From analysis specifications
          seed = 1, # Default from template
          resetCoefficients = TRUE, # From analysis specifications
          tolerance = 2e-07, # From analysis specifications
          cvRepetitions = 10, # From analysis specifications (fold/cvRepetitions)
          startingVariance = 0.01 # From analysis specifications
        )
      )

      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default from template
        covariateFilter = NULL # Default from template
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default from template
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default from template
      )

      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From analysis specifications: fitOutcomeModelArgs.modelType
        stratified = TRUE, # From analysis specifications: fitOutcomeModelArgs.stratified
        useCovariates = FALSE, # From analysis specifications: fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE, # From analysis specifications: fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior( # From analysis specifications: fitOutcomeModelArgs.prior
          priorType = "laplace", # From analysis specifications
          useCrossValidation = TRUE # From analysis specifications
        ),
        control = Cyclops::createControl( # From analysis specifications: fitOutcomeModelArgs.control
          cvType = "auto", # From analysis specifications
          seed = 1, # Default from template
          resetCoefficients = TRUE, # From analysis specifications
          startingVariance = 0.01, # From analysis specifications
          tolerance = 2e-07, # From analysis specifications
          cvRepetitions = 10, # From analysis specifications (fold/cvRepetitions)
          noiseLevel = "quiet" # From analysis specifications
        )
      )
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From analysis specifications: createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = TRUE, # From analysis specifications: createStudyPopArgs.firstExposureOnly
        washoutPeriod = 365, # From analysis specifications: createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "remove all", # From analysis specifications: createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # From analysis specifications: createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # From analysis specifications: createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # From analysis specifications: createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From analysis specifications: createStudyPopArgs.timeAtRisks.riskWindowStart
        startAnchor = timeAtRisks$startAnchor[t], # From analysis specifications: createStudyPopArgs.timeAtRisks.startAnchor
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From analysis specifications: createStudyPopArgs.timeAtRisks.riskWindowEnd
        endAnchor = timeAtRisks$endAnchor[t], # From analysis specifications: createStudyPopArgs.timeAtRisks.endAnchor
        minDaysAtRisk = 1, # From analysis specifications: createStudyPopArgs.timeAtRisks.minDaysAtRisk
        maxDaysAtRisk = 99999 # Default from template, not in analysis specifications
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
  analysesToExclude = NULL, # Default from template
  refitPsForEveryOutcome = FALSE, # Default from template
  refitPsForEveryStudyPopulation = FALSE, # Default from template
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default from template
)

# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The study name "rapidcyclejanssen" is taken from the analysis specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "rapidcyclejanssen", "rapidcyclejanssenAnalysisSpecification.json")
)