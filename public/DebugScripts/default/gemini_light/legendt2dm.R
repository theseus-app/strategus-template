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
library(ROhdsiWebApi) # Required for WebAPI calls
library(FeatureExtraction) # Required for covariate settings
library(CohortMethod) # Required for CohortMethod functions
library(Cyclops) # Required for Cyclops priors and controls
library(ParallelLogger) # Required for saving settings

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to align with internal IDs (1=target, 2=comparator, 3=outcome).
# This is done to simplify referencing in subsequent steps like cmTcList.
# Original IDs are preserved in the initial 'cohortDefinitionSet' for display, but remapped for logic.
cohortDefinitionSet <- cohortDefinitionSet %>%
  mutate(
    cohortId = case_when(
      .data$cohortId == 1794126 ~ 1, # Remap Target cohort ID
      .data$cohortId == 1794132 ~ 2, # Remap Comparator cohort ID
      .data$cohortId == 1794131 ~ 3, # Remap Outcome cohort ID
      TRUE ~ .data$cohortId # Keep other cohort IDs as is
    ),
    cohortName = case_when(
      .data$cohortId == 1 ~ "target1",     # Ensure name aligns with new ID
      .data$cohortId == 2 ~ "comparator1", # Ensure name aligns with new ID
      .data$cohortId == 3 ~ "outcome1",    # Ensure name aligns with new ID
      TRUE ~ .data$cohortName
    )
  )

# Negative control outcomes
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # From negativeControlConceptSet.id
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
  mutate(cohortId = row_number() + 100) %>% # Assign cohort IDs starting from 101 for negative controls
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs across study cohorts and negative controls
# The original script's use of c() is correct to combine vectors before checking for duplicates.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Filter for the re-numbered outcome cohort (ID 3)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window for outcomes

# Target and Comparator for the CohortMethod analysis, using re-numbered IDs
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered target cohort ID
  targetCohortName = "target1", # Name from Analysis Specifications
  comparatorCohortId = 2, # Re-numbered comparator cohort ID
  comparatorCohortName = "comparator1" # Name from Analysis Specifications
)

# For the CohortMethod LSPS we'll need to exclude specific concepts as covariates.
# Based on Analysis Specifications: covariateSelection.conceptsToExclude is empty.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all.
# Based on Analysis Specifications: covariateSelection.conceptsToInclude is empty.
includedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

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

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # All defined cohort IDs for diagnostics
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

# Study periods from Analysis Specifications: getDbCohortMethodDataArgs.studyPeriods
studyPeriods <- tibble(
  studyStartDate = c("19920101"), # From getDbCohortMethodDataArgs.studyPeriods[0].studyStartDate
  studyEndDate   = c("20211231")  # From getDbCohortMethodDataArgs.studyPeriods[0].studyEndDate
)

# Time-at-risks (TARs) for the outcomes of interest from Analysis Specifications: createStudyPopArgs.timeAtRisks
timeAtRisks <- tibble(
  label = c("1 day after cohort start to cohort end"), # Descriptive label for this TAR
  riskWindowStart  = c(1), # From createStudyPopArgs.timeAtRisks[0].riskWindowStart
  startAnchor = c("cohort start"), # From createStudyPopArgs.timeAtRisks[0].startAnchor
  riskWindowEnd  = c(0), # From createStudyPopArgs.timeAtRisks[0].riskWindowEnd
  endAnchor = c("cohort end"), # From createStudyPopArgs.timeAtRisks[0].endAnchor
  minDaysAtRisk = c(1) # From createStudyPopArgs.timeAtRisks[0].minDaysAtRisk
) 

# Propensity Score settings - match on PS from Analysis Specifications: propensityScoreAdjustment.psSettings
matchOnPsArgsList <- tibble(
  label = c("Match_MaxRatio100_Caliper0.2_StdLogit"), # Descriptive label for this matching setting
  maxRatio  = c(100), # From psSettings[0].matchOnPsArgs.maxRatio
  caliper = c(0.2), # From psSettings[0].matchOnPsArgs.caliper
  caliperScale  = c("standardized logit") # From psSettings[0].matchOnPsArgs.caliperScale
) 

# Propensity Score settings - stratify by PS from Analysis Specifications: propensityScoreAdjustment.psSettings
stratifyByPsArgsList <- tibble(
  label = c("Stratify_Strata5_BaseAll"), # Descriptive label for this stratification setting
  numberOfStrata  = c(5), # From psSettings[1].stratifyByPsArgs.numberOfStrata
  baseSelection = c("all") # From psSettings[1].stratifyByPsArgs.baseSelection
) 

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Convert "match on PS" settings into a configuration list
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

# Convert "stratify by PS" settings into a configuration list
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

# Define outcome list (independent of study period, TAR, PS settings)
# This is moved outside the loops as its content is static for all analyses.
outcomeList <- append(
  lapply(seq_len(nrow(oList)), function(i) {
    CohortMethod::createOutcome(
      outcomeId = oList$outcomeCohortId[i],
      outcomeOfInterest = TRUE,
      trueEffectSize = NA,
      priorOutcomeLookback = 99999
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

# Define target-comparator-outcome combinations (independent of study period, TAR, PS settings)
# This is moved outside the loops as its content is static for all analyses.
targetComparatorOutcomesList <- list()
for (i in seq_len(nrow(cmTcList))) {
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    # Exclude specific covariate concepts (from analysis specs).
    # The template's original inclusion of cmTcList$targetConceptId[i] etc. is removed
    # as cmTcList does not contain concept IDs and analysis specs indicate empty exclusion lists.
    excludedCovariateConceptIds = excludedCovariateConcepts$conceptId 
  )
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
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
      } else if (psCfg$method == "stratify") {
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings: uses default settings as per Analysis Specifications (conceptsToInclude/Exclude are empty)
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE,
        includedCovariateConceptIds = if (nrow(includedCovariateConcepts) > 0) includedCovariateConcepts$conceptId else c(),
        excludedCovariateConceptIds = if (nrow(excludedCovariateConcepts) > 0) excludedCovariateConcepts$conceptId else c()
      )

      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE, # From getDbCohortMethodDataArgs.restrictToCommonPeriod
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From getDbCohortMethodDataArgs.maxCohortSize
        firstExposureOnly = FALSE, # From getDbCohortMethodDataArgs.firstExposureOnly
        washoutPeriod = 0, # From getDbCohortMethodDataArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # From getDbCohortMethodDataArgs.removeDuplicateSubjects
        covariateSettings = covariateSettings
      )

      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att",
        prior = Cyclops::createPrior( # From propensityScoreAdjustment.createPsArgs.prior
          priorType = "laplace", # From prior.priorType
          exclude = c(0),
          useCrossValidation = TRUE # From prior.useCrossValidation
        ),
        control = Cyclops::createControl( # From propensityScoreAdjustment.createPsArgs.control
          noiseLevel = "silent", # From control.noiseLevel
          cvType = "auto", # From control.cvType
          seed = 1,
          resetCoefficients = TRUE, # From control.resetCoefficients
          tolerance = 2e-07, # From control.tolerance
          cvRepetitions = 10, # From control.cvRepetitions
          startingVariance = 0.01, # From control.startingVariance
          fold = 10 # From control.fold
        )
      )

      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From fitOutcomeModelArgs.modelType
        stratified = TRUE, # From fitOutcomeModelArgs.stratified
        useCovariates = FALSE, # From fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE, # From fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior( # From fitOutcomeModelArgs.prior
          priorType = "laplace", # From prior.priorType
          useCrossValidation = TRUE # From prior.useCrossValidation
        ),
        control = Cyclops::createControl( # From fitOutcomeModelArgs.control
          cvType = "auto", # From control.cvType
          seed = 1,
          resetCoefficients = TRUE, # From control.resetCoefficients
          startingVariance = 0.01, # From control.startingVariance
          tolerance = 2e-07, # From control.tolerance
          cvRepetitions = 10, # From control.cvRepetitions
          noiseLevel = "quiet", # From control.noiseLevel
          fold = 10 # From control.fold
        )
      )
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE, # From createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0, # From createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # From createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # From createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # From createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # From createStudyPopArgs.priorOutcomeLookback
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From createStudyPopArgs.timeAtRisks[0].minDaysAtRisk
        maxDaysAtRisk = 99999 # Template default, not explicitly in analysis specs
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
  targetComparatorOutcomesList = targetComparatorOutcomesList, # This is correctly generated once above the loops
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

ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "legendt2dm", "legendt2dmAnalysisSpecification.json") # Preserving original study name
)