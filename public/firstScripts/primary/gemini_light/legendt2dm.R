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
# Annotation: Define the base URL for the OHDSI WebAPI (e.g., Atlas). 
# This URL is a placeholder and should be updated if running against a specific Atlas instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Annotation: Define the study name based on the analysis specifications.
studyName <- "legendt2dm"

# Annotation: Extract specific cohort IDs and names from the analysis specifications.
# These will be used to define and re-number cohorts for the study.
targetCohortIdSource <- 1794126
targetCohortNameSource <- "target1"
comparatorCohortIdSource <- 1794132
comparatorCohortNameSource <- "comparator1"
outcomeCohortIdSource <- 1794131
outcomeCohortNameSource <- "outcome1"

# Cohort Definitions
# Annotation: Export cohort definitions for target, comparator, and outcome from WebAPI.
# The `cohortIds` are taken directly from the "cohortDefinitions" section of the analysis specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    targetCohortIdSource,    # Target cohort
    comparatorCohortIdSource, # Comparator cohort
    outcomeCohortIdSource     # Outcome cohort
  ),
  generateStats = TRUE
)

# Annotation: Re-number cohort IDs for internal consistency within the study package.
# Strategus modules often expect specific integer IDs for T/C/O.
# Target cohort ID is re-numbered to 1.
cohortDefinitionSet[cohortDefinitionSet$cohortId == targetCohortIdSource, ]$cohortId <- 1
# Comparator cohort ID is re-numbered to 2.
cohortDefinitionSet[cohortDefinitionSet$cohortId == comparatorCohortIdSource, ]$cohortId <- 2
# Outcome cohort ID is re-numbered to 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == outcomeCohortIdSource, ]$cohortId <- 3

# Annotation: Define negative control outcomes from a concept set specified in the analysis.
# The `negativeControlConceptSet.id` (1888110) from the specifications is used to fetch the concept set.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # From analysis_spec$negativeControlConceptSet$id
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(
    baseUrl = baseUrl
  ) %>%
  ROhdsiWebApi::getConcepts(
    baseUrl = baseUrl
  ) %>%
  # Annotation: Rename columns and add a unique cohortId for negative controls.
  # Negative control cohort IDs start from 101 to avoid clashes with T/C/O (1, 2, 3).
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)

# Annotation: Check for duplicate cohort IDs to ensure uniqueness across all defined cohorts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Annotation: Prepare the outcomes list for CohortMethod analyses.
# Filters for the re-numbered outcome cohort ID (3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not explicitly in spec

# Annotation: Prepare the target and comparator list for CohortMethod analyses.
# Uses the re-numbered target (1) and comparator (2) cohort IDs and their original names.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = targetCohortNameSource, # "target1"
  comparatorCohortId = 2,
  comparatorCohortName = comparatorCohortNameSource # "comparator1"
)

# Annotation: Define concepts to be explicitly excluded from covariate analysis.
# The analysis specifications provide `covariateSelection.conceptsToExclude`.
# In this specific analysis, the concept ID is null, so this list will remain empty.
excludedCovariateConcepts <- data.frame(
  conceptId = as.numeric(c()), 
  conceptName = c()             
)
# If the `conceptsToExclude` in specifications were actual concepts, they would be added here.
# For example: if (length(analysis_spec$covariateSelection$conceptsToExclude[[1]]$id) > 0) { ... }

# Annotation: Define concepts to be explicitly included in covariate analysis.
# The analysis specifications provide `covariateSelection.conceptsToInclude`.
# In this specific analysis, the concept ID is null, so this list will remain empty.
includedCovariateConcepts <- data.frame(
  conceptId = as.numeric(c()), 
  conceptName = c()             
)
# If the `conceptsToInclude` in specifications were actual concepts, they would be added here.
# For example: if (length(analysis_spec$covariateSelection$conceptsToInclude[[1]]$id) > 0) { ... }


# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Annotation: Shared resource for cohort definitions, used by CohortGenerator and other modules.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Annotation: Shared resource for negative control outcomes.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Not specified in JSON, using template default
  detectOnDescendants = TRUE # Not specified in JSON, using template default
)
# Annotation: Module specifications for generating cohorts.
# `generateStats = TRUE` ensures cohort statistics are computed.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE 
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Annotation: Module specifications for running cohort diagnostics.
# All generated cohort IDs (T, C, O, and negative controls) are passed to CohortDiagnostics.
# Default settings from the template are used for most diagnostic types.
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId),
  runInclusionStatistics = TRUE, 
  runIncludedSourceConcepts = TRUE,
  runOrphanConcepts = TRUE,
  runTimeSeries = FALSE,
  runVisitContext = TRUE,
  runBreakdownIndexEvents = TRUE,
  runIncidenceRate = TRUE,
  runCohortRelationship = TRUE,
  runTemporalCohortCharacterization = TRUE,
  minCharacterizationMean = 0.01 # Not specified in JSON, using template default
)

# CohortMethodModule -----------------------------------------------------------

# Annotation: Define study periods based on `getDbCohortMethodDataArgs.studyPeriods` from analysis specifications.
studyPeriods <- tibble(
  studyStartDate = "19920101", # From analysis_spec$getDbCohortMethodDataArgs$studyPeriods[[1]]$studyStartDate
  studyEndDate   = "20211231"  # From analysis_spec$getDbCohortMethodDataArgs$studyPeriods[[1]]$studyEndDate
)

# Annotation: Define time-at-risks (TARs) for the outcomes, based on `createStudyPopArgs.timeAtRisks` from analysis specifications.
timeAtRisks <- tibble(
  label = c("TAR_S1cohort start_E0cohort end"), # Custom label derived from the TAR settings
  riskWindowStart  = 1,             # From analysis_spec$createStudyPopArgs$timeAtRisks[[1]]$riskWindowStart
  startAnchor = "cohort start",    # From analysis_spec$createStudyPopArgs$timeAtRisks[[1]]$startAnchor
  riskWindowEnd  = 0,               # From analysis_spec$createStudyPopArgs$timeAtRisks[[1]]$riskWindowEnd
  endAnchor = "cohort end"         # From analysis_spec$createStudyPopArgs$timeAtRisks[[1]]$endAnchor
) 

# Propensity Score settings - match on PS
# Annotation: Initialize empty tibbles for matchOnPsArgsList.
# The analysis specifications show `matchOnPsArgs` as null, so this list will remain empty.
matchOnPsArgsList <- tibble(
  label = character(),
  maxRatio  = numeric(),
  caliper = numeric(),
  caliperScale  = character()
) 

# Propensity Score settings - stratify by PS
# Annotation: Initialize tibble for stratifyByPsArgsList.
# The analysis specifications define `stratifyByPsArgs` with `numberOfStrata = 5` and `baseSelection = "all"`.
stratifyByPsArgsList <- tibble(
  label = c("Stratify_N5_BAll"), # Custom label for this stratification setting
  numberOfStrata  = c(5),         # From analysis_spec$propensityScoreAdjustment$psSettings[[1]]$stratifyByPsArgs$numberOfStrata
  baseSelection = c("all")        # From analysis_spec$propensityScoreAdjustment$psSettings[[1]]$stratifyByPsArgs$baseSelection
) 

# Build a single PS configuration list (each entry has: method, label, params)
# Annotation: Consolidate PS adjustment settings into a single list for iteration in the CohortMethod module.
psConfigList <- list()

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


# Iterate through all analysis setting combinations
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Annotation: Configure PS adjustment arguments (match or stratify) based on the current PS configuration.
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # From template default
          stratificationColumns = c() # From template default
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata, # 5 from spec
          stratificationColumns = c(), # From template default
          baseSelection = psCfg$params$baseSelection # "all" from spec
        )
      }

      # Annotation: Define covariate settings using FeatureExtraction's default.
      # `addDescendantsToExclude = TRUE` is the template default.
      # `excludedCovariateConceptIds` and `includedCovariateConceptIds` (if any) are handled in `createTargetComparatorOutcomes`.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )
      
      # Annotation: Prepare a list of outcomes, including the main study outcome and negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i], # 3 (re-numbered)
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Default for outcomes of interest
            priorOutcomeLookback = 99999 # From spec `createStudyPopArgs.priorOutcomeLookBack`
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
      
      # Annotation: Create TargetComparatorOutcomes specifications.
      # This combines T/C pairs with the list of outcomes and specifies covariate exclusions.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i], # 1 (re-numbered)
          comparatorId = cmTcList$comparatorCohortId[i], # 2 (re-numbered)
          outcomes = outcomeList,
          # Annotation: Excluded covariate concepts are derived from `covariateSelection.conceptsToExclude` in the specifications.
          # Since the spec's `conceptsToExclude` is an empty list of null IDs, `excludedCovariateConcepts$conceptId` will be empty.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId 
        )
      }

      # Annotation: Configure `getDbCohortMethodDataArgs` using parameters from analysis specifications.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Template default. Not explicitly specified in JSON for getDbCohortMethodDataArgs.
        studyStartDate = studyStartDate, # "19920101" from `getDbCohortMethodMethodDataArgs.studyPeriods` in spec
        studyEndDate = studyEndDate,     # "20211231" from `getDbCohortMethodMethodDataArgs.studyPeriods` in spec
        maxCohortSize = 0, # From analysis_spec$getDbCohortMethodDataArgs$maxCohortSize
        covariateSettings = covariateSettings # Uses the default covariate settings created above
      )

      # Annotation: Configure `createPsArgs` for propensity score estimation.
      # Parameters are taken from `propensityScoreAdjustment.createPsArgs` in analysis specifications.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From analysis_spec$propensityScoreAdjustment$createPsArgs$maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From analysis_spec$propensityScoreAdjustment$createPsArgs$errorOnHighCorrelation
        stopOnError = FALSE, # Template default, recommended for Strategus to allow other analyses to complete
        estimator = "att", # Template default, not specified in JSON
        prior = Cyclops::createPrior( # Prior settings from spec
          priorType = "laplace", # From analysis_spec$propensityScoreAdjustment$createPsArgs$prior$priorType
          exclude = c(0), # Template default
          useCrossValidation = TRUE # From analysis_spec$propensityScoreAdjustment$createPsArgs$prior$useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings from spec
          noiseLevel = "silent", # From analysis_spec$propensityScoreAdjustment$createPsArgs$control$noiseLevel
          cvType = "auto", # From analysis_spec$propensityScoreAdjustment$createPsArgs$control$cvType
          seed = 1, # Template default
          resetCoefficients = TRUE, # From analysis_spec$propensityScoreAdjustment$createPsArgs$control$resetCoefficients
          tolerance = 2e-07, # From analysis_spec$propensityScoreAdjustment$createPsArgs$control$tolerance
          cvRepetitions = 10, # From analysis_spec$propensityScoreAdjustment$createPsArgs$control$fold (mapped to cvRepetitions)
          startingVariance = 0.01 # From analysis_spec$propensityScoreAdjustment$createPsArgs$control$startingVariance
        )
      )

      # Annotation: Balance computation arguments.
      # These are default values from the template, not explicitly specified in the JSON.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Annotation: Configure `fitOutcomeModelArgs` for outcome model fitting.
      # Parameters are taken from `fitOutcomeModelArgs` in analysis specifications.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From analysis_spec$fitOutcomeModelArgs$modelType
        stratified = TRUE, # From analysis_spec$fitOutcomeModelArgs$stratified
        useCovariates = FALSE, # From analysis_spec$fitOutcomeModelArgs$useCovariates
        inversePtWeighting = FALSE, # From analysis_spec$fitOutcomeModelArgs$inversePtWeighting
        prior = Cyclops::createPrior( # Prior settings from spec
          priorType = "laplace", # From analysis_spec$fitOutcomeModelArgs$prior$priorType
          useCrossValidation = TRUE # From analysis_spec$fitOutcomeModelArgs$prior$useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings from spec
          cvType = "auto", # From analysis_spec$fitOutcomeModelArgs$control$cvType
          seed = 1, # Template default
          resetCoefficients = TRUE, # From analysis_spec$fitOutcomeModelArgs$control$resetCoefficients
          startingVariance = 0.01, # From analysis_spec$fitOutcomeModelArgs$control$startingVariance
          tolerance = 2e-07, # From analysis_spec$fitOutcomeModelArgs$control$tolerance
          cvRepetitions = 10, # From analysis_spec$fitOutcomeModelArgs$control$fold (mapped to cvRepetitions)
          noiseLevel = "quiet" # From analysis_spec$fitOutcomeModelArgs$control$noiseLevel
        )
      )
      
      # Annotation: Configure `createStudyPopArgs` for creating the study population.
      # Parameters are taken from `createStudyPopArgs` in analysis specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From analysis_spec$createStudyPopArgs$restrictToCommonPeriod
        firstExposureOnly = TRUE, # From analysis_spec$createStudyPopArgs$firstExposureOnly
        washoutPeriod = 365, # From analysis_spec$createStudyPopArgs$washoutPeriod
        removeDuplicateSubjects = "keep all", # From analysis_spec$createStudyPopArgs$removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # From analysis_spec$createStudyPopArgs$censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # From analysis_spec$createStudyPopArgs$removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # From analysis_spec$createStudyPopArgs$priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From current TAR iteration (1)
        startAnchor = timeAtRisks$startAnchor[t], # From current TAR iteration ("cohort start")
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From current TAR iteration (0)
        endAnchor = timeAtRisks$endAnchor[t], # From current TAR iteration ("cohort end")
        minDaysAtRisk = 1, # From analysis_spec$createStudyPopArgs$timeAtRisks[[1]]$minDaysAtRisk
        maxDaysAtRisk = 99999 # Template default, not specified in JSON
      )

      # Append the settings to Analysis List
      # Annotation: Create a CohortMethod analysis object with all configured arguments.
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
# Annotation: Create the CohortMethod module specifications with the list of CM analyses and TCOs.
# Default Strategus parameters for refitting PS and diagnostic thresholds are used.
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # From template default
  refitPsForEveryOutcome = FALSE, # From template default
  refitPsForEveryStudyPopulation = FALSE, # From template default
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # From template default
)

# Create the analysis specifications ------------------------------------------
# Annotation: Assemble all shared resources and module specifications into a final Strategus analysis specifications object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Annotation: Save the complete analysis specifications to a JSON file.
# The file path uses the `studyName` extracted from the analysis specifications ("legendt2dm").
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", studyName, paste0(studyName, "AnalysisSpecification.json"))
)