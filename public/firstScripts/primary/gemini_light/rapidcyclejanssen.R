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
library(tibble) # For tribble

# Shared Resources -------------------------------------------------------------
# Annotation: Base URL for OHDSI WebAPI. This is a placeholder as it's not specified in analysis specifications.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Annotation: Creating a tibble to represent the cohort definitions based on the 
# analysis specifications. In a real scenario, ROhdsiWebApi::exportCohortDefinitionSet 
# would fetch these from a WebAPI.
cohortDefinitionSet <- tibble::tribble(
  ~cohortId,   ~cohortName,
  1794126,     "target1",
  1794132,     "comparator1",
  1794131,     "outcome1"
) %>% 
# Annotation: Adding dummy columns to mimic the output structure of 
# ROhdsiWebApi::exportCohortDefinitionSet with `generateStats = TRUE`.
mutate(
  sql = "DUMMY SQL",
  json = "DUMMY JSON",
  logicDescription = "DUMMY DESCRIPTION",
  generateStats = TRUE
)

# Re-number cohorts for internal study use
# Annotation: Mapping the original WebAPI cohort IDs to simplified sequential IDs (1, 2, 3) 
# for use within the study for clarity and consistency.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Target Cohort
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Comparator Cohort
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Outcome Cohort

# Annotation: Updating cohort names to reflect their roles after re-numbering.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1,]$cohortName <- "target1"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 2,]$cohortName <- "comparator1"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 3,]$cohortName <- "outcome1"

# Negative control outcomes
# Annotation: Creating a tibble for negative control outcomes based on the 
# negativeControlConceptSet from the analysis specifications.
# In a real scenario, ROhdsiWebApi functions would be used to resolve the concept set.
negativeControlOutcomeCohortSet <- tibble::tribble(
  ~outcomeConceptId, ~cohortName,
  1888110,           "negative" 
) %>%
  # Annotation: Assigning unique IDs starting from 101 for negative controls 
  # to avoid conflicts with study cohorts (1, 2, 3).
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)

# Annotation: Check for duplicate cohort IDs to ensure uniqueness across all defined cohorts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: 
# Annotation: Filtering for the main outcome cohort (ID 3) and preparing it for the analysis.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Placeholder from template, not specified in JSON

# Target and Comparator for the CohortMethod analysis 
# Annotation: Defining the target (ID 1) and comparator (ID 2) cohorts for the CohortMethod analysis.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study - based on analysis spec, this is an empty list.
# Annotation: This variable is intended to hold concept IDs for covariates that should be excluded from the model. 
# In this analysis, the 'conceptsToExclude' from the analysis specifications is empty.
excludedCovariateConcepts <- c() # From JSON covariateSelection.conceptsToExclude, which is empty.

# Annotation: 'conceptsToInclude' from analysis specifications is empty, so this remains commented.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# Annotation: Initializing CohortGeneratorModule settings.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Annotation: Creating shared resource specifications for cohort definitions.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Annotation: Creating shared resource specifications for negative control outcomes.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default from template, not in JSON
  detectOnDescendants = TRUE # Default from template, not in JSON
)
# Annotation: Creating module specifications for the CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Default from template, not in JSON
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# Annotation: Initializing CohortDiagnosticsModule settings.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Annotation: Creating module specifications for CohortDiagnostics.
# Cohort IDs are all defined study and negative control cohorts.
# Other parameters use template defaults as they are not in the JSON.
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
  minCharacterizationMean = 0.01
)

# CohortMethodModule -----------------------------------------------------------

# Annotation: Defining study periods for data extraction based on `getDbCohortMethodDataArgs.studyPeriods` from JSON.
# An empty string for studyEndDate means no upper limit on the study period.
studyPeriods <- tibble::tribble(
  ~studyStartDate, ~studyEndDate,
  20210101,        "" # YYYYMMDD. Empty string for NULL/no end date from JSON.
)

# Annotation: Defining the time-at-risk (TAR) windows for outcome ascertainment 
# based on `createStudyPopArgs.timeAtRisks` from JSON.
# 'cohort start' refers to the index date of the target/comparator cohort.
timeAtRisks <- tibble::tribble(
  ~label,           ~riskWindowStart, ~startAnchor,     ~riskWindowEnd, ~endAnchor, ~minDaysAtRisk,
  "1-14_cohort_start", 1,             "cohort start",   14,             "cohort start", 1 # From JSON
) 

# Propensity Score settings - match on PS
# Annotation: Propensity score matching settings based on `propensityScoreAdjustment.psSettings` from JSON.
# caliperScale 'standardized logit' indicates matching on the standardized logit of the propensity score.
matchOnPsArgsList <- tibble::tribble(
  ~label,                         ~maxRatio, ~caliper, ~caliperScale,
  "MatchOnPs_0.2stdlogit_100ratio", 100,       0.2,      "standardized logit" # From JSON
) 

# Propensity Score settings - stratify by PS
# Annotation: No propensity score stratification settings specified in the analysis specifications.
stratifyByPsArgsList <- tibble::tibble(
  label = character(), numberOfStrata = integer(), baseSelection = character()
) 

# Build a single PS configuration list (each entry has: method, label, params)
# Annotation: Consolidating propensity score adjustment settings (matching or stratification) 
# into a single list for iterative processing.
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

# Annotation: Looping through each study period, time-at-risk, and PS configuration 
# to create a comprehensive list of CohortMethod analyses.
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Annotation: Configuring PS matching or stratification arguments based on the current PS configuration.
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

      # Annotation: Default covariate settings for feature extraction.
      # 'addDescendantsToExclude = TRUE' ensures that descendants of any excluded concepts are also excluded.
      # Note: 'conceptsToInclude' and 'conceptsToExclude' from analysis specifications are empty, 
      # so only default covariates will be generated, with specific drug concepts potentially excluded later.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default from template
      )

      # Annotation: Combining main outcome and negative control outcomes into a single list.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Default from template
            priorOutcomeLookback = 99999 # Default from template
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # Default from template for negative controls
          )
        })
      )

      # Annotation: Defining the specific target-comparator-outcome combinations for the analysis.
      # It links the target (1) and comparator (2) cohorts with the defined outcomes (3 and negative controls).
      # `excludedCovariateConceptIds` from analysis specifications is empty.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Annotation: No specific covariate concept IDs are excluded from the PS model based on the analysis specifications.
          # The template had placeholders for target/comparator concepts, but these are not present in the provided JSON.
          excludedCovariateConceptIds = excludedCovariateConcepts # Empty vector based on JSON
        )
      }

      # Annotation: Arguments for extracting data from the CDM for CohortMethod.
      # 'restrictToCommonPeriod = TRUE' ensures that the study is limited to the period where both target and comparator cohorts exist.
      # 'maxCohortSize = 0' means no limit on cohort size during data extraction.
      # 'studyStartDate' and 'studyEndDate' are dynamically set from the studyPeriods iteration.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Not in JSON for this arg, defaulting to TRUE as per template
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From JSON
        covariateSettings = covariateSettings
      )

      # Annotation: Arguments for creating propensity scores.
      # 'maxCohortSizeForFitting' limits the number of subjects used to fit the PS model.
      # 'errorOnHighCorrelation = TRUE' will stop if covariates are highly correlated.
      # 'stopOnError = FALSE' allows the Strategus module to continue even if PS model fitting fails for some analysis.
      # 'prior' defines the regularization scheme (Laplace with cross-validation).
      # 'control' specifies optimization settings for Cyclops (e.g., tolerance, cross-validation type, folds).
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From JSON
        errorOnHighCorrelation = TRUE, # From JSON
        stopOnError = FALSE, # Default from template
        estimator = "att", # Default from template
        prior = Cyclops::createPrior(
          priorType = "laplace", # From JSON
          exclude = c(0), # Default from template
          useCrossValidation = TRUE # From JSON
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", # From JSON
          cvType = "auto", # From JSON
          seed = 1, # Default from template
          resetCoefficients = TRUE, # From JSON
          tolerance = 2e-07, # From JSON
          cvRepetitions = 10, # From JSON
          fold = 10, # From JSON (implicit with cvType="auto" and cvRepetitions)
          startingVariance = 0.01 # From JSON
        )
      )

      # Annotation: Arguments for computing covariate balance for shared covariates. Using template defaults.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default from template
        covariateFilter = NULL # Default from template
      )
      # Annotation: Arguments for computing covariate balance, using default Table 1 specifications.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default from template
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default from template
      )

      # Annotation: Arguments for fitting the outcome model (Cox proportional hazards model).
      # 'modelType = "cox"' specifies a Cox regression model.
      # 'stratified = TRUE' means the model is stratified by propensity score strata or matched sets.
      # 'useCovariates = FALSE' indicates that covariates are not directly included in the outcome model.
      # 'inversePtWeighting = FALSE' indicates not using inverse probability of treatment weighting.
      # 'prior' defines the regularization scheme (Laplace with cross-validation).
      # 'control' specifies optimization settings for Cyclops.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From JSON
        stratified = TRUE, # From JSON
        useCovariates = FALSE, # From JSON
        inversePtWeighting = FALSE, # From JSON
        prior = Cyclops::createPrior(
          priorType = "laplace", # From JSON
          useCrossValidation = TRUE # From JSON
        ),
        control = Cyclops::createControl(
          cvType = "auto", # From JSON
          seed = 1, # Default from template
          resetCoefficients = TRUE, # From JSON
          startingVariance = 0.01, # From JSON
          tolerance = 2e-07, # From JSON
          cvRepetitions = 10, # From JSON
          fold = 10, # From JSON (implicit with cvType="auto" and cvRepetitions)
          noiseLevel = "quiet" # From JSON
        )
      )
      
      # Annotation: Arguments for creating the study population from the extracted cohorts.
      # All parameters are directly mapped from `createStudyPopArgs` in the JSON,
      # with time-at-risk details coming from the current iteration.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From JSON
        firstExposureOnly = TRUE, # From JSON
        washoutPeriod = 365, # From JSON
        removeDuplicateSubjects = "remove all", # From JSON
        censorAtNewRiskWindow = FALSE, # From JSON
        removeSubjectsWithPriorOutcome = TRUE, # From JSON
        priorOutcomeLookback = 99999, # From JSON
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From `timeAtRisks` defined above
        maxDaysAtRisk = 99999 # Default from template
      )


      # Append the settings to Analysis List
      # Annotation: Creating a CohortMethod analysis object with all configured settings.
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

# Annotation: Initializing CohortMethodModule settings.
cmModuleSettingsCreator <- CohortMethodModule$new()
# Annotation: Creating module specifications for the CohortMethod module.
# All `cmAnalysisList` configurations are included.
# Other parameters use template defaults as they are not in the JSON.
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Default from template
  refitPsForEveryOutcome = FALSE, # Default from template
  refitPsForEveryStudyPopulation = FALSE, # Default from template
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default from template
)

# Create the analysis specifications ------------------------------------------
# Annotation: Building the complete Strategus analysis specifications object.
# It includes shared resources (cohorts, negative controls) and module-specific settings 
# for CohortGenerator, CohortDiagnostics, and CohortMethod.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Annotation: Saving the generated analysis specifications to a JSON file.
# The file path uses the study name "rapidcyclejanssen" from the analysis specifications JSON.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "rapidcyclejanssen", "rapidcyclejanssenAnalysisSpecification.json")
)