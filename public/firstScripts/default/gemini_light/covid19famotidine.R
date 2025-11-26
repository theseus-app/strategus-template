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

# Shared Resources -------------------------------------------------------------
# Base URL for the WebAPI to retrieve cohort definitions and concept sets.
# Note: For production use, you might set this to an environment variable or configuration.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions from analysis specifications
# These cohorts will be used as Target, Comparator, and Outcomes.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: "target1"
    1794132, # Comparator: "comparator1"
    1794131  # Outcome: "outcome1"
  ),
  generateStats = TRUE
)

# Re-number cohorts to standard IDs (1 for Target, 2 for Comparator, 3 for Outcome)
# This mapping simplifies downstream processing in CohortMethod.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Map Target (id: 1794126) to 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Map Comparator (id: 1794132) to 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Map Outcome (id: 1794131) to 3

# Negative control outcomes from analysis specifications
# These are used to estimate the residual bias in the study.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Negative control concept set ID from analysis specifications
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
  # Assign unique cohort IDs for negative controls, starting from 101
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)


if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found. Ensure all cohort IDs are unique. ***")
}

# Create data frames to hold the cohorts used in each analysis -----------------
# Outcomes: Extract outcome cohort (ID 3) from the re-numbered cohortDefinitionSet
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Filter for the re-numbered outcome cohort (ID 3)
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window for outcomes

# Target and Comparator for the CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered Target cohort ID
  targetCohortName = cohortDefinitionSet[cohortDefinitionSet$cohortId == 1,]$cohortName, # Original name of Target cohort
  comparatorCohortId = 2, # Re-numbered Comparator cohort ID
  comparatorCohortName = cohortDefinitionSet[cohortDefinitionSet$cohortId == 2,]$cohortName # Original name of Comparator cohort
)

# For the CohortMethod analysis, we can specify covariates to exclude.
# From analysis specifications, "conceptsToExclude" is empty (id: null, name: "").
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all
# From analysis specifications, "conceptsToInclude" is empty (id: null, name: "").
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# This module generates the cohorts specified in the cohortDefinitionSet.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Detect the first occurrence of the negative control outcome
  detectOnDescendants = TRUE # Detect occurrences on descendants of the negative control concept
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Generate cohort statistics
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# This module runs diagnostics on the generated cohorts.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # All cohort IDs (target, comparator, outcomes) for diagnostics
  runInclusionStatistics = TRUE,
  runIncludedSourceConcepts = TRUE,
  runOrphanConcepts = TRUE,
  runTimeSeries = FALSE,
  runVisitContext = TRUE,
  runBreakdownIndexEvents = TRUE,
  runIncidenceRate = TRUE,
  runCohortRelationship = TRUE,
  runTemporalCohortCharacterization = TRUE,
  minCharacterizationMean = 0.01 # Minimum mean for characterization covariates
)

# CohortMethodModule -----------------------------------------------------------

# Study periods from analysis specifications (getDbCohortMethodDataArgs.studyPeriods)
studyPeriods <- tibble(
  studyStartDate = c("20200201"), # YYYYMMDD
  studyEndDate   = c("20200530")  # YYYYMMDD
)

# Time-at-risks (TARs) for the outcomes of interest in your study
# From analysis specifications (createStudyPopArgs.timeAtRisks)
timeAtRisks <- tibble(
  label = c("TAR 1-30 days"), # Descriptive label for the TAR
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd  = c(30),
  endAnchor = c("cohort start"), # "cohort start" | "cohort end"
  minDaysAtRisk = c(1) # Minimum days at risk from analysis specifications
) 

# Propensity Score settings - match on PS
# From analysis specifications (propensityScoreAdjustment.psSettings)
matchOnPsArgsList <- tibble(
  label = c("1:1 Match on PS Caliper 0.2 Std Logit"), # Descriptive label
  maxRatio  = c(1), # Max ratio from analysis specifications
  caliper = c(0.2), # Caliper from analysis specifications
  caliperScale  = c("standardized logit") # Caliper scale from analysis specifications
) 

# Propensity Score settings - stratify by PS
# From analysis specifications (propensityScoreAdjustment.psSettings)
stratifyByPsArgsList <- tibble(
  label = c("Stratify by 5 PS Strata All"), # Descriptive label
  numberOfStrata  = c(5), # Number of strata from analysis specifications
  baseSelection = c("all") # Base selection from analysis specifications
) 

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Convert "match on PS" settings from tibble to list format
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

# Convert "stratify by PS" settings from tibble to list format
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

# Iterate through all analysis setting combinations to create CohortMethod analysis definitions
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {
    # Extract TAR settings for current iteration
    current_riskWindowStart <- timeAtRisks$riskWindowStart[t]
    current_startAnchor <- timeAtRisks$startAnchor[t]
    current_riskWindowEnd <- timeAtRisks$riskWindowEnd[t]
    current_endAnchor <- timeAtRisks$endAnchor[t]
    current_minDaysAtRisk <- timeAtRisks$minDaysAtRisk[t]

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Define PS adjustment arguments based on the current PS configuration
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c() # No stratification columns specified in analysis specifications
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # No stratification columns specified in analysis specifications
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings: Use default settings as no specific inclusions/exclusions are provided in analysis specifications
      # The template's addDescendantsToExclude=TRUE is a good default.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE,
        excludedCovariateConceptIds = excludedCovariateConcepts$conceptId # Empty if no specific concepts to exclude
      )

      # Outcome list: Combine main outcomes and negative control outcomes
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # For true outcomes, effect size is unknown
            priorOutcomeLookback = 99999 # From analysis specifications (createStudyPopArgs.priorOutcomeLookBack for outcomes)
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE, # Negative controls are not outcomes of interest
            trueEffectSize = 1 # True effect size for negative controls is assumed to be 1
          )
        })
      )
      
      # Target-Comparator-Outcome combinations
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude the study concepts themselves from covariates, if specified,
          # and any other concepts listed in 'excludedCovariateConcepts'
          excludedCovariateConceptIds = c(
            excludedCovariateConcepts$conceptId # From analysis specifications (covariateSelection.conceptsToExclude)
          )
        )
      }

      # Arguments for getting cohort method data from the database
      # Values extracted from analysis specifications (getDbCohortMethodDataArgs)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE, # From analysis specifications
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From analysis specifications
        firstExposureOnly = TRUE, # From analysis specifications
        washoutPeriod = 0, # From analysis specifications
        removeDuplicateSubjects = "remove all", # From analysis specifications
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores
      # Values extracted from analysis specifications (propensityScoreAdjustment.createPsArgs)
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From analysis specifications
        errorOnHighCorrelation = TRUE, # From analysis specifications
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att", # Default in template, not specified in analysis spec
        prior = Cyclops::createPrior(
          priorType = "laplace", # From analysis specifications (prior.priorType)
          exclude = c(0), # Exclude intercept from regularization
          useCrossValidation = TRUE # From analysis specifications (prior.useCrossValidation)
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", # From analysis specifications (control.noiseLevel)
          cvType = "auto", # From analysis specifications (control.cvType)
          seed = 1, # Default in template, not specified in analysis spec
          resetCoefficients = TRUE, # From analysis specifications (control.resetCoefficients)
          tolerance = 2e-07, # From analysis specifications (control.tolerance)
          cvRepetitions = 10, # From analysis specifications (control.cvRepetitions)
          startingVariance = 0.01 # From analysis specifications (control.startingVariance)
        )
      )

      # Arguments for computing shared covariate balance - using default as not specified in analysis spec
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      # Arguments for computing covariate balance - using default as not specified in analysis spec
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting the outcome model
      # Values extracted from analysis specifications (fitOutcomeModelArgs)
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From analysis specifications (modelType)
        stratified = TRUE, # From analysis specifications (stratified)
        useCovariates = FALSE, # From analysis specifications (useCovariates)
        inversePtWeighting = FALSE, # From analysis specifications (inversePtWeighting)
        prior = Cyclops::createPrior(
          priorType = "laplace", # From analysis specifications (prior.priorType)
          useCrossValidation = TRUE # From analysis specifications (prior.useCrossValidation)
        ),
        control = Cyclops::createControl(
          cvType = "auto", # From analysis specifications (control.cvType)
          seed = 1, # Default in template, not specified in analysis spec
          resetCoefficients = TRUE, # From analysis specifications (control.resetCoefficients)
          startingVariance = 0.01, # From analysis specifications (control.startingVariance)
          tolerance = 2e-07, # From analysis specifications (control.tolerance)
          cvRepetitions = 10, # From analysis specifications (control.cvRepetitions)
          noiseLevel = "quiet" # From analysis specifications (control.noiseLevel)
        )
      )

      # Arguments for creating the study population
      # Values extracted from analysis specifications (createStudyPopArgs)
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From analysis specifications
        firstExposureOnly = FALSE, # From analysis specifications
        washoutPeriod = 0, # From analysis specifications
        removeDuplicateSubjects = "keep all", # From analysis specifications
        censorAtNewRiskWindow = FALSE, # From analysis specifications
        removeSubjectsWithPriorOutcome = FALSE, # From analysis specifications
        priorOutcomeLookback = 99999, # From analysis specifications
        riskWindowStart = current_riskWindowStart, # From current TAR iteration
        startAnchor = current_startAnchor, # From current TAR iteration
        riskWindowEnd = current_riskWindowEnd, # From current TAR iteration
        endAnchor = current_endAnchor, # From current TAR iteration
        minDaysAtRisk = current_minDaysAtRisk, # From current TAR iteration
        maxDaysAtRisk = 99999 # Not specified in analysis spec, using large default
      )

      # Append the settings to the CohortMethod analysis list
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study Dates: %s-%s; TAR: %s; PS Adjustment: %s",
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

# CohortMethod Module Specifications
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # No specific analyses to exclude
  refitPsForEveryOutcome = FALSE, # Default, not specified in analysis spec
  refitPsForEveryStudyPopulation = FALSE, # Default, not specified in analysis spec
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Using default diagnostic thresholds
)

# Create the overall analysis specifications for Strategus ---------------------
# Combine all shared resources and module specifications.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file
# The study name is "covid19famotidine" from analysis specifications
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "covid19famotidine", "covid19famotidineAnalysisSpecification.json")
)