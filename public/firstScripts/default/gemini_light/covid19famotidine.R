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
# Base URL for the OHDSI WebAPI (Atlas instance)
# This is not specified in the analysis specifications, using a common demo URL.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Extract cohort IDs and names from the analysis specifications.
# The template re-numbers these for internal use within Strategus modules.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1 (from analysis specifications)
    1794132, # Comparator: comparator1 (from analysis specifications)
    1794131  # Outcome: outcome1 (from analysis specifications)
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal consistency within the study package.
# Target cohort gets ID 1, Comparator gets ID 2, Outcome gets ID 3.
cohortDefinitionSet <- cohortDefinitionSet %>%
  mutate(cohortId = case_when(
    cohortId == 1794126 ~ 1, # Target cohort ID
    cohortId == 1794132 ~ 2, # Comparator cohort ID
    cohortId == 1794131 ~ 3, # Outcome cohort ID
    TRUE ~ cohortId # Keep other IDs as is if any
  ))

# Negative control outcomes
# The analysis specification provides a concept set ID for negative controls.
# We resolve this concept set to individual concepts and treat each as a negative outcome cohort.
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
  mutate(cohortId = row_number() + 100) %>% # Assign unique cohort IDs starting from 101 for negative controls
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs between study cohorts and negative controls.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Filter for the outcome cohort (re-numbered to 3)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Outcome cohort ID after re-numbering
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not specified in analysis spec

# Target and Comparator for the CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1, # Target cohort ID after re-numbering
  targetCohortName = cohortDefinitionSet$cohortName[cohortDefinitionSet$cohortId == 1],
  comparatorCohortId = 2, # Comparator cohort ID after re-numbering
  comparatorCohortName = cohortDefinitionSet$cohortName[cohortDefinitionSet$cohortId == 2]
)

# For the CohortMethod LSPS we'll need to exclude specific covariates.
# The analysis specification provides an empty list for conceptsToExclude,
# so this will be an empty data frame.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all
# The analysis specification provides an empty list for conceptsToInclude,
# so this will be an empty data frame.
includedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default from template, not specified in analysis spec
  detectOnDescendants = TRUE # Default from template, not specified in analysis spec
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Default from template, not specified in analysis spec
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE, # Default from template, not specified in analysis spec
  runIncludedSourceConcepts = TRUE, # Default from template, not specified in analysis spec
  runOrphanConcepts = TRUE, # Default from template, not specified in analysis spec
  runTimeSeries = FALSE, # Default from template, not specified in analysis spec
  runVisitContext = TRUE, # Default from template, not specified in analysis spec
  runBreakdownIndexEvents = TRUE, # Default from template, not specified in analysis spec
  runIncidenceRate = TRUE, # Default from template, not specified in analysis spec
  runCohortRelationship = TRUE, # Default from template, not specified in analysis spec
  runTemporalCohortCharacterization = TRUE, # Default from template, not specified in analysis spec
  minCharacterizationMean = 0.01 # Default from template, not specified in analysis spec
)

# CohortMethodModule -----------------------------------------------------------

# Study periods from analysis specifications (getDbCohortMethodDataArgs.studyPeriods)
studyPeriods <- tibble(
  studyStartDate = c("20200201"), # YYYYMMDD from analysis specifications
  studyEndDate   = c("20200530")  # YYYYMMDD from analysis specifications
)

# Time-at-risks (TARs) for the outcomes of interest in your study
# From analysis specifications (createStudyPopArgs.timeAtRisks)
timeAtRisks <- tibble(
  label = c("1-30 days from cohort start"), # Descriptive label for this TAR
  riskWindowStart  = c(1), # From analysis specifications
  startAnchor = c("cohort start"), # From analysis specifications
  riskWindowEnd  = c(30), # From analysis specifications
  endAnchor = c("cohort start"), # From analysis specifications
  minDaysAtRisk = c(1) # From analysis specifications
)

# Propensity Score settings - match on PS
# From analysis specifications (propensityScoreAdjustment.psSettings where matchOnPsArgs is not null)
matchOnPsArgsList <- tibble(
  label = c("1:1 Match on PS (caliper 0.2 standardized logit)"), # Descriptive label
  maxRatio  = c(1), # From analysis specifications
  caliper = c(0.2), # From analysis specifications
  caliperScale  = c("standardized logit") # From analysis specifications
)

# Propensity Score settings - stratify by PS
# From analysis specifications (propensityScoreAdjustment.psSettings where stratifyByPsArgs is not null)
stratifyByPsArgsList <- tibble(
  label = c("Stratify by PS (5 strata, all)"), # Descriptive label
  numberOfStrata  = c(5), # From analysis specifications
  baseSelection = c("all") # From analysis specifications
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

# Define covariate settings for FeatureExtraction.
# The analysis specification has empty lists for conceptsToInclude and conceptsToExclude,
# so we use default covariate settings.
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE # Default from template, not specified in analysis spec
)

# Define outcome list for CohortMethod.
# Includes the main outcome and all negative control outcomes.
outcomeList <- append(
  lapply(seq_len(nrow(oList)), function(i) {
    CohortMethod::createOutcome(
      outcomeId = oList$outcomeCohortId[i],
      outcomeOfInterest = TRUE,
      trueEffectSize = NA, # Not specified in analysis spec, keep NA
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

# Define target-comparator-outcomes list for CohortMethod.
# This specifies which T-C pairs are evaluated against which outcomes.
targetComparatorOutcomesList <- list()
for (i in seq_len(nrow(cmTcList))) {
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    # No specific concepts to exclude from covariates based on analysis specifications
    excludedCovariateConceptIds = c()
  )
}

# Iterate through all analysis setting combinations (study periods, time-at-risks, PS adjustments)
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Determine PS adjustment method (matching or stratification)
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default from template, not specified in analysis spec
          stratificationColumns = c() # Default from template, not specified in analysis spec
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default from template, not specified in analysis spec
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Arguments for fetching cohort method data from the database
      # Populated from analysis specifications (getDbCohortMethodDataArgs)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE, # From analysis specifications
        studyStartDate = studyStartDate, # From loop variable
        studyEndDate = studyEndDate,     # From loop variable
        maxCohortSize = 0, # From analysis specifications
        firstExposureOnly = TRUE, # From analysis specifications
        washoutPeriod = 0, # From analysis specifications
        removeDuplicateSubjects = "remove all", # From analysis specifications
        covariateSettings = covariateSettings # Defined above
      )

      # Arguments for creating propensity scores
      # Populated from analysis specifications (propensityScoreAdjustment.createPsArgs)
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From analysis specifications
        errorOnHighCorrelation = TRUE, # From analysis specifications
        stopOnError = FALSE, # Default from template, allowing Strategus to complete
        estimator = "att", # Default from template
        prior = Cyclops::createPrior(
          priorType = "laplace", # From analysis specifications
          exclude = c(0), # Default from template
          useCrossValidation = TRUE # From analysis specifications
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", # From analysis specifications
          cvType = "auto", # From analysis specifications
          seed = 1, # Default from template
          resetCoefficients = TRUE, # From analysis specifications
          tolerance = 2e-07, # From analysis specifications
          fold = 10, # From analysis specifications (propensityScoreAdjustment.createPsArgs.control.fold)
          cvRepetitions = 10, # From analysis specifications (propensityScoreAdjustment.createPsArgs.control.cvRepetitions)
          startingVariance = 0.01 # From analysis specifications
        )
      )

      # Arguments for computing covariate balance (shared and specific)
      # Not specified in analysis specifications, using template defaults.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting the outcome model
      # Populated from analysis specifications (fitOutcomeModelArgs)
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From analysis specifications
        stratified = TRUE, # From analysis specifications
        useCovariates = FALSE, # From analysis specifications
        inversePtWeighting = FALSE, # From analysis specifications
        prior = Cyclops::createPrior(
          priorType = "laplace", # From analysis specifications
          useCrossValidation = TRUE # From analysis specifications
        ),
        control = Cyclops::createControl(
          cvType = "auto", # From analysis specifications
          seed = 1, # Default from template
          resetCoefficients = TRUE, # From analysis specifications
          startingVariance = 0.01, # From analysis specifications
          tolerance = 2e-07, # From analysis specifications
          fold = 10, # From analysis specifications (fitOutcomeModelArgs.control.fold)
          cvRepetitions = 10, # From analysis specifications (fitOutcomeModelArgs.control.cvRepetitions)
          noiseLevel = "quiet" # From analysis specifications
        )
      )

      # Arguments for creating the study population
      # Populated from analysis specifications (createStudyPopArgs) and loop variables (timeAtRisks)
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From analysis specifications
        firstExposureOnly = FALSE, # From analysis specifications
        washoutPeriod = 0, # From analysis specifications
        removeDuplicateSubjects = "keep all", # From analysis specifications
        censorAtNewRiskWindow = FALSE, # From analysis specifications
        removeSubjectsWithPriorOutcome = FALSE, # From analysis specifications
        priorOutcomeLookback = 99999, # From analysis specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From loop variable
        startAnchor = timeAtRisks$startAnchor[t], # From loop variable
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From loop variable
        endAnchor = timeAtRisks$endAnchor[t], # From loop variable
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From loop variable
        maxDaysAtRisk = 99999 # Default from template, not specified in analysis spec
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
  analysesToExclude = NULL, # Not specified in analysis spec
  refitPsForEveryOutcome = FALSE, # Default from template, not specified in analysis spec
  refitPsForEveryStudyPopulation = FALSE, # Default from template, not specified in analysis spec
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Not specified in analysis spec
)

# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path uses the study name from the analysis specifications.
studyName <- "covid19famotidine" # From analysis specifications: name
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", studyName, paste0(studyName, "AnalysisSpecification.json"))
)