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
library(tibble)

# Placeholder for Analysis Specifications JSON - In a real scenario, this would be loaded
# from a JSON file or passed as an object. For this script, we'll embed the structure
# to directly extract values.
# Note: In a real Strategus setup, this 'analysisSpecificationsJson' would not be
# defined directly in the script but passed as an input.
analysisSpecificationsJson <- list(
  name = "uveitissafety",
  cohortDefinitions = list(
    targetCohort = list(
      id = 1794126,
      name = "target1"
    ),
    comparatorCohort = list(
      id = 1794132,
      name = "comparator1"
    ),
    outcomeCohort = list(
      list(
        id = 1794131,
        name = "outcome1"
      )
    )
  ),
  negativeControlConceptSet = list(
    id = 1888110,
    name = "negative"
  ),
  covariateSelection = list(
    conceptsToInclude = list(
      list(
        id = NULL,
        name = ""
      )
    ),
    conceptsToExclude = list(
      list(
        id = NULL,
        name = ""
      )
    )
  ),
  getDbCohortMethodDataArgs = list(
    studyPeriods = list(
      list(
        studyStartDate = NULL,
        studyEndDate = NULL
      )
    ),
    maxCohortSize = 0
  ),
  createStudyPopArgs = list(
    restrictToCommonPeriod = TRUE,
    firstExposureOnly = TRUE,
    washoutPeriod = 365,
    removeDuplicateSubjects = "keep all",
    censorAtNewRiskWindow = TRUE,
    removeSubjectsWithPriorOutcome = TRUE,
    priorOutcomeLookBack = 99999,
    timeAtRisks = list(
      list(
        riskWindowStart = 1,
        startAnchor = "cohort start",
        riskWindowEnd = 0,
        endAnchor = "cohort end",
        minDaysAtRisk = 1
      )
    )
  ),
  propensityScoreAdjustment = list(
    psSettings = list(
      list(
        matchOnPsArgs = list(
          maxRatio = 10,
          caliper = 0.2,
          caliperScale = "standardized logit"
        ),
        stratifyByPsArgs = NULL
      )
    ),
    createPsArgs = list(
      maxCohortSizeForFitting = 250000,
      errorOnHighCorrelation = TRUE,
      prior = list(
        priorType = "laplace",
        useCrossValidation = TRUE
      ),
      control = list(
        tolerance = 2e-7,
        cvType = "auto",
        fold = 10,
        cvRepetitions = 10,
        noiseLevel = "silent",
        resetCoefficients = TRUE,
        startingVariance = 0.01
      )
    )
  ),
  fitOutcomeModelArgs = list(
    modelType = "cox",
    stratified = TRUE,
    useCovariates = FALSE,
    inversePtWeighting = FALSE,
    prior = list(
      priorType = "laplace",
      useCrossValidation = TRUE
    ),
    control = list(
      tolerance = 2e-7,
      cvType = "auto",
      fold = 10,
      cvRepetitions = 10,
      noiseLevel = "quiet",
      resetCoefficients = TRUE,
      startingVariance = 0.01
    )
  ),
  maxCohortSize = 0
)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
# Base URL for OHDSI WebAPI. This is a demo URL, replace with your institution's WebAPI.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Extract cohort IDs from the analysis specifications
targetCohortId <- analysisSpecificationsJson$cohortDefinitions$targetCohort$id
comparatorCohortId <- analysisSpecificationsJson$cohortDefinitions$comparatorCohort$id
outcomeCohortId <- analysisSpecificationsJson$cohortDefinitions$outcomeCohort[[1]]$id

# Cohort Definitions: Export cohort definitions from WebAPI using their original IDs.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    targetCohortId, # Target Cohort ID from specifications
    comparatorCohortId, # Comparator Cohort ID from specifications
    outcomeCohortId # Outcome Cohort ID from specifications
  ),
  generateStats = TRUE
)

# Re-number cohorts for consistent internal use (1=target, 2=comparator, 3=outcome).
# This simplifies referencing them within the study.
cohortDefinitionSet[cohortDefinitionSet$cohortId == targetCohortId,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == targetCohortId,]$cohortName <- analysisSpecificationsJson$cohortDefinitions$targetCohort$name
cohortDefinitionSet[cohortDefinitionSet$cohortId == comparatorCohortId,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == comparatorCohortId,]$cohortName <- analysisSpecificationsJson$cohortDefinitions$comparatorCohort$name
cohortDefinitionSet[cohortDefinitionSet$cohortId == outcomeCohortId,]$cohortId <- 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == outcomeCohortId,]$cohortName <- analysisSpecificationsJson$cohortDefinitions$outcomeCohort[[1]]$name

# Negative control outcomes: Resolve concept set for negative controls.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = analysisSpecificationsJson$negativeControlConceptSet$id, # Negative control concept set ID from specifications
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
  mutate(cohortId = row_number() + 100) %>% # Assign cohort IDs starting from 101 for negative controls to avoid collision with T/C/O
  select(cohortId, cohortName, outcomeConceptId)


if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found among study cohorts and negative controls ***")
}

# Create data frames to hold the cohorts used in each analysis ---------------
# Outcomes: Filter for the main outcome cohort (re-numbered to ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # cleanWindow is not in spec, using template default.

# Target and Comparator for the CohortMethod analysis.
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered target cohort ID
  targetCohortName = analysisSpecificationsJson$cohortDefinitions$targetCohort$name,
  comparatorCohortId = 2, # Re-numbered comparator cohort ID
  comparatorCohortName = analysisSpecificationsJson$cohortDefinitions$comparatorCohort$name
)

# Covariates to exclude from analysis. If 'conceptsToExclude' is an empty list in spec,
# an empty data frame is created.
if (length(analysisSpecificationsJson$covariateSelection$conceptsToExclude) > 0 &&
    !is.null(analysisSpecificationsJson$covariateSelection$conceptsToExclude[[1]]$id)) {
  excludedCovariateConcepts <- do.call(rbind, lapply(analysisSpecificationsJson$covariateSelection$conceptsToExclude, as.data.frame))
} else {
  excludedCovariateConcepts <- data.frame(conceptId = integer(0), conceptName = character(0))
}

# Optional: If you want to define covariates to include instead of including them all
# Based on analysis specifications, conceptsToInclude is empty, so this block remains commented.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Shared resource for primary study cohorts (T, C, O)
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Shared resource for negative control outcome cohorts
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Not specified in input, using template default.
  detectOnDescendants = TRUE # Not specified in input, using template default.
)
# Module specifications for CohortGenerator, enabling statistics generation.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Module specifications for CohortDiagnostics, using all generated cohort IDs and various diagnostics.
# Specific diagnostic options are not provided in the input, so template defaults are used.
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # All re-numbered cohorts
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

# Study periods: Extract study start and end dates from specifications.
# If null in specifications, it means no restriction, so NA_character_ is used.
specStudyPeriod <- analysisSpecificationsJson$getDbCohortMethodDataArgs$studyPeriods[[1]]
studyPeriods <- tibble(
  studyStartDate = ifelse(is.null(specStudyPeriod$studyStartDate), NA_character_, specStudyPeriod$studyStartDate),
  studyEndDate = ifelse(is.null(specStudyPeriod$studyEndDate), NA_character_, specStudyPeriod$studyEndDate)
)
# Annotation: If studyStartDate or studyEndDate are NULL, they are set to NA_character_
# to indicate no date restriction for the study period.

# Time-at-risks (TARs) for the outcomes of interest in your study.
# Extract from the first timeAtRisks entry in the specifications.
specTimeAtRisk <- analysisSpecificationsJson$createStudyPopArgs$timeAtRisks[[1]]
timeAtRisks <- tibble(
  label = sprintf("TAR_S%d_E%d", specTimeAtRisk$riskWindowStart, specTimeAtRisk$riskWindowEnd), # Dynamic label for TAR
  riskWindowStart = specTimeAtRisk$riskWindowStart,
  startAnchor = specTimeAtRisk$startAnchor,
  riskWindowEnd = specTimeAtRisk$riskWindowEnd,
  endAnchor = specTimeAtRisk$endAnchor,
  minDaysAtRisk = specTimeAtRisk$minDaysAtRisk
)

# Propensity Score settings - match on PS or stratify by PS.
# Based on the analysis specifications, only 'matchOnPsArgs' is provided.
matchOnPsArgsList <- tibble(
  label = character(),
  maxRatio = numeric(),
  caliper = numeric(),
  caliperScale = character()
)

stratifyByPsArgsList <- tibble(
  label = character(),
  numberOfStrata = numeric(),
  baseSelection = character()
)

# Populate psArgsList based on the analysis specifications.
for (psSetting in analysisSpecificationsJson$propensityScoreAdjustment$psSettings) {
  if (!is.null(psSetting$matchOnPsArgs)) {
    matchOnPsArgsList <- matchOnPsArgsList %>%
      add_row(
        label = "PS Matching", # A descriptive label
        maxRatio = psSetting$matchOnPsArgs$maxRatio,
        caliper = psSetting$matchOnPsArgs$caliper,
        caliperScale = psSetting$matchOnPsArgs$caliperScale
      )
  }
  if (!is.null(psSetting$stratifyByPsArgs)) {
    stratifyByPsArgsList <- stratifyByPsArgsList %>%
      add_row(
        label = "PS Stratification", # A descriptive label
        numberOfStrata = psSetting$stratifyByPsArgs$numberOfStrata,
        baseSelection = psSetting$stratifyByPsArgs$baseSelection
      )
  }
}

# Build a single PS configuration list (each entry has: method, label, params).
psConfigList <- list()

# Convert "match on PS" settings into config list.
if (nrow(matchOnPsArgsList) > 0) {
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

# Convert "stratify by PS" settings into config list.
if (nrow(stratifyByPsArgsList) > 0) {
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

# Iterate through all analysis setting combinations to create CohortMethod analysis objects.
cmAnalysisList <- list()
analysisId <- 1

# Loop through each defined study period.
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  # Loop through each defined time-at-risk window.
  for (t in seq_len(nrow(timeAtRisks))) {

    # Loop through each defined propensity score adjustment method (matching/stratification).
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Determine PS adjustment arguments based on the method in psConfigList.
      matchOnPsArgs <- NULL
      stratifyByPsArgs <- NULL
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Using template default.
          stratificationColumns = c() # Using template default.
        )
      } else if (psCfg$method == "stratify") {
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Using template default.
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Default covariate settings. The analysis specifications did not provide
      # specific FeatureExtraction settings, so defaults are used.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Using template default.
      )

      # Combine main outcomes and negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not applicable for observational studies.
            priorOutcomeLookback = analysisSpecificationsJson$createStudyPopArgs$priorOutcomeLookBack # From spec
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # Expected effect size for negative controls.
          )
        })
      )
      
      # Create TargetComparatorOutcomes list for each T-C pair.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Excluded covariate concept IDs: Using only concepts from `excludedCovariateConcepts`
          # which is an empty data frame based on current analysis specifications.
          # The template had placeholder concept IDs, removed as per spec.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId 
        )
      }

      # Arguments for retrieving cohort method data from the database.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = analysisSpecificationsJson$createStudyPopArgs$restrictToCommonPeriod, # From spec
        studyStartDate = studyStartDate, # From current loop iteration
        studyEndDate = studyEndDate, # From current loop iteration
        maxCohortSize = analysisSpecificationsJson$getDbCohortMethodDataArgs$maxCohortSize, # From spec
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = analysisSpecificationsJson$propensityScoreAdjustment$createPsArgs$maxCohortSizeForFitting, # From spec
        errorOnHighCorrelation = analysisSpecificationsJson$propensityScoreAdjustment$createPsArgs$errorOnHighCorrelation, # From spec
        stopOnError = FALSE, # Template default: set to FALSE to allow Strategus to continue if PS model fitting fails for some reason.
        estimator = "att", # Template default.
        prior = Cyclops::createPrior(
          priorType = analysisSpecificationsJson$propensityScoreAdjustment$createPsArgs$prior$priorType, # From spec
          useCrossValidation = analysisSpecificationsJson$propensityScoreAdjustment$createPsArgs$prior$useCrossValidation # From spec
          # exclude = c(0) not in spec, removed from template.
        ),
        control = Cyclops::createControl(
          noiseLevel = analysisSpecificationsJson$propensityScoreAdjustment$createPsArgs$control$noiseLevel, # From spec
          cvType = analysisSpecificationsJson$propensityScoreAdjustment$createPsArgs$control$cvType, # From spec
          # seed = 1 not in spec, removed from template.
          resetCoefficients = analysisSpecificationsJson$propensityScoreAdjustment$createPsArgs$control$resetCoefficients, # From spec
          tolerance = analysisSpecificationsJson$propensityScoreAdjustment$createPsArgs$control$tolerance, # From spec
          cvRepetitions = analysisSpecificationsJson$propensityScoreAdjustment$createPsArgs$control$cvRepetitions, # From spec
          startingVariance = analysisSpecificationsJson$propensityScoreAdjustment$createPsArgs$control$startingVariance # From spec
        )
      )

      # Arguments for computing covariate balance before PS adjustment.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Template default.
        covariateFilter = NULL # Template default.
      )
      # Arguments for computing covariate balance after PS adjustment.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Template default.
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Template default.
      )

      # Arguments for fitting the outcome model.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = analysisSpecificationsJson$fitOutcomeModelArgs$modelType, # From spec
        stratified = analysisSpecificationsJson$fitOutcomeModelArgs$stratified, # From spec
        useCovariates = analysisSpecificationsJson$fitOutcomeModelArgs$useCovariates, # From spec
        inversePtWeighting = analysisSpecificationsJson$fitOutcomeModelArgs$inversePtWeighting, # From spec
        prior = Cyclops::createPrior(
          priorType = analysisSpecificationsJson$fitOutcomeModelArgs$prior$priorType, # From spec
          useCrossValidation = analysisSpecificationsJson$fitOutcomeModelArgs$prior$useCrossValidation # From spec
        ),
        control = Cyclops::createControl(
          cvType = analysisSpecificationsJson$fitOutcomeModelArgs$control$cvType, # From spec
          # seed = 1 not in spec, removed from template.
          resetCoefficients = analysisSpecificationsJson$fitOutcomeModelArgs$control$resetCoefficients, # From spec
          startingVariance = analysisSpecificationsJson$fitOutcomeModelArgs$control$startingVariance, # From spec
          tolerance = analysisSpecificationsJson$fitOutcomeModelArgs$control$tolerance, # From spec
          cvRepetitions = analysisSpecificationsJson$fitOutcomeModelArgs$control$cvRepetitions, # From spec
          noiseLevel = analysisSpecificationsJson$fitOutcomeModelArgs$control$noiseLevel # From spec
        )
      )

      # Arguments for creating the study population.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = analysisSpecificationsJson$createStudyPopArgs$restrictToCommonPeriod, # From spec
        firstExposureOnly = analysisSpecificationsJson$createStudyPopArgs$firstExposureOnly, # From spec
        washoutPeriod = analysisSpecificationsJson$createStudyPopArgs$washoutPeriod, # From spec
        removeDuplicateSubjects = analysisSpecificationsJson$createStudyPopArgs$removeDuplicateSubjects, # From spec
        censorAtNewRiskWindow = analysisSpecificationsJson$createStudyPopArgs$censorAtNewRiskWindow, # From spec
        removeSubjectsWithPriorOutcome = analysisSpecificationsJson$createStudyPopArgs$removeSubjectsWithPriorOutcome, # From spec
        priorOutcomeLookback = analysisSpecificationsJson$createStudyPopArgs$priorOutcomeLookBack, # From spec
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From current loop iteration
        startAnchor = timeAtRisks$startAnchor[t], # From current loop iteration
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From current loop iteration
        endAnchor = timeAtRisks$endAnchor[t], # From current loop iteration
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From current loop iteration
        maxDaysAtRisk = 99999 # Not specified in input, using a reasonable default.
      )


      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          # Using paste0 to handle potential NA study dates for description
          ifelse(is.na(studyStartDate), "NoStartDateRestriction", studyStartDate), 
          ifelse(is.na(studyEndDate), "NoEndDateRestriction", studyEndDate), 
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
# Module specifications for CohortMethod.
# `refitPsForEveryOutcome` and `refitPsForEveryStudyPopulation` are not specified
# in input, so template defaults are used.
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # No analyses specified for exclusion.
  refitPsForEveryOutcome = FALSE, 
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Using default diagnostic thresholds.
)

# Create the analysis specifications ------------------------------------------
# Initialize an empty analysis specifications object and add all shared resources and module specifications.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the complete analysis specifications to a JSON file.
# The file path uses the study name from the analysis specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", analysisSpecificationsJson$name, paste0(analysisSpecificationsJson$name, "AnalysisSpecification.json"))
)