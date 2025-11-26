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
library(CohortMethod) # Required for CohortMethod settings
library(FeatureExtraction) # Required for Covariate settings
library(Cyclops) # Required for Cyclops priors and controls

# Analysis name from the Analysis Specifications
analysisName <- "iudehre"

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
# Base URL for the OHDSI Atlas WebAPI. This needs to be configured by the user.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions from Analysis Specifications
# Target, Comparator, and Outcome cohorts are defined here.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal Strategus use (1=target, 2=comparator, 3=outcome).
# This provides consistent internal IDs across analyses.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Target cohort ID
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Comparator cohort ID
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Outcome cohort ID

# Negative control outcomes from Analysis Specifications
# These are concept sets that are resolved into specific concepts to be used as negative outcomes.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Negative Control Concept Set ID: negative
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(
    baseUrl = baseUrl
  ) %>%
  ROhdsiWebApi::getConcepts(
    baseUrl = baseUrl
  ) %>%
  # Rename columns to match expected format for negative control outcome cohorts
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  # Assign unique cohort IDs for negative controls, starting from 101 to avoid conflicts with T/C/O (1, 2, 3)
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)


if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found. Please ensure all cohort IDs (T/C/O and negative controls) are unique. ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Filter for the main outcome cohort (ID 3, which is 1794131 originally).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # Default cleanWindow. Not specified in analysis config, using common default.
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis.
# Uses the re-numbered IDs (1 and 2).
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate Selection: Excluded concepts from Analysis Specifications
# If 'id' is null, it indicates no specific concept is to be excluded.
if (length(Filter(function(x) !is.null(x$id), list())) > 0) { # Replace 'list()' with actual data from analysis spec if available
  # If conceptsToExclude were provided in analysis spec:
  # excludedCovariateConcepts <- data.frame(
  #   conceptId = c(list_of_concept_ids),
  #   conceptName = c(list_of_concept_names)
  # )
  # For the given analysis spec, covariateSelection.conceptsToExclude is empty.
  excludedCovariateConcepts <- data.frame(
    conceptId = integer(),
    conceptName = character()
  )
} else {
  excludedCovariateConcepts <- data.frame(
    conceptId = integer(),
    conceptName = character()
  )
}

# Optional: If you want to define covariates to include instead of including them all
# For the given analysis spec, covariateSelection.conceptsToInclude is empty.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )


# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Negative controls shared resources. Uses the dynamically created negative control outcome cohort set.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default from template, not specified in analysis config.
  detectOnDescendants = TRUE # Default from template, not specified in analysis config.
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Default from template, not specified in analysis config.
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # Run diagnostics for all defined cohorts (T, C, O)
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

# Study periods from Analysis Specifications (getDbCohortMethodDataArgs.studyPeriods)
studyPeriods <- tibble(
  studyStartDate = c("20030101"), # YYYYMMDD format
  # For null studyEndDate in analysis spec, use NA_character_ in R
  studyEndDate   = c(NA_character_)
)

# Time-at-risks (TARs) for the outcomes of interest in your study from Analysis Specifications (createStudyPopArgs.timeAtRisks)
timeAtRisks <- tibble(
  label = c("Initial TAR"), # Adding a label for readability
  riskWindowStart  = c(30),
  startAnchor = c("cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd  = c(5475),
  endAnchor = c("cohort start") # "cohort start" | "cohort end"
)

# Propensity Score settings - match on PS from Analysis Specifications (propensityScoreAdjustment.psSettings)
# "matchOnPsArgs" is not null in the analysis spec, so populate this list.
matchOnPsArgsList <- tibble(
  label = c("Match on PS - Caliper 0.2"), # Adding a label for readability
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # "propensity score" | "standardized" | "standardized logit"
)

# Propensity Score settings - stratify by PS from Analysis Specifications (propensityScoreAdjustment.psSettings)
# "stratifyByPsArgs" is null in the analysis spec, so this list will be empty.
stratifyByPsArgsList <- tibble(
  label = character(),
  numberOfStrata  = integer(),
  baseSelection = character() # "all" | "target" | "comparator"
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

      # Covariate settings for getDbCohortMethodDataArgs
      # Using default settings, no specific includes/excludes from the `covariateSelection`
      # in the analysis spec as they were empty/null.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default from template
      )

      # Outcome list includes the main outcome and all negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i], # Main outcome (ID 3)
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # For true effect size, usually NA for real studies
            priorOutcomeLookback = 99999 # From createStudyPopArgs.priorOutcomeLookBack
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i, # Negative control outcomes
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # Assuming a true effect size of 1 for negative controls
          )
        })
      )
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i], # Target cohort ID (1)
          comparatorId = cmTcList$comparatorCohortId[i], # Comparator cohort ID (2)
          outcomes = outcomeList,
          # Excluded covariate concept IDs. The analysis spec does not provide
          # target/comparator drug concept IDs to exclude. Only using general
          # excludedCovariateConcepts which is empty based on the spec.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # getDbCohortMethodDataArgs from Analysis Specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Default from template, not in analysis spec for this function
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From getDbCohortMethodDataArgs.maxCohortSize
        covariateSettings = covariateSettings
      )

      # createPsArgs from Analysis Specifications (propensityScoreAdjustment.createPsArgs)
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From analysis spec
        errorOnHighCorrelation = TRUE, # From analysis spec
        stopOnError = FALSE, # Default from template
        estimator = "att", # Default from template
        prior = Cyclops::createPrior(
          priorType = "laplace", # From analysis spec prior.priorType
          exclude = c(0), # Default from template
          useCrossValidation = TRUE # From analysis spec prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", # From analysis spec control.noiseLevel
          cvType = "auto", # From analysis spec control.cvType
          seed = 1, # Default from template
          resetCoefficients = TRUE, # From analysis spec control.resetCoefficients
          tolerance = 2e-07, # From analysis spec control.tolerance
          cvRepetitions = 10, # From analysis spec control.cvRepetitions (template had 1, spec has 10)
          startingVariance = 0.01 # From analysis spec control.startingVariance
        )
      )

      # Compute covariate balance arguments (using template defaults for maxCohortSize)
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default from template
        covariateFilter = NULL # Default from template
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default from template
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default from template
      )

      # fitOutcomeModelArgs from Analysis Specifications
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From analysis spec modelType
        stratified = FALSE, # From analysis spec stratified (template had TRUE, updated to spec)
        useCovariates = FALSE, # From analysis spec useCovariates
        inversePtWeighting = FALSE, # From analysis spec inversePtWeighting
        prior = Cyclops::createPrior(
          priorType = "laplace", # From analysis spec prior.priorType
          useCrossValidation = TRUE # From analysis spec prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          cvType = "auto", # From analysis spec control.cvType
          seed = 1, # Default from template
          resetCoefficients = TRUE, # From analysis spec control.resetCoefficients
          startingVariance = 0.01, # From analysis spec control.startingVariance
          tolerance = 2e-07, # From analysis spec control.tolerance
          cvRepetitions = 10, # From analysis spec control.cvRepetitions (template had 1, spec has 10)
          noiseLevel = "quiet" # From analysis spec control.noiseLevel
        )
      )

      # createStudyPopArgs from Analysis Specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = TRUE, # From createStudyPopArgs.firstExposureOnly
        washoutPeriod = 365, # From createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # From createStudyPopArgs.removeDuplicateSubjects (template had "keep first", updated to spec)
        censorAtNewRiskWindow = FALSE, # From createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # From createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # From createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999 # Default from template, not specified in analysis config.
      )


      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        # Description combining study period, time-at-risk, and PS adjustment method
        description = sprintf(
          "Analysis: %s; Study period: %s-%s; TAR: %s; PS: %s",
          analysisName, # Overall analysis name
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

# Save the analysis specifications to a JSON file
# The file path uses the 'analysisName' from the Analysis Specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", analysisName, paste0(analysisName, "AnalysisSpecification.json"))
)