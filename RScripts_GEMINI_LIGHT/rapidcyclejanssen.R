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
library(tibble) # For tibble

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI" # Default ATLAS base URL

# Cohort Definitions from analysis specifications
# Target: { "id": 1794126, "name": "target1" }
# Comparator: { "id": 1794132, "name": "comparator1" }
# Outcome: { "id": 1794131, "name": "outcome1" }
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131 # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal consistency within the study package
# Target (1794126) re-numbered to 1
# Comparator (1794132) re-numbered to 2
# Outcome (1794131) re-numbered to 3
cohortDefinitionSet <- cohortDefinitionSet %>%
  mutate(cohortId = case_when(
    cohortId == 1794126 ~ 1,
    cohortId == 1794132 ~ 2,
    cohortId == 1794131 ~ 3,
    TRUE ~ cohortId # Keep other cohortIds as is, though none expected here
  ))

# Negative control outcomes from analysis specifications
# negativeControlConceptSet: { "id": 1888110, "name": "negative" }
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # ID from analysis specifications
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
  # Assign cohort IDs starting from 101 to avoid overlap with T/C/O (1, 2, 3)
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs between study cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Filter for the outcome cohort, which was re-numbered to 3
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default value from template, not specified in analysis specifications

# Target and Comparator for the CohortMethod analysis
# Uses the re-numbered target (1) and comparator (2)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = cohortDefinitionSet %>% filter(cohortId == 1) %>% pull(cohortName),
  comparatorCohortId = 2,
  comparatorCohortName = cohortDefinitionSet %>% filter(cohortId == 2) %>% pull(cohortName)
)

# For the CohortMethod LSPS we'll need to exclude specific concepts.
# analysis specifications -> covariateSelection -> conceptsToExclude: [ { "id": null, "name": "" } ]
# Since the specification is empty, create an empty data frame.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# analysis specifications -> covariateSelection -> conceptsToInclude: [ { "id": null, "name": "" } ]
# Since the specification is empty, create an empty data frame.
includedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Create shared resource specifications for study cohorts
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Create shared resource specifications for negative control outcomes
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default from template, not specified in analysis specifications
  detectOnDescendants = TRUE # Default from template, not specified in analysis specifications
)
# Create module specifications for CohortGenerator
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Default from template, not specified in analysis specifications
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Create module specifications for CohortDiagnostics using all study cohorts
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
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

# Study periods from analysis specifications -> getDbCohortMethodDataArgs -> studyPeriods
# { "studyStartDate": 20210101, "studyEndDate": null }
studyPeriods <- tibble(
  studyStartDate = c(20210101),
  studyEndDate   = c(NA_real_) # Use NA_real_ for NULL studyEndDate
)

# Time-at-risks (TARs) for the outcomes of interest from analysis specifications -> createStudyPopArgs -> timeAtRisks
timeAtRisks <- tibble(
  label = c("TAR_1_14", "TAR_1_28", "TAR_1_42", "TAR_1_90", "TAR_0_2"), # Descriptive labels for each TAR
  riskWindowStart  = c(1, 1, 1, 1, 0),
  startAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd  = c(14, 28, 42, 90, 2),
  endAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start"),
  minDaysAtRisk = c(1, 1, 1, 1, 1) # minDaysAtRisk from spec
)

# Propensity Score settings - match on PS from analysis specifications -> propensityScoreAdjustment -> psSettings
# { "matchOnPsArgs": { "maxRatio": 0, "caliper": 0.2, "caliperScale": "standardized logit" }, "stratifyByPsArgs": null }
matchOnPsArgsList <- tibble(
  label = c("Match0.2StdLogit"), # Descriptive label
  maxRatio  = c(0),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# Propensity Score settings - stratify by PS from analysis specifications -> propensityScoreAdjustment -> psSettings
# "stratifyByPsArgs": null, so this list will be empty
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
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
      
      # Determine PS adjustment method based on psConfigList entry
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default from template
          stratificationColumns = c() # Default from template
        )
        stratifyByPsArgs <- NULL # Only one PS adjustment method can be active
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL # Only one PS adjustment method can be active
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default from template
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Default covariate settings for FeatureExtraction.
      # analysis specifications -> covariateSelection -> conceptsToInclude and conceptsToExclude are empty,
      # so default settings are used and specific exclusions are handled in targetComparatorOutcomesList.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default from template
      )

      # Combine study outcome cohorts and negative control outcome cohorts
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # For true outcomes, effect size is unknown
            priorOutcomeLookback = 99999 # From analysis specifications -> createStudyPopArgs -> priorOutcomeLookBack
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # For negative controls, true effect size is assumed to be 1
          )
        })
      )
      
      # Create TargetComparatorOutcomes list for all T/C pairs
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude concepts defined in covariateSelection.conceptsToExclude.
          # Since the spec's conceptsToExclude is empty, this will be an empty vector.
          # The template's inclusion of cmTcList$targetConceptId and cmTcList$comparatorConceptId is removed
          # because the spec doesn't provide treatment concept IDs.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # getDbCohortMethodDataArgs from analysis specifications
      # getDbCohortMethodDataArgs: { "studyPeriods": [...], "maxCohortSize": 0 }
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Default from template
        studyStartDate = studyStartDate, # From the loop over studyPeriods
        studyEndDate = studyEndDate, # From the loop over studyPeriods
        maxCohortSize = 0, # From analysis specifications
        covariateSettings = covariateSettings
      )

      # createPsArgs from analysis specifications -> propensityScoreAdjustment -> createPsArgs
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From analysis specifications
        errorOnHighCorrelation = TRUE, # From analysis specifications
        stopOnError = FALSE, # Default from template, not in spec. Allowing to complete analysis if PS fails
        estimator = "att", # Default from template, not in spec
        prior = Cyclops::createPrior( # From analysis specifications -> prior
          priorType = "laplace", 
          exclude = c(0), # Default from template (exclude intercept from regularization)
          useCrossValidation = TRUE # From analysis specifications
        ),
        control = Cyclops::createControl( # From analysis specifications -> control
          noiseLevel = "silent", # From analysis specifications
          cvType = "auto", # From analysis specifications
          seed = 1, # Default from template, not in spec
          resetCoefficients = TRUE, # From analysis specifications
          tolerance = 2e-07, # From analysis specifications
          cvRepetitions = 10, # From analysis specifications (fold: 10, cvRepetitions: 10)
          startingVariance = 0.01 # From analysis specifications
        )
      )

      # computeSharedCovariateBalanceArgs and computeCovariateBalanceArgs using template defaults
      # Not specified in analysis specifications
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs from analysis specifications
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From analysis specifications
        stratified = FALSE, # From analysis specifications
        useCovariates = FALSE, # From analysis specifications
        inversePtWeighting = FALSE, # From analysis specifications
        prior = Cyclops::createPrior( # From analysis specifications -> prior
          priorType = "laplace", 
          useCrossValidation = TRUE # From analysis specifications
        ),
        control = Cyclops::createControl( # From analysis specifications -> control
          cvType = "auto", # From analysis specifications
          seed = 1, # Default from template, not in spec
          resetCoefficients = TRUE, # From analysis specifications
          startingVariance = 0.01, # From analysis specifications
          tolerance = 2e-07, # From analysis specifications
          cvRepetitions = 10, # From analysis specifications (fold: 10, cvRepetitions: 10)
          noiseLevel = "quiet" # From analysis specifications
        )
      )
      
      # createStudyPopArgs from analysis specifications -> createStudyPopArgs
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From analysis specifications
        firstExposureOnly = TRUE, # From analysis specifications
        washoutPeriod = 365, # From analysis specifications
        removeDuplicateSubjects = "remove all", # From analysis specifications
        censorAtNewRiskWindow = FALSE, # From analysis specifications
        removeSubjectsWithPriorOutcome = TRUE, # From analysis specifications
        priorOutcomeLookback = 99999, # From analysis specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From the loop over timeAtRisks
        startAnchor = timeAtRisks$startAnchor[t], # From the loop over timeAtRisks
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From the loop over timeAtRisks
        endAnchor = timeAtRisks$endAnchor[t], # From the loop over timeAtRisks
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From the loop over timeAtRisks
        maxDaysAtRisk = 99999 # Default from template, not specified in analysis specifications
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
        matchOnPsArgs = matchOnPsArgs, # Conditional based on psCfg$method
        stratifyByPsArgs = stratifyByPsArgs, # Conditional based on psCfg$method
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
# Combine all shared resources and module specifications
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file
# Using the study name "rapidcyclejanssen" from analysis specifications
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "rapidcyclejanssen", "rapidcyclejanssenAnalysisSpecification.json")
)