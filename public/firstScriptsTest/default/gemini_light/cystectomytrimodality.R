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
# Base URL for the WebAPI, not specified in Analysis Specifications, using a placeholder.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Extracting cohort IDs and names from Analysis Specifications:
# targetCohort: id 1794126, name "target1"
# comparatorCohort: id 1794132, name "comparator1"
# outcomeCohort: id 1794131, name "outcome1"
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal study use (1 for target, 2 for comparator, 3 for outcome)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Extracting negativeControlConceptSet ID from Analysis Specifications: id 1888110, name "negative"
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # From Analysis Specifications: negativeControlConceptSet.id
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
  mutate(cohortId = row_number() + 100) %>% # Assigning new cohort IDs starting from 101 for negative controls
  select(cohortId, cohortName, outcomeConceptId)


if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Filter for the re-numbered outcome cohort (ID 3)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default value from template, not specified in Analysis Specifications

# Target and Comparator for the CohortMethod analysis
# Using the re-numbered target (ID 1) and comparator (ID 2) cohorts
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered target cohort ID
  targetCohortName = "target1", # From Analysis Specifications: cohortDefinitions.targetCohort.name
  comparatorCohortId = 2, # Re-numbered comparator cohort ID
  comparatorCohortName = "comparator1" # From Analysis Specifications: cohortDefinitions.comparatorCohort.name
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study.
# From Analysis Specifications: covariateSelection.conceptsToExclude is empty.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0), # No concepts to exclude specified in Analysis Specifications
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# From Analysis Specifications: covariateSelection.conceptsToInclude is empty.
# Keeping this commented out as per template and empty specification.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

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
  cohortIds = cohortDefinitionSet$cohortId, # All cohort IDs defined in cohortDefinitionSet
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

# If you are not restricting your study to a specific time window, 
# please make these strings empty
# From Analysis Specifications: getDbCohortMethodDataArgs.studyPeriods
studyPeriods <- tibble(
  studyStartDate = c("20050101"), # From Analysis Specifications: getDbCohortMethodDataArgs.studyPeriods[0].studyStartDate
  studyEndDate   = c("20171231")  # From Analysis Specifications: getDbCohortMethodDataArgs.studyPeriods[0].studyEndDate
)

# Time-at-risks (TARs) for the outcomes of interest in your study
# From Analysis Specifications: createStudyPopArgs.timeAtRisks
timeAtRisks <- tibble(
  label = c("TAR1"), # Custom label for this time-at-risk setting
  riskWindowStart  = c(1), # From Analysis Specifications: createStudyPopArgs.timeAtRisks[0].riskWindowStart
  startAnchor = c("cohort start"), # From Analysis Specifications: createStudyPopArgs.timeAtRisks[0].startAnchor
  riskWindowEnd  = c(99999), # From Analysis Specifications: createStudyPopArgs.timeAtRisks[0].riskWindowEnd
  endAnchor = c("cohort start"), # From Analysis Specifications: createStudyPopArgs.timeAtRisks[0].endAnchor
  minDaysAtRisk = c(1) # From Analysis Specifications: createStudyPopArgs.timeAtRisks[0].minDaysAtRisk
) 

# Propensity Score settings - match on PS
# From Analysis Specifications: propensityScoreAdjustment.psSettings (all are matchOnPsArgs)
matchOnPsArgsList <- tibble(
  label = c("Match_MaxRatio3", "Match_MaxRatio1", "Match_MaxRatio2", "Match_MaxRatio4"), # Custom labels for each matching setting
  maxRatio  = c(3, 1, 2, 4), # From Analysis Specifications: propensityScoreAdjustment.psSettings[i].matchOnPsArgs.maxRatio
  caliper = c(0.2, 0.2, 0.2, 0.2), # From Analysis Specifications: propensityScoreAdjustment.psSettings[i].matchOnPsArgs.caliper
  caliperScale  = c("standardized logit", "standardized logit", "standardized logit", "standardized logit") # From Analysis Specifications: propensityScoreAdjustment.psSettings[i].matchOnPsArgs.caliperScale
) 

# Propensity Score settings - stratify by PS
# No stratifyByPsArgs specified in Analysis Specifications
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

      # Covariate settings: Analysis Specifications has empty conceptsToInclude and conceptsToExclude,
      # so using default covariate settings.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default from template
      )

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
            trueEffectSize = 1 # Default from template
          )
        })
      )
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # excludedCovariateConceptIds: Using the empty excludedCovariateConcepts from Analysis Specifications.
          # Removed targetConceptId and comparatorConceptId from template as they are not in spec.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId 
        )
      }

      # getDbCohortMethodDataArgs settings from Analysis Specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # From Analysis Specifications: getDbCohortMethodDataArgs.restrictToCommonPeriod
        studyStartDate = studyStartDate, # From loop (Analysis Specifications: getDbCohortMethodDataArgs.studyPeriods[0].studyStartDate)
        studyEndDate = studyEndDate, # From loop (Analysis Specifications: getDbCohortMethodDataArgs.studyPeriods[0].studyEndDate)
        maxCohortSize = 0, # From Analysis Specifications: getDbCohortMethodDataArgs.maxCohortSize
        firstExposureOnly = FALSE, # From Analysis Specifications: getDbCohortMethodDataArgs.firstExposureOnly
        washoutPeriod = 0, # From Analysis Specifications: getDbCohortMethodDataArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # From Analysis Specifications: getDbCohortMethodDataArgs.removeDuplicateSubjects
        covariateSettings = covariateSettings
      )

      # createPsArgs settings from Analysis Specifications
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Default from template
        estimator = "att", # Default from template
        prior = Cyclops::createPrior(
          priorType = "laplace", # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.prior.priorType
          exclude = c(0), # Default from template
          useCrossValidation = TRUE # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.noiseLevel
          cvType = "auto", # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.cvType
          seed = 1, # Default from template
          resetCoefficients = TRUE, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.resetCoefficients
          tolerance = 2e-07, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.tolerance
          cvRepetitions = 10, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.cvRepetitions
          startingVariance = 0.01, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.startingVariance
          fold = 10 # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.fold
        )
      )

      # computeSharedCovariateBalanceArgs and computeCovariateBalanceArgs are using template defaults
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default from template
        covariateFilter = NULL # Default from template
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default from template
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default from template
      )

      # fitOutcomeModelArgs settings from Analysis Specifications
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From Analysis Specifications: fitOutcomeModelArgs.modelType
        stratified = TRUE, # From Analysis Specifications: fitOutcomeModelArgs.stratified
        useCovariates = TRUE, # From Analysis Specifications: fitOutcomeModelArgs.useCovariates (changed from template FALSE)
        inversePtWeighting = FALSE, # From Analysis Specifications: fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior(
          priorType = "laplace", # From Analysis Specifications: fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE # From Analysis Specifications: fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          cvType = "auto", # From Analysis Specifications: fitOutcomeModelArgs.control.cvType
          seed = 1, # Default from template
          resetCoefficients = TRUE, # From Analysis Specifications: fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01, # From Analysis Specifications: fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07, # From Analysis Specifications: fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10, # From Analysis Specifications: fitOutcomeModelArgs.control.cvRepetitions
          noiseLevel = "quiet", # From Analysis Specifications: fitOutcomeModelArgs.control.noiseLevel
          fold = 10 # From Analysis Specifications: fitOutcomeModelArgs.control.fold
        )
      )
      
      # createStudyPopArgs settings from Analysis Specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications: createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE, # From Analysis Specifications: createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0, # From Analysis Specifications: createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # From Analysis Specifications: createStudyPopArgs.removeDuplicateSubjects (changed from template "keep first")
        censorAtNewRiskWindow = FALSE, # From Analysis Specifications: createStudyPopArgs.censorAtNewRiskWindow (changed from template TRUE)
        removeSubjectsWithPriorOutcome = TRUE, # From Analysis Specifications: createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # From Analysis Specifications: createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From loop (Analysis Specifications: createStudyPopArgs.timeAtRisks[0].riskWindowStart)
        startAnchor = timeAtRisks$startAnchor[t], # From loop (Analysis Specifications: createStudyPopArgs.timeAtRisks[0].startAnchor)
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From loop (Analysis Specifications: createStudyPopArgs.timeAtRisks[0].riskWindowEnd)
        endAnchor = timeAtRisks$endAnchor[t], # From loop (Analysis Specifications: createStudyPopArgs.timeAtRisks[0].endAnchor)
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From loop (Analysis Specifications: createStudyPopArgs.timeAtRisks[0].minDaysAtRisk)
        maxDaysAtRisk = 99999 # Default from template
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

# Save the analysis specifications to a JSON file
# Using the name "cystectomytrimodality" from Analysis Specifications: name
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "cystectomytrimodality", "cystectomytrimodalityAnalysisSpecification.json")
)