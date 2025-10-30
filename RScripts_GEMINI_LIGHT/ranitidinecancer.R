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
library(CohortGenerator)
library(CohortDiagnostics)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
# Base URL for the WebAPI instance to retrieve cohort definitions.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions for Target, Comparator, and Outcome cohorts.
# The cohortIds are mapped from the analysis specifications and then re-numbered
# internally for consistency within Strategus modules (1 for target, 2 for comparator, 3 for outcome).
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: ranitidinecancer target1
    1794132, # Comparator: ranitidinecancer comparator1
    1794131 # Outcome: ranitidinecancer outcome1
  ),
  generateStats = TRUE # Generate cohort statistics during cohort generation.
)

# Re-number cohorts to a standard internal representation for the analysis.
# Target cohort ID 1794126 becomes 1.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# Comparator cohort ID 1794132 becomes 2.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# Outcome cohort ID 1794131 becomes 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes are derived from a concept set specified in the analysis specifications.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Concept set ID for negative controls from analysis specifications.
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet( # Resolve the concept set to individual concepts.
    baseUrl = baseUrl
  ) %>%
  ROhdsiWebApi::getConcepts( # Retrieve details for the resolved concepts.
    baseUrl = baseUrl
  ) %>%
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  mutate(cohortId = row_number() + 100) %>% # Assign cohort IDs starting from 101 for negative controls to avoid conflict with T/C/O.
  select(cohortId, cohortName, outcomeConceptId)


if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts used in each analysis ---------------
# Outcomes: This list will be used in CohortMethod analyses.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Filter for the re-numbered outcome cohort (ID 3).
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Clean window is not specified in analysis specifications, using template default.

# Target and Comparator for the CohortMethod analysis.
# Uses the re-numbered cohort IDs for target (1) and comparator (2).
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1", # Name from analysis specifications.
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1" # Name from analysis specifications.
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study. The 'conceptsToExclude' in analysis specifications is empty.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0), # Empty as per analysis specifications 'conceptsToExclude'.
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# 'conceptsToInclude' in analysis specifications is empty, so this is not used.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Defines shared resources for cohort definitions.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Defines shared resources for negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Not specified in analysis specifications, using template default.
  detectOnDescendants = TRUE # Not specified in analysis specifications, using template default.
)
# Creates module specifications for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Generate cohort statistics as specified.
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Creates module specifications for CohortDiagnostics.
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # All defined cohorts will be diagnosed.
  runInclusionStatistics = TRUE, # Not specified in analysis specifications, using template default.
  runIncludedSourceConcepts = TRUE, # Not specified in analysis specifications, using template default.
  runOrphanConcepts = TRUE, # Not specified in analysis specifications, using template default.
  runTimeSeries = FALSE, # Not specified in analysis specifications, using template default.
  runVisitContext = TRUE, # Not specified in analysis specifications, using template default.
  runBreakdownIndexEvents = TRUE, # Not specified in analysis specifications, using template default.
  runIncidenceRate = TRUE, # Not specified in analysis specifications, using template default.
  runCohortRelationship = TRUE, # Not specified in analysis specifications, using template default.
  runTemporalCohortCharacterization = TRUE, # Not specified in analysis specifications, using template default.
  minCharacterizationMean = 0.01 # Not specified in analysis specifications, using template default.
)

# CohortMethodModule -----------------------------------------------------------

# Study periods from analysis specifications.
# If studyStartDate and studyEndDate are null, they are represented as NA.
# The loop will run once with NA values, which will then be converted to empty strings for CohortMethod functions
# to indicate no date restriction.
studyPeriods <- tibble(
  studyStartDate = c(NA_character_), # From analysis specifications: studyStartDate = null
  studyEndDate   = c(NA_character_)  # From analysis specifications: studyEndDate = null
)

# Time-at-risks (TARs) for the outcomes of interest in your study from analysis specifications.
timeAtRisks <- tibble(
  label = c("TAR 365-9999"), # Custom label for the time-at-risk window.
  riskWindowStart  = c(365), # From analysis specifications.
  startAnchor = c("cohort start"), # From analysis specifications.
  riskWindowEnd  = c(9999), # From analysis specifications.
  endAnchor = c("cohort start") # From analysis specifications.
) 

# Propensity Score settings - match on PS from analysis specifications.
# The 'psSettings' in analysis specifications has one 'matchOnPsArgs' entry.
matchOnPsArgsList <- tibble(
  label = c("Match_Ratio1_Caliper0.2_StdLogit"), # Custom label for this PS adjustment.
  maxRatio  = c(1), # From analysis specifications.
  caliper = c(0.2), # From analysis specifications.
  caliperScale  = c("standardized logit") # From analysis specifications.
) 

# Propensity Score settings - stratify by PS.
# 'stratifyByPsArgs' in analysis specifications is null, so this list is empty.
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata   = integer(0),
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
# This block will be skipped as stratifyByPsArgsList is empty.
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
  # Handle null study dates from analysis specifications by using empty strings.
  studyStartDate <- if (is.na(studyPeriods$studyStartDate[s])) "" else studyPeriods$studyStartDate[s]
  studyEndDate <- if (is.na(studyPeriods$studyEndDate[s])) "" else studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      if (psCfg$method == "match") {
        # createMatchOnPsArgs settings from analysis specifications.
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio, # From analysis specifications.
          caliper = psCfg$params$caliper, # From analysis specifications.
          caliperScale = psCfg$params$caliperScale, # From analysis specifications.
          allowReverseMatch = FALSE, # Not specified in analysis specifications, using template default.
          stratificationColumns = c() # Not specified in analysis specifications, using template default.
        )
        stratifyByPsArgs <- NULL # No stratification if matching is used.
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL # No matching if stratification is used.
        # This block will not be executed as stratifyByPsArgs is null in analysis specifications.
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings. 'covariateSelection' in analysis specifications is empty.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Not specified in analysis specifications, using template default.
      )

      # Outcome list including the main outcome and negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not specified in analysis specifications, using template default.
            priorOutcomeLookback = 365 # From analysis specifications createStudyPopArgs.priorOutcomeLookBack.
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # Not specified in analysis specifications, using template default.
          )
        })
      )
      
      # Target-Comparator-Outcomes list.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i], # Renumbered target cohort ID.
          comparatorId = cmTcList$comparatorCohortId[i], # Renumbered comparator cohort ID.
          outcomes = outcomeList,
          # excludedCovariateConceptIds is empty as 'conceptsToExclude' is empty in analysis specifications,
          # and cmTcList does not contain target/comparator concept IDs directly.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # getDbCohortMethodDataArgs settings from analysis specifications.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Not specified in analysis specifications for this function, using template default.
        studyStartDate = studyStartDate, # From analysis specifications (will be "" if null).
        studyEndDate = studyEndDate, # From analysis specifications (will be "" if null).
        maxCohortSize = 0, # From analysis specifications.
        covariateSettings = covariateSettings
      )

      # createPsArgs settings from analysis specifications.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From analysis specifications.
        errorOnHighCorrelation = TRUE, # From analysis specifications.
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail (template default).
        estimator = "att", # Not specified in analysis specifications, using template default.
        prior = Cyclops::createPrior( # prior settings from analysis specifications.
          priorType = "laplace", # From analysis specifications.
          exclude = c(0), # Template default for Laplace prior.
          useCrossValidation = TRUE # From analysis specifications.
        ),
        control = Cyclops::createControl( # control settings from analysis specifications.
          noiseLevel = "silent", # From analysis specifications.
          cvType = "auto", # From analysis specifications.
          # seed = 1, # Not specified in analysis specifications, removed from template default.
          resetCoefficients = TRUE, # From analysis specifications.
          tolerance = 2e-07, # From analysis specifications.
          cvRepetitions = 10, # From analysis specifications (template had 1).
          startingVariance = 0.01 # From analysis specifications.
        )
      )

      # computeSharedCovariateBalanceArgs settings. Not specified in analysis specifications, using template defaults.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Template default.
        covariateFilter = NULL # Template default.
      )
      # computeCovariateBalanceArgs settings. Not specified in analysis specifications, using template defaults.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Template default.
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Template default.
      )

      # fitOutcomeModelArgs settings from analysis specifications.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From analysis specifications.
        stratified = FALSE, # From analysis specifications (template had TRUE).
        useCovariates = FALSE, # From analysis specifications.
        inversePtWeighting = FALSE, # From analysis specifications.
        prior = Cyclops::createPrior( # prior settings from analysis specifications.
          priorType = "laplace", # From analysis specifications.
          useCrossValidation = TRUE # From analysis specifications.
        ),
        control = Cyclops::createControl( # control settings from analysis specifications.
          cvType = "auto", # From analysis specifications.
          # seed = 1, # Not specified in analysis specifications, removed from template default.
          resetCoefficients = TRUE, # From analysis specifications.
          startingVariance = 0.01, # From analysis specifications.
          tolerance = 2e-07, # From analysis specifications.
          cvRepetitions = 10, # From analysis specifications (template had 1).
          noiseLevel = "quiet" # From analysis specifications.
        )
      )
      
      # createStudyPopArgs settings from analysis specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From analysis specifications.
        firstExposureOnly = FALSE, # From analysis specifications.
        washoutPeriod = 365, # From analysis specifications.
        removeDuplicateSubjects = "keep all", # From analysis specifications (template had "keep first").
        censorAtNewRiskWindow = FALSE, # From analysis specifications (template had TRUE).
        removeSubjectsWithPriorOutcome = TRUE, # From analysis specifications.
        priorOutcomeLookback = 365, # From analysis specifications.
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From analysis specifications.
        startAnchor = timeAtRisks$startAnchor[t], # From analysis specifications.
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From analysis specifications.
        endAnchor = timeAtRisks$endAnchor[t], # From analysis specifications.
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From analysis specifications.
        maxDaysAtRisk = 99999 # Not specified in analysis specifications, using template default.
      )


      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        # Generate a description based on current analysis settings.
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          # Using "NoRestriction" if study dates are empty/NA.
          ifelse(studyStartDate == "", "NoRestriction", studyStartDate),
          ifelse(studyEndDate == "", "NoRestriction", studyEndDate),
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

# CohortMethodModule specifications.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Not specified in analysis specifications, using template default.
  refitPsForEveryOutcome = FALSE, # Not specified in analysis specifications, using template default.
  refitPsForEveryStudyPopulation = FALSE, # Not specified in analysis specifications, using template default.
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Not specified in analysis specifications, using template default.
)

# Create the analysis specifications ------------------------------------------
# Initialize empty analysis specifications.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Add shared resources for cohort definitions.
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  # Add shared resources for negative control outcomes.
  Strategus::addSharedResources(negativeControlsShared) |>
  # Add CohortGenerator module specifications.
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  # Add CohortDiagnostics module specifications.
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  # Add CohortMethod module specifications.
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the complete analysis specifications to a JSON file.
# The filename uses the 'name' from analysis specifications: "ranitidinecancer".
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "ranitidinecancer", "ranitidinecancerAnalysisSpecification.json")
)