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
# Get the list of cohorts
# This URL is a placeholder; users should update it to their WebAPI instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions from Analysis Specifications
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to a standard internal representation (1, 2, 3...)
# This makes it easier to refer to them consistently within the analysis settings.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Target
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Comparator
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Outcome

# Negative control outcomes from Analysis Specifications
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Negative Control Concept Set ID from Analysis Specifications
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
  # Assign unique cohort IDs for negative controls, starting after the main cohorts
  mutate(cohortId = row_number() + 100) %>% 
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
  mutate(cleanWindow = 365) # cleanWindow not in analysis spec, keeping template default

# Target and Comparator for the CohortMethod analysis, using re-numbered IDs and names
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered Target Cohort ID
  targetCohortName = "target1", # Target Cohort Name from Analysis Specifications
  comparatorCohortId = 2, # Re-numbered Comparator Cohort ID
  comparatorCohortName = "comparator1" # Comparator Cohort Name from Analysis Specifications
)

# For the CohortMethod LSPS we'll need to exclude specific concepts as covariates.
# Based on Analysis Specifications, 'conceptsToExclude' is empty.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all
# Based on Analysis Specifications, 'conceptsToInclude' is empty, so this is commented out.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Share the defined cohort set with the CohortGenerator module
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Share the negative control outcome cohort set
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Not specified in JSON, keeping template default
  detectOnDescendants = TRUE # Not specified in JSON, keeping template default
)
# Create module specifications for CohortGenerator
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Not specified in JSON, keeping template default
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Create module specifications for CohortDiagnostics, using all cohort IDs defined
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # All re-numbered cohorts for diagnostics
  runInclusionStatistics = TRUE, # Not specified in JSON, keeping template default
  runIncludedSourceConcepts = TRUE, # Not specified in JSON, keeping template default
  runOrphanConcepts = TRUE, # Not specified in JSON, keeping template default
  runTimeSeries = FALSE, # Not specified in JSON, keeping template default
  runVisitContext = TRUE, # Not specified in JSON, keeping template default
  runBreakdownIndexEvents = TRUE, # Not specified in JSON, keeping template default
  runIncidenceRate = TRUE, # Not specified in JSON, keeping template default
  runCohortRelationship = TRUE, # Not specified in JSON, keeping template default
  runTemporalCohortCharacterization = TRUE, # Not specified in JSON, keeping template default
  minCharacterizationMean = 0.01 # Not specified in JSON, keeping template default
)

# CohortMethodModule -----------------------------------------------------------

# Define study periods based on 'getDbCohortMethodDataArgs.studyPeriods' from Analysis Specifications
studyPeriods <- tibble(
  studyStartDate = c("20101019"), # YYYYMMDD from Analysis Specifications
  studyEndDate   = c("20181231")  # YYYYMMDD from Analysis Specifications
)

# Time-at-risks (TARs) for the outcomes of interest, based on 'createStudyPopArgs.timeAtRisks'
timeAtRisks <- tibble(
  label = c("main"), # Adding a descriptive label for the TAR
  riskWindowStart  = c(1), # From Analysis Specifications
  startAnchor = c("cohort start"), # From Analysis Specifications
  riskWindowEnd  = c(0), # From Analysis Specifications
  endAnchor = c("cohort end"), # From Analysis Specifications
  minDaysAtRisk = c(1) # From Analysis Specifications
) 

# Propensity Score settings - match on PS, based on 'propensityScoreAdjustment.psSettings.matchOnPsArgs'
matchOnPsArgsList <- tibble(
  label = c("main ps match"), # Adding a descriptive label for the PS strategy
  maxRatio  = c(1), # From Analysis Specifications
  caliper = c(0.2), # From Analysis Specifications
  caliperScale  = c("standardized logit") # From Analysis Specifications
) 

# Propensity Score settings - stratify by PS
# Based on Analysis Specifications, 'stratifyByPsArgs' is null, so this list remains empty.
stratifyByPsArgsList <- tibble(
  label = character(),
  numberOfStrata  = integer(),
  baseSelection = character() 
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
      
      # Determine PS adjustment method based on the configuration
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Not specified in JSON, keeping template default
          stratificationColumns = c() # Not specified in JSON, keeping template default
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Not specified in JSON, keeping template default
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings. Analysis Specifications 'covariateSelection' is empty,
      # so using default settings.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Not specified in JSON, keeping template default
      )

      # List of outcomes (main outcome + negative controls)
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # For true effect size for main outcomes
            priorOutcomeLookback = 99999 # Not specified in JSON, keeping template default
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # For negative controls (assumed true effect size of 1)
          )
        })
      )

      # Define target-comparator-outcome combinations
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # excludedCovariateConceptIds: based on Analysis Specifications, 'conceptsToExclude' is empty.
          # The template's original references to cmTcList$targetConceptId/comparatorConceptId
          # were removed as cmTcList contains cohort IDs, not concept IDs for exclusion from covariates.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId 
        )
      }

      # GetDbCohortMethodDataArgs based on 'getDbCohortMethodDataArgs' from Analysis Specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Not specified in JSON, keeping template default
        studyStartDate = studyStartDate, # From studyPeriods
        studyEndDate = studyEndDate, # From studyPeriods
        maxCohortSize = 0, # From Analysis Specifications
        covariateSettings = covariateSettings
      )

      # CreatePsArgs based on 'propensityScoreAdjustment.createPsArgs' from Analysis Specifications
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications
        errorOnHighCorrelation = TRUE, # From Analysis Specifications
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail (template default)
        estimator = "att", # Not specified in JSON, keeping template default
        prior = Cyclops::createPrior( # Prior settings from Analysis Specifications
          priorType = "laplace", 
          exclude = c(0), # Not specified in JSON, keeping template default
          useCrossValidation = TRUE # From Analysis Specifications
        ),
        control = Cyclops::createControl( # Control settings from Analysis Specifications
          noiseLevel = "silent", # From Analysis Specifications
          cvType = "auto", # From Analysis Specifications
          seed = 1, # Not specified in JSON, keeping template default
          resetCoefficients = TRUE, # From Analysis Specifications
          tolerance = 2e-07, # From Analysis Specifications
          cvRepetitions = 10, # From Analysis Specifications ('fold' implies number of repetitions for CV)
          startingVariance = 0.01 # From Analysis Specifications
        )
      )

      # ComputeSharedCovariateBalanceArgs (keeping template defaults as not in Analysis Specifications)
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, 
        covariateFilter = NULL
      )
      # ComputeCovariateBalanceArgs (keeping template defaults as not in Analysis Specifications)
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # FitOutcomeModelArgs based on 'fitOutcomeModelArgs' from Analysis Specifications
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From Analysis Specifications
        stratified = FALSE, # From Analysis Specifications
        useCovariates = FALSE, # From Analysis Specifications
        inversePtWeighting = FALSE, # From Analysis Specifications
        prior = Cyclops::createPrior( # Prior settings from Analysis Specifications
          priorType = "laplace", 
          useCrossValidation = TRUE # From Analysis Specifications
        ),
        control = Cyclops::createControl( # Control settings from Analysis Specifications
          cvType = "auto", # From Analysis Specifications
          seed = 1, # Not specified in JSON, keeping template default
          resetCoefficients = TRUE, # From Analysis Specifications
          startingVariance = 0.01, # From Analysis Specifications
          tolerance = 2e-07, # From Analysis Specifications
          cvRepetitions = 10, # From Analysis Specifications ('fold' implies number of repetitions for CV)
          noiseLevel = "quiet" # From Analysis Specifications
        )
      )

      # CreateStudyPopArgs based on 'createStudyPopArgs' and 'timeAtRisks' from Analysis Specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications
        firstExposureOnly = TRUE, # From Analysis Specifications
        washoutPeriod = 365, # From Analysis Specifications
        removeDuplicateSubjects = "keep all", # From Analysis Specifications
        censorAtNewRiskWindow = FALSE, # From Analysis Specifications
        removeSubjectsWithPriorOutcome = TRUE, # From Analysis Specifications
        priorOutcomeLookback = 99999, # From Analysis Specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From timeAtRisks
        startAnchor = timeAtRisks$startAnchor[t], # From timeAtRisks
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From timeAtRisks
        endAnchor = timeAtRisks$endAnchor[t], # From timeAtRisks
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From timeAtRisks
        maxDaysAtRisk = 99999 # Not specified in JSON, keeping template default
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

# CohortMethodModule Specifications
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Not specified in JSON, keeping template default
  refitPsForEveryOutcome = FALSE, # Not specified in JSON, keeping template default
  refitPsForEveryStudyPopulation = FALSE, # Not specified in JSON, keeping template default 
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Not specified in JSON, keeping template default
)

# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "doacsandwarfarin", "doacsandwarfarinAnalysisSpecification.json") # File path using study name from Analysis Specifications
)