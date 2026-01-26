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

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Define the cohort IDs for target, comparator, and outcome cohorts from Analysis Specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to a simpler scheme (1, 2, 3...) for internal use in the study.
# Target cohort (ID 1794126) is re-numbered to 1.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# Comparator cohort (ID 1794132) is re-numbered to 2.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# Outcome cohort (ID 1794131) is re-numbered to 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Define negative control outcomes using the concept set ID from Analysis Specifications.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # negativeControlConceptSet.id from Analysis Specifications
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
  mutate(cohortId = row_number() + 100) %>% # target/comparator cohort ids start with 1, 2, 3... negativeControl -> 101, 102, 103...
  select(cohortId, cohortName, outcomeConceptId)


if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes:
# Filter for the outcome cohort (re-numbered ID 3) from Analysis Specifications.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Corresponds to outcomeCohort (ID 1794131)
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not specified in Analysis Specifications

# Target and Comparator for the CohortMethod analysis
# Use the re-numbered target (ID 1) and comparator (ID 2) cohort IDs and names.
cmTcList <- data.frame(
  targetCohortId = 1, # Corresponds to targetCohort (ID 1794126)
  targetCohortName = "target1", # Name from Analysis Specifications
  comparatorCohortId = 2, # Corresponds to comparatorCohort (ID 1794132)
  comparatorCohortName = "comparator1" # Name from Analysis Specifications
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study. The 'conceptsToExclude' in Analysis Specifications is empty, so this
# dataframe will be empty. Specific target/comparator drug concepts will be
# handled directly in createTargetComparatorOutcomes.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all
# The 'conceptsToInclude' in Analysis Specifications is empty, so this is commented out.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default, not specified in Analysis Specifications
  detectOnDescendants = TRUE # Default, not specified in Analysis Specifications
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Default, not specified in Analysis Specifications
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE, # Default, not specified in Analysis Specifications
  runIncludedSourceConcepts = TRUE, # Default, not specified in Analysis Specifications
  runOrphanConcepts = TRUE, # Default, not specified in Analysis Specifications
  runTimeSeries = FALSE, # Default, not specified in Analysis Specifications
  runVisitContext = TRUE, # Default, not specified in Analysis Specifications
  runBreakdownIndexEvents = TRUE, # Default, not specified in Analysis Specifications
  runIncidenceRate = TRUE, # Default, not specified in Analysis Specifications
  runCohortRelationship = TRUE, # Default, not specified in Analysis Specifications
  runTemporalCohortCharacterization = TRUE, # Default, not specified in Analysis Specifications
  minCharacterizationMean = 0.01 # Default, not specified in Analysis Specifications
)

# CohortMethodModule -----------------------------------------------------------

# Study periods from getDbCohortMethodDataArgs.studyPeriods in Analysis Specifications.
studyPeriods <- tibble(
  studyStartDate = c("20130401"), # YYYYMMDD
  studyEndDate   = c("20200331")  # YYYYMMDD
)

# Time-at-risks (TARs) for the outcomes of interest in your study
# From createStudyPopArgs.timeAtRisks in Analysis Specifications.
timeAtRisks <- tibble(
  label = c("TAR 1-0 (cohort end)"), # Descriptive label for the TAR
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end") # "cohort start" | "cohort end"
) 

# Propensity Score settings - match on PS
# From propensityScoreAdjustment.psSettings.matchOnPsArgs in Analysis Specifications.
matchOnPsArgsList <- tibble(
  label = c("Match on PS (Ratio 2, Caliper 0.2)"), # Descriptive label for PS setting
  maxRatio  = c(2),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # "propensity score" | "standardized" | "standardized logit"
) 

# Propensity Score settings - stratify by PS
# 'stratifyByPsArgs' is null in Analysis Specifications, so this list will be empty.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c() # "all" | "target" | "comparator"
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
# This block will not execute as stratifyByPsArgsList is empty based on Analysis Specifications.
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
        # Create matchOnPsArgs based on propensityScoreAdjustment.psSettings.matchOnPsArgs
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default, not specified in Analysis Specifications
          stratificationColumns = c() # Default, not specified in Analysis Specifications
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # This block will not be executed as only matching is specified in Analysis Specifications.
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings. 'covariateSelection' in Analysis Specifications is empty,
      # so using default settings.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default, not specified in Analysis Specifications
      )

      # Define outcomes, including the main outcome and negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Default, not specified in Analysis Specifications
            priorOutcomeLookback = 99999 # Default, not specified in Analysis Specifications
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # Default for negative controls, not specified in Analysis Specifications
          )
        })
      )
      
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude the target and comparator cohort IDs themselves from covariates.
          # 'excludedCovariateConcepts' from Analysis Specifications is empty.
          excludedCovariateConceptIds = c(
            cmTcList$targetCohortId[i], # Using targetCohortId as proxy for target drug concept
            cmTcList$comparatorCohortId[i] # Using comparatorCohortId as proxy for comparator drug concept
          )
        )
      }

      # getDbCohortMethodDataArgs from Analysis Specifications.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # From createStudyPopArgs.restrictToCommonPeriod in Analysis Specifications
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From getDbCohortMethodDataArgs.maxCohortSize in Analysis Specifications
        covariateSettings = covariateSettings
      )

      # createPsArgs from propensityScoreAdjustment.createPsArgs in Analysis Specifications.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications
        errorOnHighCorrelation = TRUE, # From Analysis Specifications
        stopOnError = FALSE, # Default, not specified in Analysis Specifications
        estimator = "att", # Default, not specified in Analysis Specifications
        prior = Cyclops::createPrior( # From propensityScoreAdjustment.createPsArgs.prior in Analysis Specifications
          priorType = "laplace", 
          exclude = c(0), # Default, not specified in Analysis Specifications
          useCrossValidation = TRUE # From Analysis Specifications
        ),
        control = Cyclops::createControl( # From propensityScoreAdjustment.createPsArgs.control in Analysis Specifications
          noiseLevel = "silent", # From Analysis Specifications
          cvType = "auto", # From Analysis Specifications
          fold = 10, # From Analysis Specifications
          cvRepetitions = 10, # From Analysis Specifications
          seed = 1, # Default, not specified in Analysis Specifications
          resetCoefficients = TRUE, # From Analysis Specifications
          tolerance = 2e-07, # From Analysis Specifications
          startingVariance = 0.01 # From Analysis Specifications
        )
      )

      # Default balance computation arguments, not specified in Analysis Specifications.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default, not specified in Analysis Specifications
        covariateFilter = NULL # Default, not specified in Analysis Specifications
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default, not specified in Analysis Specifications
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default, not specified in Analysis Specifications
      )

      # fitOutcomeModelArgs from Analysis Specifications.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From Analysis Specifications
        stratified = TRUE, # From Analysis Specifications
        useCovariates = FALSE, # From Analysis Specifications
        inversePtWeighting = FALSE, # From Analysis Specifications
        prior = Cyclops::createPrior( # From fitOutcomeModelArgs.prior in Analysis Specifications
          priorType = "laplace", 
          useCrossValidation = TRUE # From Analysis Specifications
        ),
        control = Cyclops::createControl( # From fitOutcomeModelArgs.control in Analysis Specifications
          cvType = "auto", # From Analysis Specifications
          fold = 10, # From Analysis Specifications
          cvRepetitions = 10, # From Analysis Specifications
          seed = 1, # Default, not specified in Analysis Specifications
          resetCoefficients = TRUE, # From Analysis Specifications
          startingVariance = 0.01, # From Analysis Specifications
          tolerance = 2e-07, # From Analysis Specifications
          noiseLevel = "quiet" # From Analysis Specifications
        )
      )
      
      # createStudyPopArgs from Analysis Specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE, # From createStudyPopArgs.restrictToCommonPeriod in Analysis Specifications
        firstExposureOnly = FALSE, # From createStudyPopArgs.firstExposureOnly in Analysis Specifications
        washoutPeriod = 0, # From createStudyPopArgs.washoutPeriod in Analysis Specifications
        removeDuplicateSubjects = "keep all", # From createStudyPopArgs.removeDuplicateSubjects in Analysis Specifications
        censorAtNewRiskWindow = FALSE, # From createStudyPopArgs.censorAtNewRiskWindow in Analysis Specifications
        removeSubjectsWithPriorOutcome = TRUE, # From createStudyPopArgs.removeSubjectsWithPriorOutcome in Analysis Specifications
        priorOutcomeLookback = 99999, # From createStudyPopArgs.priorOutcomeLookBack in Analysis Specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From createStudyPopArgs.timeAtRisks.minDaysAtRisk in Analysis Specifications
        maxDaysAtRisk = 99999 # Default, not specified in Analysis Specifications
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
  analysesToExclude = NULL, # Default, not specified in Analysis Specifications
  refitPsForEveryOutcome = FALSE, # Default, not specified in Analysis Specifications
  refitPsForEveryStudyPopulation = FALSE, # Default, not specified in Analysis Specifications
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default, not specified in Analysis Specifications
)

# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path uses the 'name' from Analysis Specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "sglt2imetformin", "sglt2imetforminAnalysisSpecification.json")
)