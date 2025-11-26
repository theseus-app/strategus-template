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
library(CohortMethod) # Required for CohortMethod functions
library(FeatureExtraction) # Required for createDefaultCovariateSettings
library(Cyclops) # Required for createPrior and createControl
library(ParallelLogger) # Required for saveSettingsToJson

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI" # Default base URL from template

# Cohort Definitions
# IDs from analysisSpecifications$cohortDefinitions in the Analysis Specifications JSON
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: "target1" from analysisSpecifications$cohortDefinitions$targetCohort$id
    1794132, # Comparator: "comparator1" from analysisSpecifications$cohortDefinitions$comparatorCohort$id
    1794131 # Outcome: "outcome1" from analysisSpecifications$cohortDefinitions$outcomeCohort[1]$id
  ),
  generateStats = TRUE # Template default, not specified in JSON
)

# Re-number cohorts as per template for internal consistency in Strategus
# Target cohort ID is re-numbered to 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# Comparator cohort ID is re-numbered to 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# Outcome cohort ID is re-numbered to 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Concept set ID from analysisSpecifications$negativeControlConceptSet$id in the Analysis Specifications JSON
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # From analysisSpecifications$negativeControlConceptSet$id
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
  mutate(cohortId = row_number() + 100) %>% # Re-number negative control cohort IDs to start from 101, 102, ...
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs between study cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: 
# Filter for the re-numbered outcome cohort (ID 3)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Outcome cohort with re-numbered ID 3
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Template default, not specified in JSON

# Target and Comparator for the CohortMethod analysis 
# Using re-numbered target (ID 1) and comparator (ID 2) cohort IDs and names from analysisSpecifications
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered target cohort ID
  targetCohortName = "target1", # From analysisSpecifications$cohortDefinitions$targetCohort$name
  comparatorCohortId = 2, # Re-numbered comparator cohort ID
  comparatorCohortName = "comparator1" # From analysisSpecifications$cohortDefinitions$comparatorCohort$name
)

# For the CohortMethod analysis, we can specify covariates to exclude.
# Based on analysisSpecifications$covariateSelection$conceptsToExclude, which is an empty array in the JSON.
# Therefore, this data frame will be empty.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0), # analysisSpecifications$covariateSelection$conceptsToExclude is empty
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# analysisSpecifications$covariateSelection$conceptsToInclude is also an empty array, so keep this commented.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Template default, not specified in JSON
  detectOnDescendants = TRUE # Template default, not specified in JSON
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Template default, not specified in JSON
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # All cohort IDs (T, C, O, NC)
  runInclusionStatistics = TRUE, # Template default, not specified in JSON
  runIncludedSourceConcepts = TRUE, # Template default, not specified in JSON
  runOrphanConcepts = TRUE, # Template default, not specified in JSON
  runTimeSeries = FALSE, # Template default, not specified in JSON
  runVisitContext = TRUE, # Template default, not specified in JSON
  runBreakdownIndexEvents = TRUE, # Template default, not specified in JSON
  runIncidenceRate = TRUE, # Template default, not specified in JSON
  runCohortRelationship = TRUE, # Template default, not specified in JSON
  runTemporalCohortCharacterization = TRUE, # Template default, not specified in JSON
  minCharacterizationMean = 0.01 # Template default, not specified in JSON
)

# CohortMethodModule -----------------------------------------------------------

# Study periods from analysisSpecifications$getDbCohortMethodDataArgs$studyPeriods in JSON
studyPeriods <- tibble(
  studyStartDate = c(20171201), # From analysisSpecifications$getDbCohortMethodDataArgs$studyPeriods[1]$studyStartDate
  studyEndDate   = c(20231231)  # From analysisSpecifications$getDbCohortMethodDataArgs$studyPeriods[1]$studyEndDate
)

# Time-at-risks (TARs) for the outcomes of interest in your study
# From analysisSpecifications$createStudyPopArgs$timeAtRisks in JSON
timeAtRisks <- tibble(
  label = c("Default TAR"), # Custom label for clarity in analysis description
  riskWindowStart  = c(1), # From analysisSpecifications$createStudyPopArgs$timeAtRisks[1]$riskWindowStart
  startAnchor = c("cohort start"), # From analysisSpecifications$createStudyPopArgs$timeAtRisks[1]$startAnchor
  riskWindowEnd  = c(0), # From analysisSpecifications$createStudyPopArgs$timeAtRisks[1]$riskWindowEnd
  endAnchor = c("cohort end") # From analysisSpecifications$createStudyPopArgs$timeAtRisks[1]$endAnchor
) 

# Propensity Score settings - match on PS
# From analysisSpecifications$propensityScoreAdjustment$psSettings where matchOnPsArgs is not null in JSON
matchOnPsArgsList <- tibble(
  label = c("Match 1:1, Caliper 0.2 Std Logit"), # Custom label for this setting
  maxRatio  = c(1), # From analysisSpecifications$propensityScoreAdjustment$psSettings[1]$matchOnPsArgs$maxRatio
  caliper = c(0.2), # From analysisSpecifications$propensityScoreAdjustment$psSettings[1]$matchOnPsArgs$caliper
  caliperScale  = c("standardized logit") # From analysisSpecifications$propensityScoreAdjustment$psSettings[1]$matchOnPsArgs$caliperScale
) 

# Propensity Score settings - stratify by PS
# From analysisSpecifications$propensityScoreAdjustment$psSettings where stratifyByPsArgs is not null in JSON
stratifyByPsArgsList <- tibble(
  label = c("Stratify by 5 strata, base all"), # Custom label for this setting
  numberOfStrata  = c(5), # From analysisSpecifications$propensityScoreAdjustment$psSettings[2]$stratifyByPsArgs$numberOfStrata
  baseSelection = c("all") # From analysisSpecifications$propensityScoreAdjustment$psSettings[2]$stratifyByPsArgs$baseSelection
) 

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Convert "match on PS" settings from the data frame into the psConfigList
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

# Convert "stratify by PS" settings from the data frame into the psConfigList
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

# Iterate through all analysis setting combinations to create CohortMethod analyses
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Determine PS adjustment arguments based on the current PS configuration
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Template default, not specified in JSON
          stratificationColumns = c() # Template default, not specified in JSON
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Template default, not specified in JSON
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings for propensity score model
      # analysisSpecifications$covariateSelection$conceptsToInclude and $conceptsToExclude are empty in JSON.
      # Using createDefaultCovariateSettings for a broad set of covariates.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Template default, not specified in JSON
      )

      # Outcome list for CohortMethod. Includes primary outcomes and negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE, # Designated as outcome of interest
            trueEffectSize = NA, # Template default for primary outcomes, not specified in JSON (unknown true effect)
            priorOutcomeLookback = 99999 # Template default, not specified in JSON
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE, # Designated as negative control
            trueEffectSize = 1 # Template default for negative controls, not specified in JSON (assumed true effect of 1)
          )
        })
      )
      
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # excludedCovariateConceptIds from analysisSpecifications$covariateSelection$conceptsToExclude, which is empty.
          # The template had placeholder target/comparator concept IDs, but the JSON only provides cohort IDs, not specific concepts to exclude from the outcome model.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId # This correctly resolves to integer(0) because excludedCovariateConcepts is empty
        )
      }

      # getDbCohortMethodDataArgs from analysisSpecifications$getDbCohortMethodDataArgs in JSON
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate, # From loop variable, derived from analysisSpecifications$getDbCohortMethodDataArgs$studyPeriods
        studyEndDate = studyEndDate, # From loop variable, derived from analysisSpecifications$getDbCohortMethodDataArgs$studyPeriods
        maxCohortSize = 0, # From analysisSpecifications$getDbCohortMethodDataArgs$maxCohortSize (0 means no restriction)
        restrictToCommonPeriod = TRUE, # From analysisSpecifications$getDbCohortMethodDataArgs$restrictToCommonPeriod
        firstExposureOnly = FALSE, # From analysisSpecifications$getDbCohortMethodDataArgs$firstExposureOnly
        washoutPeriod = 0, # From analysisSpecifications$getDbCohortMethodDataArgs$washoutPeriod
        removeDuplicateSubjects = "keep all", # From analysisSpecifications$getDbCohortMethodDataArgs$removeDuplicateSubjects
        covariateSettings = covariateSettings
      )

      # createPsArgs from analysisSpecifications$propensityScoreAdjustment$createPsArgs in JSON
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From analysisSpecifications$propensityScoreAdjustment$createPsArgs$maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From analysisSpecifications$propensityScoreAdjustment$createPsArgs$errorOnHighCorrelation
        stopOnError = FALSE, # Template default, not in JSON (allows Strategus to continue if PS model fitting fails)
        estimator = "att", # Template default, not in JSON
        prior = Cyclops::createPrior( # From analysisSpecifications$propensityScoreAdjustment$createPsArgs$prior
          priorType = "laplace", # From analysisSpecifications$propensityScoreAdjustment$createPsArgs$prior$priorType
          # 'exclude' parameter not specified in JSON, so removed from template.
          useCrossValidation = TRUE # From analysisSpecifications$propensityScoreAdjustment$createPsArgs$prior$useCrossValidation
        ),
        control = Cyclops::createControl( # From analysisSpecifications$propensityScoreAdjustment$createPsArgs$control
          noiseLevel = "silent", # From analysisSpecifications$propensityScoreAdjustment$createPsArgs$control$noiseLevel
          cvType = "auto", # From analysisSpecifications$propensityScoreAdjustment$createPsArgs$control$cvType
          seed = 1, # Template default, not in JSON
          resetCoefficients = TRUE, # From analysisSpecifications$propensityScoreAdjustment$createPsArgs$control$resetCoefficients
          tolerance = 2e-07, # From analysisSpecifications$propensityScoreAdjustment$createPsArgs$control$tolerance
          cvRepetitions = 10, # From analysisSpecifications$propensityScoreAdjustment$createPsArgs$control$cvRepetitions (JSON specified 10, template was 1)
          startingVariance = 0.01 # From analysisSpecifications$propensityScoreAdjustment$createPsArgs$control$startingVariance
        )
      )

      # computeSharedCovariateBalanceArgs and computeCovariateBalanceArgs are template defaults, not specified in JSON.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Template default, not in JSON
        covariateFilter = NULL # Template default, not in JSON
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Template default, not in JSON
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Template default, not in JSON
      )

      # fitOutcomeModelArgs from analysisSpecifications$fitOutcomeModelArgs in JSON
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From analysisSpecifications$fitOutcomeModelArgs$modelType
        stratified = TRUE, # From analysisSpecifications$fitOutcomeModelArgs$stratified
        useCovariates = FALSE, # From analysisSpecifications$fitOutcomeModelArgs$useCovariates
        inversePtWeighting = FALSE, # From analysisSpecifications$fitOutcomeModelArgs$inversePtWeighting
        prior = Cyclops::createPrior( # From analysisSpecifications$fitOutcomeModelArgs$prior
          priorType = "laplace", # From analysisSpecifications$fitOutcomeModelArgs$prior$priorType
          useCrossValidation = TRUE # From analysisSpecifications$fitOutcomeModelArgs$prior$useCrossValidation
        ),
        control = Cyclops::createControl( # From analysisSpecifications$fitOutcomeModelArgs$control
          cvType = "auto", # From analysisSpecifications$fitOutcomeModelArgs$control$cvType
          seed = 1, # Template default, not in JSON
          resetCoefficients = TRUE, # From analysisSpecifications$fitOutcomeModelArgs$control$resetCoefficients
          startingVariance = 0.01, # From analysisSpecifications$fitOutcomeModelArgs$control$startingVariance
          tolerance = 2e-07, # From analysisSpecifications$fitOutcomeModelArgs$control$tolerance
          cvRepetitions = 10, # From analysisSpecifications$fitOutcomeModelArgs$control$cvRepetitions (JSON specified 10, template was 1)
          noiseLevel = "quiet" # From analysisSpecifications$fitOutcomeModelArgs$control$noiseLevel
        )
      )
      
      # createStudyPopArgs from analysisSpecifications$createStudyPopArgs in JSON and loop variables
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From analysisSpecifications$createStudyPopArgs$restrictToCommonPeriod
        firstExposureOnly = FALSE, # From analysisSpecifications$createStudyPopArgs$firstExposureOnly
        washoutPeriod = 0, # From analysisSpecifications$createStudyPopArgs$washoutPeriod
        removeDuplicateSubjects = "keep all", # From analysisSpecifications$createStudyPopArgs$removeDuplicateSubjects
        censorAtNewRiskWindow = TRUE, # From analysisSpecifications$createStudyPopArgs$censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # From analysisSpecifications$createStudyPopArgs$removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # From analysisSpecifications$createStudyPopArgs$priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From loop variable, derived from analysisSpecifications$createStudyPopArgs$timeAtRisks
        startAnchor = timeAtRisks$startAnchor[t], # From loop variable, derived from analysisSpecifications$createStudyPopArgs$timeAtRisks
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From loop variable, derived from analysisSpecifications$createStudyPopArgs$timeAtRisks
        endAnchor = timeAtRisks$endAnchor[t], # From loop variable, derived from analysisSpecifications$createStudyPopArgs$timeAtRisks
        minDaysAtRisk = 1, # From analysisSpecifications$createStudyPopArgs$timeAtRisks[1]$minDaysAtRisk
        maxDaysAtRisk = 99999 # Template default, not specified in JSON
      )

      # Append the combined settings to the CohortMethod Analysis List
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
  analysesToExclude = NULL, # Template default, not specified in JSON
  refitPsForEveryOutcome = FALSE, # Template default, not specified in JSON
  refitPsForEveryStudyPopulation = FALSE, # Template default, not specified in JSON
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Template default, not specified in JSON
)

# Create the overall analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file
# The file path uses the name "semaglutideandnaion" from analysisSpecifications$name in the JSON
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "semaglutideandnaion", "semaglutideandnaionAnalysisSpecification.json")
)