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

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
# Note: baseUrl is not provided in Analysis Specifications, using a placeholder.
# Users should replace this with their actual WebAPI URL.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI" 

# Cohort Definitions from Analysis Specifications
# Target: antivegfkidney.cohortDefinitions.targetCohort
# Comparator: antivegfkidney.cohortDefinitions.comparatorCohort
# Outcome: antivegfkidney.cohortDefinitions.outcomeCohort
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to a simplified scheme for internal use (1, 2, 3...)
# Target cohort ID 1794126 -> 1
# Comparator cohort ID 1794132 -> 2
# Outcome cohort ID 1794131 -> 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes from Analysis Specifications
# Negative Control Concept Set ID: antivegfkidney.negativeControlConceptSet.id
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # From analysisSpecifications.negativeControlConceptSet.id
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


if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ------
# Outcomes: based on re-numbered cohort IDs
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Re-numbered outcome cohort ID is 3
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default cleanWindow, not specified in analysis specifications

# Target and Comparator for the CohortMethod analysis
# Based on re-numbered cohort IDs 1 and 2
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered target cohort ID
  targetCohortName = cohortDefinitionSet %>% filter(cohortId == 1) %>% pull(cohortName),
  comparatorCohortId = 2, # Re-numbered comparator cohort ID
  comparatorCohortName = cohortDefinitionSet %>% filter(cohortId == 2) %>% pull(cohortName)
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study. These are specified in covariateSelection.conceptsToExclude.
# As per analysis specifications, this list is empty (id: null, name: "").
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(), # Empty as per analysisSpecifications.covariateSelection.conceptsToExclude
  conceptName = character()
)

# Optional: If you want to define covariates to include instead of including them all
# As per analysis specifications, this list is empty (id: null, name: "").
# includedCovariateConcepts <- data.frame(
#   conceptId = numeric(), 
#   conceptName = character()
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default, not in analysis specifications
  detectOnDescendants = TRUE # Default, not in analysis specifications
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # From template, not explicitly in analysis specifications but good practice
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # All re-numbered cohorts
  runInclusionStatistics = TRUE, # Default, not in analysis specifications
  runIncludedSourceConcepts = TRUE, # Default, not in analysis specifications
  runOrphanConcepts = TRUE, # Default, not in analysis specifications
  runTimeSeries = FALSE, # Default, not in analysis specifications
  runVisitContext = TRUE, # Default, not in analysis specifications
  runBreakdownIndexEvents = TRUE, # Default, not in analysis specifications
  runIncidenceRate = TRUE, # Default, not in analysis specifications
  runCohortRelationship = TRUE, # Default, not in analysis specifications
  runTemporalCohortCharacterization = TRUE, # Default, not in analysis specifications
  minCharacterizationMean = 0.01 # Default, not in analysis specifications
)

# CohortMethodModule -----------------------------------------------------------

# If you are not restricting your study to a specific time window, 
# please make these strings empty.
# From analysisSpecifications.getDbCohortMethodDataArgs.studyPeriods
# An empty string "" for studyStartDate/studyEndDate means no date restriction.
studyPeriods <- tibble(
  studyStartDate = c(""), # From analysisSpecifications.getDbCohortMethodDataArgs.studyPeriods[0].studyStartDate
  studyEndDate   = c("")  # From analysisSpecifications.getDbCohortMethodDataArgs.studyPeriods[0].studyEndDate
)

# Time-at-risks (TARs) for the outcomes of interest in your study
# From analysisSpecifications.createStudyPopArgs.timeAtRisks
timeAtRisks <- tibble(
  label = c("TAR1_1_to_0_CE", "TAR2_1_to_99999_CS"), # Generating simple labels
  riskWindowStart  = c(1, 1), # From analysisSpecifications.createStudyPopArgs.timeAtRisks[0].riskWindowStart, [1].riskWindowStart
  startAnchor = c("cohort start", "cohort start"), # From analysisSpecifications.createStudyPopArgs.timeAtRisks[0].startAnchor, [1].startAnchor
  riskWindowEnd  = c(0, 99999), # From analysisSpecifications.createStudyPopArgs.timeAtRisks[0].riskWindowEnd, [1].riskWindowEnd
  endAnchor = c("cohort end", "cohort start"), # From analysisSpecifications.createStudyPopArgs.timeAtRisks[0].endAnchor, [1].endAnchor
  minDaysAtRisk = c(1, 1) # From analysisSpecifications.createStudyPopArgs.timeAtRisks[0].minDaysAtRisk, [1].minDaysAtRisk
) 

# Propensity Score settings - match on PS
# From analysisSpecifications.propensityScoreAdjustment.psSettings
# Only one matchOnPsArgs setting is provided
matchOnPsArgsList <- tibble(
  label = c("match_1to1_cal0.2_stdLogit"), # Generating simple label
  maxRatio  = c(1), # From analysisSpecifications.propensityScoreAdjustment.psSettings[0].matchOnPsArgs.maxRatio
  caliper = c(0.2), # From analysisSpecifications.propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliper
  caliperScale  = c("standardized logit") # From analysisSpecifications.propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliperScale
) 

# Propensity Score settings - stratify by PS
# From analysisSpecifications.propensityScoreAdjustment.psSettings
# stratifyByPsArgs is null, so this list will be empty
stratifyByPsArgsList <- tibble(
  label = character(),
  numberOfStrata  = numeric(),
  baseSelection = character(),
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

# Loop over study periods (if studyPeriods is empty, this loop will still run once if "" is present)
for (s in seq_len(nrow(studyPeriods))) {
  currentStudyStartDate <- studyPeriods$studyStartDate[s]
  currentStudyEndDate <- studyPeriods$studyEndDate[s]

  # Loop over time-at-risks
  for (t in seq_len(nrow(timeAtRisks))) {

    # Loop over propensity score adjustment settings
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Determine PS adjustment method
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio, # From psConfigList (derived from analysisSpecifications.propensityScoreAdjustment.psSettings.matchOnPsArgs.maxRatio)
          caliper = psCfg$params$caliper, # From psConfigList (derived from analysisSpecifications.propensityScoreAdjustment.psSettings.matchOnPsArgs.caliper)
          caliperScale = psCfg$params$caliperScale, # From psConfigList (derived from analysisSpecifications.propensityScoreAdjustment.psSettings.matchOnPsArgs.caliperScale)
          allowReverseMatch = FALSE, # Default, not in analysis specifications
          stratificationColumns = c() # Default, not in analysis specifications
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata, # From psConfigList (derived from analysisSpecifications.propensityScoreAdjustment.psSettings.stratifyByPsArgs.numberOfStrata)
          stratificationColumns = c(), # Default, not in analysis specifications
          baseSelection = psCfg$params$baseSelection # From psConfigList (derived from analysisSpecifications.propensityScoreAdjustment.psSettings.stratifyByPsArgs.baseSelection)
        )
      }

      # Covariate settings for feature extraction
      # As per analysisSpecifications.covariateSelection, no specific concepts to include/exclude for general covariates.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default, not in analysis specifications
        # No specific concept IDs from analysisSpecifications.covariateSelection.conceptsToInclude/Exclude
        # as they are listed as null/empty in the JSON
      )

      # List of outcomes (actual outcomes + negative controls)
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i], # Re-numbered outcome cohort ID
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not applicable for real outcomes
            priorOutcomeLookback = 99999 # From analysisSpecifications.createStudyPopArgs.priorOutcomeLookBack
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i, # Negative control cohort ID
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # Expected effect size for negative controls
          )
        })
      )

      # Target-comparator-outcome list
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i], # Re-numbered target cohort ID
          comparatorId = cmTcList$comparatorCohortId[i], # Re-numbered comparator cohort ID
          outcomes = outcomeList,
          # Exclude the target and comparator concept IDs themselves, plus any specified
          # in analysisSpecifications.covariateSelection.conceptsToExclude (which is empty here)
          excludedCovariateConceptIds = c(
            cmTcList$targetCohortId[i], 
            cmTcList$comparatorCohortId[i],
            excludedCovariateConcepts$conceptId # This will be numeric(0) if dataframe is empty
          )
        )
      }

      # Arguments for fetching cohort method data
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # From analysisSpecifications.getDbCohortMethodDataArgs.restrictToCommonPeriod
        studyStartDate = currentStudyStartDate, # From studyPeriods (derived from analysisSpecifications.getDbCohortMethodDataArgs.studyPeriods)
        studyEndDate = currentStudyEndDate, # From studyPeriods (derived from analysisSpecifications.getDbCohortMethodDataArgs.studyPeriods)
        maxCohortSize = 0, # From analysisSpecifications.getDbCohortMethodDataArgs.maxCohortSize
        firstExposureOnly = FALSE, # From analysisSpecifications.getDbCohortMethodDataArgs.firstExposureOnly
        washoutPeriod = 0, # From analysisSpecifications.getDbCohortMethodDataArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # From analysisSpecifications.getDbCohortMethodDataArgs.removeDuplicateSubjects
        covariateSettings = covariateSettings # General covariate settings created above
      )

      # Arguments for creating propensity scores
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From analysisSpecifications.propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From analysisSpecifications.propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail (default in template)
        estimator = "att", # Default in template, not in analysis specifications
        prior = Cyclops::createPrior( # From analysisSpecifications.propensityScoreAdjustment.createPsArgs.prior
          priorType = "laplace", # From analysisSpecifications.propensityScoreAdjustment.createPsArgs.prior.priorType
          exclude = c(0), # Default in template, not in analysis specifications
          useCrossValidation = TRUE # From analysisSpecifications.propensityScoreAdjustment.createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # From analysisSpecifications.propensityScoreAdjustment.createPsArgs.control
          noiseLevel = "silent", # From analysisSpecifications.propensityScoreAdjustment.createPsArgs.control.noiseLevel
          cvType = "auto", # From analysisSpecifications.propensityScoreAdjustment.createPsArgs.control.cvType
          seed = 1, # Default in template, not in analysis specifications
          resetCoefficients = TRUE, # From analysisSpecifications.propensityScoreAdjustment.createPsArgs.control.resetCoefficients
          tolerance = 2e-07, # From analysisSpecifications.propensityScoreAdjustment.createPsArgs.control.tolerance
          numberOfCvFolds = 10, # 'fold' from analysisSpecifications.propensityScoreAdjustment.createPsArgs.control.fold
          cvRepetitions = 10, # From analysisSpecifications.propensityScoreAdjustment.createPsArgs.control.cvRepetitions
          startingVariance = 0.01 # From analysisSpecifications.propensityScoreAdjustment.createPsArgs.control.startingVariance
        )
      )

      # Arguments for computing shared covariate balance
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default in template, not in analysis specifications
        covariateFilter = NULL # Default in template, not in analysis specifications
      )
      # Arguments for computing covariate balance for table 1
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default in template, not in analysis specifications
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default in template, not in analysis specifications
      )

      # Arguments for fitting the outcome model
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From analysisSpecifications.fitOutcomeModelArgs.modelType
        stratified = FALSE, # From analysisSpecifications.fitOutcomeModelArgs.stratified
        useCovariates = FALSE, # From analysisSpecifications.fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE, # From analysisSpecifications.fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior( # From analysisSpecifications.fitOutcomeModelArgs.prior
          priorType = "laplace", # From analysisSpecifications.fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE # From analysisSpecifications.fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # From analysisSpecifications.fitOutcomeModelArgs.control
          cvType = "auto", # From analysisSpecifications.fitOutcomeModelArgs.control.cvType
          seed = 1, # Default in template, not in analysis specifications
          resetCoefficients = TRUE, # From analysisSpecifications.fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01, # From analysisSpecifications.fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07, # From analysisSpecifications.fitOutcomeModelArgs.control.tolerance
          numberOfCvFolds = 10, # 'fold' from analysisSpecifications.fitOutcomeModelArgs.control.fold
          cvRepetitions = 10, # From analysisSpecifications.fitOutcomeModelArgs.control.cvRepetitions
          noiseLevel = "quiet" # From analysisSpecifications.fitOutcomeModelArgs.control.noiseLevel
        )
      )

      # Arguments for creating the study population
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From analysisSpecifications.createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE, # From analysisSpecifications.createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0, # From analysisSpecifications.createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # From analysisSpecifications.createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # From analysisSpecifications.createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # From analysisSpecifications.createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # From analysisSpecifications.createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From timeAtRisks (derived from analysisSpecifications.createStudyPopArgs.timeAtRisks)
        startAnchor = timeAtRisks$startAnchor[t], # From timeAtRisks (derived from analysisSpecifications.createStudyPopArgs.timeAtRisks)
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From timeAtRisks (derived from analysisSpecifications.createStudyPopArgs.timeAtRisks)
        endAnchor = timeAtRisks$endAnchor[t], # From timeAtRisks (derived from analysisSpecifications.createStudyPopArgs.timeAtRisks)
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From timeAtRisks (derived from analysisSpecifications.createStudyPopArgs.timeAtRisks)
        maxDaysAtRisk = 99999 # Default, not in analysis specifications
      )


      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          currentStudyStartDate,
          currentStudyEndDate,
          timeAtRisks$label[t],
          psCfg$label
        ),
        getDbCohortMethodDataArgs = getDbCohortMethodDataArgs,
        createStudyPopArgs = createStudyPopArgs,
        createPsArgs = createPsArgs,
        matchOnPsArgs = matchOnPsArgs, # Null if stratify, populated if match
        stratifyByPsArgs = stratifyByPsArgs, # Populated if stratify, null if match
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
  analysesToExclude = NULL, # Default, not in analysis specifications
  refitPsForEveryOutcome = FALSE, # Default, not in analysis specifications
  refitPsForEveryStudyPopulation = FALSE, # Default, not in analysis specifications
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default, not in analysis specifications
)

# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the specifications to a JSON file
# The folder 'inst/studyName/' must exist for this to work.
# Replace "studyName" with the actual study name, e.g., "antivegfkidney".
# For example, file.path("inst", "antivegfkidney", "antivegfkidneyAnalysisSpecification.json")
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "antivegfkidney", "antivegfkidneyAnalysisSpecification.json")
)