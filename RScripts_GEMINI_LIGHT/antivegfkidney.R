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
# Base URL for the OHDSI WebAPI, used to fetch cohort definitions and concept sets.
# This URL is fixed in the template and not provided in the analysis specification.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Extract cohort IDs from the analysis specifications for target, comparator, and outcome.
# These IDs are used to retrieve the full cohort definitions from WebAPI.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: antivegfkidney (from analysisSpecifications.cohortDefinitions.targetCohort.id)
    1794132, # Comparator: antivegfkidney (from analysisSpecifications.cohortDefinitions.comparatorCohort.id)
    1794131  # Outcome: antivegfkidney (from analysisSpecifications.cohortDefinitions.outcomeCohort[0].id)
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal consistency within Strategus modules.
# Target cohort (ID 1794126) is re-assigned ID 1.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# Comparator cohort (ID 1794132) is re-assigned ID 2.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# Outcome cohort (ID 1794131) is re-assigned ID 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve negative control concept set definition from WebAPI using the ID from analysis specifications.
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
  # Assign unique cohort IDs for negative controls, starting after the main study cohorts (1, 2, 3).
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs between study cohorts and negative controls to prevent conflicts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Filters the re-numbered cohortDefinitionSet for the outcome cohort (ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # 'cleanWindow' is not specified in the analysis specifications, using a template default.
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis.
# Uses the re-numbered IDs and names from analysis specifications.
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered ID for target1
  targetCohortName = "target1", # Name from analysisSpecifications.cohortDefinitions.targetCohort.name
  comparatorCohortId = 2, # Re-numbered ID for comparator1
  comparatorCohortName = "comparator1" # Name from analysisSpecifications.cohortDefinitions.comparatorCohort.name
)

# For the CohortMethod LSPS, define covariates to exclude.
# In this specification, 'conceptsToExclude' is an empty entry ({id: null, name: ""}),
# so an empty data frame is created.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# 'conceptsToInclude' is also an empty entry in the analysis specifications,
# so this section remains commented out as per the template.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# This module generates the cohorts based on the definitions provided.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Shared resource for main study cohorts.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Shared resource for negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Template default.
  detectOnDescendants = TRUE # Template default.
)
# Module specifications for cohort generation.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Template default.
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# This module performs diagnostics on the generated cohorts.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # Run diagnostics for all defined study cohorts.
  runInclusionStatistics = TRUE, # Template default.
  runIncludedSourceConcepts = TRUE, # Template default.
  runOrphanConcepts = TRUE, # Template default.
  runTimeSeries = FALSE, # Template default.
  runVisitContext = TRUE, # Template default.
  runBreakdownIndexEvents = TRUE, # Template default.
  runIncidenceRate = TRUE, # Template default.
  runCohortRelationship = TRUE, # Template default.
  runTemporalCohortCharacterization = TRUE, # Template default.
  minCharacterizationMean = 0.01 # Template default.
)

# CohortMethodModule -----------------------------------------------------------

# Study Periods: Defines specific date ranges for the study.
# From analysisSpecifications.getDbCohortMethodDataArgs.studyPeriods, which has null dates.
# Interpreted as no specific study period restriction, effectively running for all data.
# Represented as a single entry with empty strings to ensure the loop runs once.
studyPeriods <- tibble(
  studyStartDate = c(""), # If null in spec, interpreted as no restriction.
  studyEndDate   = c("")  # If null in spec, interpreted as no restriction.
)

# Time-at-risks (TARs) for the outcomes of interest.
# From analysisSpecifications.createStudyPopArgs.timeAtRisks.
timeAtRisks <- tibble(
  label = c("TAR_0_CS_0_CE"), # A descriptive label generated from the settings.
  riskWindowStart  = c(0), # From spec.createStudyPopArgs.timeAtRisks.riskWindowStart
  startAnchor = c("cohort start"), # From spec.createStudyPopArgs.timeAtRisks.startAnchor
  riskWindowEnd  = c(0), # From spec.createStudyPopArgs.timeAtRisks.riskWindowEnd
  endAnchor = c("cohort end"), # From spec.createStudyPopArgs.timeAtRisks.endAnchor
  minDaysAtRisk = c(1) # From spec.createStudyPopArgs.timeAtRisks.minDaysAtRisk
) 

# Propensity Score settings - match on PS.
# From analysisSpecifications.propensityScoreAdjustment.psSettings.matchOnPsArgs.
matchOnPsArgsList <- tibble(
  label = c("Match_MaxRatio1_Cal0.2_StdLogit"), # A descriptive label.
  maxRatio  = c(1), # From spec.propensityScoreAdjustment.psSettings.matchOnPsArgs.maxRatio
  caliper = c(0.2), # From spec.propensityScoreAdjustment.psSettings.matchOnPsArgs.caliper
  caliperScale  = c("standardized logit") # From spec.propensityScoreAdjustment.psSettings.matchOnPsArgs.caliperScale
) 

# Propensity Score settings - stratify by PS.
# From analysisSpecifications.propensityScoreAdjustment.psSettings.stratifyByPsArgs, which is null.
# Therefore, this tibble remains empty.
stratifyByPsArgsList <- tibble(
  label = character(),
  numberOfStrata  = integer(),
  baseSelection = character()
) 

# Build a single PS configuration list (each entry has: method, label, params).
# This list will contain the matching configuration defined above.
psConfigList <- list()

# If 'matchOnPsArgsList' has rows (which it does based on the spec), convert each row to a config.
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

# If 'stratifyByPsArgsList' has rows (which it doesn't in this spec), convert each row to a config.
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

# Iterate through all analysis setting combinations to create CohortMethod analysis specifications.
cmAnalysisList <- list()
analysisId <- 1

# Loop through each defined study period.
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  # Loop through each defined time-at-risk.
  for (t in seq_len(nrow(timeAtRisks))) {

    # Loop through each propensity score adjustment configuration (matching or stratification).
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Determine if PS matching or stratification is used for the current configuration.
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Template default.
          stratificationColumns = c() # Template default.
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Template default.
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings for FeatureExtraction.
      # 'conceptsToInclude' and 'conceptsToExclude' are null in spec, so default settings are used.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Template default.
      )

      # Combine study outcomes and negative control outcomes into a single list.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not specified in analysis spec, template default.
            priorOutcomeLookback = 99999 # Template default.
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # For negative controls, assumed true effect size is 1 (no effect).
          )
        })
      )
      
      # Define target-comparator-outcome combinations for the analysis.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # 'excludedCovariateConceptIds':
          # The analysis spec has 'conceptsToExclude' as null, so this list will be empty.
          # The template includes `cmTcList$targetConceptId[i]` and `cmTcList$comparatorConceptId[i]`,
          # but these are not present in the analysis spec, so they are removed.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId 
        )
      }

      # Arguments for retrieving cohort method data from the database.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        # 'restrictToCommonPeriod' is set to FALSE, derived from analysisSpecifications.createStudyPopArgs.restrictToCommonPeriod
        # (the template had TRUE, but the spec's createStudyPopArgs indicates FALSE).
        restrictToCommonPeriod = FALSE, 
        studyStartDate = studyStartDate, # From the current loop iteration.
        studyEndDate = studyEndDate, # From the current loop iteration.
        maxCohortSize = 0, # From analysisSpecifications.getDbCohortMethodDataArgs.maxCohortSize
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # All values are directly mapped from analysisSpecifications.propensityScoreAdjustment.createPsArgs.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From spec.propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From spec.propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Template default, allows Strategus to complete if PS model fitting fails.
        estimator = "att", # Template default.
        prior = Cyclops::createPrior( # prior = NULL if 'use regularization' == false
          priorType = "laplace", # From spec.propensityScoreAdjustment.createPsArgs.prior.priorType
          exclude = c(0), # Template default.
          useCrossValidation = TRUE # From spec.propensityScoreAdjustment.createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # control = NULL if 'use regularization' == false
          noiseLevel = "silent", # From spec.propensityScoreAdjustment.createPsArgs.control.noiseLevel
          cvType = "auto", # From spec.propensityScoreAdjustment.createPsArgs.control.cvType
          seed = 1, # Template default.
          resetCoefficients = TRUE, # From spec.propensityScoreAdjustment.createPsArgs.control.resetCoefficients
          tolerance = 2e-07, # From spec.propensityScoreAdjustment.createPsArgs.control.tolerance
          cvRepetitions = 10, # From spec.propensityScoreAdjustment.createPsArgs.control.cvRepetitions
          startingVariance = 0.01 # From spec.propensityScoreAdjustment.createPsArgs.control.startingVariance
        )
      )

      # Arguments for computing shared covariate balance (template defaults).
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      # Arguments for computing covariate balance (template defaults).
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting the outcome model.
      # All values are directly mapped from analysisSpecifications.fitOutcomeModelArgs.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From spec.fitOutcomeModelArgs.modelType
        stratified = FALSE, # From spec.fitOutcomeModelArgs.stratified
        useCovariates = FALSE, # From spec.fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE, # From spec.fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior( # prior = NULL if 'use regularization' == false
          priorType = "laplace", # From spec.fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE # From spec.fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # control = NULL if 'use regularization' == false
          cvType = "auto", # From spec.fitOutcomeModelArgs.control.cvType
          seed = 1, # Template default.
          resetCoefficients = TRUE, # From spec.fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01, # From spec.fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07, # From spec.fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10, # From spec.fitOutcomeModelArgs.control.cvRepetitions
          noiseLevel = "quiet" # From spec.fitOutcomeModelArgs.control.noiseLevel
        )
      )
      
      # Arguments for creating the study population.
      # All values are directly mapped from analysisSpecifications.createStudyPopArgs,
      # with TAR specific values from the current loop iteration.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From spec.createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = TRUE, # From spec.createStudyPopArgs.firstExposureOnly
        washoutPeriod = 365, # From spec.createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # From spec.createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # From spec.createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # From spec.createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # From spec.createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From current TAR loop.
        startAnchor = timeAtRisks$startAnchor[t], # From current TAR loop.
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From current TAR loop.
        endAnchor = timeAtRisks$endAnchor[t], # From current TAR loop.
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From current TAR loop.
        maxDaysAtRisk = 99999 # Template default, not in spec.
      )

      # Append the settings for the current analysis combination to the list.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(studyStartDate == "", "No restriction", studyStartDate), # Handle empty study dates for description.
          ifelse(studyEndDate == "", "No restriction", studyEndDate), # Handle empty study dates for description.
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
  analysesToExclude = NULL, # Template default.
  refitPsForEveryOutcome = FALSE, # Template default.
  refitPsForEveryStudyPopulation = FALSE, # Template default.
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Template default.
)

# Create the overall analysis specifications object. ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path uses the study name from the analysis specifications ("antivegfkidney").
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "antivegfkidney", "antivegfkidneyAnalysisSpecification.json")
)