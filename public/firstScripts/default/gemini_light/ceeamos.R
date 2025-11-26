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
# Base URL for OHDSI WebAPI to retrieve cohort and concept set definitions.
# This is a demo URL, replace with your institution's WebAPI URL if different.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Retrieves target, comparator, and outcome cohort definitions from WebAPI.
# Cohort IDs are taken directly from the "cohortDefinitions" in analysis specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1 (from analysis specifications)
    1794132, # Comparator: comparator1 (from analysis specifications)
    1794131  # Outcome: outcome1 (from analysis specifications)
  ),
  generateStats = TRUE
)

# Re-number cohorts to standard internal IDs for Strategus (1 for target, 2 for comparator, 3 for outcome).
# This is a common practice in OHDSI studies for consistency in analysis setup.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Renumber target cohort
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Renumber comparator cohort
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Renumber outcome cohort

# Negative control outcomes
# Retrieves concept set definition for negative controls using the provided conceptSetId.
# This concept set is then resolved into individual concepts, and assigned as outcome cohorts.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # From "negativeControlConceptSet.id" in analysis specifications
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
  # Assign unique cohort IDs for negative controls, starting from 101 to avoid clash with study cohorts.
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs between study cohorts and negative controls.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Filter for the primary outcome cohort based on its re-numbered ID (3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # cleanWindow is not specified in analysis, using template default.

# Target and Comparator for the CohortMethod analysis.
# Uses the re-numbered target (ID 1) and comparator (ID 2) cohorts.
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered target cohort ID
  targetCohortName = "target1", # Name from analysis specifications
  comparatorCohortId = 2, # Re-numbered comparator cohort ID
  comparatorCohortName = "comparator1" # Name from analysis specifications
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study. "covariateSelection.conceptsToExclude" is empty in analysis specifications.
excludedCovariateConcepts <- data.frame(
  conceptId = c(), # Empty as "covariateSelection.conceptsToExclude" is empty
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all
# "covariateSelection.conceptsToInclude" is empty in analysis specifications.
includedCovariateConcepts <- data.frame(
  conceptId = c(), # Empty as "covariateSelection.conceptsToInclude" is empty
  conceptName = c()
)


# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Creates shared resource specifications for study cohorts.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Creates shared resource specifications for negative control outcomes.
# occurrenceType and detectOnDescendants are template defaults, not specified.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", 
  detectOnDescendants = TRUE
)
# Creates module specifications for cohort generation, generating statistics.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Creates module specifications for Cohort Diagnostics.
# All parameters are set to TRUE or default values as per template, as no specific
# CohortDiagnosticsModule settings were provided in the analysis specifications.
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
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

# If you are not restricting your study to a specific time window, 
# please make these strings empty.
# "getDbCohortMethodDataArgs.studyPeriods" in analysis specifications is empty.
studyPeriods <- tibble(
  studyStartDate = character(), # Empty as per analysis specifications
  studyEndDate   = character()  # Empty as per analysis specifications
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# Populated from "createStudyPopArgs.timeAtRisks" in analysis specifications.
timeAtRisks <- tibble(
  label = c("TAR_1_0_CE", "TAR_1_99999_CS"), # Descriptive labels for each time-at-risk window
  riskWindowStart  = c(1, 1),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd  = c(0, 99999),
  endAnchor = c("cohort end", "cohort start"),
  minDaysAtRisk = c(1, 1) # From analysis specifications
) 

# Propensity Score settings - match on PS
# Populated from "propensityScoreAdjustment.psSettings" where "matchOnPsArgs" is present.
matchOnPsArgsList <- tibble(
  label = c("Match_Ratio10_Cal0.2_StdLogit"), # Descriptive label
  maxRatio  = c(10), # From analysis specifications
  caliper = c(0.2), # From analysis specifications
  caliperScale  = c("standardized logit") # From analysis specifications
) 

# Propensity Score settings - stratify by PS
# "stratifyByPsArgs" is null in analysis specifications, so this list remains empty.
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
# This block will not execute as stratifyByPsArgsList is empty based on analysis specifications.
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

# Loop through defined study periods. If `studyPeriods` is empty, this loop will run once with empty strings.
for (s in seq_len(nrow(studyPeriods) + (nrow(studyPeriods) == 0))) { # Ensure loop runs at least once if studyPeriods is empty
  studyStartDate <- if (nrow(studyPeriods) > 0) studyPeriods$studyStartDate[s] else ""
  studyEndDate <- if (nrow(studyPeriods) > 0) studyPeriods$studyEndDate[s] else ""

  # Loop through defined time-at-risks.
  for (t in seq_len(nrow(timeAtRisks))) {

    # Loop through defined propensity score adjustment configurations.
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment arguments based on method (match or stratify).
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Template default
          stratificationColumns = c() # Template default
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Template default
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings: Uses default settings as no specific covariates to include/exclude are provided.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE, # Template default
        # excludedCovariateConceptIds and includedCovariateConceptIds are empty,
        # so they are not explicitly passed here from excludedCovariateConcepts/includedCovariateConcepts.
        # This will result in default covariate inclusion/exclusion behavior by FeatureExtraction.
      )

      # Define outcomes for the current analysis: primary outcome and negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # For real outcomes, true effect size is unknown
            priorOutcomeLookback = 99999 # Template default, and matches analysis specifications
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # For negative controls, assume true effect size is 1 (no effect)
          )
        })
      )

      # Define target-comparator-outcome combinations.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Excluded covariate concepts: Uses the empty list if no concepts were specified.
          # The template's placeholders (cmTcList$targetConceptId[i], cmTcList$comparatorConceptId[i])
          # are removed as they are not part of analysis specifications for cmTcList.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Arguments for retrieving cohort method data from the database.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate, # From loop, will be empty if no specific study periods
        studyEndDate = studyEndDate,     # From loop, will be empty if no specific study periods
        maxCohortSize = 0,               # From "getDbCohortMethodDataArgs.maxCohortSize" in analysis specifications
        restrictToCommonPeriod = FALSE,  # From "getDbCohortMethodDataArgs.restrictToCommonPeriod" in analysis specifications
        firstExposureOnly = FALSE,       # From "getDbCohortMethodDataArgs.firstExposureOnly" in analysis specifications
        washoutPeriod = 0,               # From "getDbCohortMethodDataArgs.washoutPeriod" in analysis specifications
        removeDuplicateSubjects = "keep first", # From "getDbCohortMethodDataArgs.removeDuplicateSubjects" in analysis specifications
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # Populated from "propensityScoreAdjustment.createPsArgs" in analysis specifications.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,              # From analysis specifications
        errorOnHighCorrelation = TRUE,                  # From analysis specifications
        stopOnError = FALSE,                            # Template default, not in specs
        estimator = "att",                              # Template default, not in specs
        prior = Cyclops::createPrior( 
          priorType = "laplace",                        # From analysis specifications
          exclude = c(0),                               # Template default, not in specs
          useCrossValidation = TRUE                     # From analysis specifications
        ),
        control = Cyclops::createControl( 
          noiseLevel = "silent",                        # From analysis specifications
          cvType = "auto",                              # From analysis specifications
          seed = 1,                                     # Template default, not in specs
          resetCoefficients = TRUE,                     # From analysis specifications
          tolerance = 2e-07,                            # From analysis specifications
          cvRepetitions = 10,                           # From analysis specifications (fold * cvRepetitions; here, cvRepetitions is 10)
          startingVariance = 0.01                       # From analysis specifications
          # The 'fold' parameter (10) in analysis specifications is handled by cvType="auto" in Cyclops.
        )
      )

      # Arguments for computing covariate balance. Template defaults used.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Template default
        covariateFilter = NULL  # Template default
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Template default
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Template default
      )

      # Arguments for fitting the outcome model.
      # Populated from "fitOutcomeModelArgs" in analysis specifications.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",                             # From analysis specifications
        stratified = TRUE,                             # From analysis specifications
        useCovariates = FALSE,                         # From analysis specifications
        inversePtWeighting = FALSE,                    # From analysis specifications
        prior = Cyclops::createPrior( 
          priorType = "laplace",                       # From analysis specifications
          useCrossValidation = TRUE                    # From analysis specifications
        ),
        control = Cyclops::createControl( 
          cvType = "auto",                             # From analysis specifications
          seed = 1,                                    # Template default, not in specs
          resetCoefficients = TRUE,                    # From analysis specifications
          startingVariance = 0.01,                     # From analysis specifications
          tolerance = 2e-07,                           # From analysis specifications
          cvRepetitions = 10,                          # From analysis specifications
          noiseLevel = "quiet"                         # From analysis specifications
          # The 'fold' parameter (10) in analysis specifications is handled by cvType="auto" in Cyclops.
        )
      )

      # Arguments for creating the study population.
      # Populated from "createStudyPopArgs" in analysis specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,             # From analysis specifications
        firstExposureOnly = FALSE,                  # From analysis specifications
        washoutPeriod = 0,                          # From analysis specifications
        removeDuplicateSubjects = "keep all",       # From analysis specifications
        censorAtNewRiskWindow = FALSE,              # From analysis specifications
        removeSubjectsWithPriorOutcome = TRUE,      # From analysis specifications
        priorOutcomeLookback = 99999,               # From analysis specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From current timeAtRisks configuration
        startAnchor = timeAtRisks$startAnchor[t],         # From current timeAtRisks configuration
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],     # From current timeAtRisks configuration
        endAnchor = timeAtRisks$endAnchor[t],             # From current timeAtRisks configuration
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],     # From current timeAtRisks configuration
        maxDaysAtRisk = 99999                       # Template default, not specified in analysis specifications
      )

      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        # Dynamic description reflecting current study period, time-at-risk, and PS adjustment.
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

# CohortMethod module specifications.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # No analyses to exclude specified.
  refitPsForEveryOutcome = FALSE, # Template default.
  refitPsForEveryStudyPopulation = FALSE, # Template default.
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Template default.
)

# Create the analysis specifications ------------------------------------------
# Combine all shared resources and module specifications into a single analysis specifications object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the complete analysis specifications to a JSON file.
# The filename and path are derived from the "name" in analysis specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "ceeamos", "ceeamosAnalysisSpecification.json") # "ceeamos" from "name" in analysis specifications
)