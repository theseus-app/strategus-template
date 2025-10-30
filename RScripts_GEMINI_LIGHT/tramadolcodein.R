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
library(ROhdsiWebApi) # Required for interacting with an OHDSI WebAPI instance
library(CohortMethod) # Required for CohortMethod-specific functions
library(FeatureExtraction) # Required for covariate settings
library(Cyclops) # Required for prior and control settings in PS and outcome models

# Shared Resources -------------------------------------------------------------
# Base URL for the WebAPI instance where cohort definitions are stored.
# This is a placeholder and should be updated to your specific WebAPI instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions from WebAPI using their original IDs, as specified
# in the <Analysis Specifications>.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: "target1" from Analysis Specifications: "cohortDefinitions.targetCohort.id"
    1794132, # Comparator: "comparator1" from Analysis Specifications: "cohortDefinitions.comparatorCohort.id"
    1794131  # Outcome: "outcome1" from Analysis Specifications: "cohortDefinitions.outcomeCohort[0].id"
  ),
  generateStats = TRUE # Generate cohort statistics during export
)

# Re-number cohorts for internal use within Strategus.
# The re-numbered IDs (1, 2, 3...) simplify referencing.
# Ensure the cohortName is correctly associated with the new internal ID.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1,]$cohortName <- "target1" # From Analysis Specifications: "cohortDefinitions.targetCohort.name"

cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 2,]$cohortName <- "comparator1" # From Analysis Specifications: "cohortDefinitions.comparatorCohort.name"

cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 3,]$cohortName <- "outcome1" # From Analysis Specifications: "cohortDefinitions.outcomeCohort[0].name"


# Negative control outcomes
# Retrieve negative control concept set definition from WebAPI using the concept set ID.
# This will be used to generate negative control outcome cohorts.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # From Analysis Specifications: "negativeControlConceptSet.id"
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet( # Resolve concepts within the set
    baseUrl = baseUrl
  ) %>%
  ROhdsiWebApi::getConcepts( # Get detailed information for the resolved concepts
    baseUrl = baseUrl
  ) %>%
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  mutate(cohortId = row_number() + 100) %>% # Assign unique internal IDs for negative controls, starting from 101
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs between study cohorts and negative controls.
# Duplicate IDs would cause conflicts and errors in Strategus.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts used in each analysis ---------------
# Outcomes: This list defines the primary outcomes for the CohortMethod analysis.
# Filters for the re-numbered outcome cohort (ID 3) based on analysis specifications.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not explicitly specified in analysis specifications.

# Target and Comparator for the CohortMethod analysis
# Uses the re-numbered target (ID 1) and comparator (ID 2) cohorts.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1", # From Analysis Specifications: "cohortDefinitions.targetCohort.name"
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1" # From Analysis Specifications: "cohortDefinitions.comparatorCohort.name"
)

# Excluded covariate concepts:
# The 'covariateSelection.conceptsToExclude' in the analysis specifications is empty.
# Therefore, this data frame will be empty.
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(0),
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# The 'covariateSelection.conceptsToInclude' in the analysis specifications is empty.
# So, this is left commented out as in the template.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Shared resource for the main study cohorts.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Shared resource for negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default from template
  detectOnDescendants = TRUE # Default from template
)
# Module specifications for generating cohorts and associated statistics.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Generate statistics for the cohorts
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Module specifications for running cohort diagnostics.
# 'cohortIds' include all re-numbered study cohorts (target, comparator, outcome)
# and all negative control cohorts for comprehensive diagnostics.
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId),
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

# Study Periods: From Analysis Specifications "getDbCohortMethodDataArgs.studyPeriods".
# The JSON specifies `null` for studyStartDate and studyEndDate, indicating no restriction.
# We create a single entry with empty strings that will be interpreted as NULL later
# to signify no date restriction in CohortMethod.
studyPeriods <- tibble(
  studyStartDate = c(""), # Empty string will resolve to NULL, indicating no study start date restriction
  studyEndDate   = c("")  # Empty string will resolve to NULL, indicating no study end date restriction
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# Populated from Analysis Specifications "createStudyPopArgs.timeAtRisks".
timeAtRisks <- tibble(
  label = c(
    "0-0_CS-CE",    # Label for TAR 1: start at cohort start, end at cohort end, 0 offset
    "0-9999_CS-CS"  # Label for TAR 2: start at cohort start, end at cohort start + 9999 days, 0 offset
  ),
  riskWindowStart  = c(
    0,    # From Analysis Specifications: "createStudyPopArgs.timeAtRisks[0].riskWindowStart"
    0     # From Analysis Specifications: "createStudyPopArgs.timeAtRisks[1].riskWindowStart"
  ),
  startAnchor = c(
    "cohort start", # From Analysis Specifications: "createStudyPopArgs.timeAtRisks[0].startAnchor"
    "cohort start"  # From Analysis Specifications: "createStudyPopArgs.timeAtRisks[1].startAnchor"
  ),
  riskWindowEnd  = c(
    0,    # From Analysis Specifications: "createStudyPopArgs.timeAtRisks[0].riskWindowEnd"
    9999  # From Analysis Specifications: "createStudyPopArgs.timeAtRisks[1].riskWindowEnd"
  ),
  endAnchor = c(
    "cohort end",   # From Analysis Specifications: "createStudyPopArgs.timeAtRisks[0].endAnchor"
    "cohort start"  # From Analysis Specifications: "createStudyPopArgs.timeAtRisks[1].endAnchor"
  )
)

# Propensity Score settings - match on PS
# Populated from Analysis Specifications "propensityScoreAdjustment.psSettings.matchOnPsArgs".
matchOnPsArgsList <- tibble(
  label = c("Match_MaxRatio1_Cal0.2_StdLogit"), # Descriptive label for this PS matching strategy
  maxRatio  = c(1), # From Analysis Specifications: "propensityScoreAdjustment.psSettings.matchOnPsArgs.maxRatio"
  caliper = c(0.2), # From Analysis Specifications: "propensityScoreAdjustment.psSettings.matchOnPsArgs.caliper"
  caliperScale  = c("standardized logit") # From Analysis Specifications: "propensityScoreAdjustment.psSettings.matchOnPsArgs.caliperScale"
)

# Propensity Score settings - stratify by PS
# From Analysis Specifications: "propensityScoreAdjustment.psSettings.stratifyByPsArgs" is null.
# So, this data frame remains empty, and no stratification analyses will be generated.
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = numeric(0),
  baseSelection = character(0)
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# If a data frame for "match on PS" exists and has rows, convert each row to a config
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "match", # Identify the PS adjustment method
      label  = matchOnPsArgsList$label[i], # Human-readable label
      params = list( # Parameter bundle passed to createMatchOnPsArgs later
        maxRatio     = matchOnPsArgsList$maxRatio[i],
        caliper      = matchOnPsArgsList$caliper[i],
        caliperScale = matchOnPsArgsList$caliperScale[i]
      )
    )
  }
}

# If a data frame for "stratify by PS" exists and has rows, convert each row to a config
# (This block will not add anything as stratifyByPsArgsList is empty based on analysis specs)
if (exists("stratifyByPsArgsList") && nrow(stratifyByPsArgsList) > 0) {
  for (i in seq_len(nrow(stratifyByPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "stratify", # Identify the PS adjustment method
      label  = stratifyByPsArgsList$label[i], # Human-readable label
      params = list( # Parameter bundle passed to createStratifyByPsArgs later
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection  = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}

# Iterate through all analysis setting combinations to create CmAnalysis objects
cmAnalysisList <- list()
analysisId <- 1 # Initialize analysis ID

# Loop through each defined study period (in this case, only one entry with no restrictions).
for (s in seq_len(nrow(studyPeriods))) {
  # Resolve study start and end dates, converting empty strings to NULL for no restriction.
  studyStartDate <- if (studyPeriods$studyStartDate[s] == "") NULL else studyPeriods$studyStartDate[s]
  studyEndDate <- if (studyPeriods$studyEndDate[s] == "") NULL else studyPeriods$studyEndDate[s]

  # Loop through each defined Time-at-Risk (TAR) configuration.
  for (t in seq_len(nrow(timeAtRisks))) {

    # Loop through each propensity score adjustment method (matching or stratification).
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Determine PS adjustment arguments based on the method type.
      if (psCfg$method == "match") {
        # Create matching on PS arguments using parameters from analysis specifications.
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default from template
          stratificationColumns = c() # Default from template
        )
        stratifyByPsArgs <- NULL # No stratification if matching is used.
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL # No matching if stratification is used.
        # Create stratify by PS arguments (this path is not taken based on analysis specs).
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default from template
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings: Uses default settings as 'covariateSelection'
      # in analysis specifications is empty.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default from template
      )

      # Outcome list for the analysis, combining the primary study outcome
      # and all negative control outcomes.
      outcomeList <- append(
        # Main study outcome: re-numbered ID 3, "outcome1".
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not specified in analysis specifications, default to NA.
            priorOutcomeLookback = 99999 # Default from template; distinct from createStudyPopArgs.priorOutcomeLookback.
          )
        }),
        # Negative control outcomes: labeled as not of interest, with true effect size 1.
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # True effect size for negative controls.
          )
        })
      )
      
      # Target-Comparator-Outcomes list for the CohortMethod module.
      # Defines which target/comparator pairs will be evaluated against which outcomes.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # 'excludedCovariateConceptIds' is an empty vector because
          # 'covariateSelection.conceptsToExclude' in analysis specifications is empty,
          # and specific target/comparator drug concepts were not provided for exclusion from covariates.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Arguments for retrieving cohort method data from the database.
      # Populated from Analysis Specifications "getDbCohortMethodDataArgs" and "createStudyPopArgs".
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        # From Analysis Specifications: "createStudyPopArgs.restrictToCommonPeriod" (false).
        restrictToCommonPeriod = FALSE, 
        studyStartDate = studyStartDate, # Derived from studyPeriods, NULL if no restriction.
        studyEndDate = studyEndDate,     # Derived from studyPeriods, NULL if no restriction.
        # From Analysis Specifications: "getDbCohortMethodDataArgs.maxCohortSize" (0).
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # Populated from Analysis Specifications "propensityScoreAdjustment.createPsArgs".
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications: "maxCohortSizeForFitting".
        errorOnHighCorrelation = TRUE, # From Analysis Specifications: "errorOnHighCorrelation".
        stopOnError = FALSE, # Default from template; setting to FALSE allows Strategus to complete all CM operations even if a PS model cannot be fitted.
        estimator = "att", # Default from template; Average Treatment Effect on the Treated.
        prior = Cyclops::createPrior( # Populated from Analysis Specifications: "propensityScoreAdjustment.createPsArgs.prior".
          priorType = "laplace", # From Analysis Specifications: "priorType".
          exclude = c(0), # Default from template; excludes intercept from regularization.
          useCrossValidation = TRUE # From Analysis Specifications: "useCrossValidation".
        ),
        control = Cyclops::createControl( # Populated from Analysis Specifications: "propensityScoreAdjustment.createPsArgs.control".
          tolerance = 2e-7, # From Analysis Specifications: "tolerance".
          cvType = "auto", # From Analysis Specifications: "cvType".
          fold = 10, # From Analysis Specifications: "fold".
          cvRepetitions = 10, # From Analysis Specifications: "cvRepetitions".
          noiseLevel = "silent", # From Analysis Specifications: "noiseLevel".
          resetCoefficients = TRUE, # From Analysis Specifications: "resetCoefficients".
          startingVariance = 0.01, # From Analysis Specifications: "startingVariance".
          seed = 1 # Default from template; for reproducibility.
        )
      )

      # Arguments for computing shared covariate balance.
      # Default from template, not explicitly specified in analysis specifications.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      
      # Arguments for computing covariate balance.
      # Default from template, not explicitly specified in analysis specifications.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting the outcome model.
      # Populated from Analysis Specifications "fitOutcomeModelArgs".
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From Analysis Specifications: "modelType".
        stratified = FALSE, # From Analysis Specifications: "stratified".
        useCovariates = FALSE, # From Analysis Specifications: "useCovariates".
        inversePtWeighting = FALSE, # From Analysis Specifications: "inversePtWeighting".
        prior = Cyclops::createPrior( # Populated from Analysis Specifications: "fitOutcomeModelArgs.prior".
          priorType = "laplace", # From Analysis Specifications: "priorType".
          useCrossValidation = TRUE # From Analysis Specifications: "useCrossValidation".
        ),
        control = Cyclops::createControl( # Populated from Analysis Specifications: "fitOutcomeModelArgs.control".
          tolerance = 2e-7, # From Analysis Specifications: "tolerance".
          cvType = "auto", # From Analysis Specifications: "cvType".
          fold = 10, # From Analysis Specifications: "fold".
          cvRepetitions = 10, # From Analysis Specifications: "cvRepetitions".
          noiseLevel = "quiet", # From Analysis Specifications: "noiseLevel".
          resetCoefficients = TRUE, # From Analysis Specifications: "resetCoefficients".
          startingVariance = 0.01, # From Analysis Specifications: "startingVariance".
          seed = 1 # Default from template; for reproducibility.
        )
      )

      # Arguments for creating the study population.
      # Populated from Analysis Specifications "createStudyPopArgs".
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications: "restrictToCommonPeriod".
        firstExposureOnly = FALSE, # From Analysis Specifications: "firstExposureOnly".
        washoutPeriod = 0, # From Analysis Specifications: "washoutPeriod".
        removeDuplicateSubjects = "keep all", # From Analysis Specifications: "removeDuplicateSubjects".
        censorAtNewRiskWindow = FALSE, # From Analysis Specifications: "censorAtNewRiskWindow".
        removeSubjectsWithPriorOutcome = TRUE, # From Analysis Specifications: "removeSubjectsWithPriorOutcome".
        priorOutcomeLookback = 365, # From Analysis Specifications: "priorOutcomeLookBack".
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From current TAR loop iteration.
        startAnchor = timeAtRisks$startAnchor[t], # From current TAR loop iteration.
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From current TAR loop iteration.
        endAnchor = timeAtRisks$endAnchor[t], # From current TAR loop iteration.
        minDaysAtRisk = 1, # From Analysis Specifications: "timeAtRisks[].minDaysAtRisk".
        maxDaysAtRisk = 99999 # Default from template.
      )

      # Append the combined settings to the CohortMethod analysis list.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s", # Detailed description for this specific analysis run.
          if (is.null(studyStartDate)) "NoStartDate" else studyStartDate, # Handle NULL study dates for description.
          if (is.null(studyEndDate)) "NoEndDate" else studyEndDate,     # Handle NULL study dates for description.
          timeAtRisks$label[t], # Label of the current TAR.
          psCfg$label # Label of the current PS adjustment method.
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
      analysisId <- analysisId + 1 # Increment analysis ID for the next configuration.
    }
  }
}

cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Default from template.
  refitPsForEveryOutcome = FALSE, # Default from template.
  refitPsForEveryStudyPopulation = FALSE, # Default from template.
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default thresholds for diagnostics.
)

# Create the analysis specifications ------------------------------------------
# Combine all module specifications and shared resources into a single Strategus analysis specification object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the complete analysis specifications to a JSON file.
# The filename uses the "name" from Analysis Specifications: "tramadolcodein".
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "tramadolcodein", "tramadolcodeinAnalysisSpecification.json")
)