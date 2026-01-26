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
# Base URL for the WebAPI instance where cohort definitions are stored.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Extract cohort IDs from the analysis specifications.
# Target: 1794126, Comparator: 1794132, Outcome: 1794131
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal use in Strategus modules.
# This maps the original WebAPI cohort IDs to simpler, sequential IDs (1, 2, 3).
# Target cohort (1794126) is mapped to ID 1.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# Comparator cohort (1794132) is mapped to ID 2.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# Outcome cohort (1794131) is mapped to ID 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve the concept set definition for negative controls from WebAPI.
# The conceptSetId is 1888110 as specified in analysis specifications.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Negative control concept set ID from analysis specifications
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
  # Assign unique cohort IDs for negative controls, starting from 101 to avoid
  # clashes with target/comparator/outcome cohorts (1, 2, 3).
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs between study cohorts and negative controls.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes:
# Filter for the outcome cohort (re-numbered ID 3) and prepare for CM analysis.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not specified in analysis specs

# Target and Comparator for the CohortMethod analysis
# Use the re-numbered IDs for target (1) and comparator (2).
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1", # Name from analysis specifications
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1" # Name from analysis specifications
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study. The analysis specifications provide an empty list for conceptsToExclude,
# so this data frame will be empty.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# The analysis specifications provide an empty list for conceptsToInclude,
# so this section remains commented out.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default, not specified in analysis specs
  detectOnDescendants = TRUE # Default, not specified in analysis specs
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # As specified in the template
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE, # Default, not specified in analysis specs
  runIncludedSourceConcepts = TRUE, # Default, not specified in analysis specs
  runOrphanConcepts = TRUE, # Default, not specified in analysis specs
  runTimeSeries = FALSE, # Default, not specified in analysis specs
  runVisitContext = TRUE, # Default, not specified in analysis specs
  runBreakdownIndexEvents = TRUE, # Default, not specified in analysis specs
  runIncidenceRate = TRUE, # Default, not specified in analysis specs
  runCohortRelationship = TRUE, # Default, not specified in analysis specs
  runTemporalCohortCharacterization = TRUE, # Default, not specified in analysis specs
  minCharacterizationMean = 0.01 # Default, not specified in analysis specs
)

# CohortMethodModule -----------------------------------------------------------

# If you are not restricting your study to a specific time window,
# please make these strings empty.
# The analysis specifications provide empty strings for studyStartDate and studyEndDate,
# indicating no specific study period restriction. We will represent this as a single
# row with NULL values, which CohortMethod::createGetDbCohortMethodDataArgs handles
# by not applying a study period filter.
studyPeriods <- tibble(
  studyStartDate = c(NA_character_), # YYYYMMDD, NA_character_ for no restriction
  studyEndDate   = c(NA_character_)  # YYYYMMDD, NA_character_ for no restriction
)

# Time-at-risks (TARs) for the outcomes of interest in your study
# Populated from createStudyPopArgs.timeAtRisks in analysis specifications.
timeAtRisks <- tibble(
  label = c("TAR1", "TAR2"), # Custom labels for description
  riskWindowStart  = c(1, 1),
  startAnchor = c("cohort start", "cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd  = c(0, 99999),
  endAnchor = c("cohort end", "cohort start") # "cohort start" | "cohort end"
)

# Propensity Score settings - match on PS
# Populated from propensityScoreAdjustment.psSettings.matchOnPsArgs in analysis specifications.
matchOnPsArgsList <- tibble(
  label = c("Match1", "Match2"), # Custom labels for description
  maxRatio  = c(10, 1),
  caliper = c(0.2, 0.2),
  caliperScale  = c("standardized logit", "standardized logit") # "propensity score" | "standardized" | "standardized logit"
)

# Propensity Score settings - stratify by PS
# The analysis specifications have stratifyByPsArgs: null, so this remains empty.
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0) # "all" | "target" | "comparator"
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
  # Use studyStartDate and studyEndDate from the tibble. NA_character_ will be passed as NULL.
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      if (psCfg$method == "match") {
        # Create MatchOnPsArgs based on the current PS configuration.
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default, not specified in analysis specs
          stratificationColumns = c() # Default, not specified in analysis specs
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        # Create StratifyByPsArgs based on the current PS configuration.
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default, not specified in analysis specs
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings: using default settings as conceptsToInclude/Exclude are empty in spec.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default, not specified in analysis specs
      )

      # Combine study outcomes and negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not applicable for observed outcomes
            priorOutcomeLookback = 99999 # From createStudyPopArgs.priorOutcomeLookBack in analysis specs
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

      # Create target-comparator-outcome combinations.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude specific covariate concepts.
          # The analysis specifications' conceptsToExclude is empty, so only
          # concepts from `excludedCovariateConcepts` (which is empty) are included here.
          # Note: The template's `cmTcList$targetConceptId[i]` and `cmTcList$comparatorConceptId[i]`
          # refer to cohort IDs, not concept IDs. These are removed as they are not concept IDs
          # to be excluded from covariates.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # GetDbCohortMethodDataArgs settings from analysis specifications.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # From analysis specifications
        studyStartDate = studyStartDate, # From current loop iteration (NULL if no restriction)
        studyEndDate = studyEndDate, # From current loop iteration (NULL if no restriction)
        maxCohortSize = 0, # From analysis specifications
        covariateSettings = covariateSettings,
        # Other parameters from analysis specifications not directly mapped to createGetDbCohortMethodDataArgs:
        # firstExposureOnly = FALSE (handled by createStudyPopArgs)
        # washoutPeriod = 0 (handled by createStudyPopArgs)
        # removeDuplicateSubjects = "keep all" (handled by createStudyPopArgs)
      )

      # CreatePsArgs settings from analysis specifications.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From analysis specifications
        errorOnHighCorrelation = TRUE, # From analysis specifications
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att", # Default, not specified in analysis specs
        prior = Cyclops::createPrior( # Prior settings from analysis specifications
          priorType = "laplace", # From analysis specifications
          exclude = c(0), # Default, not specified in analysis specs
          useCrossValidation = TRUE # From analysis specifications
        ),
        control = Cyclops::createControl( # Control settings from analysis specifications
          noiseLevel = "silent", # From analysis specifications
          cvType = "auto", # From analysis specifications
          seed = 1, # Default, not specified in analysis specs
          resetCoefficients = TRUE, # From analysis specifications
          tolerance = 2e-07, # From analysis specifications
          cvRepetitions = 10, # From analysis specifications
          startingVariance = 0.01, # From analysis specifications
          fold = 10 # From analysis specifications
        )
      )

      # ComputeSharedCovariateBalanceArgs and ComputeCovariateBalanceArgs
      # These are default settings from the template, not explicitly in analysis specs.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default, not specified in analysis specs
        covariateFilter = NULL # Default, not specified in analysis specs
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default, not specified in analysis specs
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default, not specified in analysis specs
      )

      # FitOutcomeModelArgs settings from analysis specifications.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From analysis specifications
        stratified = TRUE, # From analysis specifications
        useCovariates = FALSE, # From analysis specifications
        inversePtWeighting = FALSE, # From analysis specifications
        prior = Cyclops::createPrior( # Prior settings from analysis specifications
          priorType = "laplace", # From analysis specifications
          useCrossValidation = TRUE # From analysis specifications
        ),
        control = Cyclops::createControl( # Control settings from analysis specifications
          cvType = "auto", # From analysis specifications
          seed = 1, # Default, not specified in analysis specs
          resetCoefficients = TRUE, # From analysis specifications
          startingVariance = 0.01, # From analysis specifications
          tolerance = 2e-07, # From analysis specifications
          cvRepetitions = 10, # From analysis specifications
          noiseLevel = "quiet", # From analysis specifications
          fold = 10 # From analysis specifications
        )
      )

      # CreateStudyPopulationArgs settings from analysis specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From analysis specifications
        firstExposureOnly = FALSE, # From analysis specifications
        washoutPeriod = 0, # From analysis specifications
        removeDuplicateSubjects = "keep all", # From analysis specifications
        censorAtNewRiskWindow = FALSE, # From analysis specifications
        removeSubjectsWithPriorOutcome = TRUE, # From analysis specifications
        priorOutcomeLookback = 99999, # From analysis specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From current TAR loop iteration
        startAnchor = timeAtRisks$startAnchor[t], # From current TAR loop iteration
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From current TAR loop iteration
        endAnchor = timeAtRisks$endAnchor[t], # From current TAR loop iteration
        minDaysAtRisk = 1, # From analysis specifications (common for both TARs)
        maxDaysAtRisk = 99999 # Default, not specified in analysis specs
      )


      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          # If studyStartDate/EndDate are NA, replace with "No Restriction" for description
          ifelse(is.na(studyStartDate), "No Restriction", studyStartDate),
          ifelse(is.na(studyEndDate), "No Restriction", studyEndDate),
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
  analysesToExclude = NULL, # Default, not specified in analysis specs
  refitPsForEveryOutcome = FALSE, # Default, not specified in analysis specs
  refitPsForEveryStudyPopulation = FALSE, # Default, not specified in analysis specs
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default, not specified in analysis specs
)

# Create the analysis specifications ------------------------------------------
# Combine all shared resources and module specifications into a single analysis specification object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the complete analysis specifications to a JSON file.
# The file path is constructed using "inst", "studyName" (from analysis specs),
# and "studyNameAnalysisSpecification.json".
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "uveitissafety", "uveitissafetyAnalysisSpecification.json")
)