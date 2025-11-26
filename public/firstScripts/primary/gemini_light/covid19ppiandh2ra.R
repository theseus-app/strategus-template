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
# Base URL for the WebAPI instance where cohort definitions are stored.
# This is a placeholder and should be updated to your specific WebAPI instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions from the analysis specifications
# These cohorts will be used as Target, Comparator, and Outcome in the study.
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
# This makes it easier to refer to them consistently within the Strategus modules.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes from the analysis specifications
# These are used to estimate the residual bias in the study design.
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
  # Assign cohort IDs starting from 101 to avoid collision with T/C/O cohorts
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs to prevent errors in Strategus execution.
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ------
# Outcomes: Filter for the re-numbered outcome cohort (ID 3)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window for outcomes

# Target and Comparator for the CohortMethod analysis 
# Using the re-numbered target (ID 1) and comparator (ID 2) cohorts
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1", # Name from analysis specifications
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1" # Name from analysis specifications
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study. The analysis specifications indicate no specific concepts to exclude
# beyond the target/comparator treatments themselves.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0), # Empty data frame as per analysis specifications
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# The analysis specifications indicate no specific concepts to include.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# This module is responsible for generating cohorts in the CDM.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Shared resource for the cohort definitions.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Shared resource for the negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",      # Detect the first occurrence of the outcome
  detectOnDescendants = TRUE     # Detect outcomes on descendants of the concept IDs
)
# Module specifications for cohort generation, including generating cohort statistics.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# This module runs diagnostics on the generated cohorts.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # Run diagnostics for all defined cohorts
  runInclusionStatistics = TRUE,
  runIncludedSourceConcepts = TRUE,
  runOrphanConcepts = TRUE,
  runTimeSeries = FALSE,                 # Not specified in analysis spec, keeping default false
  runVisitContext = TRUE,
  runBreakdownIndexEvents = TRUE,
  runIncidenceRate = TRUE,
  runCohortRelationship = TRUE,
  runTemporalCohortCharacterization = TRUE,
  minCharacterizationMean = 0.01
)

# CohortMethodModule -----------------------------------------------------------
# This module performs the core comparative effectiveness analysis.

# Study periods from the analysis specifications
# If not restricting to a specific time window, these strings should be empty.
studyPeriods <- tibble(
  studyStartDate = c("20200101"), # YYYYMMDD from analysis specifications
  studyEndDate   = c("20200515")  # YYYYMMDD from analysis specifications
)

# Time-at-risks (TARs) for the outcomes of interest in your study
# Defined in createStudyPopArgs.timeAtRisks in analysis specifications
timeAtRisks <- tibble(
  label = c("COVID-19 Risk Window"), # A descriptive label for this time-at-risk period
  riskWindowStart  = c(1),            # From analysis specifications
  startAnchor = c("cohort start"),   # From analysis specifications
  riskWindowEnd  = c(99999),          # From analysis specifications
  endAnchor = c("cohort start"),     # From analysis specifications
  minDaysAtRisk = c(1)               # From analysis specifications
) 

# Propensity Score settings - match on PS
# Analysis specifications indicate null for matchOnPsArgs, so this is an empty tibble.
matchOnPsArgsList <- tibble(
  label = character(0),
  maxRatio  = integer(0),
  caliper = numeric(0),
  caliperScale  = character(0)
) 

# Propensity Score settings - stratify by PS
# From analysis specifications: numberOfStrata = 5, baseSelection = "all"
stratifyByPsArgsList <- tibble(
  label = c("Stratify_5_All"), # A descriptive label for this stratification method
  numberOfStrata  = c(5),
  baseSelection = c("all")
) 

# Build a single PS configuration list (each entry has: method, label, params)
# This loop processes both matching and stratification configurations if present.
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

# Iterate through all analysis setting combinations to create a list of CM analyses
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Determine PS adjustment arguments based on the method
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Default covariate settings for FeatureExtraction
      # The analysis specifications do not include specific covariate concepts to include or exclude
      # at this level, so we use default settings.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Include descendants of excluded concepts
      )

      # Combine outcome cohorts and negative control outcomes
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE, # This is the primary outcome
            trueEffectSize = NA,      # No true effect size for observed outcomes
            priorOutcomeLookback = 99999 # From createStudyPopArgs.priorOutcomeLookBack
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE, # These are negative controls
            trueEffectSize = 1         # True effect size of 1 for negative controls (null hypothesis)
          )
        })
      )

      # Define target-comparator-outcome combinations for the analysis
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude specific covariate concepts (e.g., the target/comparator treatments themselves)
          # 'excludedCovariateConcepts$conceptId' is an empty vector based on analysis specifications.
          # Removed cmTcList$targetConceptId and cmTcList$comparatorConceptId as they are not defined in cmTcList.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId 
        )
      }

      # Arguments for fetching cohort method data from the database
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Keep TRUE as common practice (not explicitly false in spec)
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,             # From analysis specifications: 0 means no restriction on cohort size
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From analysis specifications
        errorOnHighCorrelation = TRUE,    # From analysis specifications
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att",   # Average Treatment Effect on the Treated (default)
        prior = Cyclops::createPrior( # Prior settings from analysis specifications
          priorType = "laplace",      # From analysis specifications
          exclude = c(0),             # Exclude intercept from regularization
          useCrossValidation = TRUE   # From analysis specifications
        ),
        control = Cyclops::createControl( # Control settings for Cyclops from analysis specifications
          noiseLevel = "silent",      # From analysis specifications
          cvType = "auto",            # From analysis specifications
          seed = 1,                   # Fixed seed for reproducibility
          resetCoefficients = TRUE,   # From analysis specifications
          tolerance = 2e-07,          # From analysis specifications
          cvRepetitions = 10,         # From analysis specifications (updated from template 1)
          startingVariance = 0.01     # From analysis specifications
        )
      )

      # Arguments for computing covariate balance
      # Using default maxCohortSize and no specific covariate filter
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      # Arguments for computing covariate balance for table 1
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting the outcome model
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",            # From analysis specifications
        stratified = TRUE,            # From analysis specifications
        useCovariates = FALSE,        # From analysis specifications
        inversePtWeighting = FALSE,   # From analysis specifications
        prior = Cyclops::createPrior( # Prior settings from analysis specifications
          priorType = "laplace",      # From analysis specifications
          useCrossValidation = TRUE   # From analysis specifications
        ),
        control = Cyclops::createControl( # Control settings for Cyclops from analysis specifications
          cvType = "auto",            # From analysis specifications
          seed = 1,                   # Fixed seed for reproducibility
          resetCoefficients = TRUE,   # From analysis specifications
          startingVariance = 0.01,    # From analysis specifications
          tolerance = 2e-07,          # From analysis specifications
          cvRepetitions = 10,         # From analysis specifications (updated from template 1)
          noiseLevel = "quiet"        # From analysis specifications
        )
      )

      # Arguments for creating the study population
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,      # From analysis specifications
        firstExposureOnly = TRUE,            # From analysis specifications
        washoutPeriod = 365,                 # From analysis specifications
        removeDuplicateSubjects = "keep all",# From analysis specifications (updated from template "keep first")
        censorAtNewRiskWindow = FALSE,       # From analysis specifications
        removeSubjectsWithPriorOutcome = TRUE,# From analysis specifications
        priorOutcomeLookback = 99999,        # From analysis specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From timeAtRisks tibble
        startAnchor = timeAtRisks$startAnchor[t],         # From timeAtRisks tibble
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],     # From timeAtRisks tibble
        endAnchor = timeAtRisks$endAnchor[t],             # From timeAtRisks tibble
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],     # From timeAtRisks tibble
        maxDaysAtRisk = 99999                # Not specified in analysis spec, using large default for unbounded
      )

      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Analysis: covid19ppiandh2ra; Study Period: %s-%s; TAR: %s; PS Adjustment: %s", # Descriptive label for the analysis
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

# CohortMethodModule specifications
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Using default diagnostic thresholds
)

# Create the analysis specifications ------------------------------------------
# Combine all shared resources and module specifications into a single analysis specification object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path is constructed based on the study name from analysis specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "covid19ppiandh2ra", "covid19ppiandh2raAnalysisSpecification.json")
)