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
library(ROhdsiWebApi) # Required for cohort definition retrieval
library(CohortGenerator) # Required for cohort generation functions
library(CohortMethod) # Required for CohortMethod analysis functions
library(FeatureExtraction) # Required for covariate settings
library(Cyclops) # Required for prior and control settings in PS and outcome models
library(ParallelLogger) # Required for saving JSON

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from Atlas using the provided IDs in Analysis Specifications.
# The baseUrl is a placeholder and should be updated by the user for their specific Atlas instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions from Analysis Specifications
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1 (from Analysis Specifications)
    1794132, # Comparator: comparator1 (from Analysis Specifications)
    1794131  # Outcome: outcome1 (from Analysis Specifications)
  ),
  generateStats = TRUE
)

# Re-number cohorts to internal study IDs for consistency and simplicity in the analysis
# Target (1794126) becomes 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# Comparator (1794132) becomes 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# Outcome (1794131) becomes 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3


# Negative control outcomes from Analysis Specifications
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
  # Assign unique cohort IDs for negative controls, starting after the re-numbered study cohorts (1, 2, 3)
  mutate(cohortId = row_number() + 100) %>% # target/comparator/outcome cohort ids start with 1, 2, 3... negativeControl -> 101, 102, 103...
  select(cohortId, cohortName, outcomeConceptId)


# Check for duplicate cohort IDs to prevent conflicts
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts for CohortMethod analysis ---------------

# Outcomes: Filter to the main outcome cohort (re-numbered ID 3)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Filtering for re-numbered outcome cohort 'outcome1' (original ID 1794131)
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default cleanWindow, not specified in Analysis Specifications

# Target and Comparator for the CohortMethod analysis (re-numbered IDs 1 and 2)
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered 'target1' (original ID 1794126)
  targetCohortName = "target1", # Name from Analysis Specifications
  comparatorCohortId = 2, # Re-numbered 'comparator1' (original ID 1794132)
  comparatorCohortName = "comparator1" # Name from Analysis Specifications
)

# For the CohortMethod, covariates to exclude.
# Analysis Specifications explicitly provides an empty list for conceptsToExclude.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# Analysis Specifications provides an empty list for conceptsToInclude, so this remains commented.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Create shared resource for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Create shared resource for negative control outcome cohorts
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default, not specified in Analysis Specifications
  detectOnDescendants = TRUE # Default, not specified in Analysis Specifications
)
# Create module specifications for CohortGenerator
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # From Analysis Specifications (implied by wanting stats for cohort generation)
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# CohortDiagnostics settings are not detailed in Analysis Specifications, using template defaults.
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

# Study periods from Analysis Specifications -> getDbCohortMethodDataArgs -> studyPeriods
studyPeriods <- tibble(
  studyStartDate = c(20171201), # YYYYMMDD from Analysis Specifications
  studyEndDate   = c(20231231)  # YYYYMMDD from Analysis Specifications
)

# Time-at-risks (TARs) for the outcomes of interest in your study
# From Analysis Specifications -> createStudyPopArgs -> timeAtRisks
timeAtRisks <- tibble(
  label = c("0-0_cohort_start-cohort_end"), # Custom label for this TAR
  riskWindowStart  = c(0), # From Analysis Specifications
  startAnchor = c("cohort start"), # From Analysis Specifications
  riskWindowEnd  = c(0), # From Analysis Specifications
  endAnchor = c("cohort end") # From Analysis Specifications
) 

# Propensity Score settings - match on PS
# From Analysis Specifications -> propensityScoreAdjustment -> psSettings (first entry)
matchOnPsArgsList <- tibble(
  label = c("match_1"), # Custom label for this PS setting
  maxRatio  = c(1), # From Analysis Specifications
  caliper = c(0.2), # From Analysis Specifications
  caliperScale  = c("standardized logit") # From Analysis Specifications
) 

# Propensity Score settings - stratify by PS
# From Analysis Specifications -> propensityScoreAdjustment -> psSettings (second entry)
stratifyByPsArgsList <- tibble(
  label = c("stratify_1"), # Custom label for this PS setting
  numberOfStrata  = c(5), # From Analysis Specifications
  baseSelection = c("all") # From Analysis Specifications
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
      
      # Create match or stratify arguments based on PS configuration
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper, # Fixed typo from template: removed extra ')'
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default, not specified in Analysis Specifications
          stratificationColumns = c() # Default, not specified in Analysis Specifications
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default, not specified in Analysis Specifications
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Default covariate settings (default from FeatureExtraction)
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default, not specified in Analysis Specifications
      )

      # List of outcomes including the main outcome and negative controls
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # For real outcomes, true effect size is unknown
            priorOutcomeLookback = 99999 # From Analysis Specifications -> createStudyPopArgs -> priorOutcomeLookBack
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE, # Mark as negative control
            trueEffectSize = 1 # Assuming true effect size of 1 for negative controls
          )
        })
      )

      # Create target-comparator-outcome list
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude concepts specified in Analysis Specifications -> covariateSelection -> conceptsToExclude
          # Since the spec lists an empty array, excludedCovariateConcepts$conceptId will be an empty vector
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId # Only use explicitly excluded concepts
        )
      }

      # getDbCohortMethodDataArgs from Analysis Specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Template default, not specified in Analysis Specifications
        studyStartDate = studyStartDate, # From Analysis Specifications
        studyEndDate = studyEndDate, # From Analysis Specifications
        maxCohortSize = 0, # From Analysis Specifications
        covariateSettings = covariateSettings
      )

      # createPsArgs from Analysis Specifications -> propensityScoreAdjustment -> createPsArgs
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications
        errorOnHighCorrelation = FALSE, # From Analysis Specifications
        stopOnError = FALSE, # Template default, allowing Strategus to complete even if PS model fitting fails (diagnostics will capture this)
        estimator = "att", # Template default, not specified in Analysis Specifications
        prior = Cyclops::createPrior( # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> prior
          priorType = "laplace", 
          exclude = c(0), # Template default for Laplace prior, not specified in Analysis Specifications
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl( # From Analysis Specifications -> propensityScoreAdjustment -> createPsArgs -> control
          tolerance = 2e-7,
          cvType = "auto", 
          fold = 10, # From Analysis Specifications
          cvRepetitions = 10, # From Analysis Specifications
          noiseLevel = "quiet", # From Analysis Specifications
          resetCoefficients = FALSE, # From Analysis Specifications
          startingVariance = 0.01, # From Analysis Specifications
          seed = 1 # Template default, not specified in Analysis Specifications
        )
      )

      # computeSharedCovariateBalanceArgs - using template defaults
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Template default
        covariateFilter = NULL # Template default
      )
      # computeCovariateBalanceArgs - using template defaults
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Template default
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Template default
      )

      # fitOutcomeModelArgs from Analysis Specifications
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From Analysis Specifications
        stratified = FALSE, # From Analysis Specifications
        useCovariates = FALSE, # From Analysis Specifications
        inversePtWeighting = FALSE, # From Analysis Specifications
        prior = Cyclops::createPrior( # From Analysis Specifications -> fitOutcomeModelArgs -> prior
          priorType = "laplace", 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl( # From Analysis Specifications -> fitOutcomeModelArgs -> control
          tolerance = 2e-7,
          cvType = "auto", 
          fold = 10, # From Analysis Specifications
          cvRepetitions = 10, # From Analysis Specifications
          noiseLevel = "quiet", # From Analysis Specifications
          resetCoefficients = TRUE, # From Analysis Specifications
          startingVariance = 0.01, # From Analysis Specifications
          seed = 1 # Template default, not specified in Analysis Specifications
        )
      )

      # createStudyPopArgs from Analysis Specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications
        firstExposureOnly = FALSE, # From Analysis Specifications
        washoutPeriod = 365, # From Analysis Specifications
        removeDuplicateSubjects = "keep all", # From Analysis Specifications
        censorAtNewRiskWindow = FALSE, # From Analysis Specifications
        removeSubjectsWithPriorOutcome = TRUE, # From Analysis Specifications
        priorOutcomeLookback = 99999, # From Analysis Specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From Analysis Specifications -> createStudyPopArgs -> timeAtRisks
        startAnchor = timeAtRisks$startAnchor[t], # From Analysis Specifications -> createStudyPopArgs -> timeAtRisks
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From Analysis Specifications -> createStudyPopArgs -> timeAtRisks
        endAnchor = timeAtRisks$endAnchor[t], # From Analysis Specifications -> createStudyPopArgs -> timeAtRisks
        minDaysAtRisk = 1, # From Analysis Specifications
        maxDaysAtRisk = 99999 # Template default, not specified in Analysis Specifications
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
  analysesToExclude = NULL, # Template default, not specified in Analysis Specifications
  refitPsForEveryOutcome = FALSE, # Template default, not specified in Analysis Specifications
  refitPsForEveryStudyPopulation = FALSE, # Template default, not specified in Analysis Specifications
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Not specified in Analysis Specifications, using defaults
)

# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file
# The file path is constructed using the 'name' from Analysis Specifications
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "semaglutideandnaion", "semaglutideandnaionAnalysisSpecification.json")
)