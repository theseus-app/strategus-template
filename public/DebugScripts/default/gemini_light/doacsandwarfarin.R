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
library(ROhdsiWebApi) # Required for fetching cohort definitions and concept sets
library(CohortMethod) # Required for CohortMethod settings
library(FeatureExtraction) # Required for covariate settings
library(Cyclops) # Required for Cyclops prior and control settings

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Fetch the target, comparator, and outcome cohort definitions using their IDs
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1 (from Analysis Specifications)
    1794132, # Comparator: comparator1 (from Analysis Specifications)
    1794131  # Outcome: outcome1 (from Analysis Specifications)
  ),
  generateStats = TRUE # Generate statistics for these cohorts
)

# Re-number cohorts to a simpler internal ID scheme for the study
# Target cohort ID 1794126 becomes 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# Comparator cohort ID 1794132 becomes 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# Outcome cohort ID 1794131 becomes 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Fetch the negative control concept set definition and resolve it to concepts
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Negative control concept set ID from Analysis Specifications
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet( # Resolve the concept set to individual concepts
    baseUrl = baseUrl
  ) %>%
  ROhdsiWebApi::getConcepts( # Get detailed concept information
    baseUrl = baseUrl
  ) %>%
  rename(outcomeConceptId = "conceptId", # Rename conceptId to outcomeConceptId
         cohortName = "conceptName") %>% # Rename conceptName to cohortName
  mutate(cohortId = row_number() + 100) %>% # Assign unique cohort IDs for negative controls, starting from 101
  select(cohortId, cohortName, outcomeConceptId) # Select relevant columns

# Check for duplicate cohort IDs between study cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Filter for the main outcome cohort (ID 3)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Outcome1 with re-numbered ID 3
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window (not specified in JSON)

# Target and Comparator for the CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered ID for target1
  targetCohortName = "target1", # Name from Analysis Specifications
  comparatorCohortId = 2, # Re-numbered ID for comparator1
  comparatorCohortName = "comparator1" # Name from Analysis Specifications
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study. The Analysis Specifications has an empty list for conceptsToExclude.
excludedCovariateConcepts <- data.frame(
  conceptId = c(), # No specific concept IDs to exclude from Analysis Specifications
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Create shared resource specifications for study cohorts
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Create shared resource specifications for negative control outcome cohorts
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default from template
  detectOnDescendants = TRUE # Default from template
)
# Create module specifications for CohortGenerator
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Generate statistics for the cohorts
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# Create module specifications for CohortDiagnostics
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # Run diagnostics on all defined study cohorts
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

# If you are not restricting your study to a specific time window, 
# please make these strings empty
# Study periods from getDbCohortMethodDataArgs.studyPeriods in Analysis Specifications
studyPeriods <- tibble(
  studyStartDate = c("20101019"), # YYYYMMDD format
  studyEndDate   = c("20181231")  # YYYYMMDD format
)

# Time-at-risks (TARs) for the outcomes of interest in your study
# Time-at-risks from createStudyPopArgs.timeAtRisks in Analysis Specifications
timeAtRisks <- tibble(
  label = c("TAR1", "TAR2", "TAR3"), # Descriptive labels for each TAR
  riskWindowStart  = c(1, 1, 1),
  startAnchor = c("cohort start", "cohort start", "cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd  = c(5, 0, 99999),
  endAnchor = c("cohort end", "cohort end", "cohort start") # "cohort start" | "cohort end"
) 

# Propensity Score settings - match on PS
# Propensity score matching settings from propensityScoreAdjustment.psSettings in Analysis Specifications
matchOnPsArgsList <- tibble(
  label = c("PS_Match_Ratio1", "PS_Match_Ratio100"), # Descriptive labels for each matching strategy
  maxRatio  = c(1, 100),
  caliper = c(0.2, 0.2),
  caliperScale  = c("standardized logit", "standardized logit") # "propensity score" | "standardized" | "standardized logit"
) 

# Propensity Score settings - stratify by PS
# No stratification settings provided in Analysis Specifications, so this remains empty
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
# (This section will not be executed as stratifyByPsArgsList is empty based on Analysis Specifications)
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
      
      # Determine PS adjustment method based on configuration
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper, # Fixed missing comma from template example
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default from template
          stratificationColumns = c() # Default from template
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default from template
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Create default covariate settings
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default from template
      )

      # Combine study outcomes and negative control outcomes
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999 # Default from template
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )
      
      # Create target-comparator-outcomes list
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        # excludedCovariateConceptIds are the cohort IDs themselves (if they represent drug concepts)
        # plus any other concepts explicitly excluded (none in this case based on Analysis Specifications)
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude the target and comparator cohort IDs themselves from covariates,
          # assuming they represent drug concepts to be excluded from background features.
          # excludedCovariateConcepts is empty based on Analysis Specifications.
          excludedCovariateConceptIds = c(
            cmTcList$targetCohortId[i], # Retained from original script's logic
            cmTcList$comparatorCohortId[i], # Retained from original script's logic
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # Settings for fetching cohort method data
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications (original script had FALSE, template TRUE)
        studyStartDate = studyStartDate, # Dynamic from studyPeriods loop
        studyEndDate = studyEndDate, # Dynamic from studyPeriods loop
        maxCohortSize = 0, # From Analysis Specifications
        covariateSettings = covariateSettings
        # Parameters firstExposureOnly, washoutPeriod, removeDuplicateSubjects moved to createStudyPopArgs
      )

      # Settings for creating propensity scores
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications
        errorOnHighCorrelation = TRUE, # From Analysis Specifications
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail (from template)
        estimator = "att", # Default from template
        prior = Cyclops::createPrior( # Prior settings from propensityScoreAdjustment.createPsArgs.prior
          priorType = "laplace",
          exclude = c(0), # Default from template
          useCrossValidation = TRUE # From Analysis Specifications
        ),
        control = Cyclops::createControl( # Control settings from propensityScoreAdjustment.createPsArgs.control
          noiseLevel = "silent", # From Analysis Specifications
          cvType = "auto", # From Analysis Specifications
          seed = 1, # Default from template
          resetCoefficients = TRUE, # From Analysis Specifications
          tolerance = 2e-07, # From Analysis Specifications
          cvRepetitions = 10, # From Analysis Specifications (template has 1, JSON has 10)
          startingVariance = 0.01 # From Analysis Specifications
        )
      )

      # Settings for computing shared covariate balance (template defaults)
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      # Settings for computing covariate balance (template defaults)
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Settings for fitting the outcome model
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From Analysis Specifications
        stratified = TRUE, # From Analysis Specifications
        useCovariates = FALSE, # From Analysis Specifications
        inversePtWeighting = FALSE, # From Analysis Specifications
        prior = Cyclops::createPrior( # Prior settings from fitOutcomeModelArgs.prior
          priorType = "laplace",
          useCrossValidation = TRUE # From Analysis Specifications
        ),
        control = Cyclops::createControl( # Control settings from fitOutcomeModelArgs.control
          cvType = "auto", # From Analysis Specifications
          seed = 1, # Default from template
          resetCoefficients = TRUE, # From Analysis Specifications
          startingVariance = 0.01, # From Analysis Specifications
          tolerance = 2e-07, # From Analysis Specifications
          cvRepetitions = 10, # From Analysis Specifications (template has 1, JSON has 10)
          noiseLevel = "quiet" # From Analysis Specifications
        )
      )
      
      # Settings for creating the study population
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications
        firstExposureOnly = FALSE, # From Analysis Specifications (consistent with original script's createStudyPopArgs)
        washoutPeriod = 0, # From Analysis Specifications (consistent with original script's createStudyPopArgs)
        removeDuplicateSubjects = "keep all", # From Analysis Specifications (consistent with original script's createStudyPopArgs)
        censorAtNewRiskWindow = FALSE, # From Analysis Specifications
        removeSubjectsWithPriorOutcome = FALSE, # From Analysis Specifications
        priorOutcomeLookback = 99999, # From Analysis Specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t], # Dynamic from timeAtRisks loop
        startAnchor = timeAtRisks$startAnchor[t], # Dynamic from timeAtRisks loop
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # Dynamic from timeAtRisks loop
        endAnchor = timeAtRisks$endAnchor[t], # Dynamic from timeAtRisks loop
        minDaysAtRisk = 1, # Fixed: Original script attempted to use timeAtRisks$minDaysAtRisk which did not exist.
        maxDaysAtRisk = 99999 # Default from template
      )

      # Append the settings to Analysis List for each combination of study period, TAR, and PS method
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
# Create module specifications for CohortMethod
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Default from template
  refitPsForEveryOutcome = FALSE, # Default from template
  refitPsForEveryStudyPopulation = FALSE, # Default from template
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default from template
)

# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> # Add shared study cohort definitions
  Strategus::addSharedResources(negativeControlsShared) |> # Add shared negative control definitions
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |> # Add CohortGenerator module
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |> # Add CohortDiagnostics module
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications) # Add CohortMethod module

# Define the output path
outputPath <- file.path("inst", "doacsandwarfarin")
outputFile <- file.path(outputPath, "doacsandwarfarinAnalysisSpecification.json")

# Ensure the output directory exists before saving the file
if (!dir.exists(outputPath)) {
  dir.create(outputPath, recursive = TRUE)
}

# Save the analysis specifications to a JSON file
# The study name "doacsandwarfarin" is taken from the Analysis Specifications
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  outputFile
)