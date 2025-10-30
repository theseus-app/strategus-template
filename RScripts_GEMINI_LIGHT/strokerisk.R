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

# Define study name from analysis specifications
studyName <- "strokerisk"

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
# Placeholder for Atlas/WebAPI base URL. Users should replace this with their own.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions from analysis specifications
# The cohortIds are mapped to generic IDs (1, 2, 3...) for internal consistency.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1 (from Analysis Specifications)
    1794132, # Comparator: comparator1 (from Analysis Specifications)
    1794131 # Outcome: outcome1 (from Analysis Specifications)
  ),
  generateStats = TRUE
)

# Re-number cohorts to a standard internal representation
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Map target1 to ID 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Map comparator1 to ID 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Map outcome1 to ID 3

# Negative control outcomes from analysis specifications
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # From Analysis Specifications: negativeControlConceptSet.id
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
  mutate(cohortId = row_number() + 100) %>% # Target/comparator cohort IDs start with 1, 2, 3... negativeControl -> 101, 102, 103...
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs between study cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found. Please ensure all cohort IDs are unique. ***")
}

# Create data frames to hold the cohorts for analysis ---------------
# Outcomes: filter for the outcome cohort (ID 3 after re-mapping)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default value from template, not specified in analysis specifications

# Target and Comparator for the CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1, # Mapped ID for target1
  targetCohortName = "target1", # Name from Analysis Specifications
  comparatorCohortId = 2, # Mapped ID for comparator1
  comparatorCohortName = "comparator1" # Name from Analysis Specifications
)

# Covariate concepts to exclude (if any specified in Analysis Specifications)
# The analysis specifications provide null/empty entries for conceptsToExclude.
# So, this list will be empty.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)
# If concepts were specified:
# excludedCovariateConcepts <- data.frame(
#   conceptId = c(analysis_specs_excluded_concept_ids),
#   conceptName = c(analysis_specs_excluded_concept_names)
# )

# Optional: If you want to define specific covariates to include instead of including them all
# The analysis specifications provide null/empty entries for conceptsToInclude.
# So, this list will be empty.
includedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)
# If concepts were specified:
# includedCovariateConcepts <- data.frame(
#   conceptId = c(analysis_specs_included_concept_ids),
#   conceptName = c(analysis_specs_included_concept_names)
# )


# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default from template, not specified in analysis specifications
  detectOnDescendants = TRUE # Default from template, not specified in analysis specifications
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Default from template, not specified in analysis specifications
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # All re-mapped cohort IDs
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

# Study periods from analysis specifications (getDbCohortMethodDataArgs.studyPeriods)
studyPeriods <- tibble(
  studyStartDate = c("20010101", "20010101"), # YYYYMMDD
  studyEndDate   = c("20171231", "20151130") # YYYYMMDD
)

# Time-at-risks (TARs) for the outcomes of interest from analysis specifications (createStudyPopArgs.timeAtRisks)
timeAtRisks <- tibble(
  label = c("1 day after cohort start to cohort end"), # Descriptive label for this TAR
  riskWindowStart  = c(1), # From Analysis Specifications: createStudyPopArgs.timeAtRisks[0].riskWindowStart
  startAnchor = c("cohort start"), # From Analysis Specifications: createStudyPopArgs.timeAtRisks[0].startAnchor
  riskWindowEnd  = c(0), # From Analysis Specifications: createStudyPopArgs.timeAtRisks[0].riskWindowEnd
  endAnchor = c("cohort end"), # From Analysis Specifications: createStudyPopArgs.timeAtRisks[0].endAnchor
  minDaysAtRisk = c(1) # From Analysis Specifications: createStudyPopArgs.timeAtRisks[0].minDaysAtRisk
) 

# Propensity Score settings - match on PS from analysis specifications (propensityScoreAdjustment.psSettings)
# Filter out null entries and extract matchOnPsArgs
matchOnPsArgsList <- tibble(
  label = c("Match 1:1, Caliper 0.05 PS", "Match 10:1, Caliper 0.2 Std Logit"), # Descriptive labels
  maxRatio  = c(1, 10), # From Analysis Specifications: propensityScoreAdjustment.psSettings[1/2].matchOnPsArgs.maxRatio
  caliper = c(0.05, 0.2), # From Analysis Specifications: propensityScoreAdjustment.psSettings[1/2].matchOnPsArgs.caliper
  caliperScale  = c("propensity score", "standardized logit") # From Analysis Specifications: propensityScoreAdjustment.psSettings[1/2].matchOnPsArgs.caliperScale
) 

# Propensity Score settings - stratify by PS (all null in analysis specifications)
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0)
) 

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Convert "match on PS" settings into the config list
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

# Convert "stratify by PS" settings into the config list
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

# Iterate through all analysis setting combinations
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
      } else {
        # If no PS adjustment method, set both to NULL
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- NULL
      }


      # Covariate settings: using default as analysis specifications have empty lists for include/exclude.
      # If specific concepts were to be included/excluded in FeatureExtraction directly,
      # a more customized createCovariateSettings would be needed.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default from template
        # If there were concepts in `includedCovariateConcepts` or `excludedCovariateConcepts` (from spec),
        # these would be passed to `createDefaultCovariateSettings` or a custom `createCovariateSettings` here.
        # Given spec has empty lists, default settings are used.
      )

      # Combine outcome cohorts and negative control cohorts
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Default from template
            priorOutcomeLookback = 99999 # From Analysis Specifications: createStudyPopArgs.priorOutcomeLookBack
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # Default from template
          )
        })
      )

      # Target-Comparator-Outcome list
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # In the analysis specifications, covariateSelection.conceptsToExclude is empty.
          # The template had placeholders for target/comparator concepts, but cmTcList only has cohort IDs.
          # Therefore, excludedCovariateConceptIds will be empty based on the analysis specifications.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Arguments for getting cohort method data
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Default from template
        studyStartDate = studyStartDate, # From current loop iteration
        studyEndDate = studyEndDate, # From current loop iteration
        maxCohortSize = 0, # From Analysis Specifications: getDbCohortMethodDataArgs.maxCohortSize
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Default from template. Setting to FALSE to allow Strategus complete all CM operations; when a model cannot be fit, the equipoise diagnostic should fail.
        estimator = "att", # Default from template.
        prior = Cyclops::createPrior( # Prior settings from Analysis Specifications: propensityScoreAdjustment.createPsArgs.prior
          priorType = "laplace", # From Analysis Specifications: priorType
          exclude = c(0), # Default from template (for intercept)
          useCrossValidation = TRUE # From Analysis Specifications: useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings from Analysis Specifications: propensityScoreAdjustment.createPsArgs.control
          noiseLevel = "quiet", # From Analysis Specifications: noiseLevel
          cvType = "auto", # From Analysis Specifications: cvType
          seed = 1, # Default from template for reproducibility
          resetCoefficients = TRUE, # From Analysis Specifications: resetCoefficients
          tolerance = 2e-07, # From Analysis Specifications: tolerance
          cvRepetitions = 10, # From Analysis Specifications: fold and cvRepetitions (using 10)
          startingVariance = 0.01 # From Analysis Specifications: startingVariance
        )
      )

      # Arguments for computing shared covariate balance (template default)
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default from template
        covariateFilter = NULL # Default from template
      )

      # Arguments for computing covariate balance (template default)
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default from template
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default from template
      )

      # Arguments for fitting outcome model
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From Analysis Specifications: fitOutcomeModelArgs.modelType
        stratified = TRUE, # From Analysis Specifications: fitOutcomeModelArgs.stratified
        useCovariates = FALSE, # From Analysis Specifications: fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE, # From Analysis Specifications: fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior( # Prior settings from Analysis Specifications: fitOutcomeModelArgs.prior
          priorType = "laplace", # From Analysis Specifications: priorType
          useCrossValidation = TRUE # From Analysis Specifications: useCrossValidation
        ),
        control = Cyclops::createControl( # Control settings from Analysis Specifications: fitOutcomeModelArgs.control
          cvType = "auto", # From Analysis Specifications: cvType
          seed = 1, # Default from template for reproducibility
          resetCoefficients = TRUE, # From Analysis Specifications: resetCoefficients
          startingVariance = 0.01, # From Analysis Specifications: startingVariance
          tolerance = 2e-07, # From Analysis Specifications: tolerance
          cvRepetitions = 10, # From Analysis Specifications: fold and cvRepetitions (using 10)
          noiseLevel = "quiet" # From Analysis Specifications: noiseLevel
        )
      )

      # Arguments for creating study population
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications: createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE, # From Analysis Specifications: createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0, # From Analysis Specifications: createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # From Analysis Specifications: createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # From Analysis Specifications: createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # From Analysis Specifications: createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # From Analysis Specifications: createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From current timeAtRisks loop iteration
        startAnchor = timeAtRisks$startAnchor[t], # From current timeAtRisks loop iteration
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From current timeAtRisks loop iteration
        endAnchor = timeAtRisks$endAnchor[t], # From current timeAtRisks loop iteration
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From current timeAtRisks loop iteration
        maxDaysAtRisk = 99999 # Default from template, not specified in analysis specifications
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
  analysesToExclude = NULL, # Default from template
  refitPsForEveryOutcome = FALSE, # Default from template
  refitPsForEveryStudyPopulation = FALSE, # Default from template
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default from template
)

# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file
# The file path uses the 'name' field from the Analysis Specifications ("strokerisk")
# to create the directory and file name.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", studyName, paste0(studyName, "AnalysisSpecification.json"))
)