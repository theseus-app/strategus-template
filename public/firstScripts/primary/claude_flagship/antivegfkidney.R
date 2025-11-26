################################################################################
# Strategus Analysis Specification for antivegfkidney Study
#
# This script creates analysis specifications using the OHDSI Strategus package
# based on the study design settings provided in the Analysis Specifications.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from the ATLAS instance
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions for target, comparator, and outcome cohorts
# using the exact cohort IDs specified in the Analysis Specifications
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to sequential IDs (1, 2, 3) for internal use
# This simplifies referencing cohorts in the analysis specifications
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve the negative control concept set (id: 1888110, name: negative)
# and create cohort definitions for each negative control outcome
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,
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
  mutate(cohortId = row_number() + 100) %>% # Negative control cohort IDs start at 101 to avoid conflicts
  select(cohortId, cohortName, outcomeConceptId)

# Check for duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts for each analysis --------------------

# Outcomes: Define the outcome cohort(s) of interest
# Using cohortId = 3 (outcome1) with a 365-day clean window
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Target: cohortId = 1 (target1)
# Comparator: cohortId = 2 (comparator1)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate exclusion settings
# Note: Analysis Specifications indicate no specific concepts to exclude (id: null)
# so we create an empty data frame as a placeholder
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# Note: Analysis Specifications indicate no specific concepts to include (id: null)
# If needed in the future, uncomment and populate:
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# Configure the CohortGenerator module to create cohorts in the CDM database
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# Configure the CohortDiagnostics module to assess cohort quality
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
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

# Study Period Settings
# Analysis Specifications: studyStartDate = null, studyEndDate = null
# This means no restriction on study period - using all available data
studyPeriods <- tibble(
  studyStartDate = c(""), # Empty string = no start date restriction
  studyEndDate   = c("")  # Empty string = no end date restriction
)

# Time-at-risk (TAR) settings
# Analysis Specifications: riskWindowStart = 1, startAnchor = "cohort start",
# riskWindowEnd = 0, endAnchor = "cohort end", minDaysAtRisk = 1
timeAtRisks <- tibble(
  label = c("On treatment"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
) 

# Propensity Score Adjustment Settings
# Analysis Specifications: matchOnPsArgs with maxRatio=1, caliper=0.2, caliperScale="standardized logit"
# stratifyByPsArgs = null (no stratification)
matchOnPsArgsList <- tibble(
  label = c("PS Match 1:1 cal0.2"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
) 

# No stratification by PS in this study (stratifyByPsArgs = null)
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = numeric(0),
  baseSelection = character(0)
) 

# Build a single PS configuration list
# This consolidates all PS adjustment methods into one list for iteration
psConfigList <- list()

# Convert matchOnPsArgsList to configuration list
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

# Convert stratifyByPsArgsList to configuration list (if any)
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
# This creates a separate analysis for each combination of study period, TAR, and PS adjustment
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment method based on configuration
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

      # Covariate settings: using default LSPS covariates
      # Analysis Specifications: conceptsToInclude/Exclude have id=null (no specific inclusion/exclusion)
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome list: includes outcomes of interest and negative controls
      # Analysis Specifications: removeSubjectsWithPriorOutcome=true, priorOutcomeLookBack=99999
      outcomeList <- append(
        # Outcomes of interest
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        # Negative control outcomes (for empirical calibration)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )
      
      # Create target-comparator-outcomes combinations
      # Note: excludedCovariateConceptIds is empty per Analysis Specifications
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # GetDbCohortMethodData settings
      # Analysis Specifications: maxCohortSize=0 (no limit)
      # Note: restrictToCommonPeriod is set in createStudyPopArgs, but we also set it here
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE, # Using setting from createStudyPopArgs (false)
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # No limit per Analysis Specifications
        covariateSettings = covariateSettings
      )

      # CreatePs settings for propensity score model
      # Analysis Specifications: 
      # - maxCohortSizeForFitting=250000
      # - errorOnHighCorrelation=true
      # - prior: priorType="laplace", useCrossValidation=true
      # - control: tolerance=2e-7, cvType="auto", fold=10, cvRepetitions=10, 
      #   noiseLevel="silent", resetCoefficients=true, startingVariance=0.01
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # Allow Strategus to complete all operations even if PS model fails
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace", 
          exclude = c(0), 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", 
          cvType = "auto", 
          seed = 1, 
          resetCoefficients = TRUE, 
          tolerance = 2e-07, 
          cvRepetitions = 10, # Using cvRepetitions from Analysis Specifications
          startingVariance = 0.01,
          fold = 10 # Using fold from Analysis Specifications
        )
      )

      # Covariate balance computation settings
      # Shared balance uses all covariates, regular balance uses Table 1 specs
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # FitOutcomeModel settings
      # Analysis Specifications:
      # - modelType="cox"
      # - stratified=false
      # - useCovariates=false
      # - inversePtWeighting=false
      # - prior: priorType="laplace", useCrossValidation=true
      # - control: tolerance=2e-7, cvType="auto", fold=10, cvRepetitions=10,
      #   noiseLevel="quiet", resetCoefficients=true, startingVariance=0.01
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = FALSE, # Using false per Analysis Specifications
        useCovariates = FALSE,
        inversePtWeighting = FALSE,
        prior = Cyclops::createPrior(
          priorType = "laplace", 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          cvType = "auto", 
          seed = 1, 
          resetCoefficients = TRUE,
          startingVariance = 0.01, 
          tolerance = 2e-07, 
          cvRepetitions = 10, # Using cvRepetitions from Analysis Specifications
          noiseLevel = "quiet",
          fold = 10 # Using fold from Analysis Specifications
        )
      )
      
      # CreateStudyPopulation settings
      # Analysis Specifications:
      # - restrictToCommonPeriod=false
      # - firstExposureOnly=true
      # - washoutPeriod=365
      # - removeDuplicateSubjects="keep all"
      # - censorAtNewRiskWindow=false
      # - removeSubjectsWithPriorOutcome=true
      # - priorOutcomeLookBack=99999
      # - minDaysAtRisk=1
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = TRUE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Append the complete analysis configuration to the list
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

# Create CohortMethod module specifications
# This bundles all CM analyses together for execution by Strategus
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the complete analysis specifications ---------------------------------
# This combines all modules (CohortGenerator, CohortDiagnostics, CohortMethod)
# and shared resources into a single specification file
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to JSON file
# File naming follows the pattern: <studyName>AnalysisSpecification.json
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "antivegfkidney", "antivegfkidneyAnalysisSpecification.json")
)