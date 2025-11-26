################################################################################
# COVID-19 Famotidine Study - Strategus Analysis Specification
# 
# This script creates analysis specifications for a comparative cohort study
# using the OHDSI Strategus framework. The study compares target and comparator
# cohorts with respect to outcome occurrence.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from WebAPI
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions from ATLAS using the cohort IDs specified in the analysis specifications
# Target cohort ID: 1794126, Comparator cohort ID: 1794132, Outcome cohort ID: 1794131
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
# This standardization simplifies downstream analysis configuration
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Extract negative control outcome concept set (ID: 1888110) from ATLAS
# These are used to empirically calibrate p-values and confidence intervals
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
  mutate(cohortId = row_number() + 100) %>% # Negative control cohort IDs start at 101 to avoid conflicts with main cohorts (1, 2, 3)
  select(cohortId, cohortName, outcomeConceptId)

# Validation check: Ensure no duplicate cohort IDs exist between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts we'll use in each analysis -----------

# Outcomes: Define the outcome of interest (cohort ID 3)
# cleanWindow is set to 365 days (not used in this analysis but included for completeness)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Define the comparison: cohort 1 (target1) vs cohort 2 (comparator1)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this study
# The analysis specifications indicate no specific concepts to exclude (both id and name are null/empty)
# Creating an empty data frame to maintain code structure
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# Optional: No specific covariates to include are defined in the analysis specifications
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# This module generates cohorts from the cohort definitions and negative control outcomes
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
# This module performs diagnostic checks on the generated cohorts
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

# Study period restriction: Analysis specifications define a specific study window
# Study period: February 1, 2020 to May 30, 2020
studyPeriods <- tibble(
  studyStartDate = c("20200201"), # YYYYMMDD format
  studyEndDate   = c("20200530")  # YYYYMMDD format
)

# Time-at-risks (TARs) for the outcomes of interest in your study
# From analysis specifications: riskWindowStart=1, startAnchor="cohort start", 
# riskWindowEnd=30, endAnchor="cohort start", minDaysAtRisk=1
timeAtRisks <- tibble(
  label = c("1-30d"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(30),
  endAnchor = c("cohort start")
) 

# Propensity Score settings - match on PS
# Analysis specifications do not define matchOnPsArgs (it is null)
# Creating empty data frame to maintain code structure
matchOnPsArgsList <- tibble(
  label = character(0),
  maxRatio  = numeric(0),
  caliper = numeric(0),
  caliperScale  = character(0)
) 

# Propensity Score settings - stratify by PS
# From analysis specifications: numberOfStrata=5, baseSelection="all"
stratifyByPsArgsList <- tibble(
  label = c("Stratify-5strata"),
  numberOfStrata  = c(5),
  baseSelection = c("all")
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
# This will be used as we have stratifyByPsArgs defined in the analysis specifications
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
# This creates a CohortMethod analysis for each combination of:
# - Study period
# - Time-at-risk window
# - Propensity score adjustment method
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment based on method type (match or stratify)
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
        # From analysis specifications: numberOfStrata=5, baseSelection="all"
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings: Use default Large-Scale Propensity Score (LSPS) covariates
      # addDescendantsToExclude ensures that descendant concepts of excluded concepts are also excluded
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome list combining outcomes of interest and negative controls
      # Outcomes of interest: priorOutcomeLookback from analysis specs is 30 days
      # Negative controls: trueEffectSize=1 (null effect)
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 30 # From analysis specifications
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
      
      # Create target-comparator-outcomes combinations
      # Exclude target and comparator concepts from covariate construction to avoid bias
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId # Empty in this study
        )
      }

      # Get database cohort method data arguments
      # From analysis specifications: restrictToCommonPeriod=FALSE, maxCohortSize=0 (no limit),
      # studyStartDate="20200201", studyEndDate="20200530"
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE, # From analysis specifications
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From analysis specifications: 0 means no limit
        covariateSettings = covariateSettings
      )

      # Create propensity score arguments
      # From analysis specifications: maxCohortSizeForFitting=250000, errorOnHighCorrelation=TRUE,
      # priorType="laplace", useCrossValidation=TRUE
      # Control settings: tolerance=2e-7, cvType="auto", fold=10, cvRepetitions=10, 
      # noiseLevel="silent", resetCoefficients=TRUE, startingVariance=0.01
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # Setting to FALSE to allow Strategus to complete all CM operations
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace", # From analysis specifications
          exclude = c(0), 
          useCrossValidation = TRUE # From analysis specifications
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", # From analysis specifications
          cvType = "auto", # From analysis specifications
          seed = 1, 
          resetCoefficients = TRUE, # From analysis specifications
          tolerance = 2e-07, # From analysis specifications
          cvRepetitions = 10, # From analysis specifications (note: template had 1, specs say 10)
          startingVariance = 0.01, # From analysis specifications
          fold = 10 # From analysis specifications
        )
      )

      # Compute shared covariate balance arguments (for all covariates)
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      
      # Compute covariate balance arguments (for Table 1 covariates only)
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit outcome model arguments
      # From analysis specifications: modelType="cox", stratified=TRUE, useCovariates=FALSE,
      # inversePtWeighting=FALSE, priorType="laplace", useCrossValidation=TRUE
      # Control settings: tolerance=2e-7, cvType="auto", fold=10, cvRepetitions=10,
      # noiseLevel="quiet", resetCoefficients=TRUE, startingVariance=0.01
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From analysis specifications
        stratified = TRUE, # From analysis specifications
        useCovariates = FALSE, # From analysis specifications
        inversePtWeighting = FALSE, # From analysis specifications
        prior = Cyclops::createPrior(
          priorType = "laplace", # From analysis specifications
          useCrossValidation = TRUE # From analysis specifications
        ),
        control = Cyclops::createControl(
          cvType = "auto", # From analysis specifications
          seed = 1, 
          resetCoefficients = TRUE, # From analysis specifications
          startingVariance = 0.01, # From analysis specifications
          tolerance = 2e-07, # From analysis specifications
          cvRepetitions = 10, # From analysis specifications
          noiseLevel = "quiet", # From analysis specifications
          fold = 10 # From analysis specifications
        )
      )
      
      # Create study population arguments
      # From analysis specifications: restrictToCommonPeriod=FALSE, firstExposureOnly=FALSE,
      # washoutPeriod=0, removeDuplicateSubjects="keep all", censorAtNewRiskWindow=FALSE,
      # removeSubjectsWithPriorOutcome=TRUE, priorOutcomeLookBack=30
      # Time-at-risk: riskWindowStart=1, startAnchor="cohort start", riskWindowEnd=30, 
      # endAnchor="cohort start", minDaysAtRisk=1
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From analysis specifications
        firstExposureOnly = FALSE, # From analysis specifications
        washoutPeriod = 0, # From analysis specifications
        removeDuplicateSubjects = "keep all", # From analysis specifications
        censorAtNewRiskWindow = FALSE, # From analysis specifications
        removeSubjectsWithPriorOutcome = TRUE, # From analysis specifications
        priorOutcomeLookback = 30, # From analysis specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1, # From analysis specifications (in timeAtRisks)
        maxDaysAtRisk = 99999
      )

      # Append the settings to Analysis List
      # Each analysis is uniquely identified by analysisId and has a descriptive label
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
# refitPsForEveryOutcome=FALSE: PS model is fit once and reused across outcomes
# refitPsForEveryStudyPopulation=FALSE: PS model is fit once and reused across study populations
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the analysis specifications ------------------------------------------
# Combine all module specifications and shared resources into a single analysis specification
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "covid19famotidine", "covid19famotidineAnalysisSpecification.json")
)