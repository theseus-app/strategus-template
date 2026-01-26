################################################################################
# Strategus Analysis Specification for uveitissafety study
# 
# This script creates analysis specifications for a comparative safety study
# using the OHDSI Strategus framework. The study compares two treatment cohorts
# for the risk of developing uveitis-related outcomes.
# 
# Key components:
# 1. Cohort definitions (target, comparator, outcome)
# 2. Negative control outcomes for empirical calibration
# 3. CohortMethod analysis with propensity score matching
# 4. Cox proportional hazards outcome models
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
################################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from ATLAS
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions - using IDs from analysis specifications
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal use in the analysis
# Strategus requires sequential cohort IDs starting from 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes for empirical calibration
# Using concept set ID 1888110 (named "negative") from specifications
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
  mutate(cohortId = row_number() + 100) %>% # target/comparator cohort ids start with 1, 2, 3... negativeControl -> 101, 102, 103...
  select(cohortId, cohortName, outcomeConceptId)

# Validate that there are no duplicate cohort IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts used in each analysis ---------------
# Outcomes: only outcome1 (cohort ID 3) with 365-day clean window
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis 
# Using re-numbered cohort IDs (1 for target, 2 for comparator)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# No specific drug concepts to exclude from covariates (empty from specifications)
# Note: The analysis specifications had empty conceptsToInclude and conceptsToExclude
# excludedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# Optional: If you want to define covariates to include instead of including them all
# Note: Analysis specifications have empty conceptsToInclude, so we don't create this
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# Create shared resources and module specifications for cohort generation
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

# CohortDiagnoticsModule Settings ----------------------------------------------
# Create module specifications for cohort diagnostics
# Using all cohort IDs from the cohort definition set
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
# Study periods: no restrictions (null dates in specifications)
# Create empty study periods as specified
studyPeriods <- tibble(
  studyStartDate = character(), # Empty as per specifications
  studyEndDate   = character()  # Empty as per specifications
)

# Time-at-risks (TARs) for the outcomes from analysis specifications
# Single TAR with risk window from day 1 after cohort start to cohort end
# Minimum 1 day at risk required
timeAtRisks <- tibble(
  label = c("Day1ToCohortEnd"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
) 

# Propensity Score settings - match on PS only (stratifyByPsArgs is null in specs)
# Using parameters from analysis specifications:
# - maxRatio: 10 (maximum matching ratio)
# - caliper: 0.2 (caliper width)
# - caliperScale: "standardized logit" (scale for caliper)
matchOnPsArgsList <- tibble(
  label = c("MatchOnPs"),
  maxRatio  = c(10),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
) 

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Convert matchOnPsArgsList to configuration list
# Since stratifyByPsArgs is null in specifications, we only create match configurations
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

# Create outcome list including both primary outcome and negative controls
# Primary outcome (outcome1) with outcomeOfInterest = TRUE
# Negative controls with outcomeOfInterest = FALSE and trueEffectSize = 1
outcomeList <- append(
  lapply(seq_len(nrow(oList)), function(i) {
    CohortMethod::createOutcome(
      outcomeId = oList$outcomeCohortId[i],
      outcomeOfInterest = TRUE,
      trueEffectSize = NA,
      priorOutcomeLookback = 99999  # From createStudyPopArgs in specifications
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

# Create target comparator outcomes list
# No excluded covariate concepts specified (empty in specifications)
targetComparatorOutcomesList <- list()
for (i in seq_len(nrow(cmTcList))) {
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    excludedCovariateConceptIds = integer(0)  # No concepts to exclude per specifications
  )
}

# Iterate through all analysis setting combinations
# With empty study periods, we have one iteration (s=1 with empty dates)
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(max(1, nrow(studyPeriods)))) {
  # Handle empty study periods (use NULL for dates)
  studyStartDate <- if (nrow(studyPeriods) > 0) studyPeriods$studyStartDate[s] else ""
  studyEndDate <- if (nrow(studyPeriods) > 0) studyPeriods$studyEndDate[s] else ""
  
  for (t in seq_len(nrow(timeAtRisks))) {
    
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Create PS adjustment arguments based on method
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
      
      # Covariate settings - using default settings
      # No specific inclusion/exclusion concepts from specifications
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )
      
      # GetDbCohortMethodDataArgs from analysis specifications
      # restrictToCommonPeriod = TRUE, maxCohortSize = 0 (no restriction)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = if (studyStartDate == "") NULL else studyStartDate,
        studyEndDate = if (studyEndDate == "") NULL else studyEndDate,
        maxCohortSize = 0,  # No restriction per specifications
        covariateSettings = covariateSettings
      )
      
      # CreatePsArgs from analysis specifications
      # Using Laplace prior with cross-validation as specified
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # From specifications
        errorOnHighCorrelation = TRUE,     # From specifications
        stopOnError = FALSE,               # Allow Strategus to complete all operations
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",           # From specifications
          exclude = c(0), 
          useCrossValidation = TRUE        # From specifications
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",           # From specifications
          cvType = "auto",                 # From specifications
          seed = 1, 
          resetCoefficients = TRUE,        # From specifications
          tolerance = 2e-07,               # From specifications
          cvRepetitions = 10,              # From specifications (not 1 as in template)
          startingVariance = 0.01,         # From specifications
          fold = 10                        # From specifications
        )
      )
      
      # Covariate balance computation arguments
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )
      
      # FitOutcomeModelArgs from analysis specifications
      # Cox model with stratification by PS, no covariate adjustment
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",                 # From specifications
        stratified = TRUE,                 # From specifications
        useCovariates = FALSE,             # From specifications
        inversePtWeighting = FALSE,        # From specifications
        prior = Cyclops::createPrior(
          priorType = "laplace",           # From specifications
          useCrossValidation = TRUE        # From specifications
        ),
        control = Cyclops::createControl(
          cvType = "auto",                 # From specifications
          seed = 1, 
          resetCoefficients = TRUE,        # From specifications
          startingVariance = 0.01,         # From specifications
          tolerance = 2e-07,               # From specifications
          cvRepetitions = 10,              # From specifications (not 1 as in template)
          noiseLevel = "quiet",            # From specifications
          fold = 10                        # From specifications
        )
      )
      
      # CreateStudyPopArgs from analysis specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE,     # From specifications
        firstExposureOnly = TRUE,          # From specifications
        washoutPeriod = 365,               # From specifications
        removeDuplicateSubjects = "keep all",  # From specifications
        censorAtNewRiskWindow = TRUE,      # From specifications
        removeSubjectsWithPriorOutcome = TRUE,  # From specifications
        priorOutcomeLookback = 99999,      # From specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,                 # From specifications
        maxDaysAtRisk = 99999
      )
      
      # Append the settings to Analysis List
      # Create descriptive label for this analysis configuration
      description <- sprintf(
        "Study: %s-%s; TAR: %s; PS: %s",
        if (studyStartDate == "") "AllTime" else studyStartDate,
        if (studyEndDate == "") "AllTime" else studyEndDate,
        timeAtRisks$label[t],
        psCfg$label
      )
      
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = description,
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
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "uveitissafety", "uveitissafetyAnalysisSpecification.json")
)