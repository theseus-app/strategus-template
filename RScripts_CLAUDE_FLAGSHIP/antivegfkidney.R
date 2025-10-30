################################################################################
# Analysis Specifications for antivegfkidney Study
# 
# This script creates the Strategus analysis specifications for a 
# comparative cohort study examining anti-VEGF therapy and kidney outcomes.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Base URL for the OHDSI WebAPI instance
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Export cohort definitions from WebAPI for:
# - Target cohort (ID: 1794126)
# - Comparator cohort (ID: 1794132)
# - Outcome cohort (ID: 1794131)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to sequential IDs (1, 2, 3)
# This standardization simplifies downstream analysis configuration
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve negative control concept set (ID: 1888110) from WebAPI
# Negative controls are outcomes not expected to be associated with the exposures
# and are used to calibrate empirical p-values
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
  # Assign cohort IDs starting at 101 to avoid conflicts with main cohorts (1,2,3)
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames for cohorts used in each analysis -------------------------

# Outcomes: Define the outcome of interest (cohort ID 3)
# cleanWindow is not directly used but included for documentation purposes
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Target: cohort ID 1 (target1)
# Comparator: cohort ID 2 (comparator1)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate exclusions/inclusions
# Per specifications, no specific concepts are included or excluded
# (conceptsToInclude and conceptsToExclude have null/empty values)
# If needed in future, define excludedCovariateConcepts here
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# CohortGeneratorModule --------------------------------------------------------
# This module generates the cohorts defined above in the target database
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Share the cohort definitions across modules
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Share negative control outcome cohort specifications
# occurrenceType = "first": only the first occurrence of the outcome is considered
# detectOnDescendants = TRUE: include descendant concepts in outcome detection
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Configure cohort generation to include cohort statistics
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# This module performs diagnostic checks on generated cohorts
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
# This module performs the comparative cohort analysis

# Study periods
# Per specifications: studyStartDate = null, studyEndDate = null
# Empty vectors indicate no restriction to specific calendar periods
studyPeriods <- tibble(
  studyStartDate = c(""),
  studyEndDate = c("")
)

# Time-at-risk (TAR) definitions
# Defines when outcomes are counted relative to cohort entry/exit
# Per specifications:
# - riskWindowStart = 0, startAnchor = "cohort start"
# - riskWindowEnd = 0, endAnchor = "cohort end"
# - minDaysAtRisk = 1
# This creates an "on-treatment" analysis (from cohort start to cohort end)
timeAtRisks <- tibble(
  label = c("OnTreatment"),
  riskWindowStart = c(0),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(0),
  endAnchor = c("cohort end")
)

# Propensity Score settings - match on PS
# Per specifications:
# - maxRatio = 1 (1:1 matching)
# - caliper = 0.2
# - caliperScale = "standardized logit"
matchOnPsArgsList <- tibble(
  label = c("Match_1to1_cal0.2"),
  maxRatio = c(1),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# Propensity Score settings - stratify by PS
# Per specifications: stratifyByPsArgs = null (not used)
# Create empty data frame to skip stratification
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata = numeric(0),
  baseSelection = character(0)
)

# Build a single PS configuration list
# Each entry contains: method (match/stratify), label, and parameters
psConfigList <- list()

# Convert each row of matchOnPsArgsList to a PS configuration
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "match",
      label = matchOnPsArgsList$label[i],
      params = list(
        maxRatio = matchOnPsArgsList$maxRatio[i],
        caliper = matchOnPsArgsList$caliper[i],
        caliperScale = matchOnPsArgsList$caliperScale[i]
      )
    )
  }
}

# Convert each row of stratifyByPsArgsList to a PS configuration
# (empty in this analysis)
if (exists("stratifyByPsArgsList") && nrow(stratifyByPsArgsList) > 0) {
  for (i in seq_len(nrow(stratifyByPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "stratify",
      label = stratifyByPsArgsList$label[i],
      params = list(
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}

# Iterate through all analysis setting combinations
# Creates a separate analysis for each combination of:
# - Study period
# - Time-at-risk definition
# - Propensity score method
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS method based on configuration type
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

      # Covariate settings: use default LSPS (Large-Scale Propensity Score) covariates
      # addDescendantsToExclude = TRUE: when excluding concepts, also exclude their descendants
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome list combining:
      # 1. Outcomes of interest (from oList)
      # 2. Negative control outcomes
      outcomeList <- append(
        # Outcomes of interest
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999  # Per specifications: priorOutcomeLookBack = 99999
          )
        }),
        # Negative control outcomes
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1  # Assumed true effect size for negative controls is 1 (null effect)
          )
        })
      )
      
      # Create target-comparator-outcomes combinations
      # Links target cohort, comparator cohort, and all outcomes
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Per specifications: conceptsToExclude has null/empty values
          # Only exclude if specific concepts were defined
          excludedCovariateConceptIds = if(nrow(excludedCovariateConcepts) > 0) {
            excludedCovariateConcepts$conceptId
          } else {
            c()
          }
        )
      }

      # Arguments for extracting cohort method data from the database
      # Per specifications:
      # - studyStartDate = null, studyEndDate = null (no date restrictions)
      # - maxCohortSize = 0 (no limit on cohort size)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,  # Per specifications: restrictToCommonPeriod = false
        studyStartDate = if(studyStartDate == "") "" else studyStartDate,
        studyEndDate = if(studyEndDate == "") "" else studyEndDate,
        maxCohortSize = 0,  # Per specifications: maxCohortSize = 0
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity score model
      # Per specifications:
      # - maxCohortSizeForFitting = 250000
      # - errorOnHighCorrelation = true
      # - prior: priorType = "laplace", useCrossValidation = true
      # - control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      #   noiseLevel = "silent", resetCoefficients = true, startingVariance = 0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,  # Allow Strategus to complete all operations even if PS fitting fails
        estimator = "att",    # Average treatment effect in the treated
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
          cvRepetitions = 10,  # Per specifications: cvRepetitions = 10
          startingVariance = 0.01,
          fold = 10  # Per specifications: fold = 10
        )
      )

      # Arguments for computing shared covariate balance
      # Uses all covariates (no filter)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      
      # Arguments for computing covariate balance (for Table 1)
      # Uses default Table 1 specifications
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting outcome model
      # Per specifications:
      # - modelType = "cox"
      # - stratified = false (specification says false, but typically true for matched/stratified)
      # - useCovariates = false
      # - inversePtWeighting = false
      # - prior: priorType = "laplace", useCrossValidation = true
      # - control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      #   noiseLevel = "quiet", resetCoefficients = true, startingVariance = 0.01
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = FALSE,  # Per specifications: stratified = false
        useCovariates = FALSE,  # Per specifications: useCovariates = false
        inversePtWeighting = FALSE,  # Per specifications: inversePtWeighting = false
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
          cvRepetitions = 10,  # Per specifications: cvRepetitions = 10
          noiseLevel = "quiet",
          fold = 10  # Per specifications: fold = 10
        )
      )
      
      # Arguments for creating study population
      # Per specifications:
      # - restrictToCommonPeriod = false
      # - firstExposureOnly = true
      # - washoutPeriod = 365
      # - removeDuplicateSubjects = "keep all"
      # - censorAtNewRiskWindow = false
      # - removeSubjectsWithPriorOutcome = true
      # - priorOutcomeLookBack = 99999
      # - Time-at-risk parameters from timeAtRisks table
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
        minDaysAtRisk = 1,  # Per specifications: minDaysAtRisk = 1
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
# refitPsForEveryOutcome = FALSE: reuse PS model across outcomes
# refitPsForEveryStudyPopulation = FALSE: reuse PS model across study populations
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
# Combine all modules and shared resources into a single specification
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "antivegfkidney", "antivegfkidneyAnalysisSpecification.json")
)