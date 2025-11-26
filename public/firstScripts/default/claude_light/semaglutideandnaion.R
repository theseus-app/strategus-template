################################################################################
# Strategus Analysis Specification Script
# Study: semaglutideandnaion
# Purpose: Create analysis specifications for CohortMethod study examining
#          the relationship between semaglutide exposure and NAION outcome
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources: Cohort and Outcome Definitions ----------------------------

# IMPORTANT: Replace baseUrl with your actual WebAPI instance
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Export cohort definitions from ATLAS
# Cohort IDs: 1794126 (target), 1794132 (comparator), 1794131 (outcome)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: Semaglutide users
    1794132, # Comparator: Alternative treatment
    1794131  # Outcome: NAION (Nonarteritic Anterior Ischemic Optic Neuropathy)
  ),
  generateStats = TRUE
)

# Standardize cohort IDs for internal use
# Target = 1, Comparator = 2, Outcome = 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Export negative control outcome concept set from ATLAS
# Concept Set ID: 1888110 (negative controls)
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
  # Assign cohort IDs starting at 101 for negative control outcomes
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Validation: Ensure no duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohor

tSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Outcome cohorts data frame
# Contains the primary outcome of interest (NAION) with clean window
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator cohorts for CohortMethod analysis
# Maps internal cohort IDs to their names for clarity
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# CohortGeneratorModule Settings -----------------------------------------------

cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

# Create shared resource specifications for negative control outcomes
# occurrenceType: "first" - use first occurrence of outcome concept
# detectOnDescendants: TRUE - include descendant concepts in the concept set
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create CohortGenerator module specifications
# generateStats: TRUE - generate cohort characterization statistics
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings --------------------------------------------

cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Create CohortDiagnostics module specifications
# Runs comprehensive diagnostics on all cohorts
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

# CohortMethodModule Settings --------------------------------------------------

# Define study period: December 1, 2017 to December 31, 2023
# Format: YYYYMMDD (numeric)
studyPeriods <- tibble(
  studyStartDate = 20171201,
  studyEndDate = 20231231
)

# Define time-at-risk (TAR) window
# riskWindowStart: 1 day after cohort start (washout period)
# riskWindowEnd: 0 (cohort end) means end of exposure in the cohort
# minDaysAtRisk: 1 - require at least 1 day of follow-up
timeAtRisks <- tibble(
  label = "Start +1 to cohort end",
  riskWindowStart = 1,
  startAnchor = "cohort start",
  riskWindowEnd = 0,
  endAnchor = "cohort end",
  minDaysAtRisk = 1
)

# Propensity Score (PS) Configuration 1: Match on PS
# maxRatio: 1:1 matching (no many-to-one matching)
# caliper: 0.2 - maximum distance on PS scale for matching
# caliperScale: "standardized logit" - PS scale measured in standard deviations of logit(PS)
matchOnPsArgsList <- tibble(
  label = "PS matching 1:1, caliper=0.2",
  maxRatio = 1,
  caliper = 0.2,
  caliperScale = "standardized logit"
)

# Propensity Score Configuration 2: Stratify by PS
# numberOfStrata: 5 - divide population into 5 PS strata (quintiles)
# baseSelection: "all" - stratification applied to combined target + comparator population
stratifyByPsArgsList <- tibble(
  label = "PS stratification, 5 strata",
  numberOfStrata = 5,
  baseSelection = "all"
)

# Build PS configuration list combining both methods
# Each configuration specifies adjustment method and parameters
psConfigList <- list()

# Add PS matching configuration
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

# Add PS stratification configuration
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

# Create CohortMethod analyses for all study combinations
# Loop structure: study periods × time-at-risk windows × PS configurations
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create PS adjustment arguments based on method
      if (psCfg$method == "match") {
        # Configuration for PS matching
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # Configuration for PS stratification
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Create default covariate settings with descendant concepts excluded
      # covariateSelection: Include empty concepts (all covariates used)
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome list combining:
      # 1) Primary outcome of interest (NAION) with priorOutcomeLookback
      # 2) Negative control outcomes for validating method performance
      outcomeList <- append(
        # Primary outcomes
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999  # Look back entire history for prior outcome
          )
        }),
        # Negative control outcomes (for sensitivity analysis)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1  # Assume no true effect for negative controls
          )
        })
      )

      # Create target-comparator-outcome relationships
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # No exposure concepts to exclude (covariateSelection already empty)
          excludedCovariateConceptIds = c()
        )
      }

      # Get cohort method data arguments
      # Study period: 2017-12-01 to 2023-12-31
      # restrictToCommonPeriod: TRUE - both cohorts must have overlap
      # maxCohortSize: 0 - no size limit
      # firstExposureOnly: FALSE - include all exposures
      # washoutPeriod: 0 - no washout period
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        covariateSettings = covariateSettings
      )

      # Create propensity score arguments
      # Prior: Laplace (L1 regularization) with cross-validation
      # Control: Auto CV type, 10-fold CV, 10 repetitions
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,  # Continue if PS fitting fails
        estimator = "att",  # Average treatment effect on treated
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
          cvRepetitions = 10,
          fold = 10,
          startingVariance = 0.01
        )
      )

      # Compute covariate balance before/after PS adjustment (shared model)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL  # All covariates evaluated
      )

      # Compute covariate balance for standard Table 1 specification
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit outcome model arguments
      # Model type: Cox proportional hazards (survival analysis)
      # stratified: TRUE - stratified by matching variable if matched
      # useCovariates: FALSE - no adjustment covariates in outcome model
      # inversePtWeighting: FALSE - not using inverse probability weighting
      # Prior: Laplace (L1 regularization) with cross-validation
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = TRUE,
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
          cvRepetitions = 10,
          fold = 10,
          noiseLevel = "quiet"
        )
      )

      # Create study population arguments
      # Time-at-risk: Starts 1 day after cohort start, ends at cohort end
      # removeSubjectsWithPriorOutcome: TRUE - exclude those with prior outcome
      # priorOutcomeLookBack: 99999 - entire history
      # censorAtNewRiskWindow: TRUE - censor when entering a new risk window
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = TRUE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t]
      )

      # Add analysis to list
      # Each analysis combines: study period, TAR window, and PS adjustment method
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

# Create CohortMethodModule specifications
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create Final Analysis Specifications ----------------------------------------
# Combines all module specifications into single analysis specification object

analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() %>%
  # Add shared resources (cohort definitions and negative controls)
  Strategus::addSharedResources(cohortDefinitionShared) %>%
  Strategus::addSharedResources(negativeControlsShared) %>%
  # Add module specifications (CohortGenerator, CohortDiagnostics, CohortMethod)
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save analysis specifications to JSON file
# Output location should match your study directory structure
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "semaglutideandnaion", "semaglutideandnaionAnalysisSpecification.json")
)

cat("Analysis specification created successfully!\n")
cat("File saved to: inst/semaglutideandnaion/semaglutideandnaionAnalysisSpecification.json\n")