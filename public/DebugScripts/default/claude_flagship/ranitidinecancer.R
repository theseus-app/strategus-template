################################################################################
# Strategus Analysis Specification for ranitidinecancer study
# 
# This script creates analysis specifications using the OHDSI Strategus package
# for a comparative cohort study examining ranitidine and cancer outcomes.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from ATLAS
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Exporting cohort definitions for target, comparator, and outcome cohorts
# from the ATLAS instance specified in baseUrl
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to sequential IDs starting from 1
# This standardization helps with downstream analysis processes
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieve concept set for negative controls and resolve to specific concepts
# Negative controls are used to assess residual bias in the study design
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
  mutate(cohortId = row_number() + 100) %>% # Start negative control IDs at 101 to avoid conflicts
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no duplicate cohort IDs exist across cohort sets
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to organize cohorts for analysis -------------------------

# Outcomes: Define the outcome cohorts with their associated clean windows
# cleanWindow specifies the minimum time between outcome occurrences
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Defines the exposure comparison for the study
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# For the CohortMethod Large Scale Propensity Score (LSPS) analysis,
# we exclude the drugs of interest to prevent their inclusion as covariates
# Since covariateSelection has null IDs in the specification, this data frame is empty
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(0),
  conceptName = character(0)
)

# CohortGeneratorModule --------------------------------------------------------
# This module generates the cohorts defined above in the target database
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specification for main cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource specification for negative control outcome cohorts
# occurrenceType = "first" means only the first occurrence of the outcome is counted
# detectOnDescendants = TRUE includes descendant concepts in the outcome definition
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications for cohort generation
# generateStats = TRUE produces descriptive statistics for generated cohorts
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# This module performs comprehensive diagnostics on generated cohorts
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

# Study Period Definition
# Empty strings indicate no restriction on study start/end dates
# This means the analysis will use all available data in the database
studyPeriods <- tibble(
  studyStartDate = c(""),
  studyEndDate = c("")
)

# Time-at-Risk (TAR) Windows
# These define when outcomes are counted relative to exposure
# Four TAR configurations are specified:
# 1. From day 1 after cohort start to 99999 days after cohort start (entire follow-up from day 1)
# 2. From day 365 after cohort start to 99999 days after cohort start (excluding first year)
# 3. From day 1 after cohort start to cohort end (on-treatment)
# 4. From day 365 after cohort start to cohort end (on-treatment, excluding first year)
timeAtRisks <- tibble(
  label = c(
    "TAR: 1d - 99999d from cohort start",
    "TAR: 365d - 99999d from cohort start",
    "TAR: 1d from cohort start to cohort end",
    "TAR: 365d from cohort start to cohort end"
  ),
  riskWindowStart = c(1, 365, 1, 365),
  startAnchor = c("cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd = c(99999, 99999, 0, 0),
  endAnchor = c("cohort start", "cohort start", "cohort end", "cohort end")
)

# Propensity Score Adjustment Settings

# Match on PS configurations
# maxRatio: maximum number of comparator subjects matched to each target subject
# caliper: maximum allowed difference in PS for matching
# caliperScale: scale on which caliper is defined
matchOnPsArgsList <- tibble(
  label = c("PS Match 1:1 cal0.2", "PS Match 1:10 cal0.2"),
  maxRatio = c(1, 10),
  caliper = c(0.2, 0.2),
  caliperScale = c("standardized logit", "standardized logit")
)

# Stratify by PS configurations
# numberOfStrata: number of strata for stratification
# baseSelection: which population to use for defining strata
stratifyByPsArgsList <- tibble(
  label = c("PS Stratification 10 strata"),
  numberOfStrata = c(10),
  baseSelection = c("all")
)

# Build a unified PS configuration list
# This combines both matching and stratification approaches
# Each configuration will be analyzed separately
psConfigList <- list()

# Convert each row of matchOnPsArgsList to a configuration entry
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

# Convert each row of stratifyByPsArgsList to a configuration entry
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

# Add a "no adjustment" configuration (crude analysis without PS adjustment)
psConfigList[[length(psConfigList) + 1]] <- list(
  method = "none",
  label = "No PS adjustment",
  params = list()
)

# Iterate through all analysis setting combinations
# This creates a separate analysis for each combination of:
# - Study period
# - Time-at-risk window
# - PS adjustment method
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS matching arguments based on the current PS configuration
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
      } else {
        # No PS adjustment
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- NULL
      }

      # Covariate settings using default Feature Extraction settings
      # This defines what patient characteristics are extracted for PS modeling
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome list combining outcomes of interest and negative controls
      # Outcomes of interest: outcomeOfInterest = TRUE, no true effect size assumed
      # Negative controls: outcomeOfInterest = FALSE, true effect size = 1 (null effect)
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
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
      # Each combination specifies which cohorts to compare and which outcomes to assess
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Arguments for extracting cohort method data from the database
      # restrictToCommonPeriod = FALSE: use all available follow-up time
      # maxCohortSize = 0: no limit on cohort size
      # washoutPeriod is applied at this stage per specification
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 365,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity score model
      # Uses Laplace prior with cross-validation for regularization
      # maxCohortSizeForFitting limits the sample size for PS model fitting
      # errorOnHighCorrelation = TRUE will error if covariates are highly correlated
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,
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
          cvRepetitions = 10,
          startingVariance = 0.01,
          fold = 10
        )
      )

      # Arguments for computing covariate balance on all covariates (for diagnostics)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      
      # Arguments for computing covariate balance on Table 1 covariates (for reporting)
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Arguments for fitting outcome model
      # FIX: When stratified = TRUE, we must have either matching or stratification
      # For "none" PS method, we set stratified = FALSE to allow unstratified Cox model
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = (psCfg$method != "none"),
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
          noiseLevel = "quiet",
          fold = 10
        )
      )
      
      # Arguments for creating study population
      # These settings define eligibility criteria and outcome observation windows
      # restrictToCommonPeriod = FALSE: specified in analysis specifications
      # firstExposureOnly = FALSE: allow multiple exposures per person
      # washoutPeriod = 0: no additional washout (already applied in getDbCohortMethodDataArgs)
      # removeDuplicateSubjects = "keep first": when a subject appears in both target and comparator, keep first exposure
      # censorAtNewRiskWindow = FALSE: do not censor at new exposure
      # removeSubjectsWithPriorOutcome = TRUE: exclude subjects with outcome before exposure
      # priorOutcomeLookBack = 99999: look back 99999 days for prior outcomes
      # Risk window parameters are taken from timeAtRisks for current iteration
      # minDaysAtRisk = 1: subjects must have at least 1 day of follow-up
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep first",
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

      # Create and append the complete CohortMethod analysis specification
      # This bundles all the arguments above into a single analysis configuration
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(studyStartDate == "", "no start", studyStartDate),
          ifelse(studyEndDate == "", "no end", studyEndDate),
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
# This bundles all analyses into a single module for execution
# refitPsForEveryOutcome = FALSE: use same PS model for all outcomes
# refitPsForEveryStudyPopulation = FALSE: use same PS model for all study populations
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
# This combines all modules (cohort generation, diagnostics, and analysis) into
# a single specification object that can be executed by Strategus
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to JSON file
# This file can be shared and executed across different data sites
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ranitidinecancer", "ranitidinecancerAnalysisSpecification.json")
)