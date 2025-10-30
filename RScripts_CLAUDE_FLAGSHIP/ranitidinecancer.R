################################################################################
# OHDSI Strategus Analysis Specification
# Study: ranitidinecancer
# 
# This script creates analysis specifications for a comparative cohort study
# examining the relationship between ranitidine exposure and cancer outcomes
# using the OHDSI Strategus framework.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Define the base URL for the ATLAS WebAPI instance
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export cohort definitions from ATLAS using the cohort IDs specified in the
# analysis specifications:
# - Target cohort (1794126): target1
# - Comparator cohort (1794132): comparator1
# - Outcome cohort (1794131): outcome1
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to sequential IDs for internal use
# Target cohort: 1794126 -> 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# Comparator cohort: 1794132 -> 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# Outcome cohort: 1794131 -> 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative Control Outcomes ----------------------------------------------------
# Retrieve the negative control concept set (ID: 1888110, name: negative)
# and convert concepts to cohort definitions for empirical calibration
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
  # Assign cohort IDs starting at 101 to avoid conflict with primary cohorts
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Verify no duplicate cohort IDs exist between primary and negative control cohorts
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Define Analysis Cohorts ------------------------------------------------------
# Outcomes: Extract outcome cohort information (cohortId = 3)
# cleanWindow represents the prior outcome lookback period (365 days per specifications)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for CohortMethod analysis
# Target: cohortId 1 (original ID: 1794126, target1)
# Comparator: cohortId 2 (original ID: 1794132, comparator1)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate Exclusions ---------------------------------------------------------
# Per analysis specifications, conceptsToExclude is empty (no specific concepts to exclude)
# If there were concepts to exclude, they would be listed here
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# Note: conceptsToInclude is also null in specifications, so we will use
# default covariate settings which include all available covariates

# CohortGeneratorModule --------------------------------------------------------
# Create cohort generator module to instantiate cohorts in the CDM database
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Register primary cohort definitions as shared resources
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Register negative control outcome cohorts as shared resources
# occurrenceType = "first": only the first occurrence of each outcome is considered
# detectOnDescendants = TRUE: include descendant concepts in outcome detection
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Define cohort generator module specifications
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule ------------------------------------------------------
# Create cohort diagnostics module to assess cohort quality and characteristics
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
# Configure comparative cohort method analysis parameters

# Study Periods ----------------------------------------------------------------
# Per specifications: studyStartDate = null, studyEndDate = null
# This means no restriction on study period - use all available data
studyPeriods <- tibble(
  studyStartDate = c(""),
  studyEndDate = c("")
)

# Time-At-Risk (TAR) Settings --------------------------------------------------
# Define the risk window for outcome observation relative to cohort entry
# From specifications:
# - riskWindowStart: 365 days after cohort start
# - riskWindowEnd: 9999 days after cohort start (effectively follow until end)
# - minDaysAtRisk: 1 day
timeAtRisks <- tibble(
  label = c("365d_to_end"),
  riskWindowStart = c(365),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(9999),
  endAnchor = c("cohort start")
)

# Propensity Score Settings - Match on PS --------------------------------------
# Per specifications: use 1:1 matching with caliper = 0.2 on standardized logit scale
matchOnPsArgsList <- tibble(
  label = c("PS_Match_1to1_cal0.2"),
  maxRatio = c(1),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# Propensity Score Settings - Stratify by PS -----------------------------------
# Per specifications: stratifyByPsArgs is null, so no stratification analysis
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata = numeric(0),
  baseSelection = character(0)
)

# Build PS Configuration List --------------------------------------------------
# Consolidate PS adjustment methods into a single list structure
psConfigList <- list()

# Add matching configurations
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

# Add stratification configurations (none in this analysis)
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

# Create CohortMethod Analysis List --------------------------------------------
# Iterate through all combinations of study periods, TARs, and PS methods
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
        # Match on propensity score
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # Stratify by propensity score
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate Settings -----------------------------------------------------
      # Use default covariate settings (no specific concepts to include per specifications)
      # addDescendantsToExclude = TRUE: exclude descendant concepts of excluded concepts
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcome List -----------------------------------------------------------
      # Combine outcomes of interest and negative controls
      outcomeList <- append(
        # Primary outcome(s) of interest
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            # Per specifications: priorOutcomeLookback = 365
            priorOutcomeLookback = 365
          )
        }),
        # Negative control outcomes for empirical calibration
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )
      
      # Target-Comparator-Outcomes List ----------------------------------------
      # Define combinations of target, comparator, and outcomes to analyze
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude concepts specified in excludedCovariateConcepts
          # (empty in this analysis per specifications)
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Get Database Data Arguments --------------------------------------------
      # Per specifications: maxCohortSize = 0 (no limit)
      # restrictToCommonPeriod = FALSE per createStudyPopArgs
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Create Propensity Score Arguments --------------------------------------
      # Per specifications:
      # - maxCohortSizeForFitting: 250000
      # - errorOnHighCorrelation: TRUE
      # - prior: Laplace with cross-validation
      # - control: tolerance = 2e-7, cvType = auto, fold = 10, cvRepetitions = 10, etc.
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

      # Covariate Balance Arguments --------------------------------------------
      # Compute covariate balance for all covariates (shared) and table 1 covariates
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit Outcome Model Arguments --------------------------------------------
      # Per specifications:
      # - modelType: "cox"
      # - stratified: FALSE
      # - useCovariates: FALSE
      # - inversePtWeighting: FALSE
      # - prior: Laplace with cross-validation
      # - control: tolerance = 2e-7, cvType = auto, fold = 10, cvRepetitions = 10, noiseLevel = quiet
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = FALSE,
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
      
      # Create Study Population Arguments --------------------------------------
      # Per specifications:
      # - restrictToCommonPeriod: FALSE
      # - firstExposureOnly: FALSE
      # - washoutPeriod: 365 days
      # - removeDuplicateSubjects: "keep all"
      # - censorAtNewRiskWindow: FALSE
      # - removeSubjectsWithPriorOutcome: TRUE
      # - priorOutcomeLookBack: 365 days
      # - minDaysAtRisk: 1
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 365,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Create CohortMethod Analysis -----------------------------------------
      # Combine all parameter settings into a single analysis specification
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(studyStartDate == "", "all", studyStartDate),
          ifelse(studyEndDate == "", "all", studyEndDate),
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

# Create CohortMethod Module Specifications ------------------------------------
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create Complete Analysis Specifications -------------------------------------
# Combine all module specifications and shared resources into final specification
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save Analysis Specifications to JSON file ------------------------------------
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ranitidinecancer", "ranitidinecancerAnalysisSpecification.json")
)