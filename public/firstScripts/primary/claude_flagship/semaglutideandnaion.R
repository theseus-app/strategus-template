################################################################################
# OHDSI Strategus Analysis Specification for semaglutideandnaion Study
# 
# This script creates analysis specifications for a comparative cohort study
# examining the association between semaglutide (target) and NAION (outcome)
# compared to a comparator cohort.
#
# Analysis includes:
# - Target cohort: 1794126 (target1)
# - Comparator cohort: 1794132 (comparator1)
# - Outcome cohort: 1794131 (outcome1)
# - Negative controls: Concept set 1888110
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Configure the base URL for your ATLAS instance
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export cohort definitions from ATLAS using the cohort IDs specified in the
# analysis specifications:
# - 1794126: Target cohort (target1)
# - 1794132: Comparator cohort (comparator1)
# - 1794131: Outcome cohort (outcome1)
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
# This standardization simplifies referencing cohorts in downstream analyses
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative Control Outcomes ----------------------------------------------------
# Retrieve negative control outcomes from the concept set specified in the
# analysis specifications (ID: 1888110, name: negative)
# These outcomes are used to calibrate empirical p-values and confidence intervals
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
  # Assign cohort IDs starting at 101 to avoid conflicts with primary cohorts (1, 2, 3)
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Validate that there are no duplicate cohort IDs between primary and negative control cohorts
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create Data Frames for Analysis Cohorts -------------------------------------

# Outcomes: Define the outcome cohort (cohortId = 3, outcome1)
# cleanWindow = 365 days: subjects with outcome in this period before cohort start are excluded
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for CohortMethod Analysis
# This defines the comparison: cohortId 1 (target1) vs cohortId 2 (comparator1)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Excluded Covariate Concepts --------------------------------------------------
# Since covariateSelection.conceptsToExclude has null/empty values in the specifications,
# we create an empty data frame. If specific concept IDs were provided, they would be
# listed here to exclude them from the propensity score model
excludedCovariateConcepts <- data.frame(
  conceptId = character(0),
  conceptName = character(0)
)

# Note: covariateSelection.conceptsToInclude is also null/empty, so we use
# default covariate settings (all available covariates will be included)

# CohortGeneratorModule --------------------------------------------------------
# This module generates the cohorts defined above in the target database
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for primary cohorts
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource specifications for negative control outcome cohorts
# occurrenceType = "first": only the first occurrence of each negative control outcome is considered
# detectOnDescendants = TRUE: include descendant concepts when identifying outcomes
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications with generateStats = TRUE to produce cohort characterization
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# This module performs diagnostic checks on cohort definitions to assess quality
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

# Study Period -----------------------------------------------------------------
# Define the study time window from analysis specifications
# studyStartDate: 20171201 (December 1, 2017)
# studyEndDate: 20231231 (December 31, 2023)
# Note: maxCohortSize = 0 means no limit on cohort size
studyPeriods <- tibble(
  studyStartDate = c("20171201"),
  studyEndDate   = c("20231231")
)

# Time-at-Risk (TAR) Definitions -----------------------------------------------
# Define the risk window for outcome assessment based on specifications:
# - riskWindowStart = 1 day after cohort start
# - riskWindowEnd = 0 days relative to cohort end (end on cohort end date)
# - minDaysAtRisk = 1 day minimum follow-up required
timeAtRisks <- tibble(
  label = c("OnTreatment"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
)

# Propensity Score Settings: Match on PS ---------------------------------------
# Configuration for matching on propensity score from specifications:
# - maxRatio = 1: 1:1 matching
# - caliper = 0.2: maximum allowed difference in propensity scores
# - caliperScale = "standardized logit": caliper scale on standardized logit of PS
matchOnPsArgsList <- tibble(
  label = c("1:1 matching with 0.2 caliper on standardized logit"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# Propensity Score Settings: Stratify by PS -----------------------------------
# The specifications have stratifyByPsArgs = null, so we create an empty tibble
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0)
)

# Build PS Configuration List --------------------------------------------------
# This list consolidates all PS adjustment methods (match and/or stratify)
# Each configuration will generate a separate analysis
psConfigList <- list()

# Add "match on PS" configurations if specified
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

# Add "stratify by PS" configurations if specified
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

# Create CohortMethod Analysis List --------------------------------------------
# Iterate through all combinations of study periods, time-at-risk windows,
# and PS adjustment methods to create comprehensive analysis specifications
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment method based on the configuration type
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

      # Covariate Settings -------------------------------------------------------
      # Use default covariate settings since conceptsToInclude is null/empty
      # addDescendantsToExclude = TRUE ensures descendant concepts are also excluded
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcome List -------------------------------------------------------------
      # Create outcome definitions for both outcomes of interest and negative controls
      # Outcomes of interest: outcomeOfInterest = TRUE, from oList
      # Negative controls: outcomeOfInterest = FALSE, trueEffectSize = 1 (null effect)
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
      
      # Target-Comparator-Outcomes List ------------------------------------------
      # Combine target, comparator, and outcomes for each comparison
      # excludedCovariateConceptIds: concepts to exclude from PS model
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = if(nrow(excludedCovariateConcepts) > 0) {
            excludedCovariateConcepts$conceptId
          } else {
            c()
          }
        )
      }

      # GetDbCohortMethodData Arguments ------------------------------------------
      # Define how to extract data from the database
      # From specifications:
      # - restrictToCommonPeriod = FALSE (use different periods for T and C if needed)
      # - studyStartDate and studyEndDate define the observation window
      # - maxCohortSize = 0 (no restriction on cohort size)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # CreatePs Arguments -------------------------------------------------------
      # Propensity score model settings from specifications:
      # - maxCohortSizeForFitting = 250000
      # - errorOnHighCorrelation = TRUE
      # - prior: Laplace with cross-validation (useCrossValidation = TRUE)
      # - control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      #   noiseLevel = "silent", resetCoefficients = TRUE, startingVariance = 0.01
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

      # ComputeCovariateBalance Arguments ----------------------------------------
      # Settings for computing covariate balance diagnostics
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # FitOutcomeModel Arguments ------------------------------------------------
      # Outcome model settings from specifications:
      # - modelType = "cox" (Cox proportional hazards)
      # - stratified = FALSE (not stratified on matched sets)
      # - useCovariates = FALSE (no covariates in outcome model)
      # - inversePtWeighting = FALSE
      # - prior: Laplace with cross-validation
      # - control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      #   noiseLevel = "quiet", resetCoefficients = TRUE, startingVariance = 0.01
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
          noiseLevel = "quiet"
        )
      )

      # CreateStudyPopulation Arguments ------------------------------------------
      # Study population settings from specifications:
      # - restrictToCommonPeriod = FALSE
      # - firstExposureOnly = FALSE (allow multiple exposures per person)
      # - washoutPeriod = 365 days
      # - removeDuplicateSubjects = "keep all"
      # - censorAtNewRiskWindow = FALSE
      # - removeSubjectsWithPriorOutcome = TRUE
      # - priorOutcomeLookBack = 99999 days (essentially all prior history)
      # - Risk window: start = 1 day after cohort start, end = 0 days from cohort end
      # - minDaysAtRisk = 1
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
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

      # Append Complete Analysis to List ----------------------------------------
      # Each analysis combines all the settings defined above with a unique ID
      # and descriptive label
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

# Create CohortMethod Module Specifications ------------------------------------
# Bundle all CohortMethod analyses into module specifications
# - refitPsForEveryOutcome = FALSE: reuse PS across outcomes
# - refitPsForEveryStudyPopulation = FALSE: reuse PS across study populations
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the Final Analysis Specifications ------------------------------------
# Combine all modules into a complete Strategus analysis specification
# The order matters: shared resources must be added before modules that use them
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save Analysis Specifications to JSON -----------------------------------------
# Save the complete specification to a JSON file for execution
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "semaglutideandnaion", "semaglutideandnaionAnalysisSpecification.json")
)