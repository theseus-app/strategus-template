################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script builds a Strategus analysis specification using the OHDSI HADES
# modules based on the provided Analysis Specifications and the structure
# demonstrated in the Template. Detailed annotations are provided throughout.
#
# More information about Strategus HADES modules:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

library(dplyr)
library(Strategus)

# ------------------------------------------------------------------------------
# Shared Resources
# ------------------------------------------------------------------------------

# 1) Connect to your WebAPI (used to download cohort and negative control
#    definitions). Adjust baseUrl as needed for your environment.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# 2) Cohort Definitions
#    - We download the Target, Comparator, and Outcome definitions by ID.
#    - Then, we renumber them so they have compact internal IDs (1, 2, 3).
#    - These IDs are only used internally within this Strategus specification.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number internal cohort IDs for Target/Comparator/Outcome
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# 3) Negative control outcomes
#    - Download a concept set that defines candidate negative outcomes.
#    - Resolve the concept set (includes descendants and excludes) to individual concepts.
#    - Convert those concepts into a table with 'outcomeConceptId' and 'cohortName'.
#    - Assign unique cohortIds starting from 101 to avoid collision with 1,2,3...
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # negative
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
  ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
  rename(
    outcomeConceptId = "conceptId",
    cohortName = "conceptName"
  ) %>%
  mutate(cohortId = dplyr::row_number() + 100) %>%  # negative controls: 101, 102, ...
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no duplicate cohort IDs across primary cohorts and negatives
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found across primary and negative control sets ***")
}

# 4) Create simple helper data frames naming the cohorts used in analyses -------

# Outcomes (primary), we set a 'cleanWindow' (not strictly required by CM but
# often used downstream for diagnostics or conventions).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(
    outcomeCohortId = cohortId,
    outcomeCohortName = cohortName
  ) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator mapping (IDs and names must match the ones renumbered above)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate concept selection:
# Analysis Specifications indicated no specific concepts to include/exclude.
# We leave these empty, which means default covariates will be used.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)
# Optional: includedCovariateConcepts <- data.frame(conceptId = integer(0), conceptName = character(0))

# ------------------------------------------------------------------------------
# CohortGeneratorModule
#   - Generates cohorts and negative control outcome cohorts (from concepts)
#   - Shared resources: the cohorts and negative controls used by downstream modules
# ------------------------------------------------------------------------------

cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource for primary cohorts
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

# Shared resource for negative control cohorts (derived from concepts)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications: instruct CohortGenerator to compute cohort statistics
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ------------------------------------------------------------------------------
# CohortDiagnosticsModule
#   - Runs standard diagnostics on generated cohorts
# ------------------------------------------------------------------------------

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

# ------------------------------------------------------------------------------
# CohortMethodModule
#   - Parameterize study periods, risk windows, PS settings, and model-fitting
#   - This section maps all CohortMethod settings from the Analysis Specifications
# ------------------------------------------------------------------------------

# Study period(s) from Analysis Specifications:
#   20171201 to 20231231
studyPeriods <- tibble::tibble(
  studyStartDate = c(20171201),
  studyEndDate   = c(20231231)
)

# Time-at-risk(s) from Analysis Specifications:
#   One TAR: start = 1 day after cohort start, end = cohort end, minDaysAtRisk = 1
timeAtRisks <- tibble::tibble(
  label = c("TAR_1"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"), # allowed: "cohort start" | "cohort end"
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end"),     # allowed: "cohort start" | "cohort end"
  minDaysAtRisk = c(1)
)

# Propensity Score adjustment settings (from Analysis Specifications):
# Match on PS: maxRatio = 1 (1:1 matching), caliper = 0.2 (on standardized logit)
matchOnPsArgsList <- tibble::tibble(
  label = c("match 1:1 caliper 0.2 std logit"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # "propensity score" | "standardized" | "standardized logit"
)

# No stratify-by-PS configuration is specified in the Analysis Specifications
stratifyByPsArgsList <- tibble::tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0) # "all" | "target" | "comparator"
)

# Build a unified PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Convert match-on-PS rows into configuration entries
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

# Convert stratify-by-PS rows into configuration entries (none specified)
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

# Iterate through all combinations of study periods, TARs, and PS configurations
cmAnalysisList <- list()
targetComparatorOutcomesList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # PS matching or stratification arguments
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

      # Covariate settings: default covariates, no included/excluded concepts provided
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcomes: primary outcome(s) plus negative controls
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999  # from Analysis Specifications
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

      # Targets and comparators with excluded covariate concept IDs (none provided)
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Data extraction settings (GetDbCohortMethodData)
      # - study start and end dates from Analysis Specifications
      # - maxCohortSize = 0 means do not restrict
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # PS creation settings (from Analysis Specifications)
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # Allow progression even if PS fails to fit in some strata/outcomes
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          tolerance = 2e-07,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "silent",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Covariate balance settings (shared and filtered for Table 1)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model fitting settings (from Analysis Specifications)
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = FALSE,            # As specified (note: TRUE is often used after matching)
        useCovariates = FALSE,
        inversePtWeighting = FALSE,
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          tolerance = 2e-07,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Study population settings (from Analysis Specifications)
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
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999
      )

      # Assemble this analysis configuration
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

# Instantiate the CohortMethod module with all analysis configurations
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# ------------------------------------------------------------------------------
# Create and Save the full Strategus Analysis Specifications
# ------------------------------------------------------------------------------

analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Use the provided 'name' from Analysis Specifications for output path
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "semaglutideandnaion", "semaglutideandnaionAnalysisSpecification.json")
)