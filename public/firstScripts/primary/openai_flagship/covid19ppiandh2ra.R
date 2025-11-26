################################################################################
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# further details.
################################################################################

# Load packages used across the specification construction:
# - Strategus for orchestrating analysis specs and modules
# - ROhdsiWebApi to fetch cohorts and concept sets from ATLAS/WebAPI
# - dplyr / tibble for data wrangling
# - CohortMethod / FeatureExtraction / Cyclops for module settings objects
# - ParallelLogger for saving the resulting specification as JSON
library(dplyr)
library(tibble)
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(ParallelLogger)

# Study name (used in output file path); matches the "name" in the Analysis Specifications
studyName <- "covid19ppiandh2ra"

# Shared Resources -------------------------------------------------------------
# Define the ATLAS/WebAPI base URL for retrieving cohort and concept set definitions
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export the cohort definition set for the study:
# - Target: 1794126 (name: "target1")
# - Comparator: 1794132 (name: "comparator1")
# - Outcome(s): 1794131 (name: "outcome1")
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Renumber cohorts to a compact index used consistently throughout the modules:
# 1 = Target, 2 = Comparator, 3 = Outcome
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# Retrieve the concept set used to define negative control outcomes.
# The specification indicates: negativeControlConceptSet id = 1888110, name = "negative".
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
  # Standardize to columns expected by the Strategus NegativeControlOutcomeCohort shared resource:
  rename(
    outcomeConceptId = "conceptId",
    cohortName = "conceptName"
  ) %>%
  # Assign unique cohortIds for the negative controls that do not overlap with 1,2,3
  mutate(cohortId = dplyr::row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure cohort identifiers for negative controls don't overlap
# with target/comparator/outcome cohorts (1,2,3).
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found across primary cohorts and negative control cohorts ***")
}

# Create lightweight frames for outcomes and T-C pairs used in CohortMethod ----
# Outcomes list (only the main outcome here; additional negative controls are added later):
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # Optionally define a clean window (not directly used by CM, but illustrative)
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate selection (from Analysis Specifications) ---------------------------
# The Analysis Specifications specify empty include/exclude concept sets:
# "conceptsToInclude": [{"id": null, "name": ""}]
# "conceptsToExclude": [{"id": null, "name": ""}]
# We'll translate these to empty vectors for FE configuration and optional TCO exclusion list.

# Concepts to explicitly include as covariates (none specified)
includedCovariateConceptIds <- c()
includedCovariateConcepts <- tibble::tibble(
  conceptId = integer(),
  conceptName = character()
)

# Concepts to explicitly exclude as covariates (none specified)
excludedCovariateConceptIds <- c()
excludedCovariateConcepts <- tibble::tibble(
  conceptId = integer(),
  conceptName = character()
)

# Optional: Also exclude "drugs of interest" (the exposure concepts) to avoid
# collinearity, if known. Not provided in the specifications, so leaving empty.
# excludedCovariateConcepts <- tibble::tibble(
#   conceptId = c(2345678, 3456789),
#   conceptName = c("target concept name", "comparator concept name")
# )

# CohortGeneratorModule --------------------------------------------------------
# This module will instantiate cohorts and negative control cohorts as shared resources.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Shared resource for negative control outcomes (resolved from the concept set)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",           # First occurrence of each negative control outcome
  detectOnDescendants = TRUE          # Use descendant concepts for detection
)

# Module specification: ask the generator to compute cohort statistics
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# Run a standard set of cohort diagnostics on all cohorts we've defined (1: Target, 2: Comparator, 3: Outcome)
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
# Apply the study-specific settings from the Analysis Specifications.

# Study period(s): restricts data extraction (createGetDbCohortMethodDataArgs) to this window.
# From "getDbCohortMethodDataArgs.studyPeriods": 20200101 - 20200515
studyPeriods <- tibble::tibble(
  studyStartDate = c("20200101"),  # YYYYMMDD
  studyEndDate   = c("20200515")   # YYYYMMDD
)

# Time-at-risk(s): used when creating the study population (createCreateStudyPopulationArgs)
# From "createStudyPopArgs.timeAtRisks": 1 day after cohort start up to 99999 days, anchored on cohort start.
timeAtRisks <- tibble::tibble(
  label = c("TAR1: 1 to 99999 days after cohort start"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),  # "cohort start" | "cohort end"
  riskWindowEnd  = c(99999),
  endAnchor = c("cohort start")     # "cohort start" | "cohort end"
)

# Propensity Score settings - match on PS (none specified in the Analysis Specifications)
matchOnPsArgsList <- tibble::tibble(
  label = character(),
  maxRatio  = numeric(),
  caliper = numeric(),
  caliperScale  = character()       # "propensity score" | "standardized" | "standardized logit"
)

# Propensity Score settings - stratify by PS (from Analysis Specifications):
# numberOfStrata = 5, baseSelection = "all"
stratifyByPsArgsList <- tibble::tibble(
  label = c("StratifyByPS_5Strata_BaseAll"),
  numberOfStrata  = c(5),
  baseSelection = c("all")          # "all" | "target" | "comparator"
)

# Build a combined PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# If a data frame for "match on PS" exists and has rows, convert each row to a config
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

# If a data frame for "stratify by PS" exists and has rows, convert each row to a config
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

# Iterate through all combinations (study period x TAR x PS config) to build CM analyses
cmAnalysisList <- list()
targetComparatorOutcomesList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Translate PS adjustment choice into CohortMethod args
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
        stop("Unknown PS configuration method encountered.")
      }

      # FeatureExtraction default covariates; apply include/exclude concept lists (empty per specs)
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE,
        includedCovariateConceptIds = includedCovariateConceptIds,
        excludedCovariateConceptIds = excludedCovariateConceptIds
      )

      # Build the outcomes list: primary outcome(s) + negative control outcomes
      outcomeList <- append(
        # Primary outcome(s) of interest
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        # Negative control outcomes (outcomeOfInterest = FALSE)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Define the TCOs (Target/Comparator with the list of outcomes):
      # We can exclude covariate concepts here to avoid adjusting for the exposures themselves.
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = c(
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # Data-extraction settings (from getDbCohortMethodDataArgs in specs)
      # - Restrict the extraction to the study window (20200101-20200515)
      # - Use default maximum cohort size (0 = no limit)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # PS model settings (from propensityScoreAdjustment.createPsArgs in specs)
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
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

      # Covariate balance computation settings (keep defaults but demonstrate both shared and per-outcome balance)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model settings (from fitOutcomeModelArgs in specs)
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
          tolerance = 2e-07,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Study population creation settings (from createStudyPopArgs in specs)
      # Note: the TAR fields below are filled from timeAtRisks[t, ] defined earlier.
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
        minDaysAtRisk = 1
      )

      # Assemble the analysis configuration for this (period, TAR, PS) combination
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

# Create the CohortMethod module specification holding all analyses and TCO sets
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
# Compose the full Strategus analysis specification object:
# 1) Add shared resources (cohorts + negative control cohort definitions)
# 2) Add module specifications (CohortGenerator, CohortDiagnostics, CohortMethod)
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the specification to JSON for execution by Strategus
outputDir <- file.path("inst", studyName)
if (!dir.exists(outputDir)) {
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
}

ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path(outputDir, paste0(studyName, "AnalysisSpecification.json"))
)