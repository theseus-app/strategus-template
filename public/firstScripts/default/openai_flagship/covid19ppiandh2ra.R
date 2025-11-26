################################################################################
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# further details.
################################################################################

# Load required packages -------------------------------------------------------
# dplyr/tibble for data wrangling, Strategus for orchestrating modules,
# ROhdsiWebApi for retrieving cohort/concept sets from ATLAS/WebAPI,
# CohortMethod/FeatureExtraction/Cyclops for CM settings,
# ParallelLogger for saving the final analysis specification JSON.
library(dplyr)
library(tibble)
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(ParallelLogger)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from the specified WebAPI (ATLAS) instance.
# These cohort IDs and names must match EXACTLY those in the Analysis Specifications.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Retrieve the target, comparator, and outcome cohorts by their WebAPI IDs.
# Analysis Specifications mapping:
#   targetCohort id: 1794126 (name: "target1")
#   comparatorCohort id: 1794132 (name: "comparator1")
#   outcomeCohort id: 1794131 (name: "outcome1")
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to 1 (T), 2 (C), 3 (O) for downstream CohortMethod use.
# This leaves source cohort names intact while setting standardized internal IDs.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# The Analysis Specifications define a negative control concept set:
#   negativeControlConceptSet id: 1888110 (name: "negative")
# We resolve this concept set and convert it into a negative control outcome cohort set
# used by CohortMethod for diagnostics/empirical calibration.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
  ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  # Assign cohortIds that do not collide with T/C/O (which are 1,2,3).
  mutate(cohortId = dplyr::row_number() + 100L) %>% # 101, 102, ...
  select(cohortId, cohortName, outcomeConceptId)

# Sanity check: ensure no duplicate cohort IDs across T/C/O and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found across primary and negative control cohorts ***")
}

# Create supporting data frames for downstream modules -------------------------
# Outcomes list (primary outcomes only; negative controls are handled separately)
# Here we map internal cohortId=3 to an outcome entry with a default cleanWindow.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId,
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Names are taken EXACTLY from the Analysis Specifications.
cmTcList <- tibble::tibble(
  targetCohortId = 1L,
  targetCohortName = "target1",
  comparatorCohortId = 2L,
  comparatorCohortName = "comparator1"
)

# Optional: exclude specific covariate concepts (e.g., drugs of interest).
# Analysis Specifications provided empty include/exclude sets, so we leave this empty.
excludedCovariateConcepts <- tibble::tibble(
  conceptId = integer(),
  conceptName = character()
)

# CohortGeneratorModule --------------------------------------------------------
# Shared resources to:
#  - Instantiate the cohort definitions in a database (CohortDefinition shared resource)
#  - Create negative control outcome cohort set (NegativeControlOutcomeCohort shared resource)
cgModuleSettingsCreator <- CohortGeneratorModule$new()

cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",         # First occurrence of negative control outcomes
  detectOnDescendants = TRUE        # Include descendants
)

# Basic cohort generation behavior; generateStats = TRUE will capture cohort stats
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# Typical diagnostics are enabled to evaluate cohorts prior to effect estimation.
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
# Study Periods (from Analysis Specifications: 20200101 to 20200515)
# NOTE: These are applied at the data-extraction stage (getDbCohortMethodDataArgs).
studyPeriods <- tibble::tibble(
  studyStartDate = c("20200101"),
  studyEndDate   = c("20200515")
)

# Time-at-risk (TAR) definitions (from Analysis Specifications):
# One TAR:
#  - riskWindowStart = 1, startAnchor = "cohort start"
#  - riskWindowEnd = 99999, endAnchor = "cohort start"
#  - minDaysAtRisk = 1 (applied in createStudyPopulation)
timeAtRisks <- tibble::tibble(
  label = c("TAR1"),
  riskWindowStart = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(99999),
  endAnchor = c("cohort start")
)

# Propensity Score (PS) adjustment strategies (from Analysis Specifications):
# Three PS configurations:
#  1) No PS adjustment (neither match nor stratify)
#  2) Matching with maxRatio=4, caliper=0.2, caliperScale="standardized logit"
#  3) Stratification by PS with numberOfStrata=5, baseSelection="all"
matchOnPsArgsList <- tibble::tibble(
  label = c("Match"),
  maxRatio = c(4),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

stratifyByPsArgsList <- tibble::tibble(
  label = c("Stratify"),
  numberOfStrata = c(5),
  baseSelection = c("all")
)

# Build a combined PS configuration list
psConfigList <- list(
  list(method = "none", label = "No PS", params = list())
)

# Convert "match on PS" rows to configurations
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

# Convert "stratify by PS" rows to configurations
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

# Covariate settings (FeatureExtraction)
# Analysis Specifications provided empty include/exclude lists, so we use defaults.
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE
)

# Construct the outcomes list for CohortMethod:
#  - Primary outcome(s) flagged as outcomeOfInterest = TRUE
#  - Negative controls flagged as outcomeOfInterest = FALSE
#    and trueEffectSize = 1 for diagnostics/empirical calibration work
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

# Set Target-Comparator-Outcomes (TCO) combinations to analyze.
# Exclude covariate concept IDs list is empty per Analysis Specifications.
targetComparatorOutcomesList <- list()
for (i in seq_len(nrow(cmTcList))) {
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
  )
}

# Create argument sets for CohortMethod based on Analysis Specifications -------
# getDbCohortMethodDataArgs:
#   - studyStartDate/studyEndDate: 20200101 to 20200515
#   - maxCohortSize: 0
#   - restrictToCommonPeriod: FALSE
#   - covariateSettings: default (as above)
# Note: Some fields present in Analysis Specifications (e.g., firstExposureOnly, washoutPeriod)
#       are createStudyPopulation settings rather than data-extraction settings,
#       and are applied below in createStudyPopArgs.
createGetDbArgs <- function(studyStartDate, studyEndDate) {
  CohortMethod::createGetDbCohortMethodDataArgs(
    restrictToCommonPeriod = FALSE,    # From Analysis Specifications
    studyStartDate = studyStartDate,
    studyEndDate = studyEndDate,
    maxCohortSize = 0,                 # From Analysis Specifications
    covariateSettings = covariateSettings
  )
}

# createPsArgs from Analysis Specifications
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

# Fit outcome model args from Analysis Specifications
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

# Covariate balance computation settings (general defaults)
computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
  maxCohortSize = 250000,
  covariateFilter = NULL
)
computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
  maxCohortSize = 250000,
  covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
)

# Iterate through all analysis setting combinations ----------------------------
cmAnalysisList <- list()
analysisId <- 1L

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # PS adjustment configurations
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
        # No PS adjustment (unadjusted analysis)
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- NULL
      }

      # Create the study population arguments (from Analysis Specifications)
      #   restrictToCommonPeriod: FALSE
      #   firstExposureOnly: FALSE
      #   washoutPeriod: 0
      #   removeDuplicateSubjects: "keep all"
      #   censorAtNewRiskWindow: FALSE
      #   removeSubjectsWithPriorOutcome: FALSE
      #   priorOutcomeLookBack: 99999
      #   TAR: as defined above for the selected t
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = FALSE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1
      )

      # Create the data extraction args for this study period
      getDbCohortMethodDataArgs <- createGetDbArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate
      )

      # Append the settings to Analysis List
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
      analysisId <- analysisId + 1L
    }
  }
}

# Finalize CohortMethod module specifications ----------------------------------
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
# Bundle all shared resources and module specifications into a single
# Strategus analysis specification object, then save to JSON.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications JSON to a study-named folder/file.
studyName <- "covid19ppiandh2ra"
outputDir <- file.path("inst", studyName)
if (!dir.exists(outputDir)) {
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
}
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path(outputDir, paste0(studyName, "AnalysisSpecification.json"))
)