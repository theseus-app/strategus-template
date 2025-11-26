library(dplyr)
library(Strategus)

# ------------------------------------------------------------------------------
# This script creates an analysis specification JSON for the Strategus pipeline
# using the settings provided in the Analysis Specifications.
#
# The analysis name (used for the output file path) is taken from the
# "name" field in the Analysis Specifications: "doacsandwarfarin".
#
# The template layout is based on the supplied Template; cohort and module
# settings are populated using the exact IDs/names from the Analysis
# Specifications block.
# ------------------------------------------------------------------------------

# Shared Resources -------------------------------------------------------------
# Base URL for retrieving cohort definitions and concept sets from ATLAS/WebAPI
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ------------------------------------------------------------------------------
# Cohort Definitions
# We export the cohort definitions listed in the Analysis Specifications:
#   - Target cohort id 1794126  (name: target1)
#   - Comparator cohort id 1794132 (name: comparator1)
#   - Outcome cohort id 1794131 (name: outcome1)
# We set generateStats = TRUE to get cohort statistics at export time.
# ------------------------------------------------------------------------------
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts so they are 1, 2, 3 (this simplifies downstream references)
# IMPORTANT: We use the exact cohort ids from the cohortDefinitionSet (from the webapi)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# ------------------------------------------------------------------------------
# Negative control outcomes concept set
# The Analysis Specifications lists a single concept set id to be used as
# negative control outcomes: 1888110 (name: negative)
# We resolve the concept set, fetch concepts, and convert them into a cohort
# definition-like table used by Strategus (cohortId offset by +100).
# ------------------------------------------------------------------------------
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
  mutate(cohortId = row_number() + 100) %>% # negative control cohort ids start at 101, 102, ...
  select(cohortId, cohortName, outcomeConceptId)

# Sanity check: no duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# ------------------------------------------------------------------------------
# Build lists used by the CohortMethod module
# - oList: outcomes of interest (from cohortDefinitionSet; here cohortId == 3)
# - cmTcList: target/comparator list used by cohort method analyses
# - excludedCovariateConcepts: concept ids to exclude from covariates (empty here)
# NOTE: We keep column names consistent with the Template and refer to them
#       exactly as used in the Template code (e.g., targetCohortId, targetCohortName)
# ------------------------------------------------------------------------------
# Outcomes: use cohortId == 3 (renumbered) and set priorOutcomeLookback as in spec
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Template uses cleanWindow; keep default 365

# Target and Comparator for the CohortMethod analysis
# Use the exact names from the Analysis Specifications:
# targetCohortName = "target1"; comparatorCohortName = "comparator1"
# We include targetConceptId and comparatorConceptId columns (set to NA) to match
# Template references (the Template references these columns when building the
# excluded covariates list).
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  targetConceptId = as.integer(NA),
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  comparatorConceptId = as.integer(NA),
  stringsAsFactors = FALSE
)

# For this analysis the covariate selection lists in the Analysis Specifications
# were empty. Therefore we do not exclude any covariate concepts explicitly.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# If you wanted to include only specific covariates, you would set includedCovariateConcepts
# includedCovariateConcepts <- data.frame(conceptId = c(...), conceptName = c(...))

# ------------------------------------------------------------------------------
# CohortGeneratorModule --------------------------------------------------------
# Create shared resource and module specifications for cohort generation
# ------------------------------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# The cohortDefinitionSet is added as a shared resource (so modules can find it)
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Add negative control outcome cohort shared resource
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications for generating cohorts; keep generateStats = TRUE
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ------------------------------------------------------------------------------
# CohortDiagnosticsModule Settings ---------------------------------------------
# Configure CohortDiagnostics to run a set of diagnostics used commonly by studies
# We follow the Template and enable a comprehensive set of diagnostics.
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
# CohortMethodModule -----------------------------------------------------------
# Build CohortMethod analyses using the settings from the Analysis Specifications.
#
# Key mappings from the Analysis Specifications:
# - studyPeriods: one period with studyStartDate = "20101019", studyEndDate = "20181231"
# - timeAtRisks: one TAR with start = 1 (cohort start) and end = 0 (cohort end),
#               minDaysAtRisk = 1
# - propensity score settings: match on PS with maxRatio = 1, caliper = 0.2,
#   caliperScale = "standardized logit"
# - createPsArgs: parameters as provided (maxCohortSizeForFitting, prior, control)
# - createStudyPopArgs: follow the spec (firstExposureOnly = TRUE, washout = 365, ...)
# - fitOutcomeModelArgs: modelType = "cox", stratified = FALSE, useCovariates = FALSE,
#   inversePtWeighting = FALSE, prior/control as specified
# ------------------------------------------------------------------------------
# Study period: if empty strings are used for "no restriction", but the spec
# includes dates so we populate them here.
studyPeriods <- tibble::tibble(
  studyStartDate = c("20101019"), # YYYYMMDD from spec
  studyEndDate   = c("20181231")  # YYYYMMDD from spec
)

# Time-at-risks (TARs)
timeAtRisks <- tibble::tibble(
  label = c("TAR1"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
)

# Propensity Score settings - match on PS (one configuration from spec)
matchOnPsArgsList <- tibble::tibble(
  label = c("match1"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # per spec
)

# No stratification by PS configurations specified in the Analysis Specifications
stratifyByPsArgsList <- tibble::tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c()
)

# Build the PS configuration list (combining match and stratify configs)
psConfigList <- list()

# Convert matchOnPsArgsList into psConfigList entries
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

# Convert stratifyByPsArgsList into psConfigList entries (none in this spec)
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

# Iterate through all combinations of studyPeriods x timeAtRisks x psConfigs
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Build PS adjustment objects depending on method
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
        stop("Unknown PS method in psConfigList")
      }

      # Covariate settings: use default covariates and ensure descendants can be
      # excluded when building covariate sets (this mirrors the Template behavior)
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build the outcome list:
      # - outcomes from the cohortDefinitionSet (true outcomes)
      # - negative controls from the negativeControlOutcomeCohortSet (trueEffectSize = 1)
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

      # Build targetComparatorOutcomesList for all target/comparator pairs
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        # The Template expects excludedCovariateConceptIds to include:
        # - cmTcList$targetConceptId and $comparatorConceptId (may be NA)
        # - excludedCovariateConcepts$conceptId (empty here)
        # We therefore concatenate and drop NA values below.
        excludedIds <- c(
          cmTcList$targetConceptId[i],
          cmTcList$comparatorConceptId[i],
          excludedCovariateConcepts$conceptId
        )
        # Remove NAs from the excluded ids
        excludedIds <- excludedIds[!is.na(excludedIds)]

        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedIds
        )
      }

      # getDbCohortMethodDataArgs: build args for creating CohortMethod data
      # We follow the Template default for restrictToCommonPeriod (TRUE) and
      # populate the studyStartDate/studyEndDate per the Analysis Specifications.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs: configured using the Analysis Specifications values
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # allow Strategus to continue other analyses if PS fitting fails
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",
          exclude = c(0),
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",
          cvType = "auto",
          fold = 10,
          seed = 1,
          resetCoefficients = TRUE,
          tolerance = 2e-07,
          cvRepetitions = 10,
          startingVariance = 0.01
        )
      )

      # Covariate balance computation args used in the Template/standard pipeline
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs: use settings from the Analysis Specifications.
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = FALSE,                 # per spec
        useCovariates = FALSE,              # per spec
        inversePtWeighting = FALSE,         # per spec
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

      # createStudyPopArgs: build study population args using the Analysis Specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,                 # per spec
        firstExposureOnly = TRUE,                       # per spec
        washoutPeriod = 365,                            # per spec
        removeDuplicateSubjects = "keep all",           # per spec
        censorAtNewRiskWindow = FALSE,                  # per spec
        removeSubjectsWithPriorOutcome = TRUE,          # per spec
        priorOutcomeLookback = 99999,                   # per spec
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Append the settings to the CohortMethod analysis list
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

# Create the CohortMethod module specifications using the analyses built above.
# We include targetComparatorOutcomesList and set refit options to FALSE (Template defaults).
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
# Create the overall analysis specifications object and add shared resources
# and module specifications in the order the Template expects.
# ------------------------------------------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the JSON file to inst/<analysisName>/<analysisName>AnalysisSpecification.json
# Use the exact analysis name from the Analysis Specifications: "doacsandwarfarin"
outDir <- file.path("inst", "doacsandwarfarin")
if (!dir.exists(outDir)) dir.create(outDir, recursive = TRUE)

ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path(outDir, "doacsandwarfarinAnalysisSpecification.json")
)