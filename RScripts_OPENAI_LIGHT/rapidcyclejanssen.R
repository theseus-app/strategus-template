library(dplyr)
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(ParallelLogger)

################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script creates Strategus analysis specifications for the study:
#   rapidcyclejanssen
#
# The settings are taken from the provided Analysis Specifications JSON and
# translated to Strategus/HADES module specifications.  Detailed comments are
# included to explain how each JSON setting maps to Strategus configuration.
################################################################################

# Shared Resources -------------------------------------------------------------
# Base URL for cohort definitions & concept set resolution (Atlas WebAPI)
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ------------------------------------------------------------------------------
# Export cohort definitions from Atlas using exact cohort IDs from the
# Analysis Specifications:
#   targetCohort:     id = 1794126  (name: "target1")
#   comparatorCohort: id = 1794132  (name: "comparator1")
#   outcomeCohort(s): id = 1794131  (name: "outcome1")
#
# We set generateStats = TRUE so the CohortGeneratorModule will also compute
# cohort statistics while generating cohorts.
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

# ------------------------------------------------------------------------------
# Re-number cohorts so that internal analysis code uses compact cohort IDs:
#   1794126 -> 1  (target)
#   1794132 -> 2  (comparator)
#   1794131 -> 3  (outcome)
#
# This renumbering is used throughout the template / downstream modules.
# ------------------------------------------------------------------------------
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# ------------------------------------------------------------------------------
# Negative control concept set:
#   conceptSetId = 1888110 (name: "negative")
#
# We resolve the concept set to individual concepts and convert them to a
# negative control outcome cohort set.  We offset their cohortId by +100 so
# that cohort ids 1..n are reserved for target/comparator/outcomes, and
# negative controls start at 101, 102, ...
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
  mutate(cohortId = row_number() + 100) %>% # negative control cohort ids start at 101
  select(cohortId, cohortName, outcomeConceptId)

# Sanity check: ensure no duplicate cohort IDs across main cohorts and negatives
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# Create data frames describing the analyses -----------------------------------
# Outcomes list (oList) -------------------------------------------------------
# Use the renumbered cohort id for the outcome (cohortId == 3)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # cleanWindow corresponds to prior outcome washout/clean period for outcome
  mutate(cleanWindow = 365)

# Target and Comparator for CohortMethod analyses (cmTcList) ------------------
# We use the renumbered ids: target = 1, comparator = 2
# The Analysis Specifications specify names "target1" and "comparator1".
# The template expects targetConceptId and comparatorConceptId columns (used
# when passing excludedCovariateConceptIds).  Those are not specified in the
# Analysis Specifications so we include NA_integer_ placeholders to preserve
# the expected column names.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  targetConceptId = NA_integer_,
  comparatorConceptId = NA_integer_,
  stringsAsFactors = FALSE
)

# Excluded covariate concepts -------------------------------------------------
# The Analysis Specifications do not provide explicit concept IDs to exclude
# from the propensity score / outcome modelling.  Provide an empty data.frame
# so downstream code can safely reference excludedCovariateConcepts$conceptId.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# CohortGeneratorModule specifications ----------------------------------------
# Create the shared resource specification for cohort definitions and the
# negative control cohorts.  This makes the cohort definitions available to
# the CohortGenerator module.
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

# CohortDiagnosticsModule specifications --------------------------------------
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

# CohortMethodModule ----------------------------------------------------------
# The Analysis Specifications specify:
# - getDbCohortMethodDataArgs.studyPeriods: one entry with studyStartDate = 20210101
# - createStudyPopArgs: multiple time-at-risk definitions and other flags
# - propensity score creation and matching settings
# - outcome model fitting settings
#
# Build the objects required by the template below.

# Study periods ---------------------------------------------------------------
# From Analysis Specifications: one study period with studyStartDate = 20210101
# studyEndDate unspecified -> set to NA
studyPeriods <- tibble::tibble(
  studyStartDate = c(20210101), # YYYYMMDD
  studyEndDate   = c(NA_integer_)
)

# Time-at-risks (TARs) --------------------------------------------------------
# From Analysis Specifications: five TARs (with minDaysAtRisk = 1 for all)
timeAtRisks <- tibble::tibble(
  label = c("1-14", "1-28", "1-42", "1-90", "0-2"),
  riskWindowStart  = c(1, 1, 1, 1, 0),
  startAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd  = c(14, 28, 42, 90, 2),
  endAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start"),
  minDaysAtRisk = c(1, 1, 1, 1, 1)
)

# Propensity score settings ---------------------------------------------------
# The Analysis Specifications contain a single PS setting: match on PS with:
#   maxRatio = 0
#   caliper = 0.2
#   caliperScale = "standardized logit"
#
# We represent "match on PS" as an entry in matchOnPsArgsList.  The template
# will convert rows in this tibble to psConfigList entries.
matchOnPsArgsList <- tibble::tibble(
  label = c("match"),
  maxRatio  = c(0),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # allowed values: "propensity score", "standardized", "standardized logit"
)

# No stratify-by-PS settings provided in the Analysis Specifications -> empty
stratifyByPsArgsList <- tibble::tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0)
)

# Build a single PS configuration list (psConfigList) from the two tables
psConfigList <- list()

# Convert matchOnPsArgsList rows to psConfigList elements (method = "match")
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

# Convert stratifyByPsArgsList rows to psConfigList elements (method = "stratify")
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

# Iterate through all analysis setting combinations to create CmAnalyses ------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Depending on whether this PS config is a match or a stratify, create
      # the appropriate CohortMethod args.  The template later feeds these
      # directly into createCmAnalysis.
      if (psCfg$method == "match") {
        # Create matchOnPsArgs using CohortMethod helper.  This uses the exact
        # match parameters provided in the Analysis Specifications.
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
        stop("Unknown PS config method: ", psCfg$method)
      }

      # Covariate settings:
      # The Analysis Specifications do not enumerate specific covariates to
      # include/exclude (conceptsToInclude/Exclude are empty), so use the
      # FeatureExtraction default covariate settings.  We preserve descendants
      # in any exclusion lists by setting addDescendantsToExclude = TRUE.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build the outcome list for this analysis:
      # - Include the pre-defined study outcomes (oList) as outcomes of interest
      # - Append negative controls derived from the negativeControlOutcomeCohortSet
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

      # Build targetComparatorOutcomesList where each element specifies the
      # target/comparator pair and the outcomes to analyze.  We also pass
      # excludedCovariateConceptIds (empty in this study).
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = c(
            cmTcList$targetConceptId[i],
            cmTcList$comparatorConceptId[i],
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # getDbCohortMethodDataArgs ------------------------------------------------
      # Map the study-level restrictToCommonPeriod and study period dates into
      # the CohortMethod getDbCohortMethodData args.  The Analysis Specifications
      # specified restrictToCommonPeriod = TRUE for getDbCohortMethodDataArgs.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs -------------------------------------------------------------
      # From Analysis Specifications:
      #   maxCohortSizeForFitting = 250000
      #   errorOnHighCorrelation = TRUE
      #   prior: laplace with cross-validation
      #   control: tolerance = 2e-7, cvType = "auto", fold = 10,
      #            cvRepetitions = 10, noiseLevel = "silent",
      #            resetCoefficients = TRUE, startingVariance = 0.01
      #
      # These map to Cyclops::createPrior and Cyclops::createControl used by
      # CohortMethod::createCreatePsArgs.
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # allow Strategus to continue if one PS fit fails
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",
          exclude = c(0),
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          tolerance = 2e-7,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "silent",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Covariate balance computation args (shared and per-analysis) ----------
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs ------------------------------------------------------
      # From Analysis Specifications:
      #   modelType = "cox"
      #   stratified = FALSE
      #   useCovariates = FALSE
      #   inversePtWeighting = FALSE
      #   prior = laplace with CV
      #   control = tolerance 2e-7, cvType "auto", fold 10, cvRepetitions 10,
      #             noiseLevel "quiet", resetCoefficients TRUE,
      #             startingVariance 0.01
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
          tolerance = 2e-7,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # createStudyPopArgs -------------------------------------------------------
      # Map the createStudyPopArgs from the Analysis Specifications. The key
      # values are:
      #   restrictToCommonPeriod = FALSE
      #   firstExposureOnly = TRUE
      #   washoutPeriod = 365
      #   removeDuplicateSubjects = "remove all"
      #   censorAtNewRiskWindow = FALSE
      #   removeSubjectsWithPriorOutcome = TRUE
      #   priorOutcomeLookBack = 99999
      #   risk window values taken from timeAtRisks[t,]
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = TRUE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "remove all",
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

      # Append the CohortMethod analysis to cmAnalysisList ----------------------
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          studyStartDate,
          as.character(studyEndDate),
          timeAtRisks$label[t],
          psCfg$label
        ),
        getDbCohortMethodDataArgs = getDbCohortMethodDataArgs,
        createStudyPopArgs = createStudyPopArgs,
        createPsArgs = createPsArgs,
        matchOnPsArgs = matchOnPsArgs,
        stratifyByPsArgs = if (exists("stratifyByPsArgs")) stratifyByPsArgs else NULL,
        computeSharedCovariateBalanceArgs = computeSharedCovariateBalanceArgs,
        computeCovariateBalanceArgs = computeCovariateBalanceArgs,
        fitOutcomeModelArgs = fitOutcomeModelArgs
      )

      analysisId <- analysisId + 1
    }
  }
}

# Create CohortMethod module specifications -----------------------------------
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Assemble the full Strategus analysis specifications -------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Persist the specifications to JSON using a study-specific path.  The file
# name follows the template convention: inst/<studyName>/<studyName>AnalysisSpecification.json
outputFile <- file.path("inst", "rapidcyclejanssen", "rapidcyclejanssenAnalysisSpecification.json")
ParallelLogger::saveSettingsToJson(analysisSpecifications, outputFile)

# Informational message (non-failure)
cat("Saved Strategus analysis specification to:", outputFile, "\n")