library(dplyr)
library(Strategus)

# This script creates Strategus analysis specifications for the analysis defined
# in the <Analysis Specifications> JSON provided to the generator.
#
# Detailed inline comments are provided to explain how each setting from the
# Analysis Specifications is translated into Strategus / HADES module settings.
#
# IMPORTANT:
# - Cohort IDs and names are taken EXACTLY from the Analysis Specifications.
# - Do not change the cohort ids below unless you intend to change the study.
#
# Study name (used for output file path)
studyName <- "tramadolcodein"

# ------------------------------------------------------------------------------
# Shared Resources -------------------------------------------------------------
# ------------------------------------------------------------------------------
# WebAPI base URL used to export the cohort definitions and concept sets.
# Change this to your Atlas/WebAPI endpoint if needed.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# The Analysis Specifications lists three cohort definitions:
# - target cohort id 1794126, name "target1"
# - comparator cohort id 1794132, name "comparator1"
# - outcome cohort id 1794131, name "outcome1"
#
# We export these cohorts from the WebAPI as the cohortDefinitionSet which will be
# included as a shared resource for the CohortGenerator / downstream modules.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts so that target == 1, comparator == 2, outcome == 3
# This numbering convention is used downstream in the CohortMethod configuration.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes are defined as a concept set (id provided in the
# Analysis Specifications: 1888110). We export the concept set, resolve it,
# get the matching concepts, and convert them into a cohort-like table. Each
# negative control is assigned a cohortId starting at 101 (so as not to
# conflict with the main cohorts which start at 1,2,3...).
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
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Sanity check: ensure no duplicate cohort ids across core cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# ------------------------------------------------------------------------------
# Define lists used for CohortMethod analyses ----------------------------------
# ------------------------------------------------------------------------------

# Outcomes: create oList from cohortDefinitionSet: the outcome cohort was renumbered to id == 3
# We set the cleanWindow (prior outcome lookback used for some diagnostics) to 365 days
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target/Comparator list for CohortMethod analyses.
# Use EXACT cohort names from Analysis Specifications.
# The template expects columns: targetCohortId, targetCohortName, comparatorCohortId, comparatorCohortName.
# Additionally, downstream code references targetConceptId and comparatorConceptId when excluding
# covariates specific to the exposures. The Analysis Specifications did not specify explicit
# concept ids for the exposure concepts, so we include NA_integer_ entries to preserve the columns.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  targetConceptId = NA_integer_,    # no explicit exposure concept id supplied in spec
  comparatorConceptId = NA_integer_ # no explicit exposure concept id supplied in spec
)

# Excluded covariate concepts: Analysis Specifications has no concepts to exclude.
# We therefore create an empty data frame with the expected structure. This will
# be passed into the CohortMethod target/comparator excluded covariates list.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# CohortGenerator Module Shared Resources & Specifications ---------------------
# ------------------------------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create the shared resource specification for the exported cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Create negative control shared resources (we use 'first' occurrence and detect descendants)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specification to generate cohorts (we keep generateStats = TRUE as above)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ------------------------------------------------------------------------------
# CohortDiagnostics Module Specifications -------------------------------------
# ------------------------------------------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Use the cohort ids present in cohortDefinitionSet
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
# CohortMethod Module ---------------------------------------------------------
# ------------------------------------------------------------------------------
# The Analysis Specifications provides:
# - getDbCohortMethodDataArgs: studyPeriods (null) and maxCohortSize 0
# - createStudyPopArgs: detailed settings (two time-at-risk definitions)
# - propensity score settings: match on PS (maxRatio=1, caliper=0.2, caliperScale="standardized logit")
# - createPsArgs: cyclops prior/control settings
# - fitOutcomeModelArgs: cox model, no covariates, laplace prior + controls
#
# We translate these into the components used to build CohortMethod analyses.

# Study periods: the specification provided a single study period with null start/end,
# which we interpret as "no study period restriction". To follow the template structure
# used to loop over study periods, we create an empty tibble (i.e., no restriction).
studyPeriods <- tibble::tibble(
  studyStartDate = c(), # empty -> no restriction
  studyEndDate   = c()
)

# Time-at-risks (TARs) are explicitly specified in the Analysis Specifications:
# 1) riskWindowStart = 0, startAnchor = "cohort start", riskWindowEnd = 0, endAnchor = "cohort end", minDaysAtRisk = 1
# 2) riskWindowStart = 0, startAnchor = "cohort start", riskWindowEnd = 9999, endAnchor = "cohort start", minDaysAtRisk = 1
# We create two labeled TARs so labels can be used in descriptions.
timeAtRisks <- tibble::tibble(
  label = c("TAR_cohort_start_to_cohort_end", "TAR_cohort_start_to_cohort_start"),
  riskWindowStart = c(0, 0),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd = c(0, 9999),
  endAnchor = c("cohort end", "cohort start"),
  minDaysAtRisk = c(1, 1)
)

# Propensity Score - matchOnPs configuration from Analysis Specifications:
# The spec contains a single PS setting with matchOnPsArgs:
# maxRatio = 1, caliper = 0.2, caliperScale = "standardized logit"
matchOnPsArgsList <- tibble::tibble(
  label = c("match_on_ps"),
  maxRatio = c(1),
  caliper = c(0.2),
  caliperScale = c("standardized logit") # allowed: "propensity score" | "standardized" | "standardized logit"
)

# No stratifyByPs settings provided in the spec => create an empty tibble
stratifyByPsArgsList <- tibble::tibble(
  label = character(0),
  numberOfStrata = integer(0),
  baseSelection = character(0)
)

# Build psConfigList by reading the matchOnPsArgsList and stratifyByPsArgsList rows
psConfigList <- list()
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

# Build CohortMethod analyses (one per combination of studyPeriod x timeAtRisk x psConfig)
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(max(1, nrow(studyPeriods)))) {
  # If studyPeriods is empty, we'll use NA/NULL dates to indicate no restriction
  if (nrow(studyPeriods) == 0) {
    studyStartDate <- NULL
    studyEndDate <- NULL
  } else {
    studyStartDate <- studyPeriods$studyStartDate[s]
    studyEndDate <- studyPeriods$studyEndDate[s]
  }

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create the matchOnPsArgs / stratifyByPsArgs based on method
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

      # Covariate settings: by default use the FeatureExtraction defaults.
      # The Analysis Specifications included empty 'conceptsToInclude' and 'conceptsToExclude',
      # which we interpret as "use default covariates" (no explicit include/exclude beyond
      # the exposure-related excludes handled in targetComparatorOutcomesList).
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build the outcomeList that includes the outcome(s) of interest from the cohortDefinitionSet
      # (these are the 'outcome' cohorts, marked as outcomeOfInterest = TRUE), and the negative controls
      # which are included as outcomes with trueEffectSize = 1 (null).
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 365
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

      # Build targetComparatorOutcomesList: one entry per row in cmTcList
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

      # getDbCohortMethodDataArgs: per Analysis Specifications:
      # studyPeriods are null -> no restriction; maxCohortSize = 0
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = if (is.null(studyStartDate)) NULL else studyStartDate,
        studyEndDate = if (is.null(studyEndDate)) NULL else studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs: follow the Analysis Specifications fields
      # - maxCohortSizeForFitting = 250000
      # - errorOnHighCorrelation = TRUE
      # - prior: laplace, useCrossValidation = TRUE
      # - control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      #            noiseLevel = "silent", resetCoefficients = TRUE, startingVariance = 0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # allow Strategus to continue if a model fails to converge
        estimator = "att",
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

      # Covariate balance computations (shared and per-analysis)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs: per Analysis Specifications
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
          tolerance = 2e-07,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # createStudyPopArgs: per Analysis Specifications
      # Note: removeDuplicateSubjects is specified as "keep all" in the spec.
      # The CohortMethod function may expect slightly different values in some installs;
      # we use the exact string from the Analysis Specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 365,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999
      )

      # Build the CohortMethod analysis entry and append to list
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          if (is.null(studyStartDate)) "" else studyStartDate,
          if (is.null(studyEndDate)) "" else studyEndDate,
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
    } # end psConfigList loop
  } # end timeAtRisks loop
} # end studyPeriods loop

# Create the CohortMethod module specification object using the built analyses
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
# Assemble Strategus analysis specifications and save to JSON file -----------
# ------------------------------------------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save to inst/<studyName>/<studyName>AnalysisSpecification.json
outputFolder <- file.path("inst", studyName)
if (!dir.exists(outputFolder)) dir.create(outputFolder, recursive = TRUE)

ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path(outputFolder, sprintf("%sAnalysisSpecification.json", studyName))
)