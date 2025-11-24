library(dplyr)
library(Strategus)

# ------------------------------------------------------------------------------
# This script builds a Strategus analysis specification for the study named:
#   uveitissafety
#
# The settings are taken directly from the provided Analysis Specifications and
# assembled into Strategus module specifications.  Detailed inline comments
# explain how each setting from the Analysis Specifications is applied.
# ------------------------------------------------------------------------------

# Shared Resources -------------------------------------------------------------

# Base WebAPI used to fetch cohort and concept set definitions.
# Keep as-is or change to your Atlas WebAPI.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ------------------------------------------------------------------------------
# Cohort Definitions
# ------------------------------------------------------------------------------
# We fetch the cohort definitions by their Atlas cohort IDs. The Analysis
# Specifications list three cohorts:
#   - Target cohort:      id = 1794126, name = "target1"
#   - Comparator cohort:  id = 1794132, name = "comparator1"
#   - Outcome cohort(s):  id = 1794131, name = "outcome1"
#
# exportCohortDefinitionSet returns the cohort definitions (SQL, JSON, metadata).
# We set generateStats = TRUE to compute some cohort statistics during
# cohort generation (this is optional but commonly useful).
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Renumber cohorts in the exported set so that we use compact ids within the
# Strategus specification. This is purely for internal consistency in this
# specification: target -> 1, comparator -> 2, outcome -> 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# ------------------------------------------------------------------------------
# Negative control concept set
# ------------------------------------------------------------------------------
# Analysis Specifications provided a concept set id for negative controls:
#   id = 1888110, name = "negative"
#
# We fetch the concept set definition, resolve it to concrete concepts, and
# convert it to a small cohort-like table that the CohortMethod module expects.
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
  # Align column names to what later code expects:
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  # Assign cohort ids for the negative controls starting at 101 to avoid
  # colliding with main cohorts (1,2,3).
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safeguard: ensure no duplicate cohort IDs across main cohorts and negative
# control cohorts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between main cohorts and negative controls ***")
}

# ------------------------------------------------------------------------------
# Build lists / data frames required for the CohortMethod module
# ------------------------------------------------------------------------------

# Outcomes list for CohortMethod:
# The analysis specification lists one outcome cohort: cohortId mapped to 3.
# We make a small table with the outcome cohort id, name and a "cleanWindow".
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # The Analysis Specifications set a clean window of 365 days for outcomes.
  mutate(cleanWindow = 365)

# Target / Comparator mapping for CohortMethod analyses:
# Map the renumbered cohort ids to human readable names using exact names
# specified in the Analysis Specifications.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  stringsAsFactors = FALSE
)

# Excluded covariate concepts:
# Analysis Specifications left the covariate selection lists empty (no concepts
# explicitly included or excluded). We therefore create an empty data.frame so
# the code that expects this object can still operate.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# CohortGeneratorModule shared resources & module specification
# ------------------------------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource containing the cohort definitions to be generated by the
# CohortGenerator module.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Shared resource containing the negative control outcome cohort concept set,
# resolved into concrete concepts. We set occurrenceType = "first" and allow
# detection on descendants (standard choices for negative control outcomes).
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specification instructing Strategus to run cohort generation and, if
# desired, compute statistics (we set generateStats = TRUE to match earlier call).
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ------------------------------------------------------------------------------
# CohortDiagnosticsModule specification
# ------------------------------------------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# We enable a set of diagnostics commonly useful for checking cohort quality.
# The cohort IDs come from the re-numbered cohortDefinitionSet (1,2,3...).
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
  # Set a minimum mean value for temporal characterization (from template example).
  minCharacterizationMean = 0.01
)

# ------------------------------------------------------------------------------
# CohortMethodModule: assemble CM analyses according to the Analysis Specifications
# ------------------------------------------------------------------------------

# Study periods:
# The Analysis Specifications provided a studyPeriods entry with null/NA
# studyStartDate and studyEndDate indicating "no restriction". In the template
# we indicate this by using empty strings. We include a single row so the main
# loop iterates once and produces analyses that are not restricted by dates.
studyPeriods <- tibble::tibble(
  studyStartDate = c(""), # empty => not restricting start
  studyEndDate   = c("")  # empty => not restricting end
)

# Time-at-risk (TAR) as specified:
# Analysis Specifications gives a single TAR:
#   riskWindowStart = 1, startAnchor = "cohort start",
#   riskWindowEnd = 0, endAnchor = "cohort end", minDaysAtRisk = 1
# We provide a label ("TAR1") for human-readable descriptions.
timeAtRisks <- tibble::tibble(
  label = c("TAR1"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end") # "cohort start" | "cohort end"
)

# Propensity Score configuration:
# Analysis Specifications provides a single PS setting that uses matching:
#   maxRatio = 10, caliper = 0.2, caliperScale = "standardized logit"
matchOnPsArgsList <- tibble::tibble(
  label = c("match_ratio10_caliper0.2"),
  maxRatio  = c(10),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # exact string as provided
)

# No stratification-by-PS settings were provided (NULL in spec), so we leave
# stratifyByPsArgsList empty.
stratifyByPsArgsList <- tibble::tibble(
  label = character(0),
  numberOfStrata = integer(0),
  baseSelection = character(0)
)

# Build a unified PS configuration list (psConfigList) used by the loop below.
psConfigList <- list()

# Convert "match on PS" rows into configurations
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

# Convert "stratify by PS" rows into configurations (none in this study).
if (exists("stratifyByPsArgsList") && nrow(stratifyByPsArgsList) > 0) {
  for (i in seq_len(nrow(stratifyByPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "stratify",
      label = stratifyByPsArgsList$label[i],
      params = list(
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection  = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}

# Now iterate through combinations of study periods, TARs and PS configurations
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Convert the selected PS configuration into CohortMethod args.
      if (psCfg$method == "match") {
        # Create matching arguments. We set allowReverseMatch = FALSE (standard)
        # and do not include additional stratification columns.
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

      # Covariate settings: by default include standard covariates. The
      # Analysis Specifications did not include specific covariate includes or
      # excludes, so we use the default covariate settings and allow excluding
      # descendants if needed.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build the outcome list that CohortMethod will use:
      # - Include the outcome(s) of interest (outcome cohort ids from oList)
      # - Append the negative control cohorts (from negativeControlOutcomeCohortSet)
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          # Outcome of interest (trueEffectSize unknown -> NA)
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          # Negative control outcomes: marked as outcomeOfInterest = FALSE
          # We set trueEffectSize = 1 for negative controls (null effect).
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # For each target-comparator pair, create target/comparator/outcome mapping.
      # Note: excludedCovariateConceptIds is empty (no explicit excludes provided).
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # getDbCohortMethodDataArgs: fetch the cohort-method ready data. The
      # Analysis Specifications provided:
      #   studyPeriods: (no restriction) -> we pass the studyStartDate and
      #                 studyEndDate (empty strings).
      #   maxCohortSize: 0  -> include all subjects
      #   restrictToCommonPeriod: TRUE (from createStudyPopArgs, but also commonly
      #                            set for getDbCohortMethodDataArgs)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs: build the Cyclops prior & control settings according to
      # the Analysis Specifications:
      # createPsArgs settings from Analysis Specifications:
      #   maxCohortSizeForFitting = 250000
      #   errorOnHighCorrelation = true
      #   prior: priorType = "laplace", useCrossValidation = true
      #   control: tolerance = 2e-7, cvType = "auto", fold = 10,
      #            cvRepetitions = 10, noiseLevel = "silent",
      #            resetCoefficients = true, startingVariance = 0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        # We set stopOnError = FALSE so Strategus can continue with analyses
        # even if fitting a PS model fails for a specific target-comparator -
        # this mirrors the template approach and is useful for large batch runs.
        stopOnError = FALSE,
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",
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

      # Covariate balance computation arguments:
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs: configure the outcome model according to the
      # Analysis Specifications:
      #   modelType = "cox", stratified = TRUE, useCovariates = FALSE,
      #   inversePtWeighting = FALSE
      #   prior: laplace with cross-validation
      #   control: tolerance = 2e-7, cvType = "auto", fold = 10,
      #            cvRepetitions = 10, noiseLevel = "quiet",
      #            resetCoefficients = TRUE, startingVariance = 0.01
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
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          tolerance = 2e-7
        )
      )

      # createStudyPopArgs: settings for creating the study population (per
      # Analysis Specifications). These settings control how subjects are
      # selected and censored.
      #
      # The Analysis Specifications requested:
      #   restrictToCommonPeriod = true
      #   firstExposureOnly = true
      #   washoutPeriod = 365
      #   removeDuplicateSubjects = "keep all"
      #   censorAtNewRiskWindow = true
      #   removeSubjectsWithPriorOutcome = true
      #   priorOutcomeLookBack = 99999
      #   timeAtRisks: riskWindowStart=1 (cohort start), riskWindowEnd=0 (cohort end),
      #                minDaysAtRisk = 1
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE,
        firstExposureOnly = TRUE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = TRUE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Build a descriptive label for this analysis so outputs are understandable.
      description <- sprintf(
        "Study: uveitissafety; period: %s-%s; TAR: %s; PS: %s",
        ifelse(studyStartDate == "", "all", studyStartDate),
        ifelse(studyEndDate == "", "all", studyEndDate),
        timeAtRisks$label[t],
        psCfg$label
      )

      # Append the CohortMethod analysis specification to the list.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = description,
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
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  # Do not refit PS for every outcome / population in this study (matches many
  # safety studies where the same PS is used across outcomes)
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# ------------------------------------------------------------------------------
# Assemble final Strategus analysis specifications object
# ------------------------------------------------------------------------------

analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Persist the specification to the inst/<studyName> directory for packaging.
# File path uses the exact study name from the Analysis Specifications: uveitissafety
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "uveitissafety", "uveitissafetyAnalysisSpecification.json")
)