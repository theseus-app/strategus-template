library(dplyr)
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)

################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script builds the Strategus analysis specification JSON for the study
# defined in the Analysis Specifications JSON block. It uses the OHDSI
# Strategus packages (and underlying HADES modules) to construct:
#   - shared resources (cohort definitions, negative control concept set)
#   - module specifications (CohortGenerator, CohortDiagnostics, CohortMethod)
#
# The generated analysis specification is saved to:
#   inst/ceeamos/ceeamosAnalysisSpecification.json
#
# NOTES:
# - This script follows the "template" layout while applying the exact
#   settings provided in the Analysis Specifications block.
# - Extensive comments are included to explain how each setting is applied.
################################################################################

# Shared Resources -------------------------------------------------------------
# WebAPI base URL to download cohort definitions and concept sets.
# (Using the Atlas demo server here; change if you host your own WebAPI.)
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ------------------------------------------------------------------------------
# Cohort Definitions
# ------------------------------------------------------------------------------
# Export the cohort definitions for target, comparator and outcome by the exact
# cohort IDs provided in the Analysis Specifications.
#
# The IDs (exact):
#   Target cohort id: 1794126 (name: "target1")
#   Comparator cohort id: 1794132 (name: "comparator1")
#   Outcome cohort id: 1794131 (name: "outcome1")
#
# We request generateStats = TRUE to include cohort statistics in the export.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts so that internal IDs are small and convenient:
# Target -> 1, Comparator -> 2, Outcome -> 3
# (Many Strategus templates expect small cohort IDs for building analyses.)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# ------------------------------------------------------------------------------
# Negative control concept set
# ------------------------------------------------------------------------------
# The Analysis Specifications provide a concept set id for negative controls:
#   negative control concept set id: 1888110 (name: "negative")
#
# We fetch the concept set definition, resolve descendants, and get the
# individual concepts. These are converted into "cohort-like" records used as
# negative control outcomes. The cohortId for negative controls is offset by
# +100 so they don't conflict with the primary cohort ids (1,2,3...).
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
  dplyr::rename(
    outcomeConceptId = "conceptId",
    cohortName = "conceptName"
  ) %>%
  dplyr::mutate(cohortId = dplyr::row_number() + 100) %>% # negative controls start at 101, 102...
  dplyr::select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no duplicate cohort IDs between primary cohorts and
# negative control cohorts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between primary cohorts and negative controls ***")
}

# Create data frames used for building CohortMethod analyses -------------------
# Outcomes list: build from the re-numbered cohortDefinitionSet (outcome is id 3)
# Add cleanWindow equal to the prior outcome lookback from the Analysis Specs
# (priorOutcomeLookBack = 365).
oList <- cohortDefinitionSet %>%
  dplyr::filter(.data$cohortId == 3) %>%
  dplyr::mutate(outcomeCohortId = cohortId,
                outcomeCohortName = cohortName) %>%
  dplyr::select(outcomeCohortId, outcomeCohortName) %>%
  dplyr::mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis. We use the re-numbered
# IDs 1 and 2, and keep human-readable names to be used in descriptions.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  stringsAsFactors = FALSE
)

# Excluded covariate concepts: the Analysis Specifications list no explicit
# covariate to exclude. We create an empty data.frame; when building the
# excludedCovariateConceptIds we will pass this (empty) vector.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# Optional: included covariates were empty in the Analysis Specifications,
# so we will use default covariate settings below.

# CohortGeneratorModule -------------------------------------------------------
# Create shared resource specifications for cohort generation using the exact
# cohortDefinitionSet and negative controls we prepared above.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",        # use the first occurrence as negative control outcome
  detectOnDescendants = TRUE      # include descendant concepts
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule -----------------------------------------------------
# Create CohortDiagnostics module specifications using the full set of
# cohortDefinitionSet$cohortId (includes 1,2,3)
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
# The Analysis Specifications indicate no restricted study period (studyStartDate
# and studyEndDate are null). Per the template instructions, when not restricting
# set these as empty strings.
studyPeriods <- tibble::tibble(
  studyStartDate = c(""), # empty string indicates no restriction
  studyEndDate = c("")    # empty string indicates no restriction
)

# Time-at-risks: the Analysis Specifications provide a single TAR with:
#  - riskWindowStart = 0 (start anchored at cohort start)
#  - riskWindowEnd = 0   (end anchored at cohort end)
#  - minDaysAtRisk = 1
timeAtRisks <- tibble::tibble(
  label = c("TAR1"),
  riskWindowStart = c(0),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(0),
  endAnchor = c("cohort end")
)

# Propensity Score settings - match on PS as specified
# Analysis Specifications indicate:
#   maxRatio = 10
#   caliper = 0.2
#   caliperScale = "standardized logit"
matchOnPsArgsList <- tibble::tibble(
  label = c("match_on_ps"),
  maxRatio = c(10),
  caliper = c(0.2),
  caliperScale = c("standardized logit") # exact string from specs
)

# No "stratify by PS" entries in the specifications
stratifyByPsArgsList <- tibble::tibble(
  label = character(0),
  numberOfStrata = integer(0),
  baseSelection = character(0)
)

# Build a configuration list combining PS approaches (match and/or stratify).
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

# Build CohortMethod analysis configurations by iterating study periods,
# time-at-risks and PS configurations.
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Build matchOnPsArgs or stratifyByPsArgs depending on the method
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
        stop("Unknown PS configuration method.")
      }

      # Covariate settings: Analysis Specifications provide no explicit include/exclude
      # lists, so we take the default covariate set but keep descendants excluded
      # for anything we exclude (there are none here).
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build outcome list: primary outcome(s) (from oList) marked as outcomes of
      # interest, and negative controls (from negativeControlOutcomeCohortSet)
      # marked as outcomeOfInterest = FALSE with a trueEffectSize of 1.
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

      # Build the target-comparator-outcomes combinations. For each row in cmTcList
      # we create an entry combining target, comparator and the outcome list.
      # The excluded covariate concept ids are taken from excludedCovariateConcepts.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Create GetDbCohortMethodDataArgs:
      # - restrictToCommonPeriod: we use TRUE here as in the template (this restricts
      #   cohort extraction to the common observation period when study period strings
      #   are provided or empty).
      # - studyStartDate/studyEndDate: pass through the (possibly empty) strings.
      # - maxCohortSize: use the value from the Analysis Specifications (maxCohortSize = 0).
      # - covariateSettings: default covariate bundle built above.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Create PS fitting arguments based on the Analysis Specifications.
      # createPsArgs:
      #   maxCohortSizeForFitting = 250000
      #   errorOnHighCorrelation = TRUE
      #   prior: laplace, useCrossValidation = TRUE
      #   control: tolerance = 2e-7, cvType = "auto", fold = 10,
      #            cvRepetitions = 10, noiseLevel = "silent",
      #            resetCoefficients = TRUE, startingVariance = 0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # allow Strategus to continue; matches template behaviour
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

      # Covariate balance computation settings: use 'maxCohortSize' from specs (0 = no limit)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 0,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 0,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit outcome model arguments per Analysis Specifications:
      #   modelType = "cox", stratified = FALSE, useCovariates = FALSE,
      #   inversePtWeighting = FALSE
      #   prior: laplace, useCrossValidation = TRUE
      #   control: tolerance=2e-7, cvType="auto", fold=10, cvRepetitions=10,
      #            noiseLevel="quiet", resetCoefficients=TRUE, startingVariance=0.01
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

      # CreateStudyPopulation arguments: use the values from Analysis Specifications:
      #   restrictToCommonPeriod = FALSE,
      #   firstExposureOnly = FALSE,
      #   washoutPeriod = 365,
      #   removeDuplicateSubjects = "remove all",
      #   censorAtNewRiskWindow = FALSE,
      #   removeSubjectsWithPriorOutcome = TRUE,
      #   priorOutcomeLookback = 365,
      #   riskWindowStart/End and anchors from timeAtRisks,
      #   minDaysAtRisk = 1
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "remove all",
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

      # Append a CohortMethod analysis specification entry to the list.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s; TAR: %s; PS: %s",
          "ceeamos",
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
    } # end PS configs
  } # end TARs
} # end study periods

# Build CohortMethod module specifications object
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the final analysis specifications object and add resources/modules -----
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Persist the analysis specification JSON to the inst/ directory for the study.
# File name follows the pattern: inst/<studyName>/<studyName>AnalysisSpecification.json
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ceeamos", "ceeamosAnalysisSpecification.json")
)