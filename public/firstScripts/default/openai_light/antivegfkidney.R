library(dplyr)
library(Strategus)

# ------------------------------------------------------------------------------
# CreateStrategusAnalysisSpecification.R
#
# This script creates a Strategus analysis specification JSON for the study
# "antivegfkidney" using the settings provided in the Analysis Specifications.
#
# The script mirrors the template structure and uses EXACT names from the
# Analysis Specifications where indicated (cohort ids, cohort names, negative
# control concept set id, module / argument names). Comments explain how each
# setting from the specification is applied.
# ------------------------------------------------------------------------------

# Shared Resources -------------------------------------------------------------
# Base URL for cohort export from ATLAS / WebAPI
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions:
# Using the cohort ids supplied in the Analysis Specifications:
#   targetCohort:     id = 1794126, name = "target1"
#   comparatorCohort: id = 1794132, name = "comparator1"
#   outcomeCohort(s): id = 1794131, name = "outcome1"
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to a small internal id space used for the analyses:
# target -> 1, comparator -> 2, outcome -> 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes:
# Use the concept set id provided in the Analysis Specifications:
#   negativeControlConceptSet: id = 1888110, name = "negative"
#
# We resolve the concept set and convert the concepts to a cohort-like table
# which will be appended to the outcome list as negative controls.
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
  # Assign cohort ids to negative controls that do not overlap with target/comparator/outcome ids (start at 101)
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no duplicate cohort ids between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ------
# Outcomes: build oList from cohortDefinitionSet entries that correspond to our outcome cohort(s)
# We add cleanWindow = 365 as in the template (typical clean window for outcome definitions)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis:
# Use EXACT names from Analysis Specifications:
#   targetCohortName  = "target1"
#   comparatorCohortName = "comparator1"
# We also include placeholder concept id columns (targetConceptId, comparatorConceptId)
# that are referenced downstream when specifying excluded covariates. Set to NA_integer_
# because explicit concept ids for the exposures were not provided in the specification.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  targetConceptId = NA_integer_,
  comparatorConceptId = NA_integer_,
  stringsAsFactors = FALSE
)

# Excluded covariate concepts:
# Template expects a data.frame of concepts to exclude (e.g. the drug concept ids)
# The Analysis Specifications did not supply specific concepts to exclude; keep an empty data.frame.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# CohortGeneratorModule --------------------------------------------------------
# Create shared resources and module specifications for cohort generation (CohortGenerator)
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource: cohort definitions (exported earlier)
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Shared resource: negative control outcome cohort set (resolved earlier)
# occurrenceType = "first", detectOnDescendants = TRUE is commonly used; template uses these
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specification: request cohort generation and statistics (generateStats = TRUE)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule -----------------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Create Cohort Diagnostics module specification using commonly useful diagnostics.
# The template uses a set of diagnostics; we replicate those here.
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
# The Analysis Specifications include detailed settings for:
#  - getDbCohortMethodDataArgs
#  - createStudyPopArgs (including 2 time-at-risk definitions)
#  - propensity score creation and adjustment (match on PS)
#  - fitOutcomeModelArgs
#
# We now translate those settings into the structures expected by the Strategus/CohortMethod module.

# If you are not restricting your study to a specific time window, these values
# should be empty strings. The Analysis Specifications provided empty study
# period strings, so we create a single-row tibble with empty strings so the
# iteration below runs once with no restriction.
studyPeriods <- tibble::tibble(
  studyStartDate = c(""), # empty -> no restriction
  studyEndDate   = c("")  # empty -> no restriction
)

# Time-at-risks (TARs) as specified in Analysis Specifications (two windows)
# Note: labels are arbitrary human-readable identifiers for each TAR.
timeAtRisks <- tibble::tibble(
  label = c("TAR_cohortStart_to_cohortEnd", "TAR_cohortStart_to_cohortStart_plus"),
  riskWindowStart  = c(1, 1),
  startAnchor = c("cohort start", "cohort start"), # "cohort start" per spec
  riskWindowEnd  = c(0, 99999),
  endAnchor = c("cohort end", "cohort start") # per spec
)

# Propensity Score settings - match on PS as specified:
# There is a single PS setting in the Analysis Specifications: match with caliper 0.2 (standardized logit), maxRatio = 1
matchOnPsArgsList <- tibble::tibble(
  label = c("match_on_ps_1"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # matches the specification
)

# No stratification-by-PS entries in the provided Analysis Specifications; leave empty
stratifyByPsArgsList <- tibble::tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0)
)

# Build psConfigList combining match and stratify configurations (template logic)
psConfigList <- list()

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

# Iterate through all combinations of study period, TAR, and PS configuration to
# create the set of CohortMethod analyses to run.
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create matchOnPsArgs or stratifyByPsArgs depending on the psCfg method
      if (psCfg$method == "match") {
        # Use CohortMethod::createMatchOnPsArgs and pass caliperScale exactly as specified
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
        stop("Unknown psCfg method: ", psCfg$method)
      }

      # Covariate settings: Analysis Specifications provided empty include/exclude lists,
      # so we use the default covariate settings but ensure descendants are added to excluded
      # when excluding (template approach).
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build the list of outcomes for CohortMethod:
      # - Include the main outcome(s) from oList (these are outcomeOfInterest = TRUE)
      # - Append negative controls resolved earlier (these are outcomeOfInterest = FALSE and trueEffectSize = 1)
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

      # Create targetComparatorOutcomes list: for each pair in cmTcList create the
      # required CohortMethod::createTargetComparatorOutcomes structure.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude exposure concept ids (if available) and any other excluded covariates.
          # The template concatenates the target/comparator concept ids with excluded covariate ids.
          excludedCovariateConceptIds = c(
            cmTcList$targetConceptId[i],
            cmTcList$comparatorConceptId[i],
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # getDbCohortMethodDataArgs: translate the getDbCohortMethodDataArgs from spec
      # Analysis Specifications:
      #   maxCohortSize = 0
      #   restrictToCommonPeriod = TRUE
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs: use Cyclops prior/control settings as specified
      # Analysis Specifications for createPsArgs:
      #   maxCohortSizeForFitting = 250000
      #   errorOnHighCorrelation = TRUE
      #   prior: priorType = "laplace", useCrossValidation = TRUE
      #   control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      #            noiseLevel = "silent", resetCoefficients = TRUE, startingVariance = 0.01
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        # stopOnError set to FALSE to allow the Strategus run to proceed even when PS fitting fails for one comparison
        stopOnError = FALSE,
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",
          exclude = c(0),
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          tolerance = 2e-07,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "silent",
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          seed = 1
        )
      )

      # Covariate balance computation arguments
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs: settings from Analysis Specifications:
      #   modelType = "cox"
      #   stratified = FALSE
      #   useCovariates = FALSE
      #   inversePtWeighting = FALSE
      #   prior: laplace, useCrossValidation = TRUE
      #   control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      #            noiseLevel = "quiet", resetCoefficients = TRUE, startingVariance = 0.01
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
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
          startingVariance = 0.01,
          seed = 1
        )
      )

      # createStudyPopArgs: use the createStudyPopulationArgs factory and fill in
      # the TAR-specific fields based on the current timeAtRisks row.
      # Analysis Specifications for createStudyPopArgs:
      #   restrictToCommonPeriod = FALSE
      #   firstExposureOnly = FALSE
      #   washoutPeriod = 0
      #   removeDuplicateSubjects = "keep all"
      #   censorAtNewRiskWindow = FALSE
      #   removeSubjectsWithPriorOutcome = TRUE
      #   priorOutcomeLookBack = 99999
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
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

      # Append the CohortMethod analysis specification to the list
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

# Create CohortMethod module specification bundling all analyses and target/comparator/outcome mappings
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the full Strategus analysis specifications object ---------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specification JSON to inst/<studyName>/<studyName>AnalysisSpecification.json
# Use the study name from the Analysis Specifications: "antivegfkidney"
outputFile <- file.path("inst", "antivegfkidney", "antivegfkidneyAnalysisSpecification.json")
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  outputFile
)

# Informational message (not strictly required, but useful when running interactively)
message("Analysis specification JSON saved to: ", outputFile)