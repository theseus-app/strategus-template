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
# This script builds a Strategus analysis specification for the study "corazon"
# using the settings provided in the Analysis Specifications JSON.
#
# Detailed inline comments explain how each setting from the specification is
# translated into Strategus / CohortMethod module settings.
################################################################################

# Base Atlas WebAPI URL used to retrieve cohort and concept set definitions.
# Replace with your organization's Atlas/WebAPI URL if needed.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ------------------------------------------------------------------------------
# Cohort Definitions
# ------------------------------------------------------------------------------
# The Analysis Specifications list one target, one comparator and one outcome
# cohort. Export all three from the WebAPI and then re-number them to 1,2,3
# (required by the template and subsequent module wiring).
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number the cohortId values to our internal convention:
# target -> 1, comparator -> 2, outcome -> 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# ------------------------------------------------------------------------------
# Negative Control Concept Set
# ------------------------------------------------------------------------------
# The JSON specified a single concept set id for negative controls (1888110).
# We resolve it to the underlying concepts and convert them to "negative
# control outcome cohort" entries with cohortId starting from 101.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
  ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  mutate(cohortId = row_number() + 100) %>% # negative control cohort IDs start at 101
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no duplicate cohort ids across sets
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# ------------------------------------------------------------------------------
# Build lists/data.frames used downstream
# ------------------------------------------------------------------------------
# Outcomes (from cohortDefinitionSet). We will treat the exported outcome (now
# cohortId == 3) as an outcome of interest. We supply a cleanWindow of 365 as
# a reasonable default for cohort method analyses (can be adjusted if needed).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId,
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target / Comparator pair used for the CohortMethod analyses. We include
# placeholder columns for targetConceptId and comparatorConceptId (NA)
# because the template's excluded covariates list references these columns.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  targetConceptId = NA_integer_,    # No specific target concept id provided
  comparatorConceptId = NA_integer_  # No specific comparator concept id provided
)

# Excluded covariate concepts: The Analysis Specifications had empty lists for
# conceptsToInclude and conceptsToExclude. We therefore create an empty data
# frame so that downstream calls that expect a data.frame exist but are empty.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# CohortGenerator Module Specifications
# ------------------------------------------------------------------------------
# Create shared resources and module specifications for cohort generation.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource with cohort definitions for the CohortGenerator module.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

# Negative control outcome shared resource for cohort generation.
# occurrenceType = "first" and detectOnDescendants = TRUE are common choices.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specification: generate cohorts and compute statistics (generateStats = TRUE)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ------------------------------------------------------------------------------
# CohortDiagnostics Module Specifications
# ------------------------------------------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
# We enable many diagnostic options to inspect cohort quality and characterizations.
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
# CohortMethod Module Specifications
# ------------------------------------------------------------------------------
# The Analysis Specifications define two study periods and two time-at-risk
# definitions. We translate these straight into the variables used to build
# the CohortMethod analyses.

# Study periods from getDbCohortMethodDataArgs in the Analysis Specifications:
# 1) 20100101 - 20191231
# 2) 20120101 - 20191231
studyPeriods <- tibble::tibble(
  studyStartDate = c("20100101", "20120101"),
  studyEndDate   = c("20191231", "20191231")
)

# Time-at-risks: two entries from createStudyPopArgs in the Analysis Specifications.
# We create human-readable labels to distinguish them (these labels propagate
# into analysis descriptions).
timeAtRisks <- tibble::tibble(
  label = c("TAR_1_to_0_endAnchor_cohortEnd", "TAR_1_to_99999_endAnchor_cohortStart"),
  riskWindowStart  = c(1, 1),
  startAnchor = c("cohort start", "cohort start"), # allowed values: "cohort start" | "cohort end"
  riskWindowEnd  = c(0, 99999),
  endAnchor = c("cohort end", "cohort start")     # allowed values: "cohort start" | "cohort end"
)

# Propensity score adjustment settings: two configurations were specified
# in the Analysis Specifications:
# - stratify by PS with numberOfStrata = 5, baseSelection = "all"
# - match on PS with maxRatio = 0, caliper = 0.2, caliperScale = "standardized logit"
# We construct two small data frames describing these configs so we can build a
# unified psConfigList below.
stratifyByPsArgsList <- tibble::tibble(
  label = c("stratify_5_all"),
  numberOfStrata = c(5),
  baseSelection = c("all")
)

matchOnPsArgsList <- tibble::tibble(
  label = c("match_caliper_0.2"),
  maxRatio = c(0),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# Build a combined list of PS configurations. Each element contains:
#  - method: "match" | "stratify"
#  - label: human-friendly label
#  - params: list with parameters used when creating match/stratify args
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

# Prepare the main list of CohortMethod analyses (one per combination of
# studyPeriod x timeAtRisk x PS configuration).
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create matchOnPsArgs OR stratifyByPsArgs depending on the config.
      if (psCfg$method == "match") {
        # matchOnPsArgs: use CohortMethod::createMatchOnPsArgs
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
        stop("Unknown psCfg$method: expected 'match' or 'stratify'")
      }

      # Covariate settings: the Analysis Specifications did not specify
      # custom covariates to include/exclude, so we use the default covariate
      # settings. If you want to include/exclude specific concept sets, you
      # would construct includedCovariateConcepts / excludedCovariateConcepts
      # data.frames and feed them into FeatureExtraction covariate settings.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build the list of outcomes for this analysis:
      # - the outcome(s) of interest from the cohortDefinitionSet (outcomeList)
      # - the negative control outcomes created from the negative control concept set
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

      # For each target-comparator pair, create a TargetComparatorOutcomes entry.
      # The excludedCovariateConceptIds argument collects IDs of concepts to
      # exclude from the covariate set; here we include any explicit target/comparator
      # concept IDs (NA in this specification) and the excludedCovariateConcepts
      # data frame (which is empty).
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = c(
            # convert NA to integer(0) so createTargetComparatorOutcomes
            # receives a proper vector; no concepts to exclude in this study.
            na.omit(c(cmTcList$targetConceptId[i], cmTcList$comparatorConceptId[i], excludedCovariateConcepts$conceptId))
          )
        )
      }

      # getDbCohortMethodDataArgs: controls how cohort data are pulled from the DB.
      # The Analysis Specifications set:
      # - studyPeriods (per-iteration)
      # - maxCohortSize = 0 (no cap)
      # - restrictToCommonPeriod = FALSE
      # - firstExposureOnly = FALSE
      # - washoutPeriod = 0
      # - removeDuplicateSubjects = "keep all"
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs: settings for fitting the propensity score model.
      # The Analysis Specifications requested regularized logistic regression
      # (Laplace prior) with cross-validation and specific control parameters.
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        # We set stopOnError = FALSE so Strategus attempts to continue other analyses
        # even if PS fitting fails for one configuration. This mirrors the template
        # behavior and allows downstream diagnostics to be produced.
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
          startingVariance = 0.01
        )
      )

      # Covariate balance computation arguments.
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs: settings for the outcome model. The Analysis
      # Specifications requested a stratified Cox model without additional
      # covariates (useCovariates = FALSE) and a Laplace prior with cross-validation.
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

      # createStudyPopArgs: build study population arguments using the
      # time-at-risk configuration for this iteration. These settings reflect
      # the JSON:
      # - restrictToCommonPeriod = FALSE
      # - firstExposureOnly = FALSE
      # - washoutPeriod = 0
      # - removeDuplicateSubjects = "keep all"
      # - censorAtNewRiskWindow = FALSE
      # - removeSubjectsWithPriorOutcome = TRUE
      # - priorOutcomeLookBack = 99999
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

      # Finally, append a CohortMethod analysis specification to the list.
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

# Create the CohortMethod module specifications object. We pass:
# - the list of analyses we created
# - the target-comparator-outcomes list (used to map outcomes to pairs)
# - refit flags set to FALSE as per template
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
# Combine module specifications into a single Strategus analysisSpecifications
# ------------------------------------------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specification JSON to inst/corazon/corazonAnalysisSpecification.json
# so it can be used by Strategus run scripts and for provenance tracking.
outFile <- file.path("inst", "corazon", "corazonAnalysisSpecification.json")
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  outFile
)

# Print location for user convenience (also helpful when running interactively)
message("Analysis specification saved to: ", outFile)