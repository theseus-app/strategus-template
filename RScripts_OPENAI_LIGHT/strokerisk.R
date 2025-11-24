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
# This script builds Strategus analysis specifications for the "strokerisk"
# study using the provided Analysis Specifications. The script follows the
# structure of the provided template but applies the exact settings from the
# JSON Analysis Specifications.
#
# Detailed commentary (inline) explains how each JSON field is translated into
# Strategus / CohortMethod / HADES module settings.
################################################################################

# -----------------------
# Shared Resources
# -----------------------
# The webapi baseUrl used to export cohort definitions and concept set definitions.
# Replace this with your Atlas/WebAPI endpoint if needed.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Export the required cohort definitions from Atlas / WebAPI.
# The Analysis Specifications lists:
#   targetCohort     id = 1794126 name = "target1"
#   comparatorCohort id = 1794132 name = "comparator1"
#   outcomeCohort    id = 1794131 name = "outcome1"
#
# We export these three cohort definitions so Strategus HADES modules
# (CohortGenerator, CohortDiagnostics, CohortMethod) can use them as shared
# cohort definition resources.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # target: target1
    1794132, # comparator: comparator1
    1794131  # outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to small integer ids used later in the specifications:
# - target -> 1
# - comparator -> 2
# - outcome -> 3
#
# This is purely a convenience for downstream settings that refer to cohorts by
# small integers (as in the template).
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# -----------------------
# Negative control outcomes
# -----------------------
# The Analysis Specifications contain a negative control "concept set" with:
# conceptSetId = 1888110, name = "negative"
#
# We resolve the concept set through the WebAPI and convert its concepts into a
# cohort-like table that we will append as negative control outcome cohorts.
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

# Sanity check: ensure the newly-assigned negative control cohortIds do not
# duplicate any cohortDefinitionSet cohortIds.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohorts and negative controls ***")
}

# -----------------------
# Create small helper data.frames for later use
# -----------------------

# Outcomes (oList)
# We extract the outcome cohort (renumbered to cohortId == 3) into the structure
# expected by the template. We also supply a cleanWindow of 365 days as a
# practical default (this can be adjusted).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target & Comparator for CohortMethod analyses
# Use the exact cohort names from the Analysis Specifications.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  # The template sometimes references targetConceptId/comparatorConceptId to
  # exclude the index drugs from covariates. The Analysis Specifications do not
  # supply explicit concept ids to exclude, so leave these columns NA (we will
  # use an empty excludedCovariateConcepts below).
  targetConceptId = NA_integer_,
  comparatorConceptId = NA_integer_,
  stringsAsFactors = FALSE
)

# Excluded covariates: the Analysis Specifications did not provide any
# explicit concepts to exclude from the covariate set. Create an empty table
# so excludedCovariateConcepts$conceptId is an empty numeric vector.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# NOTE:
# If you want to explicitly include certain covariates only (instead of the
# default covariate set), you could create an includedCovariateConcepts data.frame
# here and add it to the covariateSettings in getDbCohortMethodDataArgs below.

# -----------------------
# CohortGeneratorModule specifications
# -----------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource: cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Shared resource: negative controls (resolved concept set -> cohort-like set)
# occurrenceType = "first" and detectOnDescendants = TRUE are sensible defaults;
# adjust if your negative controls require different behavior.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications: generate stats (matches the template default)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# -----------------------
# CohortDiagnosticsModule specifications
# -----------------------
# We enable a broad set of diagnostics (similar to the template). The numeric
# threshold (minCharacterizationMean) is set to 0.01 here, as in the template.
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

# -----------------------
# CohortMethodModule specifications
# -----------------------
# The Analysis Specifications define two study periods:
#  - 2001-01-01 to 2017-12-31
#  - 2001-01-01 to 2015-11-30
studyPeriods <- tibble::tibble(
  studyStartDate = c("20010101", "20010101"),
  studyEndDate   = c("20171231", "20151130") # YYYYMMDD strings
)

# The Analysis Specifications define one Time-at-Risk:
#   riskWindowStart = 1 (startAnchor = "cohort start")
#   riskWindowEnd   = 0 (endAnchor   = "cohort end")
#   minDaysAtRisk   = 1
timeAtRisks <- tibble::tibble(
  label = c("riskWindow_1_cohortStart_to_cohortEnd"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")      # "cohort start" | "cohort end"
)

# -----------------------
# Propensity score adjustment configurations
# -----------------------
# The Analysis Specifications list three PS settings:
# 1) No PS adjustment (both matchOnPsArgs and stratifyByPsArgs are NULL)
# 2) Match on PS with maxRatio=1, caliper=0.05, caliperScale="propensity score"
# 3) Match on PS with maxRatio=10, caliper=0.2, caliperScale="standardized logit"
#
# We'll build a psConfigList explicitly with these three configurations.
psConfigList <- list(
  # 1) No PS adjustment (unadjusted)
  list(
    method = "none",
    label = "unadjusted",
    params = list()
  ),
  # 2) Match: maxRatio = 1, caliper = 0.05, caliperScale = "propensity score"
  list(
    method = "match",
    label = "match_maxRatio1_caliper0.05_propensity score",
    params = list(
      maxRatio     = 1,
      caliper      = 0.05,
      caliperScale = "propensity score"
    )
  ),
  # 3) Match: maxRatio = 10, caliper = 0.2, caliperScale = "standardized logit"
  list(
    method = "match",
    label = "match_maxRatio10_caliper0.2_standardized logit",
    params = list(
      maxRatio     = 10,
      caliper      = 0.2,
      caliperScale = "standardized logit"
    )
  )
)

# -----------------------
# Prepare to build CM analyses
# -----------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {
    # For each PS configuration (including 'none') build an analysis specification
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Build matchOnPsArgs and stratifyByPsArgs according to psCfg$method
      if (psCfg$method == "match") {
        # Create CohortMethod match-on-PS arguments using the specified params.
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # (not used in these Analysis Specifications, but included for completeness)
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      } else {
        # 'none' or other: no PS adjustment
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- NULL
      }

      # Covariate settings:
      # The Analysis Specifications did not provide an 'include-only' covariate
      # set, so use the default covariate settings. We set addDescendantsToExclude
      # = TRUE to be conservative for excluded covariates.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build the outcome list containing:
      # - the outcome(s) of interest (from oList, outcomeCohortId)
      # - the negative control outcome cohorts (from negativeControlOutcomeCohortSet)
      #
      # For outcomes of interest we set outcomeOfInterest = TRUE and priorOutcomeLookback = 99999
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        # Negative controls: mark as outcomeOfInterest = FALSE and trueEffectSize = 1
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Build target-comparator-outcomes list for all entries in cmTcList
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        # The Analysis Specifications do not supply target/comparator concept ids
        # to exclude. Therefore, we pass only the excludedCovariateConcepts (empty
        # in this configuration). If you have drug concept ids for the index
        # exposures, supply them as targetConceptId/comparatorConceptId in
        # cmTcList and include them here.
        excludedIds <- excludedCovariateConcepts$conceptId
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedIds
        )
      }

      # getDbCohortMethodDataArgs:
      # The Analysis Specifications provided studyPeriods (we iterate over them),
      # covariateSettings defined above, and maxCohortSize = 0. We set
      # restrictToCommonPeriod = TRUE to limit covariate extraction to the study
      # period (template default). Adjust if necessary.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs:
      # Values are directly taken from the Analysis Specifications:
      #   maxCohortSizeForFitting = 250000
      #   errorOnHighCorrelation = TRUE
      #   prior: priorType = "laplace", useCrossValidation = TRUE
      #   control: tolerance = 2e-7, cvType = "auto", fold = 10,
      #            cvRepetitions = 10, noiseLevel = "quiet",
      #            resetCoefficients = TRUE, startingVariance = 0.01
      #
      # We also set stopOnError = FALSE to allow the overall Strategus process to
      # continue even if a single PS-fitting fails (keeps the run robust).
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
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
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Covariate balance computation arguments:
      # - computeSharedCovariateBalanceArgs uses a generic covariateFilter = NULL
      #   and a large maxCohortSize so balance can be computed on large cohorts.
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      # - computeCovariateBalanceArgs uses the default Table 1 specifications
      #   for summarizing covariate balance (FeatureExtraction helper).
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs:
      # Use Analysis Specs:
      #   modelType = "cox", stratified = TRUE, useCovariates = FALSE,
      #   inversePtWeighting = FALSE
      #   prior: laplace + CV
      #   control: tolerance, cvType, fold, cvRepetitions, noiseLevel, resetCoefficients, startingVariance
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

      # createStudyPopArgs:
      # Use the fields from Analysis Specifications:
      #   restrictToCommonPeriod = FALSE
      #   firstExposureOnly = FALSE
      #   washoutPeriod = 0
      #   removeDuplicateSubjects = "keep all"
      #   censorAtNewRiskWindow = FALSE
      #   removeSubjectsWithPriorOutcome = TRUE
      #   priorOutcomeLookBack = 99999
      #   riskWindow parameters taken from timeAtRisks
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

      # Append the settings as a single CohortMethod analysis
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "StudyPeriod: %s-%s; TAR: %s; PS: %s",
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
    } # end PS configurations loop
  } # end timeAtRisks loop
} # end studyPeriods loop

# Build CohortMethod module specifications using the cmAnalysisList and the
# targetComparatorOutcomesList constructed above.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# -----------------------
# Combine module specifications into a single Strategus analysisSpecifications
# -----------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specification json to inst/strokerisk/strokeriskAnalysisSpecification.json
# This path matches the study name from the Analysis Specifications ("strokerisk").
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "strokerisk", "strokeriskAnalysisSpecification.json")
)

################################################################################
# End of CreateStrategusAnalysisSpecification.R
################################################################################