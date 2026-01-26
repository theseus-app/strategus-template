################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script creates a Strategus analysis specification JSON using the OHDSI
# Strategus package, following the provided template and applying the settings
# from <Analysis Specifications> EXACTLY (no name auto-correct).
#
# Study name (from specs): iudehre
#
# Notes:
# - Cohort IDs are exported from ATLAS WebAPI, then re-numbered to 1/2/3 for
#   target/comparator/outcome to match Strategus/CohortMethod conventions.
# - Negative controls are created from a Concept Set and assigned cohortIds
#   starting at 101 to avoid collisions with 1/2/3.
# - CohortMethod settings (study period, TAR, PS, outcome model) are configured
#   to match the provided JSON.
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# WebAPI endpoint used to export cohort definitions and resolve concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export the target, comparator, and outcome cohort definitions from WebAPI.
# IDs and names come from <Analysis Specifications>:
# - targetCohort:     id=1794126, name="target1"
# - comparatorCohort: id=1794132, name="comparator1"
# - outcomeCohort:    id=1794131, name="outcome1"
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts ------------------------------------------------------------
# Strategus/CohortMethod analyses typically refer to target/comparator/outcome
# cohorts using small integer IDs. We map:
# - target1      -> cohortId 1
# - comparator1  -> cohortId 2
# - outcome1     -> cohortId 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# Negative controls are defined by a Concept Set in ATLAS:
# - negativeControlConceptSet: id=1888110, name="negative"
#
# We resolve the concept set to a list of concepts, then create a "negative
# control outcome cohort set" where each concept becomes an outcome definition.
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
  rename(
    outcomeConceptId = "conceptId",
    cohortName = "conceptName"
  ) %>%
  # Assign cohort IDs for negative controls starting at 101 to avoid collisions
  # with the main cohorts (1,2,3).
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no cohortId collisions between main cohorts and negative controls.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames used by CohortMethod module ------------------------------
# Outcomes of interest list (oList):
# - We include the single outcome cohort (cohortId == 3).
# - cleanWindow is included as in the template; not used directly by CM here.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target/Comparator list for CohortMethod:
# Names are taken EXACTLY from <Analysis Specifications>.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate inclusion/exclusion concepts --------------------------------------
# <Analysis Specifications> provides conceptsToInclude and conceptsToExclude,
# but both contain id=null and name="" (i.e., no explicit include/exclude list).
#
# We therefore:
# - Do not define includedCovariateConcepts (use default covariate set).
# - Define excludedCovariateConcepts as an empty data frame.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# CohortGeneratorModule --------------------------------------------------------
# Shared resources:
# - cohortDefinitionShared: the cohort definitions to instantiate in the CDM.
# - negativeControlsShared: negative control outcomes created from concept set.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule ------------------------------------------------------
# Use the template defaults for diagnostics module settings.
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
# Study periods (getDbCohortMethodDataArgs.studyPeriods):
# <Analysis Specifications>:
# - studyStartDate = "20030101"
# - studyEndDate   = null  (represented as NA in R)
studyPeriods <- tibble::tibble(
  studyStartDate = c("20030101"),
  studyEndDate   = c(NA_character_)
)

# Time-at-risk settings (createStudyPopArgs.timeAtRisks):
# <Analysis Specifications> provides a single TAR:
# - riskWindowStart = 30, startAnchor = "cohort start"
# - riskWindowEnd   = 5475, endAnchor = "cohort start"
# - minDaysAtRisk   = 1
#
# CohortMethod::createCreateStudyPopulationArgs expects TAR fields directly
# (riskWindowStart/startAnchor/riskWindowEnd/endAnchor/minDaysAtRisk).
# We keep a label for description building.
timeAtRisks <- tibble::tibble(
  label = c("TAR_30_to_5475_from_cohort_start"),
  riskWindowStart = c(30),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(5475),
  endAnchor = c("cohort start"),
  minDaysAtRisk = c(1)
)

# Propensity score adjustment settings ----------------------------------------
# <Analysis Specifications>:
# - psSettings: one entry with matchOnPsArgs (maxRatio=1, caliper=0.2,
#   caliperScale="standardized logit"), stratifyByPsArgs=null
#
# We represent this as a "match on PS" configuration list.
matchOnPsArgsList <- tibble::tibble(
  label = c("match_maxRatio1_caliper0.2_stdLogit"),
  maxRatio = c(1),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# No stratification configs (explicitly NULL in specs).
stratifyByPsArgsList <- tibble::tibble(
  label = character(),
  numberOfStrata = integer(),
  baseSelection = character()
)

# Build a single PS configuration list (each entry has: method, label, params)
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

# Iterate through all analysis setting combinations ---------------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create PS adjustment args based on method -----------------------------
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
      }

      # Covariate settings ----------------------------------------------------
      # <Analysis Specifications> does not provide custom covariate settings
      # beyond include/exclude lists (which are empty). We therefore use the
      # default covariate settings, and set addDescendantsToExclude=TRUE as in
      # the template.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcomes list ---------------------------------------------------------
      # - Outcome of interest: outcome1 (cohortId 3)
      # - Negative controls: each concept from concept set "negative" mapped to
      #   cohortIds 101+.
      #
      # <Analysis Specifications>:
      # - removeSubjectsWithPriorOutcome = TRUE
      # - priorOutcomeLookBack = 99999
      # We apply priorOutcomeLookback to outcomes of interest (as in template).
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

      # Target-Comparator-Outcomes bundles -----------------------------------
      # Excluded covariate concept IDs:
      # - <Analysis Specifications> provides no explicit include/exclude concept IDs
      #   (null), so we only pass excludedCovariateConcepts (empty).
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = c(excludedCovariateConcepts$conceptId)
        )
      }

      # getDbCohortMethodDataArgs --------------------------------------------
      # <Analysis Specifications>:
      # - studyStartDate = "20030101"
      # - studyEndDate   = null
      # - maxCohortSize  = 0
      #
      # Note: The template includes restrictToCommonPeriod here, but in the
      # provided specs restrictToCommonPeriod is part of createStudyPopArgs and
      # is FALSE. We set restrictToCommonPeriod=FALSE to align with specs.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs ----------------------------------------------------------
      # <Analysis Specifications> (propensityScoreAdjustment.createPsArgs):
      # - maxCohortSizeForFitting = 250000
      # - errorOnHighCorrelation  = TRUE
      # - prior: laplace, useCrossValidation=TRUE
      # - control: tolerance=2e-7, cvType=auto, fold=10, cvRepetitions=10,
      #            noiseLevel=silent, resetCoefficients=TRUE, startingVariance=0.01
      #
      # Note: CohortMethod::createCreatePsArgs uses Cyclops::createControl; the
      # template includes some fields (seed, cvRepetitions) but not fold. We map
      # fold -> "fold" argument if supported by Cyclops; if not, Cyclops will
      # ignore unknown args. We keep seed=1 as in template for reproducibility.
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # keep FALSE to allow pipeline completion if a fit fails
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

      # Covariate balance args ------------------------------------------------
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs ---------------------------------------------------
      # <Analysis Specifications> (fitOutcomeModelArgs):
      # - modelType="cox"
      # - stratified=FALSE
      # - useCovariates=FALSE
      # - inversePtWeighting=FALSE
      # - prior: laplace, useCrossValidation=TRUE
      # - control: tolerance=2e-7, cvType=auto, fold=10, cvRepetitions=10,
      #            noiseLevel=quiet, resetCoefficients=TRUE, startingVariance=0.01
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
          startingVariance = 0.01,
          seed = 1
        )
      )

      # createStudyPopArgs ----------------------------------------------------
      # <Analysis Specifications> (createStudyPopArgs):
      # - restrictToCommonPeriod = FALSE
      # - firstExposureOnly = TRUE
      # - washoutPeriod = 365
      # - removeDuplicateSubjects = "keep all"
      # - censorAtNewRiskWindow = FALSE
      # - removeSubjectsWithPriorOutcome = TRUE
      # - priorOutcomeLookBack = 99999
      # - timeAtRisks: start=30 from cohort start; end=5475 from cohort start; minDaysAtRisk=1
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
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999
      )

      # Append analysis settings ---------------------------------------------
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          studyStartDate,
          ifelse(is.na(studyEndDate), "NA", studyEndDate),
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

# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save to JSON ----------------------------------------------------------------
# Output path follows the template pattern: inst/<studyName>/<studyName>AnalysisSpecification.json
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "iudehre", "iudehreAnalysisSpecification.json")
)