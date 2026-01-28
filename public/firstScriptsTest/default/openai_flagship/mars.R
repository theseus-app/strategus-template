################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script creates a Strategus analysis specification JSON using the OHDSI
# Strategus package, following the provided Template and applying the exact
# settings from <Analysis Specifications>.
#
# Key customizations applied from <Analysis Specifications>:
# - Study name: "mars"
# - Cohorts (from ATLAS WebAPI):
#     Target cohort:     1794126 ("target1")  -> re-numbered to cohortId = 1
#     Comparator cohort: 1794132 ("comparator1") -> re-numbered to cohortId = 2
#     Outcome cohort:    1794131 ("outcome1") -> re-numbered to cohortId = 3
# - Negative control concept set: 1888110 ("negative") -> converted to outcome cohorts 101+
# - getDbCohortMethodDataArgs:
#     studyStartDate = 20110101, studyEndDate = 20131231
#     restrictToCommonPeriod = TRUE, maxCohortSize = 0
#     (Other fields like firstExposureOnly/washout/removeDuplicateSubjects are
#      applied at the study population level per Template usage.)
# - createStudyPopArgs:
#     restrictToCommonPeriod = FALSE, firstExposureOnly = FALSE, washoutPeriod = 0
#     removeDuplicateSubjects = "keep all"
#     censorAtNewRiskWindow = FALSE
#     removeSubjectsWithPriorOutcome = TRUE, priorOutcomeLookBack = 99999
#     time-at-risk: start=3 from "cohort start", end=90 from "cohort start", minDaysAtRisk=1
# - Propensity score adjustment:
#     matchOnPsArgs: maxRatio=1, caliper=0.2, caliperScale="standardized logit"
#     stratifyByPsArgs: NULL
#     createPsArgs: maxCohortSizeForFitting=250000, errorOnHighCorrelation=TRUE,
#                   prior laplace w/ cross-validation, Cyclops control as specified
# - fitOutcomeModelArgs:
#     modelType="cox", stratified=FALSE, useCovariates=FALSE, inversePtWeighting=FALSE,
#     prior laplace w/ cross-validation, Cyclops control as specified
#
# Notes:
# - "No name auto-correct": all names/strings are used exactly as provided.
# - Covariate include/exclude concept lists in <Analysis Specifications> contain
#   null/empty entries; therefore, this script uses default covariate settings
#   and only excludes the target/comparator concepts (as in the Template).
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# ATLAS WebAPI base URL used to export cohort definitions and resolve concept sets
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export the target, comparator, and outcome cohort definitions from WebAPI.
# We then re-number them to 1, 2, 3 to match the Template's downstream logic.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts (EXACT mapping required by the Template)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# Pull the negative control concept set definition, resolve it, and convert each
# concept into a "negative control outcome cohort" entry with cohortId 101+.
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
  mutate(
    cohortId = row_number() + 100
  ) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no overlap between main cohort IDs (1..3) and negative controls (101+)
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis --------
# Outcomes: include the primary outcome cohort (cohortId == 3).
# cleanWindow is used by some diagnostics; keep Template default (365).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Excluded covariate concepts --------------------------------------------------
# The Template excludes the target/comparator concepts from covariates to avoid
# adjusting for the exposure itself. We use the cohort names from the exported
# cohortDefinitionSet to populate conceptName fields for clarity.
#
# Note: <Analysis Specifications> provided covariateSelection conceptsToInclude/
# conceptsToExclude with null/empty entries, so we do not add additional include/
# exclude concepts beyond the exposure concepts.
excludedCovariateConcepts <- data.frame(
  conceptId = c(1794126, 1794132),
  conceptName = c("target1", "comparator1")
)

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource: cohort definitions (target/comparator/outcome)
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Shared resource: negative control outcomes derived from concept set 1888110
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications: generate cohort stats
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnoticsModule Settings ---------------------------------------------
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

# Study periods ---------------------------------------------------------------
# <Analysis Specifications> includes exactly one study period:
#   20110101 - 20131231
studyPeriods <- tibble(
  studyStartDate = c("20110101"),
  studyEndDate   = c("20131231")
)

# Time-at-risks (TARs) --------------------------------------------------------
# <Analysis Specifications> includes exactly one TAR:
#   start = 3 days after cohort start
#   end   = 90 days after cohort start
#   minDaysAtRisk = 1
#
# The Template expects a data frame with a label and TAR fields.
timeAtRisks <- tibble(
  label = c("TAR_3_90_cohort start_cohort start"),
  riskWindowStart = c(3),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(90),
  endAnchor = c("cohort start"),
  minDaysAtRisk = c(1)
)

# Propensity Score settings - match on PS -------------------------------------
# <Analysis Specifications> includes one PS setting: match on PS
matchOnPsArgsList <- tibble(
  label = c("match_maxRatio1_caliper0.2_standardized logit"),
  maxRatio = c(1),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# Propensity Score settings - stratify by PS ----------------------------------
# <Analysis Specifications> stratifyByPsArgs is null, so keep an empty tibble.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata = c(),
  baseSelection = c()
)

# Build a single PS configuration list (each entry has: method, label, params) -
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

      # Create PS adjustment args based on the selected method ----------------
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
      # <Analysis Specifications> does not provide concrete include/exclude
      # concept IDs (null/empty), so we use default covariate settings.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcomes list ---------------------------------------------------------
      # Combine the outcome of interest (cohortId=3) with negative controls (101+).
      # priorOutcomeLookback is set per <Analysis Specifications> (99999) for the
      # outcome of interest.
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

      # Target-Comparator-Outcomes mapping -----------------------------------
      # Exclude exposure concepts from covariates (Template pattern).
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = c(
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # getDbCohortMethodDataArgs --------------------------------------------
      # Apply <Analysis Specifications>:
      # - restrictToCommonPeriod = TRUE
      # - studyStartDate/studyEndDate as specified
      # - maxCohortSize = 0
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs ----------------------------------------------------------
      # Apply <Analysis Specifications> exactly:
      # - maxCohortSizeForFitting = 250000
      # - errorOnHighCorrelation = TRUE
      # - prior: laplace, useCrossValidation = TRUE
      # - control: tolerance=2e-7, cvType="auto", fold=10, cvRepetitions=10,
      #            noiseLevel="silent", resetCoefficients=TRUE, startingVariance=0.01
      #
      # Note: Template includes stopOnError and estimator; keep Template defaults.
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
          noiseLevel = "silent",
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          seed = 1,
          resetCoefficients = TRUE,
          tolerance = 2e-07,
          startingVariance = 0.01
        )
      )

      # Covariate balance args (Template defaults) ----------------------------
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs ---------------------------------------------------
      # Apply <Analysis Specifications> exactly:
      # - modelType = "cox"
      # - stratified = FALSE
      # - useCovariates = FALSE
      # - inversePtWeighting = FALSE
      # - prior: laplace, useCrossValidation = TRUE
      # - control: tolerance=2e-7, cvType="auto", fold=10, cvRepetitions=10,
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
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          seed = 1,
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          tolerance = 2e-07,
          noiseLevel = "quiet"
        )
      )

      # createStudyPopArgs ----------------------------------------------------
      # Apply <Analysis Specifications> exactly:
      # - restrictToCommonPeriod = FALSE
      # - firstExposureOnly = FALSE
      # - washoutPeriod = 0
      # - removeDuplicateSubjects = "keep all"
      # - censorAtNewRiskWindow = FALSE
      # - removeSubjectsWithPriorOutcome = TRUE
      # - priorOutcomeLookBack = 99999
      # - TAR: start=3, end=90, anchors="cohort start", minDaysAtRisk=1
      #
      # Note: CohortMethod arg name is priorOutcomeLookback (Template spelling),
      # while <Analysis Specifications> uses priorOutcomeLookBack; we pass the
      # correct CohortMethod argument name.
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
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999
      )

      # Append the settings to Analysis List ---------------------------------
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

# CohortMethod module specifications ------------------------------------------
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

# Save to JSON (Template path pattern) ----------------------------------------
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "mars", "marsAnalysisSpecification.json")
)