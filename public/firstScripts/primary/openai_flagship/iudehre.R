################################################################################
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# further details.
################################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from an ATLAS/WebAPI instance.
# Note: Replace baseUrl if you intend to pull from a different WebAPI.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Pull the three study cohorts specified in <Analysis Specifications>
# - Target cohort (id: 1794126, name: "target1")
# - Comparator cohort (id: 1794132, name: "comparator1")
# - Outcome cohort (id: 1794131, name: "outcome1")
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for downstream modules/analyses
# Map to canonical IDs used in this script:
#   1 = Target, 2 = Comparator, 3 = Outcome
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes:
# Use the concept set (id: 1888110, name: "negative") to construct
# a set of negative control outcome cohorts that CohortGenerator will build for us.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
  ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
  rename(
    outcomeConceptId = "conceptId",
    cohortName = "conceptName"
  ) %>%
  mutate(
    # Target/comparator/outcome cohort ids are 1, 2, 3.
    # Assign negative controls 101, 102, ... to avoid ID collisions.
    cohortId = dplyr::row_number() + 100
  ) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: Ensure no overlap between study cohort IDs (1,2,3) and NCs (101+)
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis --------
# Outcomes list (from the single outcomes cohort; ID=3 after re-numbering)
# Clean window is often used by diagnostic steps; not part of the CM TAR below.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator pairs for the CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate inclusion/exclusion lists
# From <Analysis Specifications>:
#   "covariateSelection": conceptsToInclude = [], conceptsToExclude = []
# We keep both empty; descendants can still be excluded via FeatureExtraction setting.
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)
includedCovariateConcepts <- data.frame(
  conceptId = numeric(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# CohortGeneratorModule --------------------------------------------------------
# These shared resources inform the modules which cohorts to create and
# which negative controls to auto-generate from the concept set.
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

# CohortDiagnosticsModule Settings ---------------------------------------------
# The diagnostics module will run a comprehensive set of diagnostics on
# all cohorts (study and negative controls).
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

# Study periods per <Analysis Specifications>:
#   studyStartDate = "20030101"
#   studyEndDate   = null  -> represented as empty string "" for CM args
studyPeriods <- tibble::tibble(
  studyStartDate = c("20030101"),
  studyEndDate   = c("") # empty = no end restriction
)

# Time-at-risk (TAR) per <Analysis Specifications>:
#   riskWindowStart = 30, startAnchor = "cohort start",
#   riskWindowEnd = 5475, endAnchor = "cohort start",
#   minDaysAtRisk = 1
timeAtRisks <- tibble::tibble(
  label = c("primary"),
  riskWindowStart = c(30),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(5475),
  endAnchor = c("cohort start")
)

# Propensity Score settings per <Analysis Specifications>
# Only the "match on PS" configuration is provided.
matchOnPsArgsList <- tibble::tibble(
  label = c("PS-match-1:1-caliper-0.2-std-logit"),
  maxRatio = c(1),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# No "stratify by PS" configuration in <Analysis Specifications>
stratifyByPsArgsList <- tibble::tibble(
  label = c(),
  numberOfStrata = c(),
  baseSelection = c()
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# If a data frame for "match on PS" exists and has rows, convert each row to a config
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

# If a data frame for "stratify by PS" exists and has rows, convert each row to a config
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

# Iterate through all analysis setting combinations (here: 1 period x 1 TAR x 1 PS)
cmAnalysisList <- list()
analysisId <- 1
targetComparatorOutcomesList <- list()

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]
  # Ensure end date is "" if unspecified (matches CM expectations)
  if (is.na(studyEndDate) || length(studyEndDate) == 0) {
    studyEndDate <- ""
  }

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

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

      # FeatureExtraction covariate settings
      # Apply empty include/exclude lists, and addDescendantsToExclude = TRUE as in template
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        excludedCovariateConceptIds = excludedCovariateConcepts$conceptId,
        includedCovariateConceptIds = includedCovariateConcepts$conceptId,
        addDescendantsToExclude = TRUE
      )

      # Build outcome list: primary outcome(s) + negative control outcomes
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999 # from <Analysis Specifications>
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

      # Target-Comparator-Outcome set for the analysis combination
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # No specific exposure concepts to exclude based on <Analysis Specifications>
          excludedCovariateConceptIds = c(excludedCovariateConcepts$conceptId)
        )
      }

      # getDbCohortMethodDataArgs per <Analysis Specifications>
      #   studyStartDate = "20030101"
      #   studyEndDate   = "" (no restriction)
      #   maxCohortSize  = 0
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        # The 'restrictToCommonPeriod' parameter can be left at default (FALSE)
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs per <Analysis Specifications>
      #   maxCohortSizeForFitting = 250000
      #   errorOnHighCorrelation = TRUE
      #   prior = laplace, useCrossValidation = TRUE
      #   control: tolerance = 2e-7, cvType = "auto", fold = 10,
      #            cvRepetitions = 10, noiseLevel = "silent",
      #            resetCoefficients = TRUE, startingVariance = 0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # allow full run even if PS fails in some strata
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

      # Compute covariate balance settings (kept from template)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs per <Analysis Specifications>
      #   modelType = "cox"
      #   stratified = FALSE
      #   useCovariates = FALSE
      #   inversePtWeighting = FALSE
      #   prior = laplace, useCrossValidation = TRUE
      #   control: tolerance = 2e-7, cvType = "auto", fold = 10,
      #            cvRepetitions = 10, noiseLevel = "quiet",
      #            resetCoefficients = TRUE, startingVariance = 0.01
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

      # createStudyPopArgs per <Analysis Specifications>
      #   restrictToCommonPeriod = FALSE
      #   firstExposureOnly = TRUE
      #   washoutPeriod = 365
      #   removeDuplicateSubjects = "keep all"
      #   censorAtNewRiskWindow = FALSE
      #   removeSubjectsWithPriorOutcome = TRUE
      #   priorOutcomeLookback = 99999
      #   Time-at-risk params from 'timeAtRisks' + minDaysAtRisk = 1
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
        minDaysAtRisk = 1
      )

      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          studyStartDate,
          ifelse(nchar(studyEndDate) == 0, "none", studyEndDate),
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
# Compose the Strategus analysis by adding shared resources and module settings.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Persist analysis specifications to JSON (named after <Analysis Specifications> "name": "iudehre")
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "iudehre", "iudehreAnalysisSpecification.json")
)