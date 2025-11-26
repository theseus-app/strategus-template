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
# Get the list of cohorts from the OHDSI ATLAS/WebAPI server. The cohort IDs and
# the negative control concept set ID are taken EXACTLY from <Analysis Specifications>.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions (Target, Comparator, Outcome)
# IDs come directly from the <Analysis Specifications>:
#  - targetCohort:    id = 1794126 (name = "target1")
#  - comparatorCohort: id = 1794132 (name = "comparator1")
#  - outcomeCohort:   id = 1794131 (name = "outcome1")
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to a contiguous 1, 2, 3 as used by many CM workflows and to
# avoid potential collisions with negative control cohorts (which will start at 101).
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes
# From <Analysis Specifications>:
#  - negativeControlConceptSet: id = 1888110 (name = "negative")
# We resolve the concept set to a list of standard conceptIds to use for negative
# control outcome cohort generation.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
  ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  mutate(
    # Assign negative control cohort IDs starting at 101 so they do not collide
    # with target = 1, comparator = 2, outcome = 3.
    cohortId = dplyr::row_number() + 100
  ) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure there are no duplicate cohort IDs across main and negative
# control cohorts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found across main and negative control cohorts ***")
}

# Create small helper data frames to use in the CohortMethod setup -------------
# Outcomes list (interest outcomes only; the negative controls will be added later)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # typical clean window; can be tuned as needed

# Target and Comparator mapping for the CohortMethod analysis (names reflect
# EXACT names from <Analysis Specifications>)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate selection settings (EXACT from <Analysis Specifications>):
# conceptsToInclude = [], conceptsToExclude = []
# Here we keep the default covariate settings and do not force include/exclude
# specific concepts. If you later need to exclude the exposure ingredients, add
# them to this data frame.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# Optional include list (kept empty per <Analysis Specifications>)
# includedCovariateConcepts <- data.frame(
#   conceptId = integer(),
#   conceptName = character()
# )

# CohortGeneratorModule --------------------------------------------------------
#  - Shares the main cohort definitions and the negative control outcome concept set
#  - Will also generate statistics for the cohorts
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

# CohortDiagnosticsModule Settings ---------------------------------------------
# Using template defaults; this will produce a broad diagnostic set for the
# cohorts we defined and re-numbered above (1, 2, 3 and the negative controls).
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
# Settings below map EXACTLY to <Analysis Specifications>.

# Study period: from <Analysis Specifications> getDbCohortMethodDataArgs.studyPeriods = [{start=null, end=null}]
# That means "no restriction". We represent that by a single row of NA, and later
# pass empty strings to the CM args (interpreted as unrestricted).
studyPeriods <- tibble(
  studyStartDate = NA_character_,  # no restriction (null)
  studyEndDate   = NA_character_   # no restriction (null)
)

# Time-at-risk (TAR): from <Analysis Specifications> createStudyPopArgs.timeAtRisks
# Only one TAR: start = 1 day after "cohort start", end = "cohort end"
timeAtRisks <- tibble(
  label = "TAR-1",
  riskWindowStart = 1,
  startAnchor = "cohort start",
  riskWindowEnd = 0,
  endAnchor = "cohort end",
  minDaysAtRisk = 1
)

# Propensity score adjustment settings: EXACT per <Analysis Specifications>
# - Only "match on PS" is specified
matchOnPsArgsList <- tibble(
  label = "PS-Match-1",
  maxRatio = 1,
  caliper = 0.2,
  caliperScale = "standardized logit"
)

# No PS stratification configuration is provided (null in <Analysis Specifications>)
stratifyByPsArgsList <- tibble(
  label = character(),
  numberOfStrata = integer(),
  baseSelection = character()
)

# Build a single PS configuration list (each entry: method, label, params)
psConfigList <- list()

# Convert each "match on PS" row to a config entry
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

# Convert each "stratify by PS" row to a config entry (none in this analysis)
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

# Now iterate through all combinations to construct the CohortMethod analyses.
cmAnalysisList <- list()
targetComparatorOutcomesList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  # Convert NA study dates to empty strings to indicate "unrestricted"
  studyStartDate <- ifelse(is.na(studyPeriods$studyStartDate[s]), "", studyPeriods$studyStartDate[s])
  studyEndDate   <- ifelse(is.na(studyPeriods$studyEndDate[s]), "", studyPeriods$studyEndDate[s])

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create PS adjustment args from the selected method
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
        stop("Unknown PS config method: ", psCfg$method)
      }

      # Covariate settings for FeatureExtraction
      # We use default covariates and allow adding descendants to exclusions,
      # which is common when excluding exposure ingredients.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcomes list for CM:
      #  - Include the main outcome (outcomeOfInterest = TRUE)
      #  - Include negative controls (outcomeOfInterest = FALSE)
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

      # Target and comparator pairs; supply any excluded covariate concepts.
      # Note: The <Analysis Specifications> do not specify conceptIds for target/
      # comparator exposures to exclude, so we only pass the (empty) exclusion set.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Pull data and construct PS covariates per <Analysis Specifications>
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # PS creation args: EXACT mapping from <Analysis Specifications>
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
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

      # Balance computation settings (Table 1 for post-PS and shared for diagnostics)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model args: EXACT mapping from <Analysis Specifications>
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

      # Study population args: EXACT mapping from <Analysis Specifications>
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
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t]
      )

      # Build and collect the analysis setting
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "StudyPeriod: %s-%s; TAR: %s; PS: %s",
          ifelse(nchar(studyStartDate) == 0, "NA", studyStartDate),
          ifelse(nchar(studyEndDate) == 0, "NA", studyEndDate),
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

# Package all CM settings into the Strategus module specification
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
# Add the shared resources and module specifications to a single Strategus spec object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the JSON to a study-specific folder using the EXACT analysis "name"
# from <Analysis Specifications>: "antivegfkidney"
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "antivegfkidney", "antivegfkidneyAnalysisSpecification.json")
)