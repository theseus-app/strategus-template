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
# WebAPI source for cohort and concept set assets
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Pull the three cohorts (target, comparator, outcome) from WebAPI by their IDs
# Provided in <Analysis Specifications>:
#   targetCohort:    id = 1794126, name = "target1"
#   comparatorCohort:id = 1794132, name = "comparator1"
#   outcomeCohort:   id = 1794131, name = "outcome1"
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts so Strategus modules can rely on a simple, consistent ID space
# 1 -> target, 2 -> comparator, 3 -> outcome
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# From <Analysis Specifications>:
#   negativeControlConceptSet: id = 1888110, name = "negative"
# This fetches and resolves the concept set to the set of descendant concepts,
# then prepares them for the CohortMethod negative control outcome configuration.
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
  mutate(
    # Assign unique cohort IDs to NC outcomes after the main cohorts (start at 101)
    cohortId = dplyr::row_number() + 100
  ) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Defensive check: ensure no ID conflicts between primary cohorts and NC outcomes
if (length(intersect(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)) > 0) {
  stop("*** Error: duplicate cohort IDs found between main cohorts and negative controls ***")
}

# Create helper data frames used in module settings ----------------------------
# Outcomes list used in CohortMethod (only the primary outcome here; NC outcomes are added separately)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # Optional: define a "clean window" if used by downstream logic (not used directly here)
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Use EXACT names from <Analysis Specifications>
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate inclusion/exclusion ------------------------------------------------
# From <Analysis Specifications>: covariateSelection has empty include/exclude lists.
# Here we keep empty frames to signify "no manual include/exclude constraints".
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)
# If you want to define covariates to include instead of including them all, create a data.frame
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# Shared resources for cohort definitions and negative controls are declared here
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",      # detect first occurrence of NC outcomes
  detectOnDescendants = TRUE     # include descendant concepts for detection
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE           # produce cohort generation diagnostics/statistics
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# Run a broad set of cohort diagnostics for the main cohorts (IDs 1,2,3)
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
# Study periods: from <Analysis Specifications> "studyPeriods" has a single row with null start/end.
# Represent this as empty strings (no restriction), but still create one row so an analysis is created.
studyPeriods <- tibble::tibble(
  studyStartDate = c(""), # no restriction
  studyEndDate   = c("")  # no restriction
)

# Time-at-risk (TAR) windows: from <Analysis Specifications>
# One TAR: start = 1 day after cohort start, end at cohort end, minDaysAtRisk = 1
timeAtRisks <- tibble::tibble(
  label = c("TAR1"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"), # EXACT from specification
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")      # EXACT from specification
)

# Propensity Score settings:
# From <Analysis Specifications> PS adjustment is matching with:
#   maxRatio = 10, caliper = 0.2, caliperScale = "standardized logit"
matchOnPsArgsList <- tibble::tibble(
  label = c("Match-1"),
  maxRatio  = c(10),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# No PS stratification specified in <Analysis Specifications>
stratifyByPsArgsList <- tibble::tibble(
  label = character(0),
  numberOfStrata = integer(0),
  baseSelection = character(0)
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Convert each "match on PS" row to a PS config
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

# Convert each "stratify by PS" row to a PS config (none for this study)
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

# Iterate through all analysis setting combinations ----------------------------
cmAnalysisList <- list()
analysisId <- 1
targetComparatorOutcomesList <- list() # declared to capture TCOs for the module spec

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create PS matching/stratification args based on the "method" selected
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

      # Covariate settings:
      # From <Analysis Specifications> covariateSelection is empty, so we use default covariates
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcomes: include the primary outcome of interest plus the NC outcomes
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

      # Target-Comparator-Outcome specification
      # Excluded covariate concept IDs are empty in this study (no manual exclusions provided)
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

      # Data fetching args for CohortMethod:
      # From <Analysis Specifications> maxCohortSize = 0 (no cap), no date restriction
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,  # set as in template (study-wide restriction happens in createStudyPopArgs below per spec)
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # PS creation args:
      # From <Analysis Specifications> (createPsArgs)
      #  maxCohortSizeForFitting = 250000
      #  errorOnHighCorrelation = TRUE
      #  prior: laplace, useCrossValidation = TRUE
      #  control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10, noiseLevel = "silent",
      #           resetCoefficients = TRUE, startingVariance = 0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # keep FALSE so the overall pipeline can complete even if a PS model fails
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
          startingVariance = 0.01,
          seed = 1
        )
      )

      # Covariate balance args (default table1 specs for reporting)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model args:
      # From <Analysis Specifications> (fitOutcomeModelArgs)
      #  modelType = "cox", stratified = TRUE, useCovariates = FALSE, inversePtWeighting = FALSE
      #  prior: laplace with CV
      #  control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10, noiseLevel = "quiet",
      #           resetCoefficients = TRUE, startingVariance = 0.01
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
          startingVariance = 0.01,
          seed = 1
        )
      )

      # Study population args:
      # From <Analysis Specifications> (createStudyPopArgs)
      #  restrictToCommonPeriod = TRUE
      #  firstExposureOnly = TRUE
      #  washoutPeriod = 365
      #  removeDuplicateSubjects = "keep all"
      #  censorAtNewRiskWindow = TRUE
      #  removeSubjectsWithPriorOutcome = TRUE
      #  priorOutcomeLookBack = 99999
      #  timeAtRisks: mapped from the timeAtRisks tibble for this iteration
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

      # Append the analysis configuration to the list
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(nchar(studyStartDate) == 0, "noStart", studyStartDate),
          ifelse(nchar(studyEndDate) == 0, "noEnd", studyEndDate),
          timeAtRisks$label[t],
          psCfg$label
        ),
        getDbCohortMethodDataArgs = getDbCohortMethodDataArgs,
        createStudyPopArgs = createStudyPopArgs,
        createPsArgs = createPsArgs,
        matchOnPsArgs = if (exists("matchOnPsArgs")) matchOnPsArgs else NULL,
        stratifyByPsArgs = if (exists("stratifyByPsArgs")) stratifyByPsArgs else NULL,
        computeSharedCovariateBalanceArgs = computeSharedCovariateBalanceArgs,
        computeCovariateBalanceArgs = computeCovariateBalanceArgs,
        fitOutcomeModelArgs = fitOutcomeModelArgs
      )
      analysisId <- analysisId + 1
    }
  }
}

# Build CohortMethod module specifications
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the analysis specifications -------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save to JSON (use EXACT study name from <Analysis Specifications>: "uveitissafety")
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "uveitissafety", "uveitissafetyAnalysisSpecification.json")
)