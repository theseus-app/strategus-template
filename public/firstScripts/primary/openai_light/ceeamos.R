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
# This script builds Strategus analysis specifications for the "ceeamos"
# study using the OHDSI Strategus conventions and the settings provided in
# the Analysis Specifications JSON.  The script follows the Template included
# with Strategus examples, but it applies the exact settings provided.
#
# IMPORTANT:
# - Names and literal strings are used exactly as provided in the Analysis
#   Specifications (no automatic renaming or "correction").
# - Detailed comments are included to make it clear how each setting is
#   mapped into Strategus / CohortMethod module specifications.
################################################################################

# ---------------------------------------------------------------------------
# Shared resources
# ---------------------------------------------------------------------------

# The WebAPI base URL used to download cohort and concept set definitions.
# This is the Atlas/WebAPI instance from which cohort definitions and concept
# sets will be exported. Change this to your Atlas/WebAPI if needed.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Study name (used to name the output specification file and directories).
studyName <- "ceeamos"

# --------------------------
# Cohort Definitions
# --------------------------
# We explicitly export the three cohorts identified in the analysis
# specifications. We then re-number them to use the internal ids 1 (target),
# 2 (comparator), and 3 (outcome) used later in the CohortMethod analysis
# definitions. We also force the cohort names to the EXACT names provided in
# the Analysis Specifications (no auto-correction).
#
# Analysis Specifications:
# - target: id = 1794126, name = "target1"
# - comparator: id = 1794132, name = "comparator1"
# - outcome: id = 1794131, name = "outcome1"
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to internal ids expected by downstream modules and
# overwrite the cohort names with the exact names provided above.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortName <- "target1"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortName <- "comparator1"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortName <- "outcome1"

# --------------------------
# Negative control concept set
# --------------------------
# We export the concept set definition (a conceptSetId was provided in the
# Analysis Specifications). We resolve it to individual concepts and convert
# them to a cohort-like table, assigning cohortIds starting at 101 to avoid
# collision with our target/comparator/outcome ids (1,2,3).
#
# Analysis Specifications:
# - negativeControlConceptSet: id = 1888110, name = "negative"
negativeControlConceptSetId <- 1888110
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = negativeControlConceptSetId,
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
  ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
  # keep the exported concept id / name but convert to the expected columns
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  # assign cohort ids for negative controls to start at 101 (template convention)
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Sanity check: ensure there are no duplicate cohort ids between the main
# cohortDefinitionSet (1,2,3) and negative controls (101+).
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# ---------------------------------------------------------------------------
# Build lists of cohorts to use in analyses
# ---------------------------------------------------------------------------

# Outcomes list (from cohortDefinitionSet). We follow the template and set
# a clean window (washout for prior outcome) equal to the createStudyPop
# priorOutcomeLookback setting (see below).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId,
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # The Analysis Specifications specify priorOutcomeLookBack = 365 --> set cleanWindow = 365
  mutate(cleanWindow = 365)

# Target / comparator pairs used in CohortMethod analyses.
# We are required to use the EXACT cohort names from the Analysis Specifications.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  # We don't have explicit concept ids for the drugs-of-interest in the specifications,
  # so we provide NA values and later omit NA when building excluded covariate ids.
  targetConceptId = NA_integer_,
  comparatorConceptId = NA_integer_,
  stringsAsFactors = FALSE
)

# Excluded covariate concepts: the Analysis Specifications provide empty include/exclude
# lists. We therefore create an empty table of excluded covariates so that no drug
# concept ids are excluded by default.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character(),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# CohortGeneratorModule (shared resources and module specification)
# ---------------------------------------------------------------------------

cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource: cohort definitions (renamed and re-numbered above).
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Shared resource: negative control outcomes (derived from the concept set).
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications: generate cohorts and stats (we set generateStats = TRUE
# to follow the template behavior).
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ---------------------------------------------------------------------------
# CohortDiagnosticsModule settings
# ---------------------------------------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# We enable the typical cohort diagnostics used in Strategus examples. The
# template sets a number of TRUE flags; we follow that convention and set the
# same value for minCharacterizationMean.
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

# ---------------------------------------------------------------------------
# CohortMethodModule settings
# ---------------------------------------------------------------------------

# Study periods: the Analysis Specifications supplied a single study period
# with null start/end values. To ensure the iteration below runs once, we
# represent that single "no restriction" period with NA values. These NA
# values are interpreted as "no restriction" when passed to CohortMethod.
studyPeriods <- tibble::tibble(
  studyStartDate = c(NA_character_), # no restriction -> NA
  studyEndDate   = c(NA_character_)  # no restriction -> NA
)

# Time-at-risk definitions: Analysis Specifications supply a single TAR:
# - riskWindowStart: 1
# - startAnchor: "cohort start"
# - riskWindowEnd: 0
# - endAnchor: "cohort end"
# - minDaysAtRisk: 1
# We'll provide a label for the TAR so it is human readable in the description.
timeAtRisks <- tibble::tibble(
  label = c("timeAtRisk1"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end"),
  minDaysAtRisk = c(1)
)

# Propensity score configurations:
# The Analysis Specifications include one PS setting: match with maxRatio = 10,
# caliper = 0.2, caliperScale = "standardized logit".
matchOnPsArgsList <- tibble::tibble(
  label = c("matchOnPs"),
  maxRatio  = c(10),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # allowed values: "propensity score" | "standardized" | "standardized logit"
)

# No stratify-by-PS settings were provided in the specifications.
stratifyByPsArgsList <- tibble::tibble(
  label = character(),
  numberOfStrata  = integer(),
  baseSelection = character()
)

# Convert PS configuration rows into a single list that will drive the iteration
psConfigList <- list()

# Convert matchOnPsArgsList (if present) into configs
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

# Convert stratifyByPsArgsList (if present) into configs
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

# ---------------------------------------------------------------------------
# Iterate over studyPeriods x timeAtRisks x psConfigList to build CM analyses
# ---------------------------------------------------------------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Build PS adjustment argument objects depending on method type
      if (psCfg$method == "match") {
        # Create matchOnPsArgs using exact specification values
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
        stop("Unknown PS method in psConfigList")
      }

      # Covariate settings: by default use the FeatureExtraction default covariate
      # generation settings. The Analysis Specifications provided empty include/exclude lists,
      # so we use the default settings while preserving descendants to exclude when requested.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build the outcome list including the outcomes of interest (from cohortDefinitionSet)
      # and the negative control outcomes (concept set converted to cohort-like items).
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            # Use priorOutcomeLookback as very large here because we handle prior
            # outcome removal at the study population creation step (priorOutcomeLookback=365).
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

      # Build targetComparatorOutcomesList: one element per row in cmTcList
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        # Build excludedCovariateConceptIds by omitting NA values
        excludedIds <- c(cmTcList$targetConceptId[i], cmTcList$comparatorConceptId[i], excludedCovariateConcepts$conceptId)
        excludedIds <- excludedIds[!is.na(excludedIds)]

        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedIds
        )
      }

      # getDbCohortMethodDataArgs: build with restrictToCommonPeriod TRUE/NA study dates,
      # and the covariate settings defined above. The Analysis Specifications set
      # maxCohortSize = 0 (no limit).
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs: set fields exactly as provided in Analysis Specifications.
      # Analysis Specifications createPsArgs:
      # - maxCohortSizeForFitting = 250000
      # - errorOnHighCorrelation = true
      # - prior: priorType = "laplace", useCrossValidation = true
      # - control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      #            noiseLevel = "silent", resetCoefficients = true, startingVariance = 0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        # stopOnError = FALSE allows Strategus to continue processing other analyses
        # even if PS fitting fails for a particular target-comparator; keeping template behavior.
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
          # Cyclops control uses 'fold' and 'cvRepetitions' for CV-based prior selection:
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "silent",
          seed = 1,
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # compute covariate balance args (shared and per-analysis). We set
      # covariateFilter for table 1 specifications as in the template.
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs: use settings from Analysis Specifications:
      # - modelType = "cox"
      # - stratified = TRUE
      # - useCovariates = FALSE
      # - inversePtWeighting = FALSE
      # - prior: laplace, useCrossValidation = TRUE
      # - control: tolerance = 2e-7, cvType = "auto", fold=10, cvRepetitions=10,
      #            noiseLevel = "quiet", resetCoefficients = TRUE, startingVariance = 0.01
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
          tolerance = 2e-07,
          noiseLevel = "quiet",
          seed = 1,
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # createStudyPopArgs: mapped from Analysis Specifications. Exact mapping:
      # - restrictToCommonPeriod = false
      # - firstExposureOnly = false
      # - washoutPeriod = 365
      # - removeDuplicateSubjects = "remove all"
      # - censorAtNewRiskWindow = false
      # - removeSubjectsWithPriorOutcome = true
      # - priorOutcomeLookBack = 365
      # - time-at-risk values taken from timeAtRisks[t,]
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
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        # set a large maxDaysAtRisk to avoid artificial truncation
        maxDaysAtRisk = 99999
      )

      # Append the CohortMethod analysis specification to the list
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(is.na(studyStartDate), "", studyStartDate),
          ifelse(is.na(studyEndDate), "", studyEndDate),
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
    } # end for p (ps configs)
  } # end for t (TARs)
} # end for s (study periods)

# Create the CohortMethod module specifications object
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# ---------------------------------------------------------------------------
# Assemble full analysis specifications and write to JSON
# ---------------------------------------------------------------------------

analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specification JSON to inst/<studyName>/<studyName>AnalysisSpecification.json
# We follow the template naming convention but replace "studyName" with the exact study name "ceeamos".
outputFile <- file.path("inst", studyName, paste0(studyName, "AnalysisSpecification.json"))
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  outputFile
)

# Print the output path for user convenience (can be removed if not desired)
cat(sprintf("Analysis specifications saved to: %s\n", outputFile))