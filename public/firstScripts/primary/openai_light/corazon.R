################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script creates a Strategus analysis specification JSON file for the
# study "corazon" using the OHDSI Strategus package and HADES modules.
#
# The content and settings are derived from the provided Analysis Specifications.
# All cohort IDs, cohort names, and configuration names are used exactly as
# provided (no automatic renaming).
#
# Detailed inline comments explain how each part of the JSON is built and how
# the specification maps to the Analysis Specifications.
################################################################################

# Load required libraries -----------------------------------------------------
# The template primarily references Strategus and dplyr. We also load the HADES
# and supporting packages that are needed when constructing CohortMethod settings
# and when exporting/ resolving concept sets from WebAPI.
library(dplyr)
library(Strategus)
library(ROhdsiWebApi)      # for exportCohortDefinitionSet and concept set resolution
library(CohortMethod)     # for building CohortMethod analysis blocks
library(FeatureExtraction) # for covariate settings and table1 specs
library(Cyclops)          # for priors / controls
library(ParallelLogger)   # for saving JSON

# Shared Resources -------------------------------------------------------------
# Base WebAPI URL used to export cohorts and resolve concept sets
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ------------------------------------------------------------------------------
# Cohort Definitions
# ------------------------------------------------------------------------------
# Use exact cohort IDs and names from the Analysis Specifications:
#   targetCohort:     id = 1794126, name = "target1"
#   comparatorCohort: id = 1794132, name = "comparator1"
#   outcomeCohort:    id = 1794131, name = "outcome1"
#
# We export these cohort definitions from the Atlas WebAPI and then renumber
# them to a compact scheme used throughout the Strategus specification:
#   target  -> cohortId = 1
#   comparator -> cohortId = 2
#   outcome -> cohortId = 3
#
# generateStats = TRUE will ask Atlas to include cohort statistics in the
# exported definitions (helpful for diagnostics).
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts so the CohortMethod/CohortGenerator workflows use small
# consistent ids (1,2,3...). This is a common pattern in Strategus templates.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# ------------------------------------------------------------------------------
# Negative control outcomes (concept set)
# ------------------------------------------------------------------------------
# The specification provides a negative control concept set id:
#   id = 1888110, name = "negative"
#
# We resolve the concept set via WebAPI into a flat list of concepts and convert
# these into cohort-like definitions to be included in the analysis as negative
# control outcomes. We assign cohortIds starting at 101 (to avoid collision
# with the target/comparator/outcome cohorts which start at 1,2,3...).
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
  # Standardize names to the columns expected later in the template:
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  # Assign cohort ids for negative controls starting at 101:
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Validate there are no duplicate cohort IDs between main cohorts and negatives
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negative controls ***")
}

# ------------------------------------------------------------------------------
# Create small helper data frames used later to build CohortMethod settings
# ------------------------------------------------------------------------------
# Outcomes (the main outcome(s) from the cohortDefinitionSet).
# We follow the template pattern where the outcome list is built from the
# renumbered cohort definitions. Here the outcome we exported was renumbered to 3.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%         # 3 corresponds to outcome1 after renumbering
  mutate(outcomeCohortId = cohortId,
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # cleanWindow corresponds to prior-outcome lookback for the outcome
  mutate(cleanWindow = 365)

# Target-Comparator pair definitions for CohortMethod CM analyses
# Using exact names from Analysis Specifications
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  # The template refers to targetConceptId/comparatorConceptId when excluding
  # covariates. Those concept ids are not provided in the Analysis
  # Specifications, so include placeholder NA_integer_ to preserve structure.
  targetConceptId = NA_integer_,
  comparatorConceptId = NA_integer_
)

# Excluded covariate concepts:
# Analysis Specifications listed no specific covariates to exclude (empty),
# so we construct an empty data.frame with the expected columns to avoid NULLs.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# If the study wanted to explicitly include covariate concept sets, we could
# create includedCovariateConcepts here. The given Analysis Specifications had
# an empty list, so nothing is added.

# ------------------------------------------------------------------------------
# CohortGeneratorModule -------------------------------------------------------
# ------------------------------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource specification for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Shared resource specification for negative control outcome cohorts
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# CohortGenerator module specifications - generate cohorts & compute stats
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ------------------------------------------------------------------------------
# CohortDiagnosticsModule Settings --------------------------------------------
# ------------------------------------------------------------------------------
# Use common diagnostics recommended in templates:
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # run diagnostics for all exported cohorts
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
# CohortMethodModule ----------------------------------------------------------
# ------------------------------------------------------------------------------
# Setup study periods and time-at-risks based on the Analysis Specifications.
# Analysis Specifications provided a single study period and a single TAR:
#   studyPeriods: start = "20100101", end = "20191231"
#   timeAtRisks: riskWindowStart = 1, startAnchor = "cohort start",
#                riskWindowEnd = 0, endAnchor = "cohort end", minDaysAtRisk = 1
#
# Note: The dates are provided in YYYYMMDD format as strings.
studyPeriods <- tibble::tibble(
  studyStartDate = c("20100101"), # YYYYMMDD
  studyEndDate   = c("20191231")  # YYYYMMDD
)

# Define the single Time-At-Risk (TAR) using a human-readable label. The label
# can be anything meaningful; we choose "TAR_1_0_cs_ce" to reflect the window
# (1 to 0 days, cohort start to cohort end).
timeAtRisks <- tibble::tibble(
  label = c("TAR_1_0_cs_ce"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")      # "cohort start" | "cohort end"
)

# ------------------------------------------------------------------------------
# Propensity Score (PS) settings
# ------------------------------------------------------------------------------
# The Analysis Specifications indicate a single PS configuration: stratify by PS
# with numberOfStrata = 5 and baseSelection = "all". No match-on-PS settings.
#
# We construct a small table that will be converted to a psConfigList below.
stratifyByPsArgsList <- tibble::tibble(
  label = c("stratify_ps_5_all"),
  numberOfStrata = c(5),
  baseSelection = c("all") # "all" | "target" | "comparator"
)

# Empty matchOnPsArgsList because psSettings.matchOnPsArgs is null in specs
matchOnPsArgsList <- tibble::tibble(
  label = character(0),
  maxRatio = numeric(0),
  caliper = numeric(0),
  caliperScale = character(0)
)

# Build a single PS configuration list (psConfigList) used by the template loop.
psConfigList <- list()

# Convert match-on-PS rows (if any) into configurations
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

# Convert stratify-by-PS rows into configurations
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

# ------------------------------------------------------------------------------
# Build CohortMethod analysis list
# ------------------------------------------------------------------------------
cmAnalysisList <- list()
analysisId <- 1

# Iterate across studyPeriods, timeAtRisks, and PS configurations to create the
# combinations of analyses. The Analysis Specifications define single values
# for each, so this will result in a single analysis entry.
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Depending on the PS method, create the appropriate CohortMethod args.
      if (psCfg$method == "match") {
        # If matching were requested, we would create MatchOnPs args here.
        # (Not used in current Analysis Specifications.)
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
        # Build stratifyByPsArgs exactly as specified (5 strata, baseSelection = "all")
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      } else {
        stop("Unknown PS method in psConfigList")
      }

      # Covariate settings: Template uses default covariates. Analysis specs
      # did not provide custom include/exclude concept sets (they are empty),
      # so use default FeatureExtraction covariates and ensure descendants are
      # excluded where appropriate.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build outcome list:
      # - Main outcome(s) from outcome cohort(s) (outcomeOfInterest = TRUE)
      # - Negative control outcomes: derived from negativeControlOutcomeCohortSet
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
          # For negative controls: outcomeOfInterest = FALSE and trueEffectSize = 1
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Build target-comparator-outcomes list used by CohortMethod module
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        # Excluded covariate concept IDs: combine explicit target/comparator
        # concept ids (if any) plus externally specified excluded covariate concepts.
        # The Analysis Specifications had none; this will effectively be empty.
        excludedCovariateConceptIds <- c(
          cmTcList$targetConceptId[i],
          cmTcList$comparatorConceptId[i],
          excludedCovariateConcepts$conceptId
        )
        # Remove NA values introduced by placeholder NA_integer_
        excludedCovariateConceptIds <- excludedCovariateConceptIds[!is.na(excludedCovariateConceptIds)]

        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConceptIds
        )
      }

      # getDbCohortMethodDataArgs: specify the study-wide date restrictions and
      # covariate settings. Analysis Specifications include:
      #   studyPeriods: start = 20100101, end = 20191231
      #   maxCohortSize = 0  (no limit)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # use common period across cohorts for data extraction
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs: map Analysis Specifications exactly:
      #   maxCohortSizeForFitting = 250000
      #   errorOnHighCorrelation = TRUE
      #   prior: priorType = "laplace", useCrossValidation = TRUE
      #   control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      #            noiseLevel = "silent", resetCoefficients = TRUE, startingVariance = 0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        # keep stopOnError = FALSE so Strategus attempts to continue analyses even if PS fitting fails
        stopOnError = FALSE,
        estimator = "att", # default estimator for PS (att = average treatment effect on treated)
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

      # Covariate balance arguments used for diagnostics and table1 constructions
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs: use Cox, stratified = TRUE, no covariates (we rely
      # on PS stratification), prior and control parameters from specs.
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

      # createStudyPopArgs: map the CreateStudyPop args from Analysis Specifications:
      #   restrictToCommonPeriod = FALSE,
      #   firstExposureOnly = FALSE,
      #   washoutPeriod = 0,
      #   removeDuplicateSubjects = "keep all",
      #   censorAtNewRiskWindow = FALSE,
      #   removeSubjectsWithPriorOutcome = TRUE,
      #   priorOutcomeLookBack = 99999,
      #   riskWindowStart = 1,
      #   startAnchor = "cohort start",
      #   riskWindowEnd = 0,
      #   endAnchor = "cohort end",
      #   minDaysAtRisk = 1
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

      # Build a descriptive analysis label and append the analysis to cmAnalysisList
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
    }
  }
}

# Create CohortMethod module specifications
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
# Assemble full analysis specifications and save to JSON
# ------------------------------------------------------------------------------
# Create an empty Strategus analysisSpecifications and add shared resources /
# module specifications in the order they should be executed.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the generated analysis specification JSON to the inst/<studyName> folder.
# Use the exact study name from Analysis Specifications: "corazon"
outputFile <- file.path("inst", "corazon", "corazonAnalysisSpecification.json")
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  outputFile
)

# Informational message (non-fatal) about where the JSON was written
message(sprintf("Strategus analysis specification saved to: %s", outputFile))