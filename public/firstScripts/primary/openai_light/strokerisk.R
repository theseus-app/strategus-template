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
# This script builds an analysis specification JSON for Strategus based on the
# provided analysis settings. The script follows the structure in the template
# and applies the exact names and IDs supplied in the Analysis Specifications.
#
# Key settings come from the <Analysis Specifications> block:
# - Analysis name: "strokerisk"
# - Cohorts:
#     target: cohortId = 1794126, name = "target1"
#     comparator: cohortId = 1794132, name = "comparator1"
#     outcome: cohortId = 1794131, name = "outcome1"
# - Negative control concept set id: 1888110 (will be expanded to negative control
#   outcome cohorts)
# - CohortMethod/Covariate/PS/Outcome model settings are applied below.
#
# The following code is annotated to explain how each block maps to the
# specification fields.
################################################################################

# Shared Resources -------------------------------------------------------------
# WebAPI base URL used to fetch cohort definitions and concept sets.
# Change this to your Atlas WebAPI endpoint if required.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ------------------------------------------------------------------------------
# Cohort Definitions
# ------------------------------------------------------------------------------
# Export the cohort definitions listed in the analysis specification using the
# exact cohort IDs supplied. We pass generateStats = TRUE to keep additional
# metadata (optional).
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to 1, 2, 3 for internal use in the Strategus specs.
# This mapping is required by the downstream modules which expect small
# sequential cohort ids for target/comparator/outcomes.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# ------------------------------------------------------------------------------
# Negative control outcomes
# ------------------------------------------------------------------------------
# The spec provides a concept set id for negative controls: 1888110. We fetch the
# concept set definition, resolve descendants, retrieve concepts and convert
# them into a negativeControlOutcomeCohortSet that Strategus expects.
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
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Basic duplication check to ensure no cohortId overlaps between the main
# cohortDefinitionSet and the negative control cohort ids.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between main cohorts and negative control cohorts ***")
}

# ------------------------------------------------------------------------------
# Prepare lists used for analyses
# ------------------------------------------------------------------------------
# Outcomes (oList):
# Select the single outcome cohort we imported (renumbered to cohortId == 3).
# We attach a default cleanWindow of 365 days similar to the template example.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId,
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for CohortMethod analysis (cmTcList).
# Use the exact cohort names from the Analysis Specifications.
# We also create placeholder columns targetConceptId and comparatorConceptId so
# that the template code which references them will run. They are left NA
# because the spec did not provide specific concept IDs to exclude.
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
# The Analysis Specifications include empty concept lists for include/exclude.
# We therefore set an empty data.frame for excludedCovariateConcepts. If you
# want to exclude specific conceptIds (e.g. the drug exposures themselves),
# put them here as integers and names.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# Optional included covariate concept list is also empty in the spec;
# leaving it commented here to show how one would set it.
# includedCovariateConcepts <- data.frame(conceptId = c(), conceptName = c())

# CohortGeneratorModule --------------------------------------------------------
# Create shared resources & module specifications for cohort generation.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Negative controls shared resource (use first occurrence and detect on descendants)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Cohort generation module specification (we request generation of cohort stats)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
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
# Build studyPeriods and timeAtRisks from the specification.
# getDbCohortMethodDataArgs.studyPeriods was provided as one period:
#   studyStartDate: "20010101", studyEndDate: "20171231"
studyPeriods <- tibble::tibble(
  studyStartDate = c("20010101"),
  studyEndDate   = c("20171231")
)

# The createStudyPopArgs.timeAtRisks in the spec contains a single TAR:
# riskWindowStart = 1 (startAnchor = "cohort start")
# riskWindowEnd = 0 (endAnchor = "cohort end")
# minDaysAtRisk = 1
# We'll create a label for the TAR so it can be referenced in descriptions.
timeAtRisks <- tibble::tibble(
  label = c("TAR_1_start1_end0"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end") # "cohort start" | "cohort end"
)

# Propensity Score (PS) configuration:
# The Analysis Specifications define a single PS config using matching with:
#   maxRatio = 10, caliper = 0.2, caliperScale = "standardized logit"
matchOnPsArgsList <- tibble::tibble(
  label = c("match_on_ps"),
  maxRatio  = c(10),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # matches the spec text
)

# There are no stratification PS settings in the spec so leave this empty
stratifyByPsArgsList <- tibble::tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0)
)

# Build a list of PS adjustments (psConfigList) by converting the PS data frames
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

# Iterate through analysis setting combinations to create CM analyses.
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create matchOnPsArgs or stratifyByPsArgs based on psCfg method.
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
        stop("Unknown PS method encountered.")
      }

      # Covariate settings:
      # The spec left covariate selection lists empty; we use the default
      # covariate settings but ensure descendants are excluded for the
      # "excluded" concept list if one is specified.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build the outcome list. We include the outcome(s) of interest (from
      # oList) and append all negative controls we resolved earlier.
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

      # Build the per-target/comparator outcome list used by the CohortMethod module:
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Build excluded covariate concept IDs vector by combining the (possibly NA)
          # target/comparator concept ids and any explicitly excluded covariate concepts.
          excludedCovariateConceptIds = c(
            cmTcList$targetConceptId[i],
            cmTcList$comparatorConceptId[i],
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # getDbCohortMethodDataArgs:
      # The Analysis Specifications provided studyPeriods and maxCohortSize. We
      # pass the study window and the covariate settings created above. The
      # template had restrictToCommonPeriod = TRUE; we'll set it to TRUE here to
      # retrieve only records in the study window for cohort creation.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs:
      # Apply the createPsArgs from the Analysis Specifications. Note that the
      # spec requests regularization with a Laplace prior and cross-validation.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # allow Strategus to continue if a single fit fails
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",
          exclude = c(0),
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          tolerance = 2e-7,
          cvType = "auto",
          seed = 1,
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "silent",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Covariate balance computation args:
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs:
      # Use Cox model, stratified = TRUE, no covariates included (effect estimated
      # after PS matching/stratification). Prior and control match the spec.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
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
          seed = 1,
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          tolerance = 2e-7
        )
      )

      # createStudyPopArgs:
      # Apply the exact fields from the Analysis Specifications:
      # - restrictToCommonPeriod = false
      # - firstExposureOnly = false
      # - washoutPeriod = 0
      # - removeDuplicateSubjects = "keep all"
      # - censorAtNewRiskWindow = false
      # - removeSubjectsWithPriorOutcome = true
      # - priorOutcomeLookBack = 99999
      # - risk window settings from timeAtRisks
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

      # Append the CM analysis configuration to the list. The description includes
      # the study period, TAR label and the PS label for clarity.
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

# Create the CohortMethod module specifications.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the analysis specifications object by composing shared resources and
# module specifications. This object is what Strategus will consume when
# executing the study pipeline.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the specification JSON to inst/<analysisName>/<analysisName>AnalysisSpecification.json
# Use the exact analysis name from the spec: "strokerisk"
outDir <- file.path("inst", "strokerisk")
if (!dir.exists(outDir)) {
  dir.create(outDir, recursive = TRUE)
}
outfile <- file.path(outDir, "strokeriskAnalysisSpecification.json")

ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  outfile
)

message("Strategus analysis specification saved to: ", outfile)