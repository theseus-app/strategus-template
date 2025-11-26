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
# This script builds a Strategus analysis specification JSON for the "ceeamos"
# analysis using the OHDSI Strategus and related HADES modules (CohortGenerator,
# CohortDiagnostics, CohortMethod).  The content and settings are taken
# verbatim from the provided Analysis Specifications JSON.
#
# IMPORTANT: Comments in this file document how each setting from the Analysis
# Specifications is applied. Do not rename the IDs or cohort names; the exact
# cohort IDs from the specifications are used below.
################################################################################

# Shared Resources -------------------------------------------------------------
# The Atlas/WebAPI base URL used to fetch cohort definitions and concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Use the exact cohort IDs provided in the Analysis Specifications JSON.
# - Target cohort id: 1794126  (name: "target1")
# - Comparator cohort id: 1794132  (name: "comparator1")
# - Outcome cohort id: 1794131  (name: "outcome1")
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to the canonical 1 (target), 2 (comparator), 3 (outcome)
# This is necessary because the downstream module specifications (CohortMethod
# settings) commonly reference cohorts by small integers.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, "cohortId"] <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, "cohortId"] <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, "cohortId"] <- 3

# Negative control outcomes
# Use the exact concept set id from the Analysis Specifications JSON:
# negativeControlConceptSet$id = 1888110
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
  ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  mutate(
    # Allocate cohort ids for negative controls starting at 101 so they don't
    # collide with the study target/comparator/outcome cohort ids (1,2,3).
    cohortId = row_number() + 100
  ) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Fail early if any cohort ids collide (sanity check)
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# Prepare lists / data frames for CohortMethod configuration --------------------
# Outcomes: Build oList from the outcome cohort (renumbered to 3)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # Use a 365-day clean window by default (typical choice; adapt if needed)
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis (renumbered ids 1 & 2)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",     # exact name from Analysis Specifications
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1" # exact name from Analysis Specifications
)

# Excluded covariate concepts: Analysis Specifications contained empty lists
# for covariatesToInclude / covariatesToExclude. We create an empty data.frame
# so downstream code can safely reference it.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# CohortGeneratorModule --------------------------------------------------------
# Create shared resources for the cohort definitions and negative controls.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource that contains the cohort definitions we exported above.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

# Shared resource for negative control outcome cohorts created from the concept set.
# occurrenceType = "first" (detect the first occurrence) and detectOnDescendants = TRUE
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specification: have the cohort generator produce statistics for the
# exported cohorts (generateStats = TRUE).
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule -----------------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Configure cohort diagnostics to run a wide set of diagnostics. These options
# are typical and consistent with the template; they produce inclusion stats,
# incidence, etc. The minimum characterization mean is set to 0.01 as in template.
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
# Build CohortMethod analysis list according to the Analysis Specifications.

# studyPeriods:
# The Analysis Specifications provided a single study period entry with empty
# strings for studyStartDate / studyEndDate. Following the template guidance,
# an empty string indicates no restriction to a specific time window (i.e.,
# include all available time).
studyPeriods <- tibble::tibble(
  studyStartDate = c(""), # empty string => no restriction
  studyEndDate   = c("")  # empty string => no restriction
)

# Time-at-risks (TARs) - taken directly from createStudyPopArgs in the JSON:
# There are two TARs specified. We include the label field to describe each TAR.
timeAtRisks <- tibble::tibble(
  label = c("TAR_1_cohort_start_to_cohort_end", "TAR_2_cohort_start_to_cohort_start"),
  riskWindowStart  = c(1, 1),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd  = c(0, 99999),
  endAnchor = c("cohort end", "cohort start"),
  minDaysAtRisk = c(1, 1)
)

# Propensity Score settings - from propensityScoreAdjustment in the JSON:
# There is a single PS setting using match with maxRatio=10, caliper=0.2,
# caliperScale="standardized logit".
matchOnPsArgsList <- tibble::tibble(
  label = c("match_ps_default"),
  maxRatio = c(10),
  caliper = c(0.2),
  caliperScale = c("standardized logit") # exact string from JSON
)

# No stratify-by-PS settings provided in the JSON -> empty tibble
stratifyByPsArgsList <- tibble::tibble(
  label = character(0),
  numberOfStrata = integer(0),
  baseSelection = character(0)
)

# Build a single PS configuration list (psConfigList) from the match and
# stratify tables. This approach mirrors the template: each row becomes one
# configuration entry that eventually produces a CohortMethod analysis.
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

# Iterate through all analysis setting combinations to build cmAnalysisList
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create matchOnPsArgs / stratifyByPsArgs based on the PS configuration
      if (psCfg$method == "match") {
        # Use CohortMethod::createMatchOnPsArgs with the parameters from JSON.
        # Note: allowReverseMatch and stratificationColumns are set to safe defaults.
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
        stop("Unknown PS method: ", psCfg$method)
      }

      # Covariate settings: the Analysis Specifications did not provide
      # explicit covariate inclusions/exclusions (empty lists). We use the
      # default covariate settings while ensuring descendants are excluded if
      # needed (template set addDescendantsToExclude = TRUE).
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build a combined outcome list: the main outcome(s) from oList plus
      # the negative control cohorts constructed from the concept set.
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
          # Negative controls are labeled outcomeOfInterest = FALSE with
          # trueEffectSize = 1 (null)
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # For CohortMethod target-comparator-outcome mapping build a single list
      # entry for each target-comparator pair (we only have one pair).
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Excluded covariate concept ids: use the (empty) excludedCovariateConcepts
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # getDbCohortMethodDataArgs: createGetDbCohortMethodDataArgs parameters.
      # According to the JSON: restrictToCommonPeriod = false, studyStartDate/studyEndDate empty,
      # maxCohortSize = 0, and covariateSettings as created above.
      # NOTE: Use if (...) NULL else ... instead of ifelse(..., NULL, ...) to avoid
      # errors when supplying NULL as replacement in ifelse.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = if (identical(studyStartDate, "")) NULL else studyStartDate,
        studyEndDate = if (identical(studyEndDate, "")) NULL else studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs: translate the createPsArgs block from the JSON.
      # JSON fields:
      #   maxCohortSizeForFitting = 250000
      #   errorOnHighCorrelation = true
      #   prior: priorType = "laplace", useCrossValidation = true
      #   control: tolerance = 2e-7, cvType = "auto", fold = 10,
      #            cvRepetitions = 10, noiseLevel = "silent",
      #            resetCoefficients = true, startingVariance = 0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # keep execution going across analyses if a PS fit fails
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",
          exclude = c(0),
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          tolerance = 2e-7,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "silent",
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          seed = 1
        )
      )

      # Covariate balance computation args: a shared and a per-analysis setting.
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs: from JSON's fitOutcomeModelArgs block
      # modelType = "cox", stratified = TRUE, useCovariates = FALSE,
      # inversePtWeighting = FALSE; prior = laplace with CV; control as specified.
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = TRUE,
        useCovariates = FALSE,
        inversePtWeighting = FALSE,
        prior = Cyclops::createPrior(
          priorType = "laplace",
          exclude = c(0),
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          tolerance = 2e-7,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          seed = 1
        )
      )

      # createStudyPopArgs: settings copied from JSON createStudyPopArgs
      # JSON fields:
      #   restrictToCommonPeriod = false
      #   firstExposureOnly = false
      #   washoutPeriod = 0
      #   removeDuplicateSubjects = "keep all"
      #   censorAtNewRiskWindow = false
      #   removeSubjectsWithPriorOutcome = true
      #   priorOutcomeLookBack = 99999
      #   two time-at-risk specifications defined in timeAtRisks
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

      # Append the CohortMethod analysis to cmAnalysisList
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s - %s; TAR: %s; PS: %s",
          ifelse(studyStartDate == "", "ALL", studyStartDate),
          ifelse(studyEndDate == "", "ALL", studyEndDate),
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

# Build CohortMethod module specifications object
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the final analysis specifications object by combining shared resources
# and module specifications. The order here follows the typical execution order:
# 1) cohort generator shared resources and module, 2) cohort diagnostics module,
# 3) cohort method module.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() %>%
  Strategus::addSharedResources(cohortDefinitionShared) %>%
  Strategus::addSharedResources(negativeControlsShared) %>%
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save analysis specification JSON to inst/<studyName>/<studyName>AnalysisSpecification.json
# Use the analysis name exactly as provided in the Analysis Specifications: "ceeamos"
outFile <- file.path("inst", "ceeamos", "ceeamosAnalysisSpecification.json")
dir.create(dirname(outFile), recursive = TRUE, showWarnings = FALSE)
ParallelLogger::saveSettingsToJson(analysisSpecifications, outFile)

# Informational message (optional) to indicate where the file was written.
message("Saved Strategus analysis specification to: ", outFile)