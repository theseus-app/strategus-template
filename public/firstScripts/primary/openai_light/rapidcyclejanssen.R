library(dplyr)
library(Strategus)

# ------------------------------------------------------------------------------
# CreateStrategusAnalysisSpecification.R
#
# This script creates an analysis specification JSON for Strategus based on the
# provided Analysis Specifications and the example Template. It uses the OHDSI
# Strategus package to assemble:
#   - CohortGenerator module specifications
#   - CohortDiagnostics module specifications
#   - CohortMethod module specifications
#
# IMPORTANT:
# - This file intentionally uses the exact names provided in the Analysis
#   Specifications (e.g. cohort ids, concept set ids, and the analysis name).
# - The resulting JSON is saved to inst/<analysisName>/<analysisName>AnalysisSpecification.json
#
# Edit only the baseUrl (if needed) and the output path if you want to store the
# JSON in a different location.
# ------------------------------------------------------------------------------

# Base WebAPI URL for cohort exports (change to your Atlas WebAPI if required)
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ------------------------------------------------------------------------------
# Cohort Definitions
# ------------------------------------------------------------------------------
# Export cohort definitions from the WebAPI. We request generateStats = TRUE so
# that cohort generation statistics are available as shared resources.
#
# We use the exact cohort IDs provided in the Analysis Specifications:
#   - target cohort id:      1794126 (name: target1)
#   - comparator cohort id: 1794132 (name: comparator1)
#   - outcome cohort id:    1794131 (name: outcome1)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts so they are 1, 2, 3 for target/comparator/outcome.
# This is convenient for building the CohortMethod structures later.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# ------------------------------------------------------------------------------
# Negative control outcomes - concept set
# ------------------------------------------------------------------------------
# Use the exact negative control concept set id provided: 1888110
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
    # Assign cohort ids for negative control outcome cohorts starting at 101
    # (so they won't conflict with target/comparator/outcome cohort ids = 1,2,3...)
    cohortId = row_number() + 100
  ) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check - ensure no duplicate ids across cohortDefinitionSet and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# ------------------------------------------------------------------------------
# Build lists/tables used by the CohortMethod module
# ------------------------------------------------------------------------------
# Outcomes (primary outcomes list). We create entries for the outcome cohorts
# defined in cohortDefinitionSet where cohortId == 3 (renumbered outcome).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(
    outcomeCohortId = cohortId,
    outcomeCohortName = cohortName
  ) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # A cleanWindow corresponds to the "prior outcome lookback" to define incident events.
  # We set 365 days here (common default) — adjust if needed.
  mutate(cleanWindow = 365)

# Target and Comparator mapping for CohortMethod
# Use exact names from the Analysis Specifications
# Note: we include targetConceptId and comparatorConceptId columns (NA) to avoid
# errors where the template references those columns; these are intentionally NA
# because the Analysis Specifications did not supply specific concept ids to
# exclude for the target/comparator.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  targetConceptId = NA_integer_,
  comparatorConceptId = NA_integer_
)

# Excluded covariate concepts: The Analysis Specifications included an empty
# list for conceptsToExclude. We therefore create an empty data.frame with the
# expected columns so downstream code can safely reference it.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# ------------------------------------------------------------------------------
# CohortGeneratorModule shared resources and module specifications
# ------------------------------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource containing exported cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Shared resources for negative control outcome cohorts (first occurrence, include descendants)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create cohort generator module specifications (generates cohorts and cohort stats)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ------------------------------------------------------------------------------
# CohortDiagnosticsModule settings
# ------------------------------------------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# We run a broad set of diagnostics (mirrors the Template)
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

# ------------------------------------------------------------------------------
# CohortMethodModule settings
# ------------------------------------------------------------------------------
# Study periods: use the study period provided in the Analysis Specifications.
# The specification includes a single study period with studyStartDate = 20210101
# and studyEndDate = null. When studyEndDate is unknown we use NA.
studyPeriods <- tibble::tibble(
  studyStartDate = c(20210101), # YYYYMMDD
  studyEndDate   = c(NA_integer_)
)

# Time-at-risk (TAR) definitions: the Analysis Specifications includes a single
# risk window from 1 to 14 days anchored on 'cohort start'. We add a label to
# identify the TAR in the analysis descriptions.
timeAtRisks <- tibble::tibble(
  label = c("1_to_14_days"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"), # allowed: "cohort start" | "cohort end"
  riskWindowEnd  = c(14),
  endAnchor = c("cohort start")    # allowed: "cohort start" | "cohort end"
)

# Propensity score / PS adjustment settings:
# The Analysis Specifications define one PS setting: match with maxRatio=100,
# caliper=0.2, caliperScale="standardized logit".
matchOnPsArgsList <- tibble::tibble(
  label = c("matchOnPs"),
  maxRatio  = c(100),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # allowed values e.g. "propensity score" | "standardized" | "standardized logit"
)

# No stratified PS settings supplied in the Analysis Specifications
stratifyByPsArgsList <- tibble::tibble(
  label = character(0),
  numberOfStrata = integer(0),
  baseSelection = character(0)
)

# Build psConfigList from matchOnPsArgsList and stratifyByPsArgsList
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

# ------------------------------------------------------------------------------
# Build CohortMethod analyses (iterate over study periods, TARs, and PS configs)
# ------------------------------------------------------------------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Configure PS adjustment method (match or stratify)
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
        stop("Unknown PS configuration method: ", psCfg$method)
      }

      # Covariate settings: we use default covariate extraction settings.
      # The Template uses addDescendantsToExclude = TRUE - preserve that behavior.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build the outcome list for CohortMethod:
      # - true outcomes (the study outcome) with priorOutcomeLookback = 99999 (per spec)
      # - negative control outcomes (from negativeControlOutcomeCohortSet) as non-target outcomes
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

      # Build target-comparator-outcome combinations for this analysis.
      # Excluded covariate concept ids combines:
      #  - any target-specific concept id (NA if none)
      #  - any comparator-specific concept id (NA if none)
      #  - the excludedCovariateConcepts$conceptId (empty here)
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        # Combine excluded covariate ids into a single integer vector (dropping NA)
        excludedIds <- c(cmTcList$targetConceptId[i], cmTcList$comparatorConceptId[i], excludedCovariateConcepts$conceptId)
        excludedIds <- excludedIds[!is.na(excludedIds)]

        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedIds
        )
      }

      # getDbCohortMethodDataArgs - controls how covariates / cohorts are assembled
      # The Analysis Specifications provided "studyPeriods" and "maxCohortSize".
      # We set restrictToCommonPeriod = TRUE to mirror the template behavior.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs - use settings from Analysis Specifications:
      # - maxCohortSizeForFitting = 250000
      # - errorOnHighCorrelation = TRUE
      # - prior: laplace with cross-validation
      # - control: tolerance, cvType, fold, cvRepetitions, noiseLevel, resetCoefficients, startingVariance
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # keep false to allow analysis to continue if PS fitting fails for a pair
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
          startingVariance = 0.01
        )
      )

      # Covariate balance computation args (shared and per-analysis)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs - use settings from Analysis Specifications:
      # - Cox model, stratified = TRUE, no covariates, no IPTW
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

      # createStudyPopArgs - follow the Analysis Specifications exactly:
      # - restrictToCommonPeriod = FALSE
      # - firstExposureOnly = TRUE
      # - washoutPeriod = 365
      # - removeDuplicateSubjects = "remove all"
      # - censorAtNewRiskWindow = FALSE
      # - removeSubjectsWithPriorOutcome = TRUE
      # - priorOutcomeLookBack = 99999
      # - riskWindowStart = 1, riskWindowEnd = 14 anchored on cohort start
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = TRUE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "remove all",
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

      # Append the CohortMethod analysis configuration to the list
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          as.character(studyStartDate),
          ifelse(is.na(studyEndDate), "NA", as.character(studyEndDate)),
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

# ------------------------------------------------------------------------------
# Build CohortMethod module specifications
# ------------------------------------------------------------------------------
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
# Assemble the full analysis specifications and save to JSON
# ------------------------------------------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() %>%
  Strategus::addSharedResources(cohortDefinitionShared) %>%
  Strategus::addSharedResources(negativeControlsShared) %>%
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specification JSON to inst/<analysisName>/<analysisName>AnalysisSpecification.json
# Use the exact analysis name from the Analysis Specifications: "rapidcyclejanssen"
outputDir <- file.path("inst", "rapidcyclejanssen")
if (!dir.exists(outputDir)) dir.create(outputDir, recursive = TRUE)

ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path(outputDir, "rapidcyclejanssenAnalysisSpecification.json")
)