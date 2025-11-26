library(dplyr)
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)

# -------------------------------------------------------------------------
# Script: CreateStrategusAnalysisSpecification.R
# Study: rapidcyclejanssen
#
# This script builds an analysis specification JSON for Strategus using the
# OHDSI HADES modules (CohortGenerator, CohortDiagnostics, CohortMethod).
# All names and IDs are taken exactly from the provided Analysis Specifications.
# Detailed comments explain how each setting from the Analysis Specifications
# is applied to the Strategus/CohortMethod configuration.
# -------------------------------------------------------------------------

# Base Atlas/WebAPI URL used to export cohort definitions and concept sets.
# NOTE: change this to your WebAPI server if not using atlas-demo.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# -----------------------------------------------------------------------------
# 1) Cohort Definitions
#    - Use the exact cohort ids from Analysis Specifications:
#      target:      id = 1794126  (will be re-numbered to 1)
#      comparator:  id = 1794132  (will be re-numbered to 2)
#      outcome:     id = 1794131  (will be re-numbered to 3)
# -----------------------------------------------------------------------------
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to the small integers used in downstream specifications:
# target -> 1, comparator -> 2, outcome -> 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# -----------------------------------------------------------------------------
# 2) Negative control concept set
#    - Use the exact concept set id from Analysis Specifications:
#      conceptSetId = 1888110 (name = "negative")
#    - Convert the concept set to a set of outcome cohort records for negative
#      controls. We offset the cohortId for negative controls such that their
#      cohortIds do not collide with the primary cohorts (they will start at 101).
# -----------------------------------------------------------------------------
negativeControlConceptSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,
  baseUrl = baseUrl
)

# Resolve the concept set and retrieve the concepts (conceptId, conceptName)
negativeControlOutcomeCohortSet <- negativeControlConceptSet %>%
  ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
  ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  # Assign cohortId values starting from 101 (so no collision with 1,2,3...)
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Sanity check: ensure no duplicated cohort IDs between the main cohortDefinitionSet
# and the negative control cohort set.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between main cohorts and negative controls ***")
}

# -----------------------------------------------------------------------------
# 3) Assemble simple lookup/data frames for outcomes and target-comparator pairs
# -----------------------------------------------------------------------------
# Outcomes list (primary outcomes). We use cohortId == 3 (re-numbered).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # "cleanWindow" is commonly used to indicate prior-outcome exclusion window;
  # here set to 365 days for diagnostics and other modules that may use it.
  mutate(cleanWindow = 365)

# Target/Comparator table for CohortMethod analyses.
# Use the re-numbered ids:
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  # We don't have separate concept ids for target/comparator beyond the cohorts;
  # include NA placeholders to match template expectation (these columns may be
  # referenced when building excluded covariates).
  targetConceptId = NA_integer_,
  comparatorConceptId = NA_integer_
)

# Excluded covariate concepts: Analysis Specifications included empty lists.
# Keep an empty data.frame to satisfy template structure.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# -----------------------------------------------------------------------------
# 4) Create Strategus module shared resource and module specifications
#    - CohortGenerator shared resources for the cohorts and negative controls.
# -----------------------------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource specification for the cohort definitions we exported
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

# Shared resource specification for negative control outcomes derived from the
# concept set. We follow the template: occurrenceType = "first", detectOnDescendants = TRUE
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specification for CohortGenerator (create cohorts on the server)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# -----------------------------------------------------------------------------
# 5) Cohort Diagnostics Module specifications (use defaults from template)
# -----------------------------------------------------------------------------
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

# -----------------------------------------------------------------------------
# 6) CohortMethod Module settings
#    - Map the Analysis Specifications into the CohortMethod configuration.
# -----------------------------------------------------------------------------

# Study periods: Analysis Specifications defined a single study period:
# studyStartDate = 20210101, studyEndDate = null (no end date)
studyPeriods <- tibble::tibble(
  studyStartDate = c(20210101L),
  studyEndDate   = c(NA_integer_) # NA indicates no specific end date
)

# Time-at-risks (TARs) from Analysis Specifications.
# Each TAR entry includes risk window start/end anchored to "cohort start".
timeAtRisks <- tibble::tibble(
  label = c("1-14", "1-28", "1-42", "1-90", "0-2"),
  riskWindowStart  = c(1L, 1L, 1L, 1L, 0L),
  startAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd  = c(14L, 28L, 42L, 90L, 2L),
  endAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start"),
  minDaysAtRisk = c(1L, 1L, 1L, 1L, 1L)
)

# Propensity score settings: Analysis Specifications included a single PS setting
# which is a match with maxRatio = 100, caliper = 0.2, caliperScale = "standardized logit".
# We will convert this into the psConfigList used by the template loop.
psConfigList <- list(
  list(
    method = "match",
    label = "match_maxRatio100_caliper0.2_standardizedLogit",
    params = list(
      maxRatio = 100,
      caliper = 0.2,
      caliperScale = "standardized logit"
    )
  )
)

# Covariate settings: Analysis Specifications included empty include/exclude lists,
# therefore we use the default covariate set (FeatureExtraction::createDefaultCovariateSettings).
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE
)

# Prepare containers for CohortMethod analyses
cmAnalysisList <- list()
analysisId <- 1

# Loop over study periods, time-at-risks, and PS configurations to create analyses
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- if (is.na(studyPeriods$studyStartDate[s])) NULL else as.character(studyPeriods$studyStartDate[s])
  studyEndDate <- if (is.na(studyPeriods$studyEndDate[s])) NULL else as.character(studyPeriods$studyEndDate[s])

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Build matchOnPsArgs or stratifyByPsArgs depending on the configuration.
      if (psCfg$method == "match") {
        # Analysis Specifications: matchOnPsArgs:
        #   maxRatio = 100
        #   caliper = 0.2
        #   caliperScale = "standardized logit"
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # Not used in these specifications, but included for completeness.
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      } else {
        stop("Unknown PS method in psConfigList")
      }

      # Build outcomeList combining the primary outcomes (oList) and negative controls
      outcomeList <- append(
        # Primary outcomes (marked as outcomeOfInterest = TRUE)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        # Negative control outcomes (marked as outcomeOfInterest = FALSE, trueEffectSize = 1)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Build the list of target-comparator-outcomes for CohortMethod
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        # excludedCovariateConceptIds: combine any target/comparator drug concepts and
        # the excludedCovariateConcepts list (empty in this specification).
        excludedIds <- c(
          as.integer(cmTcList$targetConceptId[i]),
          as.integer(cmTcList$comparatorConceptId[i]),
          excludedCovariateConcepts$conceptId
        )
        # Remove NAs and ensure numeric vector
        excludedIds <- excludedIds[!is.na(excludedIds)]

        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedIds
        )
      }

      # getDbCohortMethodDataArgs: map from Analysis Specifications (getDbCohortMethodDataArgs)
      # Analysis Specifications:
      #   studyPeriods: studyStartDate = 20210101, studyEndDate = null
      #   maxCohortSize = 0
      #   restrictToCommonPeriod = true
      #   firstExposureOnly = true
      #   washoutPeriod = 365
      #   removeDuplicateSubjects = "remove all"
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        firstExposureOnly = TRUE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "remove all",
        covariateSettings = covariateSettings
      )

      # createPsArgs: map from Analysis Specifications (propensityScoreAdjustment -> createPsArgs)
      # Analysis Specs:
      #   maxCohortSizeForFitting = 250000
      #   errorOnHighCorrelation = true
      #   prior: priorType = "laplace", useCrossValidation = true
      #   control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      #            noiseLevel = "silent", resetCoefficients = true, startingVariance = 0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        # Keep stopOnError = FALSE to allow Strategus to continue through operations
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
          seed = 1,
          fold = 10,
          cvRepetitions = 10,
          resetCoefficients = TRUE,
          tolerance = 2e-07,
          startingVariance = 0.01
        )
      )

      # Covariate balance computation arguments
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs: mapped from Analysis Specifications (fitOutcomeModelArgs)
      # Model settings:
      #   modelType = "cox", stratified = TRUE, useCovariates = FALSE,
      #   inversePtWeighting = FALSE
      # Regularization prior and control mirror the createPsArgs control/prior structure
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
          cvType = "auto",
          seed = 1,
          fold = 10,
          cvRepetitions = 10,
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          tolerance = 2e-07,
          noiseLevel = "quiet"
        )
      )

      # createStudyPopArgs: map from Analysis Specifications (createStudyPopArgs)
      # Analysis Specs values:
      #   restrictToCommonPeriod = false
      #   firstExposureOnly = false
      #   washoutPeriod = 0
      #   removeDuplicateSubjects = "keep all"
      #   censorAtNewRiskWindow = false
      #   removeSubjectsWithPriorOutcome = true
      #   priorOutcomeLookBack = 99999
      #   risk windows: obtained from timeAtRisks[t,]
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

      # Create a description for the analysis that documents the key settings
      description <- sprintf(
        "rapidcyclejanssen; StudyStart=%s; StudyEnd=%s; TAR=%s; PS=%s",
        ifelse(is.null(studyStartDate), "NA", studyStartDate),
        ifelse(is.null(studyEndDate), "NA", studyEndDate),
        timeAtRisks$label[t],
        psCfg$label
      )

      # Build the CohortMethod analysis specification and append to list
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = description,
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

# Create the CohortMethod module specifications
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# -----------------------------------------------------------------------------
# 7) Assemble final analysisSpecifications object and write to JSON
#    - Use the study name EXACTLY as given: "rapidcyclejanssen"
# -----------------------------------------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specification JSON under inst/<studyName>/
outputFile <- file.path("inst", "rapidcyclejanssen", "rapidcyclejanssenAnalysisSpecification.json")
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  outputFile
)

# End of script. The resulting JSON is ready to be used with Strategus run workflows.