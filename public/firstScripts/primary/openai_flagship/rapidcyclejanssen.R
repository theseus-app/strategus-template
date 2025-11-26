################################################################################
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
#
# This script builds a Strategus analysis specification using the
# OHDSI HADES modules and your Analysis Specifications.
#
# It:
# 1) Pulls the study cohorts from ATLAS/WebAPI.
# 2) Resolves a negative control outcome concept set.
# 3) Constructs settings for CohortGenerator, CohortDiagnostics, and CohortMethod.
# 4) Applies your Propensity Score, Study Population, and Outcome Model settings.
# 5) Saves the full Strategus analysis specification to JSON.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

library(dplyr)
library(Strategus)

# Study name used for output path
studyName <- "rapidcyclejanssen"

# Shared Resources -------------------------------------------------------------
# WebAPI location hosting the cohort definitions and concept sets
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ------------------------------------------------------------------------------
# Cohort Definitions
# - We export the three cohorts: target, comparator, and outcome.
# - These are re-numbered to 1 (target), 2 (comparator), 3 (outcome) as required
#   by the CohortMethod/TCO convention used later.
# ------------------------------------------------------------------------------

cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts so that we have 1, 2, 3 for T, C, and O respectively
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# ------------------------------------------------------------------------------
# Negative control outcomes
# - We resolve a concept set into concept IDs that will act as negative control
#   outcomes. These are assigned cohortIds starting at 101 to avoid collision
#   with the T/C/O cohort IDs.
# ------------------------------------------------------------------------------
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
    cohortId = dplyr::row_number() + 100 # reserve 1,2,3 for T/C/O
  ) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Basic safety check for ID collisions between T/C/O and negative controls
combinedIds <- c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)
if (any(duplicated(combinedIds))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# ------------------------------------------------------------------------------
# Create some data frames to hold the cohorts we'll use in each analysis
# ------------------------------------------------------------------------------
# Outcomes (your "outcome1" -> cohortId == 3)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(
    # Optional: a clean window, not directly consumed by CM settings below, but
    # kept for clarity/documentation with this object.
    cleanWindow = 365
  )

# Target and Comparator pairs for the CohortMethod analysis
# Note: Including "Name" fields is useful in downstream reporting.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  # Add optional columns for concept IDs of exposures (if known) so we can
  # exclude these concepts as covariates. We don't have these concept IDs in
  # the analysis specifications, so we set them to NA and handle gracefully.
  targetConceptId = NA_integer_,
  comparatorConceptId = NA_integer_
)

# Covariate selection lists from the Analysis Specifications
# The provided lists contain null/blank entries, which results in empty vectors.
# These can be used to restrict covariate inclusion/exclusion in FeatureExtraction.
includedCovariateConcepts <- data.frame(
  conceptId = integer(),  # none provided
  conceptName = character()
)
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),  # none provided
  conceptName = character()
)

# CohortGeneratorModule --------------------------------------------------------
# - Defines the shared resources for cohorts and negative controls and instructs
#   the module to generate cohort statistics for diagnostics/review.
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
# - We will run a comprehensive set of diagnostics to understand cohort behavior,
#   incidence rates, and characterization measures.
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
# Setup analysis grid:
# - studyPeriods (from getDbCohortMethodDataArgs in Analysis Specifications)
# - timeAtRisks (from createStudyPopArgs/timeAtRisks in Analysis Specifications)
# - propensity score configs (from propensityScoreAdjustment in Analysis Specifications)

# Study period: start 20210101, no explicit end date (run until end of data).
# Strategus/CM accept empty string "" when not restricting on end date.
studyPeriods <- tibble::tibble(
  studyStartDate = c("20210101"),
  studyEndDate   = c("")
)

# Time-at-risk: single window 1 to 14 days after cohort start, min 1 day at risk
timeAtRisks <- tibble::tibble(
  label = c("TAR_1_14_cs_cs"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd  = c(14),
  endAnchor = c("cohort start")    # "cohort start" | "cohort end"
)

# Propensity Score settings - match on PS (as specified)
matchOnPsArgsList <- tibble::tibble(
  label = c("match_cal0.2_logit_max100"),
  maxRatio  = c(100),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # "propensity score" | "standardized" | "standardized logit"
)

# Propensity Score settings - stratify by PS (none specified)
stratifyByPsArgsList <- tibble::tibble(
  label = character(),
  numberOfStrata  = integer(),
  baseSelection = character() # "all" | "target" | "comparator"
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

# Iterate through all analysis setting combinations
cmAnalysisList <- list()
analysisId <- 1L

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Build the PS adjustment args based on method
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

      # Covariate settings
      # - We use default FeatureExtraction covariates.
      # - If included/excluded concept lists are provided, they are applied here.
      includedIds <- if (nrow(includedCovariateConcepts)) includedCovariateConcepts$conceptId else NULL
      excludedIds <- if (nrow(excludedCovariateConcepts)) excludedCovariateConcepts$conceptId else NULL
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE,
        includedCovariateConceptIds = includedIds,
        excludedCovariateConceptIds = excludedIds
      )

      # Outcome list comprising:
      # - The primary outcome of interest (outcome1; cohortId = 3)
      # - The resolved negative control outcomes (trueEffectSize = 1)
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

      # Target-comparator-outcomes
      # - Exclude covariates representing the exposure concepts if those are available.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        excludedIdsForTco <- c(
          cmTcList$targetConceptId[i],
          cmTcList$comparatorConceptId[i],
          excludedCovariateConcepts$conceptId
        )
        excludedIdsForTco <- excludedIdsForTco[!is.na(excludedIdsForTco)]
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedIdsForTco
        )
      }

      # Data extraction settings
      # - Uses the study period start/end; maxCohortSize=0 means no downsampling.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # PS model settings
      # - Laplace prior with CV, Cyclops control matches specification.
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # Let CM proceed even if PS model can't be fit in some subsets
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

      # Covariate balance computation settings
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model settings
      # - Cox model, stratified, no additional covariates (since using PS),
      #   Laplace prior with CV, Cyclops control per specification.
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

      # Study population settings (createStudyPopArgs)
      # - From Analysis Specifications:
      #   restrictToCommonPeriod = FALSE
      #   firstExposureOnly = TRUE
      #   washoutPeriod = 365
      #   removeDuplicateSubjects = "remove all"
      #   censorAtNewRiskWindow = FALSE
      #   removeSubjectsWithPriorOutcome = TRUE
      #   priorOutcomeLookback = 99999
      #   TAR: start=1, end=14, both anchored on cohort start, minDaysAtRisk=1
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
        minDaysAtRisk = 1
      )

      # Append the settings to the analysis list
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study period: %s-%s; TAR: %s; PS: %s",
          studyStartDate,
          ifelse(nchar(studyEndDate) == 0, "endOfData", studyEndDate),
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
      analysisId <- analysisId + 1L
    }
  }
}

# Build CohortMethod module specification
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

# Save to JSON (inst/<studyName>/<studyName>AnalysisSpecification.json)
# This file can be consumed by Strategus::runStudy.
outputPath <- file.path("inst", studyName)
if (!dir.exists(outputPath)) {
  dir.create(outputPath, recursive = TRUE, showWarnings = FALSE)
}
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path(outputPath, paste0(studyName, "AnalysisSpecification.json"))
)