################################################################################
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# further details.
################################################################################

# Load required packages for data manipulation and Strategus orchestration
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Configure the ATLAS/WebAPI base URL where the cohorts and concept sets are hosted.
# This script assumes the OHDSI demo ATLAS instance. Change if needed.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Pull the 3 cohorts (Target, Comparator, Outcome) defined in <Analysis Specifications>
# - Target cohort:      id = 1794126, name = "target1"
# - Comparator cohort:  id = 1794132, name = "comparator1"
# - Outcome cohort:     id = 1794131, name = "outcome1"
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to local IDs used within the analysis:
# Target -> 1, Comparator -> 2, Outcome -> 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes
# In this analysis, a negative control concept set is provided in <Analysis Specifications>
# conceptSetId = 1888110, name = "negative"
# We resolve that concept set into concepts and convert them to "negative control outcome cohorts".
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
  ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
  # Rename to the column names expected by the CohortMethod negative control structure
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  # Assign unique cohort IDs to negative controls starting at 101 to avoid overlap with 1,2,3 above
  mutate(cohortId = dplyr::row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Ensure no duplicate cohort IDs exist across the main cohorts and negative control cohorts
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames for the cohorts used in each analysis ---------------------
# Outcomes (primary outcomes to estimate):
# - Based on the outcome cohort (now cohortId = 3).
# - Assign a clean window of 365 days (per <Template>).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Use EXACT names from <Analysis Specifications> ("target1", "comparator1")
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  # These columns can be used to exclude the specific exposure concepts from covariates (if desired).
  # Not provided in <Analysis Specifications>, so set to NA.
  targetConceptId = NA_integer_,
  comparatorConceptId = NA_integer_
)

# Covariate concept selections (inclusion/exclusion) from <Analysis Specifications>
# The provided lists have null/empty entries, which we interpret as none specified.
includedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# CohortGeneratorModule --------------------------------------------------------
# Create shared resources for cohorts and negative controls and the module specs.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource (cohorts to generate)
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Shared resource (negative control outcome cohorts)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications for cohort generation
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# Diagnostics for all generated cohorts (1, 2, 3, and negative control cohorts will be included
# via the shared resource). Here we run a broad set of diagnostics to aid data quality review.
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

# studyPeriods from <Analysis Specifications>:
# - One period, studyStartDate = null, studyEndDate = null => not restricted (leave as NA)
# - maxCohortSize in getDbCohortMethodDataArgs = 0 (no cap)
# Note: Using NA here indicates no restriction; this aligns with template guidance.
studyPeriods <- tibble::tibble(
  studyStartDate = as.integer(NA), # YYYYMMDD or NA
  studyEndDate   = as.integer(NA)  # YYYYMMDD or NA
)

# Time-at-risks (TARs) from <Analysis Specifications>:
# - One TAR:
#   riskWindowStart = 1, startAnchor = "cohort start"
#   riskWindowEnd   = 0, endAnchor   = "cohort end"
#   minDaysAtRisk   = 1
# We add a label for readability in the analysis description.
timeAtRisks <- tibble::tibble(
  label = c("TAR1"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end"),     # "cohort start" | "cohort end"
  minDaysAtRisk = c(1)
)

# Propensity Score settings from <Analysis Specifications>:
# - Adjustment method: matchOnPsArgs with maxRatio = 10, caliper = 0.2, caliperScale = "standardized logit"
# - No stratifyByPsArgs
matchOnPsArgsList <- tibble::tibble(
  label = c("match-stdlogit-0.2-r10"),
  maxRatio  = c(10),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # "propensity score" | "standardized" | "standardized logit"
)

# Empty stratification list (none specified)
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

# Iterate through all analysis setting combinations (Study Period x TAR x PS config)
# In this specification, there is 1 x 1 x 1 = 1 analysis.
cmAnalysisList <- list()
targetComparatorOutcomesList <- list()
analysisId <- 1

# Set up covariate selection; here we use default FeatureExtraction settings and optionally
# include or exclude specific concepts (empty in this specification).
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE
)

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Configure PS adjustment method-specific args
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

      # Build the list of outcomes for the CM analysis:
      # - Mark the main outcome (cohortId=3) as "outcomeOfInterest = TRUE"
      # - Add all negative control outcomes as "outcomeOfInterest = FALSE"
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            # Use the specified priorOutcomeLookBack for study population (365) for the main outcome
            priorOutcomeLookback = 365
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

      # Target-Comparator-Outcome sets:
      # Exclude any specified exposure concepts from covariates (none provided here).
      for (i in seq_len(nrow(cmTcList))) {
        excludedIds <- c(
          cmTcList$targetConceptId[i],
          cmTcList$comparatorConceptId[i],
          excludedCovariateConcepts$conceptId
        )
        excludedIds <- excludedIds[!is.na(excludedIds)]
        includedIds <- includedCovariateConcepts$conceptId

        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          includedCovariateConceptIds = includedIds,
          excludedCovariateConceptIds = excludedIds
        )
      }

      # Data extraction args for CohortMethod from <Analysis Specifications> getDbCohortMethodDataArgs:
      # - studyStartDate: NA (no restriction)
      # - studyEndDate: NA (no restriction)
      # - maxCohortSize: 0 (no cap)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # PS model fitting args from <Analysis Specifications> propensityScoreAdjustment.createPsArgs
      # - maxCohortSizeForFitting = 250000
      # - errorOnHighCorrelation = TRUE
      # - prior = laplace with cross-validation
      # - control = per specified tolerance, cv, folds, noise level, etc.
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # Strategus can continue even if PS fails for a subset
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

      # Covariate balance computation settings (two flavors per <Template>)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model settings from <Analysis Specifications> fitOutcomeModelArgs:
      # - modelType = "cox", stratified = TRUE, useCovariates = FALSE, inversePtWeighting = FALSE
      # - prior = laplace with cross-validation
      # - control = per specified tolerance, cv, folds, noise level, etc.
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

      # Study population args from <Analysis Specifications> createStudyPopArgs:
      # - restrictToCommonPeriod = FALSE
      # - firstExposureOnly = FALSE
      # - washoutPeriod = 365
      # - removeDuplicateSubjects = "remove all"
      # - censorAtNewRiskWindow = FALSE
      # - removeSubjectsWithPriorOutcome = TRUE
      # - priorOutcomeLookBack = 365
      # - TAR = from timeAtRisks[t, ]
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
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t]
      )

      # Append the configured analysis to the analysis list
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          as.character(studyStartDate),
          as.character(studyEndDate),
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

# Build the CohortMethod module specifications with all analyses and TCO sets
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the overall Strategus analysis specifications --------------------------
# Assemble:
# - Shared resources (cohorts and negative controls)
# - Modules: CohortGenerator, CohortDiagnostics, CohortMethod
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the full analysis specification as JSON to inst/<name>/<name>AnalysisSpecification.json
# Use EXACT study name from <Analysis Specifications>: "ceeamos"
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ceeamos", "ceeamosAnalysisSpecification.json")
)