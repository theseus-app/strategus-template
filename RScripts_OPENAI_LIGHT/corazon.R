library(dplyr)
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)

################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script builds Strategus analysis specifications using the provided
# Analysis Specifications. The structure follows the provided template closely,
# and all variable / function names are kept consistent with the template and
# the analysis specification JSON.
#
# Detailed annotations explain how each setting from the Analysis Specifications
# is applied.
################################################################################

# Shared Resources -------------------------------------------------------------
# Base URL for the ATLAS/WebAPI instance used to retrieve cohort and concept
# set definitions. Keep as-is or change to your WebAPI endpoint.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ------------------------------------------------------------------------------
# Cohort Definitions
# ------------------------------------------------------------------------------
# Export the cohort definitions for:
# - Target cohort (id: 1794126, name: "target1")
# - Comparator cohort (id: 1794132, name: "comparator1")
# - Outcome cohort (id: 1794131, name: "outcome1")
#
# generateStats = TRUE requests cohort generation statistics be returned where
# available.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts so that internal study ids start at 1, 2, 3 (template
# convention). This mapping makes later code (cmTcList etc.) simpler.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# ------------------------------------------------------------------------------
# Negative control outcomes (concept set)
# ------------------------------------------------------------------------------
# Analysis Specifications provide a negative control concept set id:
# - id: 1888110, name: "negative"
#
# We resolve the concept set (expand descendants if necessary) and convert to a
# cohort-like data.frame so it can be passed as a shared resource.
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
  mutate(cohortId = row_number() + 100) %>% # negative controls start at 101, 102,...
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no duplicated cohort IDs across target/comparator/outcome
# and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# ------------------------------------------------------------------------------
# Create data frames that describe the T/E/O relationships used in CohortMethod
# ------------------------------------------------------------------------------
# Outcomes list: pick the (renumbered) outcome cohort (cohortId == 3)
# The template sets a clean window (prior outcome lookback for describing the
# outcome) to 365 days; keep that here.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target & Comparator for CohortMethod analyses (use the renumbered ids and
# the names from the Analysis Specifications).
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  # The template references targetConceptId/comparatorConceptId in excluded lists.
  # We do not have those specific concept ids in the specification, so set to NA.
  targetConceptId = NA_integer_,
  comparatorConceptId = NA_integer_,
  stringsAsFactors = FALSE
)

# Excluded covariate concepts: The Analysis Specifications' covariateSelection
# included empty concept lists. To follow the template logic, create an empty
# data.frame (so appending it is harmless).
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character(),
  stringsAsFactors = FALSE
)

# Optional: includedCovariateConcepts could be built from covariateSelection if
# specified. The provided spec does not request specific inclusions.

# CohortGeneratorModule --------------------------------------------------------
# Create shared resources and module specifications for cohort generation.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource built from cohortDefinitionSet exported earlier
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Negative control outcomes as a shared resource.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications: request generation of statistics (generateStats = TRUE)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Create cohort diagnostics module specifications. The template enables many
# diagnostics; we follow that example.
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
# CohortMethodModule
# ------------------------------------------------------------------------------
# The Analysis Specifications include:
# - two study periods:
#     20100101 -> 20191231
#     20120101 -> 20191231
# - two time-at-risk definitions in createStudyPopArgs
#
# Build studyPeriods tibble (each row will be used to construct separate CM
# analyses)
studyPeriods <- tibble::tibble(
  studyStartDate = c("20100101", "20120101"),
  studyEndDate   = c("20191231", "20191231")
)

# Time-at-risks (TARs) derived from the Analysis Specifications' createStudyPopArgs
# Two TARs are provided:
# 1) riskWindowStart = 0, startAnchor = "cohort start", riskWindowEnd = 0, endAnchor = "cohort end"
# 2) riskWindowStart = 0, startAnchor = "cohort start", riskWindowEnd = 9999, endAnchor = "cohort start"
timeAtRisks <- tibble::tibble(
  label = c("TAR_cohortStart_to_cohortEnd", "TAR_cohortStart_to_cohortStart9999"),
  riskWindowStart  = c(0, 0),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd  = c(0, 9999),
  endAnchor = c("cohort end", "cohort start")
)

# ------------------------------------------------------------------------------
# Propensity Score (PS) configurations
# ------------------------------------------------------------------------------
# The Analysis Specifications describe two PS settings:
# - A stratification by PS with numberOfStrata = 5 and baseSelection = "all"
# - A matching on PS with maxRatio = 0, caliper = 0.2, caliperScale = "standardized logit"
#
# Create data.frames to describe both types. These will be converted into the
# generic psConfigList used in the loop below.
stratifyByPsArgsList <- tibble::tibble(
  label = c("stratify_ps_5_all"),
  numberOfStrata  = c(5),
  baseSelection = c("all") # "all" per spec
)

matchOnPsArgsList <- tibble::tibble(
  label = c("match_ps_caliper_0.2"),
  maxRatio  = c(0),        # 0 indicates 1:1 matching in some conventions; follow template usage
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # per spec
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Add match configurations (if any)
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

# Add stratify configurations (if any)
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
# Iterate through combinations of study periods, time-at-risks, and PS configs
# to create CohortMethod analyses.
# ------------------------------------------------------------------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Build matchOnPsArgs or stratifyByPsArgs depending on configuration
      if (psCfg$method == "match") {
        # Create CohortMethod match settings according to template & spec
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
        stop("Unknown PS method encountered")
      }

      # covariateSettings: use default covariates (FeatureExtraction default). The
      # template includes addDescendantsToExclude = TRUE; keep that as a sensible
      # default for excluding descendant concepts of excluded items.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcomes list built from oList and negative controls.
      # For true outcomes (oList), outcomeOfInterest = TRUE and no trueEffectSize
      # (NA). For negative controls, outcomeOfInterest = FALSE and trueEffectSize =
      # 1 (null).
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

      # Build the target-comparator-outcomes list expected by cohortMethodModule
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # excludedCovariateConceptIds: combine any provided target/comparator
          # concept ids and the general excludedCovariateConcepts list.
          excludedCovariateConceptIds = c(
            cmTcList$targetConceptId[i],
            cmTcList$comparatorConceptId[i],
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # createGetDbCohortMethodDataArgs: control data extraction and covariates
      # For study-level extraction, pass the per-loop studyStartDate/studyEndDate,
      # and the covariate settings built above. Analysis spec's getDbCohortMethodDataArgs
      # had maxCohortSize = 0; we follow that.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createCreatePsArgs: follow the Analysis Specifications exactly for the
      # Cyclops prior + control and related settings.
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # keep false so Strategus can continue other analyses if PS fitting fails
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
          startingVariance = 0.01
        )
      )

      # Compute covariate balance args:
      # - computeSharedCovariateBalanceArgs used for balance across the shared set
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      # - computeCovariateBalanceArgs used for the table1 / covariate-level balance
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs built according to the Analysis Specifications:
      # modelType = "cox", stratified = TRUE, useCovariates = FALSE,
      # inversePtWeighting = FALSE, and Cyclops prior/control settings.
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
          tolerance = 2e-7,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # createStudyPopArgs: construct study population arguments according to the
      # Analysis Specifications under "createStudyPopArgs".
      # NOTE: we consume timeAtRisks[t,] to populate the risk window fields.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all", # per Analysis Specifications
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

      # Append the CohortMethod analysis to cmAnalysisList with a human-readable
      # description that carries study period, TAR label, and PS config label.
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
    } # end psConfigList loop
  } # end timeAtRisks loop
} # end studyPeriods loop

# Build the CohortMethod module specifications using the analyses constructed.
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
# Combine all module specifications and shared resources into a single
# Strategus analysis specification object.
# ------------------------------------------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specification to disk (path follows template)
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "studyName", "studyNameAnalysisSpecification.json")
)

# End of script ----------------------------------------------------------------
# The generated JSON file contains the Strategus analysis specification that can
# be used by the Strategus package to execute the CohortGenerator, Cohort
# Diagnostics, and CohortMethod modules according to the provided settings.
# ------------------------------------------------------------------------------