library(dplyr)
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(ParallelLogger)

###############################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script builds a Strategus analysis specification JSON for the
# "ranitidinecancer" study using the OHDSI Strategus package. It follows the
# provided template and the detailed settings in the Analysis Specifications.
#
# The script contains detailed inline annotations describing how each setting
# from the Analysis Specifications is translated into Strategus / CohortMethod
# module settings.
###############################################################################

# Base WebAPI URL used to fetch cohort and concept set definitions.
# Replace with your Atlas WebAPI endpoint if not using the demo instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# -----------------------------------------------------------------------------
# Cohort Definitions
# -----------------------------------------------------------------------------
# Export the cohort definitions from the WebAPI for:
# - Target cohort (id = 1794126)
# - Comparator cohort (id = 1794132)
# - Outcome cohort (id = 1794131)
#
# We then renumber the cohortId values so they start at 1, 2, 3 for use inside
# Strategus and downstream HADES modules (this is the convention used in the
# template).
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts so they are 1, 2, 3 (target, comparator, outcome)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# -----------------------------------------------------------------------------
# Negative control outcomes (concept set)
# -----------------------------------------------------------------------------
# Use the provided concept set id (1888110) to fetch the concepts that will be
# used as negative control outcomes. The pipeline resolves descendant concepts
# as used in the template and converts the concepts into a cohort-like table
# with cohortId values starting at 101 to avoid colliding with target/comparator.
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
  dplyr::rename(outcomeConceptId = "conceptId",
                cohortName = "conceptName") %>%
  dplyr::mutate(cohortId = dplyr::row_number() + 100) %>% # negative controls start from 101
  dplyr::select(cohortId, cohortName, outcomeConceptId)

# Safety check to ensure cohort ids do not collide
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# -----------------------------------------------------------------------------
# Build lists/tables describing the analyses (target/comparator/outcomes)
# -----------------------------------------------------------------------------
# Outcomes (the primary outcome cohort(s) for the study)
# - We locate the outcome cohort (renumbered to cohortId == 3),
#   attach the required fields and set a cleanWindow (prior-outcome washout).
oList <- cohortDefinitionSet %>%
  dplyr::filter(.data$cohortId == 3) %>%
  dplyr::mutate(outcomeCohortId = cohortId,
                outcomeCohortName = cohortName) %>%
  dplyr::select(outcomeCohortId, outcomeCohortName) %>%
  # The Analysis Specifications requested priorOutcomeLookBack = 99999 for createStudyPopArgs;
  # here we set a cleanWindow of 365 days for cohort diagnostics / standardization (template example).
  dplyr::mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analyses
# - Cohort IDs have been renumbered above: target -> 1, comparator -> 2
# - Names are taken from the Analysis Specifications: "target1", "comparator1"
# - We include placeholder columns for targetConceptId and comparatorConceptId
#   to match usage in the template (no specific drug concept ids were provided).
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  # No explicit concept IDs were provided in the Analysis Specifications.
  # We set these to NA_integer_ to make them explicit but absent.
  targetConceptId = NA_integer_,
  comparatorConceptId = NA_integer_,
  stringsAsFactors = FALSE
)

# Excluded covariate concepts
# - Analysis Specifications included empty include/exclude lists; so we create
#   an empty data.frame for excludedCovariateConcepts consistent with the template.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# CohortGeneratorModule specifications (shared resources + module spec)
# -----------------------------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource: the cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Shared resource: negative control outcome cohort definitions (the resolved concept set)
# - occurrenceType = "first" aligns with the template usage
# - detectOnDescendants = TRUE ensures descendant concepts are included
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specs: create cohort generation module specification. generateStats = TRUE
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# -----------------------------------------------------------------------------
# CohortDiagnosticsModule specifications
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
  # The template uses minCharacterizationMean = 0.01
  minCharacterizationMean = 0.01
)

# -----------------------------------------------------------------------------
# CohortMethodModule (main analytical settings)
# -----------------------------------------------------------------------------
cmModuleSettingsCreator <- CohortMethodModule$new()

# Study periods
# - The Analysis Specifications provided empty strings for study periods which
#   indicates we are NOT restricting the study to explicit calendar windows.
studyPeriods <- tibble::tibble(
  studyStartDate = c(), # empty -> no restriction
  studyEndDate   = c()
)

# Time-at-risks (TARs)
# The Analysis Specifications provide four TAR definitions:
# 1) riskWindowStart = 1, startAnchor = "cohort start", riskWindowEnd = 99999, endAnchor = "cohort start"
# 2) riskWindowStart = 365, startAnchor = "cohort start", riskWindowEnd = 99999, endAnchor = "cohort start"
# 3) riskWindowStart = 1, startAnchor = "cohort start", riskWindowEnd = 0, endAnchor = "cohort end"
# 4) riskWindowStart = 365, startAnchor = "cohort start", riskWindowEnd = 0, endAnchor = "cohort end"
timeAtRisks <- tibble::tibble(
  label = c(
    "TAR_1_to_infinite_anchor_cohort_start",
    "TAR_365_to_infinite_anchor_cohort_start",
    "TAR_1_to_cohort_end",
    "TAR_365_to_cohort_end"
  ),
  riskWindowStart = c(1, 365, 1, 365),
  startAnchor = c("cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd = c(99999, 99999, 0, 0),
  endAnchor = c("cohort start", "cohort start", "cohort end", "cohort end")
)

# -----------------------------------------------------------------------------
# Propensity score adjustment settings
# -----------------------------------------------------------------------------
# The Analysis Specifications list 4 PS configurations:
#  - Match with maxRatio = 1, caliper = 0.2, caliperScale = "standardized logit"
#  - Match with maxRatio = 10, caliper = 0.2, caliperScale = "standardized logit"
#  - Stratify with numberOfStrata = 10, baseSelection = "all"
#  - No PS adjustment (both matchOnPsArgs and stratifyByPsArgs null)
#
# We build two data frames (matchOnPsArgsList and stratifyByPsArgsList) for the
# configurations that apply, and then append an explicit "none" configuration.
matchOnPsArgsList <- tibble::tibble(
  label = c("match_1_0.2_stdLogit", "match_10_0.2_stdLogit"),
  maxRatio = c(1, 10),
  caliper = c(0.2, 0.2),
  caliperScale = c("standardized logit", "standardized logit")
)

stratifyByPsArgsList <- tibble::tibble(
  label = c("stratify_10_all"),
  numberOfStrata = c(10),
  baseSelection = c("all")
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Convert matchOnPsArgsList rows to psConfigList entries (method = "match")
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

# Convert stratifyByPsArgsList rows to psConfigList entries (method = "stratify")
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

# Append a configuration representing "no PS adjustment" (both match and stratify are NULL)
psConfigList[[length(psConfigList) + 1]] <- list(
  method = "none",
  label = "no_ps_adjustment",
  params = list()
)

# -----------------------------------------------------------------------------
# Convert PS configuration list into CohortMethod analyses
# -----------------------------------------------------------------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  # If studyPeriods is empty (no rows), still iterate once with empty strings
  if (nrow(studyPeriods) == 0) {
    studyStartDate <- ""
    studyEndDate <- ""
    s <- 1
  }

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Build matchOnPsArgs or stratifyByPsArgs depending on configuration.
      if (psCfg$method == "match") {
        # Create matchOnPsArgs using CohortMethod::createMatchOnPsArgs
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
        # method == "none" (no PS adjustment)
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- NULL
      }

      # Covariate settings
      # - Use default covariates, and addDescendantsToExclude = TRUE as in template.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcomes: primary outcomes (from oList) + negative controls (from resolved concept set)
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

      # Build a list of Target-Comparator-Outcomes specifications for CohortMethod
      # We loop through cmTcList rows (in this case we have a single row)
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # excludedCovariateConceptIds: combine target/comparator concept ids (if provided)
          # with any explicitly excluded covariate concepts. In this study no such
          # concept ids were provided, so these will be NA or an empty vector.
          excludedCovariateConceptIds = c(
            cmTcList$targetConceptId[i],
            cmTcList$comparatorConceptId[i],
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # getDbCohortMethodDataArgs
      # - Use the study period values (empty strings if not restricting)
      # - restrictToCommonPeriod is set from the Analysis Specifications:
      #   getDbCohortMethodDataArgs.restrictToCommonPeriod = false
      # - maxCohortSize = 0 as specified
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs
      # - The Analysis Specifications provide a Cyclops prior and control settings.
      # - We set maxCohortSizeForFitting = 250000 and errorOnHighCorrelation = TRUE
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # allow Strategus to continue non-fatal PS fitting errors
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",
          exclude = c(0),
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          tolerance = 2e-07,
          cvType = "auto",
          seed = 1,
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "silent",
          resetCoefficients = TRUE,
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

      # fitOutcomeModelArgs
      # - Fit a stratified Cox model without additional covariates (useCovariates = FALSE),
      #   using a Laplace prior with cross-validation as specified.
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
          seed = 1,
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # createStudyPopArgs
      # - Create study population args according to the Analysis Specifications:
      #   restrictToCommonPeriod = false
      #   firstExposureOnly = false
      #   washoutPeriod = 0
      #   removeDuplicateSubjects = "keep all"
      #   censorAtNewRiskWindow = false
      #   removeSubjectsWithPriorOutcome = true
      #   priorOutcomeLookBack = 99999
      # - The TAR-specific fields are taken from the timeAtRisks table for row t.
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

      # Append the CohortMethod analysis specification to cmAnalysisList
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

# Create the CohortMethod module specifications using the assembled analysis list
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# -----------------------------------------------------------------------------
# Assemble final Strategus analysis specifications object
# -----------------------------------------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# -----------------------------------------------------------------------------
# Save to JSON
# - The file is placed under inst/ranitidinecancer/ranitidinecancerAnalysisSpecification.json
#   so that it can be bundled within an R package or used by Strategus workflows.
# -----------------------------------------------------------------------------
outputDir <- file.path("inst", "ranitidinecancer")
if (!dir.exists(outputDir)) dir.create(outputDir, recursive = TRUE)

ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path(outputDir, "ranitidinecancerAnalysisSpecification.json")
)