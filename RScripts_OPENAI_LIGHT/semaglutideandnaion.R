################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script creates Strategus analysis specifications for the study:
# semaglutideandnaion
#
# It follows the Template and applies the settings from the provided
# Analysis Specifications JSON. The comments explain how each setting maps
# to Strategus / CohortMethod / Cyclops function arguments.
################################################################################

library(dplyr)
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)

# ------------------------------------------------------------------------------
# Shared Resources / WebAPI
# ------------------------------------------------------------------------------
# Base Atlas WebAPI URL used to fetch cohort definitions and concept sets.
# This can be replaced with your own Atlas WebAPI endpoint.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ------------------------------------------------------------------------------
# Cohort Definitions
# ------------------------------------------------------------------------------
# Analysis Specifications provided:
# target cohort id = 1794126  (name: "target1")
# comparator cohort id = 1794132 (name: "comparator1")
# outcome cohort id(s) = 1794131 (name: "outcome1")
#
# We export these cohort definitions from the WebAPI and then re-number them
# 1 (target), 2 (comparator), 3 (outcome) to match internal expectations of the
# template and Strategus shared resources.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohort IDs so they are consecutive starting at 1:
# - original 1794126 -> 1 (target)
# - original 1794132 -> 2 (comparator)
# - original 1794131 -> 3 (outcome)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# ------------------------------------------------------------------------------
# Negative control outcomes (concept set)
# ------------------------------------------------------------------------------
# Analysis Specifications:
# negativeControlConceptSet id = 1888110 (name: "negative")
#
# We load the concept set definition, resolve descendants, and convert to a
# format compatible with the template (cohortId, cohortName, outcomeConceptId).
# Negative control cohortIds are offset starting at 101 to avoid colliding with
# target/comparator/outcome cohortIds (1,2,3,...).
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
  mutate(cohortId = row_number() + 100) %>% # negative control cohort ids -> 101, 102, ...
  select(cohortId, cohortName, outcomeConceptId)

# Sanity check: no duplicated cohort ids across primary cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# ------------------------------------------------------------------------------
# Build lists / data.frames used by CohortMethod module
# ------------------------------------------------------------------------------
# Outcomes (oList):
# We take the re-numbered cohortDefinitionSet entry for the outcome (cohortId == 3)
# and create the minimal outcome list used in the template. We set a cleanWindow
# (prior outcome window) value consistent with typical epidemiologic studies.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId,
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # 365 days clean window (template default)

# Target and Comparator mapping for CohortMethod analyses:
# Use the re-numbered cohort ids and exact names from the Analysis Specifications.
# The template expects columns targetCohortId, targetCohortName, comparatorCohortId, comparatorCohortName.
# Additionally, the template references targetConceptId and comparatorConceptId when
# specifying excluded covariates; those are not provided in the Analysis Specifications,
# so we include them as NA (explicit columns) to avoid missing-column errors.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  # concept ids for the exposure concepts (not specified in the Analysis Specifications)
  # set to NA to indicate "no explicit excluded target/comparator concept ids supplied".
  targetConceptId = as.integer(NA),
  comparatorConceptId = as.integer(NA)
)

# excludedCovariateConcepts:
# The Template uses this to exclude the drugs of interest from LSPS construction.
# The Analysis Specifications contained empty include/exclude arrays for covariate selection,
# so create an empty data.frame here (no excluded covariates specified).
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# If you wanted to specify included covariates explicitly, you could create:
# includedCovariateConcepts <- data.frame(conceptId = c(...), conceptName = c(...))

# ------------------------------------------------------------------------------
# CohortGeneratorModule (shared resources + module specification)
# ------------------------------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource specifying the cohort definitions we exported earlier.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

# Shared resource for negative control outcomes (converted from concept set).
# occurrenceType = "first" -> we look for first occurrence of each negative concept.
# detectOnDescendants = TRUE -> include descendant concepts in the concept set resolution.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications for cohort generation: generateStats = TRUE to produce
# cohort statistics during cohort generation.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ------------------------------------------------------------------------------
# CohortDiagnosticsModule Settings
# ------------------------------------------------------------------------------
# The template enables a broad set of diagnostics. We follow the template defaults
# and enable multiple diagnostics to help inspect the cohorts.
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

# ------------------------------------------------------------------------------
# CohortMethodModule (build analyses)
# ------------------------------------------------------------------------------
# The Analysis Specifications provide getDbCohortMethodDataArgs.studyPeriods with:
# studyStartDate = 20171201, studyEndDate = 20231231
# We construct a studyPeriods tibble accordingly.
studyPeriods <- tibble::tibble(
  studyStartDate = c(20171201), # YYYYMMDD
  studyEndDate   = c(20231231)  # YYYYMMDD
)

# Time-at-risk definitions (createStudyPopArgs/timeAtRisks in the Analysis Specifications)
# The provided single time-at-risk:
# riskWindowStart = 0, startAnchor = "cohort start",
# riskWindowEnd = 0, endAnchor = "cohort end"
# We include a label to human-readably identify the TAR in analysis descriptions.
timeAtRisks <- tibble::tibble(
  label = c("main"),
  riskWindowStart = c(0),
  startAnchor = c("cohort start"), # allowed values: "cohort start" | "cohort end"
  riskWindowEnd = c(0),
  endAnchor = c("cohort end")
)

# Propensity score adjustment settings (two PS configurations as per Analysis Specifications):
# 1) match on PS with maxRatio = 1, caliper = 0.2, caliperScale = "standardized logit"
# 2) stratify by PS with numberOfStrata = 5, baseSelection = "all"
matchOnPsArgsList <- tibble::tibble(
  label = c("ps_match_1to1"),
  maxRatio = c(1),
  caliper = c(0.2),
  caliperScale = c("standardized logit") # options: "propensity score" | "standardized" | "standardized logit"
)

stratifyByPsArgsList <- tibble::tibble(
  label = c("ps_stratify_5"),
  numberOfStrata = c(5),
  baseSelection = c("all") # options: "all" | "target" | "comparator"
)

# Build a combined PS configuration list (psConfigList) used by the loop below.
psConfigList <- list()

# Convert match rows to psConfigList entries
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

# Convert stratify rows to psConfigList entries
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

# Prepare container for CohortMethod analyses
cmAnalysisList <- list()
analysisId <- 1

# Iterate over study periods, time-at-risks, and PS configurations to build analyses
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create either matchOnPsArgs or stratifyByPsArgs based on psCfg$method
      if (psCfg$method == "match") {
        # Match-on-PS settings (CohortMethod::createMatchOnPsArgs)
        # - maxRatio: maximum number of comparator subjects matched to each target
        # - caliper: allowed caliper width
        # - caliperScale: scale for caliper (here "standardized logit")
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # Stratify-by-PS settings (CohortMethod::createStratifyByPsArgs)
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
        matchOnPsArgs <- NULL
      } else {
        stop("Unknown PS method in psConfigList")
      }

      # Covariate settings for PS and outcome modelling:
      # The Analysis Specifications included an empty covariate selection,
      # so we rely on the default covariates (FeatureExtraction::createDefaultCovariateSettings).
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build the outcome list which includes:
      # - the study outcome(s) from cohortDefinitionSet (outcomeOfInterest = TRUE)
      # - negative control outcomes (outcomeOfInterest = FALSE, trueEffectSize = 1)
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

      # Build the targetComparatorOutcomesList: one element per target/comparator pair
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        # excludedCovariateConceptIds: combine any explicit target/comparator concept ids
        # (available as NA here if not provided) and the excludedCovariateConcepts vector
        excludedIds <- c(cmTcList$targetConceptId[i],
                         cmTcList$comparatorConceptId[i],
                         excludedCovariateConcepts$conceptId)
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedIds
        )
      }

      # getDbCohortMethodDataArgs: controls how we extract data from the CDM for
      # CohortMethod analysis. The Analysis Specifications provided:
      # - studyPeriods (we apply the studyStartDate / studyEndDate here)
      # - maxCohortSize = 0
      #
      # We set restrictToCommonPeriod = TRUE to ensure PS is fit only on persons
      # whose exposure window overlaps the provided study period. This is consistent
      # with the template pattern for applying study periods during data extraction.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs: settings for PS model creation (Analysis Specifications)
      # - maxCohortSizeForFitting = 250000
      # - errorOnHighCorrelation = FALSE
      # - prior: laplace, useCrossValidation = TRUE
      # - control: Cyclops control with provided tuning parameters
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = FALSE,
        stopOnError = FALSE, # allow Strategus to continue if PS fitting fails for a pair
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
          noiseLevel = "quiet",
          resetCoefficients = FALSE,
          startingVariance = 0.01
        )
      )

      # Covariate balance computation args:
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs: settings for the outcome model (Analysis Specifications)
      # - modelType = "cox"
      # - stratified = FALSE
      # - useCovariates = FALSE
      # - inversePtWeighting = FALSE
      # - prior: laplace + cross-validation
      # - control: Cyclops control with provided tuning parameters and resetCoefficients = TRUE
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = FALSE,
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
          startingVariance = 0.01
        )
      )

      # createStudyPopArgs: intake of createStudyPopulationArgs from Analysis Specifications:
      # - restrictToCommonPeriod = FALSE
      # - firstExposureOnly = FALSE
      # - washoutPeriod = 365
      # - removeDuplicateSubjects = "keep all"
      # - censorAtNewRiskWindow = FALSE
      # - removeSubjectsWithPriorOutcome = TRUE
      # - priorOutcomeLookBack = 99999
      # Time-at-risk values come from the timeAtRisks tibble for this iteration.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1
      )

      # Append the analysis configuration to cmAnalysisList.
      # The description is human-readable and documents which study period, TAR,
      # and PS configuration are being used.
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

# Create the CohortMethod module specifications:
# - cmAnalysisList: built above
# - targetComparatorOutcomesList: the list we created while building analyses
# - refit flags and diagnostic thresholds left as default of template (no refitting)
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
# Compose the final analysis specifications object consumed by Strategus
# ------------------------------------------------------------------------------
# The name of the study (from Analysis Specifications) is "semaglutideandnaion".
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# ------------------------------------------------------------------------------
# Save the analysis specifications JSON to inst/<studyName>/<studyName>AnalysisSpecification.json
# This is the file that will be used by Strategus to run the study.
# ------------------------------------------------------------------------------
outputDir <- file.path("inst", "semaglutideandnaion")
if (!dir.exists(outputDir)) dir.create(outputDir, recursive = TRUE)
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path(outputDir, "semaglutideandnaionAnalysisSpecification.json")
)