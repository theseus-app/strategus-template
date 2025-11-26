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
# This script creates a Strategus analysis specification JSON for the study
# defined in <Analysis Specifications>. The script follows the structure of the
# provided <Template> but applies the exact settings from the
# <Analysis Specifications> JSON.
#
# IMPORTANT:
# - Cohort IDs and names are used exactly as provided in the Analysis
#   Specifications (no auto-correction).
# - The output analysis specification file is written to:
#     inst/covid19famotidine/covid19famotidineAnalysisSpecification.json
#
# The script contains detailed inline annotations describing how the provided
# settings are mapped to Strategus / CohortMethod settings.
################################################################################

# Base URL for the Atlas WebAPI used to export cohort definitions and concept sets.
# The Template used the atlas-demo server. Change this if you need to export from
# another Atlas instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ----------------------------------------------------------------------
# Cohort Definitions
# ----------------------------------------------------------------------
# Export the cohorts that will be used in the study. The Analysis
# Specifications define 3 cohorts (target, comparator, outcome). We use the
# same numeric cohort ids as given:
#   - target cohort id: 1794126  (name: "target1")
#   - comparator cohort id: 1794132 (name: "comparator1")
#   - outcome cohort id: 1794131 (name: "outcome1")
#
# We set generateStats = TRUE to include cohort statistics in the exported
# cohortDefinitionSet.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts so that:
#   target -> cohortId = 1
#   comparator -> cohortId = 2
#   outcome -> cohortId = 3
# This renumbering is used throughout Strategus/CohortMethod module
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# ----------------------------------------------------------------------
# Negative Control Concept Set
# ----------------------------------------------------------------------
# The Analysis Specifications provide a concept set id for negative controls:
#   conceptSetId = 1888110 (name: "negative")
# We will resolve this concept set into the set of concepts that will become
# negative control outcome cohorts. The template pipeline converts the concept
# set into concept rows and then assigns cohortIds starting at 101 to avoid
# colliding with the main study cohorts (1,2,3...).
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
  # Assign cohort IDs for negative controls starting at 101, 102, ...
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no duplicate cohort ids between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between main cohorts and negative controls ***")
}

# ----------------------------------------------------------------------
# Build lists/data frames used in module specifications
# ----------------------------------------------------------------------

# Outcomes: create the outcome list from the cohortDefinitionSet for the
# outcome of interest(s). The re-numbered outcome cohort has cohortId == 3.
# We also add a clean window (cleanWindow) — commonly used in cohort diagnostics.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # days

# Target and Comparator for the CohortMethod analysis
# Use the exact cohort names provided in the Analysis Specifications:
#   targetCohortName = "target1"
#   comparatorCohortName = "comparator1"
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  stringsAsFactors = FALSE
)

# Excluded covariates:
# The Analysis Specifications specify empty lists for conceptsToInclude / conceptsToExclude.
# We therefore create an empty excludedCovariateConcepts data.frame to pass through the pipeline.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# ----------------------------------------------------------------------
# CohortGeneratorModule settings
# ----------------------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource for the exported cohortDefinitionSet
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Shared resource for negative controls (converted to cohorts). We follow the
# Template: occurrenceType = "first" and detectOnDescendants = TRUE
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications for cohort generation (generateStats = TRUE so that
# cohort statistics are produced)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ----------------------------------------------------------------------
# CohortDiagnosticsModule settings
# ----------------------------------------------------------------------
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

# ----------------------------------------------------------------------
# CohortMethodModule settings
# ----------------------------------------------------------------------

# The Analysis Specifications defines a single study period:
#   studyStartDate = "20200201"
#   studyEndDate   = "20200530"
# We convert these to the tibble used by the template pipeline.
studyPeriods <- tibble::tibble(
  studyStartDate = c("20200201"), # YYYYMMDD as strings
  studyEndDate   = c("20200530")
)

# Time-at-risks (TAR) defined in Analysis Specifications:
# Single TAR:
#   riskWindowStart = 1
#   startAnchor = "cohort start"
#   riskWindowEnd = 30
#   endAnchor = "cohort start"
#   minDaysAtRisk = 1
timeAtRisks <- tibble::tibble(
  label = c("1to30_from_cohort_start"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(30),
  endAnchor = c("cohort start"),
  minDaysAtRisk = c(1)
)

# Propensity score configuration from Analysis Specifications:
# There is a single PS setting that uses stratification:
#   numberOfStrata = 5
#   baseSelection = "all"
stratifyByPsArgsList <- tibble::tibble(
  label = c("stratify_5_all"),
  numberOfStrata  = c(5),
  baseSelection = c("all"),
  stringsAsFactors = FALSE
)

# No "match on PS" settings provided in the Analysis Specifications, so leave matchOnPsArgsList empty.
matchOnPsArgsList <- tibble::tibble(
  label = character(0),
  maxRatio  = numeric(0),
  caliper = numeric(0),
  caliperScale  = character(0),
  stringsAsFactors = FALSE
)

# Build a single PS configuration list (each entry contains method, label, params)
psConfigList <- list()

# Convert match-on-PS rows to psConfigList entries (none in this study)
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

# Convert stratify-by-PS rows to psConfigList entries (one in this study)
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

# Prepare the outcomeList: include the outcome(s) of interest (from oList)
# plus the negative controls (these are non-target outcomes with trueEffectSize = 1).
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
    # Negative control outcomes: marked as NOT outcome of interest, trueEffectSize = 1
    CohortMethod::createOutcome(
      outcomeId = i,
      outcomeOfInterest = FALSE,
      trueEffectSize = 1
    )
  })
)

# Build targetComparatorOutcomesList for the CohortMethod module.
# Each entry links a target-comparator pair to the outcomeList. The template
# included excluded covariates based on study-specific concepts (target/comparator
# drugs); our Analysis Specifications did not supply such concept ids, so we only
# pass the explicit excludedCovariateConcepts (empty here).
targetComparatorOutcomesList <- list()
for (i in seq_len(nrow(cmTcList))) {
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
  )
}

# Covariate settings:
# Analysis Specifications provided empty concept include/exclude lists. We
# therefore use the CohortMethod default covariate settings (FeatureExtraction
# defaults). The Template used createDefaultCovariateSettings with
# addDescendantsToExclude = TRUE which we also apply here.
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE
)

# Build CohortMethod analyses (iterate combinations of studyPeriods x timeAtRisks x PS configs)
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Depending on the PS method type, create either matchOnPsArgs or stratifyByPsArgs.
      if (psCfg$method == "match") {
        # This branch won't be used for this study because no match configs were supplied,
        # but we include it for completeness.
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # Analysis Specifications require stratification by PS with numberOfStrata = 5,
        # baseSelection = "all".
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      } else {
        stop("Unknown PS method in psConfigList")
      }

      # getDbCohortMethodDataArgs:
      # Map the Analysis Specifications getDbCohortMethodDataArgs:
      #   studyPeriods: already set above
      #   maxCohortSize: 0
      # We set restrictToCommonPeriod = TRUE in getDbCohortMethodDataArgs to match
      # the Template default behavior for data extraction; note that the
      # createStudyPopArgs.restrictToCommonPeriod is set separately (and is set
      # to FALSE as requested in the Analysis Specifications).
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs:
      # The Analysis Specifications provided detailed Cyclops prior/control settings:
      #   maxCohortSizeForFitting = 250000
      #   errorOnHighCorrelation = TRUE
      #   prior: priorType = "laplace", useCrossValidation = TRUE
      #   control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      #            noiseLevel = "silent", resetCoefficients = TRUE, startingVariance = 0.01
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # allow Strategus pipeline to continue when a PS cannot be fit for some pairs
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

      # Compute covariate balance args:
      # Shared covariate balance for diagnostics and covariate balance for table1.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs:
      # Map settings from Analysis Specifications:
      #   modelType = "cox", stratified = TRUE, useCovariates = FALSE, inversePtWeighting = FALSE
      #   prior: laplace with useCrossValidation = TRUE
      #   control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      #            noiseLevel = "quiet", resetCoefficients = TRUE, startingVariance = 0.01
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
          tolerance = 2e-07,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # createStudyPopArgs:
      # Map to Analysis Specifications createStudyPopArgs:
      #   restrictToCommonPeriod = FALSE
      #   firstExposureOnly = FALSE
      #   washoutPeriod = 0
      #   removeDuplicateSubjects = "keep all"
      #   censorAtNewRiskWindow = FALSE
      #   removeSubjectsWithPriorOutcome = TRUE
      #   priorOutcomeLookback = 30
      #   timeAtRisks: riskWindowStart, startAnchor, riskWindowEnd, endAnchor, minDaysAtRisk=1
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 30,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999
      )

      # Compose the CohortMethod analysis specification and append it to cmAnalysisList.
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
    } # end psConfigList loop
  } # end timeAtRisks loop
} # end studyPeriods loop

# Create the CohortMethod module specifications object
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  # The Analysis Specifications do not request refitting PS for every outcome/population
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# ----------------------------------------------------------------------
# Compose final analysisSpecifications object and save JSON
# ----------------------------------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Ensure the output directory exists (inst/covid19famotidine)
outputFolder <- file.path("inst", "covid19famotidine")
if (!dir.exists(outputFolder)) dir.create(outputFolder, recursive = TRUE)

# Save the analysis specifications JSON using ParallelLogger helper.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path(outputFolder, "covid19famotidineAnalysisSpecification.json")
)

# End of script.