library(dplyr)
library(Strategus)
library(ROhdsiWebApi)
library(FeatureExtraction)
library(CohortMethod)
library(Cyclops)
library(ParallelLogger)

################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script builds a Strategus analysis specification JSON using the
# OHDSI Strategus helper functions and the settings provided in the
# Analysis Specifications.  The script follows the structure of the
# provided Template and applies the exact names/IDs from the
# Analysis Specifications (no auto-correction of names).
#
# Annotations are provided throughout to explain how each block maps to
# the input settings.
################################################################################

# ----------------------
# Shared Resources
# ----------------------
# Base Atlas/WebAPI endpoint used to download cohort/definition resources.
# Change if you need to fetch definitions from a different Atlas instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# -----------------------------------------------------------------------------
# Cohort Definitions
# -----------------------------------------------------------------------------
# Use the cohort IDs from the Analysis Specifications exactly as provided:
# - targetCohort:       id = 1794126, name = "target1"
# - comparatorCohort:   id = 1794132, name = "comparator1"
# - outcomeCohort(s):   id = 1794131, name = "outcome1"
#
# We export these cohort definitions from the WebAPI and generate basic stats
# (generateStats = TRUE) which can be used by later modules (CohortDiagnostics, etc.).
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
# - target -> cohortId == 1
# - comparator -> cohortId == 2
# - outcome -> cohortId == 3
# This numbering convention is used throughout the template and Strategus modules.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# -----------------------------------------------------------------------------
# Negative control outcomes (Concept Set)
# -----------------------------------------------------------------------------
# The Analysis Specifications specify a negativeControlConceptSet with:
# id = 1888110, name = "negative"
# We fetch the concept set definition and resolve it to a list of concepts.
# Each negative control will be represented as its own cohort with cohortIds
# starting at 101 (100 + row number).
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
  mutate(cohortId = row_number() + 100) %>% # negative controls -> cohortIds 101, 102, ...
  select(cohortId, cohortName, outcomeConceptId)

# Safety check for duplicate cohort IDs between main cohortDefinitionSet and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between main cohorts and negative control cohorts ***")
}

# ----------------------
# Build lists/dataframes used by modules
# ----------------------

# Outcomes list (oList)
# We select the outcome cohort from the cohortDefinitionSet (cohortId == 3).
# We also attach a cleanWindow (used by some modules), using the createStudyPopArg
# priorOutcomeLookBack from the Analysis Specifications (365 days).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId,
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # priorOutcomeLookBack = 365 (exact from Analysis Specifications)

# Target and Comparator for the CohortMethod analysis (cmTcList)
# According to the Analysis Specifications:
# target name = "target1" (mapped to cohortId 1)
# comparator name = "comparator1" (mapped to cohortId 2)
# We also include placeholders for targetConceptId and comparatorConceptId (not provided in spec),
# set to NA. These columns exist in the Template and are used when assembling excluded covariates.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  targetConceptId = NA_integer_,
  comparatorConceptId = NA_integer_,
  stringsAsFactors = FALSE
)

# Excluded covariate concepts
# The Analysis Specifications' covariateSelection has empty conceptsToInclude / conceptsToExclude,
# so we create an empty data.frame for excludedCovariateConcepts (no covariates to exclude).
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character(),
  stringsAsFactors = FALSE
)

# ----------------------
# CohortGeneratorModule specifications
# ----------------------
# Use Strategus helper to create shared resource specifications for cohorts.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Negative controls as shared resource for cohort generator
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# CohortGenerator module specifications - generate stats for the cohorts (keeps in line with cohortDefinitionSet generateStats = TRUE)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ----------------------
# CohortDiagnosticsModule specifications
# ----------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# We provide the cohort IDs to run diagnostics on. This includes the main cohorts
# (1,2,3) and any negative control cohortIds (101, 102, ...).
allCohortIdsForDiagnostics <- c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)

cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = allCohortIdsForDiagnostics,
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

# ----------------------
# CohortMethodModule specifications
# ----------------------

# Study Periods
# The Analysis Specifications provided a studyPeriods list with null dates.
# Per the Template, if you are not restricting the study to a specific time window,
# use empty strings; but keep at least one row so the outer loop iterates once.
studyPeriods <- tibble::tibble(
  studyStartDate = c(""), # empty string => no restriction on start
  studyEndDate   = c("")  # empty string => no restriction on end
)

# Time-at-risks (TARs) from createStudyPopArgs in Analysis Specifications:
# One TAR defined:
# - riskWindowStart = 365
# - startAnchor = "cohort start"
# - riskWindowEnd = 99999
# - endAnchor = "cohort start"
# - minDaysAtRisk = 1
# We add a human-friendly label for the TAR; the template expects a label column.
timeAtRisks <- tibble::tibble(
  label = c("TAR_365_to_end"),
  riskWindowStart = c(365),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(99999),
  endAnchor = c("cohort start")
)

# Propensity Score settings
# The Analysis Specifications specify one PS configuration (match on PS):
# - maxRatio = 1
# - caliper = 0.2
# - caliperScale = "standardized logit"
matchOnPsArgsList <- tibble::tibble(
  label = c("match_on_ps_1"),
  maxRatio = c(1),
  caliper = c(0.2),
  caliperScale = c("standardized logit") # matches allowed values in template
)

# No stratify-by-PS settings specified in the Analysis Specifications
stratifyByPsArgsList <- tibble::tibble(
  label = character(),
  numberOfStrata = integer(),
  baseSelection = character()
)

# Build PS configuration list (psConfigList) as in the Template.
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

# Prepare lists that will become entries in the cmAnalysisList (one per analysis)
cmAnalysisList <- list()
analysisId <- 1

# Iterate over study periods, time-at-risks, and PS configurations to create analyses
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create covariateSettings to be used when extracting covariates.
      # The Analysis Specifications' covariateSelection lists are empty, so we
      # use default covariate settings (includes many covariates). If you want
      # to restrict covariates, modify this block to create setting objects that
      # include/exclude specific concept sets.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcomes: combine study outcome(s) (from oList) plus negative controls.
      # The Template creates CohortMethod::createOutcome objects for each.
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

      # Build the targetComparatorOutcomesList (one element per target-comparator pair).
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {

        # Assemble excluded covariate concept IDs:
        # - target concept id (if provided)
        # - comparator concept id (if provided)
        # - any additional excluded covariate concepts (empty in this analysis spec)
        excludedIds <- c()
        if (!is.na(cmTcList$targetConceptId[i])) excludedIds <- c(excludedIds, cmTcList$targetConceptId[i])
        if (!is.na(cmTcList$comparatorConceptId[i])) excludedIds <- c(excludedIds, cmTcList$comparatorConceptId[i])
        if (nrow(excludedCovariateConcepts) > 0) excludedIds <- c(excludedIds, excludedCovariateConcepts$conceptId)

        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedIds
        )
      }

      # Create the args for getDbCohortMethodData using the covariateSettings and
      # study period restrictions (empty strings mean no restriction).
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Create PS-fitting arguments (createPsArgs) according to Analysis Specifications:
      # - maxCohortSizeForFitting = 250000
      # - errorOnHighCorrelation = TRUE
      # - prior: Laplace with cross-validation
      # - control: tolerance, cvType, fold, cvRepetitions, noiseLevel, resetCoefficients, startingVariance
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # keep FALSE so Strategus attempts all tasks; aligns with Template pattern
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

      # Compute covariate balance args (shared and per-analysis)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit outcome model args from Analysis Specifications:
      # - modelType = "cox"
      # - stratified = FALSE
      # - useCovariates = FALSE
      # - inversePtWeighting = FALSE
      # - prior = laplace with cross-validation
      # - control settings as provided
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = FALSE,
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

      # Create study population args (createStudyPopArgs) using values from Analysis Specifications:
      # - restrictToCommonPeriod = FALSE
      # - firstExposureOnly = FALSE
      # - washoutPeriod = 365
      # - removeDuplicateSubjects = "keep all"
      # - censorAtNewRiskWindow = FALSE
      # - removeSubjectsWithPriorOutcome = TRUE
      # - priorOutcomeLookBack = 365
      # - riskWindowStart/End and anchors from timeAtRisks row
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 365,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Construct PS adjustment method-specific arguments
      matchOnPsArgs <- NULL
      stratifyByPsArgs <- NULL
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
      } else if (psCfg$method == "stratify") {
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Append the CohortMethod analysis configuration to cmAnalysisList
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(studyStartDate == "", "unrestricted", studyStartDate),
          ifelse(studyEndDate == "", "unrestricted", studyEndDate),
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
    } # end p loop (psConfigList)
  } # end t loop (timeAtRisks)
} # end s loop (studyPeriods)

# Now create the CohortMethod module specifications via the Strategus helper.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# ----------------------
# Compose the overall analysisSpecifications object
# ----------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# ----------------------
# Save the analysis specifications JSON
# ----------------------
# Use the exact analysis name from the Analysis Specifications for the file path:
# "ranitidinecancer"
outputFile <- file.path("inst", "ranitidinecancer", "ranitidinecancerAnalysisSpecification.json")

# Ensure the destination directory exists
outputDir <- dirname(outputFile)
if (!dir.exists(outputDir)) dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)

# Save to JSON using ParallelLogger helper (keeps Strategus/ParallelLogger formatting)
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  outputFile
)

# Informational message (not required by the instruction but useful when running the script)
message("Strategus analysis specification saved to: ", outputFile)