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
# This script builds a Strategus analysis specification JSON using the
# OHDSI Strategus package. The settings are taken exactly from the provided
# Analysis Specifications document (no name auto-correction). Detailed
# comments are provided to explain how each setting from the specifications
# is applied.
#
# IMPORTANT:
# - This script uses the exact cohort IDs and names as provided in the
#   Analysis Specifications JSON.
# - The final JSON is saved to inst/iudehre/iudehreAnalysisSpecification.json
#   (study name taken from Analysis Specifications: "iudehre").
################################################################################

# ------------------------------------------------------------------------------
# Shared resources / WebAPI settings
# ------------------------------------------------------------------------------
# Base Atlas WebAPI URL used to fetch cohort definitions and concept sets.
# This template uses the Atlas demo instance by default. Replace if needed.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ------------------------------------------------------------------------------
# Cohort definitions (use EXACT ids & names from Analysis Specifications)
# ------------------------------------------------------------------------------
# Analysis Specifications:
#   targetCohort:      id = 1794126, name = "target1"
#   comparatorCohort:  id = 1794132, name = "comparator1"
#   outcomeCohort:     id = 1794131, name = "outcome1"
#
# We export the cohort definition set using the ROhdsiWebApi function and then
# renumber cohorts to smaller internal IDs (1, 2, 3) so they play nicely as
# shared resources within Strategus.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to internal ids: target -> 1, comparator -> 2, outcome -> 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# ------------------------------------------------------------------------------
# Negative control outcomes (Concept Set)
# ------------------------------------------------------------------------------
# Analysis Specifications:
#   negativeControlConceptSet: id = 1888110, name = "negative"
#
# We obtain the concept set definition for the given conceptSetId and then
# resolve it to explicit concepts. We then assign cohort IDs for these
# negative control outcomes starting at 101 (so they don't collide with
# target/comparator/outcome cohort ids 1,2,3).
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # exact id from specifications
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(
    baseUrl = baseUrl
  ) %>%
  ROhdsiWebApi::getConcepts(
    baseUrl = baseUrl
  ) %>%
  # rename fields to the names expected later by Strategus components
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  # assign cohort ids for the negative controls starting at 101
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Sanity check: ensure no duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# ------------------------------------------------------------------------------
# Prepare lists/data frames that will be used to construct CohortMethod analyses
# ------------------------------------------------------------------------------
# Build outcomes list (oList). We convert the exported outcome cohort (internal
# id 3) into the oList format required by the template. The 'cleanWindow'
# corresponds to the look-back period to ignore repeated events (set per specs).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId,
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # The Analysis Specifications used a priorOutcomeLookBack of 99999 for createStudyPopArgs;
  # in template the per-outcome clean window was 365. We document and set 365 here for
  # the outcome list's cleanWindow (this can be adjusted if different semantics are desired).
  mutate(cleanWindow = 365)

# Target and Comparator for CohortMethod analyses (use exact names/IDs)
# Analysis Specifications mapping:
#   targetCohort -> internal id 1 (name: "target1")
#   comparatorCohort -> internal id 2 (name: "comparator1")
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  # The template referenced targetConceptId/comparatorConceptId in excluded covariate lists.
  # The Analysis Specifications did not supply such concept ids; we include NA columns
  # so downstream code that references these columns will not error (they contribute
  # nothing to the excluded covariate vector).
  targetConceptId = NA_integer_,
  comparatorConceptId = NA_integer_,
  stringsAsFactors = FALSE
)

# Excluded covariates (the template expects a data.frame of conceptIds to exclude).
# The Analysis Specifications' covariateSelection lists were empty, so we keep this empty.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# Covariate settings
# ------------------------------------------------------------------------------
# The Analysis Specifications provided an empty 'conceptsToInclude' / 'conceptsToExclude'
# list. We'll use the default covariate set (FeatureExtraction::createDefaultCovariateSettings).
# We set addDescendantsToExclude = TRUE to ensure any excluded concepts (if later provided)
# will have their descendants excluded as well.
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE
)

# ------------------------------------------------------------------------------
# Build the outcome list combining the main outcome(s) and the negative controls
# ------------------------------------------------------------------------------
# The CohortMethod outcome objects must identify which outcomes are "outcome of interest"
# (trueEffectSize = NA) and which are negative controls (trueEffectSize = 1).
outcomeList <- append(
  # Primary outcomes from oList: these are the outcomes of interest
  lapply(seq_len(nrow(oList)), function(i) {
    CohortMethod::createOutcome(
      outcomeId = oList$outcomeCohortId[i],
      outcomeOfInterest = TRUE,
      trueEffectSize = NA,
      priorOutcomeLookback = 99999
    )
  }),
  # Negative control outcomes derived from the concept set resolution above
  lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
    CohortMethod::createOutcome(
      outcomeId = i,
      outcomeOfInterest = FALSE,
      trueEffectSize = 1
    )
  })
)

# Build the targetComparatorOutcomesList: for each target/comparator pair include the
# shared outcome list and any excluded covariate concept ids (none in this specification)
targetComparatorOutcomesList <- list()
for (i in seq_len(nrow(cmTcList))) {
  # Build a vector of excluded covariate concept ids:
  # - the template included target/comparator concept ids (not provided here)
  # - plus the excludedCovariateConcepts data.frame (empty here)
  excludedCovariateConceptIds <- c(
    # these entries are NA from cmTcList as placeholder; remove NA values
    cmTcList$targetConceptId[i],
    cmTcList$comparatorConceptId[i],
    excludedCovariateConcepts$conceptId
  )
  excludedCovariateConceptIds <- excludedCovariateConceptIds[!is.na(excludedCovariateConceptIds)]

  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    excludedCovariateConceptIds = excludedCovariateConceptIds
  )
}

# ------------------------------------------------------------------------------
# CohortGeneratorModule: create shared resource specifications and module specs
# ------------------------------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Negative controls shared resource for cohort generator (occurrenceType = "first")
# detectOnDescendants = TRUE ensures concept descendants are resolved when building cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications for cohort generation: we requested generateStats = TRUE above
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ------------------------------------------------------------------------------
# CohortDiagnosticsModule: settings (we enable a broad set of diagnostics)
# ------------------------------------------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # this is c(1,2,3) after renumbering
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
# CohortMethod Module: prepare study periods, time-at-risks, and PS configurations
# ------------------------------------------------------------------------------
# The Analysis Specifications provided a single study period with start = "20030101"
# and studyEndDate = NULL (no end). We store as a tibble; when creating CM args
# we will convert NA -> NULL so the CreateGetDbCohortMethodDataArgs receives NULL.
studyPeriods <- tibble::tibble(
  studyStartDate = c("20030101"), # YYYYMMDD as string
  studyEndDate   = c(NA_character_) # NA to indicate 'no end' (converted to NULL later)
)

# Time-at-risk windows: two TARs specified in the Analysis Specifications
# 1) riskWindowStart = 30, startAnchor = "cohort start", riskWindowEnd = 5475, endAnchor = "cohort start", minDaysAtRisk = 1
# 2) riskWindowStart = 365, startAnchor = "cohort start", riskWindowEnd = 5475, endAnchor = "cohort start", minDaysAtRisk = 1
timeAtRisks <- tibble::tibble(
  label = c("30_to_5475_from_cohort_start", "365_to_5475_from_cohort_start"),
  riskWindowStart = c(30L, 365L),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd = c(5475L, 5475L),
  endAnchor = c("cohort start", "cohort start"),
  minDaysAtRisk = c(1L, 1L)
)

# Propensity Score settings (two PS strategies specified)
# 1) match on PS: maxRatio = 1, caliper = 0.2, caliperScale = "standardized logit"
# 2) stratify by PS: numberOfStrata = 5, baseSelection = "all"
matchOnPsArgsList <- tibble::tibble(
  label = c("match_1_0.2_stdlogit"),
  maxRatio = c(1L),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

stratifyByPsArgsList <- tibble::tibble(
  label = c("stratify_5_all"),
  numberOfStrata = c(5L),
  baseSelection = c("all")
)

# Build a single combined PS configuration list (psConfigList) that we will iterate
psConfigList <- list()

# Convert matchOnPsArgsList rows to psConfigList elements (method = "match")
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

# Convert stratifyByPsArgsList rows to psConfigList elements (method = "stratify")
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

# ------------------------------------------------------------------------------
# Prepare common Cyclops prior and control objects used for PS creation and
# outcome model fitting (values taken from Analysis Specifications)
# ------------------------------------------------------------------------------
# createPrior for cyclops as specified:
#   priorType = "laplace"
#   useCrossValidation = TRUE
cyclopsPriorForPsAndOutcome <- Cyclops::createPrior(
  priorType = "laplace",
  exclude = c(0),
  useCrossValidation = TRUE
)

# createControl for Cyclops used when fitting PS models and outcome models.
# The Analysis Specifications provided the following control parameters for PS:
#   tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
#   noiseLevel = "silent", resetCoefficients = true, startingVariance = 0.01
cyclopsControlForPs <- Cyclops::createControl(
  tolerance = 2e-07,
  cvType = "auto",
  seed = 1,
  fold = 10,
  cvRepetitions = 10,
  noiseLevel = "silent",
  resetCoefficients = TRUE,
  startingVariance = 0.01
)

# For the outcome model, Analysis Specifications indicated noiseLevel = "quiet"
cyclopsControlForOutcome <- Cyclops::createControl(
  tolerance = 2e-07,
  cvType = "auto",
  seed = 1,
  fold = 10,
  cvRepetitions = 10,
  noiseLevel = "quiet",
  resetCoefficients = TRUE,
  startingVariance = 0.01
)

# ------------------------------------------------------------------------------
# Build CohortMethod analyses by iterating through studyPeriods, timeAtRisks,
# and PS configurations (psConfigList)
# ------------------------------------------------------------------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  # Study period boundaries
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]
  # Convert NA studyEndDate -> NULL so createGetDbCohortMethodDataArgs receives NULL
  if (is.na(studyEndDate)) {
    studyEndDateArg <- NULL
  } else {
    studyEndDateArg <- studyEndDate
  }

  for (t in seq_len(nrow(timeAtRisks))) {
    # TAR fields for this iteration
    tar_label <- timeAtRisks$label[t]
    riskWindowStart_val <- timeAtRisks$riskWindowStart[t]
    startAnchor_val <- timeAtRisks$startAnchor[t]
    riskWindowEnd_val <- timeAtRisks$riskWindowEnd[t]
    endAnchor_val <- timeAtRisks$endAnchor[t]
    minDaysAtRisk_val <- timeAtRisks$minDaysAtRisk[t]

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Depending on the PS configuration, create matchOnPsArgs or stratifyByPsArgs
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
        stop("Unknown PS method encountered in psConfigList")
      }

      # createGetDbCohortMethodDataArgs: arguments mapped from Analysis Specifications:
      #   studyPeriods: studyStartDate = "20030101", studyEndDate = NULL
      #   maxCohortSize = 0
      #   restrictToCommonPeriod = FALSE
      #   firstExposureOnly = TRUE
      #   washoutPeriod = 365
      #   removeDuplicateSubjects = "remove all"
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDateArg,
        maxCohortSize = 0,
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = TRUE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "remove all",
        covariateSettings = covariateSettings
      )

      # createCreatePsArgs: mapping from Analysis Specifications
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # keep Strategus completing other tasks when fitting fails
        estimator = "att",
        prior = cyclopsPriorForPsAndOutcome,
        control = cyclopsControlForPs
      )

      # compute covariate balance args (shared and per-analysis)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs: mapping from Analysis Specifications:
      #   modelType = "cox", stratified = TRUE, useCovariates = FALSE, inversePtWeighting = FALSE
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = TRUE,
        useCovariates = FALSE,
        inversePtWeighting = FALSE,
        prior = cyclopsPriorForPsAndOutcome,
        control = cyclopsControlForOutcome
      )

      # createStudyPopArgs: mapping from Analysis Specifications:
      #   restrictToCommonPeriod = FALSE, firstExposureOnly = FALSE, washoutPeriod = 0,
      #   removeDuplicateSubjects = "keep all", censorAtNewRiskWindow = FALSE,
      #   removeSubjectsWithPriorOutcome = FALSE, priorOutcomeLookBack = 99999,
      #   risk window values taken from timeAtRisks tibble
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = FALSE,
        priorOutcomeLookback = 99999,
        riskWindowStart = riskWindowStart_val,
        startAnchor = startAnchor_val,
        riskWindowEnd = riskWindowEnd_val,
        endAnchor = endAnchor_val,
        minDaysAtRisk = minDaysAtRisk_val,
        maxDaysAtRisk = 99999
      )

      # Build human-readable description for this analysis for provenance
      description <- sprintf(
        "Study: %s-%s; TAR: %s; PS: %s",
        studyStartDate,
        ifelse(is.null(studyEndDateArg), "", studyEndDateArg),
        tar_label,
        psCfg$label
      )

      # Append the CohortMethod analysis specification to the cmAnalysisList
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
    } # end PS loop
  } # end TAR loop
} # end studyPeriods loop

# ------------------------------------------------------------------------------
# Create CohortMethod module specifications using the cmAnalysisList and
# targetComparatorOutcomesList we prepared earlier.
# - refitPsForEveryOutcome: FALSE (per Analysis Specifications)
# - refitPsForEveryStudyPopulation: FALSE (per Analysis Specifications)
# - cmDiagnosticThresholds: default thresholds
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
# Assemble the full Strategus analysis specifications object
# ------------------------------------------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() %>%
  Strategus::addSharedResources(cohortDefinitionShared) %>%
  Strategus::addSharedResources(negativeControlsShared) %>%
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# ------------------------------------------------------------------------------
# Save analysis specification JSON to inst/iudehre/iudehreAnalysisSpecification.json
# (file path uses the study name exactly as provided: "iudehre")
# ------------------------------------------------------------------------------
outputFile <- file.path("inst", "iudehre", "iudehreAnalysisSpecification.json")

# Create target directory if it doesn't exist
dir.create(dirname(outputFile), recursive = TRUE, showWarnings = FALSE)

# Save JSON
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  outputFile
)

# End of script.