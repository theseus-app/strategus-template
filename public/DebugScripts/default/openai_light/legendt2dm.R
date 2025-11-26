################################################################################
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
#
# This script builds Strategus analysis specifications based on the
# Analysis Specifications provided for the study named exactly:
#   legendt2dm
#
# IMPORTANT:
# - Cohort and concept IDs and all setting names are taken exactly from the
#   Analysis Specifications JSON. Do NOT rename these items if you intend to
#   preserve exact mapping between the spec and generated JSON.
# - This script uses ROhdsiWebApi to fetch cohort and concept set definitions
#   from an Atlas/WebAPI instance. Update baseUrl if you are not using the
#   demo Atlas instance.
#
# The resulting analysis specification JSON file will be written to:
#   inst/legendt2dm/legendt2dmAnalysisSpecification.json
#
################################################################################

library(dplyr)
library(Strategus)

# Base Atlas/WebAPI url to fetch cohort definitions and concept sets.
# (Template default is the Atlas demo instance; change if needed)
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ------------------------------------------------------------------------------
# Cohort Definitions
# ------------------------------------------------------------------------------

# Export the cohort definitions specified in the Analysis Specifications.
# The Analysis Specifications specify:
#  - targetCohort: id = 1794126, name = "target1"
#  - comparatorCohort: id = 1794132, name = "comparator1"
#  - outcomeCohort: id = 1794131, name = "outcome1"
#
# We fetch them and then re-number them to 1 (target), 2 (comparator), 3 (outcome)
# to be consistent with downstream module conventions used in this template.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to 1,2,3 to simplify references in CohortMethod module
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# ------------------------------------------------------------------------------
# Negative control outcomes concept set
# ------------------------------------------------------------------------------

# The Analysis Specifications specify a negative control concept set:
#   id = 1888110, name = "negative"
#
# We fetch the concept set definition, resolve descendants, and expand to
# a list of concepts which we convert to a pseudo-cohort-set for use as
# negative control outcomes. We assign cohortIds starting at 101 to avoid
# colliding with the target/comparator/outcome cohorts (1,2,3...).
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
  dplyr::mutate(cohortId = row_number() + 100) %>% # negative controls -> 101, 102, ...
  dplyr::select(cohortId, cohortName, outcomeConceptId)

# Safety check for duplicate cohort ids
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# ------------------------------------------------------------------------------
# Build the lists used by CohortMethod module
# ------------------------------------------------------------------------------

# Outcomes: pick out the (re-numbered) outcome cohort (id == 3)
# We also include a cleanWindow (lookback for prior outcomes) here consistent
# with the template pattern. The Analysis Specifications set priorOutcomeLookBack
# to 99999 later in study population creation; here we set a generic cleanWindow.
oList <- cohortDefinitionSet %>%
  dplyr::filter(.data$cohortId == 3) %>%
  dplyr::mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  dplyr::select(outcomeCohortId, outcomeCohortName) %>%
  dplyr::mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# The Analysis Specifications map:
#  - targetCohort -> id 1 (renumbered), name "target1"
#  - comparatorCohort -> id 2 (renumbered), name "comparator1"
# NOTE: We include placeholder columns targetConceptId and comparatorConceptId
# to match the template's usage (they are NA because no specific concept ids
# were provided in the Analysis Specifications).
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  targetConceptId = NA_integer_,
  comparatorConceptId = NA_integer_,
  stringsAsFactors = FALSE
)

# Excluded covariate concepts for LPS/LSPS generation
# Analysis Specifications provided an empty conceptsToInclude/Exclude lists,
# so we create an empty data.frame to pass through the template flow.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# CohortGeneratorModule specifications
# ------------------------------------------------------------------------------

cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource for cohort definitions (cohortDefinitionSet)
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Shared resource for negative control outcomes (negativeControlOutcomeCohortSet)
# We detect on descendants and use the 'first' occurrence for negative controls
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specification to actually generate cohorts (generateStats = TRUE retains template behaviour)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ------------------------------------------------------------------------------
# CohortDiagnosticsModule specifications
# ------------------------------------------------------------------------------

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
# CohortMethodModule specifications
# ------------------------------------------------------------------------------

# Study periods - from Analysis Specifications getDbCohortMethodDataArgs.studyPeriods:
# single entry: studyStartDate = "19920101", studyEndDate = "20211231"
studyPeriods <- tibble::tibble(
  studyStartDate = c("19920101"),
  studyEndDate   = c("20211231")
)

# Time-at-risks (TARs) - from Analysis Specifications createStudyPopArgs.timeAtRisks:
# single entry with riskWindowStart = 1, startAnchor = "cohort start",
# riskWindowEnd = 0, endAnchor = "cohort end", minDaysAtRisk = 1
timeAtRisks <- tibble::tibble(
  label = c("TAR_1_start1_end0"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"), # allowed: "cohort start" | "cohort end"
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end"),     # allowed: "cohort start" | "cohort end"
  minDaysAtRisk = c(1)
)

# Propensity Score settings:
# Analysis Specifications defined two PS settings:
# 1) match on PS: maxRatio = 100, caliper = 0.2, caliperScale = "standardized logit"
# 2) stratify by PS: numberOfStrata = 5, baseSelection = "all"
matchOnPsArgsList <- tibble::tibble(
  label = c("match_on_ps"),
  maxRatio  = c(100),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # as specified
)

stratifyByPsArgsList <- tibble::tibble(
  label = c("stratify_by_ps"),
  numberOfStrata  = c(5),
  baseSelection = c("all") # as specified
)

# Build a single psConfigList combining match and stratify entries while
# preserving their labels and parameters.
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

# Prepare to create CohortMethod analyses: iterate over study periods, TARs, and PS configurations
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create either matchOnPsArgs or stratifyByPsArgs according to psCfg$method
      if (psCfg$method == "match") {
        # createMatchOnPsArgs parameters taken from psCfg$params (exact names from template/spec)
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
        stop("Unknown PS method in psConfigList: ", psCfg$method)
      }

      # Covariate settings: use default covariate settings and keep the option
      # to add descendants to exclude (template default)
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build outcome list:
      # - include the main outcome(s) defined in oList as outcomes of interest
      # - include negative control outcomes from negativeControlOutcomeCohortSet as non-interest
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

      # Build target-comparator-outcome mapping for CohortMethod:
      # createTargetComparatorOutcomes expects targetId, comparatorId and outcomes.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        # Build excluded covariate ids as in template: include the target/comparator concept ids
        # and explicit excludedCovariateConcepts. We also na.omit to avoid passing NA values.
        excludedIds <- na.omit(
          c(
            cmTcList$targetConceptId[i],
            cmTcList$comparatorConceptId[i],
            excludedCovariateConcepts$conceptId
          )
        )
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = as.integer(excludedIds)
        )
      }

      # getDbCohortMethodDataArgs - create with settings from Analysis Specifications:
      # Analysis Specifications getDbCohortMethodDataArgs:
      #  - studyPeriods: use studyStartDate, studyEndDate
      #  - maxCohortSize = 0
      #  - restrictToCommonPeriod = false
      #  - firstExposureOnly = false
      #  - washoutPeriod = 0
      #  - removeDuplicateSubjects = "keep all"
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs - map Analysis Specifications createPsArgs exactly:
      # maxCohortSizeForFitting = 250000
      # errorOnHighCorrelation = TRUE
      # prior: priorType = "laplace", useCrossValidation = TRUE
      # control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      #          noiseLevel = "silent", resetCoefficients = TRUE, startingVariance = 0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
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

      # Compute covariate balance args - keep template defaults but restrict
      # computeCovariateBalanceArgs to Table1 specifications for a descriptive balance
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs - from Analysis Specifications:
      # modelType = "cox", stratified = TRUE, useCovariates = FALSE,
      # inversePtWeighting = FALSE
      # prior: priorType = "laplace", useCrossValidation = TRUE
      # control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      #          noiseLevel = "quiet", resetCoefficients = TRUE, startingVariance = 0.01
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

      # createStudyPopArgs - from Analysis Specifications createStudyPopArgs:
      # restrictToCommonPeriod = false, firstExposureOnly = false, washoutPeriod = 0,
      # removeDuplicateSubjects = "keep all", censorAtNewRiskWindow = false,
      # removeSubjectsWithPriorOutcome = true, priorOutcomeLookBack = 99999,
      # timeAtRisks -> riskWindowStart/riskWindowEnd etc
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

      # Create a human readable description summarizing this analysis config
      description <- sprintf(
        "Study: %s-%s; TAR: %s; PS: %s",
        studyStartDate,
        studyEndDate,
        timeAtRisks$label[t],
        psCfg$label
      )

      # Append the CohortMethod analysis to the list
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

# Create CohortMethod module specifications:
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
# Assemble Strategus analysis specifications and write to file
# ------------------------------------------------------------------------------

analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Ensure target directory exists before attempting to write JSON file
# (Fixes "cannot open file ... No such file or directory" errors)
outputDir <- file.path("inst", "legendt2dm")
if (!dir.exists(outputDir)) {
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
}

# Save to inst/<studyName>/<studyName>AnalysisSpecification.json
# Study name (exactly from Analysis Specifications) is: legendt2dm
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path(outputDir, "legendt2dmAnalysisSpecification.json")
)