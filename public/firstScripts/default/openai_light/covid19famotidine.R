library(dplyr)
library(tibble)
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(ParallelLogger)

################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script uses the OHDSI Strategus package to build an analysis
# specification JSON for the study defined in the Analysis Specifications.
# The analysis is named exactly as in the specifications: "covid19famotidine".
#
# The script mirrors the structure of the provided Template and applies the
# settings from the Analysis Specifications JSON.  Detailed annotations are
# provided throughout to help users understand how each setting is applied.
################################################################################

# ------------------------------------------------------------------------------
# Shared Resources -------------------------------------------------------------
# ------------------------------------------------------------------------------

# WebAPI base URL (using the Atlas demo server in the Template).
# If you run this against your own Atlas/ATLAS WebAPI instance, change this URL.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ------------------------------------------------------------------------------
# Cohort Definitions -----------------------------------------------------------
# ------------------------------------------------------------------------------

# Export the cohort definitions from the WebAPI using the EXACT cohort IDs from
# the Analysis Specifications:
#   targetCohort id = 1794126  (name: "target1")
#   comparatorCohort id = 1794132 (name: "comparator1")
#   outcomeCohort id = 1794131  (name: "outcome1")
#
# generateStats = TRUE to capture cohort-generation statistics (useful for
# cohort diagnostics and review).
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts so that the study uses the conventional small IDs:
#   target -> 1
#   comparator -> 2
#   outcome -> 3
# This renumbering is purely internal to the analysis specifications and keeps
# the rest of the specification compact.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# ------------------------------------------------------------------------------
# Negative Control Outcomes ----------------------------------------------------
# ------------------------------------------------------------------------------

# The Analysis Specifications provide a single negative control concept set:
#   id = 1888110 (name "negative")
# We download, resolve, and expand it to concrete concepts and convert into a
# cohort-like data.frame that Strategus expects for negative controls.
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
  # rename columns to the convention used in the Template:
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  # assign cohortIds for negative controls that avoid colliding with target/comparator IDs:
  # Template used +100 offset (i.e. negative control cohort ids start at 101)
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Quick sanity check: ensure there are no duplicated cohort IDs between our main
# cohorts (1,2,3) and the negative controls.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# ------------------------------------------------------------------------------
# Build Outcome & Target/Comparator Data.frames for CohortMethod --------------
# ------------------------------------------------------------------------------

# Outcomes (oList)
# Use the renumbered cohortId 3 for the outcome specified in the Analysis Specs.
# The template used cleanWindow = 365 for outcome definitions; keep the same.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId,
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for CohortMethod analyses (cmTcList)
# Use the exact names from the Analysis Specifications for target/comparator.
# Note: template expects certain columns (targetCohortId, targetCohortName,
# comparatorCohortId, comparatorCohortName). We also add columns
# targetConceptId and comparatorConceptId to match the Template variable names
# used later; these are left as NA because the Analysis Specifications did not
# provide explicit concept ids for the drug exposures.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  # The following two columns exist in the Template references but are not
  # provided in the Analysis Specifications. We create them (with NA) but will
  # ensure to omit NA values when using them later.
  targetConceptId = NA_integer_,
  comparatorConceptId = NA_integer_,
  stringsAsFactors = FALSE
)

# Excluded covariates data.frame:
# The Analysis Specifications provided empty include/exclude lists. The
# Template often uses an excludedCovariateConcepts data.frame. We create an
# empty data.frame with the expected columns so downstream code can refer to it.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# Modules: CohortGenerator & CohortDiagnostics --------------------------------
# ------------------------------------------------------------------------------

# CohortGeneratorModule (shared resources + module specs)
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Negative control shared resource specification: we mark occurrenceType = "first"
# and detectOnDescendants = TRUE to capture descendant concepts of the provided
# negative-control concept set.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule settings (we enable many diagnostics following the Template)
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
# CohortMethod Module Settings -------------------------------------------------
# ------------------------------------------------------------------------------

# Study Periods
# The Analysis Specifications define one study period:
#   studyStartDate = "20200201"
#   studyEndDate   = "20200530"
# If the study were not restricted to specific dates, these would be empty strings.
studyPeriods <- tibble(
  studyStartDate = c("20200201"),
  studyEndDate   = c("20200530")
)

# Time-at-risk (TAR) settings
# The Analysis Specifications provide a single time-at-risk:
#   riskWindowStart = 1 (anchor: "cohort start")
#   riskWindowEnd   = 30 (anchor: "cohort start")
#   minDaysAtRisk   = 1
# We'll give it a human-readable label for identification in results.
timeAtRisks <- tibble(
  label = c("TAR_1_to_30_cohort_start"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(30),
  endAnchor = c("cohort start"),
  minDaysAtRisk = c(1)
)

# Propensity Score (PS) adjustment configurations
# The Analysis Specifications define two PS adjustment strategies:
#  1) stratify: numberOfStrata = 5, baseSelection = "all"
#  2) match:    maxRatio = 1, caliper = 0.2, caliperScale = "standardized logit"
#
# We create two small data.frames that will be converted into a combined psConfigList.

# Match-on-PS data.frame (one row for the match specification)
matchOnPsArgsList <- tibble(
  label = c("match_1_0.2_stdlogit"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # permitted: "propensity score" | "standardized" | "standardized logit"
)

# Stratify-by-PS data.frame (one row for the stratify specification)
stratifyByPsArgsList <- tibble(
  label = c("stratify_5_all"),
  numberOfStrata  = c(5),
  baseSelection = c("all") # permitted: "all" | "target" | "comparator"
)

# Build a unified PS configuration list (psConfigList) where each entry indicates
# whether the strategy is matching or stratification and includes the relevant parameters.
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
# Now build CohortMethod analyses combining study periods, TARs, and PS configs
# ------------------------------------------------------------------------------

cmAnalysisList <- list()
analysisId <- 1

# Iterate over the study periods
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  # Iterate over time-at-risk definitions
  for (t in seq_len(nrow(timeAtRisks))) {

    # Iterate over each PS strategy
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create either matchOnPsArgs or stratifyByPsArgs depending on the method
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
        stop("Unknown PS method: ", psCfg$method)
      }

      # Covariate settings:
      # The Analysis Specifications provided empty concept lists for inclusion/exclusion.
      # We'll use the default covariate set and exclude descendants of any (empty)
      # excluded set - effectively using FeatureExtraction defaults.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build the list of outcomes for CohortMethod:
      # - Include the outcome(s) of interest (oList)
      # - Append the negative controls (from negativeControlOutcomeCohortSet)
      #
      # For the primary outcomes we set outcomeOfInterest = TRUE and no trueEffectSize.
      # For negative controls we set outcomeOfInterest = FALSE and trueEffectSize = 1.
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

      # Build targetComparatorOutcomesList:
      # The Template iterates over cmTcList and calls createTargetComparatorOutcomes.
      # We follow the same pattern but carefully construct the excluded covariate IDs
      # by omitting NA values from target/concept columns and combining with the
      # excludedCovariateConcepts list (which is empty in our specs).
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        # collect potential excluded covariate concept ids, omitting NA:
        to_exclude <- na.omit(c(
          cmTcList$targetConceptId[i],
          cmTcList$comparatorConceptId[i],
          excludedCovariateConcepts$conceptId
        ))
        # Ensure vector is numeric (will be length 0 if no ids)
        excludedCovariateIds <- if (length(to_exclude) == 0) integer(0) else as.integer(to_exclude)

        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateIds
        )
      }

      # getDbCohortMethodDataArgs:
      # Map settings from Analysis Specifications -> CohortMethod::createGetDbCohortMethodDataArgs
      # Analysis Specs getDbCohortMethodDataArgs:
      #   studyPeriods = [studyStartDate=20200201, studyEndDate=20200530]
      #   maxCohortSize = 0
      #   restrictToCommonPeriod = false
      #   firstExposureOnly = true
      #   washoutPeriod = 0
      #   removeDuplicateSubjects = "remove all"
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings,
        firstExposureOnly = TRUE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "remove all"
      )

      # createPsArgs: build according to Analysis Specifications createPsArgs
      # Settings from Analysis Specifications:
      #   maxCohortSizeForFitting = 250000
      #   errorOnHighCorrelation = TRUE
      #   prior: priorType = "laplace", useCrossValidation = TRUE
      #   control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      #            noiseLevel = "silent", resetCoefficients = TRUE, startingVariance = 0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # allow Strategus to proceed if one PS fit fails
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
          seed = 1,
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Covariate balance computation arguments (shared and per-analysis)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      # For the full covariate balance (table1 style) use default table1 spec from FeatureExtraction
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs: Map Analysis Specifications fitOutcomeModelArgs
      #   modelType = "cox"
      #   stratified = true
      #   useCovariates = false
      #   inversePtWeighting = false
      #   prior: laplace, useCrossValidation = TRUE
      #   control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      #            noiseLevel = "quiet", resetCoefficients = TRUE, startingVariance = 0.01
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
          tolerance = 2e-7,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          seed = 1,
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # createStudyPopArgs:
      # Map Analysis Specifications createStudyPopArgs:
      #  restrictToCommonPeriod = false
      #  firstExposureOnly = false
      #  washoutPeriod = 0
      #  removeDuplicateSubjects = "keep all"
      #  censorAtNewRiskWindow = false
      #  removeSubjectsWithPriorOutcome = false
      #  priorOutcomeLookBack = 99999
      #  timeAtRisks: riskWindowStart = 1, startAnchor = "cohort start", riskWindowEnd = 30, endAnchor = "cohort start", minDaysAtRisk = 1
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = FALSE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999
      )

      # Construct a human-readable description for this analysis to be embedded in the
      # analysis JSON. This helps identify results later.
      description_text <- sprintf(
        "Study: %s-%s; TAR: %s; PS: %s",
        studyStartDate,
        studyEndDate,
        timeAtRisks$label[t],
        psCfg$label
      )

      # Append the CM analysis (createCmAnalysis aggregates all settings for a single
      # CohortMethod analysis).
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = description_text,
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

# Create the CohortMethod module specifications object for Strategus --------------------------------
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
# Assemble the full analysis specification and save to JSON --------------------
# ------------------------------------------------------------------------------

# Start with an empty Strategus analysisSpecifications object and add shared
# resources and module specifications in the order used in the Template.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to the inst/<studyName>/<studyName>AnalysisSpecification.json
# path, using the exact study name from the Analysis Specifications: "covid19famotidine".
outputFile <- file.path("inst", "covid19famotidine", "covid19famotidineAnalysisSpecification.json")
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  outputFile
)

# End of script ----------------------------------------------------------------
# Notes:
# - All key settings in the Analysis Specifications have been translated into the
#   corresponding Strategus / CohortMethod constructs. If you wish to change
#   any of the settings (e.g. add additional TARs, PS strategies, or study
#   periods) edit the corresponding blocks above.
# - When running Strategus with this specification, ensure the referenced cohort
#   definitions and concept sets are available on the specified WebAPI (baseUrl).
# - If you want to include specific covariates to include/exclude, populate
#   excludedCovariateConcepts and/or provide includedCovariateConcepts and then
#   modify covariateSettings accordingly (e.g. using createCovariateSettingsFrom...).
# ------------------------------------------------------------------------------