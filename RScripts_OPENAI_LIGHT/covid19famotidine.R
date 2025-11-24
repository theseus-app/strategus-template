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
# This script creates Strategus analysis specifications for the study defined
# in the provided Analysis Specifications. It follows the Template structure and
# applies the exact names and settings from the Analysis Specifications JSON.
#
# NOTE: The script contains detailed inline annotations to explain how each
# setting from the Analysis Specifications is applied.
################################################################################

# ------------------------------------------------------------------------------
# Shared Resources -------------------------------------------------------------
# ------------------------------------------------------------------------------

# Base WebAPI URL used to export cohort and concept-set definitions.
# (Template used atlas-demo.ohdsi.org; keeping same here unless you need to
# change for your environment.)
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ------------------------------------------------------------------------------
# Cohort Definitions (export from WebAPI)
# ------------------------------------------------------------------------------

# Analysis Specifications specify the following cohort IDs and names:
# - targetCohort: id = 1794126, name = "target1"
# - comparatorCohort: id = 1794132, name = "comparator1"
# - outcomeCohort: id = 1794131, name = "outcome1"
#
# We export all three cohort definitions from the WebAPI.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts so that they start 1, 2, 3 to match the template structure.
# This remapping will be used throughout the analysis specification.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# ------------------------------------------------------------------------------
# Negative control outcomes (concept set)
# ------------------------------------------------------------------------------

# Analysis Specifications:
# negativeControlConceptSet: id = 1888110, name = "negative"
#
# We load the concept set definition and resolve it to obtain all member concepts.
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
  # Align column names with what Strategus/CohortMethod expects:
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  # Assign cohortId values for negative controls starting at 101:
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no duplicate cohort IDs between primary cohorts and
# negative control cohorts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# ------------------------------------------------------------------------------
# Create small helper tables used to assemble analyses
# ------------------------------------------------------------------------------

# Outcomes list: select the outcome cohort (renumbered to cohortId == 3)
# and set a cleanWindow (used for prior outcome exclusion/clean window).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # The template uses cleanWindow = 365 for the outcome definition table
  mutate(cleanWindow = 365)

# Target and Comparator table (for CohortMethod)
# Use the exact names from Analysis Specifications: "target1" and "comparator1".
# The template expects targetConceptId / comparatorConceptId columns when
# excluding the drugs of interest; those are not provided in the
# Analysis Specifications, so set NA (they will not be used because we will
# not exclude any specific drug concept ids here).
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  # These columns exist in the template; keep them but set to NA_integer_
  # since Analysis Specifications did not provide specific concept IDs.
  targetConceptId = NA_integer_,
  comparatorConceptId = NA_integer_
)

# Excluded covariate concepts: the Analysis Specifications' covariateSelection
# has empty include/exclude lists. We therefore create an empty data frame here.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# ------------------------------------------------------------------------------
# CohortGeneratorModule specifications (shared resources and module settings)
# ------------------------------------------------------------------------------

cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create a shared resource for the cohort definitions we exported
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

# Create a shared resource for the negative control cohort set
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications for CohortGenerator (keep generateStats = TRUE
# as used in the Template).
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ------------------------------------------------------------------------------
# CohortDiagnosticsModule specifications
# ------------------------------------------------------------------------------

cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# The Template sets many cohort diagnostics options to TRUE; follow the same
# here so that diagnostics will be generated for all cohorts.
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
# CohortMethodModule -----------------------------------------------------------
# Build PS configurations and CohortMethod analyses using Analysis Specifications
# ------------------------------------------------------------------------------

# 1) Study periods
# Analysis Specifications: getDbCohortMethodDataArgs.studyPeriods contains one
# entry with studyStartDate = "20200201" and studyEndDate = "20200530".
studyPeriods <- tibble::tibble(
  studyStartDate = c("20200201"), # YYYYMMDD
  studyEndDate   = c("20200530")  # YYYYMMDD
)

# 2) Time-at-risks (TARs)
# Analysis Specifications createStudyPopArgs.timeAtRisks contains a single TAR:
# riskWindowStart = 1 (anchor: "cohort start"), riskWindowEnd = 30 (anchor: "cohort start"),
# minDaysAtRisk = 1.
timeAtRisks <- tibble::tibble(
  label = c("1-30_cohort_start"), # human-readable label for the TAR
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(30),
  endAnchor = c("cohort start")
)

# 3) Propensity Score (PS) configurations
# Analysis Specifications list two PS settings:
# - stratifyByPsArgs: numberOfStrata = 5, baseSelection = "all"
# - matchOnPsArgs: maxRatio = 1, caliper = 0.2, caliperScale = "standardized logit"
#
# We'll convert these into a single psConfigList used to generate analyses.

psConfigList <- list()

# Add stratification PS config (first PS setting in JSON)
psConfigList[[length(psConfigList) + 1]] <- list(
  method = "stratify",
  label = sprintf("stratify_%s", 5),
  params = list(
    numberOfStrata = 5,
    baseSelection = "all"
  )
)

# Add match-on-PS config (second PS setting in JSON)
psConfigList[[length(psConfigList) + 1]] <- list(
  method = "match",
  label = sprintf("match_ratio_%s_caliper_%s", 1, 0.2),
  params = list(
    maxRatio = 1,
    caliper = 0.2,
    caliperScale = "standardized logit"
  )
)

# ------------------------------------------------------------------------------
# Build CohortMethod analysis list
# ------------------------------------------------------------------------------

cmAnalysisList <- list()
analysisId <- 1

# Loop over study periods
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  # Loop over time-at-risk definitions
  for (t in seq_len(nrow(timeAtRisks))) {

    # Loop over PS configurations
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create matchOnPsArgs or stratifyByPsArgs according to psCfg$method
      if (psCfg$method == "match") {
        # Create matching arguments using CohortMethod::createMatchOnPsArgs
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
        stop("Unknown PS configuration method: ", psCfg$method)
      }

      # Covariate settings: Analysis Specifications' covariateSelection is empty,
      # so we use the default covariate settings (as in the Template).
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build the list of outcomes to include in the CohortMethod analysis.
      # This includes the outcome(s) of interest (from oList) and the negative
      # control outcomes (from negativeControlOutcomeCohortSet).
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

      # Build target-comparator-outcome bundle(s).
      # We only have one target-comparator pair in cmTcList.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        # The template combines excludedCovariateConceptIds from the target/comparator
        # and any additional excluded covariate concepts. Our Analysis Specifications
        # do not provide target/comparator drug concept IDs so those entries are NA.
        # The excludedCovariateConcepts data.frame is empty, so the resulting vector
        # will effectively be empty.
        excludedIds <- c(
          # include the target/comparator concept ids if provided (may be NA)
          cmTcList$targetConceptId[i],
          cmTcList$comparatorConceptId[i],
          excludedCovariateConcepts$conceptId
        )
        # Remove NA values (so excludedIds becomes integer(0) if none provided)
        excludedIds <- excludedIds[!is.na(excludedIds)]

        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedIds
        )
      }

      # getDbCohortMethodDataArgs:
      # Analysis Specifications' getDbCohortMethodDataArgs has studyPeriods and maxCohortSize = 0.
      # We set restrictToCommonPeriod = TRUE (following the Template), pass the studyStart/end
      # for the current study period, and include the covariateSettings prepared above.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs:
      # Analysis Specifications (propensityScoreAdjustment.createPsArgs) specify:
      # - maxCohortSizeForFitting = 250000
      # - errorOnHighCorrelation = true
      # - prior: priorType = "laplace", useCrossValidation = true
      # - control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      #            noiseLevel = "silent", resetCoefficients = true, startingVariance = 0.01
      #
      # Map these to Cyclops prior/control objects and createCreatePsArgs.
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # allow Strategus to continue in case one PS fit fails
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

      # computeSharedCovariateBalanceArgs and computeCovariateBalanceArgs
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs:
      # Analysis Specifications specify a Cox model with regularization prior
      # (laplace) and specific Cyclops control parameters.
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
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          tolerance = 2e-07
        )
      )

      # createStudyPopArgs:
      # Map values from Analysis Specifications' createStudyPopArgs:
      # - restrictToCommonPeriod: false
      # - firstExposureOnly: false
      # - washoutPeriod: 0
      # - removeDuplicateSubjects: "keep all"
      # - censorAtNewRiskWindow: false
      # - removeSubjectsWithPriorOutcome: true
      # - priorOutcomeLookBack: 30
      # - timeAtRisks: the single TAR defined above (1 to 30 days, anchored to cohort start)
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
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Append the CohortMethod analysis specification entry to the list.
      # The description provides a short explanation including study dates, TAR
      # label, and PS configuration label.
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
    } # end PS loop
  } # end TAR loop
} # end study periods loop

# Create the CohortMethod module specifications using the assembled analysis list
# and the target-comparator-outcomes bundle. We do not request refitting the PS
# for every outcome/study population (as per typical template defaults).
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
# Assemble final analysisSpecifications object and save to JSON
# ------------------------------------------------------------------------------

analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specification to inst/<studyName>/<studyName>AnalysisSpecification.json
# Use the exact study name from the Analysis Specifications: "covid19famotidine"
outputFile <- file.path("inst", "covid19famotidine", "covid19famotidineAnalysisSpecification.json")
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  outputFile
)

# Print a short message (useful when running interactively)
message("Analysis specification saved to: ", outputFile)