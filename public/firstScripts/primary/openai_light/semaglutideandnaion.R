library(dplyr)
library(Strategus)

################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script builds a Strategus analysis specification for the study:
#   semaglutideandnaion
#
# The settings used below are derived exactly from the provided
# <Analysis Specifications>. Variable names are kept exactly as in the
# <Template> and the <Analysis Specifications> to avoid any automatic renaming.
#
# Detailed comments are included throughout to explain how each setting
# from the specifications is applied to the Strategus / CohortMethod
# configuration objects.
################################################################################

# ------------------------------------------------------------------------------
# Shared Resources -------------------------------------------------------------
# ------------------------------------------------------------------------------
# Base Atlas WebAPI URL used to export cohort definitions and concept sets.
# Change this if you use a different Atlas/WebAPI instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ------------------------------------------------------------------------------
# Cohort Definitions
# ------------------------------------------------------------------------------
# Export the cohort definitions from the WebAPI using the exact cohort IDs
# provided in the Analysis Specifications:
#   target  : cohort id 1794126 (name: "target1")
#   comparator: cohort id 1794132 (name: "comparator1")
#   outcome : cohort id 1794131 (name: "outcome1")
#
# generateStats = TRUE will cause cohort statistics to be computed and stored
# within the exported cohortDefinitionSet object (useful for diagnostics).
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target (target1)
    1794132, # Comparator (comparator1)
    1794131  # Outcome (outcome1)
  ),
  generateStats = TRUE
)

# The exported cohortDefinitionSet will contain the original cohort IDs.
# For the analysis specification we re-number them to be small integers
# (1 = target, 2 = comparator, 3 = outcome). This renumbering is required
# by downstream modules that expect compact cohortId values.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# ------------------------------------------------------------------------------
# Negative control concept set (conceptSetId)
# ------------------------------------------------------------------------------
# The negative control concept set is referenced by its conceptSetId in the
# Analysis Specifications: 1888110 (name: "negative")
# Here we resolve the concept set into individual concepts, then convert them
# into a "cohort-like" table that will be included as negative control
# outcome cohorts in the analysis. We assign cohortIds starting at 101.
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
  mutate(cohortId = row_number() + 100) %>% # negative control cohort ids: 101, 102, ...
  select(cohortId, cohortName, outcomeConceptId)

# Sanity check: ensure no duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# ------------------------------------------------------------------------------
# Build cohorts lists used in CohortMethod module
# ------------------------------------------------------------------------------
# Outcomes list (oList)
# We select the re-numbered outcome cohort from cohortDefinitionSet (cohortId == 3)
# and attach the cleanWindow specified in the template/specifications (365 days).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # as in the Template example

# Target and Comparator list for CohortMethod (cmTcList)
# Use the renumbered cohortIds 1 (target) and 2 (comparator) and preserve the
# exact cohort names from the analysis specifications.
# We include targetConceptId and comparatorConceptId columns (as NA)
# to match usage in the Template for excludedCovariateConceptIds building.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  targetConceptId = NA_integer_,     # no explicit target concept id provided in spec
  comparatorConceptId = NA_integer_  # no explicit comparator concept id provided in spec
)

# Excluded covariates for LSPS / PS: The Analysis Specifications did not
# provide specific concepts to exclude, so we create an empty data.frame.
# This mirrors the Template usage and avoids accidental exclusion of concepts.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# If you wanted to include a limited set of covariates only, you could define
# includedCovariateConcepts here. The specifications contained empty entries,
# so we do not populate it.
# includedCovariateConcepts <- data.frame(conceptId = c(), conceptName = c())


# ------------------------------------------------------------------------------
# CohortGeneratorModule Specifications -----------------------------------------
# ------------------------------------------------------------------------------
# Create shared resources and module specifications for cohort generation.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource for the exported cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Shared resource for negative control outcome cohorts created from the
# concept set. occurrenceType = "first" and detectOnDescendants = TRUE are
# common choices for negative controls; they follow the Template pattern.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specification: run the cohort generation and optionally compute stats
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ------------------------------------------------------------------------------
# CohortDiagnosticsModule Specifications --------------------------------------
# ------------------------------------------------------------------------------
# Request a range of cohort diagnostics for the created cohorts. The settings
# below mirror the Template and enable broad diagnostics useful for study QC.
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
# CohortMethodModule Specifications -------------------------------------------
# ------------------------------------------------------------------------------
# Study periods: from Analysis Specifications -> getDbCohortMethodDataArgs.studyPeriods
# The specification provided a single study period:
#   studyStartDate: 20171201
#   studyEndDate  : 20231231
# Use character strings in YYYYMMDD format.
studyPeriods <- tibble::tibble(
  studyStartDate = c("20171201"),
  studyEndDate   = c("20231231")
)

# Time-at-risks: from Analysis Specifications -> createStudyPopArgs.time_at_risks
# The specification includes a single time-at-risk:
#   riskWindowStart = 1 (start anchor = "cohort start")
#   riskWindowEnd   = 0 (end anchor = "cohort end")
#   minDaysAtRisk   = 1
# We create a single TAR row labeled "TAR_1" (label value is for human-readability)
timeAtRisks <- tibble::tibble(
  label = c("TAR_1"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"), # allowed: "cohort start" | "cohort end"
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")      # allowed: "cohort start" | "cohort end"
)

# Propensity Score (PS) configuration
# The Analysis Specifications provide a single PS setting: match on PS with:
#   maxRatio = 1
#   caliper = 0.2
#   caliperScale = "standardized logit"
matchOnPsArgsList <- tibble::tibble(
  label = c("match_1to1_caliper0.2"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # options: "propensity score" | "standardized" | "standardized logit"
)

# No stratify-by-PS configurations were provided in the specifications:
stratifyByPsArgsList <- tibble::tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c()
)

# Build a single PS configuration list (psConfigList) used later to produce
# CohortMethod analysis entries. Each list element contains:
#   - method: "match" or "stratify"
#   - label: human readable label
#   - params: a list of parameters relevant to the method
psConfigList <- list()

# Convert matchOnPsArgsList rows to psConfigList entries
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

# Convert stratifyByPsArgsList rows (none in this spec) to psConfigList entries
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
# Build CohortMethod analysis configurations (cmAnalysisList)
# ------------------------------------------------------------------------------
cmAnalysisList <- list()
analysisId <- 1

# Loop over study periods, time-at-risks, and PS configurations to create
# one or more CohortMethod analyses that cover the specified analysis matrix.
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Build matchOnPsArgs or stratifyByPsArgs based on the psCfg$method
      if (psCfg$method == "match") {
        # createMatchOnPsArgs parameters:
        #   maxRatio, caliper, caliperScale
        # We also set allowReverseMatch = FALSE and no stratification columns.
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

      # Covariate settings:
      # The Template used FeatureExtraction::createDefaultCovariateSettings with
      # addDescendantsToExclude = TRUE (used for excluded covariates later).
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build the outcome list for this analysis: include the main outcomes and
      # append the negative controls (treated as outcomes with trueEffectSize=1)
      outcomeList <- append(
        # Main outcomes from oList (these are outcomes of interest)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        # Negative control outcomes from negativeControlOutcomeCohortSet
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Build targetComparatorOutcomesList: one entry per target-comparator pair
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        # The Template included excluded covariate concept ids composed of:
        #   cmTcList$targetConceptId[i],
        #   cmTcList$comparatorConceptId[i],
        #   excludedCovariateConcepts$conceptId
        # Here those values are NA or empty, which is acceptable. NA values
        # will be included but typically ignored when empty.
        excludedIds <- c(
          cmTcList$targetConceptId[i],
          cmTcList$comparatorConceptId[i],
          excludedCovariateConcepts$conceptId
        )
        # Remove NA values from the excludedIds vector
        excludedIds <- excludedIds[!is.na(excludedIds)]

        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedIds
        )
      }

      # getDbCohortMethodDataArgs:
      # The Analysis Specifications specify restrictToCommonPeriod = TRUE for
      # getDbCohortMethodDataArgs.studyPeriods. The template uses restrictToCommonPeriod = TRUE.
      # Use the covariateSettings created above and maxCohortSize = 0 (no limit).
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs:
      # From Analysis Specifications:
      #   maxCohortSizeForFitting = 250000
      #   errorOnHighCorrelation = TRUE
      #   prior: laplace, useCrossValidation = TRUE
      #   control: tolerance=2e-7, cvType="auto", fold=10, cvRepetitions=10,
      #            noiseLevel="silent", resetCoefficients=TRUE, startingVariance=0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # allow Strategus to continue other operations if PS fails for one pair
        estimator = "att",   # Template default (ATT estimator)
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

      # Covariate balance computation arguments
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs:
      # From Analysis Specifications:
      #   modelType = "cox"
      #   stratified = FALSE
      #   useCovariates = FALSE
      #   inversePtWeighting = FALSE
      #   prior: laplace, useCrossValidation = TRUE
      #   control: tolerance=2e-7, cvType="auto", fold=10, cvRepetitions=10,
      #            noiseLevel="quiet", resetCoefficients=TRUE, startingVariance=0.01
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
          tolerance = 2e-7,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # createStudyPopArgs:
      # Use the Study Population settings from the Analysis Specifications:
      #   restrictToCommonPeriod = FALSE
      #   firstExposureOnly = FALSE
      #   washoutPeriod = 365
      #   removeDuplicateSubjects = "keep all"
      #   censorAtNewRiskWindow = FALSE
      #   removeSubjectsWithPriorOutcome = TRUE
      #   priorOutcomeLookBack = 99999
      #   time-at-risk: use the row from timeAtRisks[t, ]
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
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Append the CohortMethod analysis configuration to the cmAnalysisList
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

# Build the CohortMethod module specifications:
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
# Combine all module specifications into a single analysisSpecifications object
# ------------------------------------------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# ------------------------------------------------------------------------------
# Save analysis specifications to JSON in inst/<studyName>/...
# File name and folder follow the Template pattern; the study name is taken
# from the Analysis Specifications: "semaglutideandnaion".
# ------------------------------------------------------------------------------
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "semaglutideandnaion", "semaglutideandnaionAnalysisSpecification.json")
)

# End of CreateStrategusAnalysisSpecification.R
# ------------------------------------------------------------------------------
# Notes:
# - All parameter values are applied exactly as specified in the provided
#   <Analysis Specifications> JSON.
# - If you modify cohort IDs, concept set IDs, or add more PS/time-at-risk
#   combinations, ensure the corresponding lists/tibbles above are updated.
# - The script intentionally leaves some lists (e.g. excludedCovariateConcepts)
#   empty because the Analysis Specifications did not provide entries; users
#   may populate them as needed.
# ------------------------------------------------------------------------------