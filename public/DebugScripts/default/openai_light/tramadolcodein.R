################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script creates Strategus analysis specifications for the study:
#   tramadolcodein
#
# The configuration below follows the Analysis Specifications provided in
# the JSON fragment named "Analysis Specifications".  The script builds:
#  - cohort definition shared resources (target, comparator, outcome)
#  - negative control cohort shared resources
#  - CohortGenerator module specifications
#  - CohortDiagnostics module specifications
#  - CohortMethod module specifications (two Time-at-Risk configurations,
#    propensity-score matching as specified, PS fitting and outcome model settings)
#
# The resulting Strategus analysisSpecifications object is saved as:
#   inst/tramadolcodein/tramadolcodeinAnalysisSpecification.json
#
# NOTE: This script preserves the exact cohort IDs and names provided in the
# Analysis Specifications (no auto-correct or renaming beyond the internal
# renumbering used by Strategus/CohortMethod examples).
################################################################################

library(dplyr)
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(ParallelLogger)

# ------------------------------------------------------------------------------
# Shared Resources: WebAPI base URL and Cohort Definitions
# ------------------------------------------------------------------------------
# Use a public Atlas/WebAPI as an example. In practice change baseUrl to your
# WebAPI instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Export cohort definitions from Atlas/WebAPI.
# We use the exact cohort IDs provided in the Analysis Specifications:
#   - Target cohort:      id = 1794126, name = "target1"
#   - Comparator cohort:  id = 1794132, name = "comparator1"
#   - Outcome cohort(s):  id = 1794131, name = "outcome1"
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# The CohortMethod/CohortGenerator examples in Strategus often expect the core
# study cohorts to be numbered starting at 1, 2, 3 ... for target, comparator,
# outcome. We re-number the exported cohortDefinitionSet to map:
#   original 1794126 -> 1 (target)
#   original 1794132 -> 2 (comparator)
#   original 1794131 -> 3 (outcome)
#
# This renumbering is only for internal reference inside the specifications;
# the original cohort definitions are kept intact in the shared resource.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# ------------------------------------------------------------------------------
# Negative control outcomes (concept set)
# ------------------------------------------------------------------------------
# Use the exact negative control concept set id from the Analysis Specifications:
#   negativeControlConceptSet id = 1888110, name = "negative"
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
  # Assign unique cohort IDs for these negative controls that do not conflict
  # with our target/comparator/outcome (1,2,3). Start at 101 (100 + row_number).
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between study cohorts and negative controls ***")
}

# ------------------------------------------------------------------------------
# Create small helper data frames used by the CohortMethod module
# ------------------------------------------------------------------------------
# Outcomes: use the renumbered outcome cohort (3) and set a clean window as per
# the template/examples. The Analysis Specifications specified one outcome:
#   - id = 1794131 (renumbered to 3), name = "outcome1"
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId,
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # prior outcome clean window set to 365 days (commonly used; adjust if needed)
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Use the renumbered ids (1 and 2) and exact names from the Analysis Specifications.
# Add columns targetConceptId/comparatorConceptId filled with NA to match the
# template's expected structure when building excluded covariates.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  # these are placeholders (no explicit target/comparator covariate exclusion ids provided)
  targetConceptId = as.integer(NA),
  comparatorConceptId = as.integer(NA)
)

# Excluded covariate concepts: Analysis Specifications did not list any specific
# covariates to exclude. We create an empty data.frame to pass through the template.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# Optional: included covariates (none specified in the Analysis Specifications)
# includedCovariateConcepts <- data.frame(conceptId = integer(0), conceptName = character(0))

# ------------------------------------------------------------------------------
# CohortGeneratorModule: create shared resource & module specifications
# ------------------------------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# The shared resource points to the cohort definitions we exported above
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Create shared resource for negative control outcome cohort set
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specification: generate stats for CohortGenerator (mirrors template)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ------------------------------------------------------------------------------
# CohortDiagnosticsModule: module settings
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
  # minimum mean for temporal characterization (example from template)
  minCharacterizationMean = 0.01
)

# ------------------------------------------------------------------------------
# CohortMethodModule: PS, study population, and outcome model settings
# ------------------------------------------------------------------------------
# Study periods: The Analysis Specifications provided an empty start/end (i.e.
# not restricting to a calendar window). We'll keep empty strings to indicate
# "no restriction". If you want to restrict the study to a calendar window,
# replace the empty strings with "YYYYMMDD" formatted dates.
studyPeriods <- tibble(
  studyStartDate = c(""), # empty means no restriction
  studyEndDate   = c("")
)

# Time-at-risks (TARs) as specified in createStudyPopArgs
# Analysis Specifications defined two TARs:
# 1) riskWindowStart = 1, startAnchor = "cohort start", riskWindowEnd = 0, endAnchor = "cohort end"
#    minDaysAtRisk = 1
# 2) riskWindowStart = 1, startAnchor = "cohort start", riskWindowEnd = 99999, endAnchor = "cohort start"
#    minDaysAtRisk = 1
timeAtRisks <- tibble(
  label = c("TAR_cohortStart_to_cohortEnd", "TAR_cohortStart_infinite"),
  riskWindowStart  = c(1, 1),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd  = c(0, 99999),
  endAnchor = c("cohort end", "cohort start"),
  minDaysAtRisk = c(1, 1)
)

# Propensity Score settings - match on PS as specified
# Analysis Specifications specified one PS setting: match with maxRatio=1,
# caliper=0.2, caliperScale="standardized logit"
matchOnPsArgsList <- tibble(
  label = c("match_1to1_caliper0.2"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # options: "propensity score" | "standardized" | "standardized logit"
)

# No stratify-by-PS settings were specified in the Analysis Specifications:
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata = integer(0),
  baseSelection = character(0)
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Convert matchOnPsArgsList rows into psConfigList entries
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

# Convert stratifyByPsArgsList rows into psConfigList entries (none in this spec)
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

# Iterate through study periods, time-at-risks and PS configs to create CM analyses
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  rawStudyStartDate <- studyPeriods$studyStartDate[s]
  rawStudyEndDate <- studyPeriods$studyEndDate[s]

  # Convert empty-string markers to NULL for createGetDbCohortMethodDataArgs
  studyStartDate <- if (is.null(rawStudyStartDate) || rawStudyStartDate == "") NULL else rawStudyStartDate
  studyEndDate   <- if (is.null(rawStudyEndDate) || rawStudyEndDate == "") NULL else rawStudyEndDate

  for (t in seq_len(nrow(timeAtRisks))) {
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Build match or stratify args according to the configuration
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
        stop("Unknown PS method in psConfigList")
      }

      # Covariate settings: by default include all covariates but allow configuration
      # to exclude descendants of specific concepts if needed.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build the full outcome list: include study outcomes (outcome of interest)
      # and the negative control outcomes (outcomeOfInterest = FALSE)
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

      # Build target-comparator-outcome linkage list. This specifies which target
      # and comparator are paired and which outcomes apply. We also pass excluded
      # covariate concept ids (none in this Analysis Specifications).
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Combine NA target/comparator concept placeholders and any global excluded covariates
          excludedCovariateConceptIds = c(
            cmTcList$targetConceptId[i],
            cmTcList$comparatorConceptId[i],
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # getDbCohortMethodDataArgs: settings for extracting data from the CDM
      # The Analysis Specifications defined:
      #   studyPeriods: empty (no restriction)
      #   maxCohortSize = 0
      #   restrictToCommonPeriod = false
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs: create propensity score model fitting arguments
      # The Analysis Specifications provided detailed Cyclops regularization settings:
      #   maxCohortSizeForFitting = 250000
      #   errorOnHighCorrelation = true
      #   prior: priorType="laplace", useCrossValidation = true
      #   control: tolerance=2e-7, cvType="auto", fold=10, cvRepetitions=10,
      #            noiseLevel="silent", resetCoefficients=TRUE, startingVariance=0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # allow Strategus to continue if a PS model fails to fit for a specific analysis
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",
          exclude = c(0), # typically exclude intercept
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

      # Covariate balance settings for shared and analysis-specific computations
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs: settings for outcome model fitting
      # Analysis Specifications require:
      #   modelType = "cox"
      #   stratified = false
      #   useCovariates = false
      #   inversePtWeighting = false
      #   prior = laplace with cross-validation
      #   control options similar to createPsArgs but with noiseLevel="quiet"
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
          cvType = "auto",
          seed = 1,
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          tolerance = 2e-07
        )
      )

      # createStudyPopArgs: settings controlling the study population and TARs.
      # These follow the Analysis Specifications exactly:
      #   restrictToCommonPeriod = false
      #   firstExposureOnly = false
      #   washoutPeriod = 0
      #   removeDuplicateSubjects = "keep all"
      #   censorAtNewRiskWindow = false
      #   removeSubjectsWithPriorOutcome = false
      #   priorOutcomeLookBack = 99999
      #   riskWindowStart/End & anchors taken from timeAtRisks[t,]
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

      # Append the settings to the cmAnalysisList for this combination
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "tramadolcodein: studyPeriod [%s,%s]; TAR: %s; PS: %s",
          ifelse(is.null(studyStartDate), "noRestriction", rawStudyStartDate),
          ifelse(is.null(studyEndDate), "noRestriction", rawStudyEndDate),
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

# Create the CohortMethod module specification
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  # The Analysis Specifications did not require refitting PS per outcome or
  # per study population, so leave both FALSE.
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# ------------------------------------------------------------------------------
# Assemble full analysis specifications object and save to JSON
# ------------------------------------------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save to inst/<studyName>/<studyName>AnalysisSpecification.json
outputDir <- file.path("inst", "tramadolcodein")
if (!dir.exists(outputDir)) dir.create(outputDir, recursive = TRUE)

ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path(outputDir, "tramadolcodeinAnalysisSpecification.json")
)

# End of CreateStrategusAnalysisSpecification.R
# ------------------------------------------------------------------------------
# Notes / How settings map to the Analysis Specifications:
# - Cohort IDs and names were taken verbatim from the provided specifications.
# - Two TARs are created matching the createStudyPopArgs time-at-risk entries.
# - One PS configuration (1:1 match, caliper 0.2 on standardized logit) was created.
# - PS fitting and outcome model use Cyclops regularization with laplace prior
#   and cross-validation, with control parameters matching the specifications.
# - removeDuplicateSubjects is set to "keep all" where requested; adjust if you
#   need "keep first" or similar behavior.
# - studyPeriods left empty to indicate no calendar restriction; replace "" with
#   YYYYMMDD if you want to restrict to an interval.
# ------------------------------------------------------------------------------