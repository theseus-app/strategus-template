library(dplyr)
library(Strategus)

################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script creates a Strategus analysis specification for the study named
# "uveitissafety" using the OHDSI Strategus package. The script follows the
# provided template and applies the exact settings specified in the
# <Analysis Specifications> JSON.
#
# IMPORTANT: variable names and cohort ids are used exactly as provided in the
# specifications. Do not rename cohorts or change ids.
################################################################################

# -----------------------
# Shared Resources
# -----------------------

# Base WebAPI URL to download cohort and concept-set definitions.
# NOTE: Replace this with your Atlas/WebAPI endpoint if needed.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Export cohort definitions from Atlas/WebAPI.
# We request the three cohorts specified in the analysis specification:
#   - Target cohort id: 1794126 (name: target1)
#   - Comparator cohort id: 1794132 (name: comparator1)
#   - Outcome cohort id: 1794131 (name: outcome1)
# generateStats = TRUE will request cohort statistics from the WebAPI
# (may speed certain downstream operations or ensure cohort compatibility).
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts so that target = 1, comparator = 2, outcome = 3.
# This renumbering is a convention used later in building the CM analyses.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, "cohortId"] <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, "cohortId"] <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, "cohortId"] <- 3

# Negative control outcomes: import concept set definition and resolve it to a
# set of concepts. The specification provides a concept set id 1888110 named
# "negative". We will map these to artificial cohort ids starting at 101.
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
  rename(
    outcomeConceptId = "conceptId",
    cohortName = "conceptName"
  ) %>%
  # Assign cohort ids for negative controls starting at 101 to avoid clash
  # with target/comparator/outcome cohort ids (1,2,3,...).
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no duplicate cohort ids between primary cohorts and
# negative control cohorts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between primary cohorts and negative controls ***")
}

# -----------------------
# Build lists used in analyses
# -----------------------

# Outcomes (primary outcomes for the CohortMethod analyses)
# We select the cohortDefinitionSet row with cohortId == 3 (outcome).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(
    outcomeCohortId = cohortId,
    outcomeCohortName = cohortName
  ) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # cleanWindow is used by some diagnostics; set to a default 365 days.
  mutate(cleanWindow = 365)

# Target and Comparator for CohortMethod analyses.
# Use the exact names provided in the specification.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  stringsAsFactors = FALSE
)

# Excluded covariate concepts: The analysis specification did not provide any
# specific concepts to exclude from covariates (empty). Create an empty data
# frame for compatibility with the template usage.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# Optional included covariate concepts (none specified in the analysis spec).
# includedCovariateConcepts <- data.frame(conceptId = integer(0), conceptName = character(0))

# -----------------------
# CohortGeneratorModule specifications
# -----------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource for cohort definitions (used by multiple modules)
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Shared resource for negative control outcome cohorts
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specification: generate cohorts and statistics
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# -----------------------
# CohortDiagnosticsModule specifications
# -----------------------
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
  # Minimum mean to include features in characterization outputs
  minCharacterizationMean = 0.01
)

# -----------------------
# CohortMethod module: build analyses
# -----------------------

# Study periods:
# The analysis specification provides one study period with empty start/end
# strings. When empty strings are used, the CohortMethod code will not restrict
# by calendar time. We keep the same shape used in the template.
studyPeriods <- tibble::tibble(
  studyStartDate = c(""), # empty string means 'no restriction'
  studyEndDate   = c("")  # empty string means 'no restriction'
)

# Time-at-risks (TARs) as specified in createStudyPopArgs of the specification.
# Two TARs are provided:
# 1) riskWindowStart = 1, startAnchor = "cohort start", riskWindowEnd = 0,
#    endAnchor = "cohort end", minDaysAtRisk = 1  -> typically "on treatment"
# 2) riskWindowStart = 1, startAnchor = "cohort start", riskWindowEnd = 99999,
#    endAnchor = "cohort start", minDaysAtRisk = 1 -> typically "intent-to-treat"
timeAtRisks <- tibble::tibble(
  label = c("onTreatment", "intentToTreat"),
  riskWindowStart = c(1, 1),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd = c(0, 99999),
  endAnchor = c("cohort end", "cohort start"),
  minDaysAtRisk = c(1, 1)
)

# Propensity score (PS) settings from the specification:
# Two match-on-PS settings are specified.
matchOnPsArgsList <- tibble::tibble(
  label = c("match_ratio_10", "match_ratio_1"),
  maxRatio = c(10, 1),
  caliper = c(0.2, 0.2),
  caliperScale = c("standardized logit", "standardized logit"),
  stringsAsFactors = FALSE
)

# No stratify-by-PS settings provided in this specification.
stratifyByPsArgsList <- tibble::tibble(
  label = character(0),
  numberOfStrata = integer(0),
  baseSelection = character(0),
  stringsAsFactors = FALSE
)

# Build a single PS configuration list (psConfigList) used to create multiple
# CohortMethod analyses. Each list element contains:
# - method: "match" or "stratify"
# - label: human-readable label
# - params: list of method parameters
psConfigList <- list()

# Convert match-on-PS rows to psConfigList entries
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

# Convert stratify-by-PS rows (if any) to psConfigList entries
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

# -----------------------
# Build cohort method analysis list
# -----------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create PS adjustment specific args (match or stratify)
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
      # The analysis specification did not provide explicit covariate inclusion
      # or exclusion lists (empty). Use default covariates with descendant
      # exclusions as in template.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build outcome list for this analysis:
      # - include the primary outcomes (oList)
      # - append the negative control outcomes (negativeControlOutcomeCohortSet)
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          # createOutcome: outcomeOfInterest = TRUE for the primary outcome
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          # negative controls: outcomeOfInterest = FALSE and trueEffectSize = 1
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Build targetComparatorOutcomesList. We iterate over cmTcList and create
      # a TargetComparatorOutcomes entry per row. The excluded covariates list
      # is empty in the specification (excludedCovariateConcepts is empty).
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # excludedCovariateConceptIds: here only the global excluded covariates
          # (none specified in this analysis spec).
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Create getDbCohortMethodDataArgs based on the specification's
      # getDbCohortMethodDataArgs. The specification sets:
      #   studyPeriods: (empty strings) -> we pass the local studyStartDate/studyEndDate
      #   maxCohortSize = 0
      #   restrictToCommonPeriod = true
      #   firstExposureOnly = false
      #   washoutPeriod = 0
      #   removeDuplicateSubjects = "keep all"
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all"
      )

      # Create PS fitting args based on specification -> createPsArgs
      # The specification's createPsArgs settings are applied here:
      #   maxCohortSizeForFitting = 250000
      #   errorOnHighCorrelation = true
      #   prior: laplace with useCrossValidation = true
      #   control: tolerance 2e-7, cvType "auto", fold = 10, cvRepetitions = 10,
      #            noiseLevel = "silent", resetCoefficients = true,
      #            startingVariance = 0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        # stopOnError kept FALSE to allow Strategus to attempt to continue on
        # individual CM failures (template convention). Adjust if you want hard stop.
        stopOnError = FALSE,
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
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          seed = 1
        )
      )

      # Covariate balance computation args
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit outcome model args as per specification:
      # modelType = "cox", stratified = TRUE, useCovariates = FALSE,
      # inversePtWeighting = FALSE, prior = laplace (CV), control with
      # tolerance 2e-7, cvType "auto", fold = 10, cvRepetitions = 10,
      # noiseLevel = "quiet", resetCoefficients = TRUE, startingVariance = 0.01
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
          tolerance = 2e-7,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          seed = 1
        )
      )

      # Create study population args from 'createStudyPopArgs' in specification.
      # Specification values:
      #   restrictToCommonPeriod = FALSE
      #   firstExposureOnly = FALSE
      #   washoutPeriod = 0
      #   removeDuplicateSubjects = "keep all"
      #   censorAtNewRiskWindow = false
      #   removeSubjectsWithPriorOutcome = true
      #   priorOutcomeLookBack = 99999
      # Time-at-risk values are driven by the timeAtRisks tibble current row.
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

      # Append the CohortMethod analysis settings to the analysis list
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
    } # end PS configs loop
  } # end TARs loop
} # end studyPeriods loop

# Create the CohortMethod module specifications using the built cmAnalysisList
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# -----------------------
# Assemble analysis specifications and save to JSON
# -----------------------

analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() %>%
  Strategus::addSharedResources(cohortDefinitionShared) %>%
  Strategus::addSharedResources(negativeControlsShared) %>%
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specification JSON to inst/uveitissafety/uveitissafetyAnalysisSpecification.json
# NOTE: This path follows the template convention: inst/<studyName>/<studyName>AnalysisSpecification.json
outputDir <- file.path("inst", "uveitissafety")
if (!dir.exists(outputDir)) {
  dir.create(outputDir, recursive = TRUE)
}

ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path(outputDir, "uveitissafetyAnalysisSpecification.json")
)

# End of script.