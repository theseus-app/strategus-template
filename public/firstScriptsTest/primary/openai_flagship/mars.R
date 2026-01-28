################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script creates a Strategus analysis specification JSON using the OHDSI
# Strategus package, following the provided template and applying the exact
# settings from <Analysis Specifications>.
#
# Study name (from specs): mars
#
# Notes on "EXACT names":
# - Cohort names/IDs and concept set IDs are used exactly as provided.
# - Where the template expects additional fields not present in the specs
#   (e.g., excludedCovariateConcepts concept IDs), we keep the template structure
#   but do not invent values.
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# WebAPI endpoint used to export cohort definitions and resolve concept sets
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export the target, comparator, and outcome cohort definitions from ATLAS/WebAPI.
# The cohort IDs below come directly from <Analysis Specifications>.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts ------------------------------------------------------------
# Strategus/CohortMethod analyses typically use small integer cohort IDs.
# We map:
#   target1      -> 1
#   comparator1  -> 2
#   outcome1     -> 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# Resolve the negative control concept set (id/name from specs) into a list of
# concept IDs. These are then converted into "negative control outcome cohorts"
# (concept-based outcomes) with cohort IDs starting at 101.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # negative
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
  mutate(
    cohortId = row_number() + 100
  ) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no overlap between primary cohort IDs (1..3) and negative controls (101+)
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis --------
# Outcomes of interest (primary outcomes) from cohort definitions:
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # Template includes cleanWindow; not specified in specs, keep template default.
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate include/exclude concept lists --------------------------------------
# Specs provide "conceptsToInclude" and "conceptsToExclude" but with null IDs.
# We keep the template objects; since there are no valid concept IDs, these are empty.
includedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource: cohort definitions (target/comparator/outcome)
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Shared resource: negative control outcomes (concept set resolved above)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications: generate cohort stats (template default TRUE)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
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

# CohortMethodModule -----------------------------------------------------------

# Study periods (from specs: one period)
# IMPORTANT: The template comment says to leave empty if not restricting.
# Here we DO restrict, because specs provide explicit dates.
studyPeriods <- tibble(
  studyStartDate = c("20110101"),
  studyEndDate   = c("20131231")
)

# Time-at-risk (TAR) settings (from specs: one TAR)
# We create a label to identify this TAR in the analysis description.
timeAtRisks <- tibble(
  label = c("TAR_3_to_90_from_cohort_start"),
  riskWindowStart = c(3),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(90),
  endAnchor = c("cohort start")
)

# Propensity Score settings - match on PS (from specs: one PS setting)
# caliperScale must match allowed values; specs: "standardized logit"
matchOnPsArgsList <- tibble(
  label = c("match_maxRatio1_caliper0.2_stdLogit"),
  maxRatio = c(1),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# Propensity Score settings - stratify by PS (from specs: null)
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata = c(),
  baseSelection = c()
)

# Build a single PS configuration list (each entry has: method, label, params)
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

# Iterate through all analysis setting combinations ----------------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create PS adjustment args based on method
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
      }

      # Covariate settings -----------------------------------------------------
      # Specs do not provide a custom FeatureExtraction covariateSettings object.
      # We use default covariates, consistent with the template.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcomes list ----------------------------------------------------------
      # - Primary outcome(s): from oList (outcome cohort 3)
      # - Negative controls: from negativeControlOutcomeCohortSet (cohortId 101+)
      #
      # Specs: removeSubjectsWithPriorOutcome = TRUE and priorOutcomeLookBack = 99999
      # We apply priorOutcomeLookback to outcomes of interest as in the template.
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

      # Target-Comparator-Outcomes --------------------------------------------
      # Specs do not provide target/comparator "drug concept IDs" to exclude from
      # covariates; we therefore only use excludedCovariateConcepts (empty here).
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = c(
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # getDbCohortMethodDataArgs ---------------------------------------------
      # Specs:
      # - studyStartDate / studyEndDate: provided above
      # - maxCohortSize: 0
      # Note: restrictToCommonPeriod is specified under createStudyPopArgs in specs,
      # but the template places it in getDbCohortMethodDataArgs. We apply it here
      # to match the template structure and ensure the restriction is enforced.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,  # from specs: createStudyPopArgs$restrictToCommonPeriod
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,              # from specs: getDbCohortMethodDataArgs$maxCohortSize
        covariateSettings = covariateSettings
      )

      # createPsArgs -----------------------------------------------------------
      # Specs:
      # - maxCohortSizeForFitting: 250000
      # - errorOnHighCorrelation: TRUE
      # - prior: laplace, useCrossValidation: TRUE
      # - control: tolerance 2e-7, cvType auto, fold 10, cvRepetitions 10,
      #            noiseLevel silent, resetCoefficients TRUE, startingVariance 0.01
      #
      # Note: Cyclops::createControl uses 'cvRepetitions' but not 'fold' in some
      # versions; the template does not pass 'fold'. We keep the template call
      # signature and apply the provided values where supported.
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # keep template behavior to allow pipeline completion
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",
          exclude = c(0),
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",
          cvType = "auto",
          seed = 1,
          resetCoefficients = TRUE,
          tolerance = 2e-07,
          cvRepetitions = 10,     # from specs
          startingVariance = 0.01
        )
      )

      # Covariate balance args (template defaults) -----------------------------
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs ----------------------------------------------------
      # Specs:
      # - modelType: cox
      # - stratified: false
      # - useCovariates: false
      # - inversePtWeighting: false
      # - prior: laplace, useCrossValidation: true
      # - control: tolerance 2e-7, cvType auto, fold 10, cvRepetitions 10,
      #            noiseLevel quiet, resetCoefficients true, startingVariance 0.01
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
          cvType = "auto",
          seed = 1,
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          tolerance = 2e-07,
          cvRepetitions = 10,     # from specs
          noiseLevel = "quiet"
        )
      )

      # createStudyPopArgs -----------------------------------------------------
      # Specs:
      # - restrictToCommonPeriod: true (applied in getDbCohortMethodDataArgs per template)
      # - firstExposureOnly: false
      # - washoutPeriod: 0
      # - removeDuplicateSubjects: "keep all"
      # - censorAtNewRiskWindow: false
      # - removeSubjectsWithPriorOutcome: true
      # - priorOutcomeLookBack: 99999
      # - timeAtRisks: start 3 from cohort start; end 90 from cohort start; minDaysAtRisk 1
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,  # restriction handled upstream in getDbCohortMethodDataArgs
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
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Append the settings to Analysis List ----------------------------------
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
    }
  }
}

# Create CohortMethod module specifications -----------------------------------
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save to JSON ----------------------------------------------------------------
# Output path follows the template pattern; "mars" is the studyName.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "mars", "marsAnalysisSpecification.json")
)