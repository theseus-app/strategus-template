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
# defined in <Analysis Specifications>. The study name is taken exactly as:
#   semaglutideandnaion
#
# The script follows the template provided in <Template> and applies the exact
# cohort IDs, negative control concept set ID, and analytic settings specified
# in <Analysis Specifications>.
#
# Important: This script contains inline annotations explaining which setting
# from the Analysis Specifications is applied and where.
################################################################################

# Base URL for the ATLAS/WebAPI instance used to retrieve cohort and concept set
# definitions. You may change this to your local Atlas/WebAPI as needed.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ------------------------------------------------------------------------------
# Cohort Definitions
# ------------------------------------------------------------------------------
# The analysis specification uses three main cohorts (target, comparator, outcome)
# with the exact ids/names from the Analysis Specifications:
#  - target cohort id: 1794126, name: "target1"
#  - comparator cohort id: 1794132, name: "comparator1"
#  - outcome cohort id: 1794131, name: "outcome1"
#
# We export these cohort definitions from the WebAPI so that Strategus modules
# (CohortGenerator, CohortDiagnostics, CohortMethod) can use them.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts so that target/comparator/outcome map to 1, 2, 3
# This aligns with how the template expects cohortIds for downstream modules
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# ------------------------------------------------------------------------------
# Negative control outcomes (concept set)
# ------------------------------------------------------------------------------
# Use the exact conceptSetId from Analysis Specifications:
#   negativeControlConceptSet id: 1888110, name: "negative"
#
# We convert the concept set into a set of "outcome" cohorts for negative controls.
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
  # Assign cohortId values starting at 101 so they do not overlap with 1,2,3
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no duplicate cohort ids between primary cohorts and negatives
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# ------------------------------------------------------------------------------
# Lists / data frames used for module configuration
# ------------------------------------------------------------------------------
# Outcomes: build oList from the re-numbered primary cohortDefinitionSet.
# We expect the outcome cohort was re-numbered to cohortId == 3 above.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # cleanWindow corresponds to a window during which prior outcomes would lead to
  # excluding subjects if removeSubjectsWithPriorOutcome == TRUE. We use 365 days
  # here as in the template (this is a template default and can be edited).
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Use the exact cohort names from Analysis Specifications: "target1", "comparator1"
# targetCohortId and comparatorCohortId must match the re-numbered ids (1 and 2)
# We also include placeholder columns targetConceptId and comparatorConceptId
# (set to NA) so the template-style code that references these columns works.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  targetConceptId = NA_integer_,    # no explicit drug concept id provided in spec
  comparatorConceptId = NA_integer_ # no explicit drug concept id provided in spec
)

# Excluded covariates: Analysis Specifications did not provide explicit concepts
# to exclude (empty placeholders). Create an empty data.frame to satisfy template
# code that expects a data.frame.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# If you wanted to restrict covariates to an explicit set, you would populate
# includedCovariateConcepts here (the specifications contain an empty placeholder).
# includedCovariateConcepts <- data.frame(conceptId = c(...), conceptName = c(...))

# ------------------------------------------------------------------------------
# CohortGeneratorModule specifications
# ------------------------------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Negative control outcomes are provided as a concept set (negativeControlOutcomeCohortSet)
# and we want to detect only the first occurrence of each negative control in the generated
# cohorts, including descendant concepts (detectOnDescendants = TRUE).
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specification: ask the CohortGenerator module to generate stats for the cohorts
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ------------------------------------------------------------------------------
# CohortDiagnosticsModule specifications
# ------------------------------------------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# We configure the diagnostics to run a comprehensive set of checks; these are
# template defaults and appropriate for exploratory cohort diagnostics.
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
# The Analysis Specifications define a single study period:
#   studyStartDate = 20171201
#   studyEndDate   = 20231231
# (These are expressed as YYYYMMDD integers as in the template.)
studyPeriods <- tibble::tibble(
  studyStartDate = c(20171201),
  studyEndDate   = c(20231231)
)

# Time-at-risk (TAR) settings from Analysis Specifications:
# single TAR with:
#   riskWindowStart = 1
#   startAnchor = "cohort start"
#   riskWindowEnd = 0
#   endAnchor = "cohort end"
#   minDaysAtRisk = 1
timeAtRisks <- tibble::tibble(
  label = c("RiskWindow_1_to_end"),         # descriptive label used in analysis description
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),         # allowed values: "cohort start" | "cohort end"
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
)

# Propensity score (PS) configurations:
# The Analysis Specifications specify two PS adjustment strategies:
# 1) matching on PS with maxRatio=1, caliper=0.2, caliperScale="standardized logit"
# 2) stratification by PS with numberOfStrata=5, baseSelection="all"
matchOnPsArgsList <- tibble::tibble(
  label = c("match_1_0.2_slogit"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # matches allowed template values
)

stratifyByPsArgsList <- tibble::tibble(
  label = c("stratify_5_all"),
  numberOfStrata  = c(5),
  baseSelection = c("all") # allowed: "all" | "target" | "comparator"
)

# Build a combined PS configuration list: each element is a list with method,label,params
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

# Prepare lists to collect CohortMethod analyses
cmAnalysisList <- list()
analysisId <- 1

# Loop over study periods, time-at-risks, and PS configurations to create analyses
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create matching/stratification argument objects depending on psCfg$method
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
        stop("Unknown PS config method: ", psCfg$method)
      }

      # Covariate settings: use default covariate settings (include standard covariates).
      # addDescendantsToExclude = TRUE is a template default ensuring descendant concepts
      # are included when excluding concepts (empty in this spec).
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build the outcome list: first the target outcomes (from oList),
      # then the negative controls (negativeControlOutcomeCohortSet).
      # For target outcomes we mark outcomeOfInterest = TRUE and set priorOutcomeLookback = 99999
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
          # Negative controls are treated as outcomes with trueEffectSize = 1 (null)
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Build the target-comparator-outcome configurations that CohortMethod requires
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # excludedCovariateConceptIds: combine any explicit target/comparator drug concept ids
          # (none provided in the Analysis Specifications: NA) and the excludedCovariateConcepts list.
          excludedCovariateConceptIds = c(
            cmTcList$targetConceptId[i],
            cmTcList$comparatorConceptId[i],
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # getDbCohortMethodDataArgs: these control cohort data extraction for CohortMethod:
      # Analysis Specifications 'getDbCohortMethodDataArgs' section:
      #   studyPeriods: same as above (applied here per-iteration)
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
        covariateSettings = covariateSettings
      )

      # createPsArgs: settings used when fitting the PS model. Taken from
      # Analysis Specifications -> propensityScoreAdjustment -> createPsArgs
      # Note: Cyclops prior/control are used to implement regularization; we set
      # parameters exactly as specified.
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,
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
          startingVariance = 0.01,
          seed = 1
        )
      )

      # Covariate balance settings (shared and per-analysis)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs: settings used for effect estimation (Cox model)
      # from Analysis Specifications -> fitOutcomeModelArgs
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
          startingVariance = 0.01,
          seed = 1
        )
      )

      # createStudyPopArgs: how to create the study population for outcome modeling.
      # These are taken from "createStudyPopArgs" in the Analysis Specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = TRUE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Prepare and append the CohortMethod analysis specification
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

# Create the CohortMethod module specifications with the analyses configured above.
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
# Assemble the full Strategus analysis specification
# ------------------------------------------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() %>%
  Strategus::addSharedResources(cohortDefinitionShared) %>%
  Strategus::addSharedResources(negativeControlsShared) %>%
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specification JSON to inst/<studyName>/<studyName>AnalysisSpecification.json
# Use the study name exactly as provided in Analysis Specifications: "semaglutideandnaion"
outputFile <- file.path("inst", "semaglutideandnaion", "semaglutideandnaionAnalysisSpecification.json")
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  outputFile
)

# Informational message for the user (will appear in console when script runs)
message("Strategus analysis specification saved to: ", outputFile)