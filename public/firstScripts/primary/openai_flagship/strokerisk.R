################################################################################
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# further details.
################################################################################
# This script programmatically builds a Strategus analysis specification using:
# - Cohort definitions (target, comparator, outcome) from a WebAPI
# - A negative control outcome concept set from a WebAPI
# - CohortMethod settings reflecting the Analysis Specifications provided
# - CohortGenerator and CohortDiagnostics modules
#
# Analysis name: strokerisk
#
# Notes:
# - All settings are explicitly annotated below to show how they map from the
#   provided Analysis Specifications.
# - Variable and function names match the Template and packages exactly.
################################################################################

# Libraries --------------------------------------------------------------------
library(dplyr)
library(tibble)
library(Strategus)
library(ROhdsiWebApi)
library(Cyclops)
library(FeatureExtraction)
library(CohortMethod)
library(ParallelLogger)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from an ATLAS/WebAPI instance.
# Using the OHDSI public demo instance for reproducibility.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Map to Analysis Specifications:
# - targetCohort.id = 1794126 (name = "target1")
# - comparatorCohort.id = 1794132 (name = "comparator1")
# - outcomeCohort[1].id = 1794131 (name = "outcome1")
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts so Strategus modules have simple, consistent internal IDs.
# 1 => target, 2 => comparator, 3 => outcome
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes
# Map to Analysis Specifications:
# - negativeControlConceptSet.id = 1888110 (name = "negative")
# We will resolve the concept set and create a negative control "outcome cohort set"
# used by Strategus CohortGenerator to generate negative control outcome cohorts.
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
  mutate(
    # Give negative control cohorts unique IDs that do not collide with 1,2,3...
    # For readability: 101, 102, 103, ...
    cohortId = dplyr::row_number() + 100
  ) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Basic QC to ensure no cohortId collisions between primary cohorts and NCs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between primary cohorts and negative controls ***")
}

# Build helper data frames capturing the cohorts we will use -------------------
# Outcomes list from the primary outcome cohort(s) (ID == 3 by our renumbering)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # Optional: cleanWindow is often used by diagnostics; set to 365 as a reasonable default
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis 
# Aligns with Analysis Specifications names/IDs
cmTcList <- tibble::tibble(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate include/exclude concept selection (from Analysis Specifications)
# Provided values are empty/null, so we create empty vectors.
includedConceptIds <- integer(0)  # covariateSelection.conceptsToInclude -> none
excludedConceptIds <- integer(0)  # covariateSelection.conceptsToExclude -> none

# CohortGeneratorModule --------------------------------------------------------
# Create the shared resource specifications for cohorts and negative controls
cgModuleSettingsCreator <- CohortGeneratorModule$new()

cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Configure the CohortGenerator module to generate cohorts and cohort stats
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# Configure diagnostics across all cohorts: target, comparator, outcome, and NCs (generated separately)
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
# Study period(s) mapping to Analysis Specifications:
# getDbCohortMethodDataArgs.studyPeriods: [ { studyStartDate: "20010101", studyEndDate: "20171231" } ]
studyPeriods <- tibble::tibble(
  studyStartDate = c("20010101"), # YYYYMMDD
  studyEndDate   = c("20171231")  # YYYYMMDD
)

# Time-at-risks (TARs) mapping to Analysis Specifications createStudyPopArgs.timeAtRisks:
# riskWindowStart = 1, startAnchor = "cohort start"; riskWindowEnd = 0, endAnchor = "cohort end"; minDaysAtRisk = 1
timeAtRisks <- tibble::tibble(
  label = c("TAR1"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")      # "cohort start" | "cohort end"
)

# Propensity Score settings - match on PS
# Map to Analysis Specifications propensityScoreAdjustment.psSettings.matchOnPsArgs:
# maxRatio = 10, caliper = 0.2, caliperScale = "standardized logit"
matchOnPsArgsList <- tibble::tibble(
  label = c("PS-Match-1"),
  maxRatio  = c(10),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # "propensity score" | "standardized" | "standardized logit"
)

# Propensity Score settings - stratify by PS (none specified in Analysis Specifications)
stratifyByPsArgsList <- tibble::tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0) # "all" | "target" | "comparator"
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# If a data frame for "match on PS" exists and has rows, convert each row to a config
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    # Append a new element at the end of psConfigList
    psConfigList[[length(psConfigList) + 1]] <- list(
      # Identify the PS adjustment method for this config
      method = "match",
      # Human-readable label to carry through into descriptions
      label  = matchOnPsArgsList$label[i],
      # Parameter bundle passed to createMatchOnPsArgs later
      params = list(
        maxRatio     = matchOnPsArgsList$maxRatio[i],
        caliper      = matchOnPsArgsList$caliper[i],
        caliperScale = matchOnPsArgsList$caliperScale[i]
      )
    )
  }
}

# If a data frame for "stratify by PS" exists and has rows, convert each row to a config
if (exists("stratifyByPsArgsList") && nrow(stratifyByPsArgsList) > 0) {
  for (i in seq_len(nrow(stratifyByPsArgsList))) {
    # Append a new element at the end of psConfigList
    psConfigList[[length(psConfigList) + 1]] <- list(
      # Identify the PS adjustment method for this config
      method = "stratify",
      # Human-readable label to carry through into descriptions
      label  = stratifyByPsArgsList$label[i],
      # Parameter bundle passed to createStratifyByPsArgs later
      params = list(
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection  = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}

# Iterate through all analysis setting combinations ----------------------------
cmAnalysisList <- list()
targetComparatorOutcomesList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Create PS adjustment method args based on configuration
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
        stop("Unknown PS method encountered.")
      }

      # Covariate settings (FeatureExtraction)
      # Map to Analysis Specifications covariateSelection (both include/exclude empty)
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE,
        includedCovariateConceptIds = includedConceptIds,
        excludedCovariateConceptIds = excludedConceptIds
      )

      # Build the outcome list including:
      # - The primary outcome(s) (outcomeOfInterest = TRUE)
      # - Negative control outcomes (outcomeOfInterest = FALSE, trueEffectSize = 1)
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

      # Define the Target-Comparator and associated outcomes
      # No additional excluded covariate concept IDs beyond those in covariateSettings
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedConceptIds
        )
      }

      # getDbCohortMethodData args mapping to Analysis Specifications getDbCohortMethodDataArgs:
      # studyStartDate = "20010101", studyEndDate = "20171231", maxCohortSize = 0
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs mapping to Analysis Specifications propensityScoreAdjustment.createPsArgs
      # prior = laplace with useCrossValidation = TRUE
      # control = tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10, noiseLevel = "silent",
      #           resetCoefficients = TRUE, startingVariance = 0.01
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

      # Covariate balance computation settings
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs mapping to Analysis Specifications fitOutcomeModelArgs
      # modelType = "cox", stratified = TRUE, useCovariates = FALSE, inversePtWeighting = FALSE
      # prior/control mirror the provided values (laplace + cross-validation; tolerance, cvRepetitions, etc.)
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

      # createStudyPopArgs mapping to Analysis Specifications createStudyPopArgs
      # restrictToCommonPeriod = FALSE
      # firstExposureOnly = FALSE
      # washoutPeriod = 0
      # removeDuplicateSubjects = "keep all"
      # censorAtNewRiskWindow = FALSE
      # removeSubjectsWithPriorOutcome = TRUE
      # priorOutcomeLookBack = 99999
      # timeAtRisks[1] applied below
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
        minDaysAtRisk = 1
      )

      # Append the settings to Analysis List with a descriptive label
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

# Build CohortMethod module specifications
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

# Persist analysis specifications to JSON (named for the analysis 'strokerisk')
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "strokerisk", "strokeriskAnalysisSpecification.json")
)