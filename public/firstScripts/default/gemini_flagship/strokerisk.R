################################################################################
# This script uses the OHDSI Strategus package to create a complete
# analysis specification for a comparative cohort study. The settings are
# derived from the provided <Analysis Specifications>.
#
# Refer to the "Create analysis specifications" section of the Strategus
# documentation for more details.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# provides further details on each parameter.
################################################################################
library(dplyr)
library(Strategus)

# =========== General & Cohort Settings ===========
# These settings define the cohorts to be used in the analysis, including
# target, comparator, outcome, and negative control cohorts.
# It also defines the name of the study, used for file paths.
#
# Settings from Analysis Specifications:
#   name: "strokerisk"
#   cohortDefinitions:
#     targetCohort: id=1794126, name="target1"
#     comparatorCohort: id=1794132, name="comparator1"
#     outcomeCohort: id=1794131, name="outcome1"
#   negativeControlConceptSet: id=1888110
# =================================================

# The study name is used for the output folder structure.
studyName <- "strokerisk"

# Shared Resources -------------------------------------------------------------
# Get the cohort definitions from a WebAPI instance.
# Using a public demo Atlas instance here.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Export the cohort definitions for target, comparator, and outcome(s).
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# It's a convention in Strategus to re-number cohorts to simple integers (1, 2, 3...)
# This makes referencing them in the analysis specifications easier.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1 # Target
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2 # Comparator
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3 # Outcome

# Define negative control outcomes by resolving a concept set.
# These will be used for calibration.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # From Analysis Specifications: negativeControlConceptSet$id
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
  # Assign cohort IDs starting from 101 to avoid collision with T, C, O cohorts.
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Sanity check to ensure no cohort IDs are accidentally duplicated.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohort combinations for the analysis.
# These tables will be iterated over to create the full analysis specification.

# Outcomes of interest (in this case, one outcome).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Filter for the re-numbered outcome cohort
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # Clean window for outcome definition, not specified in specs, using a reasonable default.
  mutate(cleanWindow = 365)

# Target and Comparator pairs for the CohortMethod analysis.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate Selection ----------------------------------------------------------
# Settings from Analysis Specifications:
#   covariateSelection:
#     conceptsToInclude: [] (empty)
#     conceptsToExclude: [] (empty)
#
# Based on the specifications, no additional concepts are included or excluded
# from the default covariate settings. The analysis will still exclude the

# drug concepts used to define the T & C cohorts by default in CohortMethod.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# The 'conceptsToInclude' was also empty, so we will not define an
# `includedCovariateConcepts` data frame and will rely on the default covariate
# settings from FeatureExtraction.

# =========== Module Specifications ===========
# The following sections define the settings for each Strategus module that
# will be part of the analysis execution.
# =============================================

# CohortGeneratorModule --------------------------------------------------------
# This module is responsible for generating all the cohorts defined above.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule ------------------------------------------------------
# This module runs a standard set of diagnostics on the generated cohorts.
# Settings are not specified in the Analysis Specifications, so reasonable
# defaults from the template are used.
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
# This module performs the main comparative cohort analysis.

# Define the overall study periods. The analysis will be repeated for each period.
# Settings from Analysis Specifications: getDbCohortMethodDataArgs$studyPeriods
studyPeriods <- tibble::tibble(
  studyStartDate = c("20010101", "20010101"),
  studyEndDate   = c("20171231", "20150930")
)

# Define the time-at-risk (TAR) windows. The analysis will be repeated for each TAR.
# Settings from Analysis Specifications: createStudyPopArgs$timeAtRisks
timeAtRisks <- tibble::tibble(
  label = c("On Treatment"), # A descriptive label for the TAR
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
)

# Define the propensity score (PS) adjustment strategies.
# The analysis will be repeated for each strategy.
#
# Settings from Analysis Specifications: propensityScoreAdjustment$psSettings
# Three strategies are specified:
# 1. No adjustment (crude analysis)
# 2. Matching with maxRatio=1, caliper=0.05, scale='propensity score'
# 3. Matching with maxRatio=10, caliper=0.2, scale='standardized logit'
#
# The template's logic is adapted to build a single list of configurations
# representing all three strategies.

# First, define the matching strategies in a data frame.
matchOnPsArgsList <- tibble::tibble(
  label = c("1-to-1 PS Matching", "Variable Ratio PS Logit Matching"),
  maxRatio  = c(1, 10),
  caliper = c(0.05, 0.2),
  caliperScale  = c("propensity score", "standardized logit")
)

# The specifications do not include stratification, so this data frame is empty.
stratifyByPsArgsList <- tibble::tibble()

# Build a single PS configuration list that will be iterated over.
psConfigList <- list()

# Manually add the unadjusted (crude) analysis, as it is the first option
# in the specifications (matchOnPsArgs: null, stratifyByPsArgs: null).
psConfigList[[1]] <- list(
  method = "none",
  label  = "Crude (unadjusted)",
  params = list()
)

# Convert the `matchOnPsArgsList` data frame into configuration list items.
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

# Convert the `stratifyByPsArgsList` data frame into configuration list items.
# (This loop will not run as the data frame is empty, but is kept for template consistency).
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

# Main loop to generate all analysis variations --------------------------------
# This section iterates through every combination of study period, time-at-risk,
# and PS strategy to create a complete list of `cmAnalysis` objects.
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Set the PS adjustment arguments based on the current configuration.
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
      } else { # "none"
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- NULL
      }

      # Define settings for creating the initial cohort data.
      # Settings from Analysis Specifications: getDbCohortMethodDataArgs
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = TRUE,
        removeDuplicateSubjects = "keep first",
        washoutPeriod = 183,
        maxCohortSize = 0,
        covariateSettings = FeatureExtraction::createDefaultCovariateSettings(
          addDescendantsToExclude = TRUE
        )
      )

      # Define settings for creating the study population (applying TARs, washout, etc.)
      # Settings from Analysis Specifications: createStudyPopArgs
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        removeSubjectsWithPriorOutcome = FALSE,
        priorOutcomeLookback = 99999,
        censorAtNewRiskWindow = FALSE
      )

      # Define settings for propensity score model creation.
      # Settings from Analysis Specifications: propensityScoreAdjustment$createPsArgs
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",
          cvType = "auto",
          seed = 1,
          resetCoefficients = TRUE,
          tolerance = 2e-07,
          cvRepetitions = 10,
          fold = 10,
          startingVariance = 0.01
        )
      )

      # Define settings for the outcome model.
      # Settings from Analysis Specifications: fitOutcomeModelArgs
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
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
          seed = 1,
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          tolerance = 2e-07,
          cvRepetitions = 10,
          fold = 10,
          noiseLevel = "quiet"
        )
      )

      # Create the list of outcomes, combining the primary outcome of interest
      # with the negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA
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

      # Create the list of T-C-O combinations.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorId[i],
          outcomes = outcomeList,
          # Per spec, no additional covariates are excluded.
          # The T&C concepts themselves are excluded automatically by CohortMethod.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Define settings for covariate balance computation.
      # These are not in the spec, so reasonable defaults are used.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Assemble all the defined arguments into a single cmAnalysis object.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Period: %s-%s; TAR: %s; PS: %s",
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

# Create the final CohortMethod module specifications using the list of analyses.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the final analysis specifications object ----------------------------
# This combines all the shared resources and module specifications into a single
# JSON file that can be executed by Strategus.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the specifications to a JSON file.
# The folder structure inst/studyName is a convention for R packages.
outputFolder <- file.path("inst", studyName)
if (!dir.exists(outputFolder)) {
  dir.create(outputFolder, recursive = TRUE)
}

ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path(outputFolder, paste0(studyName, "AnalysisSpecification.json"))
)