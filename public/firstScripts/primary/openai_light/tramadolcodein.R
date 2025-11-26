library(dplyr)
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(ParallelLogger)

################################################################################
# This script creates a Strategus analysis specification JSON using:
# - The settings provided in the <Analysis Specifications> block
# - The structure/template from the <Template>
#
# IMPORTANT:
# - Variable names that are present in the Template and the Analysis
#   Specifications are used as-is. Do not rename these variables.
# - The final JSON file will be written to inst/<studyName>/<studyName>AnalysisSpecification.json
#   where <studyName> is the "name" field in the Analysis Specifications (here: tramadolcodein)
################################################################################

#-------------------------------------------------------------------------------
# Shared Resources -------------------------------------------------------------
#-------------------------------------------------------------------------------
# Base URL for the Atlas/WebAPI where cohort definitions and concept sets are stored.
# Update this if you use a different Atlas instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Analysis name (from Analysis Specifications)
studyName <- "tramadolcodein"

# Cohort Definitions
# We export the target, comparator and outcome cohorts from the WebAPI using the
# exact cohort IDs supplied in the Analysis Specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to a common local numbering scheme:
# target -> 1, comparator -> 2, outcome -> 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes:
# Export the concept set definition (by conceptSetId) specified in Analysis Specifications.
# Then resolve the set, get the concepts and convert into a "cohort-like" negativeControlOutcomeCohortSet
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # from Analysis Specifications (negativeControlConceptSet.id)
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
  # Assign cohortIds for negative controls starting at 101, 102, ...
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no duplicate cohort IDs between the main cohort set and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

#-------------------------------------------------------------------------------
# Create some data frames to hold the cohorts we'll use in each analysis
#-------------------------------------------------------------------------------

# Outcome list:
# Use the re-numbered outcome cohort id (3) and the assigned cohort name from cohortDefinitionSet.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId,
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # cleanWindow corresponds to priorOutcomeLookBack in many templates; set to the value from Analysis Specifications (365)
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Use exact names from Analysis Specifications: "target1" and "comparator1"
# The target/comparator cohort ids correspond to the renumbered ids 1 and 2 above.
# Include placeholder targetConceptId/comparatorConceptId columns (NA) because template expects them.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  # Template expects targetConceptId/comparatorConceptId when building excluded covariates.
  # The Analysis Specifications did not supply these; set them to NA to preserve structure.
  targetConceptId = as.integer(NA),
  comparatorConceptId = as.integer(NA)
)

# Excluded covariate concepts:
# Analysis Specifications did not list any concepts to exclude, so create an empty data.frame.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# Optional included covariates (none specified)
# includedCovariateConcepts <- data.frame(conceptId = integer(), conceptName = character())


#-------------------------------------------------------------------------------
# CohortGeneratorModule --------------------------------------------------------
#-------------------------------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resources for cohort generation: cohortDefinitionSet
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

# Shared resources for negative control outcomes
# occurrenceType = "first" and detectOnDescendants = TRUE are commonly used defaults;
# keep them unless you want a different approach.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# CohortGenerator module specifications: generateStats = TRUE (as in template/spec)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

#-------------------------------------------------------------------------------
# CohortDiagnosticsModule Settings ---------------------------------------------
#-------------------------------------------------------------------------------
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

#-------------------------------------------------------------------------------
# CohortMethodModule -----------------------------------------------------------
#-------------------------------------------------------------------------------
# Study periods:
# Analysis Specifications provided a studyPeriods entry with null studyStartDate and studyEndDate,
# which means "not restricting to a study period". Following the Template instructions,
# leave these vectors empty.
studyPeriods <- tibble(
  studyStartDate = c(), # empty -> no restriction
  studyEndDate   = c()
)

# Time-at-risks (TARs)
# The Analysis Specifications provide a single TAR with:
# riskWindowStart = 1, startAnchor = "cohort start",
# riskWindowEnd = 0, endAnchor = "cohort end",
# minDaysAtRisk = 1
# We create one row with a descriptive label.
timeAtRisks <- tibble(
  label = c("TAR_cohortStart_to_cohortEnd"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
)

# Propensity Score settings - from Analysis Specifications
# There is one psSettings element that matches on PS with:
# maxRatio = 1, caliper = 0.2, caliperScale = "standardized logit"
matchOnPsArgsList <- tibble(
  label = c("MatchOnPs_1to1_caliper0.2"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # allowed: "propensity score" | "standardized" | "standardized logit"
)

# No stratify-by-ps configs specified in Analysis Specifications
stratifyByPsArgsList <- tibble(
  label = character(),
  numberOfStrata  = integer(),
  baseSelection = character()
)

# Build psConfigList combining match and stratify configurations
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

# Prepare to iterate and create CohortMethod analyses
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Build matching or stratification arguments depending on psCfg method
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
        stop("Unknown psCfg method")
      }

      # Covariate settings: default covariates with descendants excluded from the excluded list
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build outcome definitions:
      # - Include the outcome(s) from the cohortDefinitionSet (the primary outcome)
      # - Append negative controls constructed above (outcomeOfInterest = FALSE)
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

      # Build targetComparatorOutcomesList for the CohortMethod module:
      # This structure ties together target, comparator and the outcomes.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # excludedCovariateConceptIds includes:
          # - specific target/comparator concept ids if known (here NA),
          # - plus any globally-specified excluded covariate concepts.
          excludedCovariateConceptIds = c(
            cmTcList$targetConceptId[i],
            cmTcList$comparatorConceptId[i],
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # getDbCohortMethodDataArgs
      # Analysis Specifications' getDbCohortMethodDataArgs: studyPeriods list was null -> not restricting.
      # maxCohortSize from Analysis Specifications is 0 -> meaning no max.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE, # not restricting to a common period
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs - map to Analysis Specifications createPsArgs block
      # The Analysis Specifications specify:
      # maxCohortSizeForFitting = 250000,
      # errorOnHighCorrelation = true,
      # prior: priorType = "laplace", useCrossValidation = true
      # control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      # noiseLevel = "silent", resetCoefficients = true, startingVariance = 0.01
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        prior = Cyclops::createPrior(
          priorType = "laplace",
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

      # Compute covariate balance args
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs - map from Analysis Specifications fitOutcomeModelArgs
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = FALSE,               # from Analysis Specifications
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

      # createStudyPopArgs - map from Analysis Specifications createStudyPopArgs
      # Note: removeDuplicateSubjects is set to "keep all" exactly as provided
      # in the Analysis Specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 365,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Append the settings to cmAnalysisList as a single CohortMethod analysis
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(is.null(studyStartDate), "", as.character(studyStartDate)),
          ifelse(is.null(studyEndDate), "", as.character(studyEndDate)),
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

# If studyPeriods is empty (no restrictions) we didn't enter loop above.
# We still need to create analyses for the un-restricted studyPeriod case.
# The Template often uses empty studyPeriods; so create a single analysis for that scenario
# if cmAnalysisList is empty.
if (length(cmAnalysisList) == 0) {
  for (p in seq_along(psConfigList)) {
    psCfg <- psConfigList[[p]]

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

    covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
      addDescendantsToExclude = TRUE
    )

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

    targetComparatorOutcomesList <- list()
    for (i in seq_len(nrow(cmTcList))) {
      targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
        targetId = cmTcList$targetCohortId[i],
        comparatorId = cmTcList$comparatorCohortId[i],
        outcomes = outcomeList,
        excludedCovariateConceptIds = c(
          cmTcList$targetConceptId[i],
          cmTcList$comparatorConceptId[i],
          excludedCovariateConcepts$conceptId
        )
      )
    }

    getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
      restrictToCommonPeriod = FALSE,
      studyStartDate = NULL,
      studyEndDate = NULL,
      maxCohortSize = 0,
      covariateSettings = covariateSettings
    )

    createPsArgs = CohortMethod::createCreatePsArgs(
      maxCohortSizeForFitting = 250000,
      errorOnHighCorrelation = TRUE,
      prior = Cyclops::createPrior(
        priorType = "laplace",
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

    computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
      maxCohortSize = 250000,
      covariateFilter = NULL
    )
    computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
      maxCohortSize = 250000,
      covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
    )

    fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
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

    createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
      restrictToCommonPeriod = FALSE,
      firstExposureOnly = FALSE,
      washoutPeriod = 0,
      removeDuplicateSubjects = "keep all",
      censorAtNewRiskWindow = FALSE,
      removeSubjectsWithPriorOutcome = TRUE,
      priorOutcomeLookback = 365,
      riskWindowStart = timeAtRisks$riskWindowStart[1],
      startAnchor = timeAtRisks$startAnchor[1],
      riskWindowEnd = timeAtRisks$riskWindowEnd[1],
      endAnchor = timeAtRisks$endAnchor[1],
      minDaysAtRisk = 1,
      maxDaysAtRisk = 99999
    )

    cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
      analysisId = analysisId,
      description = sprintf(
        "Unrestricted study period; TAR: %s; PS: %s",
        timeAtRisks$label[1],
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

# Create CohortMethod module specifications
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

#-------------------------------------------------------------------------------
# Assemble analysis specifications and save to JSON ---------------------------
#-------------------------------------------------------------------------------

analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Ensure output directory exists: inst/<studyName>
outputDir <- file.path("inst", studyName)
if (!dir.exists(outputDir)) dir.create(outputDir, recursive = TRUE)

# Save the analysis specification JSON using ParallelLogger helper
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path(outputDir, sprintf("%sAnalysisSpecification.json", studyName))
)

# Annotative message (not required, but useful when sourcing interactively)
message(sprintf("Analysis specification saved to: %s", file.path(outputDir, sprintf("%sAnalysisSpecification.json", studyName))))