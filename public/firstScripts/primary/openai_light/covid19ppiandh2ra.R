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
# "covid19ppiandh2ra" using the OHDSI Strategus and HADES CohortMethod modules.
#
# The settings used here are taken from the provided Analysis Specifications
# and are annotated throughout so users can see how each setting is applied.
################################################################################

# Shared Resources -------------------------------------------------------------
# Base Atlas/WebAPI server used to retrieve cohort and concept-set definitions.
# Adjust if you need to fetch definitions from a different server.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ------------------------------------------------------------------------------
# Cohort Definitions
# ------------------------------------------------------------------------------
# Using EXACT cohort IDs and names from the Analysis Specifications:
# - target:   id = 1794126, name = "target1"
# - comparator: id = 1794132, name = "comparator1"
# - outcome:  id = 1794131, name = "outcome1"
#
# We export the cohort definition set from the WebAPI and then re-number them
# so that the study uses compact ids (1,2,3). This is a common pattern for
# Strategus-driven studies (cohorts used directly in modules are expected to be
# in the shared resource with contiguous small ids).
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to small contiguous ids used in the study specification:
# target -> 1, comparator -> 2, outcome -> 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Keep cohort names as defined in the exported definitions. If the exported
# definitions have different names, those will be preserved here.

# ------------------------------------------------------------------------------
# Negative control concept set
# ------------------------------------------------------------------------------
# Use the exact concept set id from the Analysis Specifications:
# - negative control concept set: id = 1888110, name = "negative"
#
# We convert the concept set into a negative control outcome cohort set by
# resolving the concept set and converting the concepts into "cohort-like"
# rows. We also assign cohortIds starting at 101 (so they don't collide with
# the target/comparator/outcome cohort ids 1,2,3).
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
  mutate(cohortId = row_number() + 100) %>% # negative control ids -> 101, 102, ...
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no duplicate cohort ids between the main cohort set
# and the negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# ------------------------------------------------------------------------------
# Build lightweight data frames used by the CohortMethod module code below
# ------------------------------------------------------------------------------

# Outcomes list for CohortMethod: take the outcome cohort (renumbered to 3).
# We include a cleanWindow specification for the target outcome(s) as in the
# template (here: 365 days).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId,
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator mapping for the CohortMethod analyses. We use EXACT
# cohort names from the Analysis Specifications.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  # If specific target/comparator conceptIds should be excluded from covariates,
  # they would be included here. The Analysis Specifications do not provide
  # explicit target/comparator concept ids, so we set NA.
  targetConceptId = NA_integer_,
  comparatorConceptId = NA_integer_,
  stringsAsFactors = FALSE
)

# Excluded covariate concepts:
# The Analysis Specifications provide an empty conceptsToInclude/Exclude list.
# Therefore, we create an empty excludedCovariateConcepts data.frame so no
# additional exclusions are applied beyond (optionally) the target/comparator
# themselves handled above.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character(),
  stringsAsFactors = FALSE
)

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource for cohort definitions (the set we exported & renumbered).
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Shared resource for negative control outcome cohorts
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specification: instruct the CohortGenerator to generate stats for the
# cohort set (matches the template behavior).
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule -----------------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Create cohort diagnostics module specifications with commonly useful
# diagnostics enabled (matches the Template behavior).
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
cmModuleSettingsCreator <- CohortMethodModule$new()

# ------------------------------------------------------------------------------
# Study periods
# ------------------------------------------------------------------------------
# The Analysis Specifications define a single study period:
# studyStartDate = "20200101", studyEndDate = "20200515"
# If no study period restriction is desired, these would be empty strings.
studyPeriods <- tibble::tibble(
  studyStartDate = c("20200101"), # YYYYMMDD
  studyEndDate   = c("20200515")
)

# ------------------------------------------------------------------------------
# Time-at-risk windows (TARs)
# ------------------------------------------------------------------------------
# The Analysis Specifications provide a single TAR:
# - riskWindowStart = 1, startAnchor = "cohort start"
# - riskWindowEnd   = 99999, endAnchor = "cohort start"
# - minDaysAtRisk = 1
#
# We add a human-readable label for the TAR to carry through into descriptions.
timeAtRisks <- tibble::tibble(
  label = c("TAR1"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd  = c(99999),
  endAnchor = c("cohort start")    # "cohort start" | "cohort end"
)

# ------------------------------------------------------------------------------
# Propensity score configurations
# ------------------------------------------------------------------------------
# The Analysis Specifications indicate stratification by PS with:
#   numberOfStrata = 5, baseSelection = "all"
# No match-on-PS settings are provided (matchOnPsArgs = null).
stratifyByPsArgsList <- tibble::tibble(
  label = c("stratify_ps_5"),
  numberOfStrata = c(5),
  baseSelection = c("all"),
  stringsAsFactors = FALSE
)

# Build psConfigList from the stratify data frame (no matching configs).
psConfigList <- list()
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
# Build CohortMethod analyses (iterate over studyPeriods x timeAtRisks x psConfigs)
# ------------------------------------------------------------------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Determine PS adjustment arguments (match vs stratify)
      if (psCfg$method == "match") {
        # Not used in the provided Analysis Specifications (kept for completeness).
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

      # Covariate settings: The Analysis Specifications did not provide explicit
      # covariate inclusion lists (they were blank), so we use the default
      # covariate settings but set addDescendantsToExclude = TRUE as in template.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build the list of outcomes for CohortMethod:
      # - the main outcome(s) from oList (outcome cohort id 3)
      # - the negative control outcomes resolved from the concept set (ids 101+)
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

      # Build targetComparatorOutcomes list: one entry per target/comparator pair.
      # The createTargetComparatorOutcomes call expects excludedCovariateConceptIds,
      # so we append any known target/comparator concept ids and the
      # excludedCovariateConcepts (which is empty in this spec).
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        excludedIds <- unique(na.omit(c(
          cmTcList$targetConceptId[i],
          cmTcList$comparatorConceptId[i],
          excludedCovariateConcepts$conceptId
        )))
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedIds
        )
      }

      # getDbCohortMethodDataArgs: use the study period dates from the spec and
      # set maxCohortSize as specified (0 => no limit).
      # We include the covariateSettings defined above.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # typically want cohorts defined over the same period for comparability
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs: map exactly from Analysis Specifications:
      # - maxCohortSizeForFitting = 250000
      # - errorOnHighCorrelation = TRUE
      # - prior: laplace with useCrossValidation = TRUE
      # - control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      #   noiseLevel = "silent", resetCoefficients = TRUE, startingVariance = 0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # as in the template to allow Strategus to continue other tasks
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

      # Covariate balance computation arguments; these control table 1 and
      # balance diagnostics. We keep defaults similar to the template.
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs: follow Analysis Specifications exactly:
      # - modelType = "cox", stratified = TRUE, useCovariates = FALSE, inversePtWeighting = FALSE
      # - prior = laplace with useCrossValidation = TRUE
      # - control with cvType="auto", fold=10, cvRepetitions=10, noiseLevel="quiet",
      #   resetCoefficients=TRUE, startingVariance = 0.01, tolerance = 2e-7
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
          seed = 1,
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # createStudyPopArgs: map from Analysis Specifications:
      # - restrictToCommonPeriod = FALSE
      # - firstExposureOnly = TRUE
      # - washoutPeriod = 365
      # - removeDuplicateSubjects = "keep all"
      # - censorAtNewRiskWindow = FALSE
      # - removeSubjectsWithPriorOutcome = TRUE
      # - priorOutcomeLookBack = 99999
      # - risk window values taken from timeAtRisks[t,]
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = TRUE,
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

      # Create the CohortMethod analysis specification and append to list.
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

# Create the CohortMethod module specifications: include the targetComparatorOutcomesList,
# do not refit PS for each outcome/study population (as per template defaults).
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the top-level Strategus analysis specifications object ----------------
# Add the shared resources and module specifications in the order they should be
# executed by Strategus.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Persist the analysis specification JSON to inst/<studyName>/<studyName>AnalysisSpecification.json
# Use EXACT study name from the Analysis Specifications: "covid19ppiandh2ra"
outputDir <- file.path("inst", "covid19ppiandh2ra")
if (!dir.exists(outputDir)) dir.create(outputDir, recursive = TRUE)
outputFile <- file.path(outputDir, "covid19ppiandh2raAnalysisSpecification.json")

# Save specification to disk
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  outputFile
)

# Print summary to console (helpful when running interactively)
message("Strategus analysis specification saved to: ", outputFile)
message("Study name: covid19ppiandh2ra")
message("Number of CohortMethod analyses created: ", length(cmAnalysisList))