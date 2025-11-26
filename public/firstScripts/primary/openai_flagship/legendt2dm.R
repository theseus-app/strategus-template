################################################################################
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# further details.
################################################################################

# Load packages used by this script
library(dplyr)
library(tibble)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Configure the ATLAS/WebAPI instance that hosts the cohorts and the negative control concept set
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Pull target, comparator, and outcome cohorts from WebAPI using the EXACT IDs provided
# in the Analysis Specifications, and generate characterization stats that some modules use.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to a small, consistent sequence used within the analysis specification
# 1 = target, 2 = comparator, 3 = outcome
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# Retrieve the concept list from the negative control concept set EXACTLY as specified (id: 1888110).
# We then resolve to included concepts and assign an internal cohortId space that does not
# overlap with the target/comparator/outcomes (e.g., 101, 102, ...).
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
  ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
  rename(
    outcomeConceptId = "conceptId",
    cohortName = "conceptName"
  ) %>%
  mutate(cohortId = dplyr::row_number() + 100) %>%  # ensure NCs do not clash with 1,2,3
  select(cohortId, cohortName, outcomeConceptId)

# Guard against accidental cohortId collisions between main study cohorts and NCs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found across main cohorts and negative controls ***")
}

# Create helper data frames to hold the cohorts used in each analysis ----------

# Outcomes (primary outcome(s) of interest in the main analysis).
# The Analysis Specifications define a single outcome (id 1794131 -> internal 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # Clean window (not directly used by CM, but retained for documentation/consistency)
  mutate(cleanWindow = 365)

# Target and Comparator pairs for CohortMethod analysis (EXACT names from the specifications)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate selection per Analysis Specifications ------------------------------
# The specification lists empty include/exclude sets. We therefore do not restrict
# to specific included concepts and do not add exclusions here (beyond defaults).
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# CohortGeneratorModule --------------------------------------------------------
# Create shared resource specifications for cohorts and negative controls so that
# downstream modules can reuse them. The negative controls will be generated as
# outcome cohorts from the concept list above.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",           # First occurrence of each NC
  detectOnDescendants = TRUE          # Include descendant concepts for NC detection
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE                # Generate cohort counts and characterization stats
)

# CohortDiagnosticsModule Settings --------------------------------------------
# Run CohortDiagnostics on all main cohorts (1,2,3). This does not include the negative
# control cohorts, which are constructed from concept sets and used by CM.
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
# Build a parameter grid from the Analysis Specifications. Strategus will iterate across
# all combinations (study periods x TARs x PS strategies) to produce cmAnalysisList entries.

# Study period (from getDbCohortMethodDataArgs in the Analysis Specifications)
# We set a single analysis window from 19920101 through 20211231.
studyPeriods <- tibble(
  studyStartDate = c("19920101"),
  studyEndDate   = c("20211231")
)

# Time-at-risk settings (from createStudyPopArgs in the Analysis Specifications)
# We use a single TAR: start 1 day after cohort start to cohort end; minDaysAtRisk = 1.
timeAtRisks <- tibble(
  label = c("TAR1"),
  riskWindowStart = c(1),
  startAnchor = c("cohort start"),   # EXACT anchor name per specification
  riskWindowEnd = c(0),
  endAnchor = c("cohort end"),       # EXACT anchor name per specification
  minDaysAtRisk = c(1)
)

# Propensity Score settings: Analysis Specifications indicate PS stratification into 5 strata
# with baseSelection = "all". No matching is requested.
matchOnPsArgsList <- tibble(
  label = character(0),
  maxRatio  = numeric(0),
  caliper = numeric(0),
  caliperScale  = character(0)
)

stratifyByPsArgsList <- tibble(
  label = c("Stratify_5_all"),
  numberOfStrata  = c(5),
  baseSelection = c("all")
)

# Build unified PS configuration list from match/stratify data frames ----------
psConfigList <- list()

# Convert "match on PS" rows to PS configuration (none in this setup)
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

# Convert "stratify by PS" rows to PS configuration (one requested in the specification)
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

# Build the CohortMethod analysis list -----------------------------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # PS adjustment method per current configuration
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

      # Covariates: use the default FeatureExtraction settings and add descendants to excludes
      # The Analysis Specifications did not request included/excluded concept sets, so defaults apply.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcomes list: includes the study outcome(s) of interest and the negative controls
      outcomeList <- append(
        # Main outcome(s)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999  # EXACT value from createStudyPopArgs in the Analysis Specifications
          )
        }),
        # Negative control outcomes
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Target-comparator pairs with the indicated outcomes
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # The specification lists no included/excluded covariate concept sets,
          # so we only pass the (empty) excludedCovariateConcepts constructed above.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Data extraction/time window limits per Analysis Specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Create PS (model fitting) per Analysis Specifications:
      # - Laplace prior with cross-validation
      # - Cyclops control: tolerance, cvType, fold, cvRepetitions, noiseLevel, resetCoefficients, startingVariance
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # allow Strategus to proceed even if a PS model cannot be fitted
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

      # Covariate balance settings (shared and table-1 subset)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model fitting per Analysis Specifications (Cox, stratified, no covariates, no IPTW)
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

      # Study population construction per Analysis Specifications
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
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t]
      )

      # Append this analysis configuration to the list, with a descriptive label
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

# Create the CohortMethod module specifications with all analysis definitions ---
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the analysis specifications (Strategus) --------------------------------
# We add:
# - cohort shared resources
# - negative control shared resources
# - module specifications: cohort generation, diagnostics, and cohort method
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Persist the final analysis specification JSON using the EXACT study name
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "legendt2dm", "legendt2dmAnalysisSpecification.json")
)