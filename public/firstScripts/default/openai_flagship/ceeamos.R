################################################################################
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# further details.
################################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from an ATLAS/WebAPI instance.
# We export the 3 cohorts specified in <Analysis Specifications>:
# - target1 (id = 1794126)
# - comparator1 (id = 1794132)
# - outcome1 (id = 1794131)
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions (exported from WebAPI using exact IDs)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to small sequential IDs for downstream modules:
# 1 = target, 2 = comparator, 3 = outcome
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# The <Analysis Specifications> provides a negative control concept set:
# id = 1888110, name = "negative".
# We:
# 1) Fetch the concept set definition
# 2) Resolve to concepts (including descendants if defined in the set)
# 3) Assign a unique cohortId range (100+) to avoid conflicts with 1,2,3,...
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
  ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
  rename(
    outcomeConceptId = conceptId,
    cohortName = conceptName
  ) %>%
  mutate(
    cohortId = dplyr::row_number() + 100 # target/comparator/outcome = 1,2,3; negatives = 101, 102, ...
  ) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no duplicate cohort IDs between the primary cohorts (1..3)
# and the negative controls (100+)
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between primary and negative-control sets ***")
}

# Create simple data frames to hold the cohorts used in each analysis ----------
# Outcomes: primary "outcome1" is cohortId == 3; set a default cleanWindow
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Using the exact names from <Analysis Specifications>
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  # Optional placeholders for drug-of-interest concept IDs; NA here since none were provided
  targetConceptId = NA_integer_,
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  comparatorConceptId = NA_integer_
)

# Covariate selection (include/exclude) from <Analysis Specifications> ---------
# Provided lists are effectively empty (id = null, name = ""), so no specific
# include/exclude concept constraints are applied beyond defaults.
# We keep these objects for transparency and potential future use.
includedCovariateConcepts <- data.frame(
  conceptId = integer(), # empty
  conceptName = character()
)

excludedCovariateConcepts <- data.frame(
  conceptId = integer(), # empty
  conceptName = character()
)

# CohortGeneratorModule --------------------------------------------------------
# 1) Shared resource: primary cohort definitions
# 2) Shared resource: negative control concept-derived cohorts
# 3) Module specifications: instruct CohortGenerator to generate with stats
cgModuleSettingsCreator <- CohortGeneratorModule$new()

cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# Typical diagnostics turned on; this module can help validate cohort
# definitions and incidence patterns prior to running CohortMethod.
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
# Map <Analysis Specifications> to CM settings.

# 1) Study periods: empty strings mean "no restriction"
#    (CohortMethod treat "" as not restricting by date)
studyPeriods <- tibble::tibble(
  studyStartDate = c(""), # YYYYMMDD or empty
  studyEndDate   = c("")
)

# 2) Time-at-risks (TARs) from <Analysis Specifications.createStudyPopArgs.timeAtRisks>
#    We carry minDaysAtRisk forward into StudyPopulation args.
timeAtRisks <- tibble::tibble(
  label = c("TAR 1", "TAR 2"),
  riskWindowStart  = c(1, 1),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd  = c(0, 99999),
  endAnchor = c("cohort end", "cohort start"),
  minDaysAtRisk = c(1, 1)
)

# 3) Propensity Score settings from <Analysis Specifications.propensityScoreAdjustment>
#    Only matching is specified (stratify = NULL).
matchOnPsArgsList <- tibble::tibble(
  label = c("Match on PS (caliper 0.2 std logit, maxRatio 10)"),
  maxRatio  = c(10),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # exactly as specified
) 

# Empty stratify list (not used)
stratifyByPsArgsList <- tibble::tibble(
  label = character(),
  numberOfStrata  = integer(),
  baseSelection = character()
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Convert "match on PS" rows into configs
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

# Convert "stratify by PS" rows into configs (none specified)
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

# Iterate through all analysis combinations: study period x TAR x PS-config
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # PS adjustment choice for this iteration:
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

      # Covariate settings:
      # No include/exclude concept lists are provided in <Analysis Specifications>,
      # so use defaults and allow descendants of excluded concepts if any are added later.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build outcomes list:
      # - Primary outcome (cohortId = 3) with priorOutcomeLookback = 99999 (per specs)
      # - Negative control outcomes (one for each concept, outcomeOfInterest = FALSE)
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

      # Build T-C-O container, including any excluded covariate concept IDs.
      # Here we combine: (targetConceptId, comparatorConceptId, excludedCovariateConcepts).
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

      # Get Db CohortMethod Data args from <Analysis Specifications.getDbCohortMethodDataArgs>
      # - restrictToCommonPeriod = FALSE
      # - studyStartDate and studyEndDate are empty strings
      # - maxCohortSize = 0 (no downsampling)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # PS creation args from <Analysis Specifications.propensityScoreAdjustment.createPsArgs>
      # Note: stopOnError is set FALSE to allow the full pipeline to continue in Strategus even if a PS model fails.
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,
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

      # Use default shared covariate balance settings + Table1 specs for full covariate balance
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model args from <Analysis Specifications.fitOutcomeModelArgs>
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

      # Study population args from <Analysis Specifications.createStudyPopArgs>
      # For each TAR row, we plug in riskWindowStart/end, anchors, and minDaysAtRisk.
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

      # Bundle the analysis definition
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(nchar(studyStartDate) == 0, "none", studyStartDate),
          ifelse(nchar(studyEndDate) == 0, "none", studyEndDate),
          timeAtRisks$label[t],
          psCfg$label
        ),
        getDbCohortMethodDataArgs = getDbCohortMethodDataArgs,
        createStudyPopArgs = createStudyPopArgs,
        createPsArgs = createPsArgs,
        matchOnPsArgs = if (exists("matchOnPsArgs")) matchOnPsArgs else NULL,
        stratifyByPsArgs = if (exists("stratifyByPsArgs")) stratifyByPsArgs else NULL,
        computeSharedCovariateBalanceArgs = computeSharedCovariateBalanceArgs,
        computeCovariateBalanceArgs = computeCovariateBalanceArgs,
        fitOutcomeModelArgs = fitOutcomeModelArgs
      )
      analysisId <- analysisId + 1
    }
  }
}

# Create CM module specifications
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
# Combine shared resources and modules into a single Strategus specification object
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save to JSON (use the provided analysis name "ceeamos" from <Analysis Specifications>)
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "ceeamos", "ceeamosAnalysisSpecification.json")
)