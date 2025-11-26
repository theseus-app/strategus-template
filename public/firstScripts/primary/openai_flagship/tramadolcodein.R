################################################################################
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# further details.
# ##############################################################################
library(dplyr)
library(Strategus)
# We explicitly use namespaced calls for the other HADES packages used below.
# If you prefer, you can library(ROhdsiWebApi), library(FeatureExtraction),
# library(CohortMethod), library(Cyclops).

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Analysis Specifications mapping:
# - targetCohort: id = 1794126, name = "target1"
# - comparatorCohort: id = 1794132, name = "comparator1"
# - outcomeCohort: id = 1794131, name = "outcome1"
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to localized study IDs starting from 1 for Target, 2 for Comparator, 3 for Outcome
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes
# Analysis Specifications mapping:
# - negativeControlConceptSet: id = 1888110, name = "negative"
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
  mutate(
    # Ensure these IDs do not overlap with our main cohorts (1, 2, 3)
    # Target/comparator/outcome cohort local ids start at 1, 2, 3...
    # Negative controls start at 101, 102, ...
    cohortId = dplyr::row_number() + 100
  ) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Sanity check for ID overlap
if (length(intersect(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)) > 0) {
  stop("*** Error: duplicate cohort IDs found between main cohorts and negative controls ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: use the re-numbered outcome cohort with a conventional cleanWindow.
# Analysis Specifications: priorOutcomeLookBack = 365 will be used in createStudyPopArgs;
# we annotate cleanWindow = 365 here for conventional reporting.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis 
# Using EXACT names from Analysis Specifications.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# For the CohortMethod LSPS we'll typically exclude the drugs of interest from covariates.
# Analysis Specifications did not provide concept IDs for exposures or specific covariates to exclude/include,
# so we set this to empty. You can populate conceptId/name here if desired.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all,
# Analysis Specifications show empty include/exclude lists, so we rely on default covariates.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
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

# CohortDiagnoticsModule Settings ---------------------------------------------
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

# If you are not restricting your study to a specific time window, 
# please make these strings empty
# Analysis Specifications: studyStartDate = null, studyEndDate = null
# We represent null as empty strings so the study is not restricted by time.
studyPeriods <- tibble::tibble(
  studyStartDate = c(""), # YYYYMMDD; empty = no restriction
  studyEndDate   = c("")  # YYYYMMDD; empty = no restriction
)

# Time-at-risks (TARs) for the outcomes of interest in your study
# Analysis Specifications mapping:
# - riskWindowStart = 1, startAnchor = "cohort start"
# - riskWindowEnd = 0, endAnchor = "cohort end"
# - minDaysAtRisk = 1 (applied in createStudyPopArgs)
timeAtRisks <- tibble::tibble(
  label = c("TAR1"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
) 

# Propensity Score settings - match on PS
# Analysis Specifications mapping:
# - matchOnPsArgs: maxRatio = 1, caliper = 0.2, caliperScale = "standardized logit"
matchOnPsArgsList <- tibble::tibble(
  label = c("match_max1_cal0.2_stdlogit"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
) 

# Propensity Score settings - stratify by PS
# Analysis Specifications: stratifyByPsArgs = null, so this is empty
stratifyByPsArgsList <- tibble::tibble(
  label = character(),
  numberOfStrata  = integer(),
  baseSelection = character() # "all" | "target" | "comparator"
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


# Iterate through all analysis setting combinations
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      if (psCfg$method == "match") {
        # Analysis Specifications mapping: matchOnPsArgs
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # Not used for this analysis (stratifyByPsArgs = null), included here for completeness
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings
      # Analysis Specifications: covariateSelection include/exclude lists are empty; use defaults.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcomes of interest (specified) + negative control outcomes
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            # Align with Analysis Specifications priorOutcomeLookBack = 365
            priorOutcomeLookback = 365
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

      # Target-comparator pairing and covariate exclusions
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Analysis Specifications provided no explicit concept exclusions for covariates
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Database extraction settings for CohortMethod
      # Analysis Specifications mapping:
      # - getDbCohortMethodDataArgs: studyStartDate = null, studyEndDate = null, maxCohortSize = 0
      # Note: createGetDbCohortMethodDataArgs does not take 'restrictToCommonPeriod'; that is applied in createStudyPopArgs.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Propensity score creation settings
      # Analysis Specifications mapping:
      # - createPsArgs: maxCohortSizeForFitting = 250000, errorOnHighCorrelation = true
      #   prior: laplace with useCrossValidation = true
      #   control: tolerance = 2e-7, cvType = "auto", cvRepetitions = 10, noiseLevel = "silent",
      #            resetCoefficients = true, startingVariance = 0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        # Keep Strategus robust in pipelines by not stopping on model-fitting errors
        stopOnError = FALSE,
        prior = Cyclops::createPrior(
          priorType = "laplace", 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          cvType = "auto",
          cvRepetitions = 10,
          tolerance = 2e-07,
          noiseLevel = "silent",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Covariate balance computation settings (standard practice)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model settings
      # Analysis Specifications mapping:
      # - fitOutcomeModelArgs: modelType = "cox", stratified = false, useCovariates = false,
      #   inversePtWeighting = false, prior: laplace with useCrossValidation = true,
      #   control: tolerance = 2e-7, cvType = "auto", cvRepetitions = 10, noiseLevel = "quiet",
      #            resetCoefficients = true, startingVariance = 0.01
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
          cvRepetitions = 10,
          tolerance = 2e-07,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Study population settings
      # Analysis Specifications mapping:
      # - restrictToCommonPeriod = false
      # - firstExposureOnly = false
      # - washoutPeriod = 0
      # - removeDuplicateSubjects = "keep all"
      # - censorAtNewRiskWindow = false
      # - removeSubjectsWithPriorOutcome = true
      # - priorOutcomeLookBack = 365
      # Time-at-risk maps from timeAtRisks tibble above.
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
        minDaysAtRisk = 1
      )

      # Append the settings to Analysis List
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

# Save with EXACT study name from Analysis Specifications: "tramadolcodein"
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "tramadolcodein", "tramadolcodeinAnalysisSpecification.json")
)