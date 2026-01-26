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
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)

# Shared Resources -------------------------------------------------------------
# Annotation: The baseUrl is used to fetch cohort definitions and concept sets from Atlas.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Annotation: Cohort Definitions are extracted from the 'cohortDefinitions' section
# in the Analysis Specifications.
# The cohortIds are mapped to internal IDs (1, 2, 3) for consistency within the study.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Annotation: Re-numbering cohorts to internal study IDs for easier reference.
# Target cohort (ID 1794126) is mapped to internal ID 1.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortName <- "target1"
# Comparator cohort (ID 1794132) is mapped to internal ID 2.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortName <- "comparator1"
# Outcome cohort (ID 1794131) is mapped to internal ID 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortName <- "outcome1"

# Annotation: Negative control outcomes are defined using the concept set ID
# from 'negativeControlConceptSet' in the Analysis Specifications.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # From Analysis Specifications: negativeControlConceptSet.id
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
  mutate(cohortId = row_number() + 100) %>% # Annotation: Negative control cohort IDs start from 101 to avoid clashes with T/C/O.
  select(cohortId, cohortName, outcomeConceptId)


if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Annotation: 'oList' defines the outcome cohorts for the study.
# It uses the re-numbered outcome cohort (ID 3) and sets a clean window based on 'priorOutcomeLookBack'.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Filter for the re-numbered outcome cohort
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # From Analysis Specifications: createStudyPopArgs.priorOutcomeLookBack

# Annotation: 'cmTcList' defines the target and comparator cohorts for the CohortMethod analysis.
# Uses the re-numbered target (ID 1) and comparator (ID 2) cohorts.
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered target cohort ID
  targetCohortName = "target1", # From Analysis Specifications: cohortDefinitions.targetCohort.name
  comparatorCohortId = 2, # Re-numbered comparator cohort ID
  comparatorCohortName = "comparator1" # From Analysis Specifications: cohortDefinitions.comparatorCohort.name
)

# Annotation: 'excludedCovariateConcepts' lists concepts to be explicitly excluded from covariates.
# In this specification, 'covariateSelection.conceptsToExclude' is empty, so this list will be empty.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Annotation: 'includedCovariateConcepts' is optional.
# In this specification, 'covariateSelection.conceptsToInclude' is empty, so this block is commented out.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default setting
  detectOnDescendants = TRUE # Default setting
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Default setting
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE,
  runIncludedSourceConcepts = TRUE,
  runOrphanConcepts = TRUE,
  runTimeSeries = FALSE, # Default setting
  runVisitContext = TRUE,
  runBreakdownIndexEvents = TRUE,
  runIncidenceRate = TRUE,
  runCohortRelationship = TRUE,
  runTemporalCohortCharacterization = TRUE,
  minCharacterizationMean = 0.01 # Default setting
)

# CohortMethodModule -----------------------------------------------------------

# Annotation: 'studyPeriods' defines specific date ranges for the study.
# From Analysis Specifications: getDbCohortMethodDataArgs.studyPeriods are null,
# indicating no restriction, so this tibble is empty.
studyPeriods <- tibble(
  studyStartDate = c(), # YYYYMMDD
  studyEndDate   = c()  # YYYYMMDD
)

# Annotation: 'timeAtRisks' defines the risk windows for outcome assessment.
# From Analysis Specifications: createStudyPopArgs.timeAtRisks.
timeAtRisks <- tibble(
  label = c("TAR 365-99999 days from cohort start"), # Descriptive label for this TAR
  riskWindowStart  = c(365), # From Analysis Specifications: createStudyPopArgs.timeAtRisks[0].riskWindowStart
  startAnchor = c("cohort start"), # From Analysis Specifications: createStudyPopArgs.timeAtRisks[0].startAnchor
  riskWindowEnd  = c(99999), # From Analysis Specifications: createStudyPopArgs.timeAtRisks[0].riskWindowEnd
  endAnchor = c("cohort start") # From Analysis Specifications: createStudyPopArgs.timeAtRisks[0].endAnchor
)

# Annotation: 'matchOnPsArgsList' defines parameters for propensity score matching.
# From Analysis Specifications: propensityScoreAdjustment.psSettings[0].matchOnPsArgs.
matchOnPsArgsList <- tibble(
  label = c("PS Matching (maxRatio=1, caliper=0.2, scale=standardized logit)"), # Descriptive label
  maxRatio  = c(1), # From Analysis Specifications: propensityScoreAdjustment.psSettings[0].matchOnPsArgs.maxRatio
  caliper = c(0.2), # From Analysis Specifications: propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliper
  caliperScale  = c("standardized logit") # From Analysis Specifications: propensityScoreAdjustment.psSettings[0].matchOnPsArgs.caliperScale
)

# Annotation: 'stratifyByPsArgsList' defines parameters for propensity score stratification.
# From Analysis Specifications: propensityScoreAdjustment.psSettings[0].stratifyByPsArgs is null,
# so this tibble is empty.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c()
)

# Annotation: 'psConfigList' combines all PS adjustment configurations (matching and stratification).
psConfigList <- list()

# Annotation: If matchOnPsArgsList has entries, convert each row into a PS configuration.
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

# Annotation: If stratifyByPsArgsList has entries, convert each row into a PS configuration.
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

# Annotation: If studyPeriods is empty, add a dummy row to ensure the loop runs once for no date restriction.
if (nrow(studyPeriods) == 0) {
  studyPeriods <- tibble(studyStartDate = NA_character_, studyEndDate = NA_character_)
}

# Annotation: Iterate through all analysis setting combinations (study periods, time-at-risks, PS adjustments).
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  # Annotation: studyStartDate and studyEndDate will be NA if no specific study period is defined.
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Annotation: Configure PS adjustment arguments based on the method (match or stratify).
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default setting
          stratificationColumns = c() # Default setting
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default setting
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Annotation: Covariate settings.
      # From Analysis Specifications: covariateSelection.conceptsToInclude and conceptsToExclude are empty,
      # so default covariate settings are used without specific inclusions/exclusions.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Default setting
      )

      # Annotation: Combine outcome cohorts (study outcomes and negative controls).
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # Not applicable for observed outcomes
            priorOutcomeLookback = 99999 # Default setting
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # For negative controls, true effect size is assumed to be 1
          )
        })
      )

      # Annotation: Define target-comparator-outcome combinations.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Annotation: excludedCovariateConceptIds is an empty vector because
          # 'covariateSelection.conceptsToExclude' is empty in the Analysis Specifications
          # and no specific target/comparator drug concepts were provided.
          excludedCovariateConceptIds = c()
        )
      }

      # Annotation: Arguments for fetching cohort method data from the database.
      # Values are from Analysis Specifications: getDbCohortMethodDataArgs.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications: getDbCohortMethodDataArgs.restrictToCommonPeriod
        studyStartDate = if (is.na(studyStartDate)) "" else studyStartDate, # Use empty string if NA
        studyEndDate = if (is.na(studyEndDate)) "" else studyEndDate,     # Use empty string if NA
        maxCohortSize = 0, # From Analysis Specifications: getDbCohortMethodDataArgs.maxCohortSize
        covariateSettings = covariateSettings
      )

      # Annotation: Arguments for creating propensity scores.
      # Values are from Analysis Specifications: propensityScoreAdjustment.createPsArgs.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
        estimator = "att", # Default setting
        prior = Cyclops::createPrior( # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.prior
          priorType = "laplace",
          exclude = c(0), # Default setting
          useCrossValidation = TRUE # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control
          noiseLevel = "silent", # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.noiseLevel
          cvType = "auto", # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.cvType
          seed = 1, # Default setting
          resetCoefficients = TRUE, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.resetCoefficients
          tolerance = 2e-07, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.tolerance
          cvRepetitions = 10, # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.cvRepetitions
          startingVariance = 0.01 # From Analysis Specifications: propensityScoreAdjustment.createPsArgs.control.startingVariance
        )
      )

      # Annotation: Arguments for computing shared covariate balance.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default setting
        covariateFilter = NULL # Default setting
      )
      # Annotation: Arguments for computing covariate balance.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default setting
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default setting
      )

      # Annotation: Arguments for fitting the outcome model.
      # Values are from Analysis Specifications: fitOutcomeModelArgs.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From Analysis Specifications: fitOutcomeModelArgs.modelType
        stratified = FALSE, # From Analysis Specifications: fitOutcomeModelArgs.stratified
        useCovariates = FALSE, # From Analysis Specifications: fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE, # From Analysis Specifications: fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior( # From Analysis Specifications: fitOutcomeModelArgs.prior
          priorType = "laplace", # From Analysis Specifications: fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE # From Analysis Specifications: fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # From Analysis Specifications: fitOutcomeModelArgs.control
          cvType = "auto", # From Analysis Specifications: fitOutcomeModelArgs.control.cvType
          seed = 1, # Default setting
          resetCoefficients = TRUE, # From Analysis Specifications: fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01, # From Analysis Specifications: fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07, # From Analysis Specifications: fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10, # From Analysis Specifications: fitOutcomeModelArgs.control.cvRepetitions
          noiseLevel = "quiet" # From Analysis Specifications: fitOutcomeModelArgs.control.noiseLevel
        )
      )

      # Annotation: Arguments for creating the study population.
      # Values are from Analysis Specifications: createStudyPopArgs and timeAtRisks.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications: createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE, # From Analysis Specifications: createStudyPopArgs.firstExposureOnly
        washoutPeriod = 365, # From Analysis Specifications: createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # From Analysis Specifications: createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # From Analysis Specifications: createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # From Analysis Specifications: createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 365, # From Analysis Specifications: createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1, # From Analysis Specifications: createStudyPopArgs.timeAtRisks[0].minDaysAtRisk
        maxDaysAtRisk = 99999 # Default setting, not specified in JSON
      )

      # Annotation: Append the configured CohortMethod analysis to the list.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          if (is.na(studyStartDate)) "NoStartDate" else studyStartDate,
          if (is.na(studyEndDate)) "NoEndDate" else studyEndDate,
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
  analysesToExclude = NULL, # Default setting
  refitPsForEveryOutcome = FALSE, # Default setting
  refitPsForEveryStudyPopulation = FALSE, # Default setting
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default setting
)

# Create the analysis specifications ------------------------------------------
# Annotation: Combine all module specifications into a single Strategus analysis specification.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Annotation: Save the analysis specifications to a JSON file.
# The file path uses the study name "ranitidinecancer" from the Analysis Specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ranitidinecancer", "ranitidinecancerAnalysisSpecification.json")
)