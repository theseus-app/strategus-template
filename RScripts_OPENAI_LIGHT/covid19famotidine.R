################################################################################
# CreateStrategusAnalysisSpecification.R
# This script builds an OHDSI Strategus analysis specification using the
# provided Analysis Specifications for the covid19famotidine study.
# It uses the OHDSI Strategus framework to define CohortMethod analyses,
# cohort generation, diagnostics, and PS adjustments as described in the
# <Analysis Specifications>.
#
# Notes:
# - The script closely follows the structure and object names used in the
#   Template provided with the assignment to ensure compatibility with OHDSI
#   Strategus tooling.
# - All settings (cohorts, TARs, PS adjustments, etc.) are annotated to aid
#   understanding of how the Analysis Specifications are applied.
################################################################################

# Load required libraries
library(dplyr)
library(Strategus)
library(CohortMethod)
library(Cyclops)
library(FeatureExtraction)
library(ParallelLogger)
library(ROhdsiWebApi)
library(tibble)

# Shared Resources -------------------------------------------------------------
# Base WebAPI endpoint for Atlas OHDSI WebAPI
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# We fetch the target, comparator, and outcome cohort definitions by their IDs
# provided in the Analysis Specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: covid19famotidine target1
    1794132, # Comparator: covid19famotidine comparator1
    1794131  # Outcome: covid19famotidine outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to the 1-based indexing used by Strategus (target=1, comparator=2, outcome=3)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Define negative control outcomes from a ConceptSet (id from Analysis Specifications)
negativeControlConceptSetId <- 1888110

negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = negativeControlConceptSetId,
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
  ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  mutate(cohortId = row_number() + 100) %>% # target/comparator/outcome cohorts start with 1/2/3; negatives start at 101
  select(cohortId, cohortName, outcomeConceptId)

# Covariate selection
# As per Analysis Specifications, conceptsToInclude and conceptsToExclude are empty
covariateSelection <- list(
  conceptsToInclude = data.frame(id = numeric(0), name = character(0), stringsAsFactors = FALSE),
  conceptsToExclude = data.frame(id = numeric(0), name = character(0), stringsAsFactors = FALSE)
)

# GetDbCohortMethodData arguments (study periods)
# Per Analysis Specifications
studyPeriods <- tibble(
  studyStartDate = c("20200201"),
  studyEndDate   = c("20200530")
)

# Time-at-risk (TARs) for outcomes
timeAtRisks <- tibble(
  label = c("default TAR"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(30),
  endAnchor = c("cohort start"),
  minDaysAtRisk = c(1)
)

# Propensity Score adjustments
# Two PS configurations are specified:
# 1) Stratify by PS (5 strata, base selection: all)
# 2) Match on PS (1:1 matching, caliper 0.2, standardized logit)
matchOnPsArgsList <- tibble(
  label = c("PS Matching 1"), # label for PS matching config
  maxRatio  = c(1),
  caliper   = c(0.2),
  caliperScale = c("standardized logit")
)

stratifyByPsArgsList <- tibble(
  label = c("PS Stratification 5 strata"),
  numberOfStrata  = c(5),
  baseSelection   = c("all")
)

# Build a PS configuration list compatible with Strategus
psConfigList <- list()

# If there are "match on PS" configurations, add them
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

# If there are "stratify by PS" configurations, add them
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

# Build target/comparator outcomes
# CM analysis specifics
cmAnalysisList <- list()
analysisId <- 1

# For this analysis, we have a single study period and TAR configuration
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create PS adjustment arguments based on PS config
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio     = psCfg$params$maxRatio,
          caliper      = psCfg$params$caliper,
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

      # Covariate settings: default covariates with descendant handling
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcomes: primary outcome plus negative controls
      # Primary outcomes list from oList (outcome1)
      oList <- cohortDefinitionSet %>%
        filter(.data$cohortId == 3) %>%
        mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
        select(outcomeCohortId, outcomeCohortName) %>%
        mutate(cleanWindow = 365)

      # Build list of outcome objects
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

      # Target-Comparator-Outcomes configuration
      cmTcList <- data.frame(
        targetCohortId = 1,
        targetCohortName = "target1",
        comparatorCohortId = 2,
        comparatorCohortName = "comparator1",
        targetConceptId = NA_real_,
        comparatorConceptId = NA_real_,
        stringsAsFactors = FALSE
      )

      # Excluded covariates for CM analysis (none in this specification)
      excludedCovariateConcepts <- data.frame(
        conceptId = numeric(0),
        conceptName = character(0),
        stringsAsFactors = FALSE
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

      # GetDbCohortMethodDataArgs
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Create PS arguments for the analysis
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
          noiseLevel = "silent",
          cvType = "auto",
          seed = 1,
          resetCoefficients = TRUE,
          tolerance = 2e-07,
          cvRepetitions = 10,
          startingVariance = 0.01
        )
      )

      # Compute balance arguments
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit outcome model arguments
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
          cvType = "auto",
          seed = 1,
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          tolerance = 2e-07,
          cvRepetitions = 10,
          noiseLevel = "quiet"
        )
      )

      # Create study population arguments
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 30,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1
        # maxDaysAtRisk not specified; defaults will be used
      )

      # Append the settings to CM Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: period %s-%s; TAR: %s; PS: %s",
          studyStartDate,
          studyEndDate,
          timeAtRisks$label[t],
          psCfg$label
        ),
        getDbCohortMethodDataArgs = getDbCohortMethodDataArgs,
        createStudyPopArgs = createStudyPopArgs,
        createPsArgs = createPsArgs,
        matchOnPsArgs = if (exists("matchOnPsArgs") && !is.null(matchOnPsArgs)) matchOnPsArgs else NULL,
        stratifyByPsArgs = if (exists("stratifyByPsArgs") && !is.null(stratifyByPsArgs)) stratifyByPsArgs else NULL,
        computeSharedCovariateBalanceArgs = computeSharedCovariateBalanceArgs,
        computeCovariateBalanceArgs = computeCovariateBalanceArgs,
        fitOutcomeModelArgs = fitOutcomeModelArgs,
        targetComparatorOutcomesList = targetComparatorOutcomesList
      )
      analysisId <- analysisId + 1
    }
  }
}

# CohortMethodModule (build module specifications) --------------------------------

cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# CohortGeneratorModule (cohort generation) -------------------------------------

# Cohort Generator module
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

# CohortDiagnosticsModule Settings (diagnostics) --------------------------------

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

# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() %>%
  Strategus::addSharedResources(cohortDefinitionShared) %>% 
  Strategus::addSharedResources(negativeControlsShared) %>% 
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specification JSON to the project
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "studyName", "studyNameAnalysisSpecification.json")
)

################################################################################
# End of CreateStrategusAnalysisSpecification.R
################################################################################