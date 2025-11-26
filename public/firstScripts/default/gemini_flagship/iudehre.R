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
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)

# Shared Resources -------------------------------------------------------------
# This section defines the cohorts and concept sets that will be used
# throughout the analysis. These are considered "shared resources" because they
# can be used by multiple modules.

# The baseUrl for the WebAPI instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Define the cohort IDs from your ATLAS instance.
# These IDs are sourced from the "cohortDefinitions" section of the JSON specification.
targetCohortId <- 1794126      # target1
comparatorCohortId <- 1794132  # comparator1
outcomeCohortIds <- c(1794131) # outcome1

# Use ROhdsiWebApi to export the cohort definitions from ATLAS.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    targetCohortId,
    comparatorCohortId,
    outcomeCohortIds
  ),
  generateStats = TRUE
)

# It's a good practice to re-number cohort IDs to a simple, consistent scheme
# for use within the Strategus study (e.g., T=1, C=2, O=3...).
cohortDefinitionSet[cohortDefinitionSet$cohortId == targetCohortId,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == comparatorCohortId,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == outcomeCohortIds[1],]$cohortId <- 3

# Negative Control Outcomes ----------------------------------------------------
# Define the concept set ID for negative controls.
# This ID is from the "negativeControlConceptSet" section of the JSON.
negativeControlConceptSetId <- 1888110

# Fetch the concepts in the negative control concept set.
# These will be used to generate negative control outcome cohorts.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = negativeControlConceptSetId,
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
  # Assign unique cohort IDs for negative controls, starting from 101.
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Check for any duplicate cohort IDs between defined cohorts and negative controls.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Analysis-Specific Cohort Dataframes ------------------------------------------
# Create data frames to hold the specific cohorts used in each analysis module.

# Outcomes of interest for CohortMethod.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName)

# Target and Comparator pairs for the CohortMethod analysis.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate Selection ----------------------------------------------------------
# From the "covariateSelection" section of the JSON.
# The specification has empty arrays for conceptsToInclude and conceptsToExclude.
# Therefore, we create empty dataframes here.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)
includedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# CohortGeneratorModule Settings -----------------------------------------------
# This module is responsible for generating the cohort sets defined above.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Define the cohort definition set as a shared resource.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

# Define the negative control outcome cohort set as a shared resource.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create the module specifications for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# This module runs diagnostics on the generated cohorts.
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

# CohortMethodModule Settings --------------------------------------------------
# This is the main analysis module for the comparative cohort study.

# Study Period Settings --------------------------------------------------------
# From "getDbCohortMethodDataArgs:studyPeriods" in the JSON.
# An empty string for endDate means no end date is applied.
studyPeriods <- tibble(
  studyStartDate = c("20030101"),
  studyEndDate   = c("")
)

# Time-at-Risk (TAR) Settings --------------------------------------------------
# From "createStudyPopArgs:timeAtRisks" in the JSON.
# Defines the periods during which outcomes are assessed relative to cohort entry.
timeAtRisks <- tibble(
  label = c("On Treatment (30-5475 days)", "On Treatment (365-5475 days)"),
  riskWindowStart  = c(30, 365),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd  = c(5475, 5475),
  endAnchor = c("cohort start", "cohort start")
)

# Propensity Score (PS) Adjustment Settings ------------------------------------
# From "propensityScoreAdjustment:psSettings" in the JSON.

# Defines settings for matching on propensity scores.
matchOnPsArgsList <- tibble(
  label = c("1-to-1 Matching on PS"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# Defines settings for stratifying on propensity scores.
stratifyByPsArgsList <- tibble(
  label = c("Stratification on PS (5 strata)"),
  numberOfStrata  = c(5),
  baseSelection = c("all")
)

# Build a single list of PS configurations from the tibbles above.
# This allows iterating through all desired PS adjustment methods.
psConfigList <- list()

if (exists("matchOnPsArgsList") && nrow(matchOnPsesArgsList) > 0) {
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

# Create Analysis Variants -----------------------------------------------------
# Iterate through all combinations of study periods, TARs, and PS settings
# to create a list of analysis variants (cmAnalysisList).
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create arguments for the specific PS adjustment method.
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

      # Define covariate settings. Here, we use the default settings.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )
      
      # Define the target-comparator-outcomes list.
      # This combines the positive outcomes and negative controls.
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
      
      # Create the list of T-C-O combinations to be analyzed.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude concepts based on the empty dataframe defined earlier.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Define arguments for getting data from the database.
      # Sourced from "getDbCohortMethodDataArgs" in the JSON.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = TRUE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "remove all",
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )
      
      # Define arguments for creating the study population.
      # Sourced from "createStudyPopArgs" in the JSON.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = FALSE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1
      )

      # Define arguments for creating the propensity score model.
      # Sourced from "propensityScoreAdjustment:createPsArgs" in the JSON.
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,
        estimator = "att",
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
      
      # Define arguments for computing covariate balance.
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Define arguments for fitting the outcome model.
      # Sourced from "fitOutcomeModelArgs" in the JSON.
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

      # Combine all the arguments into a single cmAnalysis object.
      cmAnalysis <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study Period: %s-%s; TAR: %s; PS: %s",
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
      
      # Add the analysis to the list and increment the ID.
      cmAnalysisList[[analysisId]] <- cmAnalysis
      analysisId <- analysisId + 1
    }
  }
}

# Create CohortMethod Module Specifications ------------------------------------
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the Final Analysis Specifications -------------------------------------
# This combines all the shared resources and module specifications into a single
# object that can be executed by Strategus.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() %>%
  Strategus::addSharedResources(cohortDefinitionShared) %>%
  Strategus::addSharedResources(negativeControlsShared) %>%
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The study name is taken from the "name" field in the JSON specification.
studyName <- "iudehre"
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", studyName, paste0(studyName, "AnalysisSpecification.json"))
)