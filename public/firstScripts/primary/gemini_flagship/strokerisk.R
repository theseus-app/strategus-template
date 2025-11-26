################################################################################
# This script uses the OHDSI Strategus package to create a complete analysis
# specification based on the settings provided in the accompanying JSON file.
#
# See the "Create analysis specifications" section of the Strategus documentation
# for more details: https://ohdsi.github.io/Strategus/articles/CreatingAnalysisSpecifications.html
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
# This help page also contains links to the corresponding HADES packages that
# provide further details on the function arguments.
################################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# This section defines the cohorts and concept sets that are used across the
# different analysis modules.

# The baseUrl for the WebAPI instance that hosts the cohort and concept set definitions.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Export the cohort definitions from ATLAS into a data frame.
# These IDs correspond to the cohort definitions in the <Analysis Specifications>.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# In Strategus, it is a convention to use small integer IDs for cohorts.
# Here, we re-number the cohorts to 1, 2, and 3 for target, comparator, and outcome, respectively.
# This mapping is used throughout the rest of the script.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Retrieve the negative control outcome concepts from the specified concept set.
# These concepts are used to generate negative control outcomes for empirical calibration.
# The concept set ID corresponds to "negativeControlConceptSet" in the <Analysis Specifications>.
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
  # To avoid ID conflicts, negative control cohorts are numbered starting from 101.
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)


if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# The outcomes of interest for this analysis. This is based on the "outcomeCohort"
# setting in the <Analysis Specifications>.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Corresponds to the re-numbered outcome cohort (ID: 1794131)
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName)

# Define the target and comparator cohorts for the CohortMethod analysis.
# This corresponds to the "targetCohort" and "comparatorCohort" settings.
cmTcList <- data.frame(
  targetCohortId = 1, # Corresponds to re-numbered target cohort "target1" (ID: 1794126)
  targetCohortName = "target1",
  comparatorCohortId = 2, # Corresponds to re-numbered comparator cohort "comparator1" (ID: 1794132)
  comparatorCohortName = "comparator1"
)

# Optional: Defining concepts to exclude from covariate construction.
# The <Analysis Specifications> for "covariateSelection" did not specify any concepts
# to include or exclude. CohortMethod's default covariate settings will automatically
# exclude the drug concepts for the target and comparator cohorts, so no explicit
# exclusion is needed here.

# CohortGeneratorModule --------------------------------------------------------
# This module is responsible for instantiating the cohorts on the target CDM.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Define the cohort definitions as a shared resource for Strategus.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Define the negative control outcome cohorts as a shared resource.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create the specific settings for the CohortGenerator module.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# This module runs a standard set of diagnostics on the instantiated cohorts.
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
# This section defines the settings for the comparative cohort analysis.

# Define the overall study period for the analysis.
# This corresponds to "studyPeriods" in "getDbCohortMethodDataArgs".
studyPeriods <- tibble(
  studyStartDate = c("20010101"), #YYYYMMDD
  studyEndDate   = c("20171231") #YYYYMMDD
)

# Define the time-at-risk (TAR) windows for the outcomes.
# This corresponds to "timeAtRisks" in "createStudyPopArgs".
timeAtRisks <- tibble(
  label = c("1-day start to cohort end"), # A descriptive label for this TAR
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
)

# Define the propensity score (PS) matching settings.
# This corresponds to "matchOnPsArgs" in "propensityScoreAdjustment".
matchOnPsArgsList <- tibble(
  label = c("10:1 Match on PS, 0.2 caliper std logit"), # A descriptive label
  maxRatio  = c(10),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# The "stratifyByPsArgs" setting was null, so we create an empty tibble.
stratifyByPsArgsList <- tibble()

# Build a single PS configuration list from the settings above.
# This structure helps iterate through different PS adjustment strategies if multiple are defined.
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


# Iterate through all analysis setting combinations to create a list of analyses.
# In this case, there is one study period, one TAR, and one PS setting, so only one analysis will be generated.
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Set up PS adjustment arguments based on the method defined in psConfigList.
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

      # Use default covariate settings from FeatureExtraction.
      # This will include a broad set of covariates and automatically exclude the
      # drug concepts for the target and comparator cohorts.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Define the list of all outcomes, including the primary outcome of interest
      # and the negative control outcomes for calibration.
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

      # Combine the target-comparator pair(s) with the full list of outcomes.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList
          # No `excludedCovariateConceptIds` needed; default settings handle this.
        )
      }

      # Define arguments for getting the data from the database.
      # Settings from "getDbCohortMethodDataArgs" in the specifications.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # 0 means no maximum size.
        covariateSettings = covariateSettings
      )

      # Define arguments for creating the propensity score model.
      # Settings from "createPsArgs" in the specifications.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # Allow Strategus to complete all operations even if one model fails.
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
          startingVariance = 0.01,
          seed = 1 # for reproducibility
        )
      )

      # Arguments for computing covariate balance. These are standard settings.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Define arguments for fitting the outcome model.
      # Settings from "fitOutcomeModelArgs" in the specifications.
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
          tolerance = 2e-7,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          seed = 1 # for reproducibility
        )
      )

      # Define arguments for creating the study population.
      # Settings from "createStudyPopArgs" in the specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        # Time-at-risk settings from the tibble defined earlier
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1
      )

      # Assemble all the defined arguments into a single CohortMethod analysis object.
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

# Create the specific settings for the CohortMethod module.
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
# This final step combines all the module specifications into a single
# JSON object that can be executed by Strategus.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path uses the study name from the <Analysis Specifications>.
analysisSpecificationsJson <- ParallelLogger::convertSettingsToJson(analysisSpecifications)
dir.create(file.path("inst", "strokerisk"), showWarnings = FALSE, recursive = TRUE)
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "strokerisk", "strokeriskAnalysisSpecification.json")
)