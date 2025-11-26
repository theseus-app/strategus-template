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

# ==============
# Part 1: Set up the connection to the WebAPI
# ==============
# This section has been pre-populated with an example of how to connect
# to the OHDSI Atlas/WebAPI instance. By default, this script will connect
# to the OHDSI atlas-demo.ohdsi.org WebAPI.
#
# @seealso ROhdsiWebApi::setAuthHeader
# @seealso ROhdsiWebApi::setBaseUrl
#
# This script requires a configured `ROhdsiWebApi` instance. This can be done
# by un-commenting the following code and updating the baseUrl. Alternatively,
# you can set the `BASE_URL` environmental variable.
#
# ROhdsiWebApi::setAuthHeader(keyring::key_get("ohdsiWebApiKey"))
# ROhdsiWebApi::setBaseUrl("https://atlas.yourorg.org/WebAPI")

# =========================================================================
# Part 2: Define the Cohorts and Concept sets for the Study
# =========================================================================
# In this section, we will create the R-objects that will be used to reference
# the cohorts and concept sets for this study.
#
# When using the OHDSI WebAPI, you will need to find the integer IDs for the
# cohorts and concept sets you'll be using.
#
# This script has been pre-populated with an example of how to retrieve the
# cohort and concept set definitions from the WebAPI.
#
# @seealso ROhdsiWebApi::exportCohortDefinitionSet
# @seealso ROhdsiWebApi::getConceptSetDefinition
# @seealso ROhdsiWebApi::resolveConceptSet
# @seealso ROhdsiWebApi::getConcepts
# =========================================================================

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from the WebAPI
# Analysis Specification: cohortDefinitions
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# This section defines the cohorts used in the analysis.
# Cohorts are retrieved from the WebAPI instance using their integer IDs.
# Analysis Specifications: cohortDefinitions
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for Strategus execution.
# It is a convention to use small integer IDs for cohorts inside a study.
# We map the WebAPI cohort IDs to a new set of IDs (1, 2, 3, ...).
# Analysis Specifications: cohortDefinitions
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# This section defines the negative control outcomes used for calibration.
# A concept set of negative controls is retrieved from the WebAPI.
# Analysis Specifications: negativeControlConceptSet
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Analysis Specifications: negativeControlConceptSet.id
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
  # Assign cohort IDs for negative controls, starting from 101 to avoid conflicts with study cohorts.
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)


# Verify that there are no duplicate cohort IDs between the main cohorts and negative controls.
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: This data frame lists the primary outcome(s) for the CohortMethod analysis.
# Analysis Specifications: cohortDefinitions.outcomeCohort
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Filter for outcome cohort (ID=3)
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName)

# Target and Comparator for the CohortMethod analysis
# This data frame defines the target-comparator pairs.
# Analysis Specifications: cohortDefinitions.targetCohort, cohortDefinitions.comparatorCohort
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# For the CohortMethod, you may want to exclude certain concepts from the covariates.
# The JSON specifications for covariateSelection.conceptsToExclude is empty.
# Thus, we create an empty data frame. The study drugs (target and comparator) are
# typically excluded automatically by CohortMethod.
# Analysis Specifications: covariateSelection.conceptsToExclude
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all
# This is left empty as per the specifications.
# Analysis Specifications: covariateSelection.conceptsToInclude
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# =========================================================================
# Part 3: Define the Analysis Modules
# =========================================================================
# In this section, we will create the R-objects that will be used to define
# the analysis modules that will be executed in this study.
#
# @seealso CohortGeneratorModule
# @seealso CohortDiagnosticsModule
# @seealso CohortMethodModule
# =========================================================================

# CohortGeneratorModule --------------------------------------------------------
# This module generates the cohorts specified in cohortDefinitionSet.
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

# CohortMethodModule -----------------------------------------------------------

# Define the study periods for the analysis.
# Analysis Specifications: getDbCohortMethodDataArgs.studyPeriods
studyPeriods <- tibble(
  studyStartDate = c(20171201),
  studyEndDate   = c(20231231)
)

# Define time-at-risk (TAR) windows.
# Analysis Specifications: createStudyPopArgs.timeAtRisks
timeAtRisks <- tibble(
  label = c("On Treatment"), # A descriptive label for the TAR
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
) 

# Define propensity score matching settings.
# Analysis Specifications: propensityScoreAdjustment.psSettings.matchOnPsArgs
matchOnPsArgsList <- tibble(
  label = c("1-to-1 Matching"), # A descriptive label
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
) 

# Define propensity score stratification settings.
# The specifications do not include stratification, so this data frame is empty.
# Analysis Specifications: propensityScoreAdjustment.psSettings.stratifyByPsArgs
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c(), # "all" | "target" | "comparator"
) 

# Build a single PS configuration list from the matching and stratification settings.
psConfigList <- list()

# Convert the matchOnPsArgsList data frame into a format suitable for the analysis loop.
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

# Convert the stratifyByPsArgsList data frame into a format suitable for the analysis loop.
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
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment arguments based on the method (matching or stratification).
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
      # Exclusions are handled automatically for study drugs.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE,
        excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
      )

      # Create a list of all outcomes, including the primary outcome and negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            # This lookback is used when checking for prior outcomes.
            # Analysis Specifications: createStudyPopArgs.priorOutcomeLookBack
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
      
      # Define the list of target-comparator-outcomes analyses.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorId[i],
          outcomes = outcomeList,
          # Excluded concepts for this specific T-C pair. Empty per specifications.
          excludedCovariateConceptIds = c()
        )
      }

      # Define arguments for retrieving data from the database.
      # Analysis Specifications: getDbCohortMethodDataArgs
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # Analysis Specifications: getDbCohortMethodDataArgs.maxCohortSize
        covariateSettings = covariateSettings
      )

      # Define arguments for creating the propensity score model.
      # Analysis Specifications: propensityScoreAdjustment.createPsArgs
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # Analysis Specifications: createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # Analysis Specifications: createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Let Strategus continue if a model fails to fit
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace", # Analysis Specifications: createPsArgs.prior.priorType
          exclude = c(0),
          useCrossValidation = TRUE # Analysis Specifications: createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", # Analysis Specifications: createPsArgs.control.noiseLevel
          cvType = "auto", # Analysis Specifications: createPsArgs.control.cvType
          seed = 1, 
          resetCoefficients = TRUE, # Analysis Specifications: createPsArgs.control.resetCoefficients
          tolerance = 2e-07, # Analysis Specifications: createPsArgs.control.tolerance
          cvRepetitions = 10, # Analysis Specifications: createPsArgs.control.cvRepetitions
          fold = 10, # Analysis Specifications: createPsArgs.control.fold
          startingVariance = 0.01 # Analysis Specifications: createPsArgs.control.startingVariance
        )
      )
      
      # Define arguments for computing covariate balance. Defaults are used.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )
      
      # Define arguments for fitting the outcome model.
      # Analysis Specifications: fitOutcomeModelArgs
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # Analysis Specifications: fitOutcomeModelArgs.modelType
        stratified = FALSE, # Analysis Specifications: fitOutcomeModelArgs.stratified
        useCovariates = FALSE, # Analysis Specifications: fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE, # Analysis Specifications: fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior(
          priorType = "laplace", # Analysis Specifications: fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE # Analysis Specifications: fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          cvType = "auto", # Analysis Specifications: fitOutcomeModelArgs.control.cvType
          seed = 1, 
          resetCoefficients = TRUE, # Analysis Specifications: fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01, # Analysis Specifications: fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07, # Analysis Specifications: fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10, # Analysis Specifications: fitOutcomeModelArgs.control.cvRepetitions
          fold = 10, # Analysis Specifications: fitOutcomeModelArgs.control.fold
          noiseLevel = "quiet" # Analysis Specifications: fitOutcomeModelArgs.control.noiseLevel
        )
      )
      
      # Define arguments for creating the study population.
      # Analysis Specifications: createStudyPopArgs
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # Analysis Specifications: createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE, # Analysis Specifications: createStudyPopArgs.firstExposureOnly
        washoutPeriod = 365, # Analysis Specifications: createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # Analysis Specifications: createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # Analysis Specifications: createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # Analysis Specifications: createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # Analysis Specifications: createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1, # Analysis Specifications: createStudyPopArgs.timeAtRisks.minDaysAtRisk
        maxDaysAtRisk = 99999
      )

      # Assemble the full analysis settings for this combination.
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

# Create the module specifications for CohortMethod.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# =========================================================================
# Part 4: Create and save the Analysis Specification
# =========================================================================
# In this section, we will create the analysis specification by combining
# the modules defined in Part 3.
#
# @seealso Strategus::createEmptyAnalysisSpecifications
# @seealso Strategus::addSharedResources
# @seealso Strategus::addModuleSpecifications
# =========================================================================
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The 'name' from the specifications is used for the folder and file name.
# Analysis Specifications: name
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "semaglutideandnaion", "semaglutideandnaionAnalysisSpecification.json")
)