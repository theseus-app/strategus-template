################################################################################
# This script uses the OHDSI Strategus package to create a study analysis
# specification based on the settings provided in the accompanying JSON file.
#
# See the Create analysis specifications section of the UsingThisTemplate.md
# for more details.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# provides further details.
################################################################################
library(dplyr)
library(Strategus)

# ===========-===================================================================
#
#           SHARED RESOURCES
#
# ===============================================================================
# In this section, we define the cohorts and concept sets that will be used
# throughout the analysis. These are considered "shared resources" as they can
# be used by multiple modules.

# The baseUrl for the WebApi instance.
# NOTE: This script uses the public OHDSI demo Atlas instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# --- Cohort Definitions -------------------------------------------------------
# We will export the cohort definitions from Atlas and load them into a
# data frame. The cohort IDs are taken from the "cohortDefinitions" section
# of the analysis specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# To simplify referencing these cohorts in the analysis, we will re-number
# them with simple, sequential IDs.
# Target cohort (ID: 1794126) will be re-assigned to ID 1.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
# Comparator cohort (ID: 1794132) will be re-assigned to ID 2.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
# Outcome cohort (ID: 1794131) will be re-assigned to ID 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# --- Negative Control Outcomes ------------------------------------------------
# We define a set of negative control outcomes using a concept set from Atlas.
# These are outcomes that are not believed to be caused by the exposure.
# The concept set ID is taken from the "negativeControlConceptSet" section.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Concept Set: "negative"
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(
    baseUrl = baseUrl
  ) %>%
  ROhdsiWebApi::getConcepts(
    baseUrl = baseUrl
  ) %>%
  # Rename columns to match the required format for Strategus
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  # Assign unique cohort IDs, starting from 101 to avoid conflicts with other cohorts
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# --- Data Frames for Analysis Settings ----------------------------------------
# These data frames help organize the various combinations of settings that will
# be used to construct the full analysis specification.

# Outcomes of interest for the study.
# This includes the primary outcome cohort defined above.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Filter for the outcome cohort (ID 3)
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName)

# Target and Comparator cohorts for the CohortMethod analysis.
# We specify the T, C, and the concept IDs for the drugs themselves, which
# will be excluded from the covariates.
# NOTE: The concept IDs for the target and comparator drugs are placeholders
# and should be replaced with the actual ingredient concept IDs.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  # Placeholder concept IDs for the T & C drugs for covariate exclusion
  targetConceptId = 1111111,
  comparatorConceptId = 2222222
)

# Covariate concepts to exclude from the analysis.
# The specifications indicate no additional concepts to exclude, so we create an empty data frame.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# Optional: Covariate concepts to include in the analysis.
# The specifications indicate no specific concepts to include, so this is commented out.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# ===============================================================================
#
#           MODULE SPECIFICATIONS
#
# ===============================================================================
# In this section, we define the settings for each Strategus module that will
# be part of the analysis.

# --- CohortGeneratorModule ----------------------------------------------------
# This module is responsible for generating the cohorts defined in the shared
# resources section.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Define the cohort definitions as a shared resource
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Define the negative control outcome cohorts as a shared resource
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
# Create the module specifications for CohortGenerator
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# --- CohortDiagnosticsModule --------------------------------------------------
# This module runs a set of diagnostics on the generated cohorts to assess their
# quality and characteristics.
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

# --- CohortMethodModule -------------------------------------------------------
# This module performs the comparative cohort analysis (estimation study).

# Study periods: Defines the calendar time range for the study.
# The specifications have null start/end dates, meaning the study is not
# restricted by calendar time. We use empty strings to signify this.
studyPeriods <- tibble(
  studyStartDate = c(""), # YYYYMMDD format or "" for no restriction
  studyEndDate   = c("")  # YYYYMMDD format or "" for no restriction
)

# Time-at-risks (TARs): Defines the period after cohort entry during which
# outcomes are counted. Based on "createStudyPopArgs:timeAtRisks".
timeAtRisks <- tibble(
  label = "On Treatment",
  # riskWindowStart: 1 day after cohort start
  riskWindowStart  = 1,
  startAnchor = "cohort start",
  # riskWindowEnd: 0 days after cohort end
  riskWindowEnd  = 0,
  endAnchor = "cohort end",
  # minDaysAtRisk: 1 day, as specified
  minDaysAtRisk = 1
)

# Propensity Score (PS) settings for matching.
# Based on "propensityScoreAdjustment:psSettings:matchOnPsArgs".
matchOnPsArgsList <- tibble(
  label = "1-to-1 Matching",
  # maxRatio: 1, for one-to-one matching
  maxRatio  = 1,
  # caliper: 0.2, as specified
  caliper = 0.2,
  # caliperScale: "standardized logit", as specified
  caliperScale  = "standardized logit"
)

# Propensity Score (PS) settings for stratification.
# The specifications set "stratifyByPsArgs" to null, so this is empty.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c()
)

# Build a single list of all PS configurations to iterate over.
# The following logic converts the data frames above into a structured list.
psConfigList <- list()
# Convert the matching settings data frame into the config list format
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
# Convert the stratification settings data frame into the config list format
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


# --- Analysis Assembly Loop ---------------------------------------------------
# We will now iterate through all combinations of settings (study periods, TARs,
# PS methods) to create a list of analysis variants.
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Configure PS adjustment arguments based on the current iteration
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

      # Define covariate settings. We use the default settings from FeatureExtraction.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create a list of all outcomes, including the primary outcome and negative controls.
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

      # Define the target, comparator, and outcome combinations.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude the T & C drug concepts and any other specified concepts from covariates
          excludedCovariateConceptIds = c(
            cmTcList$targetConceptId[i],
            cmTcList$comparatorConceptId[i],
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # Define arguments for getting data from the database.
      # Based on "getDbCohortMethodDataArgs".
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        # maxCohortSize: 0 means no limit on cohort size
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Define arguments for creating the study population.
      # Based on "createStudyPopArgs".
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        # restrictToCommonPeriod: false, as specified
        restrictToCommonPeriod = FALSE,
        # firstExposureOnly: true, as specified
        firstExposureOnly = TRUE,
        # washoutPeriod: 365 days, as specified
        washoutPeriod = 365,
        # removeDuplicateSubjects: "keep all", as specified
        removeDuplicateSubjects = "keep all",
        # censorAtNewRiskWindow: false, as specified
        censorAtNewRiskWindow = FALSE,
        # removeSubjectsWithPriorOutcome: true, as specified
        removeSubjectsWithPriorOutcome = TRUE,
        # priorOutcomeLookBack: 99999 days (essentially infinite), as specified
        priorOutcomeLookback = 99999,
        # Time-at-risk settings from the `timeAtRisks` tibble
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999 # Default to no maximum
      )

      # Define arguments for creating the propensity score model.
      # Based on "propensityScoreAdjustment:createPsArgs".
      createPsArgs <- CohortMethod::createCreatePsArgs(
        # maxCohortSizeForFitting: 250000, as specified
        maxCohortSizeForFitting = 250000,
        # errorOnHighCorrelation: true, as specified
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,
        estimator = "att",
        # prior settings for regularization
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE,
          exclude = c(0) # Do not regularize the intercept
        ),
        # control settings for the Cyclops fitter
        control = Cyclops::createControl(
          noiseLevel = "silent",
          cvType = "auto",
          seed = 1,
          resetCoefficients = TRUE,
          tolerance = 2e-07,
          # cvRepetitions: 10, as specified
          cvRepetitions = 10,
          startingVariance = 0.01,
          # fold: 10, as specified
          fold = 10
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
      # Based on "fitOutcomeModelArgs".
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        # modelType: "cox", as specified
        modelType = "cox",
        # stratified: false, as specified
        stratified = FALSE,
        # useCovariates: false, as specified
        useCovariates = FALSE,
        # inversePtWeighting: false, as specified
        inversePtWeighting = FALSE,
        # prior settings for regularization
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE
        ),
        # control settings for the Cyclops fitter
        control = Cyclops::createControl(
          noiseLevel = "quiet",
          cvType = "auto",
          seed = 1,
          resetCoefficients = TRUE,
          tolerance = 2e-07,
          # cvRepetitions: 10, as specified
          cvRepetitions = 10,
          startingVariance = 0.01,
          # fold: 10, as specified
          fold = 10
        )
      )

      # Append the complete set of analysis settings to the list.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "T: %s, C: %s, TAR: %s, PS: %s",
          cmTcList$targetCohortName[1],
          cmTcList$comparatorCohortName[1],
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

# Create the final CohortMethod module specifications.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# ===============================================================================
#
#           CREATE ANALYSIS SPECIFICATIONS
#
# ===============================================================================
# In this final section, we bring all the module specifications together into a
# single analysis specification object.

analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Add the shared resources (cohorts, concept sets)
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  # Add the module specifications
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file name is based on the "name" field in the analysis specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("antivegfkidneyAnalysisSpecification.json")
)