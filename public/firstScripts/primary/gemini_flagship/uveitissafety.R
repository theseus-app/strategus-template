################################################################################
# This script uses the OHDSI Strategus package to create a complete analysis
# specification based on the settings provided in the accompanying JSON file.
#
# See the "Create analysis specifications" section
# of the UsingThisTemplate.md for more details.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# provides further details on the function arguments.
# ##############################################################################
library(dplyr)
library(Strategus)

# =========== Shared Resources Section ===========
# This section defines the cohorts and concept sets that are used across the
# various analysis modules. These are defined once and then referenced by the
# modules.

# --- WebAPI Connection ---
# The baseUrl for the WebAPI instance where the cohort and concept set
# definitions are stored.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# --- Cohort Definitions ---
# A tibble/data.frame of cohort definitions to be imported from the WebAPI.
# The `cohortIds` should correspond to the cohort definition IDs in ATLAS.
# The `generateStats` parameter instructs CohortGenerator to generate
# characterization statistics for these cohorts.
#
# From Analysis Specifications: cohortDefinitions
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# --- Cohort ID Re-mapping ---
# Strategus uses its own internal cohort IDs (typically starting from 1).
# We remap the ATLAS cohort IDs to these new sequential IDs. It's crucial
# to use these new IDs throughout the rest of the script.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Target: target1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Comparator: comparator1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Outcome: outcome1

# --- Negative Control Outcome Cohorts ---
# This section defines the negative control outcomes. These are typically
# generated from a concept set of conditions or procedures that are not expected
# to be caused by the exposure.
#
# From Analysis Specifications: negativeControlConceptSet
negativeControlConceptSetId <- 1888110

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
  # Assign cohort IDs starting from 101 to avoid collision with manually defined cohorts
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)


# --- Data Frames for Analysis Settings ---
# These data frames are used to configure the target-comparator-outcome lists
# for the CohortMethod analysis.

# Outcomes of Interest list
# From Analysis Specifications: cohortDefinitions.outcomeCohort
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Using re-mapped cohort ID for outcome1
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # cleanWindow is not in the JSON but is a common setting

# Target and Comparator list for CohortMethod
# NOTE: You must replace the placeholder targetConceptId and comparatorConceptId
# with the actual drug concept IDs for your target and comparator cohorts. These
# are used to ensure the exposures themselves are excluded from the covariates.
#
# From Analysis Specifications: cohortDefinitions.targetCohort & cohortDefinitions.comparatorCohort
cmTcList <- data.frame(
  targetCohortId = 1, # re-mapped ID for target1
  targetCohortName = "target1",
  targetConceptId = -1, # <-- IMPORTANT: REPLACE WITH ACTUAL DRUG CONCEPT ID FOR target1
  comparatorCohortId = 2, # re-mapped ID for comparator1
  comparatorCohortName = "comparator1",
  comparatorConceptId = -1 # <-- IMPORTANT: REPLACE WITH ACTUAL DRUG CONCEPT ID FOR comparator1
)

# Covariates to exclude from the analysis.
# The target and comparator drug concepts are excluded by default within the
# createTargetComparatorOutcomes function. This list is for any *additional*
# concepts you wish to exclude. The provided specifications had this as empty.
#
# From Analysis Specifications: covariateSelection.conceptsToExclude
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# =========== CohortGeneratorModule Settings ===========
# This module is responsible for instantiating the cohort definitions against
# the CDM data source.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Defines the set of cohorts to be generated
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Defines the negative control outcome cohorts to be generated
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
# Specifies module-level settings, such as whether to generate stats.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# =========== CohortDiagnosticsModule Settings ===========
# This module runs a standard set of cohort characterization and diagnostics
# on the generated cohorts.
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

# =========== CohortMethodModule Settings ===========
# This module executes the comparative cohort study to estimate treatment effects.

# --- Analysis Settings Parameterization ---
# These tibbles define the parameter space for the study. The script will
# iterate through all combinations of these settings to create a list of
# analyses to be executed.

# Study periods: Defines overall study start and end dates.
# Empty strings indicate no date restrictions.
# From Analysis Specifications: getDbCohortMethodDataArgs.studyPeriods (null start/end date)
studyPeriods <- tibble(
  studyStartDate = c(""), # YYYYMMDD format, empty for no restriction
  studyEndDate   = c("")  # YYYYMMDD format, empty for no restriction
)

# Time-at-risks (TARs) for the outcomes of interest.
# From Analysis Specifications: createStudyPopArgs.timeAtRisks
timeAtRisks <- tibble(
  label = "1d_post_index_to_end_of_exposure",
  riskWindowStart  = c(1),              # From riskWindowStart
  startAnchor = c("cohort start"),      # From startAnchor
  riskWindowEnd  = c(0),                # From riskWindowEnd
  endAnchor = c("cohort end")           # From endAnchor
  # Note: minDaysAtRisk is set later in createCreateStudyPopulationArgs
)

# Propensity Score settings - Matching on Propensity Score.
# From Analysis Specifications: propensityScoreAdjustment.psSettings
matchOnPsArgsList <- tibble(
  label = "1_to_10_matching",
  maxRatio  = c(10),                      # From maxRatio
  caliper = c(0.2),                     # From caliper
  caliperScale  = c("standardized logit") # From caliperScale
)

# Propensity Score settings - Stratification on Propensity Score.
# The specifications did not include stratification settings.
# From Analysis Specifications: propensityScoreAdjustment.psSettings (stratifyByPsArgs is null)
stratifyByPsArgsList <- tibble()

# --- Dynamic Generation of Propensity Score Configurations ---
# This code converts the tibbles above into a list of PS configurations
# that the main analysis loop can iterate over.
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


# --- Main Analysis Loop ---
# Iterates through all combinations of study periods, TARs, and PS settings
# to create the full set of CohortMethod analyses.
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Conditionally create matching or stratification arguments
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Standard covariate settings from FeatureExtraction
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create the full list of outcomes, combining the outcomes of interest
      # and the negative control outcomes.
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
      
      # Combine targets, comparators, and outcomes into a single list structure
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Excludes the concept IDs for the T/C drugs themselves, plus any
          # additional concepts defined earlier.
          excludedCovariateConceptIds = c(
            cmTcList$targetConceptId[i],
            cmTcList$comparatorConceptId[i],
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # Define arguments for getting the initial data from the database.
      # From Analysis Specifications: getDbCohortMethodDataArgs
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From maxCohortSize and getDbCohortMethodDataArgs.maxCohortSize
        covariateSettings = covariateSettings
      )

      # Define arguments for creating the study population.
      # From Analysis Specifications: createStudyPopArgs
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE,          # From restrictToCommonPeriod
        firstExposureOnly = TRUE,               # From firstExposureOnly
        washoutPeriod = 365,                    # From washoutPeriod
        removeDuplicateSubjects = "keep all",   # From removeDuplicateSubjects
        censorAtNewRiskWindow = TRUE,           # From censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE,  # From removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999,           # From priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1                       # From timeAtRisks.minDaysAtRisk
      )

      # Define arguments for creating the propensity score model.
      # From Analysis Specifications: propensityScoreAdjustment.createPsArgs
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,   # From maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE,      # From errorOnHighCorrelation
        stopOnError = FALSE,
        prior = Cyclops::createPrior(
          priorType = "laplace",            # From prior.priorType
          useCrossValidation = TRUE         # From prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          tolerance = 2e-7,                 # From control.tolerance
          cvType = "auto",                  # From control.cvType
          fold = 10,                        # From control.fold
          cvRepetitions = 10,               # From control.cvRepetitions
          noiseLevel = "silent",            # From control.noiseLevel
          resetCoefficients = TRUE,         # From control.resetCoefficients
          startingVariance = 0.01           # From control.startingVariance
        )
      )

      # Arguments for covariate balance calculation (shared across adjusted populations)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000
      )
      # Arguments for covariate balance calculation (specific to this adjusted population)
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000
      )

      # Define arguments for fitting the outcome model.
      # From Analysis Specifications: fitOutcomeModelArgs
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",                  # From modelType
        stratified = TRUE,                  # From stratified (TRUE because we are matching)
        useCovariates = FALSE,              # From useCovariates
        inversePtWeighting = FALSE,         # From inversePtWeighting
        prior = Cyclops::createPrior(
          priorType = "laplace",            # From prior.priorType
          useCrossValidation = TRUE         # From prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          tolerance = 2e-7,                 # From control.tolerance
          cvType = "auto",                  # From control.cvType
          fold = 10,                        # From control.fold
          cvRepetitions = 10,               # From control.cvRepetitions
          noiseLevel = "quiet",             # From control.noiseLevel
          resetCoefficients = TRUE,         # From control.resetCoefficients
          startingVariance = 0.01           # From control.startingVariance
        )
      )

      # Assemble all the arguments into a single cmAnalysis object.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "T/C: %s vs %s; TAR: %s; PS: %s",
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

# --- Create the final CohortMethod module specifications ---
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE
)

# =========== Create the Analysis Specifications JSON ===========
# This final step combines all the module specifications into a single
# analysis specification object, which is then saved as a JSON file.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Add shared resources
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  # Add module specifications
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the specifications to a file. This file will be used as input for
# Strategus::execute().
# From Analysis Specifications: name
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("uveitissafetyAnalysisSpecification.json")
)