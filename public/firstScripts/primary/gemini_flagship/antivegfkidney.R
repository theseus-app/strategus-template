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

# ===========-===================================================================
#
# Part 1: Define shared resources
#
# ===========-===================================================================

# Shared Resources -------------------------------------------------------------
# This section defines the cohorts and concept sets that will be used across the
# various modules of the study.

# The baseUrl for the WebApi instance. This is used to retrieve cohort and
# concept set definitions.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Retrieve cohort definitions from WebAPI and build a cohortDefinitionSet data frame.
# We also re-assign cohort IDs to a local, consecutive sequence (1, 2, 3, ...)
# to make them easier to reference in the analysis specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-numbering cohort IDs for local study use.
# Target cohort will be ID 1.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
# Comparator cohort will be ID 2.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
# Outcome cohort will be ID 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3


# Negative Control Outcomes ----------------------------------------------------
# Retrieve the negative control concept set, resolve it into a list of concepts,
# and format it as a cohort set. Each concept will become a negative control
# outcome cohort.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Corresponds to "negative" concept set
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(
    baseUrl = baseUrl
  ) %>%
  ROhdsiWebApi::getConcepts(
    baseUrl = baseUrl
  ) %>%
  # Format the concept set into a data frame suitable for Strategus
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  # Assign cohort IDs starting from 101 to avoid collision with main cohorts
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)

# Safety check to ensure no cohort IDs are accidentally duplicated.
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Analysis-specific data frames ------------------------------------------------
# These data frames are used to configure the CohortMethod analysis by defining
# the target-comparator-outcome relationships.

# Define the outcomes of interest for the study.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Filter for the outcome cohort (ID 3)
  mutate(outcomeCohortId = cohortId, 
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # cleanWindow is not used in this specific analysis but is kept for template consistency.
  mutate(cleanWindow = 365)

# Define the target and comparator cohorts for the CohortMethod analysis.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  # NOTE: The target and comparator concept IDs are required for automatic
  # exclusion from covariates. Since they are not in the specifications,
  # placeholders are used. These should be replaced with the actual drug concept IDs.
  targetConceptId = 0,
  comparatorConceptId = 0
)

# Define concepts to be excluded from the covariate analysis.
# The analysis specification indicates no additional concepts to exclude,
# so we create an empty data frame. The target and comparator concepts will be
# excluded automatically based on the cmTcList definition.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)


# ===========-===================================================================
#
# Part 2: Define modules
#
# ===========-===================================================================

# CohortGeneratorModule Settings -----------------------------------------------
# This module is responsible for generating the cohorts defined above.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Define the cohort definition set as a shared resource for CohortGenerator
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

# Define the negative control outcome cohort set as a shared resource.
# These cohorts are generated based on the first occurrence of the concept.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create the final module specifications for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Generate inclusion rule statistics
)


# CohortDiagnosticsModule Settings ---------------------------------------------
# This module runs diagnostics on the generated cohorts to assess their quality.
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
  # Set a minimum mean value for characterization features to be included in the output.
  minCharacterizationMean = 0.01 
)


# CohortMethodModule Settings --------------------------------------------------
# This module performs the comparative cohort analysis (e.g., propensity score
# matching and outcome modeling).

# Study Period Settings --------------------------------------------------------
# Define the overall study period. Empty strings ("") indicate no restriction on
# the start or end date.
studyPeriods <- tibble(
  studyStartDate = c(""), # YYYYMMDD, empty for no limit
  studyEndDate   = c("")  # YYYYMMDD, empty for no limit
)

# Time-at-Risk (TAR) Settings --------------------------------------------------
# Define the time-at-risk windows for the outcome analysis.
# This corresponds to the `timeAtRisks` section of the analysis specifications.
timeAtRisks <- tibble(
  label = c("On Treatment (1 day offset)"),
  riskWindowStart  = c(1),              # Corresponds to riskWindowStart: 1
  startAnchor = c("cohort start"),      # Corresponds to startAnchor: "cohort start"
  riskWindowEnd  = c(0),                # Corresponds to riskWindowEnd: 0
  endAnchor = c("cohort end")           # Corresponds to endAnchor: "cohort end"
) 

# Propensity Score (PS) Method Settings ----------------------------------------
# Define the PS adjustment strategies. Here, we specify one matching strategy.

# PS Matching settings.
# This corresponds to the `matchOnPsArgs` section of the analysis specifications.
matchOnPsArgsList <- tibble(
  label = c("1-to-1 matching"),
  maxRatio  = c(1),                       # Corresponds to maxRatio: 1
  caliper = c(0.2),                     # Corresponds to caliper: 0.2
  caliperScale  = c("standardized logit") # Corresponds to caliperScale: "standardized logit"
) 

# The analysis specifications do not include PS stratification, so this is left empty.
stratifyByPsArgsList <- tibble() 

# Build a single list of all PS configurations from the tibbles above.
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


# Create CohortMethod Analysis Settings ----------------------------------------
# This section iterates through the different analysis settings (study periods,
# TARs, PS methods) to create a list of `cmAnalysis` objects.

cmAnalysisList <- list()
analysisId <- 1

# Loop through each defined study period.
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  # Loop through each defined time-at-risk window.
  for (t in seq_len(nrow(timeAtRisks))) {

    # Loop through each defined propensity score adjustment method.
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS arguments based on whether it's matching or stratification.
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
      # The specification does not require custom feature engineering.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Combine the main outcomes of interest with the negative control outcomes.
      outcomeList <- append(
        # Main outcome(s)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA
          )
        }),
        # Negative control outcome(s)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Create the list of target-comparator-outcomes settings.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude the target and comparator drug concepts from the analysis.
          excludedCovariateConceptIds = c(
            cmTcList$targetConceptId[i], 
            cmTcList$comparatorConceptId[i],
            excludedCovariateConcepts$conceptId
          )
        )
      }
      
      # Settings for retrieving data from the database.
      # Corresponds to `getDbCohortMethodDataArgs`.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # No limit on cohort size, as per spec.
        covariateSettings = covariateSettings
      )
      
      # Settings for creating the study population.
      # Corresponds to `createStudyPopArgs`.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,      # Corresponds to spec
        firstExposureOnly = TRUE,            # Corresponds to spec
        washoutPeriod = 365,                 # Corresponds to spec
        removeDuplicateSubjects = "keep all",# Corresponds to spec
        censorAtNewRiskWindow = FALSE,       # Corresponds to spec
        removeSubjectsWithPriorOutcome = TRUE, # Corresponds to spec
        priorOutcomeLookback = 99999,        # Corresponds to spec
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,                   # Corresponds to spec
        maxDaysAtRisk = 99999                # Default: no maximum
      )
      
      # Settings for creating the propensity score model.
      # Corresponds to `createPsArgs`.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,   # Corresponds to spec
        errorOnHighCorrelation = TRUE,      # Corresponds to spec
        stopOnError = FALSE,                # Allow Strategus to continue if one model fails
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",            # Corresponds to spec
          useCrossValidation = TRUE         # Corresponds to spec
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",            # Corresponds to spec
          cvType = "auto",                  # Corresponds to spec
          seed = 1,                         # Set for reproducibility
          resetCoefficients = TRUE,         # Corresponds to spec
          tolerance = 2e-07,                # Corresponds to spec
          cvRepetitions = 10,               # Corresponds to spec
          fold = 10,                        # Corresponds to spec
          startingVariance = 0.01           # Corresponds to spec
        )
      )

      # Settings for calculating covariate balance (shared across T-C pairs).
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      
      # Settings for calculating covariate balance (for Table 1).
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Settings for fitting the outcome model.
      # Corresponds to `fitOutcomeModelArgs`.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",                  # Corresponds to spec
        stratified = FALSE,                 # Corresponds to spec (FALSE)
        useCovariates = FALSE,              # Corresponds to spec
        inversePtWeighting = FALSE,         # Corresponds to spec
        prior = Cyclops::createPrior(
          priorType = "laplace",            # Corresponds to spec
          useCrossValidation = TRUE         # Corresponds to spec
        ),
        control = Cyclops::createControl(
          noiseLevel = "quiet",             # Corresponds to spec
          cvType = "auto",                  # Corresponds to spec
          seed = 1,                         # Set for reproducibility
          resetCoefficients = TRUE,         # Corresponds to spec
          startingVariance = 0.01,          # Corresponds to spec
          tolerance = 2e-07,                # Corresponds to spec
          cvRepetitions = 10,               # Corresponds to spec
          fold = 10                         # Corresponds to spec
        )
      )
      
      # Combine all the settings into a single `cmAnalysis` object.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "No study period restriction; TAR: %s; PS: %s",
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

# Create the final module specifications for CohortMethod.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)


# ===========-===================================================================
#
# Part 3: Combine modules into a single analysis specification
#
# ===========-===================================================================

analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Add shared resources
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  # Add module specifications
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)


# Save the analysis specifications object to a JSON file in the inst folder.
# This file will be used by Strategus to execute the study.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "antivegfkidney", "antivegfkidneyAnalysisSpecification.json")
)