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

# This script creates the analysis specifications for a Strategus study.
# It is based on the settings provided in the <Analysis Specifications> JSON.
# Each section is annotated to explain how the JSON settings are applied.

# Shared Resources -------------------------------------------------------------
# This section defines resources that are shared across different analysis modules,
# such as cohort definitions and concept sets.

# The baseUrl for the WebAPI instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Here we retrieve the cohort definitions from ATLAS using their IDs.
# These IDs correspond to the "cohortDefinitions" section of the JSON.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: "target1" from JSON
    1794132, # Comparator: "comparator1" from JSON
    1794131  # Outcome: "outcome1" from JSON
  ),
  generateStats = TRUE
)

# Re-numbering cohorts for internal use within the study.
# This mapping ensures consistency regardless of the original ATLAS IDs.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Target
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Comparator
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Outcome

# Negative control outcomes
# We retrieve the concept set for negative controls specified in the JSON
# under "negativeControlConceptSet".
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # "id" from "negativeControlConceptSet"
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
  # Assign unique cohort IDs to negative controls, starting from 101
  # to avoid collision with the main study cohorts (1, 2, 3).
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)


# A check to ensure there are no duplicate cohort IDs between the main cohorts
# and the negative control cohorts.
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts for different analysis parts ---------------

# Outcomes list for CohortMethod.
# This corresponds to the "outcomeCohort" array in the JSON.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Using internal ID for "outcome1"
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window, not specified in JSON

# Target and Comparator list for the CohortMethod analysis.
# This corresponds to "targetCohort" and "comparatorCohort" in the JSON.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  # IMPORTANT: The concept IDs for the T and C drugs must be specified here
  # to ensure they are excluded from the covariate analysis.
  # These were not provided in the JSON, so placeholders are used.
  targetConceptId = 99999999, # Replace with actual target drug concept ID
  comparatorConceptId = 99999998 # Replace with actual comparator drug concept ID
)

# The "covariateSelection" in the JSON is empty, meaning we use default covariates.
# It is standard practice to exclude the target and comparator drugs themselves,
# which is handled in the CohortMethod analysis creation below.
# The following data frames are commented out as they are not needed based on the JSON.
#
# excludedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )
#
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# This module is responsible for generating the cohort instances on the CDM.
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
# Settings are standard and not specified in the JSON.
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
# This module performs the comparative cohort analysis.

# Study periods are defined from the "studyPeriods" array in the JSON.
studyPeriods <- tibble(
  studyStartDate = c("20171201"), # "studyStartDate" from JSON
  studyEndDate   = c("20231231")  # "studyEndDate" from JSON
)

# Time-at-risks (TARs) are defined from the "timeAtRisks" array in "createStudyPopArgs".
timeAtRisks <- tibble(
  label = c("On Treatment"), # A descriptive label for this TAR
  riskWindowStart  = c(1),             # "riskWindowStart" from JSON
  startAnchor = c("cohort start"),   # "startAnchor" from JSON
  riskWindowEnd  = c(0),             # "riskWindowEnd" from JSON
  endAnchor = c("cohort end")      # "endAnchor" from JSON
) 

# Propensity Score settings for matching, from "psSettings" in the JSON.
matchOnPsArgsList <- tibble(
  label = c("1-to-1 Matching"), # A descriptive label
  maxRatio  = c(1),                  # "maxRatio" from JSON
  caliper = c(0.2),                # "caliper" from JSON
  caliperScale  = c("standardized logit") # "caliperScale" from JSON
) 

# Propensity Score settings for stratification, from "psSettings" in the JSON.
stratifyByPsArgsList <- tibble(
  label = c("5 Strata"), # A descriptive label
  numberOfStrata  = c(5),      # "numberOfStrata" from JSON
  baseSelection = c("all"),  # "baseSelection" from JSON
) 

# Build a single list of all PS configurations to iterate over.
psConfigList <- list()

# Convert the matching data frame to a configuration list.
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

# Convert the stratification data frame to a configuration list.
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
      
      # Configure either matching or stratification based on the current PS config.
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

      # Use default covariate settings as "covariateSelection" in JSON is empty.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Combine the main outcome of interest with the negative control outcomes.
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
          # Exclude the T & C drug concepts from covariates.
          excludedCovariateConceptIds = c(
            cmTcList$targetConceptId[i], 
            cmTcList$comparatorConceptId[i]
          )
        )
      }

      # Settings for fetching data from the database.
      # Based on "getDbCohortMethodDataArgs" in the JSON.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        restrictToCommonPeriod = TRUE,      # "restrictToCommonPeriod" from JSON
        firstExposureOnly = FALSE,          # "firstExposureOnly" from JSON
        removeDuplicateSubjects = "keep all", # "removeDuplicateSubjects" from JSON
        washoutPeriod = 0,                  # "washoutPeriod" from JSON
        maxCohortSize = 0,                  # "maxCohortSize" from JSON
        covariateSettings = covariateSettings
      )

      # Settings for creating the propensity score model.
      # Based on "createPsArgs" in "propensityScoreAdjustment" in the JSON.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # "maxCohortSizeForFitting" from JSON
        errorOnHighCorrelation = TRUE,    # "errorOnHighCorrelation" from JSON
        stopOnError = FALSE,
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",        # "prior.priorType" from JSON
          useCrossValidation = TRUE     # "prior.useCrossValidation" from JSON
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",        # "control.noiseLevel" from JSON
          cvType = "auto",              # "control.cvType" from JSON
          fold = 10,                    # "control.fold" from JSON
          cvRepetitions = 10,           # "control.cvRepetitions" from JSON
          tolerance = 2e-07,            # "control.tolerance" from JSON
          resetCoefficients = TRUE,     # "control.resetCoefficients" from JSON
          startingVariance = 0.01       # "control.startingVariance" from JSON
        )
      )

      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Settings for fitting the outcome model.
      # Based on "fitOutcomeModelArgs" in the JSON.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",              # "modelType" from JSON
        stratified = TRUE,              # "stratified" from JSON
        useCovariates = FALSE,          # "useCovariates" from JSON
        inversePtWeighting = FALSE,     # "inversePtWeighting" from JSON
        prior = Cyclops::createPrior(
          priorType = "laplace",        # "prior.priorType" from JSON
          useCrossValidation = TRUE     # "prior.useCrossValidation" from JSON
        ),
        control = Cyclops::createControl(
          noiseLevel = "quiet",         # "control.noiseLevel" from JSON
          cvType = "auto",              # "control.cvType" from JSON
          fold = 10,                    # "control.fold" from JSON
          cvRepetitions = 10,           # "control.cvRepetitions" from JSON
          tolerance = 2e-07,            # "control.tolerance" from JSON
          resetCoefficients = TRUE,     # "control.resetCoefficients" from JSON
          startingVariance = 0.01       # "control.startingVariance" from JSON
        )
      )
      
      # Settings for creating the study population.
      # Based on "createStudyPopArgs" in the JSON.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,       # "restrictToCommonPeriod" from JSON
        firstExposureOnly = FALSE,            # "firstExposureOnly" from JSON
        washoutPeriod = 0,                    # "washoutPeriod" from JSON
        removeDuplicateSubjects = "keep all", # "removeDuplicateSubjects" from JSON
        censorAtNewRiskWindow = TRUE,         # "censorAtNewRiskWindow" from JSON
        removeSubjectsWithPriorOutcome = TRUE,# "removeSubjectsWithPriorOutcome" from JSON
        priorOutcomeLookback = 99999,         # "priorOutcomeLookBack" from JSON
        minDaysAtRisk = 1,                    # "timeAtRisks.minDaysAtRisk" from JSON
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t]
      )


      # Append the complete analysis settings to the list.
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

# Create the CohortMethod module specifications.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the final analysis specifications object --------------------------------
# This combines all shared resources and module specifications into a single object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file name is based on the "name" field in the JSON.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "semaglutideandnaion", "semaglutideandnaionAnalysisSpecification.json")
)