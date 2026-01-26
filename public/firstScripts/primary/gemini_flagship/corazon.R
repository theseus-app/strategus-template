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
# It is based on the settings provided in the <Analysis Specifications> section.
# The study name is "corazon".

# Shared Resources -------------------------------------------------------------
# This section defines resources that are shared across different modules,
# such as cohort definitions and concept sets.

# The baseUrl for the WebAPI instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Here we retrieve the cohort definitions from ATLAS using their IDs.
# These IDs correspond to the "cohortDefinitions" section in the JSON specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1 from "cohortDefinitions.targetCohort"
    1794132, # Comparator: comparator1 from "cohortDefinitions.comparatorCohort"
    1794131  # Outcome: outcome1 from "cohortDefinitions.outcomeCohort"
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal consistency within the Strategus framework.
# The study code will refer to these cohorts using the new IDs (1, 2, 3).
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# We retrieve the concept set for negative controls, resolve it to a list of concepts,
# and format it as a cohort set. This corresponds to the "negativeControlConceptSet"
# section in the JSON specifications.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # from "negativeControlConceptSet.id"
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
  # Assign unique cohort IDs to negative controls, starting from 101 to avoid conflicts.
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)


if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: Define the primary outcome(s) for the analysis.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Corresponds to the re-numbered outcome cohort
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName)

# Target and Comparator for the CohortMethod analysis 
# This data frame defines the target-comparator pairs to be analyzed.
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered target cohort
  targetCohortName = "target1", # from "cohortDefinitions.targetCohort.name"
  comparatorCohortId = 2, # Re-numbered comparator cohort
  comparatorCohortName = "comparator1" # from "cohortDefinitions.comparatorCohort.name"
)

# Covariate selection: Define concepts to be excluded from the covariate construction.
# The "covariateSelection.conceptsToExclude" section in the JSON is empty,
# so this data frame is initialized as empty.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all
# The "covariateSelection.conceptsToInclude" section in the JSON is empty.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# This module is responsible for generating the cohorts defined above.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Define the cohort definitions as a shared resource.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Define the negative control outcome cohorts as a shared resource.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
# Create the specifications for the CohortGenerator module.
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
# This module performs the comparative cohort analysis.

# Define the study period for the analysis.
# Corresponds to "getDbCohortMethodDataArgs.studyPeriods" in the JSON.
studyPeriods <- tibble(
  studyStartDate = c("20100101"), # from "studyPeriods[0].studyStartDate"
  studyEndDate   = c("20191231")  # from "studyPeriods[0].studyEndDate"
)

# Time-at-risks (TARs) for the outcomes of interest in your study.
# Corresponds to "createStudyPopArgs.timeAtRisks" in the JSON.
timeAtRisks <- tibble(
  label = c("1-day-start to 0-day-end"), # A descriptive label for this TAR
  riskWindowStart  = c(1),       # from "timeAtRisks[0].riskWindowStart"
  startAnchor = c("cohort start"), # from "timeAtRisks[0].startAnchor"
  riskWindowEnd  = c(0),       # from "timeAtRisks[0].riskWindowEnd"
  endAnchor = c("cohort end")  # from "timeAtRisks[0].endAnchor"
) 

# Propensity Score settings - match on PS
# The "propensityScoreAdjustment.psSettings" in the JSON does not include matching,
# so this data frame is left empty.
matchOnPsArgsList <- tibble(
  label = c(),
  maxRatio  = c(),
  caliper = c(),
  caliperScale  = c()
) 

# Propensity Score settings - stratify by PS
# Corresponds to "propensityScoreAdjustment.psSettings[0].stratifyByPsArgs" in the JSON.
stratifyByPsArgsList <- tibble(
  label = c("5 Strata"), # A descriptive label for this PS adjustment method
  numberOfStrata  = c(5), # from "stratifyByPsArgs.numberOfStrata"
  baseSelection = c("all"), # from "stratifyByPsArgs.baseSelection"
) 

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# If a data frame for "match on PS" exists and has rows, convert each row to a config
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

# If a data frame for "stratify by PS" exists and has rows, convert each row to a config
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
      
      # Based on the method defined in psConfigList, create the appropriate PS adjustment arguments.
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

      # Use default covariate settings. The JSON spec for included/excluded concepts is empty.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Combine the main outcome(s) and the negative control outcomes.
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
      
      # Define the list of target-comparator-outcomes analyses.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Excluded concepts are based on the empty data frame defined earlier.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Settings for extracting data from the database.
      # Corresponds to "getDbCohortMethodDataArgs" in the JSON.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # from "getDbCohortMethodDataArgs.maxCohortSize"
        covariateSettings = covariateSettings
      )

      # Settings for creating the study population.
      # Corresponds to "createStudyPopArgs" in the JSON.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # from "createStudyPopArgs.restrictToCommonPeriod"
        firstExposureOnly = FALSE, # from "createStudyPopArgs.firstExposureOnly"
        washoutPeriod = 0, # from "createStudyPopArgs.washoutPeriod"
        removeDuplicateSubjects = "keep all", # from "createStudyPopArgs.removeDuplicateSubjects"
        censorAtNewRiskWindow = FALSE, # from "createStudyPopArgs.censorAtNewRiskWindow"
        removeSubjectsWithPriorOutcome = TRUE, # from "createStudyPopArgs.removeSubjectsWithPriorOutcome"
        priorOutcomeLookback = 99999, # from "createStudyPopArgs.priorOutcomeLookBack"
        # TAR settings are taken from the loop variables.
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1, # from "timeAtRisks[0].minDaysAtRisk"
        maxDaysAtRisk = 99999 # Default value
      )

      # Settings for creating the propensity score model.
      # Corresponds to "propensityScoreAdjustment.createPsArgs" in the JSON.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # from "createPsArgs.maxCohortSizeForFitting"
        errorOnHighCorrelation = TRUE, # from "createPsArgs.errorOnHighCorrelation"
        stopOnError = FALSE, # Setting to FALSE allows Strategus to complete all operations
        prior = Cyclops::createPrior(
          priorType = "laplace", # from "createPsArgs.prior.priorType"
          useCrossValidation = TRUE # from "createPsArgs.prior.useCrossValidation"
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", # from "createPsArgs.control.noiseLevel"
          cvType = "auto", # from "createPsArgs.control.cvType"
          seed = 1, # Default value
          resetCoefficients = TRUE, # from "createPsArgs.control.resetCoefficients"
          tolerance = 2e-07, # from "createPsArgs.control.tolerance"
          cvRepetitions = 10, # from "createPsArgs.control.cvRepetitions"
          startingVariance = 0.01, # from "createPsArgs.control.startingVariance"
          fold = 10 # from "createPsArgs.control.fold"
        )
      )

      # Settings for computing covariate balance.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Settings for fitting the outcome model.
      # Corresponds to "fitOutcomeModelArgs" in the JSON.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # from "fitOutcomeModelArgs.modelType"
        stratified = TRUE, # from "fitOutcomeModelArgs.stratified"
        useCovariates = FALSE, # from "fitOutcomeModelArgs.useCovariates"
        inversePtWeighting = FALSE, # from "fitOutcomeModelArgs.inversePtWeighting"
        prior = Cyclops::createPrior(
          priorType = "laplace", # from "fitOutcomeModelArgs.prior.priorType"
          useCrossValidation = TRUE # from "fitOutcomeModelArgs.prior.useCrossValidation"
        ),
        control = Cyclops::createControl(
          cvType = "auto", # from "fitOutcomeModelArgs.control.cvType"
          seed = 1, # Default value
          resetCoefficients = TRUE, # from "fitOutcomeModelArgs.control.resetCoefficients"
          startingVariance = 0.01, # from "fitOutcomeModelArgs.control.startingVariance"

          tolerance = 2e-07, # from "fitOutcomeModelArgs.control.tolerance"
          cvRepetitions = 10, # from "fitOutcomeModelArgs.control.cvRepetitions"
          noiseLevel = "quiet", # from "fitOutcomeModelArgs.control.noiseLevel"
          fold = 10 # from "fitOutcomeModelArgs.control.fold"
        )
      )

      # Append the settings to the Analysis List
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

# Create the analysis specifications ------------------------------------------
# This combines all the module specifications into a single analysis specification object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Add shared resources
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  # Add module specifications
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file name is based on the study name "corazon" from the JSON specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "corazon", "corazonAnalysisSpecification.json")
)