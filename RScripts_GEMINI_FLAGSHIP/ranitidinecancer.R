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

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from a WebAPI instance
# NOTE: This script will use the OHDSI Atlas demo instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions from <Analysis Specifications>.cohortDefinitions
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1 from cohortDefinitions.targetCohort
    1794132, # Comparator: comparator1 from cohortDefinitions.comparatorCohort
    1794131  # Outcome: outcome1 from cohortDefinitions.outcomeCohort
  ),
  generateStats = TRUE
)

# Re-number cohorts to a consistent 1, 2, 3... scheme for Strategus modules
# This is a good practice to avoid passing around large, arbitrary cohort IDs
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes from <Analysis Specifications>.negativeControlConceptSet
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Corresponds to negativeControlConceptSet.id
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
  # Start negative control cohort IDs from 101 to avoid conflicts with study cohorts (1, 2, 3)
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)


# Safety check for duplicate cohort IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: From <Analysis Specifications>.cohortDefinitions.outcomeCohort
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Filter for the re-numbered outcome cohort ID
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName)

# Target and Comparator for the CohortMethod analysis
# From <Analysis Specifications>.cohortDefinitions.targetCohort and .comparatorCohort
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate exclusion: from <Analysis Specifications>.covariateSelection.conceptsToExclude
# The specification for conceptsToExclude is empty, so this data frame is empty.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional: Covariate inclusion: from <Analysis Specifications>.covariateSelection.conceptsToInclude
# The specification for conceptsToInclude is empty, so this is commented out.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# This module generates the cohort sets defined above
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

# CohortDiagnoticsModule Settings ---------------------------------------------
# This module runs diagnostics on the generated cohorts
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

# Study periods from <Analysis Specifications>.getDbCohortMethodDataArgs.studyPeriods
# The specification defines null start and end dates, meaning the entire
# observational period is considered. We represent this with empty strings.
studyPeriods <- tibble(
  studyStartDate = c(""), # YYYYMMDD, empty for no restriction
  studyEndDate   = c("")  # YYYYMMDD, empty for no restriction
)

# Time-at-risks (TARs) for the outcomes of interest
# From <Analysis Specifications>.createStudyPopArgs.timeAtRisks
timeAtRisks <- tibble(
  label = c("Start 365d to 9999d"),
  riskWindowStart  = c(365),            # createStudyPopArgs.timeAtRisks.riskWindowStart
  startAnchor = c("cohort start"),   # createStudyPopArgs.timeAtRisks.startAnchor
  riskWindowEnd  = c(9999),            # createStudyPopArgs.timeAtRisks.riskWindowEnd
  endAnchor = c("cohort start")      # createStudyPopArgs.timeAtRisks.endAnchor
) 

# Propensity Score settings - match on PS
# From <Analysis Specifications>.propensityScoreAdjustment.psSettings
matchOnPsArgsList <- tibble(
  label = c("1-to-1 matching, 0.2 std logit caliper"),
  maxRatio  = c(1),                      # psSettings.matchOnPsArgs.maxRatio
  caliper = c(0.2),                    # psSettings.matchOnPsArgs.caliper
  caliperScale  = c("standardized logit") # psSettings.matchOnPsArgs.caliperScale
) 

# Propensity Score settings - stratify by PS
# From <Analysis Specifications>.propensityScoreAdjustment.psSettings
# The specification for stratifyByPsArgs is null, so this data frame is empty.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c(),
) 

# Build a single PS configuration list from the tibbles above
psConfigList <- list()

# Convert the "match on PS" tibble to a configuration list
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

# Convert the "stratify by PS" tibble to a configuration list (will be skipped in this case)
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


# Iterate through all analysis setting combinations to create the analysis list
cmAnalysisList <- list()
analysisId <- 1

# Note: The loops will only run once as there is one study period, one TAR, and one PS setting.
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Set up PS adjustment arguments based on the method
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

      # Default covariate settings are used as none are specified in covariateSelection
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create the list of outcomes, combining the main outcome and negative controls
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
      
      # Create the list of Target-Comparator-Outcomes
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Per spec, no concepts were specified for exclusion
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Settings for getting data from the database
      # From <Analysis Specifications>.getDbCohortMethodDataArgs
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # getDbCohortMethodDataArgs.maxCohortSize: 0 means no limit
        covariateSettings = covariateSettings
      )

      # Settings for creating the propensity score model
      # From <Analysis Specifications>.propensityScoreAdjustment.createPsArgs
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE,   # createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Setting to FALSE allows Strategus to complete all operations even if one model fails
        prior = Cyclops::createPrior(
          priorType = "laplace", # createPsArgs.prior.priorType
          useCrossValidation = TRUE # createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", # createPsArgs.control.noiseLevel
          cvType = "auto", # createPsArgs.control.cvType
          resetCoefficients = TRUE, # createPsArgs.control.resetCoefficients
          tolerance = 2e-7, # createPsArgs.control.tolerance
          cvRepetitions = 10, # createPsArgs.control.cvRepetitions
          startingVariance = 0.01, # createPsArgs.control.startingVariance
          fold = 10 # createPsArgs.control.fold
        )
      )

      # Balance computation settings (using defaults as not specified)
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )
      
      # Settings for fitting the outcome model
      # From <Analysis Specifications>.fitOutcomeModelArgs
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # fitOutcomeModelArgs.modelType
        stratified = FALSE, # fitOutcomeModelArgs.stratified
        useCovariates = FALSE, # fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE, # fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior(
          priorType = "laplace", # fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE # fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          cvType = "auto", # fitOutcomeModelArgs.control.cvType
          resetCoefficients = TRUE, # fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01, # fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-7, # fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10, # fitOutcomeModelArgs.control.cvRepetitions
          noiseLevel = "quiet", # fitOutcomeModelArgs.control.noiseLevel
          fold = 10 # fitOutcomeModelArgs.control.fold
        )
      )
      
      # Settings for creating the study population
      # From <Analysis Specifications>.createStudyPopArgs
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE, # createStudyPopArgs.firstExposureOnly
        washoutPeriod = 365, # createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 365, # createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t], # from timeAtRisks tibble
        startAnchor = timeAtRisks$startAnchor[t], # from timeAtRisks tibble
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # from timeAtRisks tibble
        endAnchor = timeAtRisks$endAnchor[t], # from timeAtRisks tibble
        minDaysAtRisk = 1, # createStudyPopArgs.timeAtRisks.minDaysAtRisk
        maxDaysAtRisk = 99999 # Using a large number for no maximum as it's not specified
      )


      # Append the fully specified analysis to the list
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "ranitidinecancer study; TAR: %s; PS: %s",
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

# Module creator for CohortMethod
cmModuleSettingsCreator <- CohortMethodModule$new()
# Create the module specification using the analysis list
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the final analysis specifications JSON object ---------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Add the shared resource definitions
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  # Add the module specifications
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications object to a file
# The output path and file name are derived from <Analysis Specifications>.name
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "ranitidinecancer", "ranitidinecancerAnalysisSpecification.json")
)