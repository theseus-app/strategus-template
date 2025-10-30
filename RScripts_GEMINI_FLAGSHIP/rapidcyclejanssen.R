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
# Get the list of cohorts
# Using a public ATLAS instance to retrieve cohort definitions
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions from <Analysis Specifications>.cohortDefinitions
# These are the cohorts that will be instantiated for the analysis
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to a local, consistent ID space (1, 2, 3, ...)
# This simplifies referencing them in the rest of the script
# targetCohort.id: 1794126 is mapped to 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# comparatorCohort.id: 1794132 is mapped to 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# outcomeCohort.id: 1794131 is mapped to 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes from <Analysis Specifications>.negativeControlConceptSet
# The concept set is resolved to a list of concepts, which are then used
# to generate negative control outcome cohorts.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # negativeControlConceptSet.id
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
  # Assign unique cohort IDs for the negative controls, starting from 101
  # to avoid conflicts with the main cohorts (1, 2, 3)
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)


# Sanity check to ensure no cohort IDs are accidentally duplicated
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the various cohort combinations for the analysis modules ---------------
# Outcomes list for CohortMethod, based on <Analysis Specifications>.cohortDefinitions.outcomeCohort
# This defines the primary outcome of interest for the study.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Filter for the re-numbered outcome cohort
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # cleanWindow is a parameter for outcome definition in CM, not in spec but a reasonable default
  mutate(cleanWindow = 365)

# Target and Comparator list for the CohortMethod analysis, from <Analysis Specifications>.cohortDefinitions
# This defines the main comparison for the study.
cmTcList <- data.frame(
  targetCohortId = 1, # Mapped from targetCohort.id: 1794126
  targetCohortName = "target1", # targetCohort.name
  comparatorCohortId = 2, # Mapped from comparatorCohort.id: 1794132
  comparatorCohortName = "comparator1" # comparatorCohort.name
)

# Covariate concepts to exclude from the analysis, from <Analysis Specifications>.covariateSelection.conceptsToExclude
# The specification is empty, so this data frame is empty.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all
# The specification for conceptsToInclude is empty, so this is not used.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# This module is responsible for creating the cohort instances in the database.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Defines the set of cohorts to be generated
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Defines the negative control outcome cohorts to be generated
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
# Specifies the settings for the CohortGenerator execution
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# This module runs a set of diagnostics on the instantiated cohorts.
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

# Define study periods from <Analysis Specifications>.getDbCohortMethodDataArgs.studyPeriods
# A null studyEndDate is represented as NA in R.
studyPeriods <- tibble(
  studyStartDate = c("20210101"), # studyStartDate: 20210101
  studyEndDate   = c(NA_character_)  # studyEndDate: null
)

# Define Time-at-risks (TARs) from <Analysis Specifications>.createStudyPopArgs.timeAtRisks
timeAtRisks <- tibble(
  label = c(
    "1-14d", "1-28d", "1-42d", "1-90d", "0-2d"
  ),
  # riskWindowStart from timeAtRisks array
  riskWindowStart  = c(1, 1, 1, 1, 0),
  # startAnchor from timeAtRisks array
  startAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start"),
  # riskWindowEnd from timeAtRisks array
  riskWindowEnd  = c(14, 28, 42, 90, 2),
  # endAnchor from timeAtRisks array
  endAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start")
) 

# Define Propensity Score matching settings from <Analysis Specifications>.propensityScoreAdjustment.psSettings
matchOnPsArgsList <- tibble(
  label = c("1-to-any matching caliper 0.2"), # Descriptive label for this PS setting
  maxRatio  = c(0), # maxRatio: 0 (for 1-to-many matching)
  caliper = c(0.2), # caliper: 0.2
  caliperScale  = c("standardized logit") # caliperScale: "standardized logit"
) 

# Propensity Score stratification settings. This is null in the specifications,
# so we create an empty tibble.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c(),
) 

# Build a single PS configuration list from the tibbles defined above.
# This loop structure allows for easily adding more PS strategies (e.g., stratification).
psConfigList <- list()

# If a data frame for "match on PS" exists and has rows, convert each row to a config
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    # Append a new element at the end of psConfigList
    psConfigList[[length(psConfigList) + 1]] <- list(
      # Identify the PS adjustment method for this config
      method = "match",
      # Human-readable label to carry through into descriptions
      label  = matchOnPsArgsList$label[i],
      # Parameter bundle passed to createMatchOnPsArgs later
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
    # Append a new element at the end of psConfigList
    psConfigList[[length(psConfigList) + 1]] <- list(
      # Identify the PS adjustment method for this config
      method = "stratify",
      # Human-readable label to carry through into descriptions
      label  = stratifyByPsArgsList$label[i],
      # Parameter bundle passed to createStratifyByPsArgs later
      params = list(
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection  = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}


# Iterate through all analysis setting combinations to create a list of analyses
cmAnalysisList <- list()
analysisId <- 1

# Loop over each study period defined
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  # Loop over each time-at-risk defined
  for (t in seq_len(nrow(timeAtRisks))) {

    # Loop over each propensity score adjustment strategy defined
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure either matching or stratification based on the PS config
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

      # Use default covariate settings. The <Analysis Specifications> does not
      # specify any custom concepts to include or exclude.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Define the outcomes for the analysis, combining the primary outcome
      # with the generated negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999 # This is defined in createStudyPopArgs; using here for consistency
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
      
      # Define the list of target-comparator-outcomes combinations.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # <Analysis Specifications>.covariateSelection is empty, so we only exclude
          # concepts from the empty excludedCovariateConcepts data frame.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Settings for data extraction, from <Analysis Specifications>.getDbCohortMethodDataArgs
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        # The spec does not define restrictToCommonPeriod for this function.
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # maxCohortSize: 0 (no maximum size)
        covariateSettings = covariateSettings
      )

      # Settings for propensity score creation, from <Analysis Specifications>.propensityScoreAdjustment.createPsArgs
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # maxCohortSizeForFitting: 250000
        errorOnHighCorrelation = TRUE, # errorOnHighCorrelation: true
        stopOnError = FALSE, # Allow Strategus to continue if a model fails to fit
        estimator = "att",
        # prior settings from createPsArgs.prior
        prior = Cyclops::createPrior(
          priorType = "laplace", # prior.priorType
          exclude = c(0), 
          useCrossValidation = TRUE # prior.useCrossValidation
        ),
        # control settings from createPsArgs.control
        control = Cyclops::createControl(
          noiseLevel = "silent", # control.noiseLevel
          cvType = "auto", # control.cvType
          seed = 1, 
          resetCoefficients = TRUE, # control.resetCoefficients
          tolerance = 2e-07, # control.tolerance
          cvRepetitions = 10, # control.cvRepetitions
          fold = 10, # control.fold
          startingVariance = 0.01 # control.startingVariance
        )
      )

      # Settings for computing covariate balance on all covariates
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      # Settings for computing covariate balance for Table 1
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Settings for fitting the outcome model, from <Analysis Specifications>.fitOutcomeModelArgs
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # modelType: "cox"
        stratified = FALSE, # stratified: false
        useCovariates = FALSE, # useCovariates: false
        inversePtWeighting = FALSE, # inversePtWeighting: false
        # prior settings from fitOutcomeModelArgs.prior
        prior = Cyclops::createPrior(
          priorType = "laplace", # prior.priorType
          useCrossValidation = TRUE # prior.useCrossValidation
        ),
        # control settings from fitOutcomeModelArgs.control
        control = Cyclops::createControl(
          cvType = "auto", # control.cvType
          seed = 1, 
          resetCoefficients = TRUE, # control.resetCoefficients
          startingVariance = 0.01, # control.startingVariance
          tolerance = 2e-07, # control.tolerance
          cvRepetitions = 10, # control.cvRepetitions
          fold = 10, # control.fold
          noiseLevel = "quiet" # control.noiseLevel
        )
      )
      
      # Settings for creating the study population, from <Analysis Specifications>.createStudyPopArgs
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # restrictToCommonPeriod: false
        firstExposureOnly = TRUE, # firstExposureOnly: true
        washoutPeriod = 365, # washoutPeriod: 365
        removeDuplicateSubjects = "remove all", # removeDuplicateSubjects: "remove all"
        censorAtNewRiskWindow = FALSE, # censorAtNewRiskWindow: false
        removeSubjectsWithPriorOutcome = TRUE, # removeSubjectsWithPriorOutcome: true
        priorOutcomeLookback = 99999, # priorOutcomeLookback: 99999
        # These TAR settings are taken from the outer loop over the timeAtRisks tibble
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1, # minDaysAtRisk: 1 for all TARs
        maxDaysAtRisk = 99999 # Default value
      )


      # Append the fully specified analysis settings to the list
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(is.na(studyStartDate), "Open", studyStartDate),
          ifelse(is.na(studyEndDate), "Open", studyEndDate),
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

# Finalize the CohortMethod module specifications
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Using default diagnostic thresholds
)

# Create the final analysis specifications object --------------------------------
# This object combines all the shared resources and module specifications.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Add shared resources
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  # Add module specifications
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications as a JSON file
# The file path uses the study name from <Analysis Specifications>.name ("rapidcyclejanssen")
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "rapidcyclejanssen", "rapidcyclejanssenAnalysisSpecification.json")
)