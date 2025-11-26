################################################################################
# This script generates the analysis specifications for a Strategus study.
# It is based on the settings provided in a JSON format and uses a template
# structure.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# ##############################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Analysis Specifications Section: Not explicitly defined, but required for cohort retrieval.
# This baseUrl is for the public OHDSI Atlas instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Analysis Specifications Section: cohortDefinitions
# Retrieve cohort definitions from the specified ATLAS instance.
# The IDs correspond to the target, comparator, and outcome cohorts.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-numbering cohort IDs to a simple, sequential order for internal use within Strategus.
# This is a common practice to simplify referencing cohorts in the analysis settings.
# Target cohort (1794126) is re-mapped to ID 1.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# Comparator cohort (1794132) is re-mapped to ID 2.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# Outcome cohort (1794131) is re-mapped to ID 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Analysis Specifications Section: negativeControlConceptSet
# Retrieve the concept set for negative controls. These are outcomes believed to have no association
# with the exposure, used for empirical calibration.
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
  # Assign unique cohort IDs starting from 101 to avoid collision with T, C, O cohorts.
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)

# Sanity check to ensure no cohort IDs are duplicated across the main cohorts and negative controls.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts for different analysis modules ---------------

# Analysis Specifications Section: createStudyPopArgs, specifically for outcomes
# Define the outcome(s) of interest for the study.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Filter for the re-numbered outcome cohort ID
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # The clean window corresponds to `priorOutcomeLookBack` in createStudyPopArgs.
  # This setting removes subjects with the outcome in the 365 days prior to index.
  mutate(cleanWindow = 365) # From createStudyPopArgs.priorOutcomeLookBack

# Analysis Specifications Section: cohortDefinitions
# Define the target-comparator pairs for the CohortMethod analysis.
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered ID for target1
  targetCohortName = "target1",
  comparatorCohortId = 2, # Re-numbered ID for comparator1
  comparatorCohortName = "comparator1"
)

# Analysis Specifications Section: covariateSelection
# This section in the specifications is empty, meaning no specific concepts are to be
# globally excluded from the covariate construction. Standard practice is to exclude
# the exposure concepts themselves, which is handled later within createTargetComparatorOutcomes.
# Therefore, this data frame is initialized as empty.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# CohortGeneratorModule --------------------------------------------------------
# This module is responsible for generating the cohort instances from their definitions.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Create shared resource specifications for the primary T, C, and O cohorts.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Create shared resource specifications for the negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
# Define the CohortGenerator module specifications.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# This module runs a set of diagnostics on the generated cohorts to assess their quality.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  # Run diagnostics on all defined cohorts (T, C, O).
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

# Analysis Specifications Section: getDbCohortMethodDataArgs.studyPeriods
# The specification indicates no specific study start or end date (null values).
# We create a tibble with one row of empty strings to ensure the analysis loop runs once.
studyPeriods <- tibble(
  studyStartDate = c(""), # Empty string for no start date restriction
  studyEndDate   = c("")  # Empty string for no end date restriction
)

# Analysis Specifications Section: createStudyPopArgs.timeAtRisks
# Defines the time-at-risk window for outcome assessment.
timeAtRisks <- tibble(
  label = "1 day after start to cohort end", # A descriptive label for this TAR
  riskWindowStart  = 1,
  startAnchor = "cohort start",
  riskWindowEnd  = 0,
  endAnchor = "cohort end"
) 

# Analysis Specifications Section: propensityScoreAdjustment.psSettings
# Defines the propensity score adjustment strategy. In this case, matching.
matchOnPsArgsList <- tibble(
  label = "1-to-1 Matching on standardized logit PS with 0.2 caliper", # A descriptive label
  maxRatio  = 1,
  caliper = 0.2,
  caliperScale  = "standardized logit"
) 

# Propensity Score settings - stratify by PS is not specified, so we create an empty tibble.
stratifyByPsArgsList <- tibble() 

# Build a single PS configuration list to iterate over. This structure allows for
# combining multiple PS methods (e.g., matching and stratification) in a single study.
psConfigList <- list()

# Convert the matchOnPsArgsList tibble into the psConfigList format.
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

# Convert the stratifyByPsArgsList tibble into the psConfigList format (will be skipped as it's empty).
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

# Since there is only one studyPeriod, TAR, and PS setting, this loop will run once.
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Based on the method ("match" or "stratify"), create the appropriate arguments object.
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

      # Use default covariate settings from FeatureExtraction. This includes a wide range of
      # demographics, conditions, drugs, procedures, etc.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Combine the specified outcome of interest with the negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # True effect size is unknown for the primary outcome
            priorOutcomeLookback = oList$cleanWindow[i]
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # Assumed true effect size of 1 for negative controls
          )
        })
      )
      
      # Define the list of T-C-O combinations to analyze.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude concepts specified in the covariateSelection section. In this case, it's empty.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Analysis Specifications Section: getDbCohortMethodDataArgs
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From getDbCohortMethodDataArgs.maxCohortSize; 0 means no limit.
        covariateSettings = covariateSettings
      )

      # Analysis Specifications Section: propensityScoreAdjustment.createPsArgs
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE,
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace", # From createPsArgs.prior.priorType
          useCrossValidation = TRUE # From createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", # From createPsArgs.control.noiseLevel
          cvType = "auto", # From createPsArgs.control.cvType
          seed = 1,
          resetCoefficients = TRUE, # From createPsArgs.control.resetCoefficients
          tolerance = 2e-07, # From createPsArgs.control.tolerance
          cvRepetitions = 10, # From createPsArgs.control.cvRepetitions
          startingVariance = 0.01, # From createPsArgs.control.startingVariance
          fold = 10 # From createPsArgs.control.fold
        )
      )

      # Settings for computing covariate balance, not specified in JSON, so using defaults.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Analysis Specifications Section: fitOutcomeModelArgs
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From fitOutcomeModelArgs.modelType
        stratified = FALSE, # From fitOutcomeModelArgs.stratified
        useCovariates = FALSE, # From fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE, # From fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior(
          priorType = "laplace", # From fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE # From fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          cvType = "auto", # From fitOutcomeModelArgs.control.cvType
          seed = 1,
          resetCoefficients = TRUE, # From fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01, # From fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07, # From fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10, # From fitOutcomeModelArgs.control.cvRepetitions
          noiseLevel = "quiet", # From fitOutcomeModelArgs.control.noiseLevel
          fold = 10 # From fitOutcomeModelArgs.control.fold
        )
      )

      # Analysis Specifications Section: createStudyPopArgs
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE, # From createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0, # From createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # From createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # From createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # From createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 365, # From createStudyPopArgs.priorOutcomeLookBack
        # The following settings are from createStudyPopArgs.timeAtRisks
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1, # From createStudyPopArgs.timeAtRisks.minDaysAtRisk
        maxDaysAtRisk = 99999 # Default value for no maximum
      )

      # Assemble the complete set of CohortMethod settings for this analysis variant.
      cmAnalysis <- CohortMethod::createCmAnalysis(
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
      
      # Append the analysis to the list and increment the ID.
      cmAnalysisList[[analysisId]] <- cmAnalysis
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
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Use default diagnostic thresholds
)

# Create the analysis specifications object ------------------------------------------
# This object combines all shared resources and module specifications into a single JSON file
# that Strategus can execute.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Add shared resources (cohorts, concept sets) that modules can use.
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  # Add the specifications for each module to be executed.
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the specifications to a JSON file.
# Analysis Specifications Section: name
# The file path and name are derived from the study name "tramadolcodein".
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "tramadolcodein", "tramadolcodeinAnalysisSpecification.json")
)