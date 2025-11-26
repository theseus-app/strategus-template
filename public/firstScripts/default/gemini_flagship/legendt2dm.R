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
# NOTE: This script will use the OHDSI Atlas demo instance to retrieve the cohort
# definitions.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions from the <Analysis Specifications>
# The following section will retrieve cohort definitions from the specified
# WebAPI instance using the IDs provided in the analysis specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-numbering cohorts for internal consistency within the Strategus framework.
# It is a common practice to use simple, sequential IDs (e.g., 1, 2, 3) for
# the primary cohorts in the analysis.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # target1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # comparator1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # outcome1

# Negative control outcomes are fetched from a concept set specified in the
# <Analysis Specifications>. These concepts are then resolved and formatted
# into a cohort set.
# The `negativeControlConceptSet` id is 1888110.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # id for "negative"
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
  # Assign cohort IDs starting from 101 to avoid conflicts with primary cohorts.
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)

# A safety check to ensure there are no duplicate cohort IDs between the primary
# cohorts and the negative control cohorts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts we'll use in each analysis -----------

# Defines the outcome(s) of interest for the study.
# Based on `cohortDefinitions.outcomeCohort` from the specifications.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Corresponds to outcome1 (original ID 1794131)
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Standard setting for outcome clean window

# Defines the target and comparator cohort pairs for the CohortMethod analysis.
# Based on `cohortDefinitions.targetCohort` and `cohortDefinitions.comparatorCohort`.
cmTcList <- data.frame(
  targetCohortId = 1, # Corresponds to target1 (original ID 1794126)
  targetCohortName = "target1",
  comparatorCohortId = 2, # Corresponds to comparator1 (original ID 1794132)
  comparatorCohortName = "comparator1"
)

# Defines concepts to be excluded from the covariate construction.
# The <Analysis Specifications> for `covariateSelection.conceptsToExclude` is empty.
# Therefore, we create an empty data frame. Covariate exclusion for the exposure
# domain is handled by default in FeatureExtraction.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all
# The <Analysis Specifications> for `covariateSelection.conceptsToInclude` is empty.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# This module is responsible for generating the cohort instances from their
# definitions on the target OMOP CDM.
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
# This module runs a standard set of diagnostics on the generated cohorts to
# assess their characteristics and quality.
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

# Defines the overall study period. If empty, the study uses the full
# observation period available in the data source.
# Based on `getDbCohortMethodDataArgs.studyPeriods`.
studyPeriods <- tibble(
  studyStartDate = c("19920101"), #YYYYMMDD
  studyEndDate   = c("20211231")  #YYYYMMDD
)

# Defines the time-at-risk (TAR) windows for the outcomes.
# Based on `createStudyPopArgs.timeAtRisks`.
timeAtRisks <- tibble(
  label = c("On Treatment"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
) 

# Defines propensity score (PS) matching settings.
# Based on `propensityScoreAdjustment.psSettings` where `matchOnPsArgs` is specified.
matchOnPsArgsList <- tibble(
  label = c("Matching 100 to 1"), # A descriptive label for this PS method
  maxRatio  = c(100),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
) 

# Defines propensity score (PS) stratification settings.
# Based on `propensityScoreAdjustment.psSettings` where `stratifyByPsArgs` is specified.
stratifyByPsArgsList <- tibble(
  label = c("Stratification 5 strata"), # A descriptive label for this PS method
  numberOfStrata  = c(5),
  baseSelection = c("all")
) 

# The following code block dynamically builds a list of PS configurations based
# on the `matchOnPsArgsList` and `stratifyByPsArgsList` tibbles defined above.
# This allows for running the analysis with multiple PS adjustment strategies.
psConfigList <- list()

# Convert each row in the matching settings tibble to a configuration list item.
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

# Convert each row in the stratification settings tibble to a configuration list item.
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

# The main loop iterates through all combinations of study periods, time-at-risks,
# and PS configurations to create a comprehensive list of analyses (cmAnalysisList).
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Based on the method ('match' or 'stratify'), create the appropriate
      # arguments object for PS adjustment.
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

      # Use default covariate settings from FeatureExtraction. This is appropriate
      # as the `covariateSelection` in the specs is empty.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create a list of outcome objects, including both the primary outcome(s)
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
      
      # Define the target-comparator-outcomes combinations for the analysis.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Excluded concept IDs are taken from the previously defined empty data frame.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Settings for extracting data from the CDM.
      # Based on `getDbCohortMethodDataArgs` from the specifications.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        removeDuplicateSubjects = "keep all",
        washoutPeriod = 0,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Settings for creating the propensity score model.
      # Based on `propensityScoreAdjustment.createPsArgs`.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace", 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", 
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          resetCoefficients = TRUE, 
          tolerance = 2e-07, 
          startingVariance = 0.01,
          seed = 1
        )
      )

      # Settings for computing covariate balance. These are standard settings.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Settings for fitting the outcome model.
      # Based on `fitOutcomeModelArgs` from the specifications.
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
          noiseLevel = "quiet",
          cvType = "auto", 
          fold = 10,
          cvRepetitions = 10,
          resetCoefficients = TRUE,
          startingVariance = 0.01, 
          tolerance = 2e-07, 
          seed = 1
        )
      )

      # Settings for creating the final study population after data extraction and PS adjustment.
      # Based on `createStudyPopArgs` from the specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1
      )

      # Each unique combination of settings is compiled into a single cmAnalysis object.
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

# Create the full module specifications for the CohortMethodModule.
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
# This final step assembles all the shared resources and module specifications
# into a single analysisSpecifications object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the specifications to a JSON file. This file will be used by Strategus
# to execute the study. The study name "legendt2dm" is taken from the specs.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "legendt2dm", "legendt2dmAnalysisSpecification.json")
)