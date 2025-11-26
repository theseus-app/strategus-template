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

# Analysis Specifications Name -------------------------------------------------
# The name of the analysis specifications. This is used to name the output file.
analysisName <- "covid19ppiandh2ra"

# Shared Resources -------------------------------------------------------------
# In this section we define the cohorts and concept sets that will be used
# across the different modules of the analysis.

# The baseUrl for the WebApi instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions from <Analysis Specifications>.cohortDefinitions
# We will use ROhdsiWebApi to retrieve the cohort definitions from the WebApi.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-numbering cohort IDs for use within the Strategus framework.
# It is a convention to use small integer IDs for the main cohorts.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes from <Analysis Specifications>.negativeControlConceptSet
# We retrieve the concept set and resolve it to get a list of concepts
# that will be used as negative control outcomes.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # negative
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(
    baseUrl = baseUrl
  ) %>%
  ROhdsiWebApi::getConcepts(
    baseUrl = baseUrl
  ) %>%
  # Rename columns to match the expected format for negative controls
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  # Assign unique cohort IDs for the negative controls, starting from 101
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# A safety check to ensure there are no overlapping cohort IDs between
# the main cohorts and the negative control outcomes.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Data frames for analysis settings ---------------------------------------------
# These data frames help organize the various combinations of analysis settings.

# Outcomes of interest, based on the outcome cohorts defined above.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # The clean window is the number of days an outcome is assumed to be the same outcome
  mutate(cleanWindow = 365) 

# Target and Comparator pairs for the CohortMethod analysis.
# Uses the re-numbered cohort IDs and names from the cohortDefinitionSet.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = cohortDefinitionSet$cohortName[cohortDefinitionSet$cohortId == 1],
  comparatorCohortId = 2,
  comparatorCohortName = cohortDefinitionSet$cohortName[cohortDefinitionSet$cohortId == 2]
)

# Covariate Selection from <Analysis Specifications>.covariateSelection
# The specifications indicate no specific concepts to include or exclude.
# Thus, we create empty data frames.
# Note: It is best practice to exclude the target and comparator drug concepts
# from the covariates, but their concept IDs are not provided in the specifications.
includedCovariateConcepts <- data.frame(conceptId = integer(), conceptName = character())
excludedCovariateConcepts <- data.frame(conceptId = integer(), conceptName = character())


# CohortGeneratorModule --------------------------------------------------------
# This module is responsible for generating the cohorts defined above.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Defines the set of cohorts to be generated.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Defines the negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
# Specifies the settings for the CohortGenerator module.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# This module runs a comprehensive set of diagnostics on the generated cohorts.
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

# Study periods from <Analysis Specifications>.getDbCohortMethodDataArgs.studyPeriods
studyPeriods <- tibble(
  studyStartDate = c("20200101"),
  studyEndDate   = c("20200515")
)

# Time-at-risks (TARs) from <Analysis Specifications>.createStudyPopArgs.timeAtRisks
timeAtRisks <- tibble(
  label = "1-99999 days post-index",
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(99999),
  endAnchor = c("cohort start")
)

# Propensity Score settings from <Analysis Specifications>.propensityScoreAdjustment.psSettings
# One setting defines matching arguments.
matchOnPsArgsList <- tibble(
  label = "PS Matching (1-to-4, 0.2 caliper)",
  maxRatio  = c(4),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
) 

# One setting defines stratification arguments.
stratifyByPsArgsList <- tibble(
  label = "PS Stratification (5 strata)",
  numberOfStrata  = c(5),
  baseSelection = c("all")
) 

# Build a single list of all Propensity Score (PS) adjustment configurations.
# This list will be iterated over to create different analyses.
psConfigList <- list()

# Add the "No PS adjustment" option based on the first entry in psSettings
# where both matchOnPsArgs and stratifyByPsArgs are null.
psConfigList[[length(psConfigList) + 1]] <- list(
  method = "none",
  label  = "No PS Adjustment",
  params = list()
)

# Convert the matchOnPsArgsList data frame into configuration list items.
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

# Convert the stratifyByPsArgsList data frame into configuration list items.
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
      
      # Conditionally create propensity score adjustment arguments based on the method.
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
      } else { # "none"
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- NULL
      }

      # Define covariate settings. Using default settings as per specification.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE,
        excludedCovariateConceptIds = excludedCovariateConcepts$conceptId,
        includedCovariateConceptIds = includedCovariateConcepts$conceptId
      )

      # Combine the outcomes of interest and the negative control outcomes into a single list.
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
          # Concepts to exclude from covariates. Here it's empty based on the specifications.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Define arguments for getting data from the database.
      # Settings from <Analysis Specifications>.getDbCohortMethodDataArgs
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = TRUE,
        washoutPeriod = 180,
        removeDuplicateSubjects = "keep first",
        covariateSettings = covariateSettings
      )
      
      # Define arguments for creating the study population.
      # Settings from <Analysis Specifications>.createStudyPopArgs
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = FALSE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1 # from timeAtRisks settings in spec
      )

      # Define arguments for creating the propensity score model.
      # Settings from <Analysis Specifications>.propensityScoreAdjustment.createPsArgs
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,
        prior = Cyclops::createPrior(
          priorType = "laplace", 
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          tolerance = 2e-7, 
          cvType = "auto", 
          fold = 10,
          cvRepetitions = 10, 
          noiseLevel = "silent", 
          resetCoefficients = TRUE, 
          startingVariance = 0.01,
          seed = 1 # for reproducibility
        )
      )

      # Arguments for computing covariate balance. Defaults are used here.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Define arguments for fitting the outcome model.
      # Settings from <Analysis Specifications>.fitOutcomeModelArgs
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
          tolerance = 2e-7,
          cvType = "auto", 
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01, 
          seed = 1 # for reproducibility
        )
      )

      # Append the complete set of analysis settings to the list.
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

# Create the analysis specifications ------------------------------------------
# This combines all the module specifications into a single JSON object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Add shared resources (cohorts, negative controls)
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  # Add module specifications
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", analysisName, paste0(analysisName, "AnalysisSpecification.json"))
)