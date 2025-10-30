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
# Analysis Specifications for the "corazon" study
#
# ===============================================================================


# Shared Resources -------------------------------------------------------------
# In this section we define the cohorts that will be used throughout the study.

# The baseUrl is the location of the WebApi instance.
# We are using the OHDSI atlas-demo instance.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Define the cohort IDs for our target, comparator, and outcome cohorts from ATLAS.
# These IDs are sourced from the <Analysis Specifications>.
cohortIds <- c(
  1794126, # Target: target1
  1794132, # Comparator: comparator1
  1794131  # Outcome: outcome1
)

# Use ROhdsiWebApi to export the cohort definitions into a data frame.
# generateStats = TRUE is recommended to ensure the cohort definitions are up-to-date.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = cohortIds,
  generateStats = TRUE
)

# To simplify referencing cohorts, we will re-number them with simpler IDs.
# T = 1, C = 2, O = 3.
cohortDefinitionSet <- cohortDefinitionSet %>%
  mutate(cohortId = case_when(
    cohortId == 1794126 ~ 1, # Target: target1
    cohortId == 1794132 ~ 2, # Comparator: comparator1
    cohortId == 1794131 ~ 3, # Outcome: outcome1
    TRUE ~ cohortId
  ))

# Define the concept set for negative controls.
# This concept set, defined in ATLAS, contains concepts that are NOT believed to be
# causally related to the outcome. These will be used for calibration.
negativeControlConceptSetId <- 1888110 # negative

# Resolve the concept set into a list of concepts and format it for Strategus.
# Cohort IDs for negative controls will start from 101 to avoid collision with main cohorts.
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
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Sanity check to ensure there are no duplicate cohort IDs.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the specific cohorts for each part of the analysis.
# This helps in organizing the analysis settings.

# Outcomes: Create a list of outcome cohorts.
# Here we only have one outcome cohort.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # cleanWindow is not in the spec, keeping template default.

# Target and Comparator: Define the Target-Comparator pair for the CohortMethod analysis.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate Selection: As per the specifications, no specific concepts are to be
# included or excluded from the covariates.
# We create empty data frames for this.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# Optional: If you want to define covariates to include instead of including them all.
# This is empty as per the specification.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )


# Module Specifications --------------------------------------------------------
# Now we define the settings for each module that will be part of the analysis.

# CohortGeneratorModule --------------------------------------------------------
# This module is responsible for generating all the cohorts defined above.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Create shared resource specifications for the main cohorts.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Create shared resource specifications for the negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
# Define the module specifications for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
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
  minCharacterizationMean = 0.01 # Standard setting
)

# CohortMethodModule -----------------------------------------------------------
# This is the core module for the comparative cohort study. It builds propensity
# score models, matches/stratifies cohorts, and fits outcome models.

# Study Periods: Define the time windows for the analysis.
# Per specifications, we have two study periods.
studyPeriods <- tibble::tibble(
  studyStartDate = c("20100101", "20120101"),
  studyEndDate   = c("20191231", "20191231")
)

# Time-at-risks (TARs): Define the periods after cohort entry where outcomes are counted.
# Per specifications, we have two TAR definitions.
timeAtRisks <- tibble::tibble(
  label = c("On Treatment", "Intent to Treat"),
  riskWindowStart  = c(0, 0),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd  = c(0, 9999),
  endAnchor = c("cohort end", "cohort start")
)

# Propensity Score settings - Stratify by PS
# Per specifications, one stratification setting is defined.
stratifyByPsArgsList <- tibble::tibble(
  label = c("Stratification 5 strata"),
  numberOfStrata  = c(5),
  baseSelection = c("all")
)

# Propensity Score settings - Match on PS
# Per specifications, one matching setting is defined.
matchOnPsArgsList <- tibble::tibble(
  label = c("Matching 0 ratio 0.2 caliper"),
  maxRatio  = c(0),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# Build a single PS configuration list that combines both matching and stratification.
# This loop structure allows for easy extension with more PS methods.
psConfigList <- list()

# Add stratification configurations to the list.
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

# Add matching configurations to the list.
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

# Main loop to create all combinations of analysis settings.
# This iterates through study periods, time-at-risks, and PS configurations.
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure either matching or stratification based on the PS method.
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

      # Standard covariate settings. No specific inclusions/exclusions defined.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Combine the single outcome of interest with the list of negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # For outcomes of interest, true effect size is unknown
            priorOutcomeLookback = 99999
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # For negative controls, true effect size is 1
          )
        })
      )
      
      # Create the list of T-C-O combinations to be analyzed.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Per spec, no concepts are excluded from covariates.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Settings for fetching data from the database.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        # maxCohortSize = 0 means no limit on cohort size.
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Settings for creating the propensity score model.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,
        estimator = "att",
        # Prior settings for regularization, as specified.
        prior = Cyclops::createPrior(
          priorType = "laplace", 
          exclude = c(0), # Do not regularize the intercept
          useCrossValidation = TRUE
        ),
        # Control settings for the Cyclops model fitting process.
        control = Cyclops::createControl(
          noiseLevel = "silent", 
          cvType = "auto", 
          seed = 1, # for reproducibility
          resetCoefficients = TRUE, 
          tolerance = 2e-07, 
          cvRepetitions = 10,
          fold = 10,
          startingVariance = 0.01
        )
      )

      # Settings for computing covariate balance.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Settings for fitting the outcome model.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = TRUE,
        useCovariates = FALSE,
        inversePtWeighting = FALSE,
        # Prior settings for regularization, as specified.
        prior = Cyclops::createPrior(
          priorType = "laplace", 
          useCrossValidation = TRUE
        ),
        # Control settings for the Cyclops model fitting process.
        control = Cyclops::createControl(
          cvType = "auto", 
          seed = 1, # for reproducibility
          resetCoefficients = TRUE,
          startingVariance = 0.01, 
          tolerance = 2e-07, 
          cvRepetitions = 10,
          fold = 10,
          noiseLevel = "quiet"
        )
      )
      
      # Settings for creating the study population.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        # TAR settings are applied here from the loop.
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999 # Standard default
      )

      # Combine all the above settings into a single CohortMethod analysis object.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        # The description provides a human-readable summary of the analysis settings.
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
      # Increment the analysis ID for the next combination.
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

# Create the analysis specifications object -----------------------------------
# This object brings together all the module specifications and shared resources.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  # Add shared resources (cohort definitions).
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  # Add module specifications. The order determines the execution sequence.
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# This file will be used by Strategus to execute the study.
# The path is based on the analysis name "corazon".
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "corazon", "corazonAnalysisSpecification.json")
)