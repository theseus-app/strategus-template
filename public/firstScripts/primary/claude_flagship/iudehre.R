################################################################################
# OHDSI Strategus Analysis Specification Script
# Study: iudehre
# 
# This script creates analysis specifications for a comparative cohort study
# using the Strategus framework and HADES modules.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------

# Define the base URL for the OHDSI WebAPI instance
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export cohort definitions from WebAPI using the exact cohort IDs specified
# Target cohort ID: 1794126 (target1)
# Comparator cohort ID: 1794132 (comparator1)
# Outcome cohort ID: 1794131 (outcome1)

cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to sequential IDs for internal use
# This standardizes cohort IDs to 1, 2, 3 for easier reference
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative Control Outcomes ----------------------------------------------------
# Retrieve negative control outcomes from the concept set ID: 1888110
# These are used to evaluate residual bias in the analysis

negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,
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
  mutate(cohortId = row_number() + 100) %>% # Negative control cohort IDs start at 101 to avoid conflicts
  select(cohortId, cohortName, outcomeConceptId)

# Validate that there are no duplicate cohort IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create Analysis Data Frames --------------------------------------------------

# Outcomes: Define the outcome cohort(s) of interest
# outcomeId = 3 (re-numbered from 1794131)
# cleanWindow = 365 days (standard washout period for outcomes)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for CohortMethod Analysis ------------------------------
# Define the exposure cohorts for comparative effectiveness analysis
# Target: cohort ID 1 (target1)
# Comparator: cohort ID 2 (comparator1)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate Selection ----------------------------------------------------------
# Per specifications: no concepts to include or exclude (both are null/empty)
# If concepts were specified, they would be defined here

# Optional: Excluded covariate concepts (currently none specified)
# excludedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# This module generates cohorts in the CDM database

cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource specifications for negative control outcome cohorts
# occurrenceType = "first" means we look for the first occurrence of the outcome
# detectOnDescendants = TRUE means we include descendant concepts
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications to generate cohort statistics
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule ------------------------------------------------------
# This module performs diagnostics on generated cohorts

cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Configure cohort diagnostics to run various diagnostic checks
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
# This module performs comparative cohort studies using propensity score methods

# Study Period Configuration ---------------------------------------------------
# Per specifications: studyStartDate = "20030101", studyEndDate = null (no end date)
studyPeriods <- tibble(
  studyStartDate = c("20030101"),
  studyEndDate = c("")
)

# Time-at-Risk Configuration ---------------------------------------------------
# Define the risk window for outcome assessment
# riskWindowStart = 30 days after cohort start
# riskWindowEnd = 5475 days after cohort start
# startAnchor and endAnchor = "cohort start"
# minDaysAtRisk = 1 day minimum follow-up
timeAtRisks <- tibble(
  label = c("TAR_30_5475"),
  riskWindowStart = c(30),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(5475),
  endAnchor = c("cohort start")
)

# Propensity Score Configuration -----------------------------------------------
# Per specifications: Using matchOnPsArgs with maxRatio=1, caliper=0.2, caliperScale="standardized logit"
# stratifyByPsArgs is null (not used)

matchOnPsArgsList <- tibble(
  label = c("PS_Match_1to1_cal0.2"),
  maxRatio = c(1),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# stratifyByPsArgs is not used per specifications (set to null)
# Creating empty data frame for consistency
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata = numeric(0),
  baseSelection = character(0)
)

# Build Propensity Score Configuration List ------------------------------------
# This list combines all PS adjustment methods (matching and/or stratification)

psConfigList <- list()

# Add "match on PS" configurations if any exist
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "match",
      label = matchOnPsArgsList$label[i],
      params = list(
        maxRatio = matchOnPsArgsList$maxRatio[i],
        caliper = matchOnPsArgsList$caliper[i],
        caliperScale = matchOnPsArgsList$caliperScale[i]
      )
    )
  }
}

# Add "stratify by PS" configurations if any exist
if (exists("stratifyByPsArgsList") && nrow(stratifyByPsArgsList) > 0) {
  for (i in seq_len(nrow(stratifyByPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "stratify",
      label = stratifyByPsArgsList$label[i],
      params = list(
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}

# Create CohortMethod Analysis List --------------------------------------------
# Iterate through all combinations of study periods, time-at-risks, and PS methods

cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment method based on the configuration
      if (psCfg$method == "match") {
        # Per specifications: maxRatio=1, caliper=0.2, caliperScale="standardized logit"
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

      # Covariate Settings -------------------------------------------------------
      # Using default covariate settings with descendant exclusion
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcome List -------------------------------------------------------------
      # Combine outcomes of interest and negative control outcomes
      # Per specifications: removeSubjectsWithPriorOutcome=TRUE, priorOutcomeLookBack=99999
      outcomeList <- append(
        # Outcomes of interest
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        # Negative control outcomes (trueEffectSize = 1 means no effect expected)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Target-Comparator-Outcomes List ------------------------------------------
      # Create combinations of target, comparator, and outcomes
      # Per specifications: no specific concepts to exclude from covariates
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = c()
        )
      }

      # Get Database Cohort Method Data Arguments --------------------------------
      # Per specifications:
      # - studyStartDate = "20030101", studyEndDate = null
      # - maxCohortSize = 0 (no limit)
      # - restrictToCommonPeriod is NOT specified in createStudyPopArgs, so using FALSE here
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Create Propensity Score Arguments ----------------------------------------
      # Per specifications:
      # - maxCohortSizeForFitting = 250000
      # - errorOnHighCorrelation = TRUE
      # - prior: priorType = "laplace", useCrossValidation = TRUE
      # - control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      #   noiseLevel = "silent", resetCoefficients = TRUE, startingVariance = 0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",
          exclude = c(0),
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",
          cvType = "auto",
          seed = 1,
          resetCoefficients = TRUE,
          tolerance = 2e-07,
          cvRepetitions = 10,
          startingVariance = 0.01,
          fold = 10
        )
      )

      # Compute Covariate Balance Arguments --------------------------------------
      # For shared balance (all covariates)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      # For Table 1 balance (default covariates)
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit Outcome Model Arguments ----------------------------------------------
      # Per specifications:
      # - modelType = "cox"
      # - stratified = FALSE
      # - useCovariates = FALSE
      # - inversePtWeighting = FALSE
      # - prior: priorType = "laplace", useCrossValidation = TRUE
      # - control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      #   noiseLevel = "quiet", resetCoefficients = TRUE, startingVariance = 0.01
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = FALSE,
        useCovariates = FALSE,
        inversePtWeighting = FALSE,
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          cvType = "auto",
          seed = 1,
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          tolerance = 2e-07,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          fold = 10
        )
      )

      # Create Study Population Arguments ----------------------------------------
      # Per specifications:
      # - restrictToCommonPeriod = FALSE
      # - firstExposureOnly = TRUE
      # - washoutPeriod = 365
      # - removeDuplicateSubjects = "keep all"
      # - censorAtNewRiskWindow = FALSE
      # - removeSubjectsWithPriorOutcome = TRUE
      # - priorOutcomeLookBack = 99999
      # - riskWindowStart = 30, startAnchor = "cohort start"
      # - riskWindowEnd = 5475, endAnchor = "cohort start"
      # - minDaysAtRisk = 1
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = TRUE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Append Analysis to List --------------------------------------------------
      # Create a descriptive label for this analysis combining all parameters
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

# Create CohortMethod Module Specifications ------------------------------------
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create Analysis Specifications -----------------------------------------------
# Combine all module specifications into a single analysis specification object

analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save Analysis Specifications -------------------------------------------------
# Save the complete analysis specifications to a JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "iudehre", "iudehreAnalysisSpecification.json")
)