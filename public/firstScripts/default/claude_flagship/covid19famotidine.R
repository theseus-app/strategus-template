################################################################################
# COVID-19 Famotidine Study - Strategus Analysis Specifications
# 
# This script creates the analysis specifications for a comparative cohort study
# examining famotidine use in COVID-19 patients.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
################################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Configure connection to ATLAS instance
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export cohort definitions from ATLAS
# - Target cohort (1794126): target1
# - Comparator cohort (1794132): comparator1  
# - Outcome cohort (1794131): outcome1
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
# Target cohort: 1794126 -> 1
# Comparator cohort: 1794132 -> 2
# Outcome cohort: 1794131 -> 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative Control Outcomes ----------------------------------------------------
# Retrieve negative control outcome concept set (ID: 1888110, name: negative)
# These are used to assess residual bias in the study
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
  mutate(cohortId = row_number() + 100) %>% # Negative control cohorts start at 101 to avoid conflicts
  select(cohortId, cohortName, outcomeConceptId)

# Verify no duplicate cohort IDs exist between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create Analysis Data Frames --------------------------------------------------
# Define outcomes of interest
# Using outcome cohort (ID: 3) with a 365-day clean window
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Define target and comparator cohorts for CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  # Placeholder concept IDs for exclusion (set to NA as not specified)
  targetConceptId = NA,
  comparatorConceptId = NA
)

# Covariate Selection ----------------------------------------------------------
# No specific concepts to exclude based on analysis specifications
# (conceptsToExclude had null/empty values)
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# No specific concepts to include based on analysis specifications
# (conceptsToInclude had null/empty values)
# Using default covariate settings which will include all available covariates

# CohortGeneratorModule --------------------------------------------------------
# Configure cohort generation module to create all cohorts in target database
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
# Configure comprehensive diagnostics for all cohorts
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

# Study Period Configuration ---------------------------------------------------
# Based on getDbCohortMethodDataArgs.studyPeriods from analysis specifications
# Study period: February 1, 2020 to May 30, 2020
studyPeriods <- tibble(
  studyStartDate = c("20200201"),
  studyEndDate   = c("20200530")
)

# Time-at-Risk Configuration ---------------------------------------------------
# Based on createStudyPopArgs.timeAtRisks from analysis specifications
# TAR: Day 1 to Day 30 after cohort start, minimum 1 day at risk
timeAtRisks <- tibble(
  label = c("Day 1-30 post cohort start"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(30),
  endAnchor = c("cohort start")
)

# Propensity Score Configuration -----------------------------------------------
# Based on propensityScoreAdjustment.psSettings from analysis specifications

# PS Matching Configuration
# maxRatio=1 (1:1 matching), caliper=0.2 on standardized logit scale
matchOnPsArgsList <- tibble(
  label = c("1:1 Match, Caliper 0.2 (Std Logit)"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# PS Stratification Configuration  
# 5 strata, using all subjects for stratification
stratifyByPsArgsList <- tibble(
  label = c("Stratify 5 Strata (All)"),
  numberOfStrata  = c(5),
  baseSelection = c("all")
)

# Build unified PS configuration list ------------------------------------------
# Combines both matching and stratification approaches
psConfigList <- list()

# Convert matching configuration to list format
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

# Convert stratification configuration to list format
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

# Generate CohortMethod Analysis List ------------------------------------------
# Iterate through all combinations of study periods, time-at-risks, and PS methods
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS method-specific arguments
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

      # Covariate Settings -------------------------------------------------------
      # Using default covariate settings as no specific concepts to include/exclude
      # were provided in analysis specifications
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Define Outcomes ----------------------------------------------------------
      # Combine outcomes of interest with negative control outcomes
      outcomeList <- append(
        # Outcomes of interest (from cohort definitions)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999 # Based on createStudyPopArgs.priorOutcomeLookBack
          )
        }),
        # Negative control outcomes
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )
      
      # Target-Comparator-Outcomes Configuration ---------------------------------
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        # Build exclusion list (excluding NA values)
        excludedIds <- c(
          cmTcList$targetConceptId[i], 
          cmTcList$comparatorConceptId[i],
          excludedCovariateConcepts$conceptId
        )
        excludedIds <- excludedIds[!is.na(excludedIds)]
        
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = if(length(excludedIds) > 0) excludedIds else c()
        )
      }

      # GetDbCohortMethodData Arguments ------------------------------------------
      # Based on getDbCohortMethodDataArgs from analysis specifications
      # restrictToCommonPeriod=FALSE, maxCohortSize=0 (no limit)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE, # From getDbCohortMethodDataArgs.restrictToCommonPeriod
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From getDbCohortMethodDataArgs.maxCohortSize (0 = no limit)
        covariateSettings = covariateSettings
      )

      # Create Propensity Score Arguments ----------------------------------------
      # Based on propensityScoreAdjustment.createPsArgs from analysis specifications
      # Uses Laplace prior with cross-validation
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE,
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace", # From createPsArgs.prior.priorType
          exclude = c(0),
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

      # Covariate Balance Arguments ----------------------------------------------
      # Shared balance (all covariates, no filter)
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      # Standard balance (Table 1 covariates only)
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit Outcome Model Arguments ----------------------------------------------
      # Based on fitOutcomeModelArgs from analysis specifications
      # Cox model, stratified, without covariates, using Laplace prior
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From fitOutcomeModelArgs.modelType
        stratified = TRUE, # From fitOutcomeModelArgs.stratified
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
      
      # Create Study Population Arguments ----------------------------------------
      # Based on createStudyPopArgs from analysis specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE, # From createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0, # From createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # From createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # From createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = FALSE, # From createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # From createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From timeAtRisks
        startAnchor = timeAtRisks$startAnchor[t], # From timeAtRisks
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From timeAtRisks
        endAnchor = timeAtRisks$endAnchor[t], # From timeAtRisks
        minDaysAtRisk = 1, # From createStudyPopArgs.timeAtRisks.minDaysAtRisk
        maxDaysAtRisk = 99999
      )

      # Create CohortMethod Analysis ---------------------------------------------
      # Combine all settings into a single analysis specification
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

# Create the analysis specifications ------------------------------------------
# Combine all modules into a single Strategus analysis specification
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save analysis specifications to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "covid19famotidine", "covid19famotidineAnalysisSpecification.json")
)