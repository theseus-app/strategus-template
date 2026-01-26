################################################################################
# CreateStrategusAnalysisSpecification.R
# Script for creating Strategus analysis specifications for tramadol vs codeine study
# 
# This script follows the OHDSI Strategus framework and applies settings from
# the provided analysis specifications. It creates specifications for:
# 1. Cohort generation using CohortGeneratorModule
# 2. Cohort diagnostics using CohortDiagnosticsModule  
# 3. Cohort method analysis using CohortMethodModule
#
# Key settings from analysis specifications:
# - Target cohort: ID 1794126, Name "target1"
# - Comparator cohort: ID 1794132, Name "comparator1"
# - Outcome cohort: ID 1794131, Name "outcome1"
# - Negative control concept set: ID 1888110
# - Propensity score matching with 1:1 ratio, 0.2 caliper on standardized logit
# - Cox proportional hazards outcome model
# - Risk window: 1 day after cohort start to cohort end
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Note: In a real execution, you would need to set baseUrl to your Atlas instance
baseUrl <- "https://your-atlas-instance/WebAPI"

# Cohort Definitions
# Fetch cohort definitions from Atlas using the IDs from analysis specifications
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: tramadol
    1794132, # Comparator: codeine  
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to sequential IDs for Strategus
# Target becomes ID 1, comparator becomes ID 2, outcome becomes ID 3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2  
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Update cohort names to match analysis specifications
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1,]$cohortName <- "target1"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 2,]$cohortName <- "comparator1"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 3,]$cohortName <- "outcome1"

# Negative control outcomes
# Fetch negative control concepts using concept set ID from analysis specifications
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
  mutate(cohortId = row_number() + 100) %>% # Negative controls start at 101
  select(cohortId, cohortName, outcomeConceptId)

# Validate no duplicate cohort IDs between main and negative control cohorts
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames for analysis specifications --------------------------------
# Outcomes for CohortMethod analysis
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window for outcomes

# Target and Comparator pairs for CohortMethod analysis
# Using exact names from analysis specifications
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2, 
  comparatorCohortName = "comparator1"
)

# Note: The analysis specifications have empty arrays for conceptsToInclude 
# and conceptsToExclude, so we use default covariate settings

# CohortGeneratorModule --------------------------------------------------------
# Create specifications for cohort generation
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resources for cohort definitions and negative controls
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",  # First occurrence of negative control outcome
  detectOnDescendants = TRUE  # Include descendant concepts
)

# Create module specifications for cohort generation
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE  # Generate cohort statistics
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# Create specifications for cohort diagnostics
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,  # Run diagnostics on all cohorts
  runInclusionStatistics = TRUE,
  runIncludedSourceConcepts = TRUE,
  runOrphanConcepts = TRUE,
  runTimeSeries = FALSE,  # Disabled as not specified in analysis specs
  runVisitContext = TRUE,
  runBreakdownIndexEvents = TRUE,
  runIncidenceRate = TRUE,
  runCohortRelationship = TRUE,
  runTemporalCohortCharacterization = TRUE,
  minCharacterizationMean = 0.01  # Minimum mean for characterization
)

# CohortMethodModule -----------------------------------------------------------

# Study periods from analysis specifications
# Both studyStartDate and studyEndDate are null, so we use empty strings
# This means no restriction on study period
studyPeriods <- tibble(
  studyStartDate = c(""),  # Empty string = no start date restriction
  studyEndDate   = c("")   # Empty string = no end date restriction
)

# Time-at-risks (TARs) from analysis specifications
# Single TAR: risk window start = 1 day after cohort start, end = cohort end
timeAtRisks <- tibble(
  label = c("Main TAR"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),  # Anchor to cohort start
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end"),  # Anchor to cohort end
  minDaysAtRisk = c(1)  # Minimum 1 day at risk required
)

# Propensity Score settings - match on PS
# Single PS matching configuration from analysis specifications
matchOnPsArgsList <- tibble(
  label = c("1:1 matching"),
  maxRatio  = c(1),  # 1:1 matching ratio
  caliper = c(0.2),  # 0.2 caliper
  caliperScale  = c("standardized logit")  # Caliper on standardized logit scale
)

# Build PS configuration list
psConfigList <- list()

# Add match on PS configurations
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

# Iterate through all analysis setting combinations
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Create PS adjustment arguments based on configuration
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

      # Covariate settings using defaults (no specific inclusions/exclusions)
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome list including both main outcomes and negative controls
      outcomeList <- append(
        # Main outcomes
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 365  # From analysis specifications
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
      
      # Create target-comparator-outcomes combinations
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = c()  # No specific exclusions
        )
      }

      # GetDbCohortMethodDataArgs from analysis specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,  # From analysis specifications
        studyStartDate = ifelse(studyStartDate == "", NULL, studyStartDate),
        studyEndDate = ifelse(studyEndDate == "", NULL, studyEndDate),
        maxCohortSize = 0,  # 0 = no limit, from analysis specifications
        covariateSettings = covariateSettings
      )

      # CreatePsArgs from analysis specifications
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # From analysis specifications
        errorOnHighCorrelation = TRUE,     # From analysis specifications
        stopOnError = FALSE,  # Allow Strategus to continue even if PS model fails
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",  # From analysis specifications
          exclude = c(0), 
          useCrossValidation = TRUE  # From analysis specifications
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",      # From analysis specifications
          cvType = "auto",            # From analysis specifications  
          seed = 1, 
          resetCoefficients = TRUE,   # From analysis specifications
          tolerance = 2e-07,          # From analysis specifications
          cvRepetitions = 10,         # From analysis specifications (was 1 in template)
          startingVariance = 0.01     # From analysis specifications
        )
      )

      # Covariate balance computation arguments
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # FitOutcomeModelArgs from analysis specifications
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",  # Cox model from analysis specifications
        stratified = FALSE,  # From analysis specifications (not stratified)
        useCovariates = FALSE,  # From analysis specifications
        inversePtWeighting = FALSE,  # From analysis specifications
        prior = Cyclops::createPrior(
          priorType = "laplace",  # From analysis specifications
          useCrossValidation = TRUE  # From analysis specifications
        ),
        control = Cyclops::createControl(
          cvType = "auto",            # From analysis specifications
          seed = 1, 
          resetCoefficients = TRUE,   # From analysis specifications
          startingVariance = 0.01,    # From analysis specifications
          tolerance = 2e-07,          # From analysis specifications
          cvRepetitions = 10,         # From analysis specifications (was 1 in template)
          noiseLevel = "quiet"        # From analysis specifications
        )
      )
      
      # CreateStudyPopArgs from analysis specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,  # From analysis specifications
        firstExposureOnly = FALSE,       # From analysis specifications
        washoutPeriod = 0,               # From analysis specifications
        removeDuplicateSubjects = "keep all",  # From analysis specifications
        censorAtNewRiskWindow = FALSE,   # From analysis specifications
        removeSubjectsWithPriorOutcome = TRUE,  # From analysis specifications
        priorOutcomeLookback = 365,      # From analysis specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999
      )

      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(studyStartDate == "", "Unrestricted", studyStartDate),
          ifelse(studyEndDate == "", "Unrestricted", studyEndDate),
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

# Create CohortMethod module specifications
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
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "tramadolcodein", "tramadolcodeinAnalysisSpecification.json")
)