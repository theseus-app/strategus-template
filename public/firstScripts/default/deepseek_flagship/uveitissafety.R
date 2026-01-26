################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates an analysis specification for the "uveitissafety" study
# using the OHDSI Strategus package. The specifications are based on the
# provided JSON configuration.
#
# Key components:
# 1. Cohort definitions (target, comparator, outcome, negative controls)
# 2. CohortMethod settings for comparative effectiveness/safety analysis
# 3. Propensity score adjustment strategies
# 4. Outcome modeling parameters
#
# The script follows the template structure while applying the exact settings
# from the provided analysis specifications.
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Note: This section sets up cohort definitions and negative controls.
# The baseUrl points to an Atlas instance where cohort definitions are stored.

baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions from Analysis Specifications
# Using EXACT cohort IDs and names from the specifications
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal use in Strategus
# This re-mapping is necessary because Strategus expects sequential cohort IDs
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes from concept set
# Using EXACT concept set ID 1888110 from specifications
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
  mutate(cohortId = row_number() + 100) %>% # Negative control IDs start at 101
  select(cohortId, cohortName, outcomeConceptId)

# Validate that there are no duplicate cohort IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames for analysis configurations -------------------------------

# Outcomes of interest from the specifications
# Using EXACT outcome cohort ID 1794131 (mapped to 3) with clean window of 365 days
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, 
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator pairs for CohortMethod analysis
# Using EXACT cohort names from specifications
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Excluded covariate concepts
# From specifications: conceptsToInclude and conceptsToExclude arrays are empty
# Therefore, no specific concepts are included or excluded beyond defaults
# Note: The target and comparator drug concepts will be automatically excluded
# during CohortMethod analysis to avoid bias

# CohortGeneratorModule --------------------------------------------------------
# This module generates the cohorts in the database
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
# This module performs the comparative analysis using propensity scores

# Study periods from specifications
# Using empty strings as specified (no restriction on study period)
studyPeriods <- tibble(
  studyStartDate = c(""), # From specifications: empty string
  studyEndDate   = c("")  # From specifications: empty string
)

# Time-at-risks (TARs) from specifications
# Two TARs as defined in createStudyPopArgs.timeAtRisks
timeAtRisks <- tibble(
  label = c("Primary", "Secondary"),
  riskWindowStart  = c(1, 1), # Both start at 1 day
  startAnchor = c("cohort start", "cohort start"), # Both anchored to cohort start
  riskWindowEnd  = c(0, 99999), # Primary: 0 (cohort end), Secondary: 99999 days
  endAnchor = c("cohort end", "cohort start"), # Primary: cohort end, Secondary: cohort start
  minDaysAtRisk = c(1, 1) # Both require at least 1 day at risk
) 

# Propensity Score settings from specifications
# Two matchOnPs configurations (no stratification specified)
matchOnPsArgsList <- tibble(
  label = c("Match 10:1", "Match 1:1"), # Labels for identification
  maxRatio  = c(10, 1), # From specifications: 10 and 1
  caliper = c(0.2, 0.2), # Both use 0.2 caliper
  caliperScale  = c("standardized logit", "standardized logit") # Both use standardized logit
) 

# Build PS configuration list
psConfigList <- list()

# Convert matchOnPs settings to configuration list
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

# Iterate through all analysis setting combinations
# This creates all permutations of study periods, TARs, and PS methods
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
      } else {
        # No stratification configurations in this specification
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- NULL
      }
      
      # Covariate settings - using default settings
      # Note: specifications have empty conceptsToInclude and conceptsToExclude
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )
      
      # Create outcome list including both outcomes of interest and negative controls
      outcomeList <- append(
        # Outcomes of interest (from oList)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999 # From specifications
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
          excludedCovariateConceptIds = c() # No specific exclusions from specifications
        )
      }
      
      # getDbCohortMethodDataArgs from specifications
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # From specifications
        studyStartDate = if (studyStartDate == "") NULL else studyStartDate,
        studyEndDate = if (studyEndDate == "") NULL else studyEndDate,
        maxCohortSize = 0, # From specifications: 0 means no limit
        covariateSettings = covariateSettings,
        firstExposureOnly = FALSE, # From specifications
        washoutPeriod = 0, # From specifications
        removeDuplicateSubjects = "keep all" # From specifications
      )
      
      # createPsArgs from specifications
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From specifications
        errorOnHighCorrelation = TRUE, # From specifications
        stopOnError = FALSE, # Allow Strategus to continue even if PS model fails
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace", # From specifications
          exclude = c(0),
          useCrossValidation = TRUE # From specifications
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", # From specifications
          cvType = "auto", # From specifications
          seed = 1,
          resetCoefficients = TRUE, # From specifications
          tolerance = 2e-07, # From specifications
          cvRepetitions = 10, # From specifications
          startingVariance = 0.01, # From specifications
          fold = 10 # From specifications
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
      
      # fitOutcomeModelArgs from specifications
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From specifications
        stratified = TRUE, # From specifications
        useCovariates = FALSE, # From specifications
        inversePtWeighting = FALSE, # From specifications
        prior = Cyclops::createPrior(
          priorType = "laplace", # From specifications
          useCrossValidation = TRUE # From specifications
        ),
        control = Cyclops::createControl(
          cvType = "auto", # From specifications
          seed = 1,
          resetCoefficients = TRUE, # From specifications
          startingVariance = 0.01, # From specifications
          tolerance = 2e-07, # From specifications
          cvRepetitions = 10, # From specifications
          noiseLevel = "quiet", # From specifications
          fold = 10 # From specifications
        )
      )
      
      # createStudyPopArgs from specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From specifications
        firstExposureOnly = FALSE, # From specifications
        washoutPeriod = 0, # From specifications
        removeDuplicateSubjects = "keep all", # From specifications
        censorAtNewRiskWindow = FALSE, # From specifications
        removeSubjectsWithPriorOutcome = TRUE, # From specifications
        priorOutcomeLookback = 99999, # From specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t]
      )
      
      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          if (studyStartDate == "") "Unrestricted" else studyStartDate,
          if (studyEndDate == "") "Unrestricted" else studyEndDate,
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

# Save the analysis specifications to a JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "uveitissafety", "uveitissafetyAnalysisSpecification.json")
)