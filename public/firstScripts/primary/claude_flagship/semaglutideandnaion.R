################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script creates the analysis specifications for the semaglutideandnaion study
# using the OHDSI Strategus package.
# 
# Study Overview:
# - Target Cohort: target1 (ID: 1794126)
# - Comparator Cohort: comparator1 (ID: 1794132)
# - Outcome Cohort: outcome1 (ID: 1794131)
# - Negative Control Concept Set: negative (ID: 1888110)
# - Study Period: 2017-12-01 to 2023-12-31
# - Washout Period: 365 days
# - Propensity Score Adjustment: 1:1 matching with caliper 0.2 on standardized logit scale
# - Outcome Model: Cox proportional hazards (unstratified)
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Define the base URL for the OHDSI WebAPI to retrieve cohort definitions
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Export cohort definitions from ATLAS using the specified cohort IDs
# These cohorts define the target, comparator, and outcome populations
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to use sequential IDs for internal processing
# This ensures consistent referencing throughout the analysis
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative Control Outcomes ----------------------------------------------------
# Retrieve negative control outcomes from the concept set definition
# Negative controls are used to detect potential systematic bias in the study
# Concept Set ID: 1888110 (name: negative)
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
  # Assign cohort IDs starting from 101 to avoid conflicts with main cohorts
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Validate that there are no duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create Data Frames for Analysis Configuration --------------------------------

# Outcomes list: Define the outcome cohorts for the analysis
# cleanWindow: 365 days - period to look back for prior outcomes
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator list for CohortMethod analysis
# Defines the exposure groups being compared
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Excluded Covariate Concepts --------------------------------------------------
# No specific concepts to exclude as per the analysis specifications
# (conceptsToExclude id is null in the specifications)
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# CohortGeneratorModule --------------------------------------------------------
# This module generates the cohorts defined above in the target database
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource specifications for cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource specifications for negative control outcome cohorts
# occurrenceType = "first": Use only the first occurrence of each negative control outcome
# detectOnDescendants = TRUE: Include descendant concepts when identifying outcomes
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create module specifications for cohort generation
# generateStats = TRUE: Generate inclusion rule statistics
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# This module runs diagnostic analyses on the generated cohorts
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE,      # Statistics on inclusion rule impact
  runIncludedSourceConcepts = TRUE,   # Source concepts included in cohort
  runOrphanConcepts = TRUE,           # Concepts that may be missing from definitions
  runTimeSeries = FALSE,              # Time series of cohort entry
  runVisitContext = TRUE,             # Visit context at cohort entry
  runBreakdownIndexEvents = TRUE,     # Breakdown of index events
  runIncidenceRate = TRUE,            # Incidence rate calculations
  runCohortRelationship = TRUE,       # Relationships between cohorts
  runTemporalCohortCharacterization = TRUE,  # Temporal characterization
  minCharacterizationMean = 0.01      # Minimum mean for characterization features
)

# CohortMethodModule -----------------------------------------------------------
# This module performs the comparative cohort analysis

# Study Periods ----------------------------------------------------------------
# Define the study observation window
# studyStartDate: 2017-12-01 (YYYYMMDD format: 20171201)
# studyEndDate: 2023-12-31 (YYYYMMDD format: 20231231)
studyPeriods <- tibble(
  studyStartDate = c("20171201"),
  studyEndDate   = c("20231231")
)

# Time-at-Risk (TAR) Settings --------------------------------------------------
# Define when outcomes are counted relative to exposure
# riskWindowStart: 1 day after cohort start (to avoid immortal time bias)
# riskWindowEnd: 0 days relative to cohort end (end of exposure)
# startAnchor: "cohort start" - risk window starts relative to exposure start
# endAnchor: "cohort end" - risk window ends relative to exposure end
# minDaysAtRisk: 1 - minimum days a subject must be at risk to be included
timeAtRisks <- tibble(
  label = c("On Treatment"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
) 

# Propensity Score Settings - Match on PS --------------------------------------
# Configure propensity score matching parameters
# maxRatio: 1 - 1:1 matching (one comparator per target)
# caliper: 0.2 - maximum allowed difference in PS for a match
# caliperScale: "standardized logit" - caliper applied on standardized logit of PS
matchOnPsArgsList <- tibble(
  label = c("1:1 PS Matching"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
) 

# Propensity Score Settings - Stratify by PS -----------------------------------
# No stratification is used in this analysis (stratifyByPsArgs is null in specifications)
stratifyByPsArgsList <- tibble(
  label = character(0),
  numberOfStrata  = integer(0),
  baseSelection = character(0)
) 

# Build PS Configuration List --------------------------------------------------
# Combine all PS adjustment methods into a single configuration list
psConfigList <- list()

# Add matching configurations if defined
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

# Add stratification configurations if defined
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

# Build Analysis List ----------------------------------------------------------
# Iterate through all combinations of study periods, time-at-risks, and PS settings
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment method based on the current configuration
      if (psCfg$method == "match") {
        # PS Matching configuration
        # allowReverseMatch = FALSE: Only match comparators to targets, not vice versa
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # PS Stratification configuration
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate Settings ------------------------------------------------------
      # Use default covariate settings from FeatureExtraction
      # addDescendantsToExclude = TRUE: When excluding concepts, also exclude their descendants
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcome List ------------------------------------------------------------
      # Create outcome objects for both outcomes of interest and negative controls
      outcomeList <- append(
        # Outcomes of interest (from oList)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,      # This is a primary outcome
            trueEffectSize = NA,           # Unknown true effect (what we're estimating)
            priorOutcomeLookback = 99999   # Look back period for prior outcomes (from specifications)
          )
        }),
        # Negative control outcomes
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,     # Not a primary outcome
            trueEffectSize = 1             # True effect is null (HR = 1) for negative controls
          )
        })
      )
      
      # Target-Comparator-Outcomes List -----------------------------------------
      # Link target and comparator cohorts with their outcomes
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude treatment concepts from covariates to avoid confounding
          excludedCovariateConceptIds = c(
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # Get Database Cohort Method Data Arguments -------------------------------
      # Configure how to extract data from the database
      # restrictToCommonPeriod: FALSE (from specifications)
      # maxCohortSize: 0 means no limit (from specifications)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Create Propensity Score Arguments ---------------------------------------
      # Configure the propensity score model fitting
      # maxCohortSizeForFitting: 250000 - maximum subjects for PS model fitting
      # errorOnHighCorrelation: TRUE - stop if high correlation detected
      # Prior: Laplace prior with cross-validation for regularization
      # Control: Cyclops control parameters for optimization
      createPsArgs = CohortMethod::createCreatePsArgs(
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
          fold = 10,
          startingVariance = 0.01
        )
      )

      # Covariate Balance Arguments ---------------------------------------------
      # Configure covariate balance computation for diagnostics
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit Outcome Model Arguments ---------------------------------------------
      # Configure the outcome model (Cox proportional hazards)
      # modelType: "cox" - Cox proportional hazards model
      # stratified: FALSE - unstratified analysis (from specifications)
      # useCovariates: FALSE - no additional covariates in outcome model
      # inversePtWeighting: FALSE - not using IPW (from specifications)
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
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
          fold = 10,
          noiseLevel = "quiet"
        )
      )
      
      # Create Study Population Arguments ---------------------------------------
      # Configure how to create the study population
      # restrictToCommonPeriod: FALSE (from specifications)
      # firstExposureOnly: FALSE - include all exposures (from specifications)
      # washoutPeriod: 365 days - require 365 days of prior observation
      # removeDuplicateSubjects: "keep all" - keep all subjects (from specifications)
      # censorAtNewRiskWindow: FALSE (from specifications)
      # removeSubjectsWithPriorOutcome: TRUE - exclude subjects with prior outcome
      # priorOutcomeLookBack: 99999 days - look back period for prior outcomes
      # minDaysAtRisk: 1 - minimum days at risk required (from specifications)
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
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

      # Create CM Analysis Object -----------------------------------------------
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

# Create the Analysis Specifications -------------------------------------------
# Combine all modules into a single analysis specification object
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the Analysis Specifications to JSON -------------------------------------
# Save the complete analysis specification to a JSON file for execution
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "semaglutideandnaion", "semaglutideandnaionAnalysisSpecification.json")
)