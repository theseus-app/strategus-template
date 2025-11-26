################################################################################
# CohortMethod Analysis Specifications for antivegfkidney Study
# 
# This script creates analysis specifications for a comparative effectiveness study
# using the OHDSI Strategus package. It defines cohort definitions, propensity score
# adjustment methods, and outcome model fitting parameters.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
# ##############################################################################

library(dplyr)
library(Strategus)

# Shared Resources: Cohort Definitions ----------------------------------------
# Define the target, comparator, and outcome cohorts for this analysis
# These cohorts should be pre-defined in your ATLAS instance or CDM

# Target Cohort: ID 1794126, name "target1"
targetCohortId <- 1
targetCohortName <- "target1"

# Comparator Cohort: ID 1794132, name "comparator1"
comparatorCohortId <- 2
comparatorCohortName <- "comparator1"

# Outcome Cohorts: ID 1794131, name "outcome1"
outcomeCohortId <- 3
outcomeCohortName <- "outcome1"

# Negative Control Outcome Concepts: ID 1888110, name "negative"
# These are used to evaluate systematic bias in the analysis
negativeControlConceptSetId <- 1888110
negativeControlConceptSetName <- "negative"

# Create data frames to organize cohorts for analysis
# Outcomes list
oList <- data.frame(
  outcomeCohortId = 3,
  outcomeCohortName = "outcome1",
  cleanWindow = 365
)

# Target and Comparator combinations for CohortMethod analysis
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# CohortGeneratorModule -------------------------------------------------------
# This module generates the cohorts used in the analysis
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("target1", "comparator1", "outcome1"),
    sql = c("", "", ""), # SQL would be populated from ATLAS exports
    json = c("", "", ""), # JSON would be populated from ATLAS exports
    logicDescription = c("", "", "")
  )
)

cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings --------------------------------------------
# This module runs diagnostic checks on the generated cohorts
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = c(1, 2, 3),
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

# CohortMethodModule ----------------------------------------------------------
# This module performs the main comparative effectiveness analysis

# Study Periods: Define time window for the study
# studyStartDate and studyEndDate are NULL, meaning no specific time restriction
studyPeriods <- tibble(
  studyStartDate = NA_character_,  # NULL means no start date restriction
  studyEndDate = NA_character_      # NULL means no end date restriction
)

# Time-at-Risk (TAR) Definition
# Risk window starts 1 day after cohort start and ends at cohort end
# minDaysAtRisk = 1 ensures subjects must have at least 1 day of follow-up
timeAtRisks <- tibble(
  riskWindowStart = 1,
  startAnchor = "cohort start",
  riskWindowEnd = 0,
  endAnchor = "cohort end",
  minDaysAtRisk = 1
)

# Propensity Score Settings - Match on PS
# Matching parameters: maxRatio = 1:1 matching, caliper = 0.2 (standardized logit scale)
matchOnPsArgsList <- tibble(
  label = "Match 1:1 with caliper 0.2 (std logit)",
  maxRatio = 1,
  caliper = 0.2,
  caliperScale = "standardized logit"
)

# Build PS configuration list with matching method
psConfigList <- list()
if (nrow(matchOnPsArgsList) > 0) {
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

# Initialize CohortMethod analysis list
cmAnalysisList <- list()
analysisId <- 1

# Iterate through study periods, time-at-risk definitions, and PS configurations
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure propensity score matching arguments
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      }

      # Covariate settings: Use default covariates (all conditions, drugs, procedures, etc.)
      # with descendants included and no specific inclusions/exclusions
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Create outcome list: includes true outcomes of interest and negative control outcomes
      outcomeList <- list()
      
      # Add outcomes of interest (from oList)
      for (i in seq_len(nrow(oList))) {
        outcomeList[[length(outcomeList) + 1]] <- CohortMethod::createOutcome(
          outcomeId = oList$outcomeCohortId[i],
          outcomeOfInterest = TRUE,
          trueEffectSize = NA,
          priorOutcomeLookback = 99999  # Look back entire history for prior outcomes
        )
      }
      
      # Create target-comparator-outcomes combinations
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = c()  # No specific covariates excluded
        )
      }

      # getDbCohortMethodData arguments: Extract data from CDM
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,  # Do not restrict to common period
        studyStartDate = if (is.na(studyStartDate)) NULL else studyStartDate,
        studyEndDate = if (is.na(studyEndDate)) NULL else studyEndDate,
        maxCohortSize = 0,  # 0 means no limit
        covariateSettings = covariateSettings
      )

      # Create propensity score model arguments
      # Uses Laplace prior with cross-validation for regularization
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # Limit PS model fitting to 250k subjects
        errorOnHighCorrelation = TRUE,     # Raise error if covariates are highly correlated
        stopOnError = FALSE,               # Continue even if PS model fails
        estimator = "att",                 # Average treatment effect on the treated
        prior = Cyclops::createPrior(
          priorType = "laplace",           # Laplace (L1) regularization
          exclude = c(0),                  # Do not regularize intercept
          useCrossValidation = TRUE        # Use cross-validation to select penalty
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",           # Minimize console output
          cvType = "auto",                 # Automatic CV type selection
          seed = 1,
          resetCoefficients = TRUE,        # Reset coefficients between CV folds
          tolerance = 2e-07,               # Convergence tolerance
          cvRepetitions = 10,              # Number of CV repetitions (per analysis spec)
          fold = 10,                       # Number of CV folds
          startingVariance = 0.01          # Starting variance for optimization
        )
      )

      # Covariate balance computation arguments
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000
      )
      
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model fitting arguments
      # Cox proportional hazards model without stratification
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",              # Cox proportional hazards model
        stratified = FALSE,             # Do not stratify by matched pairs
        useCovariates = FALSE,          # Do not adjust for covariates in outcome model
        inversePtWeighting = FALSE,     # Do not use inverse probability weighting
        prior = Cyclops::createPrior(
          priorType = "laplace",        # Laplace regularization
          useCrossValidation = TRUE     # Use cross-validation for penalty selection
        ),
        control = Cyclops::createControl(
          cvType = "auto",
          seed = 1,
          resetCoefficients = TRUE,
          tolerance = 2e-07,
          cvRepetitions = 10,
          fold = 10,
          startingVariance = 0.01,
          noiseLevel = "quiet"          # Minimal output
        )
      )

      # Create study population arguments
      # Defines inclusion/exclusion criteria and follow-up period
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,           # Do not restrict to common period
        firstExposureOnly = TRUE,                 # Only include first exposure per subject
        washoutPeriod = 365,                      # 1-year washout before exposure
        removeDuplicateSubjects = "keep all",     # Keep all duplicate subjects
        censorAtNewRiskWindow = FALSE,            # Do not censor at new exposure
        removeSubjectsWithPriorOutcome = TRUE,    # Exclude subjects with prior outcome
        priorOutcomeLookback = 99999,             # Look back entire history for prior outcomes
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t]
      )

      # Add this analysis configuration to the list
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study period: %s to %s; TAR: %d to %d (%s to %s); PS: %s",
          if (is.na(studyStartDate)) "unrestricted" else studyStartDate,
          if (is.na(studyEndDate)) "unrestricted" else studyEndDate,
          timeAtRisks$riskWindowStart[t],
          timeAtRisks$riskWindowEnd[t],
          timeAtRisks$startAnchor[t],
          timeAtRisks$endAnchor[t],
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

# CohortMethod Module Specifications ------------------------------------------
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the Complete Analysis Specifications --------------------------------
# Combine all module specifications into a single analysis specification object
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save analysis specifications to JSON file
# This JSON file can be used to execute the analysis using Strategus
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "antivegfkidney", "antivegfkidneyAnalysisSpecification.json")
)

# Confirmation message
cat("Analysis specifications for 'antivegfkidney' have been created and saved.\n")