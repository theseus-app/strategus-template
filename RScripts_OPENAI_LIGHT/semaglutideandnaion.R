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
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(ParallelLogger)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# EXACT names from Analysis Specifications:
# Target: target1 (id 1794126)
# Comparator: comparator1 (id 1794132)
# Outcome: outcome1 (id 1794131)

cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: 
    1794132, # Comparator: 
    1794131  # Outcome: 
  ),
  generateStats = TRUE
)

# Re-number cohorts to start from 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Set proper cohort names EXACTLY as specified
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1,]$cohortName <- "target1"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 2,]$cohortName <- "comparator1"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 3,]$cohortName <- "outcome1"

# Negative control outcomes
# Negative control concept set: id 1888110 name "negative"
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
  mutate(cohortId = row_number() + 100) %>% # negative controls start at 101...
  select(cohortId, cohortName, outcomeConceptId)

# Covariate selection (include/exclude) per Analysis Specifications
# Both include/exclude lists intentionally left empty (nulls in spec)
covariateSelection <- list(
  conceptsToInclude = data.frame(
    id = NA_integer_,
    name = ""
  ),
  conceptsToExclude = data.frame(
    id = NA_integer_,
    name = ""
  )
)

# Optional: If you want to define covariates to include instead of including them all
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortMethod data extraction settings
# Study periods
getDbCohortMethodDataArgs <- list(
  studyPeriods = data.frame(
    studyStartDate = 20171201,
    studyEndDate   = 20231231
  ),
  maxCohortSize = 0
)

# CreateStudyPopulation and TAR settings
# Time-at-risk settings (from Analysis Specifications)
timeAtRisks <- tibble(
  label = "default",
  riskWindowStart  = 0,
  startAnchor = "cohort start",
  riskWindowEnd  = 0,
  endAnchor = "cohort end",
  minDaysAtRisk = 1
)

createStudyPopArgs <- list(
  restrictToCommonPeriod = FALSE,
  firstExposureOnly = FALSE,
  washoutPeriod = 365,
  removeDuplicateSubjects = "keep all",
  censorAtNewRiskWindow = FALSE,
  removeSubjectsWithPriorOutcome = TRUE,
  priorOutcomeLookback = 99999,
  timeAtRisks = timeAtRisks$label[1],
  riskWindowStart = timeAtRisks$riskWindowStart[1],
  startAnchor = timeAtRisks$startAnchor[1],
  riskWindowEnd = timeAtRisks$riskWindowEnd[1],
  endAnchor = timeAtRisks$endAnchor[1],
  minDaysAtRisk = 1,
  maxDaysAtRisk = 99999
)

# Propensity Score settings
# Two PS configurations: 1) match on PS, 2) stratify by PS
matchOnPsArgsList <- tibble(
  label = c("PS_Match_1"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

stratifyByPsArgsList <- tibble(
  label = c("PS_Stratify_5"),
  numberOfStrata  = c(5),
  baseSelection = c("all")
)

# Build the PS configuration list (two configurations)
psConfigList <- list()

# If a data frame for "match on PS" exists and has rows, convert each row to a config
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

# If a data frame for "stratify by PS" exists and has rows, convert each row to a config
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

# O/W: If no PS configurations defined, psConfigList remains empty (no PS adjustment)

# Covariate balance and outcome model settings
# For the purposes of this specification, we use the defaults consistent with the Analysis Specs
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(addDescendantsToExclude = TRUE)

outcomeList <- append(
  lapply(seq_len(nrow(cohortDefinitionSet %>% filter(cohortId == 3))), function(i) {
    CohortMethod::createOutcome(
      outcomeId = 3,
      outcomeOfInterest = TRUE,
      trueEffectSize = NA,
      priorOutcomeLookback = 99999
    )
  }),
  lapply(negativeControlOutcomeCohortSet$outcomeConceptId, function(i) {
    CohortMethod::createOutcome(
      outcomeId = i,
      outcomeOfInterest = FALSE,
      trueEffectSize = 1
    )
  })
)

# Define target/comparator outcomes list
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  targetConceptId = NA_integer_,
  comparatorConceptId = NA_integer_,
  stringsAsFactors = FALSE
)

excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# GetDbCohortMethodDataArgs factory
getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
  restrictToCommonPeriod = TRUE,
  studyStartDate = 20171201,
  studyEndDate = 20231231,
  maxCohortSize = 0,
  covariateSettings = covariateSettings
)

# CreatePsArgs factory (for PS boundaries)
createPsArgs <- CohortMethod::createCreatePsArgs(
  maxCohortSizeForFitting = 250000,
  errorOnHighCorrelation = FALSE,
  stopOnError = FALSE,
  estimator = "att",
  prior = Cyclops::createPrior(
    priorType = "laplace",
    exclude = c(0),
    useCrossValidation = TRUE
  ),
  control = Cyclops::createControl(
    noiseLevel = "quiet",
    cvType = "auto",
    seed = 1,
    resetCoefficients = FALSE,
    tolerance = 2e-07,
    cvRepetitions = 10,
    startingVariance = 0.01
  )
)

# Compute covariate balance
computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
  maxCohortSize = 250000,
  covariateFilter = NULL
)
computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
  maxCohortSize = 250000,
  covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
)

# FitOutcomeModelArgs
fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
  modelType = "cox",
  stratified = TRUE,
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
    cvRepetitions = 1, 
    noiseLevel = "quiet"
  )
)

# CreateStudyPopArgs per time-at-risk and TAR settings
# Note: We follow the structure used in the Template to create a single analysis per TAR
# and per PS configuration.
# Build the list of CM analyses
cmAnalysisList <- list()
analysisId <- 1

# Study periods: exactly one period in this scenario
studyPeriods <- data.frame(
  studyStartDate = 20171201,
  studyEndDate   = 20231231,
  stringsAsFactors = FALSE
)

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {
    tarLabel <- timeAtRisks$label[t]

    # Iterate PS configurations
    if (length(psConfigList) == 0) {
      # If no PS adjustments requested, still create a basic analysis
      psCfg <- NULL
      matchOnPsArgs <- NULL
      stratifyByPsArgs <- NULL
      createPsArgs <- createPsArgs
    }

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
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
      } else {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- NULL
      }

      # Create the study population args for this TAR
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 365, # from analysis spec
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

      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          studyStartDate,
          studyEndDate,
          tarLabel,
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

cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = list( # Build target/comparator outcomes
    targetCohortId = 1,
    comparatorCohortId = 2
  ),
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

# Persist the analysis specification JSON to disk
outputPath <- file.path("inst", "semaglutideandnaion", "semaglutideandnaionAnalysisSpecification.json")
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  outputPath
)