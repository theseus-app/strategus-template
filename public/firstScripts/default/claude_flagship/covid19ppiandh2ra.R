################################################################################
# COVID-19 PPI and H2RA Study - Strategus Analysis Specifications
# 
# This script creates the analysis specifications for a comparative cohort study
# examining PPI vs H2RA in COVID-19 patients using the OHDSI Strategus framework.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
################################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from WebAPI
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Fetch cohort definitions from ATLAS:
# - 1794126: Target cohort (target1)
# - 1794132: Comparator cohort (comparator1)
# - 1794131: Outcome cohort (outcome1)
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

# Negative control outcomes
# Fetch negative control concept set (ID: 1888110) and resolve to concrete concepts
# These will be used to assess residual confounding and calibrate p-values
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
  mutate(cohortId = row_number() + 100) %>% # Negative control cohort IDs start at 101, 102, 103...
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no duplicate cohort IDs across positive and negative control cohorts
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to organize cohorts for analysis -------------------------

# Outcomes: Extract outcome cohort information
# Set cleanWindow to 365 days (standard washout period for outcomes)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Define the comparison: target1 (cohort 1) vs comparator1 (cohort 2)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate exclusions
# Based on the specification, no specific concepts are excluded (both id and name are null/empty)
# This creates an empty data frame, but the structure is maintained for potential future use
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# CohortGeneratorModule --------------------------------------------------------
# This module generates cohorts based on the definitions retrieved from ATLAS
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create shared resource for positive cohort definitions
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create shared resource for negative control outcome cohorts
# occurrenceType = "first": only the first occurrence of the outcome is counted
# detectOnDescendants = TRUE: include descendant concepts in the outcome definition
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications: generate cohort statistics for quality assessment
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule ------------------------------------------------------
# This module performs comprehensive diagnostics on the generated cohorts
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# Run diagnostics on all cohorts (target, comparator, and outcome)
# Multiple diagnostic checks are enabled to assess cohort quality
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE,              # Attrition statistics
  runIncludedSourceConcepts = TRUE,           # Source concepts used
  runOrphanConcepts = TRUE,                   # Concepts in data but not in vocabulary
  runTimeSeries = FALSE,                      # Time series analysis disabled
  runVisitContext = TRUE,                     # Visit context analysis
  runBreakdownIndexEvents = TRUE,             # Breakdown of index events
  runIncidenceRate = TRUE,                    # Incidence rate calculation
  runCohortRelationship = TRUE,               # Relationship between cohorts
  runTemporalCohortCharacterization = TRUE,   # Temporal characterization
  minCharacterizationMean = 0.01              # Minimum mean for characterization features
)

# CohortMethodModule -----------------------------------------------------------
# This module performs the comparative cohort analysis using propensity scores

# Study Period Configuration
# From specification: studyStartDate = "20200101", studyEndDate = "20200515"
# This restricts the analysis to the early COVID-19 pandemic period
studyPeriods <- tibble(
  studyStartDate = c("20200101"),  # January 1, 2020
  studyEndDate   = c("20200515")   # May 15, 2020
)

# Time-at-risk (TAR) Configuration
# From specification: riskWindowStart = 1, startAnchor = "cohort start"
#                     riskWindowEnd = 99999, endAnchor = "cohort start"
#                     minDaysAtRisk = 1
# This creates an on-treatment analysis starting 1 day after cohort entry
# and extending until the end of observation (up to 99999 days)
timeAtRisks <- tibble(
  label = c("OnTreatment"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(99999),
  endAnchor = c("cohort start")
)

# Propensity Score Adjustment Configurations
# From specification: three PS adjustment strategies are defined

# Strategy 1: No PS adjustment (NULL for both matchOnPsArgs and stratifyByPsArgs)
# This is handled by creating a psConfigList entry with method = "none"

# Strategy 2: Match on PS with maxRatio=4, caliper=0.2, caliperScale="standardized logit"
matchOnPsArgsList <- tibble(
  label = c("PS Matching 1:4"),
  maxRatio  = c(4),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# Strategy 3: Stratify by PS with numberOfStrata=5, baseSelection="all"
stratifyByPsArgsList <- tibble(
  label = c("PS Stratification 5 Strata"),
  numberOfStrata  = c(5),
  baseSelection = c("all")
)

# Build unified PS configuration list
# Each entry specifies: method (none/match/stratify), label, and parameters
psConfigList <- list()

# Add "no adjustment" configuration (corresponds to first psSettings in spec)
psConfigList[[length(psConfigList) + 1]] <- list(
  method = "none",
  label  = "No PS Adjustment",
  params = list()
)

# Convert matchOnPsArgsList rows to psConfigList entries
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

# Convert stratifyByPsArgsList rows to psConfigList entries
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

# Generate all analysis combinations -------------------------------------------
# Iterate through: study periods × time-at-risks × PS adjustments
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure PS adjustment method based on psConfigList entry
      if (psCfg$method == "none") {
        # No PS adjustment: both matchOnPsArgs and stratifyByPsArgs are NULL
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "match") {
        # Matching on PS with specified parameters
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        # Stratification by PS with specified parameters
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate Settings
      # Use default covariate settings (demographics, conditions, drugs, procedures, etc.)
      # addDescendantsToExclude = TRUE: when excluding concepts, also exclude their descendants
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcome List: combine outcomes of interest and negative controls
      # Outcomes of interest: outcomeOfInterest=TRUE, trueEffectSize=NA (unknown)
      # Negative controls: outcomeOfInterest=FALSE, trueEffectSize=1 (null effect expected)
      outcomeList <- append(
        # Outcomes of interest from oList
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999  # From specification: priorOutcomeLookBack = 99999
          )
        }),
        # Negative control outcomes
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1  # Null effect expected for negative controls
          )
        })
      )
      
      # Target-Comparator-Outcomes Configuration
      # Links each target-comparator pair with their outcomes and covariate exclusions
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # getDbCohortMethodDataArgs: Extract data from database
      # From specification:
      #   - maxCohortSize = 0 (no limit)
      #   - restrictToCommonPeriod = false
      #   - firstExposureOnly = true
      #   - washoutPeriod = 180
      #   - removeDuplicateSubjects = "keep first"
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = TRUE,
        washoutPeriod = 180,
        removeDuplicateSubjects = "keep first",
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs: Propensity score model settings
      # From specification:
      #   - maxCohortSizeForFitting = 250000
      #   - errorOnHighCorrelation = true
      #   - prior: priorType = "laplace", useCrossValidation = true
      #   - control: multiple convergence and CV parameters
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,  # Allow analysis to continue even if PS model fails
        estimator = "att",     # Average treatment effect on the treated
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
          cvRepetitions = 10,      # From specification: cvRepetitions = 10
          startingVariance = 0.01,
          fold = 10                # From specification: fold = 10
        )
      )

      # Covariate balance computation settings
      # computeSharedCovariateBalanceArgs: balance for all covariates (no filter)
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      
      # computeCovariateBalanceArgs: balance for Table 1 covariates only
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs: Outcome model settings
      # From specification:
      #   - modelType = "cox" (Cox proportional hazards)
      #   - stratified = true
      #   - useCovariates = false (no covariate adjustment beyond PS)
      #   - inversePtWeighting = false
      #   - prior and control settings for regularization
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
          cvType = "auto", 
          seed = 1, 
          resetCoefficients = TRUE,
          startingVariance = 0.01, 
          tolerance = 2e-07, 
          cvRepetitions = 10,          # From specification: cvRepetitions = 10
          noiseLevel = "quiet",        # From specification: noiseLevel = "quiet"
          fold = 10                    # From specification: fold = 10
        )
      )
      
      # createStudyPopArgs: Study population creation settings
      # From specification:
      #   - restrictToCommonPeriod = false
      #   - firstExposureOnly = false
      #   - washoutPeriod = 0
      #   - removeDuplicateSubjects = "keep all"
      #   - censorAtNewRiskWindow = false
      #   - removeSubjectsWithPriorOutcome = false (but priorOutcomeLookBack = 99999)
      #   - minDaysAtRisk = 1
      #   - TAR settings from timeAtRisks configuration
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
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Assemble complete analysis specification
      # Description includes study period, TAR label, and PS method for traceability
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

# Create CohortMethod module specifications
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,                   # Run all analyses
  refitPsForEveryOutcome = FALSE,             # Reuse PS model across outcomes
  refitPsForEveryStudyPopulation = FALSE,     # Reuse PS model across study populations
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the complete analysis specifications ---------------------------------
# Combine all modules into a single specification object
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save analysis specifications to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "covid19ppiandh2ra", "covid19ppiandh2raAnalysisSpecification.json")
)