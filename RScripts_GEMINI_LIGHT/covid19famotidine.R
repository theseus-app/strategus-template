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

# Shared Resources -------------------------------------------------------------
# Base URL for the OHDSI Atlas/WebAPI instance to retrieve cohort definitions and concept sets.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions for Target, Comparator, and Outcomes.
# The cohort IDs are retrieved from the Analysis Specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1 (from Analysis Specifications: cohortDefinitions.targetCohort.id)
    1794132, # Comparator: comparator1 (from Analysis Specifications: cohortDefinitions.comparatorCohort.id)
    1794131  # Outcome: outcome1 (from Analysis Specifications: cohortDefinitions.outcomeCohort[0].id)
  ),
  generateStats = TRUE # Keep generating stats as per template default
)

# Re-number cohorts to standard IDs (1 for Target, 2 for Comparator, 3 for Outcome)
# This standardizes the IDs for use within the analysis modules.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Target: target1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Comparator: comparator1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Outcome: outcome1

# Negative control outcomes are derived from a concept set definition.
# The concept set ID is retrieved from the Analysis Specifications.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # From Analysis Specifications: negativeControlConceptSet.id
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
  # Assign unique cohort IDs for negative controls, starting after the standard T/C/O IDs.
  mutate(cohortId = row_number() + 100) %>% # Target/comparator cohort IDs start with 1, 2, 3... negativeControl -> 101, 102, 103...
  select(cohortId, cohortName, outcomeConceptId)

# Check for any duplicated cohort IDs to prevent conflicts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts used in each analysis ---------------
# Outcomes list: filters the main cohort definition set for the outcome cohort (re-numbered to 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Filter for the re-numbered outcome cohort (outcome1)
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Template default; not specified in Analysis Specifications.

# Target and Comparator list for the CohortMethod analysis.
# Uses the re-numbered IDs and names from Analysis Specifications.
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered target cohort (target1)
  targetCohortName = "target1", # From Analysis Specifications: cohortDefinitions.targetCohort.name
  comparatorCohortId = 2, # Re-numbered comparator cohort (comparator1)
  comparatorCohortName = "comparator1" # From Analysis Specifications: cohortDefinitions.comparatorCohort.name
)

# Concepts to exclude from covariates.
# Analysis Specifications: covariateSelection.conceptsToExclude is an empty list.
# Therefore, this data frame is initialized as empty.
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(0),
  conceptName = character(0)
)

# Optional: If you want to define covariates to include instead of including them all
# Analysis Specifications: covariateSelection.conceptsToInclude is an empty list.
# Therefore, this section remains commented out.
# includedCovariateConcepts <- data.frame(
#   conceptId = numeric(0),
#   conceptName = character(0)
# )

# CohortGeneratorModule --------------------------------------------------------
# This module generates the cohorts based on the definitions.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Shared resource for cohort definitions.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Shared resource for negative control outcome cohort definitions.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Template default
  detectOnDescendants = TRUE # Template default
)
# Module specifications for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Generate cohort statistics as per template default
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# This module runs diagnostics on the generated cohorts.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # All defined cohorts will be diagnosed.
  runInclusionStatistics = TRUE, # Template default
  runIncludedSourceConcepts = TRUE, # Template default
  runOrphanConcepts = TRUE, # Template default
  runTimeSeries = FALSE, # Template default
  runVisitContext = TRUE, # Template default
  runBreakdownIndexEvents = TRUE, # Template default
  runIncidenceRate = TRUE, # Template default
  runCohortRelationship = TRUE, # Template default
  runTemporalCohortCharacterization = TRUE, # Template default
  minCharacterizationMean = 0.01 # Template default
)

# CohortMethodModule -----------------------------------------------------------
# This module performs the comparative effectiveness analysis.

# Study periods: time windows to restrict the study.
# From Analysis Specifications: getDbCohortMethodDataArgs.studyPeriods
studyPeriods <- tibble(
  studyStartDate = c("20200201"), # YYYYMMDD
  studyEndDate   = c("20200530")  # YYYYMMDD
)

# Time-at-risks (TARs) for the outcomes of interest.
# From Analysis Specifications: createStudyPopArgs.timeAtRisks
timeAtRisks <- tibble(
  label = c("1-30 days from cohort start"), # A descriptive label for this TAR
  riskWindowStart  = c(1), # From Analysis Specifications: timeAtRisks[0].riskWindowStart
  startAnchor = c("cohort start"), # From Analysis Specifications: timeAtRisks[0].startAnchor
  riskWindowEnd  = c(30), # From Analysis Specifications: timeAtRisks[0].riskWindowEnd
  endAnchor = c("cohort start") # From Analysis Specifications: timeAtRisks[0].endAnchor
)

# Propensity Score settings - match on PS
# From Analysis Specifications: propensityScoreAdjustment.psSettings (second entry)
matchOnPsArgsList <- tibble(
  label = c("PS Matching 1:1, Caliper 0.2 Std Logit"), # Descriptive label for PS method
  maxRatio  = c(1), # From Analysis Specifications: psSettings[1].matchOnPsArgs.maxRatio
  caliper = c(0.2), # From Analysis Specifications: psSettings[1].matchOnPsArgs.caliper
  caliperScale  = c("standardized logit") # From Analysis Specifications: psSettings[1].matchOnPsArgs.caliperScale
)

# Propensity Score settings - stratify by PS
# From Analysis Specifications: propensityScoreAdjustment.psSettings (first entry)
stratifyByPsArgsList <- tibble(
  label = c("PS Stratification 5 strata, All"), # Descriptive label for PS method
  numberOfStrata  = c(5), # From Analysis Specifications: psSettings[0].stratifyByPsArgs.numberOfStrata
  baseSelection = c("all") # From Analysis Specifications: psSettings[0].stratifyByPsArgs.baseSelection
)

# Build a single PS configuration list (each entry has: method, label, params)
# This structure allows iterating through different PS adjustment methods.
psConfigList <- list()

# Add matching PS configurations if defined in Analysis Specifications
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "match", # Identify the PS adjustment method
      label  = matchOnPsArgsList$label[i], # Human-readable label
      params = list( # Parameters for createMatchOnPsArgs
        maxRatio     = matchOnPsArgsList$maxRatio[i],
        caliper      = matchOnPsArgsList$caliper[i],
        caliperScale = matchOnPsArgsList$caliperScale[i]
      )
    )
  }
}

# Add stratification PS configurations if defined in Analysis Specifications
if (exists("stratifyByPsArgsList") && nrow(stratifyByPsArgsList) > 0) {
  for (i in seq_len(nrow(stratifyByPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "stratify", # Identify the PS adjustment method
      label  = stratifyByPsArgsList$label[i], # Human-readable label
      params = list( # Parameters for createStratifyByPsArgs
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection  = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}

# Iterate through all analysis setting combinations (study periods, TARs, PS methods)
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Configure PS adjustment arguments based on the current PS configuration.
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Template default
          stratificationColumns = c() # Template default
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Template default
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings
      # Analysis Specifications: covariateSelection.conceptsToInclude and conceptsToExclude are empty.
      # Therefore, using default covariate settings without specific inclusions/exclusions.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Template default; not specified in Analysis Specifications.
      )

      # List of outcomes for the analysis (study outcome + negative controls).
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # For real outcomes, trueEffectSize is NA
            priorOutcomeLookback = 99999 # Template default
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # For negative controls, trueEffectSize is 1
          )
        })
      )

      # Target-Comparator-Outcome list.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Analysis Specifications: covariateSelection.conceptsToExclude is empty.
          # The template placeholders (cmTcList$targetConceptId[i], cmTcList$comparatorConceptId[i])
          # are not actual columns in cmTcList and are likely illustrative.
          # Thus, only general excludedCovariateConcepts (which is empty) is used.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Arguments for retrieving cohort method data from the database.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Template default; not specified in Analysis Specifications.
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # From Analysis Specifications: getDbCohortMethodDataArgs.maxCohortSize
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # Settings are primarily from Analysis Specifications: propensityScoreAdjustment.createPsArgs.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From Analysis Specifications: createPsArgs.maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From Analysis Specifications: createPsArgs.errorOnHighCorrelation
        stopOnError = FALSE, # Template default; allows Strategus to continue even if PS model fails.
        estimator = "att", # Template default; not specified in Analysis Specifications.
        prior = Cyclops::createPrior( # From Analysis Specifications: createPsArgs.prior
          priorType = "laplace", # From Analysis Specifications: createPsArgs.prior.priorType
          exclude = c(0), # Template default
          useCrossValidation = TRUE # From Analysis Specifications: createPsArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # From Analysis Specifications: createPsArgs.control
          noiseLevel = "silent", # From Analysis Specifications: createPsArgs.control.noiseLevel
          cvType = "auto", # From Analysis Specifications: createPsArgs.control.cvType
          seed = 1, # Template default
          resetCoefficients = TRUE, # From Analysis Specifications: createPsArgs.control.resetCoefficients
          tolerance = 2e-07, # From Analysis Specifications: createPsArgs.control.tolerance
          cvRepetitions = 10, # From Analysis Specifications: createPsArgs.control.cvRepetitions
          startingVariance = 0.01 # From Analysis Specifications: createPsArgs.control.startingVariance
          # 'fold' is specified in Analysis Specifications but not a direct parameter in Cyclops::createControl with cvType = "auto".
        )
      )

      # Arguments for computing covariate balance (shared and detailed).
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Template default; not specified in Analysis Specifications.
        covariateFilter = NULL # Template default
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Template default; not specified in Analysis Specifications.
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Template default
      )

      # Arguments for fitting the outcome model.
      # Settings are primarily from Analysis Specifications: fitOutcomeModelArgs.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From Analysis Specifications: fitOutcomeModelArgs.modelType
        stratified = TRUE, # From Analysis Specifications: fitOutcomeModelArgs.stratified
        useCovariates = FALSE, # From Analysis Specifications: fitOutcomeModelArgs.useCovariates
        inversePtWeighting = FALSE, # From Analysis Specifications: fitOutcomeModelArgs.inversePtWeighting
        prior = Cyclops::createPrior( # From Analysis Specifications: fitOutcomeModelArgs.prior
          priorType = "laplace", # From Analysis Specifications: fitOutcomeModelArgs.prior.priorType
          useCrossValidation = TRUE # From Analysis Specifications: fitOutcomeModelArgs.prior.useCrossValidation
        ),
        control = Cyclops::createControl( # From Analysis Specifications: fitOutcomeModelArgs.control
          cvType = "auto", # From Analysis Specifications: fitOutcomeModelArgs.control.cvType
          seed = 1, # Template default
          resetCoefficients = TRUE, # From Analysis Specifications: fitOutcomeModelArgs.control.resetCoefficients
          startingVariance = 0.01, # From Analysis Specifications: fitOutcomeModelArgs.control.startingVariance
          tolerance = 2e-07, # From Analysis Specifications: fitOutcomeModelArgs.control.tolerance
          cvRepetitions = 10, # From Analysis Specifications: fitOutcomeModelArgs.control.cvRepetitions
          noiseLevel = "quiet" # From Analysis Specifications: fitOutcomeModelArgs.control.noiseLevel
          # 'fold' is specified in Analysis Specifications but not a direct parameter in Cyclops::createControl with cvType = "auto".
        )
      )

      # Arguments for creating the study population.
      # Settings are primarily from Analysis Specifications: createStudyPopArgs.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From Analysis Specifications: createStudyPopArgs.restrictToCommonPeriod
        firstExposureOnly = FALSE, # From Analysis Specifications: createStudyPopArgs.firstExposureOnly
        washoutPeriod = 0, # From Analysis Specifications: createStudyPopArgs.washoutPeriod
        removeDuplicateSubjects = "keep all", # From Analysis Specifications: createStudyPopArgs.removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # From Analysis Specifications: createStudyPopArgs.censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # From Analysis Specifications: createStudyPopArgs.removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 30, # From Analysis Specifications: createStudyPopArgs.priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1, # From Analysis Specifications: createStudyPopArgs.timeAtRisks[0].minDaysAtRisk
        maxDaysAtRisk = 99999 # Template default; not specified in Analysis Specifications.
      )

      # Append the current analysis settings to the list.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf( # Descriptive string for the analysis
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

# CohortMethodModule specifications, bundling all individual analyses.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Template default
  refitPsForEveryOutcome = FALSE, # Template default
  refitPsForEveryStudyPopulation = FALSE, # Template default
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Template default
)

# Create the final analysis specifications object ------------------------------------------
# This object combines all shared resources and module specifications.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The file path uses the study name from Analysis Specifications.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "covid19famotidine", "covid19famotidineAnalysisSpecification.json") # From Analysis Specifications: name
)