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
library(ROhdsiWebApi) # Required for WebAPI calls to retrieve cohort definitions and concept sets
library(CohortMethod) # Required for CohortMethod analysis settings
library(FeatureExtraction) # Required for CohortMethod covariate settings
library(Cyclops) # Required for CohortMethod prior and control settings for models

# Shared Resources -------------------------------------------------------------

# Base URL for the OHDSI WebAPI instance where cohort definitions are stored.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Retrieves cohort definitions from WebAPI based on specified IDs from `Analysis Specifications`.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: "target1" from Analysis Specifications
    1794132, # Comparator: "comparator1" from Analysis Specifications
    1794131  # Outcome: "outcome1" from Analysis Specifications
  ),
  generateStats = TRUE # Generate statistics for the cohorts
)

# Re-number cohorts to a simpler, sequential scheme (1, 2, 3...)
# This mapping helps in referring to cohorts within the analysis specifications
# with consistent, short identifiers.
# Target cohort (original ID 1794126) is mapped to ID 1.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
# Comparator cohort (original ID 1794132) is mapped to ID 2.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
# Outcome cohort (original ID 1794131) is mapped to ID 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes
# Retrieves a concept set definition for negative controls from WebAPI.
# The `Analysis Specifications` define conceptSetId 1888110 for "negative".
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # From Analysis Specifications: "negativeControlConceptSet": {"id": 1888110}
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
  # Assign unique cohort IDs for negative controls, starting from 101 to avoid
  # conflict with target/comparator/outcome cohorts (1, 2, 3).
  mutate(cohortId = row_number() + 100) %>% 
  select(cohortId, cohortName, outcomeConceptId)

# Critical check: Ensure no duplicate cohort IDs exist across study cohorts and negative controls.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create data frames to hold the cohorts we'll use in each analysis ---------------

# Outcomes list: Filters the re-numbered cohort definitions for the primary outcome cohort (ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Filters for the re-numbered outcome cohort (ID 3)
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # `cleanWindow` is set to 365 days, a common practice to ensure an event-free period
  # before the outcome definition's start. This is a default value as not specified.
  mutate(cleanWindow = 365) 

# Target and Comparator list for the CohortMethod analysis.
# Uses the re-numbered target (ID 1) and comparator (ID 2) cohorts.
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered ID for "target1"
  targetCohortName = "target1",
  comparatorCohortId = 2, # Re-numbered ID for "comparator1"
  comparatorCohortName = "comparator1"
)

# Concepts to explicitly exclude from covariate generation in CohortMethod.
# The `Analysis Specifications` for `covariateSelection.conceptsToExclude` is 
# `[{"id": null, "name": ""}]`, indicating no specific additional concepts to exclude.
# Thus, this data frame will be empty.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional: If you want to define specific covariates to include instead of including them all.
# The `Analysis Specifications` for `covariateSelection.conceptsToInclude` is 
# `[{"id": null, "name": ""}]`, so this section remains commented out.
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
# This module is responsible for generating the actual cohorts in the CDM.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
# Creates shared resource specifications for the study cohorts.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
# Creates shared resource specifications for negative control outcome cohorts.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Detect the first occurrence of the negative control outcome
  detectOnDescendants = TRUE # Include descendants of the negative control concepts when detecting
)
# Specifies settings for the CohortGenerator module.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Generate cohort statistics during cohort generation
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# This module runs diagnostics on the generated cohorts to ensure data quality.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # Run diagnostics for all study cohorts (IDs 1, 2, 3)
  runInclusionStatistics = TRUE, # Run inclusion rule statistics
  runIncludedSourceConcepts = TRUE, # Run included source concept analysis
  runOrphanConcepts = TRUE, # Run orphan concept analysis
  runTimeSeries = FALSE, # Set to FALSE as per analysis specifications/common practice if not explicitly needed
  runVisitContext = TRUE, # Run visit context analysis
  runBreakdownIndexEvents = TRUE, # Run breakdown index events analysis
  runIncidenceRate = TRUE, # Run incidence rate analysis
  runCohortRelationship = TRUE, # Run cohort relationship analysis
  runTemporalCohortCharacterization = TRUE, # Run temporal cohort characterization
  minCharacterizationMean = 0.01 # Minimum mean for covariate reporting in characterization
)

# CohortMethodModule -----------------------------------------------------------
# This module performs the core comparative effectiveness analysis using CohortMethod.

# Study periods: Defines time windows for the study.
# From `Analysis Specifications`: `getDbCohortMethodDataArgs.studyPeriods` specifies `null` for both
# `studyStartDate` and `studyEndDate`. This indicates no specific date restrictions for the study.
# To allow the loop to run once with no date restrictions, `NA` values are used,
# which will be interpreted as `NULL` by `createGetDbCohortMethodDataArgs`.
studyPeriods <- tibble(
  studyStartDate = as.Date(NA), # YYYYMMDD
  studyEndDate   = as.Date(NA)  # YYYYMMDD
)

# Time-at-risks (TARs): Defines the risk windows for outcomes.
# From `Analysis Specifications`: `createStudyPopArgs.timeAtRisks` specifies one TAR.
timeAtRisks <- tibble(
  label = c("1d_to_0d_relative_to_cohort_end"), # Descriptive label for this TAR
  riskWindowStart  = c(1), # From spec: `riskWindowStart: 1`
  startAnchor = c("cohort start"), # From spec: `startAnchor: "cohort start"`
  riskWindowEnd  = c(0), # From spec: `riskWindowEnd: 0`
  endAnchor = c("cohort end"), # From spec: `endAnchor: "cohort end"`
  minDaysAtRisk = c(1) # From spec: `minDaysAtRisk: 1`
) 

# Propensity Score settings - match on PS.
# From `Analysis Specifications`: `propensityScoreAdjustment.psSettings` specifies one matching setting.
matchOnPsArgsList <- tibble(
  label = c("1:1 match on PS, caliper 0.2 standardized logit"), # Descriptive label for this PS setting
  maxRatio  = c(1), # From spec: `maxRatio: 1`
  caliper = c(0.2), # From spec: `caliper: 0.2`
  caliperScale  = c("standardized logit") # From spec: `caliperScale: "standardized logit"`
) 

# Propensity Score settings - stratify by PS.
# From `Analysis Specifications`: `propensityScoreAdjustment.psSettings.stratifyByPsArgs` is `null`.
# Therefore, this list remains empty, and no stratification will be performed.
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c(),
) 

# Build a single PS configuration list. Each entry contains the method, label, and parameters.
psConfigList <- list()

# Convert "match on PS" settings from `matchOnPsArgsList` into the `psConfigList`.
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

# Convert "stratify by PS" settings from `stratifyByPsArgsList` into the `psConfigList`.
# This block will be skipped as `stratifyByPsArgsList` is empty based on the specifications.
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

# Iterate through all analysis setting combinations to create CohortMethod analyses.
cmAnalysisList <- list()
analysisId <- 1

# Loop through study periods (one entry: NA, NA for no restriction).
for (s in seq_len(nrow(studyPeriods))) {
  # Assign study dates. NA values will become NULL when passed to CohortMethod functions,
  # indicating no date restriction.
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  # Loop through time-at-risk settings (one entry from spec).
  for (t in seq_len(nrow(timeAtRisks))) {

    # Loop through propensity score adjustment configurations (one entry from spec: matching).
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Determine the PS adjustment method and create the corresponding arguments.
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Standard setting
          stratificationColumns = c() # No stratification columns specified
        )
        stratifyByPsArgs <- NULL # No stratification arguments needed for matching
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL # No matching arguments needed for stratification
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Define covariate settings for FeatureExtraction.
      # The `Analysis Specifications` does not provide specific FeatureExtraction settings,
      # so default settings are used, including descendants in exclusions.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE # Include descendants of concepts specified for exclusion
      )

      # Prepare the list of outcomes for the analysis.
      # This includes the primary outcome and all negative control outcomes.
      outcomeList <- append(
        # Primary outcome (ID 3)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE, # This is the primary outcome of interest
            trueEffectSize = NA, # For true outcomes, the true effect size is unknown
            priorOutcomeLookback = 99999 # This value is overwritten by `createStudyPopArgs` `priorOutcomeLookback`
          )
        }),
        # Negative control outcomes (IDs 101 onwards)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE, # These are not outcomes of interest for the primary analysis
            trueEffectSize = 1 # By definition, the true effect size for negative controls is 1 (null effect)
          )
        })
      )

      # Create target-comparator-outcome combinations for the analysis.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i], # Target cohort ID (1)
          comparatorId = cmTcList$comparatorCohortId[i], # Comparator cohort ID (2)
          outcomes = outcomeList,
          # Excluded covariate concept IDs.
          # The `Analysis Specifications` for `covariateSelection.conceptsToExclude`
          # is `[{"id": null, "name": ""}]`, so `excludedCovariateConcepts` data frame is empty.
          # The template had placeholders for target/comparator concept IDs which are not
          # provided in the spec and not defined in `cmTcList`, so they are removed.
          excludedCovariateConceptIds = c(excludedCovariateConcepts$conceptId) 
        )
      }

      # Arguments for `getDbCohortMethodData`.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # From template, for CohortMethod package's internal consistency
        studyStartDate = studyStartDate, # NULL from `as.Date(NA)` for no restriction
        studyEndDate = studyEndDate, # NULL from `as.Date(NA)` for no restriction
        maxCohortSize = 0, # From `Analysis Specifications`: `getDbCohortMethodDataArgs.maxCohortSize: 0`
        covariateSettings = covariateSettings
      )

      # Arguments for `createPs`.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From `Analysis Specifications`: `createPsArgs.maxCohortSizeForFitting: 250000`
        errorOnHighCorrelation = TRUE, # From `Analysis Specifications`: `createPsArgs.errorOnHighCorrelation: true`
        stopOnError = FALSE, # Setting to FALSE to allow Strategus to complete all CM operations;
                             # if a PS model cannot be fitted, the equipoise diagnostic should fail.
        estimator = "att", # Estimator for average treatment effect on the treated
        prior = Cyclops::createPrior( # Prior settings for the PS model regularization
          priorType = "laplace", # From `Analysis Specifications`: `createPsArgs.prior.priorType: "laplace"`
          exclude = c(0), # Exclude the intercept from regularization
          useCrossValidation = TRUE # From `Analysis Specifications`: `createPsArgs.prior.useCrossValidation: true`
        ),
        control = Cyclops::createControl( # Control settings for the PS model optimization
          noiseLevel = "silent", # From `Analysis Specifications`: `createPsArgs.control.noiseLevel: "silent"`
          cvType = "auto", # From `Analysis Specifications`: `createPsArgs.control.cvType: "auto"`
          fold = 10, # From `Analysis Specifications`: `createPsArgs.control.fold: 10`
          seed = 1, # From template, not explicitly in spec. Keep default.
          resetCoefficients = TRUE, # From `Analysis Specifications`: `createPsArgs.control.resetCoefficients: true`
          tolerance = 2e-07, # From `Analysis Specifications`: `createPsArgs.control.tolerance: 2e-7`
          cvRepetitions = 10, # From `Analysis Specifications`: `createPsArgs.control.cvRepetitions: 10`
          startingVariance = 0.01 # From `Analysis Specifications`: `createPsArgs.control.startingVariance: 0.01`
        )
      )

      # Arguments for computing shared covariate balance (e.g., prior to PS adjustment).
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # From template, no specific override in analysis spec
        covariateFilter = NULL # No specific filter applied here
      )
      # Arguments for computing covariate balance after PS adjustment.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # From template, no specific override in analysis spec
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Use standard Table 1 covariates
      )

      # Arguments for `fitOutcomeModel`.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From `Analysis Specifications`: `fitOutcomeModelArgs.modelType: "cox"`
        stratified = FALSE, # From `Analysis Specifications`: `fitOutcomeModelArgs.stratified: false`
        useCovariates = FALSE, # From `Analysis Specifications`: `fitOutcomeModelArgs.useCovariates: false`
        inversePtWeighting = FALSE, # From `Analysis Specifications`: `fitOutcomeModelArgs.inversePtWeighting: false`
        prior = Cyclops::createPrior( # Prior settings for the outcome model regularization
          priorType = "laplace", # From `Analysis Specifications`: `fitOutcomeModelArgs.prior.priorType: "laplace"`
          useCrossValidation = TRUE # From `Analysis Specifications`: `fitOutcomeModelArgs.prior.useCrossValidation: true`
        ),
        control = Cyclops::createControl( # Control settings for the outcome model optimization
          cvType = "auto", # From `Analysis Specifications`: `fitOutcomeModelArgs.control.cvType: "auto"`
          fold = 10, # From `Analysis Specifications`: `fitOutcomeModelArgs.control.fold: 10`
          seed = 1, # From template, not explicitly in spec. Keep default.
          resetCoefficients = TRUE, # From `Analysis Specifications`: `fitOutcomeModelArgs.control.resetCoefficients: true`
          startingVariance = 0.01, # From `Analysis Specifications`: `fitOutcomeModelArgs.control.startingVariance: 0.01`
          tolerance = 2e-07, # From `Analysis Specifications`: `fitOutcomeModelArgs.control.tolerance: 2e-7`
          cvRepetitions = 10, # From `Analysis Specifications`: `fitOutcomeModelArgs.control.cvRepetitions: 10`
          noiseLevel = "quiet" # From `Analysis Specifications`: `fitOutcomeModelArgs.control.noiseLevel: "quiet"`
        )
      )

      # Arguments for `createStudyPopulation`.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From `Analysis Specifications`: `createStudyPopArgs.restrictToCommonPeriod: false`
        firstExposureOnly = FALSE, # From `Analysis Specifications`: `createStudyPopArgs.firstExposureOnly: false`
        washoutPeriod = 0, # From `Analysis Specifications`: `createStudyPopArgs.washoutPeriod: 0`
        removeDuplicateSubjects = "keep all", # From `Analysis Specifications`: `createStudyPopArgs.removeDuplicateSubjects: "keep all"`
        censorAtNewRiskWindow = FALSE, # From `Analysis Specifications`: `createStudyPopArgs.censorAtNewRiskWindow: false`
        removeSubjectsWithPriorOutcome = TRUE, # From `Analysis Specifications`: `createStudyPopArgs.removeSubjectsWithPriorOutcome: true`
        priorOutcomeLookback = 365, # From `Analysis Specifications`: `createStudyPopArgs.priorOutcomeLookBack: 365`
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From `Analysis Specifications`: `timeAtRisks.minDaysAtRisk: 1`
        maxDaysAtRisk = 99999 # From template, not in spec. Keep default for maximum duration.
      )

      # Append the current combination of settings to the CohortMethod analysis list.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        # Generate a descriptive string for this analysis combination.
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          # Format study dates, handling NA for cases with no date restriction.
          ifelse(is.na(studyStartDate), "NoStart", format(studyStartDate, "%Y%m%d")),
          ifelse(is.na(studyEndDate), "NoEnd", format(studyEndDate, "%Y%m%d")),
          timeAtRisks$label[t],
          psCfg$label
        ),
        getDbCohortMethodDataArgs = getDbCohortMethodDataArgs,
        createStudyPopArgs = createStudyPopArgs,
        createPsArgs = createPsArgs,
        matchOnPsArgs = matchOnPsArgs, # Will contain matching arguments
        stratifyByPsArgs = stratifyByPsArgs, # Will be NULL in this case as no stratification is specified
        computeSharedCovariateBalanceArgs = computeSharedCovariateBalanceArgs,
        computeCovariateBalanceArgs = computeCovariateBalanceArgs,
        fitOutcomeModelArgs = fitOutcomeModelArgs
      )
      analysisId <- analysisId + 1
    }
  }
}

# CohortMethod module settings creator and specifications.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # No specific analyses are excluded
  refitPsForEveryOutcome = FALSE, # Propensity Score model is not refitted for each outcome
  refitPsForEveryStudyPopulation = FALSE, # Propensity Score model is not refitted for each study population
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Use default CohortMethod diagnostic thresholds
)

# Create the final analysis specifications object for Strategus ----------------
# This object aggregates all shared resources and module specifications, forming
# the complete study design for Strategus execution.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The study name from `Analysis Specifications` is "tramadolcodein".
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "tramadolcodein", "tramadolcodeinAnalysisSpecification.json")
)