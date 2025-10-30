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
library(tibble)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI" # Base URL for ATLAS/WebAPI

# Cohort Definitions - These are the primary cohorts for target, comparator, and outcome.
# IDs and names are extracted from Analysis Specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131 # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts for internal Strategus consistency (1 for target, 2 for comparator, 3 for outcome)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1 # Renumber target cohort ID
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2 # Renumber comparator cohort ID
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3 # Renumber outcome cohort ID

# Negative control outcomes - Concept set for generating negative control outcomes.
# The ID is taken from the Analysis Specifications.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # negativeControlConceptSet.id: negative
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
  # Assign unique cohort IDs for negative controls, starting from 101 to avoid conflicts
  # with target/comparator/outcome cohorts (1, 2, 3).
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)


if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: List of outcome cohorts for the study.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>% # Filtering for the re-numbered outcome cohort ID (3)
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Default clean window (not directly from JSON for this part)

# Target and Comparator for the CohortMethod analysis.
# Uses re-numbered IDs and names from Analysis Specifications.
cmTcList <- data.frame(
  targetCohortId = 1, # Re-numbered target cohort ID
  targetCohortName = "target1", # Name from Analysis Specifications
  comparatorCohortId = 2, # Re-numbered comparator cohort ID
  comparatorCohortName = "comparator1" # Name from Analysis Specifications
)

# For the CohortMethod LSPS (large-scale propensity score)
# Excluded covariate concepts: from covariateSelection.conceptsToExclude.
# In this case, the list is empty in the Analysis Specifications.
excludedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# Optional: If you want to define covariates to include instead of including them all
# includedCovariateConcepts: from covariateSelection.conceptsToInclude.
# In this case, the list is empty in the Analysis Specifications.
includedCovariateConcepts <- data.frame(
  conceptId = c(),
  conceptName = c()
)

# CohortGeneratorModule --------------------------------------------------------
# Configures the CohortGeneratorModule to generate the defined cohorts.
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first", # Default from template
  detectOnDescendants = TRUE # Default from template
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE # Default from template
)

# CohortDiagnoticsModule Settings ---------------------------------------------
# Configures the CohortDiagnosticsModule to run various diagnostics on the cohorts.
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId, # All cohort IDs defined above
  runInclusionStatistics = TRUE, # Default from template
  runIncludedSourceConcepts = TRUE, # Default from template
  runOrphanConcepts = TRUE, # Default from template
  runTimeSeries = FALSE, # Default from template
  runVisitContext = TRUE, # Default from template
  runBreakdownIndexEvents = TRUE, # Default from template
  runIncidenceRate = TRUE, # Default from template
  runCohortRelationship = TRUE, # Default from template
  runTemporalCohortCharacterization = TRUE, # Default from template
  minCharacterizationMean = 0.01 # Default from template
)

# CohortMethodModule -----------------------------------------------------------

# Study periods: Time windows for data extraction for CohortMethod.
# From getDbCohortMethodDataArgs.studyPeriods in Analysis Specifications.
studyPeriods <- tibble(
  studyStartDate = c("20100101", "20120101"), # YYYYMMDD
  studyEndDate   = c("20191231", "20191231") # YYYYMMDD
)

# Time-at-risks (TARs): Defines the risk windows for outcome assessment.
# From createStudyPopArgs.timeAtRisks in Analysis Specifications.
timeAtRisks <- tibble(
  label = c("TAR_0_CE_0_CE_Min1", "TAR_0_CS_9999_CS_Min1"), # Custom labels for identification
  riskWindowStart  = c(0, 0), # From riskWindowStart
  startAnchor = c("cohort end", "cohort start"), # From startAnchor
  riskWindowEnd  = c(0, 9999), # From riskWindowEnd
  endAnchor = c("cohort end", "cohort start"), # From endAnchor
  minDaysAtRisk = c(1, 1) # From minDaysAtRisk
)

# Propensity Score settings - match on PS
# From propensityScoreAdjustment.psSettings where matchOnPsArgs is not null.
matchOnPsArgsList <- tibble(
  label = c("Match_Ratio0_Caliper0.2_StandardizedLogit"), # Custom label
  maxRatio  = c(0), # From matchOnPsArgs.maxRatio
  caliper = c(0.2), # From matchOnPsArgs.caliper
  caliperScale  = c("standardized logit") # From matchOnPsArgs.caliperScale
)

# Propensity Score settings - stratify by PS
# From propensityScoreAdjustment.psSettings where stratifyByPsArgs is not null.
stratifyByPsArgsList <- tibble(
  label = c("Stratify_5_All"), # Custom label
  numberOfStrata  = c(5), # From stratifyByPsArgs.numberOfStrata
  baseSelection = c("all") # From stratifyByPsArgs.baseSelection
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# If a data frame for "match on PS" exists and has rows, convert each row to a config
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    # Append a new element at the end of psConfigList
    psConfigList[[length(psConfigList) + 1]] <- list(
      # Identify the PS adjustment method for this config
      method = "match",
      # Human-readable label to carry through into descriptions
      label  = matchOnPsArgsList$label[i],
      # Parameter bundle passed to createMatchOnPsArgs later
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
    # Append a new element at the end of psConfigList
    psConfigList[[length(psConfigList) + 1]] <- list(
      # Identify the PS adjustment method for this config
      method = "stratify",
      # Human-readable label to carry through into descriptions
      label  = stratifyByPsArgsList$label[i],
      # Parameter bundle passed to createStratifyByPsArgs later
      params = list(
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection  = stratifyByPsArgsList$baseSelection[i]
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
      
      # Propensity score adjustment settings
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE, # Default from template
          stratificationColumns = c() # Default from template
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(), # Default from template
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings for feature extraction.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE, # Default from template
        # includedCovariateConceptIds and excludedCovariateConceptIds are empty based on Analysis Specifications
        includedCovariateConceptIds = includedCovariateConcepts$conceptId,
        excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
      )

      # Outcome list includes both outcomes of interest and negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA, # For true outcomes, true effect size is NA
            priorOutcomeLookback = 99999 # Default from template
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1 # For negative controls, true effect size is 1 (null effect)
          )
        })
      )
      
      # Target-Comparator-Outcome combinations.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Excluded covariate concepts: only `excludedCovariateConcepts` are used as per JSON.
          # The original template had placeholders (cmTcList$targetConceptId[i] etc.) that are not in the JSON.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # Arguments for fetching cohort method data from the database.
      # From getDbCohortMethodDataArgs in Analysis Specifications.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # Default from template
        studyStartDate = studyStartDate, # From current study period loop iteration
        studyEndDate = studyEndDate, # From current study period loop iteration
        maxCohortSize = 0, # From getDbCohortMethodDataArgs.maxCohortSize
        covariateSettings = covariateSettings
      )

      # Arguments for creating propensity scores.
      # From propensityScoreAdjustment.createPsArgs in Analysis Specifications.
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000, # From maxCohortSizeForFitting
        errorOnHighCorrelation = TRUE, # From errorOnHighCorrelation
        stopOnError = FALSE, # Default from template
        estimator = "att", # Default from template
        prior = Cyclops::createPrior(
          priorType = "laplace", # From prior.priorType
          exclude = c(0), # Default from template
          useCrossValidation = TRUE # From prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent", # From control.noiseLevel
          cvType = "auto", # From control.cvType
          seed = 1, # Default from template
          resetCoefficients = TRUE, # From control.resetCoefficients
          tolerance = 2e-07, # From control.tolerance
          cvRepetitions = 10, # From control.cvRepetitions
          startingVariance = 0.01, # From control.startingVariance
          fold = 10 # From control.fold
        )
      )

      # Arguments for computing covariate balance for shared covariates.
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default from template
        covariateFilter = NULL # Default from template
      )
      
      # Arguments for computing covariate balance.
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000, # Default from template
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications() # Default from template
      )

      # Arguments for fitting the outcome model.
      # From fitOutcomeModelArgs in Analysis Specifications.
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox", # From modelType
        stratified = TRUE, # From stratified
        useCovariates = FALSE, # From useCovariates
        inversePtWeighting = FALSE, # From inversePtWeighting
        prior = Cyclops::createPrior(
          priorType = "laplace", # From prior.priorType
          useCrossValidation = TRUE # From prior.useCrossValidation
        ),
        control = Cyclops::createControl(
          cvType = "auto", # From control.cvType
          seed = 1, # Default from template
          resetCoefficients = TRUE, # From control.resetCoefficients
          startingVariance = 0.01, # From control.startingVariance
          tolerance = 2e-07, # From control.tolerance
          cvRepetitions = 10, # From control.cvRepetitions
          noiseLevel = "quiet", # From control.noiseLevel
          fold = 10 # From control.fold (added for consistency, not explicitly in fitOutcomeModelArgs in template)
        )
      )
      
      # Arguments for creating the study population.
      # From createStudyPopArgs in Analysis Specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE, # From restrictToCommonPeriod
        firstExposureOnly = FALSE, # From firstExposureOnly
        washoutPeriod = 0, # From washoutPeriod
        removeDuplicateSubjects = "keep all", # From removeDuplicateSubjects
        censorAtNewRiskWindow = FALSE, # From censorAtNewRiskWindow
        removeSubjectsWithPriorOutcome = TRUE, # From removeSubjectsWithPriorOutcome
        priorOutcomeLookback = 99999, # From priorOutcomeLookBack
        riskWindowStart = timeAtRisks$riskWindowStart[t], # From current timeAtRisks loop iteration
        startAnchor = timeAtRisks$startAnchor[t], # From current timeAtRisks loop iteration
        riskWindowEnd = timeAtRisks$riskWindowEnd[t], # From current timeAtRisks loop iteration
        endAnchor = timeAtRisks$endAnchor[t], # From current timeAtRisks loop iteration
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t], # From current timeAtRisks loop iteration
        maxDaysAtRisk = 99999 # Default from template
      )


      # Append the settings to Analysis List
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

# CohortMethodModule specifications, combining all CM analyses.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL, # Default from template
  refitPsForEveryOutcome = FALSE, # Default from template
  refitPsForEveryStudyPopulation = FALSE, # Default from template
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds() # Default thresholds
)

# Create the analysis specifications ------------------------------------------
# Consolidate all shared resources and module specifications into a single analysis specification object.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to a JSON file.
# The filename is based on the "name" from Analysis Specifications ("corazon").
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "corazon", "corazonAnalysisSpecification.json")
)