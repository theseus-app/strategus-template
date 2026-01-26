################################################################################
# CreateAnalysisSpecification.R for ranitidinecancer study
# 
# This script creates a Strategus analysis specification JSON file based on
# the provided analysis settings. The specification coordinates three modules:
# CohortGenerator, CohortDiagnostics, and CohortMethod.
#
# Key steps:
# 1. Define shared resources (cohort definitions, negative controls)
# 2. Configure each module's specifications
# 3. Combine all modules into a single analysis specification
# 4. Save as JSON file for execution
#
# Note: All cohort IDs are remapped to avoid conflicts between modules.
# Target = 1, Comparator = 2, Outcome = 3, Negative Controls = 101+
################################################################################

library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Cohort definitions are fetched from the ATLAS WebAPI and renumbered
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Export cohort definitions from ATLAS
# Note: IDs from analysis specification: target=1794126, comparator=1794132, outcome=1794131
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target cohort
    1794132, # Comparator cohort
    1794131  # Outcome cohort
  ),
  generateStats = TRUE
)

# Renumber cohorts to avoid conflicts in Strategus execution
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Create negative control outcome cohort set from concept set
# Using concept set ID 1888110 from analysis specification
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
  mutate(cohortId = row_number() + 100) %>% # Start negative controls at 101
  select(cohortId, cohortName, outcomeConceptId)

# Verify no duplicate cohort IDs exist
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Define study components for CohortMethod module ------------------------------
# Outcome list: includes both primary outcome and negative controls
# Primary outcome uses cleanWindow=365 (from analysis specification)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target-comparator pairs for CohortMethod analysis
# Using renumbered IDs: target=1, comparator=2
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# No covariate concepts to exclude specified in analysis (empty arrays)
excludedCovariateConcepts <- data.frame(
  conceptId = numeric(),
  conceptName = character()
)

# No covariate concepts to include specified in analysis (empty arrays)
includedCovariateConcepts <- data.frame(
  conceptId = numeric(),
  conceptName = character()
)

# CohortGeneratorModule --------------------------------------------------------
# Creates shared resources and specifications for cohort generation
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

# CohortDiagnosticsModule ------------------------------------------------------
# Configures cohort diagnostics for all cohorts (target, comparator, outcome)
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
# Configure CohortMethod analysis settings

# Study periods: empty strings indicate no restriction (from analysis specification)
studyPeriods <- tibble(
  studyStartDate = c(""),  # No start date restriction
  studyEndDate   = c("")   # No end date restriction
)

# Time-at-risk (TAR) definitions from analysis specification
# Four TARs defined with varying risk windows and anchors
timeAtRisks <- tibble(
  label = c("Tar1", "Tar2", "Tar3", "Tar4"),
  riskWindowStart  = c(1, 365, 1, 365),
  startAnchor = c("cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd  = c(99999, 99999, 0, 0),
  endAnchor = c("cohort start", "cohort start", "cohort end", "cohort end"),
  minDaysAtRisk = c(1, 1, 1, 1)
)

# Propensity Score settings from analysis specification
# Four PS adjustment strategies: two matching, one stratification, one unadjusted
matchOnPsArgsList <- tibble(
  label = c("Match1", "Match2"),
  maxRatio  = c(1, 10),
  caliper = c(0.2, 0.2),
  caliperScale  = c("standardized logit", "standardized logit")
)

stratifyByPsArgsList <- tibble(
  label = c("Stratify"),
  numberOfStrata  = c(10),
  baseSelection = c("all")
)

# Build PS configuration list combining match and stratify methods
psConfigList <- list()

# Add matching configurations
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

# Add stratification configurations
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

# Add unadjusted analysis (no PS adjustment)
psConfigList[[length(psConfigList) + 1]] <- list(
  method = "none",
  label  = "Unadjusted",
  params = list()
)

# Build CohortMethod analysis list by iterating through all combinations
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Set PS adjustment arguments based on configuration
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
      } else { # Unadjusted
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- NULL
      }

      # Covariate settings: use default with no included/excluded concepts
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        includedCovariateConceptIds = includedCovariateConcepts$conceptId,
        excludedCovariateConceptIds = excludedCovariateConcepts$conceptId,
        addDescendantsToExclude = TRUE
      )

      # Outcome list includes primary outcome and negative controls
      outcomeList <- append(
        # Primary outcome(s)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
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

      # Create target-comparator-outcomes specifications
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList
        )
      }

      # getDbCohortMethodDataArgs from analysis specification
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,  # From analysis spec
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,  # From analysis spec: 0 = no restriction
        covariateSettings = covariateSettings,
        firstExposureOnly = FALSE,  # From analysis spec
        washoutPeriod = 365,  # From analysis spec
        removeDuplicateSubjects = "keep first"  # From analysis spec
      )

      # createPsArgs from analysis specification
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # From analysis spec
        errorOnHighCorrelation = TRUE,  # From analysis spec
        stopOnError = FALSE,  # Allow Strategus to continue even if model fitting fails
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",  # From analysis spec
          exclude = c(0),
          useCrossValidation = TRUE  # From analysis spec
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",  # From analysis spec
          cvType = "auto",  # From analysis spec
          seed = 1,
          resetCoefficients = TRUE,  # From analysis spec
          tolerance = 2e-07,  # From analysis spec
          cvRepetitions = 10,  # From analysis spec (cvRepetitions)
          fold = 10,  # From analysis spec (fold)
          startingVariance = 0.01  # From analysis spec
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

      # fitOutcomeModelArgs from analysis specification
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",  # From analysis spec
        stratified = TRUE,  # From analysis spec
        useCovariates = FALSE,  # From analysis spec
        inversePtWeighting = FALSE,  # From analysis spec
        prior = Cyclops::createPrior(
          priorType = "laplace",  # From analysis spec
          useCrossValidation = TRUE  # From analysis spec
        ),
        control = Cyclops::createControl(
          cvType = "auto",  # From analysis spec
          seed = 1,
          resetCoefficients = TRUE,  # From analysis spec
          startingVariance = 0.01,  # From analysis spec
          tolerance = 2e-07,  # From analysis spec
          cvRepetitions = 10,  # From analysis spec (cvRepetitions)
          fold = 10,  # From analysis spec (fold)
          noiseLevel = "quiet"  # From analysis spec
        )
      )

      # createStudyPopArgs from analysis specification
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,  # From analysis spec
        firstExposureOnly = FALSE,  # From analysis spec
        washoutPeriod = 0,  # From analysis spec
        removeDuplicateSubjects = "keep all",  # From analysis spec
        censorAtNewRiskWindow = FALSE,  # From analysis spec
        removeSubjectsWithPriorOutcome = TRUE,  # From analysis spec
        priorOutcomeLookback = 99999,  # From analysis spec
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999
      )

      # Append analysis settings to list
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

# Save analysis specification to JSON file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ranitidinecancer", "ranitidinecancerAnalysisSpecification.json")
)