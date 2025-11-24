library(dplyr)
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(ParallelLogger)

################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script builds a Strategus analysis specification JSON for the study:
#   ticagrelorclopidogrel
#
# It follows the template provided and applies the settings in the supplied
# Analysis Specifications.  Detailed inline comments explain how each setting
# from the specifications is applied to Strategus / CohortMethod arguments.
################################################################################

# Shared Resources -------------------------------------------------------------
# Base WebAPI endpoint used to fetch cohort and concept set definitions.
# (The Analysis Specifications do not specify another baseUrl, so the Atlas demo
# WebAPI is used here as in the template. Replace if you have your own WebAPI.)
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# COHORT DEFINITIONS
# The Analysis Specifications list three cohort IDs:
#  - target cohort id:      1794126 (named "target1" in the specs)
#  - comparator cohort id:  1794132 (named "comparator1")
#  - outcome cohort id:     1794131 (named "outcome1")
#
# We export the cohort definitions from the WebAPI. generateStats = TRUE is kept
# to collect cohort generation statistics.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: "target1"
    1794132, # Comparator: "comparator1"
    1794131  # Outcome: "outcome1"
  ),
  generateStats = TRUE
)

# Re-number cohorts so that the target/comparator/outcome ids are 1, 2, 3
# (This numbering is used later when constructing CohortMethod inputs.)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# NEGATIVE CONTROL OUTCOMES
# The Analysis Specifications include a negative control concept set id:
#   conceptSetId = 1888110 (name "negative")
#
# We fetch that concept set, resolve it to the underlying concepts, and turn it
# into a cohort-like table for use as negative controls. Negative control
# cohortIds are offset (start at 101) to avoid collisions with target/comparator.
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
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check - ensure cohort ids do not duplicate
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between primary cohorts and negative controls ***")
}

# Create data frames describing which cohorts are outcomes and the target/comparator pair
# Outcomes (from the cohort definition set): filter to cohortId == 3 (the outcome in specs)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # In the template a cleanWindow = 365 was used; keep that (this is separate from
  # priorOutcomeLookback used when constructing CohortMethod::createOutcome)
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analyses
# Use EXACT names from the Analysis Specifications: "target1" and "comparator1".
# We include columns targetConceptId and comparatorConceptId (left NA here)
# because the template expects these when building excluded covariates.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  targetConceptId = NA_integer_,    # No explicit concept id provided in specs
  comparatorConceptId = NA_integer_ # No explicit concept id provided in specs
)

# Excluded covariate concepts: Analysis Specifications' covariateSelection
# lists empty conceptsToInclude/Exclude. Thus we do not populate excluded covariates.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0)
)

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resources for cohort generation (cohort definitions)
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

# Shared resources for negative control outcomes
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications to generate cohorts (generateStats = TRUE because cohortDefinitionSet used that)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule -----------------------------------------------------
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

# CohortMethodModule ----------------------------------------------------------
# The Analysis Specifications provided:
# - getDbCohortMethodDataArgs.studyPeriods: two intervals
# - createStudyPopArgs: various settings incl. washoutPeriod=365, removeDuplicateSubjects="keep all", etc.
# - timeAtRisks: three TARs
# - propensity score settings: match (maxRatio=1, caliper=0.2), match (maxRatio=10, caliper=0.2),
#   and stratify (numberOfStrata=10, baseSelection="all")
# - createPsArgs and fitOutcomeModelArgs parameters (priors/controls)
#
# Set up the studyPeriods tibble (two rows from the Analysis Specifications).
studyPeriods <- tibble::tibble(
  studyStartDate = c("20111101", "20130301"), # YYYYMMDD strings as supplied
  studyEndDate   = c("20190331", "20161231")
)

# Time-at-risks (TARs) - use three TARs as specified. A label column is included for
# human readable descriptions used in the analysis description string.
timeAtRisks <- tibble::tibble(
  label = c("TAR_1_365", "TAR_1_1825", "TAR_on_treatment"),
  riskWindowStart  = c(1, 1, 1),
  startAnchor = c("cohort start", "cohort start", "cohort start"),
  riskWindowEnd  = c(365, 1825, 0),
  endAnchor = c("cohort start", "cohort start", "cohort end")
)

# Propensity score - build matchOnPsArgsList and stratifyByPsArgsList from specs
matchOnPsArgsList <- tibble::tibble(
  label = c("match_1", "match_10"),
  maxRatio  = c(1, 10),
  caliper = c(0.2, 0.2),
  caliperScale  = c("standardized logit", "standardized logit")
)

stratifyByPsArgsList <- tibble::tibble(
  label = c("stratify_10"),
  numberOfStrata  = c(10),
  baseSelection = c("all") # allowed values: "all" | "target" | "comparator"
)

# Combine the PS configs into a single list (psConfigList) describing method, label, and params.
psConfigList <- list()

if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
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

if (exists("stratifyByPsArgsList") && nrow(stratifyByPsArgsList) > 0) {
  for (i in seq_len(nrow(stratifyByPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "stratify",
      label = stratifyByPsArgsList$label[i],
      params = list(
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection  = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}

# Now iterate through studyPeriods x timeAtRisks x PS configs to create CM analyses
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Define PS adjustment arguments based on the configuration method
      if (psCfg$method == "match") {
        # Create matching arguments using CohortMethod::createMatchOnPsArgs
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
        stop("Unknown PS method in psConfigList")
      }

      # Covariate settings: template uses default covariate settings and adds
      # descendants to exclude. The Analysis Specifications did not provide a
      # custom covariate set, so the default is used.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build the outcome list for CohortMethod:
      # - Outcomes from oList are outcomes of interest (trueEffectSize = NA)
      # - Negative controls from negativeControlOutcomeCohortSet are not outcomes of interest (trueEffectSize=1)
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Build the targetComparatorOutcomesList expected by createCmAnalysis
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # excludedCovariateConceptIds contains:
          # - the target/comparator concept ids (if available)
          # - any additional excludedCovariateConcepts
          excludedCovariateConceptIds = c(
            cmTcList$targetConceptId[i],
            cmTcList$comparatorConceptId[i],
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # getDbCohortMethodDataArgs: use studyStartDate/studyEndDate for each studyPeriod
      # maxCohortSize is 0 per the Analysis Specifications (no limit)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs: map the Analysis Specifications' createPsArgs settings.
      # The specs specify:
      #  - maxCohortSizeForFitting: 250000
      #  - errorOnHighCorrelation: true
      #  - prior: laplace with useCrossValidation = true
      #  - control: tolerance 2e-7, cvType "auto", fold = 10, cvRepetitions = 10,
      #             noiseLevel "silent", resetCoefficients = true, startingVariance = 0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # keep FALSE to allow Strategus to continue other analyses if one fails
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",
          exclude = c(0),
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          tolerance = 2e-07,
          cvType = "auto",
          seed = 1,
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "silent",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Covariate balance computation arguments
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs: map settings from Analysis Specifications
      # - modelType: "cox"
      # - stratified: FALSE (specs)
      # - useCovariates: FALSE
      # - inversePtWeighting: FALSE
      # - prior: laplace with useCrossValidation = TRUE
      # - control: tolerance 2e-7, cvType "auto", fold=10, cvRepetitions=10, noiseLevel "quiet",
      #            resetCoefficients = TRUE, startingVariance = 0.01
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
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
          fold = 10,
          cvRepetitions = 10,
          tolerance = 2e-07,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # createStudyPopArgs: map settings from Analysis Specifications createStudyPopArgs
      # - restrictToCommonPeriod: false
      # - firstExposureOnly: false
      # - washoutPeriod: 365
      # - removeDuplicateSubjects: "keep all"
      # - censorAtNewRiskWindow: false
      # - removeSubjectsWithPriorOutcome: true
      # - priorOutcomeLookBack: 99999
      # - time-at-risk values come from timeAtRisks[t, ]
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

      # Append the CohortMethod analysis configuration into cmAnalysisList
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
    } # end psConfigList loop
  } # end timeAtRisks loop
} # end studyPeriods loop

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

# Create the full Strategus analysis specifications by adding shared resources
# and all module specifications created above.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the specifications to a JSON file. The filename is based on the study name
# in the Analysis Specifications: "ticagrelorclopidogrel".
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ticagrelorclopidogrel", "ticagrelorclopidogrelAnalysisSpecification.json")
)