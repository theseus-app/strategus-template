library(dplyr)
library(Strategus)
# The script below creates a Strategus analysis specification for the study named exactly:
# "uveitissafety"
#
# The settings used in this script are taken directly from the provided
# Analysis Specifications. Variable names and cohort names use the exact
# identifiers supplied ("target1", "comparator1", "outcome1", "negative").
#
# Extensive comments are included to explain how each entry from the Analysis
# Specifications is applied to the Strategus analysis specification.

# ------------------------------------------------------------------------------
# Shared Resources: Cohort Definitions and Negative Controls
# ------------------------------------------------------------------------------
# The Atlas/WebAPI base URL used to export cohort definitions and concept sets.
# Change this if your Atlas/WebAPI instance is different.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Export the cohort definitions from Atlas/WebAPI using the exact cohort IDs
# provided in the Analysis Specifications:
#   targetCohort:     id = 1794126  (name: "target1")
#   comparatorCohort: id = 1794132  (name: "comparator1")
#   outcomeCohort:    id = 1794131  (name: "outcome1")
#
# generateStats = TRUE will compute the cohort generation statistics when the
# CohortGenerator module runs.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts so the target/comparator/outcome in the analysis have
# compact IDs 1,2,3. This is a common practice in Strategus projects so the
# analysis uses small local cohort ids. Renumbering does NOT change the original
# Atlas cohort definitions - only the local cohortDefinitionSet object.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131,]$cohortId <- 3

# Negative control outcomes:
# The Analysis Specifications specify a negative control concept set with:
#   id = 1888110
#   name = "negative"
#
# We export the concept set definition, resolve descendants, fetch the concepts,
# and convert them into a cohort-like table used as negative control outcomes.
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
  # Assign cohort IDs for negative controls starting at 101 to avoid colliding
  # with the study cohorts (1,2,3,...). This is a convention used in the template.
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Sanity check: ensure no duplicate cohort ids between the main cohort set and
# the negative control cohort set.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# ------------------------------------------------------------------------------
# Build helper data frames describing outcomes, target/comparator pairs, and any
# excluded covariate concepts.
# ------------------------------------------------------------------------------

# Outcomes: use the (renumbered) outcome cohort (cohortId == 3). The template /
# spec sets a clean window of 365 days for the outcome.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Use the exact cohort names/numbers from the Analysis Specifications
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1",
  stringsAsFactors = FALSE
)

# Excluded covariate concepts:
# The Analysis Specifications provide an (effectively empty) list for covariate
# inclusion/exclusion. We represent an empty exclusion list here. If you want
# to add specific conceptIds to exclude (e.g. drug ingredient concepts), add
# them to this data.frame.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# CohortGenerator Module Specifications
# ------------------------------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource: cohort definitions used by modules
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Shared resource: negative control outcome cohorts
# occurrenceType = "first" -> use first occurrence of concept in the person's history
# detectOnDescendants = TRUE -> include descendant concepts of those in the concept set
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Module specifications for the CohortGenerator module: set generateStats = TRUE
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ------------------------------------------------------------------------------
# CohortDiagnostics Module Specifications
# ------------------------------------------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()

# We request a broad set of diagnostics for all cohorts present in cohortDefinitionSet
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
  # Use a small threshold for including temporal characterization output
  minCharacterizationMean = 0.01
)

# ------------------------------------------------------------------------------
# CohortMethod Module Specifications
# ------------------------------------------------------------------------------
# The CohortMethod settings are driven by the Analysis Specifications.
# 1) getDbCohortMethodDataArgs.studyPeriods: the JSON had a single entry with null
#    start/end. The pattern used in the template is to represent "no restriction"
#    using empty strings for the studyStartDate/studyEndDate.
# 2) Time-at-risk (TAR) is a single window defined in the createStudyPopArgs.
# 3) Propensity score adjustment: one "match" configuration with the given
#    maxRatio, caliper and caliperScale.

# Study periods: using empty strings to indicate "no study-level restriction".
studyPeriods <- tibble::tibble(
  studyStartDate = c(""), # empty -> not restricting
  studyEndDate   = c("")  # empty -> not restricting
)

# Time-at-risk (TAR) as specified in Analysis Specifications:
#  - riskWindowStart = 1, startAnchor = "cohort start"
#  - riskWindowEnd = 0, endAnchor = "cohort end"
#  - minDaysAtRisk = 1
timeAtRisks <- tibble::tibble(
  label = c("TAR_1_to_end"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end"),
  minDaysAtRisk = c(1)
)

# Propensity Score settings - "match on PS" only, per Analysis Specifications
matchOnPsArgsList <- tibble::tibble(
  label = c("match_on_ps"),
  maxRatio  = c(10),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # allowed: "propensity score" | "standardized" | "standardized logit"
)

# No stratification-by-PS configs in this analysis
stratifyByPsArgsList <- tibble::tibble(
  label = character(),
  numberOfStrata  = integer(),
  baseSelection = character()
)

# Build psConfigList from matchOnPsArgsList and stratifyByPsArgsList.
psConfigList <- list()
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "match",
      label  = matchOnPsArgsList$label[i],
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
      label  = stratifyByPsArgsList$label[i],
      params = list(
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection  = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}

# Prepare the list of CohortMethod analyses that will be created. We will iterate
# through studyPeriods x timeAtRisks x psConfigList and produce a single analysis
# configuration for each combination. For this study we expect only one of each,
# resulting in a single CohortMethod analysis configuration.
cmAnalysisList <- list()
analysisId <- 1

# Loop over study periods (here, just one that is "not restricting")
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  # Loop over time-at-risk definitions (one TAR in the specifications)
  for (t in seq_len(nrow(timeAtRisks))) {

    # Loop over propensity score configs (one "match" config in the specifications)
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Build matchOnPsArgs or stratifyByPsArgs depending on the PS method
      if (psCfg$method == "match") {
        # Create the match-on-PS arguments using CohortMethod's factory
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings:
      # By default use the FeatureExtraction default covariate settings and
      # ensure addDescendantsToExclude = TRUE so excluded concepts exclude their descendants.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcome list:
      # - include the main outcome (outcome1) as outcomeOfInterest = TRUE
      # - include the negative control outcome cohorts (from the negativeControlOutcomeCohortSet)
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
          # negative controls are not outcomes of interest and assumed to have null effect
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Build target-comparator-outcomes structures for each row in cmTcList
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # No extra excluded covariates provided in the Analysis Specifications,
          # so pass the (empty) excludedCovariateConcepts$conceptId vector.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # getDbCohortMethodDataArgs:
      # The Analysis Specifications' getDbCohortMethodDataArgs contained a studyPeriods
      # entry with nulls and maxCohortSize = 0. Here we set restrictToCommonPeriod = TRUE
      # and propagate the studyStartDate/studyEndDate from the studyPeriods table
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs:
      # Settings are taken from the Analysis Specifications:
      #   maxCohortSizeForFitting = 250000
      #   errorOnHighCorrelation = TRUE
      #   prior: laplace, useCrossValidation = TRUE
      #   control: tolerance=2e-7, cvType="auto", fold=10, cvRepetitions=10,
      #            noiseLevel="silent", resetCoefficients=TRUE, startingVariance=0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        # stopOnError = FALSE is useful in Strategus runs to let the module proceed
        # for other analyses even if PS fitting fails for one configuration.
        stopOnError = FALSE,
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",
          exclude = c(0),
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          tolerance = 2e-7,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "silent",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Covariate balance computation args (shared and per-comparison).
      # Keep fairly large maxCohortSize to avoid truncation of balance diagnostics.
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs:
      # Settings taken directly from the Analysis Specifications:
      #   modelType = "cox", stratified = TRUE, useCovariates = FALSE,
      #   inversePtWeighting = FALSE,
      #   prior: laplace w/ cross-validation,
      #   control: tolerance=2e-7, cvType="auto", fold=10, cvRepetitions=10,
      #            noiseLevel="quiet", resetCoefficients=TRUE, startingVariance=0.01
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
          tolerance = 2e-7,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # createStudyPopArgs:
      # These arguments implement the createStudyPopArgs object provided in the
      # Analysis Specifications:
      #   restrictToCommonPeriod = TRUE
      #   firstExposureOnly = TRUE
      #   washoutPeriod = 365
      #   removeDuplicateSubjects = "keep all"
      #   censorAtNewRiskWindow = TRUE
      #   removeSubjectsWithPriorOutcome = TRUE
      #   priorOutcomeLookBack = 99999
      #   risk windows: taken from timeAtRisks row t
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE,
        firstExposureOnly = TRUE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = TRUE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        # A large maxDaysAtRisk avoids truncating follow-up unless the analysis
        # specifically requires a maximum
        maxDaysAtRisk = 99999
      )

      # Finally append the CohortMethod analysis configuration to cmAnalysisList
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(studyStartDate == "", "<no start>", studyStartDate),
          ifelse(studyEndDate == "", "<no end>", studyEndDate),
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
    } # end loop over psConfigList
  } # end loop over timeAtRisks
} # end loop over studyPeriods

# Create CohortMethod module specifications
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  # The Analysis Specifications did not require refitting PS per outcome or per population
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# ------------------------------------------------------------------------------
# Compose the full Strategus analysis specification object by adding shared
# resources and module specifications in the same order they should run.
# ------------------------------------------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Persist the analysis specification to the inst/<studyName> folder. The file
# name uses the study name exactly as required by the instructions.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "uveitissafety", "uveitissafetyAnalysisSpecification.json")
)