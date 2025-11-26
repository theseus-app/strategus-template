################################################################################
# This script uses the OHDSI Strategus package to create a study analysis
# specification based on the settings provided in JSON format.
#
# Refer to the "Create analysis specifications" section of the OHDSI Strategus
# documentation for more details:
# https://ohdsi.github.io/Strategus/articles/CreatingAnalysisSpecifications.html
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules
# This help page also contains links to the corresponding HADES package that
# provides further details on the function arguments.
################################################################################

# Install and load the required packages
# install.packages("devtools")
# devtools::install_github("OHDSI/Strategus")
# devtools::install_github("OHDSI/ROhdsiWebApi")
# devtools::install_github("OHDSI/CohortGenerator")
# devtools::install_github("OHDSI/CohortDiagnostics")
# devtools::install_github("OHDSI/CohortMethod")
# devtools::install_github("OHDSI/FeatureExtraction")
# devtools::install_github("OHDSI/Cyclops")
library(dplyr)
library(Strategus)

# == Shared Resources ==========================================================
# This section defines the cohorts and concept sets that are used across the
# different analysis modules.

# The baseUrl for the WebApi instance that hosts the cohort and concept set
# definitions.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# --- Cohort Definitions -------------------------------------------------------
# We use ROhdsiWebApi to retrieve the cohort definitions from the WebApi.
# The cohort IDs are taken from the "cohortDefinitions" section of the
# analysis specifications.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# In Strategus, it is a convention to re-number cohort IDs to a simple sequence
# starting from 1. Here, we map the WebApi cohort IDs to new integer IDs.
# Target cohort is assigned ID 1.
# Comparator cohort is assigned ID 2.
# Outcome cohort is assigned ID 3.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# --- Negative Control Outcomes Concept Set ------------------------------------
# Negative control outcomes are concepts that are not believed to be caused by
# the exposure. They are used to calibrate p-values and assess residual bias.
# The concept set ID is taken from the "negativeControlConceptSet" section of
# the analysis specifications.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # Corresponds to "negative" concept set
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(
    baseUrl = baseUrl
  ) %>%
  ROhdsiWebApi::getConcepts(
    baseUrl = baseUrl
  ) %>%
  # Rename columns to match the expected format for negative control outcome cohorts
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  # Assign unique cohort IDs for each negative control, starting from 101 to avoid
  # collision with the primary cohorts.
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Safety check to ensure there are no duplicate cohort IDs across all defined cohorts.
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# == Analysis-specific Settings ================================================
# This section defines the data frames that will be used to construct the
# analysis settings for the CohortMethod module.

# --- Outcomes of Interest -----------------------------------------------------
# Create a data frame for the primary outcome(s) of interest.
# Here we filter the main cohort definition set for the outcome cohort (ID 3).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # The cleanWindow is the number of days at the end of observation time that
  # are removed from the analysis. This is not specified in the JSON but is
  # a common parameter in such studies. 365 days is a reasonable default.
  mutate(cleanWindow = 365)

# --- Target and Comparator Cohorts --------------------------------------------
# Create a data frame specifying the target and comparator cohorts for the
# CohortMethod analysis.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# --- Covariate Selection ------------------------------------------------------
# The "covariateSelection" section in the analysis specifications is empty for
# both conceptsToInclude and conceptsToExclude. This implies that default
# covariate settings will be used, and no additional concepts need to be
# included or excluded. The standard behavior of CohortMethod is to automatically
# exclude the drug concepts that define the target and comparator cohorts.
# Therefore, we create an empty data frame for excluded concepts.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# == Module Specifications =====================================================
# This section defines the settings for each Strategus module that will be part
# of the analysis.

# --- CohortGeneratorModule ----------------------------------------------------
# This module is responsible for generating all the cohort definitions (target,
# comparator, outcomes, negative controls) in the database.
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Define the primary cohorts (T, C, O) as a shared resource.
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

# Define the negative control outcome cohorts as a shared resource.
# Occurrence type "first" means we only consider the first occurrence of the
# negative control outcome for each person.
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Create the final module specifications for CohortGenerator.
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# --- CohortDiagnosticsModule --------------------------------------------------
# This module runs a comprehensive set of diagnostics on the generated cohorts
# to assess their quality and characteristics.
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

# --- CohortMethodModule -------------------------------------------------------
# This module performs the comparative cohort analysis.

# --- Study Periods ------------------------------------------------------------
# The "getDbCohortMethodDataArgs.studyPeriods" section in the analysis
# specifications is empty, which means the study is not restricted to a specific
# time window. We represent this with an empty tibble.
studyPeriods <- tibble(
  studyStartDate = c(""), # YYYYMMDD format, empty string for no start date
  studyEndDate   = c("")  # YYYYMMDD format, empty string for no end date
)

# --- Time-at-Risks (TARs) -----------------------------------------------------
# This tibble defines the time-at-risk windows for the analysis, based on the
# "createStudyPopArgs.timeAtRisks" section of the specifications. Two TARs are defined.
timeAtRisks <- tibble::tribble(
  ~label, ~riskWindowStart, ~startAnchor, ~riskWindowEnd, ~endAnchor,
  # TAR 1: Starts 1 day after cohort start and ends at cohort end.
  # This is often referred to as an "on-treatment" analysis.
  "On Treatment", 1, "cohort start", 0, "cohort end",
  # TAR 2: Starts 1 day after cohort start and ends 99999 days after cohort start.
  # This represents a long-term follow-up, often used in "intent-to-treat" analyses.
  "Long-term Follow-up", 1, "cohort start", 99999, "cohort start"
) %>%
  # minDaysAtRisk is 1 for both TARs as per the specifications.
  mutate(minDaysAtRisk = 1)


# --- Propensity Score (PS) Adjustment Settings --------------------------------
# Define the PS adjustment strategies. The specification only includes matching.

# Propensity Score settings for matching, based on "propensityScoreAdjustment.psSettings".
matchOnPsArgsList <- tibble::tribble(
  ~label, ~maxRatio, ~caliper, ~caliperScale,
  # 1-to-1 matching with a caliper of 0.2 on the standardized logit scale.
  "1-to-1 matching", 1, 0.2, "standardized logit"
)

# Propensity Score settings for stratification. This is null in the specifications,
# so we create an empty tibble.
stratifyByPsArgsList <- tibble::tribble(
  ~label, ~numberOfStrata, ~baseSelection
)

# Combine all PS configurations into a single list. This loop structure allows for
# easily adding more PS strategies in the future.
psConfigList <- list()

# Convert the "matchOnPsArgsList" tibble into the required list format.
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

# Convert the "stratifyByPsArgsList" tibble into the required list format.
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

# --- Build the Full CohortMethod Analysis List --------------------------------
# Iterate through all combinations of study periods, TARs, and PS settings
# to create a list of analysis settings (cmAnalysisList).
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Create the appropriate PS adjustment arguments based on the method.
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
      }

      # Define covariate settings. We use the default settings from FeatureExtraction.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )
      
      # Define the full list of outcomes, including the primary outcome and all
      # negative control outcomes.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA
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
      
      # Create the list of target-comparator-outcomes settings.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude additional concepts from covariates. Based on the spec, this is empty.
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }
      
      # Define arguments for retrieving data from the database.
      # Settings are from the "getDbCohortMethodDataArgs" section of the specifications.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # 0 means no maximum size
        covariateSettings = covariateSettings
      )

      # Define arguments for creating the propensity score model.
      # Settings are from the "propensityScoreAdjustment.createPsArgs" section.
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",
          cvType = "auto",
          seed = 1,
          resetCoefficients = TRUE,
          tolerance = 2e-07,
          cvRepetitions = 10, # From spec
          startingVariance = 0.01
        )
      )

      # Define arguments for computing covariate balance.
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Define arguments for fitting the outcome model.
      # Settings are from the "fitOutcomeModelArgs" section of the specifications.
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = FALSE, # Set to FALSE as per spec
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
          cvRepetitions = 10, # From spec
          noiseLevel = "quiet"
        )
      )

      # Define arguments for creating the study population.
      # Settings are from the "createStudyPopArgs" section of the specifications.
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all", # From spec
        censorAtNewRiskWindow = FALSE, # From spec
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t]
      )

      # Append the complete settings for this analysis variant to the list.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(studyStartDate == "", "None", studyStartDate),
          ifelse(studyEndDate == "", "None", studyEndDate),
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

# Create the final module specifications for CohortMethod.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# == Create and Save Analysis Specifications ==================================
# This final section assembles all the module specifications into a single
# analysis specification object and saves it as a JSON file.

analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() %>%
  # Add the shared resources (cohorts, negative controls)
  Strategus::addSharedResources(cohortDefinitionShared) %>%
  Strategus::addSharedResources(negativeControlsShared) %>%
  # Add the module specifications for each part of the study
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the specifications to a JSON file. The file path should be structured
# as 'inst/study_name/analysis_specification.json' for use with Strategus.
# The study name is taken from the "name" field in the specifications.
ParallelLogger::saveSettingsToJson(
  obj = analysisSpecifications,
  fileName = file.path("antivegfkidneyAnalysisSpecification.json")
)

# Optional: To save inside a package structure
# dir.create(file.path("inst", "antivegfkidney"), recursive = TRUE, showWarnings = FALSE)
# ParallelLogger::saveSettingsToJson(
#   analysisSpecifications,
#   file.path("inst", "antivegfkidney", "antivegfkidneyAnalysisSpecification.json")
# )