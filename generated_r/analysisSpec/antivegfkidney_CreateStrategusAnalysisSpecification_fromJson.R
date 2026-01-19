# Generated from analysisSpecifications JSON
library(Strategus)

# Shared Resources -------------------------------------------------------------
cohortDefinitionSet <- data.frame(
  cohortId = c(1, 2, 3),
  cohortName = c("AMS - cohort", "Delirium - Cohort", "Delirium"),
  sql = c("", "", ""),
  json = c("{\n\t\"cdmVersionRange\" : \">=5.0.0\",\n\t\"PrimaryCriteria\" : {\n\t\t\"CriteriaList\" : [\n\t\t\t{\n\t\t\t\t\"ConditionOccurrence\" : {\n\t\t\t\t\t\"CodesetId\" : 0\n\t\t\t\t}\n\t\t\t}\n\t\t],\n\t\t\"ObservationWindow\" : {\n\t\t\t\"PriorDays\" : 0,\n\t\t\t\"PostDays\" : 0\n\t\t},\n\t\t\"PrimaryCriteriaLimit\" : {\n\t\t\t\"Type\" : \"First\"\n\t\t}\n\t},\n\t\"ConceptSets\" : [\n\t\t{\n\t\t\t\"id\" : 0,\n\t\t\t\"name\" : \"AMS_inclDescendents_v2\",\n\t\t\t\"expression\" : {\n\t\t\t\t\"items\" : [\n\t\t\t\t\t{\n\t\t\t\t\t\t\"concept\" : {\n\t\t\t\t\t\t\t\"CONCEPT_ID\" : 436222,\n\t\t\t\t\t\t\t\"CONCEPT_NAME\" : \"Altered mental status\",\n\t\t\t\t\t\t\t\"STANDARD_CONCEPT\" : \"S\",\n\t\t\t\t\t\t\t\"STANDARD_CONCEPT_CAPTION\" : \"Standard\",\n\t\t\t\t\t\t\t\"INVALID_REASON\" : \"V\",\n\t\t\t\t\t\t\t\"INVALID_REASON_CAPTION\" : \"Valid\",\n\t\t\t\t\t\t\t\"CONCEPT_CODE\" : \"419284004\",\n\t\t\t\t\t\t\t\"DOMAIN_ID\" : \"Observation\",\n\t\t\t\t\t\t\t\"VOCABULARY_ID\" : \"SNOMED\",\n\t\t\t\t\t\t\t\"CONCEPT_CLASS_ID\" : \"Clinical Finding\"\n\t\t\t\t\t\t},\n\t\t\t\t\t\t\"isExcluded\" : false,\n\t\t\t\t\t\t\"includeDescendants\" : true,\n\t\t\t\t\t\t\"includeMapped\" : false\n\t\t\t\t\t}\n\t\t\t\t]\n\t\t\t}\n\t\t}\n\t],\n\t\"QualifiedLimit\" : {\n\t\t\"Type\" : \"First\"\n\t},\n\t\"ExpressionLimit\" : {\n\t\t\"Type\" : \"First\"\n\t},\n\t\"InclusionRules\" : [],\n\t\"CensoringCriteria\" : [],\n\t\"CollapseSettings\" : {\n\t\t\"CollapseType\" : \"ERA\",\n\t\t\"EraPad\" : 0\n\t},\n\t\"CensorWindow\" : {}\n}", "{\n\t\"cdmVersionRange\" : \">=5.0.0\",\n\t\"PrimaryCriteria\" : {\n\t\t\"CriteriaList\" : [\n\t\t\t{\n\t\t\t\t\"ConditionOccurrence\" : {\n\t\t\t\t\t\"CodesetId\" : 0\n\t\t\t\t}\n\t\t\t}\n\t\t],\n\t\t\"ObservationWindow\" : {\n\t\t\t\"PriorDays\" : 0,\n\t\t\t\"PostDays\" : 0\n\t\t},\n\t\t\"PrimaryCriteriaLimit\" : {\n\t\t\t\"Type\" : \"First\"\n\t\t}\n\t},\n\t\"ConceptSets\" : [\n\t\t{\n\t\t\t\"id\" : 0,\n\t\t\t\"name\" : \"Delirium - v2\",\n\t\t\t\"expression\" : {\n\t\t\t\t\"items\" : [\n\t\t\t\t\t{\n\t\t\t\t\t\t\"concept\" : {\n\t\t\t\t\t\t\t\"CONCEPT_ID\" : 373995,\n\t\t\t\t\t\t\t\"CONCEPT_NAME\" : \"Delirium\",\n\t\t\t\t\t\t\t\"STANDARD_CONCEPT\" : \"S\",\n\t\t\t\t\t\t\t\"STANDARD_CONCEPT_CAPTION\" : \"Standard\",\n\t\t\t\t\t\t\t\"INVALID_REASON\" : \"V\",\n\t\t\t\t\t\t\t\"INVALID_REASON_CAPTION\" : \"Valid\",\n\t\t\t\t\t\t\t\"CONCEPT_CODE\" : \"2776000\",\n\t\t\t\t\t\t\t\"DOMAIN_ID\" : \"Condition\",\n\t\t\t\t\t\t\t\"VOCABULARY_ID\" : \"SNOMED\",\n\t\t\t\t\t\t\t\"CONCEPT_CLASS_ID\" : \"Disorder\"\n\t\t\t\t\t\t},\n\t\t\t\t\t\t\"isExcluded\" : false,\n\t\t\t\t\t\t\"includeDescendants\" : true,\n\t\t\t\t\t\t\"includeMapped\" : false\n\t\t\t\t\t}\n\t\t\t\t]\n\t\t\t}\n\t\t},\n\t\t{\n\t\t\t\"id\" : 1,\n\t\t\t\"name\" : \"ICU\",\n\t\t\t\"expression\" : {\n\t\t\t\t\"items\" : [\n\t\t\t\t\t{\n\t\t\t\t\t\t\"concept\" : {\n\t\t\t\t\t\t\t\"CONCEPT_ID\" : 4306818,\n\t\t\t\t\t\t\t\"CONCEPT_NAME\" : \"Telemetry unit\",\n\t\t\t\t\t\t\t\"STANDARD_CONCEPT\" : \"N\",\n\t\t\t\t\t\t\t\"STANDARD_CONCEPT_CAPTION\" : \"Non-Standard\",\n\t\t\t\t\t\t\t\"INVALID_REASON\" : \"V\",\n\t\t\t\t\t\t\t\"INVALID_REASON_CAPTION\" : \"Valid\",\n\t\t\t\t\t\t\t\"CONCEPT_CODE\" : \"422798006\",\n\t\t\t\t\t\t\t\"DOMAIN_ID\" : \"Observation\",\n\t\t\t\t\t\t\t\"VOCABULARY_ID\" : \"SNOMED\",\n\t\t\t\t\t\t\t\"CONCEPT_CLASS_ID\" : \"Location\"\n\t\t\t\t\t\t},\n\t\t\t\t\t\t\"isExcluded\" : false,\n\t\t\t\t\t\t\"includeDescendants\" : false,\n\t\t\t\t\t\t\"includeMapped\" : false\n\t\t\t\t\t},\n\t\t\t\t\t{\n\t\t\t\t\t\t\"concept\" : {\n\t\t\t\t\t\t\t\"CONCEPT_ID\" : 4149943,\n\t\t\t\t\t\t\t\"CONCEPT_NAME\" : \"Cardiac intensive care unit\",\n\t\t\t\t\t\t\t\"STANDARD_CONCEPT\" : \"N\",\n\t\t\t\t\t\t\t\"STANDARD_CONCEPT_CAPTION\" : \"Non-Standard\",\n\t\t\t\t\t\t\t\"INVALID_REASON\" : \"V\",\n\t\t\t\t\t\t\t\"INVALID_REASON_CAPTION\" : \"Valid\",\n\t\t\t\t\t\t\t\"CONCEPT_CODE\" : \"309907008\",\n\t\t\t\t\t\t\t\"DOMAIN_ID\" : \"Observation\",\n\t\t\t\t\t\t\t\"VOCABULARY_ID\" : \"SNOMED\",\n\t\t\t\t\t\t\t\"CONCEPT_CLASS_ID\" : \"Location\"\n\t\t\t\t\t\t},\n\t\t\t\t\t\t\"isExcluded\" : false,\n\t\t\t\t\t\t\"includeDescendants\" : false,\n\t\t\t\t\t\t\"includeMapped\" : false\n\t\t\t\t\t},\n\t\t\t\t\t{\n\t\t\t\t\t\t\"concept\" : {\n\t\t\t\t\t\t\t\"CONCEPT_ID\" : 4148497,\n\t\t\t\t\t\t\t\"CONCEPT_NAME\" : \"Pediatric intensive care unit\",\n\t\t\t\t\t\t\t\"STANDARD_CONCEPT\" : \"N\",\n\t\t\t\t\t\t\t\"STANDARD_CONCEPT_CAPTION\" : \"Non-Standard\",\n\t\t\t\t\t\t\t\"INVALID_REASON\" : \"V\",\n\t\t\t\t\t\t\t\"INVALID_REASON_CAPTION\" : \"Valid\",\n\t\t\t\t\t\t\t\"CONCEPT_CODE\" : \"309910001\",\n\t\t\t\t\t\t\t\"DOMAIN_ID\" : \"Observation\",\n\t\t\t\t\t\t\t\"VOCABULARY_ID\" : \"SNOMED\",\n\t\t\t\t\t\t\t\"CONCEPT_CLASS_ID\" : \"Location\"\n\t\t\t\t\t\t},\n\t\t\t\t\t\t\"isExcluded\" : false,\n\t\t\t\t\t\t\"includeDescendants\" : false,\n\t\t\t\t\t\t\"includeMapped\" : false\n\t\t\t\t\t},\n\t\t\t\t\t{\n\t\t\t\t\t\t\"concept\" : {\n\t\t\t\t\t\t\t\"CONCEPT_ID\" : 4225556,\n\t\t\t\t\t\t\t\"CONCEPT_NAME\" : \"Psychiatric intensive care unit\",\n\t\t\t\t\t\t\t\"STANDARD_CONCEPT\" : \"N\",\n\t\t\t\t\t\t\t\"STANDARD_CONCEPT_CAPTION\" : \"Non-Standard\",\n\t\t\t\t\t\t\t\"INVALID_REASON\" : \"V\",\n\t\t\t\t\t\t\t\"INVALID_REASON_CAPTION\" : \"Valid\",\n\t\t\t\t\t\t\t\"CONCEPT_CODE\" : \"404821007\",\n\t\t\t\t\t\t\t\"DOMAIN_ID\" : \"Observation\",\n\t\t\t\t\t\t\t\"VOCABULARY_ID\" : \"SNOMED\",\n\t\t\t\t\t\t\t\"CONCEPT_CLASS_ID\" : \"Location\"\n\t\t\t\t\t\t},\n\t\t\t\t\t\t\"isExcluded\" : false,\n\t\t\t\t\t\t\"includeDescendants\" : false,\n\t\t\t\t\t\t\"includeMapped\" : false\n\t\t\t\t\t},\n\t\t\t\t\t{\n\t\t\t\t\t\t\"concept\" : {\n\t\t\t\t\t\t\t\"CONCEPT_ID\" : 763903,\n\t\t\t\t\t\t\t\"CONCEPT_NAME\" : \"Trauma intensive care unit\",\n\t\t\t\t\t\t\t\"STANDARD_CONCEPT\" : \"N\",\n\t\t\t\t\t\t\t\"STANDARD_CONCEPT_CAPTION\" : \"Non-Standard\",\n\t\t\t\t\t\t\t\"INVALID_REASON\" : \"V\",\n\t\t\t\t\t\t\t\"INVALID_REASON_CAPTION\" : \"Valid\",\n\t\t\t\t\t\t\t\"CONCEPT_CODE\" : \"448391000124102\",\n\t\t\t\t\t\t\t\"DOMAIN_ID\" : \"Observation\",\n\t\t\t\t\t\t\t\"VOCABULARY_ID\" : \"SNOMED\",\n\t\t\t\t\t\t\t\"CONCEPT_CLASS_ID\" : \"Location\"\n\t\t\t\t\t\t},\n\t\t\t\t\t\t\"isExcluded\" : false,\n\t\t\t\t\t\t\"includeDescendants\" : false,\n\t\t\t\t\t\t\"includeMapped\" : false\n\t\t\t\t\t},\n\t\t\t\t\t{\n\t\t\t\t\t\t\"concept\" : {\n\t\t\t\t\t\t\t\"CONCEPT_ID\" : 4140136,\n\t\t\t\t\t\t\t\"CONCEPT_NAME\" : \"Burns intensive care unit\",\n\t\t\t\t\t\t\t\"STANDARD_CONCEPT\" : \"N\",\n\t\t\t\t\t\t\t\"STANDARD_CONCEPT_CAPTION\" : \"Non-Standard\",\n\t\t\t\t\t\t\t\"INVALID_REASON\" : \"V\",\n\t\t\t\t\t\t\t\"INVALID_REASON_CAPTION\" : \"Valid\",\n\t\t\t\t\t\t\t\"CONCEPT_CODE\" : \"426439001\",\n\t\t\t\t\t\t\t\"DOMAIN_ID\" : \"Observation\",\n\t\t\t\t\t\t\t\"VOCABULARY_ID\" : \"SNOMED\",\n\t\t\t\t\t\t\t\"CONCEPT_CLASS_ID\" : \"Location\"\n\t\t\t\t\t\t},\n\t\t\t\t\t\t\"isExcluded\" : false,\n\t\t\t\t\t\t\"includeDescendants\" : false,\n\t\t\t\t\t\t\"includeMapped\" : false\n\t\t\t\t\t},\n\t\t\t\t\t{\n\t\t\t\t\t\t\"concept\" : {\n\t\t\t\t\t\t\t\"CONCEPT_ID\" : 4305366,\n\t\t\t\t\t\t\t\"CONCEPT_NAME\" : \"Surgical intensive care unit\",\n\t\t\t\t\t\t\t\"STANDARD_CONCEPT\" : \"N\",\n\t\t\t\t\t\t\t\"STANDARD_CONCEPT_CAPTION\" : \"Non-Standard\",\n\t\t\t\t\t\t\t\"INVALID_REASON\" : \"V\",\n\t\t\t\t\t\t\t\"INVALID_REASON_CAPTION\" : \"Valid\",\n\t\t\t\t\t\t\t\"CONCEPT_CODE\" : \"418433008\",\n\t\t\t\t\t\t\t\"DOMAIN_ID\" : \"Observation\",\n\t\t\t\t\t\t\t\"VOCABULARY_ID\" : \"SNOMED\",\n\t\t\t\t\t\t\t\"CONCEPT_CLASS_ID\" : \"Location\"\n\t\t\t\t\t\t},\n\t\t\t\t\t\t\"isExcluded\" : false,\n\t\t\t\t\t\t\"includeDescendants\" : false,\n\t\t\t\t\t\t\"includeMapped\" : false\n\t\t\t\t\t},\n\t\t\t\t\t{\n\t\t\t\t\t\t\"concept\" : {\n\t\t\t\t\t\t\t\"CONCEPT_ID\" : 40481392,\n\t\t\t\t\t\t\t\"CONCEPT_NAME\" : \"Medical intensive care unit\",\n\t\t\t\t\t\t\t\"STANDARD_CONCEPT\" : \"N\",\n\t\t\t\t\t\t\t\"STANDARD_CONCEPT_CAPTION\" : \"Non-Standard\",\n\t\t\t\t\t\t\t\"INVALID_REASON\" : \"V\",\n\t\t\t\t\t\t\t\"INVALID_REASON_CAPTION\" : \"Valid\",\n\t\t\t\t\t\t\t\"CONCEPT_CODE\" : \"441994008\",\n\t\t\t\t\t\t\t\"DOMAIN_ID\" : \"Observation\",\n\t\t\t\t\t\t\t\"VOCABULARY_ID\" : \"SNOMED\",\n\t\t\t\t\t\t\t\"CONCEPT_CLASS_ID\" : \"Location\"\n\t\t\t\t\t\t},\n\t\t\t\t\t\t\"isExcluded\" : false,\n\t\t\t\t\t\t\"includeDescendants\" : false,\n\t\t\t\t\t\t\"includeMapped\" : false\n\t\t\t\t\t}\n\t\t\t\t]\n\t\t\t}\n\t\t}\n\t],\n\t\"QualifiedLimit\" : {\n\t\t\"Type\" : \"First\"\n\t},\n\t\"ExpressionLimit\" : {\n\t\t\"Type\" : \"First\"\n\t},\n\t\"InclusionRules\" : [],\n\t\"CensoringCriteria\" : [],\n\t\"CollapseSettings\" : {\n\t\t\"CollapseType\" : \"ERA\",\n\t\t\"EraPad\" : 0\n\t},\n\t\"CensorWindow\" : {}\n}", "{\n\t\"cdmVersionRange\" : \">=5.0.0\",\n\t\"PrimaryCriteria\" : {\n\t\t\"CriteriaList\" : [\n\t\t\t{\n\t\t\t\t\"ConditionOccurrence\" : {}\n\t\t\t}\n\t\t],\n\t\t\"ObservationWindow\" : {\n\t\t\t\"PriorDays\" : 0,\n\t\t\t\"PostDays\" : 0\n\t\t},\n\t\t\"PrimaryCriteriaLimit\" : {\n\t\t\t\"Type\" : \"First\"\n\t\t}\n\t},\n\t\"ConceptSets\" : [],\n\t\"QualifiedLimit\" : {\n\t\t\"Type\" : \"First\"\n\t},\n\t\"ExpressionLimit\" : {\n\t\t\"Type\" : \"First\"\n\t},\n\t\"InclusionRules\" : [],\n\t\"CensoringCriteria\" : [],\n\t\"CollapseSettings\" : {\n\t\t\"CollapseType\" : \"ERA\",\n\t\t\"EraPad\" : 0\n\t},\n\t\"CensorWindow\" : {}\n}"),
  stringsAsFactors = FALSE
)

negativeControlOutcomeCohortSet <- data.frame(
  cohortId = c(101, 102),
  cohortName = c("Acute hepatic failure caused by hepatitis virus", "Subacute hepatic failure caused by hepatitis virus"),
  outcomeConceptId = c(36716708, 36716709),
  stringsAsFactors = FALSE
)

if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# CohortGeneratorModule --------------------------------------------------------
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
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
temporalCovariateSettings <- structure(
  list(
    temporal = TRUE,
    temporalSequence = FALSE,
    DemographicsGender = TRUE,
    DemographicsAge = TRUE,
    DemographicsAgeGroup = TRUE,
    DemographicsRace = TRUE,
    DemographicsEthnicity = TRUE,
    DemographicsIndexYear = TRUE,
    DemographicsIndexMonth = TRUE,
    DemographicsPriorObservationTime = TRUE,
    DemographicsPostObservationTime = TRUE,
    DemographicsTimeInCohort = TRUE,
    DemographicsIndexYearMonth = TRUE,
    ConditionOccurrence = TRUE,
    ConditionEraStart = TRUE,
    ConditionEraOverlap = TRUE,
    ConditionEraGroupStart = TRUE,
    ConditionEraGroupOverlap = TRUE,
    DrugEraStart = TRUE,
    DrugEraGroupStart = TRUE,
    DrugEraGroupOverlap = TRUE,
    ProcedureOccurrence = TRUE,
    DeviceExposure = TRUE,
    Measurement = TRUE,
    MeasurementRangeGroup = TRUE,
    MeasurementValueAsConcept = TRUE,
    Observation = TRUE,
    ObservationValueAsConcept = TRUE,
    CharlsonIndex = TRUE,
    Dcsi = TRUE,
    Chads2 = TRUE,
    Chads2Vasc = TRUE,
    temporalStartDays = c(-9999, -365, -180, -30, -365, -30, 0, 1, 31, -9999),
    temporalEndDays = c(0, 0, 0, 0, -31, -1, 0, 30, 365, 9999),
    includedCovariateConceptIds = list(),
    addDescendantsToInclude = FALSE,
    excludedCovariateConceptIds = list(),
    addDescendantsToExclude = FALSE,
    includedCovariateIds = list()
  ),
  class = "covariateSettings",
  fun = "getDbDefaultCovariateData"
)

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
  temporalCovariateSettings = temporalCovariateSettings,
  minCharacterizationMean = 0.01,
  irWashoutPeriod = 0
)

# CohortMethodModule -----------------------------------------------------------
targetComparatorOutcomesList <- list()

targetComparatorOutcomesList[[1]] <- structure(
  list(
    targetId = 1,
    comparatorId = 2,
    outcomes = list(structure(
      list(
        outcomeId = 3,
        outcomeOfInterest = TRUE,
        trueEffectSize = NULL,
        priorOutcomeLookback = 99999
      ),
      class = "outcome"
    ), structure(
      list(
        outcomeId = 101,
        outcomeOfInterest = FALSE,
        trueEffectSize = 1
      ),
      class = "outcome"
    ), structure(
      list(
        outcomeId = 102,
        outcomeOfInterest = FALSE,
        trueEffectSize = 1
      ),
      class = "outcome"
    )),
    excludedCovariateConceptIds = list()
  ),
  class = "targetComparatorOutcomes"
)

cmAnalysisList <- list()

cmAnalysisList[[1]] <- CohortMethod::createCmAnalysis(
  analysisId = 1,
  description = "Study: Unrestricted-Unrestricted; TAR: Main TAR; PS: PS Matching",
  getDbCohortMethodDataArgs = CohortMethod::createGetDbCohortMethodDataArgs(
    studyStartDate = "",
    studyEndDate = "",
    firstExposureOnly = FALSE,
    removeDuplicateSubjects = "keep all",
    restrictToCommonPeriod = FALSE,
    washoutPeriod = 0,
    maxCohortSize = 0,
    covariateSettings = structure(
      list(
        temporal = FALSE,
        temporalSequence = FALSE,
        DemographicsGender = TRUE,
        DemographicsAgeGroup = TRUE,
        DemographicsRace = TRUE,
        DemographicsEthnicity = TRUE,
        DemographicsIndexYear = TRUE,
        DemographicsIndexMonth = TRUE,
        ConditionGroupEraLongTerm = TRUE,
        ConditionGroupEraShortTerm = TRUE,
        DrugGroupEraLongTerm = TRUE,
        DrugGroupEraShortTerm = TRUE,
        DrugGroupEraOverlapping = TRUE,
        ProcedureOccurrenceLongTerm = TRUE,
        ProcedureOccurrenceShortTerm = TRUE,
        DeviceExposureLongTerm = TRUE,
        DeviceExposureShortTerm = TRUE,
        MeasurementLongTerm = TRUE,
        MeasurementShortTerm = TRUE,
        MeasurementRangeGroupLongTerm = TRUE,
        MeasurementRangeGroupShortTerm = TRUE,
        MeasurementValueAsConceptLongTerm = TRUE,
        MeasurementValueAsConceptShortTerm = TRUE,
        ObservationLongTerm = TRUE,
        ObservationShortTerm = TRUE,
        ObservationValueAsConceptLongTerm = TRUE,
        ObservationValueAsConceptShortTerm = TRUE,
        CharlsonIndex = TRUE,
        Dcsi = TRUE,
        Chads2 = TRUE,
        Chads2Vasc = TRUE,
        includedCovariateConceptIds = list(),
        includedCovariateIds = list(),
        addDescendantsToInclude = FALSE,
        excludedCovariateConceptIds = list(),
        addDescendantsToExclude = TRUE,
        shortTermStartDays = -30,
        mediumTermStartDays = -180,
        endDays = 0,
        longTermStartDays = -365
      ),
      class = "covariateSettings",
      fun = "getDbDefaultCovariateData"
    )
  ),
  createStudyPopArgs = CohortMethod::createCreateStudyPopulationArgs(
    firstExposureOnly = TRUE,
    restrictToCommonPeriod = FALSE,
    washoutPeriod = 365,
    removeDuplicateSubjects = "keep all",
    removeSubjectsWithPriorOutcome = TRUE,
    priorOutcomeLookback = 99999,
    minDaysAtRisk = 1,
    maxDaysAtRisk = 99999,
    riskWindowStart = 1,
    startAnchor = "cohort start",
    riskWindowEnd = 0,
    endAnchor = "cohort end",
    censorAtNewRiskWindow = FALSE
  ),
  createPsArgs = CohortMethod::createCreatePsArgs(
    maxCohortSizeForFitting = 250000,
    errorOnHighCorrelation = TRUE,
    stopOnError = FALSE,
    prior = structure(
      list(
        priorType = "laplace",
        variance = 1,
        exclude = 0,
        graph = NULL,
        neighborhood = NULL,
        useCrossValidation = TRUE,
        forceIntercept = FALSE
      ),
      class = "cyclopsPrior"
    ),
    control = structure(
      list(
        maxIterations = 1000,
        tolerance = 2e-7,
        convergenceType = "gradient",
        autoSearch = TRUE,
        fold = 10,
        lowerLimit = 0.01,
        upperLimit = 20,
        gridSteps = 10,
        minCVData = 100,
        cvRepetitions = 10,
        noiseLevel = "silent",
        threads = 1,
        seed = 1,
        resetCoefficients = TRUE,
        startingVariance = 0.01,
        useKKTSwindle = FALSE,
        tuneSwindle = 10,
        selectorType = "auto",
        initialBound = 2,
        maxBoundCount = 5,
        algorithm = "ccd",
        doItAll = TRUE,
        syncCV = FALSE
      ),
      class = "cyclopsControl"
    ),
    estimator = "att"
  ),
  matchOnPsArgs = CohortMethod::createMatchOnPsArgs(
    caliper = 0.2,
    caliperScale = "standardized logit",
    maxRatio = 1,
    allowReverseMatch = FALSE
  ),
  stratifyByPsArgs = NULL,
  computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
    maxCohortSize = 250000
  ),
  computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
    maxCohortSize = 250000,
    covariateFilter = structure(
      data.frame(
        label = c("Age group", "Gender: female", "Race", "Ethnicity", "Medical history: General", "Medical history: Cardiovascular disease", "Medical history: Neoplasms", "Medication use", "Charlson comorbidity index", "CHADS2Vasc", "DCSI"),
        analysisId = c(3, 1, 4, 5, 210, 210, 210, 410, 901, 904, 902),
        covariateIds = c(NA, "8532001", NA, NA, "4006969210,438409210,4212540210,255573210,201606210,4182210210,440383210,201820210,318800210,192671210,439727210,432867210,316866210,4104000210,433736210,80180210,255848210,140168210,4030518210,80809210,435783210,4279309210,81893210,81902210,197494210,4134440210", "313217210,381591210,317576210,321588210,316139210,4185932210,321052210,440417210,444247210", "4044013210,432571210,40481902210,443392210,4112853210,4180790210,443388210,197508210,200962210", "21601782410,21602796410,21604686410,21604389410,21603932410,21601387410,21602028410,21600960410,21601664410,21601744410,21601461410,21600046410,21603248410,21600712410,21603890410,21601853410,21604254410,21604489410,21604752410", "1901", "1904", "1902"),
        stringsAsFactors = FALSE
      ),
      class = c("spec_tbl_df", "tbl_df", "tbl", "data.frame"),
      row.names = as.integer(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)),
      spec = list(
        cols = list(
          label = list(),
          analysisId = list(),
          covariateIds = list()
        ),
        default = list(),
        delim = ","
      ),
      problems = structure(list(), names = character())
    )
  ),
  fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
    modelType = "cox",
    stratified = FALSE,
    useCovariates = FALSE,
    inversePtWeighting = FALSE,
    profileBounds = c(-2.3026, 2.3026),
    prior = structure(
      list(
        priorType = "laplace",
        variance = 1,
        exclude = NULL,
        graph = NULL,
        neighborhood = NULL,
        useCrossValidation = TRUE,
        forceIntercept = FALSE
      ),
      class = "cyclopsPrior"
    ),
    control = structure(
      list(
        maxIterations = 1000,
        tolerance = 2e-7,
        convergenceType = "gradient",
        autoSearch = TRUE,
        fold = 10,
        lowerLimit = 0.01,
        upperLimit = 20,
        gridSteps = 10,
        minCVData = 100,
        cvRepetitions = 10,
        noiseLevel = "quiet",
        threads = 1,
        seed = 1,
        resetCoefficients = TRUE,
        startingVariance = 0.01,
        useKKTSwindle = FALSE,
        tuneSwindle = 10,
        selectorType = "auto",
        initialBound = 2,
        maxBoundCount = 5,
        algorithm = "ccd",
        doItAll = TRUE,
        syncCV = FALSE
      ),
      class = "cyclopsControl"
    )
  )
)

cmDiagnosticThresholds <- CohortMethod::createCmDiagnosticThresholds(
  mdrrThreshold = 10,
  easeThreshold = 0.25,
  sdmThreshold = 0.1,
  equipoiseThreshold = 0.2,
  generalizabilitySdmThreshold = 1
)

cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = cmDiagnosticThresholds
)

# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  "generated_json/analysisSpec/antivegfkidneyAnalysisSpecification.json"
)
