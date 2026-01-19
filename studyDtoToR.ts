import type { StudyDTO } from "./studyDto.ts";

type StudyDtoToROptions = {
  studyName: string;
  baseUrl?: string;
  outputJsonPath?: string;
};

const INDENT = "  ";

function indent(text: string, level: number = 1): string {
  const pad = INDENT.repeat(level);
  return text
    .split("\n")
    .map((line) => (line ? pad + line : line))
    .join("\n");
}

function escapeRString(value: string): string {
  return value
    .replace(/\\/g, "\\\\")
    .replace(/"/g, '\\"')
    .replace(/\r/g, "\\r")
    .replace(/\n/g, "\\n")
    .replace(/\t/g, "\\t");
}

function rString(value: string): string {
  return `"${escapeRString(value)}"`;
}

function rValue(value: any): string {
  if (value === null || value === undefined) return "NA";
  if (typeof value === "string") return rString(value);
  if (typeof value === "number") return Number.isFinite(value) ? String(value) : "NA";
  if (typeof value === "boolean") return value ? "TRUE" : "FALSE";
  return "NA";
}

function rVector(values: any[], type: "character" | "numeric" | "integer" | "logical"): string {
  if (!values.length) {
    if (type === "character") return "character()";
    if (type === "integer") return "integer()";
    if (type === "logical") return "logical()";
    return "numeric()";
  }
  const rendered = values.map((val) => {
    if (val === null || val === undefined || val === "") {
      if (type === "character") return "NA_character_";
      if (type === "integer") return "NA_integer_";
      if (type === "logical") return "NA";
      return "NA_real_";
    }
    if (type === "character") return rString(String(val));
    if (type === "integer") return Number.isFinite(Number(val)) ? String(Math.trunc(Number(val))) : "NA_integer_";
    if (type === "logical") return Boolean(val) ? "TRUE" : "FALSE";
    return Number.isFinite(Number(val)) ? String(Number(val)) : "NA_real_";
  });
  return `c(${rendered.join(", ")})`;
}

function rDataFrameColumns(cols: Record<string, string>): string {
  const lines = Object.entries(cols).map(([key, value]) => `${key} = ${value}`);
  return `data.frame(\n${indent(lines.join(",\n"))}\n)`;
}

function buildStudyPeriods(dto: any): string {
  const studyPeriods = Array.isArray(dto?.getDbCohortMethodDataArgs?.studyPeriods)
    ? dto.getDbCohortMethodDataArgs.studyPeriods
    : [];
  const startVals = studyPeriods.map((s: any) => s?.studyStartDate ?? null);
  const endVals = studyPeriods.map((s: any) => s?.studyEndDate ?? null);
  const cols = {
    studyStartDate: rVector(startVals, "character"),
    studyEndDate: rVector(endVals, "character"),
  };
  return `studyPeriods <- tibble(\n${indent(Object.entries(cols).map(([k, v]) => `${k} = ${v}`).join(",\n"))}\n)`;
}

function tarLabel(tar: any, index: number, total: number): string {
  const desc = typeof tar?.description === "string" ? tar.description.trim() : "";
  if (desc) return desc;
  if (total === 1) return "Main TAR";
  return `TAR-${index + 1}`;
}

function psLabel(ps: any, index: number): string {
  const desc = typeof ps?.description === "string" ? ps.description.trim() : "";
  if (desc && !/^ps\s*\d+$/i.test(desc)) return desc;
  if (ps?.matchOnPsArgs && !ps?.stratifyByPsArgs) return "PS Matching";
  if (ps?.stratifyByPsArgs && !ps?.matchOnPsArgs) return "PS Stratification";
  return `PS-${index + 1}`;
}

function buildTimeAtRisks(dto: any): string {
  const tars = Array.isArray(dto?.createStudyPopArgs?.timeAtRisks)
    ? dto.createStudyPopArgs.timeAtRisks
    : [];
  const labels = tars.map((t: any, i: number) => tarLabel(t, i, tars.length));
  const cols = {
    label: rVector(labels, "character"),
    riskWindowStart: rVector(tars.map((t: any) => t?.riskWindowStart ?? null), "numeric"),
    startAnchor: rVector(tars.map((t: any) => t?.startAnchor ?? null), "character"),
    riskWindowEnd: rVector(tars.map((t: any) => t?.riskWindowEnd ?? null), "numeric"),
    endAnchor: rVector(tars.map((t: any) => t?.endAnchor ?? null), "character"),
    minDaysAtRisk: rVector(tars.map((t: any) => t?.minDaysAtRisk ?? null), "numeric"),
  };
  return `timeAtRisks <- tibble(\n${indent(Object.entries(cols).map(([k, v]) => `${k} = ${v}`).join(",\n"))}\n)`;
}

function buildMatchOnPsArgsList(psSettings: any[]): string {
  const rows = psSettings
    .map((ps, idx) => ({ ps, idx }))
    .filter(({ ps }) => ps?.matchOnPsArgs);
  if (!rows.length) {
    return `matchOnPsArgsList <- tibble(\n${indent(
      [
        "label = character()",
        "maxRatio = numeric()",
        "caliper = numeric()",
        "caliperScale = character()",
      ].join(",\n")
    )}\n)`;
  }
  const cols = {
    label: rVector(rows.map(({ ps, idx }) => psLabel(ps, idx)), "character"),
    maxRatio: rVector(rows.map(({ ps }) => ps.matchOnPsArgs?.maxRatio ?? null), "numeric"),
    caliper: rVector(rows.map(({ ps }) => ps.matchOnPsArgs?.caliper ?? null), "numeric"),
    caliperScale: rVector(rows.map(({ ps }) => ps.matchOnPsArgs?.caliperScale ?? null), "character"),
  };
  return `matchOnPsArgsList <- tibble(\n${indent(Object.entries(cols).map(([k, v]) => `${k} = ${v}`).join(",\n"))}\n)`;
}

function buildStratifyByPsArgsList(psSettings: any[]): string {
  const rows = psSettings
    .map((ps, idx) => ({ ps, idx }))
    .filter(({ ps }) => ps?.stratifyByPsArgs);
  if (!rows.length) {
    return `stratifyByPsArgsList <- tibble(\n${indent(
      [
        "label = character()",
        "numberOfStrata = integer()",
        "baseSelection = character()",
      ].join(",\n")
    )}\n)`;
  }
  const cols = {
    label: rVector(rows.map(({ ps, idx }) => psLabel(ps, idx)), "character"),
    numberOfStrata: rVector(rows.map(({ ps }) => ps.stratifyByPsArgs?.numberOfStrata ?? null), "integer"),
    baseSelection: rVector(rows.map(({ ps }) => ps.stratifyByPsArgs?.baseSelection ?? null), "character"),
  };
  return `stratifyByPsArgsList <- tibble(\n${indent(Object.entries(cols).map(([k, v]) => `${k} = ${v}`).join(",\n"))}\n)`;
}

function buildCyclopsCall(fnName: string, obj: any): string {
  if (!obj || typeof obj !== "object") return "NULL";
  const args = Object.entries(obj).map(([key, value]) => `${key} = ${rValue(value)}`);
  if (!args.length) return "NULL";
  return `${fnName}(\n${indent(args.join(",\n"))}\n)`;
}

function buildArgsObjectLines(obj: any, skipKeys: Set<string> = new Set()): string[] {
  const entries = Object.entries(obj ?? {}).filter(([key]) => !skipKeys.has(key));
  if (!entries.length) return [];
  return entries.map(([key, value]) => `${key} = ${rValue(value)}`);
}

export function studyDtoToR(dto: StudyDTO, opts: StudyDtoToROptions): string {
  const studyName = opts.studyName;
  const baseUrl = opts.baseUrl ?? "https://atlas-demo.ohdsi.org/WebAPI";
  const outputJsonExpr = opts.outputJsonPath
    ? rString(opts.outputJsonPath)
    : `file.path("inst", "${studyName}", "${studyName}AnalysisSpecification.json")`;

  const target = dto?.cohortDefinitions?.targetCohort ?? { id: null, name: "" };
  const comparator = dto?.cohortDefinitions?.comparatorCohort ?? { id: null, name: "" };
  const outcome = dto?.cohortDefinitions?.outcomeCohort?.[0] ?? { id: null, name: "" };
  const negative = dto?.negativeControlConceptSet ?? { id: null, name: "" };

  const exclude = Array.isArray(dto?.covariateSelection?.conceptsToExclude)
    ? dto.covariateSelection.conceptsToExclude.filter(
        (x: any) =>
          x &&
          (x.id !== null && x.id !== undefined
            ? true
            : typeof x.name === "string" && x.name.trim().length > 0)
      )
    : [];
  const excludeIds = exclude.map((x: any) => x?.id ?? null);
  const excludeNames = exclude.map((x: any) => x?.name ?? "");

  const psSettings = Array.isArray(dto?.propensityScoreAdjustment?.psSettings)
    ? dto.propensityScoreAdjustment.psSettings
    : [];

  const lines: string[] = [];
  lines.push("# Generated rule-based from StudyDTO");
  lines.push("library(dplyr)");
  lines.push("library(Strategus)");
  lines.push("");
  lines.push("# Shared Resources -------------------------------------------------------------");
  lines.push(`baseUrl <- ${rString(baseUrl)}`);
  lines.push("");
  lines.push("cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(");
  lines.push(indent(`baseUrl = baseUrl,`));
  lines.push(
    indent(
      `cohortIds = c(${rValue(target.id)}, ${rValue(comparator.id)}, ${rValue(outcome.id)}),`
    )
  );
  lines.push(indent("generateStats = TRUE"));
  lines.push(")");
  lines.push("");
  lines.push(
    `cohortDefinitionSet[cohortDefinitionSet$cohortId == ${rValue(
      target.id
    )}, ]$cohortId <- 1`
  );
  lines.push(
    `cohortDefinitionSet[cohortDefinitionSet$cohortId == ${rValue(
      comparator.id
    )}, ]$cohortId <- 2`
  );
  lines.push(
    `cohortDefinitionSet[cohortDefinitionSet$cohortId == ${rValue(
      outcome.id
    )}, ]$cohortId <- 3`
  );
  lines.push("");
  lines.push("negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(");
  lines.push(indent(`conceptSetId = ${rValue(negative.id)},`));
  lines.push(indent("baseUrl = baseUrl"));
  lines.push(") %>%");
  lines.push(indent("ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%"));
  lines.push(indent("ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%"));
  lines.push(
    indent(
      'rename(outcomeConceptId = "conceptId",\n' +
        '       cohortName = "conceptName") %>%'
    )
  );
  lines.push(indent("mutate(cohortId = dplyr::row_number() + 100) %>%"));
  lines.push(indent("select(cohortId, cohortName, outcomeConceptId)"));
  lines.push("");
  lines.push(
    "if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {"
  );
  lines.push(indent('stop("*** Error: duplicate cohort IDs found ***")'));
  lines.push("}");
  lines.push("");
  lines.push("# CohortMethod setup -----------------------------------------------------------");
  lines.push("oList <- cohortDefinitionSet %>%");
  lines.push(indent("filter(.data$cohortId == 3) %>%"));
  lines.push(indent("mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%"));
  lines.push(indent("select(outcomeCohortId, outcomeCohortName) %>%"));
  lines.push(indent("mutate(cleanWindow = 365)"));
  lines.push("");
  lines.push("cmTcList <- data.frame(");
  lines.push(
    indent(
      [
        "targetCohortId = 1,",
        `targetCohortName = ${rString(target.name ?? "")},`,
        "comparatorCohortId = 2,",
        `comparatorCohortName = ${rString(comparator.name ?? "")}`,
      ].join("\n")
    )
  );
  lines.push(")");
  lines.push("");
  lines.push(
    rDataFrameColumns({
      conceptId: rVector(excludeIds, "integer"),
      conceptName: rVector(excludeNames, "character"),
    }).replace(/^data.frame/, "excludedCovariateConcepts <- data.frame")
  );
  lines.push("");
  lines.push("# Study periods and time-at-risk definitions ---------------------------------");
  lines.push(buildStudyPeriods(dto));
  lines.push("");
  lines.push(buildTimeAtRisks(dto));
  lines.push("");
  lines.push("# Propensity score settings ---------------------------------------------------");
  lines.push(buildMatchOnPsArgsList(psSettings));
  lines.push("");
  lines.push(buildStratifyByPsArgsList(psSettings));
  lines.push("");
  lines.push("psConfigList <- list()");
  lines.push("");
  lines.push("if (exists(\"matchOnPsArgsList\") && nrow(matchOnPsArgsList) > 0) {");
  lines.push(indent("for (i in seq_len(nrow(matchOnPsArgsList))) {"));
  lines.push(
    indent(
      [
        "psConfigList[[length(psConfigList) + 1]] <- list(",
        indent("method = \"match\","),
        indent("label  = matchOnPsArgsList$label[i],"),
        indent(
          "params = list(\n" +
            indent(
              [
                "maxRatio     = matchOnPsArgsList$maxRatio[i],",
                "caliper      = matchOnPsArgsList$caliper[i],",
                "caliperScale = matchOnPsArgsList$caliperScale[i]",
              ].join("\n"),
              3
            ) +
            "\n" +
            indent(")")
        ),
        ")",
      ].join("\n"),
      2
    )
  );
  lines.push(indent("}"));
  lines.push("}");
  lines.push("");
  lines.push("if (exists(\"stratifyByPsArgsList\") && nrow(stratifyByPsArgsList) > 0) {");
  lines.push(indent("for (i in seq_len(nrow(stratifyByPsArgsList))) {"));
  lines.push(
    indent(
      [
        "psConfigList[[length(psConfigList) + 1]] <- list(",
        indent("method = \"stratify\","),
        indent("label  = stratifyByPsArgsList$label[i],"),
        indent(
          "params = list(\n" +
            indent(
              [
                "numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],",
                "baseSelection  = stratifyByPsArgsList$baseSelection[i]",
              ].join("\n"),
              3
            ) +
            "\n" +
            indent(")")
        ),
        ")",
      ].join("\n"),
      2
    )
  );
  lines.push(indent("}"));
  lines.push("}");
  lines.push("");
  lines.push("# CohortGeneratorModule --------------------------------------------------------");
  lines.push("cgModuleSettingsCreator <- CohortGeneratorModule$new()");
  lines.push(
    "cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)"
  );
  lines.push(
    "negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications("
  );
  lines.push(
    indent(
      [
        "negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,",
        "occurrenceType = \"first\",",
        "detectOnDescendants = TRUE",
      ].join("\n")
    )
  );
  lines.push(")");
  lines.push(
    "cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications("
  );
  lines.push(indent("generateStats = TRUE"));
  lines.push(")");
  lines.push("");
  lines.push("# CohortDiagnosticsModule ------------------------------------------------------");
  lines.push("cdModuleSettingsCreator <- CohortDiagnosticsModule$new()");
  lines.push("cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(");
  lines.push(
    indent(
      [
        "cohortIds = cohortDefinitionSet$cohortId,",
        "runInclusionStatistics = TRUE,",
        "runIncludedSourceConcepts = TRUE,",
        "runOrphanConcepts = TRUE,",
        "runTimeSeries = FALSE,",
        "runVisitContext = TRUE,",
        "runBreakdownIndexEvents = TRUE,",
        "runIncidenceRate = TRUE,",
        "runCohortRelationship = TRUE,",
        "runTemporalCohortCharacterization = TRUE,",
        "minCharacterizationMean = 0.01",
      ].join("\n")
    )
  );
  lines.push(")");
  lines.push("");
  lines.push("# CohortMethodModule -----------------------------------------------------------");
  lines.push("cmAnalysisList <- list()");
  lines.push("targetComparatorOutcomesList <- list()");
  lines.push("analysisId <- 1");
  lines.push("");
  lines.push("for (s in seq_len(nrow(studyPeriods))) {");
  lines.push(indent("studyStartDate <- studyPeriods$studyStartDate[s]"));
  lines.push(indent("studyEndDate <- studyPeriods$studyEndDate[s]"));
  lines.push(
    indent(
      "studyStartDate <- ifelse(is.na(studyStartDate), \"\", studyStartDate)"
    )
  );
  lines.push(
    indent("studyEndDate <- ifelse(is.na(studyEndDate), \"\", studyEndDate)")
  );
  lines.push("");
  lines.push(indent("for (t in seq_len(nrow(timeAtRisks))) {"));
  lines.push(indent("for (p in seq_along(psConfigList)) {", 2));
  lines.push(indent("psCfg <- psConfigList[[p]]", 3));
  lines.push("");
  lines.push(indent("if (psCfg$method == \"match\") {", 3));
  lines.push(
    indent(
      [
        "matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(",
        indent(
          [
            "maxRatio = psCfg$params$maxRatio,",
            "caliper = psCfg$params$caliper,",
            "caliperScale = psCfg$params$caliperScale,",
            "allowReverseMatch = FALSE,",
            "stratificationColumns = c()",
          ].join("\n"),
          4
        ),
        ")",
        "stratifyByPsArgs <- NULL",
      ].join("\n"),
      3
    )
  );
  lines.push(indent("} else if (psCfg$method == \"stratify\") {", 3));
  lines.push(
    indent(
      [
        "matchOnPsArgs <- NULL",
        "stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(",
        indent(
          [
            "numberOfStrata = psCfg$params$numberOfStrata,",
            "stratificationColumns = c(),",
            "baseSelection = psCfg$params$baseSelection",
          ].join("\n"),
          4
        ),
        ")",
      ].join("\n"),
      3
    )
  );
  lines.push(indent("}", 3));
  lines.push("");
  lines.push(
    indent(
      "covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(addDescendantsToExclude = TRUE)",
      3
    )
  );
  lines.push("");
  lines.push(indent("outcomeList <- append(", 3));
  lines.push(
    indent(
      [
        "lapply(seq_len(nrow(oList)), function(i) {",
        indent(
          [
            "CohortMethod::createOutcome(",
            indent(
              [
                "outcomeId = oList$outcomeCohortId[i],",
                "outcomeOfInterest = TRUE,",
                "trueEffectSize = NA,",
                "priorOutcomeLookback = 99999",
              ].join("\n"),
              5
            ),
            ")",
          ].join("\n"),
          4
        ),
        "}),",
        "lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {",
        indent(
          [
            "CohortMethod::createOutcome(",
            indent(
              [
                "outcomeId = i,",
                "outcomeOfInterest = FALSE,",
                "trueEffectSize = 1",
              ].join("\n"),
              5
            ),
            ")",
          ].join("\n"),
          4
        ),
        "})",
      ].join("\n"),
      3
    )
  );
  lines.push(indent(")", 3));
  lines.push("");
  lines.push(indent("targetComparatorOutcomesList <- list()", 3));
  lines.push(indent("for (i in seq_len(nrow(cmTcList))) {", 3));
  lines.push(
    indent(
      [
        "targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(",
        indent(
          [
            "targetId = cmTcList$targetCohortId[i],",
            "comparatorId = cmTcList$comparatorCohortId[i],",
            "outcomes = outcomeList,",
            "excludedCovariateConceptIds = excludedCovariateConcepts$conceptId",
          ].join("\n"),
          5
        ),
        ")",
      ].join("\n"),
      4
    )
  );
  lines.push(indent("}", 3));
  lines.push("");
  const dbArgs = dto?.getDbCohortMethodDataArgs ?? {};
  const dbArgsLines = [
    "studyStartDate = studyStartDate",
    "studyEndDate = studyEndDate",
  ];
  const dbExtra = buildArgsObjectLines(dbArgs, new Set(["studyPeriods"]));
  if (dbExtra.length) {
    dbArgsLines.push(...dbExtra);
  }
  dbArgsLines.push("covariateSettings = covariateSettings");
  lines.push(
    indent(
      "getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(",
      3
    )
  );
  lines.push(indent(dbArgsLines.join(",\n"), 4));
  lines.push(indent(")", 3));
  lines.push("");
  const popArgs = dto?.createStudyPopArgs ?? {};
  const normalizedPopArgs = { ...popArgs } as Record<string, any>;
  if (Object.prototype.hasOwnProperty.call(normalizedPopArgs, "priorOutcomeLookBack")) {
    normalizedPopArgs.priorOutcomeLookback = normalizedPopArgs.priorOutcomeLookBack;
    delete normalizedPopArgs.priorOutcomeLookBack;
  }
  const popLines = buildArgsObjectLines(normalizedPopArgs, new Set(["timeAtRisks"]));
  const createPopLines: string[] = [...popLines];
  createPopLines.push("riskWindowStart = timeAtRisks$riskWindowStart[t]");
  createPopLines.push("startAnchor = timeAtRisks$startAnchor[t]");
  createPopLines.push("riskWindowEnd = timeAtRisks$riskWindowEnd[t]");
  createPopLines.push("endAnchor = timeAtRisks$endAnchor[t]");
  createPopLines.push("minDaysAtRisk = timeAtRisks$minDaysAtRisk[t]");
  lines.push(
    indent(
      "createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(",
      3
    )
  );
  lines.push(indent(createPopLines.join(",\n"), 4));
  lines.push(indent(")", 3));
  lines.push("");
  const createPsArgs = dto?.propensityScoreAdjustment?.createPsArgs ?? {};
  const psPrior = buildCyclopsCall("Cyclops::createPrior", createPsArgs.prior);
  const psControl = buildCyclopsCall("Cyclops::createControl", createPsArgs.control);
  const psArgsLines: string[] = [];
  const psArgsExtra = buildArgsObjectLines(createPsArgs, new Set(["prior", "control"]));
  if (psArgsExtra.length) psArgsLines.push(...psArgsExtra);
  psArgsLines.push(`prior = ${psPrior}`);
  psArgsLines.push(`control = ${psControl}`);
  lines.push(indent("createPsArgs <- CohortMethod::createCreatePsArgs(", 3));
  lines.push(indent(psArgsLines.join(",\n"), 4));
  lines.push(indent(")", 3));
  lines.push("");
  lines.push(
    indent(
      "computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(",
      3
    )
  );
  lines.push(indent("maxCohortSize = 250000,\n    covariateFilter = NULL", 4));
  lines.push(indent(")", 3));
  lines.push("");
  lines.push(
    indent(
      "computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(",
      3
    )
  );
  lines.push(
    indent(
      "maxCohortSize = 250000,\n    covariateFilter = FeatureExtraction::getDefaultTable1Specifications()",
      4
    )
  );
  lines.push(indent(")", 3));
  lines.push("");
  const fitArgs = dto?.fitOutcomeModelArgs ?? {};
  const fitPrior = buildCyclopsCall("Cyclops::createPrior", fitArgs.prior);
  const fitControl = buildCyclopsCall("Cyclops::createControl", fitArgs.control);
  const fitLines: string[] = [];
  const fitExtra = buildArgsObjectLines(fitArgs, new Set(["prior", "control"]));
  if (fitExtra.length) fitLines.push(...fitExtra);
  fitLines.push(`prior = ${fitPrior}`);
  fitLines.push(`control = ${fitControl}`);
  lines.push(indent("fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(", 3));
  lines.push(indent(fitLines.join(",\n"), 4));
  lines.push(indent(")", 3));
  lines.push("");
  lines.push(indent("cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(", 3));
  lines.push(
    indent(
      [
        "analysisId = analysisId,",
        "description = sprintf(",
        indent(
          [
            "\"Study: %s-%s; TAR: %s; PS: %s\",",
            "ifelse(nchar(studyStartDate) == 0, \"Unrestricted\", studyStartDate),",
            "ifelse(nchar(studyEndDate) == 0, \"Unrestricted\", studyEndDate),",
            "timeAtRisks$label[t],",
            "psCfg$label",
          ].join("\n"),
          5
        ),
        "),",
        "getDbCohortMethodDataArgs = getDbCohortMethodDataArgs,",
        "createStudyPopArgs = createStudyPopArgs,",
        "createPsArgs = createPsArgs,",
        "matchOnPsArgs = matchOnPsArgs,",
        "stratifyByPsArgs = stratifyByPsArgs,",
        "computeSharedCovariateBalanceArgs = computeSharedCovariateBalanceArgs,",
        "computeCovariateBalanceArgs = computeCovariateBalanceArgs,",
        "fitOutcomeModelArgs = fitOutcomeModelArgs",
      ].join("\n"),
      4
    )
  );
  lines.push(indent(")", 3));
  lines.push(indent("analysisId <- analysisId + 1", 3));
  lines.push(indent("}", 2));
  lines.push(indent("}", 1));
  lines.push("}");
  lines.push("");
  lines.push("cmModuleSettingsCreator <- CohortMethodModule$new()");
  lines.push("cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(");
  lines.push(
    indent(
      [
        "cmAnalysisList = cmAnalysisList,",
        "targetComparatorOutcomesList = targetComparatorOutcomesList,",
        "analysesToExclude = NULL,",
        "refitPsForEveryOutcome = FALSE,",
        "refitPsForEveryStudyPopulation = FALSE,",
        "cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()",
      ].join("\n")
    )
  );
  lines.push(")");
  lines.push("");
  lines.push("# Create the analysis specifications ------------------------------------------");
  lines.push("analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>");
  lines.push("  Strategus::addSharedResources(cohortDefinitionShared) |>");
  lines.push("  Strategus::addSharedResources(negativeControlsShared) |>");
  lines.push("  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>");
  lines.push("  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>");
  lines.push("  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)");
  lines.push("");
  lines.push(`outputJsonPath <- ${outputJsonExpr}`);
  lines.push("dir.create(dirname(outputJsonPath), recursive = TRUE, showWarnings = FALSE)");
  lines.push("ParallelLogger::saveSettingsToJson(");
  lines.push(indent("analysisSpecifications,"));
  lines.push(indent("outputJsonPath"));
  lines.push(")");
  lines.push("");
  return lines.join("\n");
}
