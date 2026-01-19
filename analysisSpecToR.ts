import fs from "node:fs/promises";
import path from "node:path";
import { pathToFileURL } from "node:url";

type AnalysisSpecToROptions = {
    studyName?: string;
    outputJsonPath?: string;
    vendor?: string;
    size?: string;
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
    const escaped = value
        .replace(/\\/g, "\\\\")
        .replace(/"/g, '\\"')
        .replace(/\r/g, "\\r")
        .replace(/\n/g, "\\n")
        .replace(/\t/g, "\\t");
    return `"${escaped}"`;
}

function isScalar(value: unknown): boolean {
    return (
        value === null ||
        typeof value === "string" ||
        typeof value === "number" ||
        typeof value === "boolean"
    );
}

function isDataFrameLike(value: any): boolean {
    if (!value || typeof value !== "object" || Array.isArray(value)) return false;
    const cls = value.attr_class;
    if (Array.isArray(cls)) return cls.includes("data.frame");
    return cls === "data.frame";
}

function extractAttributes(obj: Record<string, any>): Record<string, any> {
    const attrs: Record<string, any> = {};
    for (const [key, value] of Object.entries(obj)) {
        if (key.startsWith("attr_")) {
            attrs[key.slice("attr_".length)] = value;
        }
    }
    return attrs;
}

function renderRValue(
    value: any,
    opts: { nullAsNA?: boolean; stringAsNA?: boolean } = {}
): string {
    if (Array.isArray(value)) {
        return renderRArray(value);
    }
    if (value === null) return opts.nullAsNA ? "NA" : "NULL";
    if (typeof value === "string") {
        if (opts.stringAsNA && value === "NA") return "NA";
        return escapeRString(value);
    }
    if (typeof value === "number") {
        if (!Number.isFinite(value)) return "NA";
        return String(value);
    }
    if (typeof value === "boolean") return value ? "TRUE" : "FALSE";
    if (typeof value === "object") return renderRObject(value);
    return "NULL";
}

function renderRArray(values: any[]): string {
    if (!values.length) return "list()";
    const scalar = values.every(isScalar);
    const rendered = values.map((v) =>
        renderRValue(v, { nullAsNA: true, stringAsNA: true })
    );
    if (scalar) return `c(${rendered.join(", ")})`;
    return `list(${rendered.join(", ")})`;
}

function renderDataFrame(obj: Record<string, any>): string {
    const keys = Object.keys(obj).filter((k) => !k.startsWith("attr_"));
    const cols = keys.map((k) => `${k} = ${renderRValue(obj[k])}`);
    cols.push("stringsAsFactors = FALSE");
    const dfExpr = `data.frame(\n${indent(cols.join(",\n"))}\n)`;
    const attrs = extractAttributes(obj);
    const attrEntries = Object.entries(attrs);
    if (!attrEntries.length) {
        return dfExpr;
    }
    const attrLines = attrEntries.map(([k, v]) => {
        if (k === "row.names" && Array.isArray(v)) {
            return `${k} = as.integer(${renderRArray(v)})`;
        }
        return `${k} = ${renderRValue(v, { nullAsNA: true, stringAsNA: true })}`;
    });
    return `structure(\n${indent(dfExpr)},\n${indent(attrLines.join(",\n"))}\n)`;
}

function renderStructure(obj: Record<string, any>): string {
    const attrs = extractAttributes(obj);
    const keys = Object.keys(obj).filter((k) => !k.startsWith("attr_"));
    if (!keys.length && !Object.keys(attrs).length) {
        return "structure(list(), names = character())";
    }
    const parts = keys.map((k) => `${k} = ${renderRValue(obj[k])}`);
    const listExpr = `list(\n${indent(parts.join(",\n"))}\n)`;
    const attrEntries = Object.entries(attrs);
    if (!attrEntries.length) return listExpr;
    const attrLines = attrEntries.map(
        ([k, v]) => `${k} = ${renderRValue(v, { nullAsNA: true, stringAsNA: true })}`
    );
    return `structure(\n${indent(listExpr)},\n${indent(attrLines.join(",\n"))}\n)`;
}

function renderRObject(obj: Record<string, any>): string {
    if (isDataFrameLike(obj)) return renderDataFrame(obj);
    return renderStructure(obj);
}

function renderArgsCall(fnName: string, argsObj: Record<string, any>): string {
    const entries = Object.entries(argsObj).filter(
        ([key]) => !key.startsWith("attr_")
    );
    if (!entries.length) return `${fnName}()`;
    const args = entries.map(([key, value]) => `${key} = ${renderArgValue(value)}`);
    return `${fnName}(\n${indent(args.join(",\n"))}\n)`;
}

function renderArgValue(value: any): string {
    if (value && typeof value === "object" && !Array.isArray(value)) {
        const cls = value.attr_class;
        if (cls === "cyclopsPrior") {
            return renderStructure(value);
        }
        if (cls === "cyclopsControl") {
            return renderStructure(value);
        }
        if (cls === "covariateSettings") {
            return renderStructure(value);
        }
        if (isDataFrameLike(value)) {
            return renderDataFrame(value);
        }
    }
    return renderRValue(value);
}

function normalizeNumberish(value: any): number | string {
    if (typeof value === "number") return value;
    if (typeof value === "string" && /^-?\d+(\.\d+)?$/.test(value)) {
        return Number(value);
    }
    return value;
}

function renderCohortDefinitionSet(cohorts: any[]): string {
    const cohortIds = cohorts.map((c) => normalizeNumberish(c.cohortId));
    const cohortNames = cohorts.map((c) => c.cohortName ?? "");
    const cohortJson = cohorts.map((c) => c.cohortDefinition ?? "");
    const cohortSql = cohorts.map(() => "");
    const lines = [
        `cohortId = ${renderRArray(cohortIds)}`,
        `cohortName = ${renderRArray(cohortNames)}`,
        `sql = ${renderRArray(cohortSql)}`,
        `json = ${renderRArray(cohortJson)}`,
        "stringsAsFactors = FALSE",
    ];
    return `cohortDefinitionSet <- data.frame(\n${indent(lines.join(",\n"))}\n)`;
}

function renderNegativeControlSet(negativeControls: any[]): string {
    const cohortIds = negativeControls.map((c) =>
        normalizeNumberish(c.cohortId)
    );
    const cohortNames = negativeControls.map((c) => c.cohortName ?? "");
    const outcomeConceptIds = negativeControls.map((c) =>
        normalizeNumberish(c.outcomeConceptId)
    );
    const lines = [
        `cohortId = ${renderRArray(cohortIds)}`,
        `cohortName = ${renderRArray(cohortNames)}`,
        `outcomeConceptId = ${renderRArray(outcomeConceptIds)}`,
        "stringsAsFactors = FALSE",
    ];
    return `negativeControlOutcomeCohortSet <- data.frame(\n${indent(
        lines.join(",\n")
    )}\n)`;
}

function renderTargetComparatorOutcomesList(listItems: any[]): string {
    const lines: string[] = ["targetComparatorOutcomesList <- list()"];
    listItems.forEach((item, idx) => {
        const block = `targetComparatorOutcomesList[[${idx + 1}]] <- ${renderStructure(
            item
        )}`;
        lines.push(block);
    });
    return lines.join("\n\n");
}

function renderCmAnalysisList(listItems: any[]): string {
    const lines: string[] = ["cmAnalysisList <- list()"];
    listItems.forEach((item, idx) => {
        const args: string[] = [
            `analysisId = ${renderRValue(item.analysisId)}`,
            `description = ${renderRValue(item.description)}`,
            `getDbCohortMethodDataArgs = ${renderArgsCall(
                "CohortMethod::createGetDbCohortMethodDataArgs",
                item.getDbCohortMethodDataArgs ?? {}
            )}`,
            `createStudyPopArgs = ${renderArgsCall(
                "CohortMethod::createCreateStudyPopulationArgs",
                item.createStudyPopArgs ?? {}
            )}`,
            `createPsArgs = ${renderArgsCall(
                "CohortMethod::createCreatePsArgs",
                item.createPsArgs ?? {}
            )}`,
            `matchOnPsArgs = ${
                item.matchOnPsArgs
                    ? renderArgsCall("CohortMethod::createMatchOnPsArgs", item.matchOnPsArgs)
                    : "NULL"
            }`,
            `stratifyByPsArgs = ${
                item.stratifyByPsArgs
                    ? renderArgsCall(
                          "CohortMethod::createStratifyByPsArgs",
                          item.stratifyByPsArgs
                      )
                    : "NULL"
            }`,
            `computeSharedCovariateBalanceArgs = ${renderArgsCall(
                "CohortMethod::createComputeCovariateBalanceArgs",
                item.computeSharedCovariateBalanceArgs ?? {}
            )}`,
            `computeCovariateBalanceArgs = ${renderArgsCall(
                "CohortMethod::createComputeCovariateBalanceArgs",
                item.computeCovariateBalanceArgs ?? {}
            )}`,
            `fitOutcomeModelArgs = ${renderArgsCall(
                "CohortMethod::createFitOutcomeModelArgs",
                item.fitOutcomeModelArgs ?? {}
            )}`,
        ];
        const block = `cmAnalysisList[[${idx + 1}]] <- CohortMethod::createCmAnalysis(\n${indent(
            args.join(",\n")
        )}\n)`;
        lines.push(block);
    });
    return lines.join("\n\n");
}

function looksLikeAnalysisSpec(obj: any): boolean {
    return (
        obj &&
        typeof obj === "object" &&
        Array.isArray(obj.sharedResources) &&
        Array.isArray(obj.moduleSpecifications)
    );
}

function parseAnalysisSpec(input: Record<string, any> | string): Record<string, any> {
    if (typeof input === "string") {
        try {
            const parsed = JSON.parse(input);
            if (!looksLikeAnalysisSpec(parsed)) {
                throw new Error(
                    "Input JSON does not look like analysisSpecifications (missing sharedResources/moduleSpecifications)."
                );
            }
            return parsed;
        } catch (err: any) {
            if (err instanceof SyntaxError) {
                throw new Error("Invalid JSON string passed to analysisSpecToR.");
            }
            throw err;
        }
    }
    if (!looksLikeAnalysisSpec(input)) {
        throw new Error(
            "Input object does not look like analysisSpecifications (missing sharedResources/moduleSpecifications)."
        );
    }
    return input;
}

export function analysisSpecToR(
    analysisSpecOrJson: Record<string, any> | string,
    opts: AnalysisSpecToROptions = {}
): string {
    const analysisSpec = parseAnalysisSpec(analysisSpecOrJson);
    const studyName = opts.studyName ?? "studyName";
    const outputJsonPath =
        opts.outputJsonPath ??
        `file.path("inst", "${studyName}", "${studyName}AnalysisSpecification.json")`;

    const sharedResources = Array.isArray(analysisSpec.sharedResources)
        ? analysisSpec.sharedResources
        : [];
    const cohortDefinitions =
        sharedResources.find((sr) => sr.cohortDefinitions)?.cohortDefinitions ?? [];
    const negativeControlOutcomes =
        sharedResources.find((sr) => sr.negativeControlOutcomes)?.negativeControlOutcomes ??
        { negativeControlOutcomeCohortSet: [], occurrenceType: "first", detectOnDescendants: true };

    const moduleSpecs = Array.isArray(analysisSpec.moduleSpecifications)
        ? analysisSpec.moduleSpecifications
        : [];
    const cohortGenerator = moduleSpecs.find(
        (m) => m.module === "CohortGeneratorModule"
    );
    const cohortDiagnostics = moduleSpecs.find(
        (m) => m.module === "CohortDiagnosticsModule"
    );
    const cohortMethod = moduleSpecs.find(
        (m) => m.module === "CohortMethodModule"
    );

    const lines: string[] = [];
    lines.push("# Generated from analysisSpecifications JSON");
    lines.push("library(Strategus)");
    lines.push("");
    lines.push("# Shared Resources -------------------------------------------------------------");
    lines.push(renderCohortDefinitionSet(cohortDefinitions));
    lines.push("");
    lines.push(
        renderNegativeControlSet(
            negativeControlOutcomes.negativeControlOutcomeCohortSet ?? []
        )
    );
    lines.push("");
    lines.push(
        "if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {"
    );
    lines.push(INDENT + 'stop("*** Error: duplicate cohort IDs found ***")');
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
                `occurrenceType = ${renderRValue(
                    negativeControlOutcomes.occurrenceType
                )},`,
                `detectOnDescendants = ${renderRValue(
                    negativeControlOutcomes.detectOnDescendants
                )}`,
            ].join("\n")
        )
    );
    lines.push(")");
    lines.push(
        `cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(\n${indent(
            `generateStats = ${renderRValue(
                cohortGenerator?.settings?.generateStats ?? false
            )}`
        )}\n)`
    );
    lines.push("");
    lines.push("# CohortDiagnosticsModule ------------------------------------------------------");
    lines.push("cdModuleSettingsCreator <- CohortDiagnosticsModule$new()");
    if (cohortDiagnostics?.settings?.temporalCovariateSettings) {
        lines.push(
            `temporalCovariateSettings <- ${renderStructure(
                cohortDiagnostics.settings.temporalCovariateSettings
            )}`
        );
        lines.push("");
    }
    const cdSettings = cohortDiagnostics?.settings ?? {};
    const cdArgs: string[] = [];
    for (const [key, value] of Object.entries(cdSettings)) {
        if (key.startsWith("attr_")) continue;
        if (key === "temporalCovariateSettings") {
            cdArgs.push("temporalCovariateSettings = temporalCovariateSettings");
            continue;
        }
        cdArgs.push(`${key} = ${renderArgValue(value)}`);
    }
    lines.push(
        `cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(\n${indent(
            cdArgs.join(",\n")
        )}\n)`
    );
    lines.push("");
    lines.push("# CohortMethodModule -----------------------------------------------------------");
    const cmSettings = cohortMethod?.settings ?? {};
    lines.push(
        renderTargetComparatorOutcomesList(cmSettings.targetComparatorOutcomesList ?? [])
    );
    lines.push("");
    lines.push(renderCmAnalysisList(cmSettings.cmAnalysisList ?? []));
    lines.push("");
    if (cmSettings.cmDiagnosticThresholds) {
        lines.push(
            `cmDiagnosticThresholds <- ${renderArgsCall(
                "CohortMethod::createCmDiagnosticThresholds",
                cmSettings.cmDiagnosticThresholds
            )}`
        );
        lines.push("");
    }
    const cmArgs: string[] = [];
    for (const [key, value] of Object.entries(cmSettings)) {
        if (key.startsWith("attr_")) continue;
        if (key === "targetComparatorOutcomesList") {
            cmArgs.push("targetComparatorOutcomesList = targetComparatorOutcomesList");
            continue;
        }
        if (key === "cmAnalysisList") {
            cmArgs.push("cmAnalysisList = cmAnalysisList");
            continue;
        }
        if (key === "cmDiagnosticThresholds") {
            cmArgs.push("cmDiagnosticThresholds = cmDiagnosticThresholds");
            continue;
        }
        cmArgs.push(`${key} = ${renderArgValue(value)}`);
    }
    lines.push("cmModuleSettingsCreator <- CohortMethodModule$new()");
    lines.push(
        `cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(\n${indent(
            cmArgs.join(",\n")
        )}\n)`
    );
    lines.push("");
    lines.push("# Create the analysis specifications ------------------------------------------");
    lines.push(
        "analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>"
    );
    lines.push(
        "  Strategus::addSharedResources(cohortDefinitionShared) |>"
    );
    lines.push(
        "  Strategus::addSharedResources(negativeControlsShared) |>"
    );
    lines.push(
        "  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>"
    );
    lines.push(
        "  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>"
    );
    lines.push(
        "  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)"
    );
    lines.push("");
    lines.push("ParallelLogger::saveSettingsToJson(");
    lines.push(indent("analysisSpecifications,"));
    lines.push(indent(outputJsonPath));
    lines.push(")");
    lines.push("");

    return lines.join("\n");
}

export async function analysisSpecToRLikeJson2strategus(
    analysisSpecifications: string,
    opts: AnalysisSpecToROptions = {}
): Promise<string> {
    return analysisSpecToR(analysisSpecifications, opts);
}

async function main() {
    const args = process.argv.slice(2);
    let inputPath: string | undefined;
    let outputPath: string | undefined;
    let outDir: string | undefined;
    let outputJsonPath: string | undefined;
    for (let i = 0; i < args.length; i++) {
        const arg = args[i];
        if (arg === "--out-dir") {
            outDir = args[i + 1];
            i += 1;
            continue;
        }
        if (arg === "--output-json") {
            outputJsonPath = args[i + 1];
            i += 1;
            continue;
        }
        if (!inputPath) {
            inputPath = arg;
            continue;
        }
        if (!outputPath) {
            outputPath = arg;
            continue;
        }
    }
    if (!inputPath) {
        console.error(
            "Usage: <ts runner> analysisSpecToR.ts <analysisSpec.json> [output.R] [--out-dir <dir>] [--output-json <path>]"
        );
        process.exit(1);
    }
    const absInput = path.resolve(process.cwd(), inputPath);
    const jsonText = await fs.readFile(absInput, "utf8");
    const analysisSpec = JSON.parse(jsonText);
    const baseName = path.basename(absInput);
    const match = baseName.match(/^(.*)AnalysisSpecification\.json$/);
    const studyName = match?.[1] || "studyName";
    const rScript = analysisSpecToR(analysisSpec, {
        studyName,
        outputJsonPath: outputJsonPath ? renderRValue(outputJsonPath) : undefined,
    });
    const defaultOutDir = path.resolve(
        process.cwd(),
        "generated_r",
        "analysisSpec"
    );
    let finalOutPath = outputPath
        ? path.resolve(process.cwd(), outputPath)
        : path.join(
              path.resolve(process.cwd(), outDir ?? defaultOutDir),
              `${studyName}_CreateStrategusAnalysisSpecification_fromJson.R`
          );
    await fs.mkdir(path.dirname(finalOutPath), { recursive: true });
    await fs.writeFile(finalOutPath, rScript, "utf8");
    console.log(`[OK] Wrote ${path.relative(process.cwd(), finalOutPath)}`);
}

if (import.meta.url === pathToFileURL(process.argv[1]).href) {
    void main();
}
