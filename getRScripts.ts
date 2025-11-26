import path from "node:path";
import fs from "node:fs/promises";

import { json2strategus } from "./json2strategus";
import { loadFile, ModulePair } from "./loadFile";

// ===== 재사용 타입들 =====
export type Anchor = "cohort start" | "cohort end";
export type RemoveDuplicate = "keep all" | "keep first" | "remove all";
export type CaliperScale = "propensity score" | "standardized" | "standardized logit";
export type BaseSelection = "all" | "target" | "comparator";
export type ModelType = "logistic" | "poisson" | "cox";
export type CvType = "auto";
export type NoiseLevel = "silent" | "quiet" | "noisy";

export interface MatchOnPsArgs {
  maxRatio: number; // 0 = no max
  caliper: number; // 0 = off
  caliperScale: CaliperScale;
}
export interface StratifyByPsArgs {
  numberOfStrata: number;
  baseSelection: BaseSelection;
}
export interface Prior {
  priorType: "laplace";
  useCrossValidation: boolean;
}
export interface Control {
  tolerance: number;
  cvType: CvType;
  fold: number;
  cvRepetitions: number;
  noiseLevel: NoiseLevel;
  resetCoefficients: boolean;
  startingVariance: number; // -1 = auto
}
export interface PsSetting {
  description: string;
  matchOnPsArgs: MatchOnPsArgs | null;
  stratifyByPsArgs: StratifyByPsArgs | null;
}

export type StudyDTO = {
  name: string;
  cohortDefinitions: {
    targetCohort: { id: number | null; name: string };
    comparatorCohort: { id: number | null; name: string };
    outcomeCohort: { id: number | null; name: string }[];
  };
  negativeControlConceptSet: { id: number | null; name: string };
  covariateSelection: {
    conceptsToInclude: { id: number | null; name: string }[];
    conceptsToExclude: { id: number | null; name: string }[];
  };
  getDbCohortMethodDataArgs: {
    studyPeriods: {
      studyStartDate: string | number | null;
      studyEndDate: string | number | null;
    }[]; // yyyyMMdd
    maxCohortSize: number; // 0 = no limit
  };
  createStudyPopArgs: {
    restrictToCommonPeriod: boolean;
    firstExposureOnly: boolean;
    washoutPeriod: number;
    removeDuplicateSubjects: RemoveDuplicate;
    censorAtNewRiskWindow: boolean;
    removeSubjectsWithPriorOutcome: boolean;
    priorOutcomeLookBack: number;
    timeAtRisks: {
      description?: string;
      riskWindowStart: number;
      startAnchor: Anchor;
      riskWindowEnd: number;
      endAnchor: Anchor;
      minDaysAtRisk: number;
    }[];
  };
  propensityScoreAdjustment: {
    psSettings: PsSetting[];
    createPsArgs: {
      maxCohortSizeForFitting: number; // 0 = no downsample
      errorOnHighCorrelation: boolean;
      prior: Prior | null;
      control: Control | null;
    };
  };
  fitOutcomeModelArgs: {
    modelType: ModelType;
    stratified: boolean;
    useCovariates: boolean;
    inversePtWeighting: boolean;
    prior: Prior | null;
    control: Control | null;
  };
};

// ✅ 기본 DTO (DEFAULT 스키마 기준)
export const defaultDTO: StudyDTO = {
  name: "",
  cohortDefinitions: {
    targetCohort: { id: null, name: "" },
    comparatorCohort: { id: null, name: "" },
    outcomeCohort: [{ id: null, name: "" }],
  },
  negativeControlConceptSet: { id: null, name: "" },
  covariateSelection: {
    conceptsToInclude: [{ id: null, name: "" }],
    conceptsToExclude: [{ id: null, name: "" }],
  },
  getDbCohortMethodDataArgs: {
    studyPeriods: [
      {
        studyStartDate: null,
        studyEndDate: null,
      },
    ],
    maxCohortSize: 0,
  },
  createStudyPopArgs: {
    restrictToCommonPeriod: false,
    firstExposureOnly: false,
    washoutPeriod: 0,
    removeDuplicateSubjects: "keep all",
    censorAtNewRiskWindow: false,
    removeSubjectsWithPriorOutcome: true,
    priorOutcomeLookBack: 99999,
    timeAtRisks: [
      {
        description: "",
        riskWindowStart: 1,
        startAnchor: "cohort start",
        riskWindowEnd: 0,
        endAnchor: "cohort end",
        minDaysAtRisk: 1,
      },
    ],
  },
  propensityScoreAdjustment: {
    psSettings: [
      {
        description: "PS 1",
        matchOnPsArgs: {
          maxRatio: 1,
          caliper: 0.2,
          caliperScale: "standardized logit",
        },
        stratifyByPsArgs: null,
      },
    ],
    createPsArgs: {
      maxCohortSizeForFitting: 250000,
      errorOnHighCorrelation: true,
      prior: { priorType: "laplace", useCrossValidation: true },
      control: {
        tolerance: 2e-7,
        cvType: "auto",
        fold: 10,
        cvRepetitions: 10,
        noiseLevel: "silent",
        resetCoefficients: true,
        startingVariance: 0.01,
      },
    },
  },
  fitOutcomeModelArgs: {
    modelType: "cox",
    stratified: false,
    useCovariates: false,
    inversePtWeighting: false,
    prior: { priorType: "laplace", useCrossValidation: true },
    control: {
      tolerance: 2e-7,
      cvType: "auto",
      fold: 10,
      cvRepetitions: 10,
      noiseLevel: "quiet",
      resetCoefficients: true,
      startingVariance: 0.01,
    },
  },
};

/** PRIMARY 스키마(json) 를 DEFAULT 스키마 모양으로 normalize */
function normalizePrimaryToDefaultShape(raw: any): any {
  if (!raw || typeof raw !== "object") return raw;

  const cloned =
    typeof structuredClone === "function"
      ? structuredClone(raw)
      : JSON.parse(JSON.stringify(raw));

  // 1) studyPeriods: object -> array
  if (
    cloned.getDbCohortMethodDataArgs &&
    cloned.getDbCohortMethodDataArgs.studyPeriods &&
    !Array.isArray(cloned.getDbCohortMethodDataArgs.studyPeriods)
  ) {
    cloned.getDbCohortMethodDataArgs.studyPeriods = [
      cloned.getDbCohortMethodDataArgs.studyPeriods,
    ];
  }

  // 2) timeAtRisks: object -> array
  if (
    cloned.createStudyPopArgs &&
    cloned.createStudyPopArgs.timeAtRisks &&
    !Array.isArray(cloned.createStudyPopArgs.timeAtRisks)
  ) {
    cloned.createStudyPopArgs.timeAtRisks = [cloned.createStudyPopArgs.timeAtRisks];
  }

  // 3) psSettings: object -> array
  if (
    cloned.propensityScoreAdjustment &&
    cloned.propensityScoreAdjustment.psSettings &&
    !Array.isArray(cloned.propensityScoreAdjustment.psSettings)
  ) {
    cloned.propensityScoreAdjustment.psSettings = [
      cloned.propensityScoreAdjustment.psSettings,
    ];
  }

  return cloned;
}

// --- parse CLI args ---
function parseArgs() {
  const args = process.argv.slice(2);
  const argMap: Record<string, string> = {};
  for (const a of args) {
    const [k, v] = a.split("=");
    if (k && v) argMap[k.replace(/^--/, "").toLowerCase()] = v.toUpperCase();
  }

  const vendor = argMap["vendor"];
  const size = argMap["size"];
  const type = argMap["type"]; // ✅ DEFAULT | PRIMARY

  if (!vendor || !size || !type) {
    console.error(
      "❌ Usage: ts-node src/getRScripts.ts --vendor=OPENAI|GEMINI|DEEPSEEK|CLAUDE --size=FLAGSHIP|LIGHT --type=DEFAULT|PRIMARY",
    );
    process.exit(1);
  }

  const supportedVendors = ["OPENAI", "GEMINI", "DEEPSEEK", "CLAUDE"];
  const supportedSizes = ["FLAGSHIP", "LIGHT"];
  const supportedTypes = ["DEFAULT", "PRIMARY"];

  if (
    !supportedVendors.includes(vendor) ||
    !supportedSizes.includes(size) ||
    !supportedTypes.includes(type)
  ) {
    console.error(
      `❌ Invalid vendor/size/type. vendors: ${supportedVendors.join(
        ", ",
      )} / sizes: ${supportedSizes.join(", ")} / types: ${supportedTypes.join(", ")}`,
    );
    process.exit(1);
  }

  return { vendor, size, type };
}

const { vendor, size, type } = parseArgs();

// ✅ 결과 저장 폴더: public/firstScripts/{type}/{vendor}_{size}
const RS_DIR = path.resolve(
  process.cwd(),
  "public",
  "firstScripts",
  type.toLowerCase(), // "default" | "primary"
  `${vendor.toLowerCase()}_${size.toLowerCase()}`, // "openai_flagship" 등
);

// 파일명용 slug
function slugify(s: string) {
  return (
    s
      .trim()
      .toLowerCase()
      .replace(/[\s/\\]+/g, "-")
      .replace(/[^a-z0-9-_]/g, "")
      .replace(/-+/g, "-")
      .replace(/^-|-$/g, "") || "case"
  );
}

async function ensureDir(abs: string) {
  await fs.mkdir(abs, { recursive: true });
}

/** 기본값 병합 (fillWithDefaults) */
function fillWithDefaults<T>(base: T, gold: any): T {
  if (gold === undefined) return base;
  if (gold === null) return gold as T;

  if (Array.isArray(base)) {
    if (Array.isArray(gold)) return gold as T;
    return base;
  }

  if (typeof base === "object" && base !== null) {
    const out: any = {};
    const keys = new Set([...Object.keys(base as any), ...Object.keys(gold ?? {})]);
    for (const k of keys) {
      const bVal = (base as any)[k];
      const gHas = Object.prototype.hasOwnProperty.call(gold, k);
      const gVal = gHas ? (gold as any)[k] : undefined;
      if (gHas) {
        if (
          bVal &&
          typeof bVal === "object" &&
          !Array.isArray(bVal) &&
          gVal &&
          typeof gVal === "object" &&
          !Array.isArray(gVal)
        ) {
          out[k] = fillWithDefaults(bVal, gVal);
        } else if (Array.isArray(bVal)) {
          out[k] = Array.isArray(gVal) ? gVal : bVal;
        } else {
          out[k] = gVal;
        }
      } else {
        out[k] = bVal;
      }
    }
    return out as T;
  }

  return gold as T;
}

// ===== 429 / 네트워크 에러 대비용 helper =====
const MAX_RETRIES = 5;
const BASE_DELAY_MS = 2000; // 1st retry ~2s
const PER_CALL_DELAY_MS = 300; // 성공해도 살짝 쉬어주기

const sleep = (ms: number) => new Promise((res) => setTimeout(res, ms));

function isRateLimitOrRetriableError(err: any): boolean {
  const status =
    err?.status ??
    err?.statusCode ??
    err?.response?.status ??
    err?.response?.statusCode ??
    null;

  if (status === 429) return true;
  if (status && typeof status === "number" && status >= 500) return true;

  const msg = String(err?.message ?? err ?? "").toLowerCase();
  if (msg.includes("rate limit") || msg.includes("too many requests")) return true;
  if (msg.includes("ecconnreset") || msg.includes("etimedout") || msg.includes("enotfound"))
    return true;

  return false;
}

async function safeJson2strategus(
  dtoJson: string,
  opts: { vendor: string; size: string },
  caseName: string,
): Promise<string> {
  let lastError: any = null;

  for (let attempt = 0; attempt < MAX_RETRIES; attempt++) {
    if (attempt > 0) {
      const delay =
        BASE_DELAY_MS * Math.pow(2, attempt - 1) + Math.random() * 500;
      console.warn(
        `[WARN] ${caseName}: retrying json2strategus (attempt ${
          attempt + 1
        }/${MAX_RETRIES}) after ${Math.round(delay)} ms ...`,
      );
      await sleep(delay);
    }

    try {
      const script = await json2strategus(dtoJson, opts as any);
      // 성공한 뒤에도 살짝 딜레이 → QPS 완화
      await sleep(PER_CALL_DELAY_MS);
      return script;
    } catch (err: any) {
      lastError = err;
      if (!isRateLimitOrRetriableError(err) || attempt === MAX_RETRIES - 1) {
        console.error(
          `[ERROR] ${caseName}: json2strategus failed (attempt ${
            attempt + 1
          }/${MAX_RETRIES}) → giving up.`,
        );
        throw err;
      } else {
        console.warn(
          `[WARN] ${caseName}: json2strategus failed (attempt ${
            attempt + 1
          }/${MAX_RETRIES}) – ${String(err?.message ?? err)}`,
        );
      }
    }
  }

  throw lastError ?? new Error("json2strategus failed after retries");
}

type PerCase = {
  name: string;
  fileName: string;
  createdAt: string;
  savedPath: string;
};

export async function getRScripts() {
  // ✅ type에 따라 goldStandard 위치가 결정되도록 loadFile에 type 전달
  const pairs: ModulePair[] = await loadFile(type);
  if (!pairs.length) console.warn(`[WARN] No module pairs found for type=${type}.`);

  await ensureDir(RS_DIR);

  const results: PerCase[] = [];

  for (const p of pairs) {
    const caseSlug = slugify(p.name);
    const outName = `${caseSlug}.R`;
    const outPath = path.join(RS_DIR, outName);

    try {
      const goldForMerge =
        type === "PRIMARY" ? normalizePrimaryToDefaultShape(p.goldJson) : p.goldJson;

      const dto: StudyDTO = fillWithDefaults<StudyDTO>(defaultDTO, goldForMerge);
      dto.name = caseSlug;
      dto.cohortDefinitions = {
        targetCohort: { id: 1794126, name: "target1" },
        comparatorCohort: { id: 1794132, name: "comparator1" },
        outcomeCohort: [{ id: 1794131, name: "outcome1" }],
      };
      dto.negativeControlConceptSet = { id: 1888110, name: "negative" };

      const script = await safeJson2strategus(
        JSON.stringify(dto, null, 2),
        { vendor, size },
        p.name,
      );

      await fs.writeFile(outPath, script, "utf8");

      results.push({
        name: p.name,
        fileName: outName,
        createdAt: new Date().toISOString(),
        savedPath: path.relative(process.cwd(), outPath),
      });

      console.log(`[OK] Saved R script: ${outName}`);
    } catch (err) {
      // 여기서 에러를 잡으니까, 한 케이스 실패해도 전체 배치는 계속 감
      console.error(`[ERROR] ${p.name}:`, err);
    }
  }

  const indexPath = path.join(RS_DIR, "_summary.index.json");
  await fs.writeFile(
    indexPath,
    JSON.stringify(
      {
        createdAt: new Date().toISOString(),
        totalCases: results.length,
        results,
      },
      null,
      2,
    ),
    "utf8",
  );

  console.log(
    `[DONE] ${results.length} scripts saved. Summary → ${path.relative(
      process.cwd(),
      indexPath,
    )}`,
  );
}

// 단독 실행
if (require.main === module) {
  getRScripts().catch((e) => {
    console.error(e);
    process.exit(1);
  });
}
