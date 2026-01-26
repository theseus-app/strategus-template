import path from "node:path";
import fs from "node:fs/promises";

import { json2strategus } from "./json2strategus.ts";
import { loadFile } from "./loadFile.ts";
import type { ModulePair } from "./loadFile.ts";
import { defaultDTO, fillWithDefaults } from "./studyDto.ts";
import type { StudyDTO } from "./studyDto.ts";

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
    if (k && v) {
      const key = k.replace(/^--/, "").toLowerCase();
      // case는 대소문자 유지 (study name)
      argMap[key] = key === "case" ? v : v.toUpperCase();
    }
  }

  const vendor = argMap["vendor"];
  const size = argMap["size"];
  const type = argMap["type"]; // ✅ DEFAULT | PRIMARY
  const source = argMap["source"] || "GOLDSTANDARD"; // ✅ GOLDSTANDARD | GOLDSTANDARDTEST
  const caseFilter = argMap["case"] || ""; // ✅ 특정 케이스만 실행 (예: UveitisSafety)

  if (!vendor || !size || !type) {
    console.error(
      "❌ Usage: node --experimental-strip-types getRScripts.ts --vendor=OPENAI|GEMINI|DEEPSEEK|CLAUDE --size=FLAGSHIP|LIGHT --type=DEFAULT|PRIMARY [--source=GOLDSTANDARD|GOLDSTANDARDTEST] [--case=StudyName]",
    );
    process.exit(1);
  }

  const supportedVendors = ["OPENAI", "GEMINI", "DEEPSEEK", "CLAUDE"];
  const supportedSizes = ["FLAGSHIP", "LIGHT"];
  const supportedTypes = ["DEFAULT", "PRIMARY"];
  const supportedSources = ["GOLDSTANDARD", "GOLDSTANDARDTEST"];

  if (
    !supportedVendors.includes(vendor) ||
    !supportedSizes.includes(size) ||
    !supportedTypes.includes(type) ||
    !supportedSources.includes(source)
  ) {
    console.error(
      `❌ Invalid vendor/size/type/source. vendors: ${supportedVendors.join(
        ", ",
      )} / sizes: ${supportedSizes.join(", ")} / types: ${supportedTypes.join(", ")} / sources: ${supportedSources.join(", ")}`,
    );
    process.exit(1);
  }

  return { vendor, size, type, source, caseFilter };
}

const { vendor, size, type, source, caseFilter } = parseArgs();

// ✅ 결과 저장 폴더: public/firstScripts/{source}/{type}/{vendor}_{size}
// goldStandard -> firstScripts, goldStandardTest -> firstScriptsTest
const outputFolder = source === "GOLDSTANDARDTEST" ? "firstScriptsTest" : "firstScripts";
const RS_DIR = path.resolve(
  process.cwd(),
  "public",
  outputFolder,
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
  // ✅ type과 source에 따라 goldStandard/goldStandardTest 위치가 결정되도록 loadFile에 전달
  const sourceFolder = source === "GOLDSTANDARDTEST" ? "goldStandardTest" : "goldStandard";
  let pairs: ModulePair[] = await loadFile(type, sourceFolder);
  if (!pairs.length) console.warn(`[WARN] No module pairs found for type=${type}, source=${source}.`);

  // ✅ --case 옵션으로 특정 케이스만 필터링
  if (caseFilter) {
    const filterLower = caseFilter.toLowerCase();
    pairs = pairs.filter((p) => p.name.toLowerCase().includes(filterLower));
    if (!pairs.length) {
      console.error(`[ERROR] No matching case found for --case=${caseFilter}`);
      process.exit(1);
    }
    console.log(`[INFO] Filtering to case: ${pairs.map((p) => p.name).join(", ")}`);
  }

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

// 단독 실행 (ESM)
getRScripts().catch((e) => {
  console.error(e);
  process.exit(1);
});
