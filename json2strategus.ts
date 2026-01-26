import fs from "node:fs/promises";
import path from "node:path";
import OpenAI from "openai";
import dotenv from "dotenv";
import { pathToFileURL } from "node:url";
import { studyDtoToR } from "./studyDtoToR.ts";
import { defaultDTO, fillWithDefaults } from "./studyDto.ts";
dotenv.config();

const TEMPLATE_PATH = path.resolve(
    process.cwd(),
    "public",
    "templates",
    "CreateStrategusAnalysisSpecification_template.R"
);
// const OPENAI_API_KEY = 'openai-api-key' // <- 더 이상 안 써도 됨.

/** Remove code fences (```lang ... ```) from LLM outputs. */
export function stripCodeFences(text: string): string {
    let cleaned = text.trim();
    cleaned = cleaned.replace(/^```[\w-]*\n?/, "");
    cleaned = cleaned.replace(/\n?```$/, "");
    return cleaned.trim();
}

async function readTextFile(abs: string) {
    return fs.readFile(abs, "utf8");
}

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

async function loadJsonInput(absInput: string): Promise<{ dto: any; name: string }> {
    const ext = path.extname(absInput).toLowerCase();
    if (ext === ".ts" || ext === ".js" || ext === ".mjs" || ext === ".cjs") {
        const mod = await import(pathToFileURL(absInput).href);
        const jsonKeys = Object.keys(mod).filter((k) => /^JSON/i.test(k));
        if (jsonKeys.length !== 1) {
            throw new Error(
                `Expected exactly 1 JSON* export in ${absInput}, found ${jsonKeys.length}.`
            );
        }
        const key = jsonKeys[0];
        const dto = mod[key];
        const rawName = key.replace(/^JSON/i, "") || path.basename(absInput, ext);
        return { dto, name: slugify(rawName) };
    }
    const jsonText = await readTextFile(absInput);
    const dto = JSON.parse(jsonText);
    const rawName = path.basename(absInput, ext);
    return { dto, name: slugify(rawName) };
}

// --- 모델 맵 ---
export const MODEL_MAP = {
    OPENAI: {
        FLAGSHIP: { name: "gpt-5.2-2025-12-11", key: process.env.OPENAI_API_KEY ?? "openai-api-key" },
        LIGHT: { name: "gpt-4.1-2025-04-14", key: process.env.OPENAI_API_KEY ?? "openai-api-key" },
    },
    CLAUDE: {
        FLAGSHIP: { name: "claude-opus-4-5-20251101", key: process.env.CLAUDE_API_KEY ?? "claude-api-key" },
        LIGHT: { name: "claude-haiku-4-5-20251001", key: process.env.CLAUDE_API_KEY ?? "claude-api-key" },
    },
    GEMINI: {
        FLAGSHIP: { name: "gemini-2.5-pro", key: process.env.GOOGLE_API_KEY ?? "google_api_key" }, //2025-11 최종 업데이트
        LIGHT: { name: "gemini-2.5-flash", key: process.env.GOOGLE_API_KEY ?? "google_api_key" }, //2025-12 최종 업데이트
    },
    DEEPSEEK: {
        FLAGSHIP: { name: "deepseek-reasoner", key: process.env.DEEPSEEK_API_KEY ?? "deepseek-api-key" },
        LIGHT: { name: "deepseek-chat", key: process.env.DEEPSEEK_API_KEY ?? "deepseek-api-key" },
    },
} as const;

type Vendor = keyof typeof MODEL_MAP;
type ModelSize = "FLAGSHIP" | "LIGHT";

// ===== 재시도 로직 =====
const MAX_RETRIES = 5;
const BASE_DELAY_MS = 2000;
const sleep = (ms: number) => new Promise((res) => setTimeout(res, ms));

function isRetriableStatus(status: number): boolean {
    // 429: rate limit, 529: overloaded, 500+: server errors
    return status === 429 || status === 529 || status >= 500;
}

/**
 * 공통 LLM 호출 함수 (재시도 로직 포함)
 */
async function callLLM(prompt: string, vendor: Vendor, size: ModelSize): Promise<string> {
    const selected = MODEL_MAP[vendor][size];

    // 1) API 키 체크 (env 안 들어가 있으면 바로 에러)
    if (!selected.key || selected.key.endsWith("-api-key")) {
        throw new Error(
            `[callLLM] Missing or placeholder API key for ${vendor}.${size}. ` +
            `Please set the proper environment variable (e.g. OPENAI_API_KEY, CLAUDE_API_KEY, GOOGLE_API_KEY, DEEPSEEK_API_KEY).`
        );
    }

    console.log(`[callLLM] vendor=${vendor}, size=${size}, model=${selected.name}`);

    let lastError: any = null;

    for (let attempt = 0; attempt < MAX_RETRIES; attempt++) {
        if (attempt > 0) {
            const delay = BASE_DELAY_MS * Math.pow(2, attempt - 1) + Math.random() * 500;
            console.warn(`[callLLM] Retrying (attempt ${attempt + 1}/${MAX_RETRIES}) after ${Math.round(delay)}ms...`);
            await sleep(delay);
        }

        try {
            return await callLLMOnce(prompt, vendor, size, selected);
        } catch (err: any) {
            lastError = err;
            const status = err?.status ?? 0;
            const msg = String(err?.message ?? "");
            const statusMatch = msg.match(/API error: (\d+)/);
            const extractedStatus = statusMatch ? parseInt(statusMatch[1], 10) : status;

            if (isRetriableStatus(extractedStatus)) {
                console.warn(`[callLLM] Retriable error (status=${extractedStatus}): ${msg}`);
                if (attempt < MAX_RETRIES - 1) continue;
            }
            throw err;
        }
    }

    throw lastError ?? new Error("callLLM failed after retries");
}

/**
 * 단일 LLM 호출 (재시도 없음)
 */
async function callLLMOnce(prompt: string, vendor: Vendor, size: ModelSize, selected: { name: string; key: string }): Promise<string> {

    let completionText = "";

    if (vendor === "OPENAI") {
        const openai = new OpenAI({ apiKey: selected.key });

        const res = await openai.chat.completions.create({
            model: selected.name,
            messages: [{ role: "user", content: prompt }],
            temperature: 0,
        });

        const msg: any = res.choices[0]?.message;
        if (!msg) {
            console.error("[callLLM][OPENAI] No choices in response:", JSON.stringify(res, null, 2));
            throw new Error("OpenAI returned no choices");
        }

        // v4 SDK에서 content가 string 또는 array일 수 있어서 둘 다 케어
        if (typeof msg.content === "string") {
            completionText = msg.content;
        } else if (Array.isArray(msg.content)) {
            completionText = msg.content
                .map((part: any) => (typeof part === "string" ? part : part.text ?? ""))
                .join("");
        }
    } else if (vendor === "GEMINI") {
        const resp = await fetch(
            `https://generativelanguage.googleapis.com/v1beta/models/${selected.name}:generateContent?key=${selected.key}`,
            {
                method: "POST",
                headers: { "Content-Type": "application/json" },
                body: JSON.stringify({
                    contents: [{ parts: [{ text: prompt }] }],
                    generationConfig: { maxOutputTokens: 120000, temperature: 0 },
                }),
            }
        );

        const data = await resp.json();

        if (!resp.ok) {
            console.error("[callLLM][GEMINI] HTTP error:", resp.status, resp.statusText);
            console.error("[callLLM][GEMINI] Body:", JSON.stringify(data, null, 2));
            throw new Error(`Gemini API error: ${resp.status}`);
        }

        completionText = data.candidates?.[0]?.content?.parts?.[0]?.text ?? "";
        if (!completionText) {
            console.error("[callLLM][GEMINI] Empty completion, raw data:", JSON.stringify(data, null, 2));
        }
    } else if (vendor === "CLAUDE") {
        const resp = await fetch("https://api.anthropic.com/v1/messages", {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
                "x-api-key": selected.key,
                "anthropic-version": "2023-06-01",
            },
            body: JSON.stringify({
                model: selected.name,
                max_tokens: 64000,
                temperature: 0,
                messages: [
                    {
                        role: "user",
                        // 최근 스펙은 content를 배열로 받지만 string도 허용됨
                        content: prompt,
                    },
                ],
            }),
        });

        const data = await resp.json();

        if (!resp.ok) {
            console.error("[callLLM][CLAUDE] HTTP error:", resp.status, resp.statusText);
            console.error("[callLLM][CLAUDE] Body:", JSON.stringify(data, null, 2));
            throw new Error(`Claude API error: ${resp.status}`);
        }

        // messages API 응답 구조: { content: [{ type: "text", text: "..." }, ...] }
        completionText = data.content?.[0]?.text ?? "";
        if (!completionText) {
            console.error("[callLLM][CLAUDE] Empty completion, raw data:", JSON.stringify(data, null, 2));
        }
    } else if (vendor === "DEEPSEEK") {
        // deepseek-reasoner: max 65536, deepseek-chat: max 8192
        const deepseekMaxTokens = size === "FLAGSHIP" ? 65536 : 8192;
        const resp = await fetch("https://api.deepseek.com/chat/completions", {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
                Authorization: `Bearer ${selected.key}`,
            },
            body: JSON.stringify({
                model: selected.name,
                messages: [{ role: "user", content: prompt }],
                max_tokens: deepseekMaxTokens,
                temperature: 0,
                stream: false,
            }),
        });

        const data = await resp.json();

        if (!resp.ok) {
            console.error("[callLLM][DEEPSEEK] HTTP error:", resp.status, resp.statusText);
            console.error("[callLLM][DEEPSEEK] Body:", JSON.stringify(data, null, 2));
            throw new Error(`DeepSeek API error: ${resp.status}`);
        }

        completionText = data.choices?.[0]?.message?.content ?? "";
        if (!completionText) {
            console.error("[callLLM][DEEPSEEK] Empty completion, raw data:", JSON.stringify(data, null, 2));
        }
    }

    completionText = completionText ?? "";

    if (!completionText.trim()) {
        throw new Error(`[callLLM] LLM returned empty content for vendor=${vendor}, size=${size}`);
    }

    const stripped = stripCodeFences(completionText);
    console.log(`[callLLM] completion length (after strip): ${stripped.length}`);
    return stripped;
}

/**
 * ATLAS JSON -> Strategus R script
 */
export async function json2strategus(
    analysisSpecifications: string,
    opts: { vendor: Vendor; size: ModelSize }
): Promise<string> {
    const template = await readTextFile(TEMPLATE_PATH);
    const { vendor, size } = opts;

    const prompt = `<Instruction>
Refer to settings in <Analysis Specifications> and use the OHDSI Strategus package to write CreateStrategusAnalysisSpecification.R script. 
Refer to <Template> to write the script.
No name auto-correct: use EXACT names from <Template>/<Analysis Specifications>.
Output only the R script without any additional text.
Include detailed annotations within the script to help users understand how the settings are applied.
</Instruction>

<Analysis Specifications>
${analysisSpecifications}
</Analysis Specifications>

<Template>
${template}
</Template>`;

    const completionText = await callLLM(prompt, vendor, size);
    return completionText;
}

async function main() {
    const args = process.argv.slice(2);
    let inputPath: string | undefined;
    let outputPath: string | undefined;
    let vendor: Vendor | undefined;
    let size: ModelSize | undefined;
    let ruleBased = false;
    let outputJsonPath: string | undefined;

    for (let i = 0; i < args.length; i++) {
        const arg = args[i];
        if (arg === "--rule-based") {
            ruleBased = true;
            continue;
        }
        if (arg === "--vendor") {
            vendor = args[i + 1] as Vendor;
            i += 1;
            continue;
        }
        if (arg === "--size") {
            size = args[i + 1] as ModelSize;
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
            "Usage: <ts runner> json2strategus.ts <input.json|input.ts> [output.R] [--rule-based] [--vendor VENDOR] [--size SIZE] [--output-json <path>]"
        );
        process.exit(1);
    }

    const absInput = path.resolve(process.cwd(), inputPath);
    const { dto: rawDto, name: inferredName } = await loadJsonInput(absInput);
    const studyName = inferredName || "studyName";

    let script = "";

    if (ruleBased) {
        const merged = fillWithDefaults(defaultDTO, rawDto);
        merged.name = studyName;
        if (!merged.cohortDefinitions?.targetCohort?.id) {
            merged.cohortDefinitions = {
                targetCohort: { id: 1794126, name: "target1" },
                comparatorCohort: { id: 1794132, name: "comparator1" },
                outcomeCohort: [{ id: 1794131, name: "outcome1" }],
            };
        }
        if (!merged.negativeControlConceptSet?.id) {
            merged.negativeControlConceptSet = { id: 1888110, name: "negative" };
        }
        script = studyDtoToR(merged, {
            studyName,
            outputJsonPath: outputJsonPath,
        });
    } else {
        const chosenVendor = vendor ?? "OPENAI";
        const chosenSize = size ?? "FLAGSHIP";
        script = await json2strategus(JSON.stringify(rawDto, null, 2), {
            vendor: chosenVendor,
            size: chosenSize,
        });
    }

    if (outputPath) {
        await fs.writeFile(path.resolve(process.cwd(), outputPath), script, "utf8");
        console.log(`[OK] Wrote ${path.relative(process.cwd(), outputPath)}`);
        return;
    }

    if (ruleBased) {
        const outDir = path.resolve(process.cwd(), "generated_r", "rule_based");
        await fs.mkdir(outDir, { recursive: true });
        const outPath = path.join(
            outDir,
            `${studyName}_CreateStrategusAnalysisSpecification_rulebased.R`
        );
        await fs.writeFile(outPath, script, "utf8");
        console.log(`[OK] Wrote ${path.relative(process.cwd(), outPath)}`);
        return;
    }

    process.stdout.write(script);
}

if (import.meta.url === pathToFileURL(process.argv[1]).href) {
    void main();
}

/**
 * 디버깅용:
 * - 현재 R 스크립트와 실행 에러 로그를 함께 입력하면
 * - Template 구조를 최대한 유지하면서, 에러를 고친 R 스크립트 전체를 반환.
 * - 출력은 반드시 R 코드만 (마크다운, 설명 텍스트 X)
 */
export async function debugStrategusScript(
    args: {
        originalScript: string;
        errorLog: string;
        vendor: Vendor;
        size: ModelSize;
    }
): Promise<string> {
    const { originalScript, errorLog, vendor, size } = args;
    const template = await readTextFile(TEMPLATE_PATH);

    const prompt = `<Instruction>
You are debugging an R script that should follow the OHDSI Strategus CreateStrategusAnalysisSpecification.R template.

You are given:
- <Template>: the canonical structure and naming that the script must follow.
- <Original Script>: the current version of the R script that produced an error.
- <Error Log>: the error message and stack trace from running the script.

Your tasks:
1. Carefully identify the cause(s) of the error using <Error Log> and how <Original Script> deviates from <Template> or from valid Strategus usage.
2. Rewrite the ENTIRE script so that:
   - It follows <Template> structure and naming conventions.
   - It fixes the errors indicated in <Error Log>.
   - It preserves all correct and useful logic from <Original Script> when possible.
3. Apply minimal but sufficient changes to make the script executable and logically correct.

Constraints:
- No name auto-correct: use EXACT names from <Template>/<Original Script> unless you must change them to fix a clear error.
- Output ONLY the final R script (no Markdown fences, no natural language explanation).
- You MAY include R comments (# ...) inside the script to explain important changes or assumptions.
- DO NOT wrap the script in \`\`\` fences or add any extra text before or after the code.
</Instruction>

<Template>
${template}
</Template>

<Original Script>
${originalScript}
</Original Script>

<Error Log>
${errorLog}
</Error Log>`;

    const completionText = await callLLM(prompt, vendor, size);
    return completionText;
}
