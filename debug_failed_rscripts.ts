#!/usr/bin/env node
import fs from "node:fs/promises";
import path from "node:path";
import { debugStrategusScript } from "./json2strategus";

// json2strategus.ts 안에서 이미 dotenv.config() 호출하고 있음.

type Vendor = "OPENAI" | "CLAUDE" | "GEMINI" | "DEEPSEEK";
type ModelSize = "FLAGSHIP" | "LIGHT";
type RunType = "DEFAULT" | "PRIMARY";

interface CliArgs {
    vendor: Vendor;
    size: ModelSize;
    type: RunType;
}

/**
 * CLI 인자 파싱: --vendor=OPENAI --size=LIGHT --type=DEFAULT|PRIMARY
 */
function parseCliArgs(): CliArgs {
    const args = process.argv.slice(2);
    const kv: Record<string, string> = {};

    for (const arg of args) {
        const [key, value] = arg.split("=");
        if (key?.startsWith("--") && value) {
            kv[key.slice(2)] = value;
        }
    }

    const vendor = kv.vendor as Vendor | undefined;
    const size = kv.size as ModelSize | undefined;
    const type = kv.type as RunType | undefined;

    if (!vendor || !size || !type) {
        console.error(
            "Usage: debug_failed_rscripts --vendor=<OPENAI|CLAUDE|GEMINI|DEEPSEEK> --size=<FLAGSHIP|LIGHT> --type=<DEFAULT|PRIMARY>",
        );
        process.exit(1);
    }

    const vendorSet: Vendor[] = ["OPENAI", "CLAUDE", "GEMINI", "DEEPSEEK"];
    const sizeSet: ModelSize[] = ["FLAGSHIP", "LIGHT"];
    const typeSet: RunType[] = ["DEFAULT", "PRIMARY"];

    if (!vendorSet.includes(vendor) || !sizeSet.includes(size) || !typeSet.includes(type)) {
        console.error(
            `Invalid vendor/size/type. vendor=${vendor}, size=${size}, type=${type}`,
        );
        process.exit(1);
    }

    return { vendor, size, type };
}

/**
 * summary.txt에서 Failed Scripts 목록 파싱
 */
function parseFailedScripts(summaryText: string): string[] {
    const lines = summaryText.split(/\r?\n/);
    const failed: string[] = [];
    let inFailedSection = false;

    for (const raw of lines) {
        const line = raw.trim();

        if (line.startsWith("❌ Failed Scripts") || line.startsWith("Failed Scripts")) {
            inFailedSection = true;
            continue;
        }
        if (!inFailedSection) continue;

        if (!line || line.startsWith("---") || line.startsWith("Logs saved")) {
            if (failed.length > 0) break;
            continue;
        }

        const m = line.match(/-\s*(.+)$/);
        if (m && m[1]) failed.push(m[1].trim());
    }

    return failed;
}

// ===== 429 / 네트워크 에러 대비 helper =====
const MAX_RETRIES = 5;
const BASE_DELAY_MS = 2000; // 1st retry 2s
const JITTER_MS = 500;

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
    if (msg.includes("ecconnreset") || msg.includes("etimedout") || msg.includes("enotfound")) return true;

    return false;
}

async function safeDebugStrategusScript(params: {
    originalScript: string;
    errorLog: string;
    vendor: Vendor;
    size: ModelSize;
    name: string;
}): Promise<string> {
    const { originalScript, errorLog, vendor, size, name } = params;
    let lastError: any = null;

    for (let attempt = 0; attempt < MAX_RETRIES; attempt++) {
        if (attempt > 0) {
            const delay =
                BASE_DELAY_MS * Math.pow(2, attempt - 1) + Math.random() * JITTER_MS;
            console.warn(
                `[WARN] ${name}: retrying debugStrategusScript (attempt ${
                    attempt + 1
                }/${MAX_RETRIES}) after ${Math.round(delay)} ms ...`,
            );
            await sleep(delay);
        }

        try {
            const fixedScript = await debugStrategusScript({
                originalScript,
                errorLog,
                vendor,
                size,
            });
            return fixedScript;
        } catch (err: any) {
            lastError = err;
            const retriable = isRateLimitOrRetriableError(err);
            const isLast = attempt === MAX_RETRIES - 1;

            if (!retriable || isLast) {
                console.error(
                    `[ERROR] ${name}: debugStrategusScript failed (attempt ${
                        attempt + 1
                    }/${MAX_RETRIES}) → giving up.`,
                );
                throw err;
            } else {
                console.warn(
                    `[WARN] ${name}: debugStrategusScript failed (attempt ${
                        attempt + 1
                    }/${MAX_RETRIES}) – ${String(err?.message ?? err)}`,
                );
            }
        }
    }

    throw lastError ?? new Error("debugStrategusScript failed after retries");
}

async function main() {
    const { vendor, size, type } = parseCliArgs();

    const baseDir = process.cwd();

    const vendorLower = vendor.toLowerCase();
    const sizeLower = size.toLowerCase();
    const typeLower = type.toLowerCase();

    // run_rscripts.sh 기준 경로:
    //   input R scripts: public/firstScripts/{type}/{vendor_lower}_{size_lower}
    //   logs + summary : public/ResultFirstScripts/{type}/{vendor_lower}_{size_lower}
    // 여기서는 실패한 애들만 디버깅해서:
    //   debug scripts:  public/DebugScripts/{type}/{vendor_lower}_{size_lower}
    const scriptDir = path.join(
        baseDir,
        "public",
        "firstScripts",
        typeLower,
        `${vendorLower}_${sizeLower}`,
    );
    const resultRoot = path.join(
        baseDir,
        "public",
        "ResultFirstScripts",
        typeLower,
        `${vendorLower}_${sizeLower}`,
    );
    const logDir = path.join(resultRoot, "logs");
    const summaryPath = path.join(resultRoot, "summary.txt");
    const debugDir = path.join(
        baseDir,
        "public",
        "DebugScripts",
        typeLower,
        `${vendorLower}_${sizeLower}`,
    );

    await fs.mkdir(debugDir, { recursive: true });

    console.log(`==> Vendor : ${vendor}`);
    console.log(`==> Size   : ${size}`);
    console.log(`==> Type   : ${type}`);
    console.log(`==> Script dir : ${scriptDir}`);
    console.log(`==> Log dir    : ${logDir}`);
    console.log(`==> Summary    : ${summaryPath}`);
    console.log(`==> Debug dir  : ${debugDir}`);

    // summary.txt 읽기
    let summary: string;
    try {
        summary = await fs.readFile(summaryPath, "utf8");
    } catch (err) {
        console.error(`Failed to read summary.txt at: ${summaryPath}`);
        console.error(err);
        process.exit(1);
    }

    // Failed Scripts 파싱
    const failedScripts = parseFailedScripts(summary);
    if (failedScripts.length === 0) {
        console.log("No failed scripts found in summary.txt. Nothing to debug.");
        process.exit(0);
    }

    console.log("==> Failed scripts found:");
    for (const name of failedScripts) console.log(`   - ${name}`);

    // 디버깅 실행
    for (const name of failedScripts) {
        const scriptPath = path.join(scriptDir, `${name}.R`);
        const logPath = path.join(logDir, `${name}.log`);
        const outputPath = path.join(debugDir, `${name}.R`);

        console.log(`\n=== Debugging script: ${name} ===`);
        console.log(`   Script: ${scriptPath}`);
        console.log(`   Log   : ${logPath}`);

        let originalScript = "";
        let errorLog = "";

        try {
            originalScript = await fs.readFile(scriptPath, "utf8");
        } catch (err) {
            console.error(`   !! Cannot read script file: ${scriptPath}`);
            console.error(err);
            continue;
        }

        try {
            errorLog = await fs.readFile(logPath, "utf8");
        } catch {
            console.warn(`   !! Cannot read log file: ${logPath} (using empty error log)`);
            errorLog = "";
        }

        try {
            const fixedScript = await safeDebugStrategusScript({
                originalScript,
                errorLog,
                vendor,
                size,
                name,
            });

            await fs.writeFile(outputPath, fixedScript, "utf8");
            console.log(`   ✅ Debug script saved to: ${outputPath}`);
        } catch (err) {
            console.error(`   ❌ Failed to debug script: ${name}`);
            console.error(err);
        }
    }

    console.log("\nAll failed scripts processed.");
}

main().catch((err) => {
    console.error("Unexpected error in debug_failed_rscripts:");
    console.error(err);
    process.exit(1);
});
