#!/usr/bin/env node
import fs from "node:fs/promises";
import path from "node:path";
import { debugStrategusScript } from "./json2strategus";

// json2strategus.ts 안에서 이미 dotenv.config() 호출하고 있음.

type Vendor = "OPENAI" | "CLAUDE" | "GEMINI" | "DEEPSEEK";
type ModelSize = "FLAGSHIP" | "LIGHT";

interface CliArgs {
    vendor: Vendor;
    size: ModelSize;
}

/**
 * CLI 인자 파싱: --vendor=OPENAI --size=LIGHT
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

    if (!vendor || !size) {
        console.error("Usage: debug_failed_rscripts --vendor=<OPENAI|CLAUDE|GEMINI|DEEPSEEK> --size=<FLAGSHIP|LIGHT>");
        process.exit(1);
    }

    return { vendor, size };
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

async function main() {
    const { vendor, size } = parseCliArgs();

    const baseDir = process.cwd();
    const scriptDir = path.join(baseDir, `RScripts_${vendor}_${size}`);
    const logDir = path.join(baseDir, "log", `RScripts_${vendor}_${size}`);
    const summaryPath = path.join(logDir, "summary.txt");

    // NEW: 디버그 결과 저장 폴더
    const debugDir = path.join(baseDir, `DEBUG_RScripts_${vendor}_${size}`);
    await fs.mkdir(debugDir, { recursive: true });

    console.log(`==> Vendor: ${vendor}, Size: ${size}`);
    console.log(`==> Script dir: ${scriptDir}`);
    console.log(`==> Log dir   : ${logDir}`);
    console.log(`==> Debug dir : ${debugDir}`);

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
            const fixedScript = await debugStrategusScript({
                originalScript,
                errorLog,
                vendor,
                size,
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
