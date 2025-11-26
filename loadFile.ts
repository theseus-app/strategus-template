import fs from "node:fs/promises";
import path from "node:path";
import { pathToFileURL } from "node:url";

async function fileExists(abs: string) {
    try {
        await fs.access(abs);
        return true;
    } catch {
        return false;
    }
}

export type ModulePair = { name: string; goldJson: any; studyText?: string; pdfPath?: string };

export async function loadFile(type: string = "DEFAULT"): Promise<ModulePair[]> {

    // 타입별 폴더 매핑
    const folderMap: Record<string, string> = {
        DEFAULT: "default",
        PRIMARY: "primary",
        METHOD: "method",
        PDF: "pdf",
    };

    const upperType = type?.toUpperCase?.() ?? "DEFAULT";
    const folderName = folderMap[upperType] ?? folderMap.DEFAULT;
    const isPdfType = upperType === "PDF";

    const GOLD_DIR = path.resolve(process.cwd(), "public", "goldStandard", folderName);

    const entries = await fs.readdir(GOLD_DIR, { withFileTypes: true });

    // 파일 확장자: ts / js 지원 (필요시 추가)
    const candidates = entries
        .filter((e) => e.isFile())
        .map((e) => e.name)
        .filter((n) => /\.(ts|js|mjs|cjs)$/i.test(n));

    const pairs: ModulePair[] = [];

    for (const file of candidates) {
        const abs = path.join(GOLD_DIR, file);
        const url = pathToFileURL(abs).href;

        // tsx 실행기(ESM) 하에서 dynamic import
        const mod = await import(url);

        // export 키에서 TEXT*, JSON* 찾기
        const textKeys = Object.keys(mod).filter((k) => /^TEXT/i.test(k));
        const jsonKeys = Object.keys(mod).filter((k) => /^JSON/i.test(k));

        if (jsonKeys.length !== 1) {
            console.warn(`[WARN] Skip ${file}: expected exactly 1 JSON* export (got JSON=${jsonKeys.length}).`);
            continue;
        }

        if (!isPdfType && textKeys.length !== 1) {
            console.warn(`[WARN] Skip ${file}: expected exactly 1 TEXT* and 1 JSON* export (got TEXT=${textKeys.length}, JSON=${jsonKeys.length}).`);
            continue;
        }

        if (isPdfType && textKeys.length > 1) {
            console.warn(`[WARN] Skip ${file}: expected 0 or 1 TEXT* export for PDF type (got TEXT=${textKeys.length}).`);
            continue;
        }

        const textKey = textKeys.length === 1 ? textKeys[0] : null;
        const jsonKey = jsonKeys[0];
        const studyText = textKey ? String(mod[textKey]) : undefined;
        const goldJson = mod[jsonKey];

        const nameFromExport = (textKey ?? "").replace(/^TEXT/i, "") || jsonKey.replace(/^JSON/i, "");
        const baseName = path.basename(file, path.extname(file));
        const caseName = nameFromExport || baseName;

        const pdfPath = isPdfType
            ? path.resolve(GOLD_DIR, "papers", `${baseName}.pdf`)
            : undefined;

        if (isPdfType && !(await fileExists(pdfPath as string))) {
            console.warn(`[WARN] Skip ${file}: PDF not found at ${pdfPath}`);
            continue;
        }

        pairs.push({ name: caseName, goldJson, studyText, pdfPath });
    }

    if (!pairs.length) {
        console.warn(`[WARN] No module pairs found in ${GOLD_DIR}. For text types export one TEXT* and one JSON* const; for PDF types ensure the PDF exists under papers/ and one JSON* const is exported.`);
    }
    return pairs;
}
