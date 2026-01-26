# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

OHDSI network study framework that converts JSON-based study specifications (StudyDTO) into executable R scripts for Strategus/HADES medical research analysis. Supports two pipelines:

- **Rule-Based Pipeline**: Deterministic conversion from StudyDTO → R script → analysisSpecification.json
- **LLM-Powered Pipeline**: AI-assisted generation using OpenAI, Claude, Gemini, or DeepSeek

## Common Commands

### Run TypeScript files (requires Node.js with experimental flag)
```bash
node --experimental-strip-types <file.ts> [args]
```

### Rule-Based Pipeline (deterministic, no LLM)
```bash
# Generate R script from StudyDTO
node --experimental-strip-types json2strategus.ts \
  public/goldStandard/default/AntiVEGFKidney.ts \
  --rule-based \
  --output-json generated_json/analysisSpec/antivegfkidneyAnalysisSpecification.json

# Execute generated R script (requires renv setup)
RENV_CONFIG_AUTOLOADER_ENABLED=FALSE \
R_LIBS_USER=renv/library/macos/R-4.5/aarch64-apple-darwin24.4.0 \
Rscript generated_r/rule_based/<studyname>_CreateStrategusAnalysisSpecification_rulebased.R
```

### LLM-Powered Pipeline
```bash
# Single study with specific vendor/size
node --experimental-strip-types json2strategus.ts \
  public/goldStandard/default/AntiVEGFKidney.ts \
  --vendor CLAUDE \
  --size LIGHT

# Batch processing all studies
node --experimental-strip-types getRScripts.ts \
  --vendor=OPENAI|GEMINI|DEEPSEEK|CLAUDE \
  --size=FLAGSHIP|LIGHT \
  --type=DEFAULT|PRIMARY
```

### Debug Failed Scripts
```bash
node --experimental-strip-types debug_failed_rscripts.ts \
  --vendor=CLAUDE --size=LIGHT --type=PRIMARY
```

## Architecture

```
StudyDTO (TypeScript/JSON)
    │
    ├──[--rule-based]──→ studyDtoToR.ts ──→ R script
    │
    └──[--vendor X]───→ callLLM() ──→ R script
                              │
                              ▼
                    Rscript execution
                              │
                              ▼
                analysisSpecification.json
```

### Key Files
- **studyDto.ts**: Central data model (`StudyDTO` interface) with `fillWithDefaults()`
- **json2strategus.ts**: Main CLI entry point, LLM orchestration, `callLLM()` multi-vendor abstraction
- **studyDtoToR.ts**: Rule-based compiler with R code generation helpers (`rValue()`, `rVector()`, `indent()`)
- **loadFile.ts**: Loads gold standard studies, expects `JSON*` and `TEXT*` exports
- **getRScripts.ts**: Batch processing for multiple studies

### Directory Structure
- `public/goldStandard/default/` - 15 reference study definitions (TypeScript exports)
- `public/goldStandard/primary/` - Alternative schema study definitions
- `public/templates/` - R code templates for generation
- `generated_r/rule_based/` - Output R scripts from rule-based pipeline
- `generated_json/analysisSpec/` - Output analysis specification JSONs
- `inst/<studyName>/` - Per-study artifacts (cohort definitions, analysis specs)
- `log/` - Execution logs by vendor/model

## Environment Variables

Required in `.env` for LLM pipeline:
- `OPENAI_API_KEY` - OpenAI (gpt-5, gpt-5-mini)
- `CLAUDE_API_KEY` - Anthropic (claude-sonnet-4-5, claude-haiku-4-5)
- `GOOGLE_API_KEY` - Google (gemini-2.5-pro, gemini-2.5-flash)
- `DEEPSEEK_API_KEY` - DeepSeek (deepseek-reasoner, deepseek-chat)

## Conventions

- **Cohort IDs**: Target=1, Comparator=2, Outcome=3, Negative Controls=101+
- **Slugification**: Study names → lowercase-hyphenated (e.g., `AntiVEGFKidney` → `antivegfkidney`)
- **R indentation**: 2 spaces
- **Gold standard exports**: `JSONStudyName` for structured data, `TEXTStudyName` for descriptions
- **Model sizes**: FLAGSHIP (powerful/slower) vs LIGHT (fast/cheaper)

## R Dependencies

The R environment uses renv for reproducibility. Key packages: Strategus, CohortMethod, CohortGenerator, CohortDiagnostics from OHDSI/HADES ecosystem.

R scripts fetch cohort definitions from `https://atlas-demo.ohdsi.org/WebAPI` at runtime.
