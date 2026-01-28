StrategusStudyRepoTemplate
=================

## Pipeline Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              goldStandard                                   │
│  (StudyDTO - 입력 데이터)                                                    │
│                                                                             │
│  public/goldStandard/                                                       │
│  ├── default/    ← 기본 스키마 (15개 연구)                                    │
│  ├── primary/    ← 대안 스키마                                               │
│  ├── method/     ← 메소드 기반 데이터                                         │
│  └── pdf/        ← PDF 기반 데이터                                           │
└────────────────────────────────┬────────────────────────────────────────────┘
                                 │
                                 ▼
                      ┌─────────────────────┐
                      │   json2strategus.ts │
                      │   (메인 변환 CLI)    │
                      └──────────┬──────────┘
                                 │
           ┌─────────────────────┴─────────────────────┐
           │                                           │
           ▼                                           ▼
┌──────────────────────┐                  ┌───────────────────────┐
│  --rule-based        │                  │  --vendor X --size Y  │
│  (결정론적 변환)       │                  │  (LLM 파이프라인)       │
│                      │                  │                       │
│  → studyDtoToR.ts    │                  │  → callLLM()          │
│  → generated_r/      │                  │  → getRScripts.ts     │
│     rule_based/      │                  │                       │
└──────────────────────┘                  └───────────┬───────────┘
                                                      │
                                                      ▼
                                   ┌──────────────────────────────────┐
                                   │          firstScripts            │
                                   │  (LLM 생성 R 스크립트 출력)         │
                                   │                                  │
                                   │  public/firstScripts/            │
                                   │  ├── default/                    │
                                   │  │   ├── openai_flagship/        │
                                   │  │   ├── openai_light/           │
                                   │  │   ├── gemini_flagship/        │
                                   │  │   ├── gemini_light/           │
                                   │  │   ├── claude_flagship/        │
                                   │  │   ├── claude_light/           │
                                   │  │   ├── deepseek_flagship/      │
                                   │  │   └── deepseek_light/         │
                                   │  └── primary/                    │
                                   │      └── (동일 구조)              │
                                   └──────────────────────────────────┘
```

---

## LLM Batch Processing (getRScripts.ts)

LLM을 사용하여 여러 연구를 일괄 처리하는 명령어입니다.

### goldStandard (기존 15개 연구)
```bash
node --experimental-strip-types getRScripts.ts \
  --vendor=OPENAI|GEMINI|DEEPSEEK|CLAUDE \
  --size=FLAGSHIP|LIGHT \
  --type=DEFAULT|PRIMARY
```
출력: `public/firstScripts/{type}/{vendor}_{size}/`

### goldStandardTest (테스트용 5개 연구)
```bash
node --experimental-strip-types getRScripts.ts \
  --vendor=OPENAI|GEMINI|DEEPSEEK|CLAUDE \
  --size=FLAGSHIP|LIGHT \
  --type=DEFAULT|PRIMARY \
  --source=GOLDSTANDARDTEST
```
출력: `public/firstScriptsTest/{type}/{vendor}_{size}/`

### 예시
```bash
# goldStandardTest/default를 처리
node --experimental-strip-types getRScripts.ts --vendor=OPENAI --size=FLAGSHIP --type=DEFAULT --source=GOLDSTANDARDTEST
node --experimental-strip-types getRScripts.ts --vendor=OPENAI --size=LIGHT --type=DEFAULT --source=GOLDSTANDARDTEST
node --experimental-strip-types getRScripts.ts --vendor=CLAUDE --size=FLAGSHIP --type=DEFAULT --source=GOLDSTANDARDTEST
node --experimental-strip-types getRScripts.ts --vendor=CLAUDE --size=LIGHT --type=DEFAULT --source=GOLDSTANDARDTEST
node --experimental-strip-types getRScripts.ts --vendor=GEMINI --size=FLAGSHIP --type=DEFAULT --source=GOLDSTANDARDTEST
node --experimental-strip-types getRScripts.ts --vendor=GEMINI --size=LIGHT --type=DEFAULT --source=GOLDSTANDARDTEST
node --experimental-strip-types getRScripts.ts --vendor=DEEPSEEK --size=FLAGSHIP --type=DEFAULT --source=GOLDSTANDARDTEST
node --experimental-strip-types getRScripts.ts --vendor=DEEPSEEK --size=LIGHT --type=DEFAULT --source=GOLDSTANDARDTEST

#goldstandardTest/primary를 처리
node --experimental-strip-types getRScripts.ts --vendor=OPENAI --size=FLAGSHIP --type=PRIMARY --source=GOLDSTANDARDTEST
node --experimental-strip-types getRScripts.ts --vendor=OPENAI --size=LIGHT --type=PRIMARY --source=GOLDSTANDARDTEST
node --experimental-strip-types getRScripts.ts --vendor=CLAUDE --size=FLAGSHIP --type=PRIMARY --source=GOLDSTANDARDTEST
node --experimental-strip-types getRScripts.ts --vendor=CLAUDE --size=LIGHT --type=PRIMARY --source=GOLDSTANDARDTEST
node --experimental-strip-types getRScripts.ts --vendor=GEMINI --size=FLAGSHIP --type=PRIMARY --source=GOLDSTANDARDTEST
node --experimental-strip-types getRScripts.ts --vendor=GEMINI --size=LIGHT --type=PRIMARY --source=GOLDSTANDARDTEST
node --experimental-strip-types getRScripts.ts --vendor=DEEPSEEK --size=FLAGSHIP --type=PRIMARY --source=GOLDSTANDARDTEST
node --experimental-strip-types getRScripts.ts --vendor=DEEPSEEK --size=LIGHT --type=PRIMARY --source=GOLDSTANDARDTEST


# goldStandard/primary를 OpenAI Flagship으로 처리
node --experimental-strip-types getRScripts.ts --vendor=OPENAI --size=FLAGSHIP --type=PRIMARY
```

| 옵션 | 값 | 설명 |
|------|-----|------|
| `--vendor` | OPENAI, GEMINI, DEEPSEEK, CLAUDE | LLM 벤더 |
| `--size` | FLAGSHIP, LIGHT | 모델 크기 (성능 vs 비용) |
| `--type` | DEFAULT, PRIMARY | 스키마 타입 |
| `--source` | GOLDSTANDARD, GOLDSTANDARDTEST | 입력 데이터 소스 (기본값: GOLDSTANDARD) |

---

## Debug Failed Scripts (debug_failed_rscripts.ts)

R 스크립트 실행 중 실패한 스크립트를 LLM으로 디버깅합니다.

### 사용법
```bash
node --experimental-strip-types debug_failed_rscripts.ts \
  --vendor=OPENAI|GEMINI|DEEPSEEK|CLAUDE \
  --size=FLAGSHIP|LIGHT \
  --type=DEFAULT|PRIMARY \
  [--source=GOLDSTANDARD|GOLDSTANDARDTEST]
```

### 경로 구조
| source | 입력 (firstScripts) | 로그 (ResultFirstScripts) | 출력 (DebugScripts) |
|--------|---------------------|---------------------------|---------------------|
| GOLDSTANDARD | `public/firstScripts/{type}/{vendor}_{size}/` | `public/ResultFirstScripts/{type}/{vendor}_{size}/` | `public/DebugScripts/{type}/{vendor}_{size}/` |
| GOLDSTANDARDTEST | `public/firstScriptsTest/{type}/{vendor}_{size}/` | `public/ResultFirstScriptsTest/{type}/{vendor}_{size}/` | `public/DebugScriptsTest/{type}/{vendor}_{size}/` |

### 예시
```bash
# goldStandard 실패 스크립트 디버깅
node --experimental-strip-types debug_failed_rscripts.ts --vendor=CLAUDE --size=LIGHT --type=DEFAULT

# goldStandardTest 실패 스크립트 디버깅
node --experimental-strip-types debug_failed_rscripts.ts --vendor=CLAUDE --size=LIGHT --type=DEFAULT --source=GOLDSTANDARDTEST
```

---

## Run R Scripts (run_rscripts.sh)

생성된 firstScripts R 스크립트를 실행하여 에러 여부를 확인합니다.

### 사용법
```bash
./run_rscripts.sh \
  --vendor=OPENAI|GEMINI|DEEPSEEK|CLAUDE \
  --size=FLAGSHIP|LIGHT \
  --type=DEFAULT|PRIMARY \
  [--source=GOLDSTANDARD|GOLDSTANDARDTEST] \
  [--runner=R|Rscript]
```

### 경로 구조
| source | 입력 (firstScripts) | 결과 (ResultFirstScripts) |
|--------|---------------------|---------------------------|
| GOLDSTANDARD | `public/firstScripts/{type}/{vendor}_{size}/` | `public/ResultFirstScripts/{type}/{vendor}_{size}/` |
| GOLDSTANDARDTEST | `public/firstScriptsTest/{type}/{vendor}_{size}/` | `public/ResultFirstScriptsTest/{type}/{vendor}_{size}/` |

### 예시
```bash
# goldStandard firstScripts 실행
./run_rscripts.sh --vendor=CLAUDE --size=LIGHT --type=DEFAULT

# goldStandardTest firstScripts 실행
./run_rscripts.sh --vendor=OPENAI --size=FLAGSHIP --type=DEFAULT --source=GOLDSTANDARDTEST
./run_rscripts.sh --vendor=OPENAI --size=LIGHT --type=DEFAULT --source=GOLDSTANDARDTEST
./run_rscripts.sh --vendor=CLAUDE --size=FLAGSHIP --type=DEFAULT --source=GOLDSTANDARDTEST
./run_rscripts.sh --vendor=CLAUDE --size=LIGHT --type=DEFAULT --source=GOLDSTANDARDTEST
./run_rscripts.sh --vendor=GEMINI --size=FLAGSHIP --type=DEFAULT --source=GOLDSTANDARDTEST
./run_rscripts.sh --vendor=GEMINI --size=LIGHT --type=DEFAULT --source=GOLDSTANDARDTEST
./run_rscripts.sh --vendor=DEEPSEEK --size=FLAGSHIP --type=DEFAULT --source=GOLDSTANDARDTEST
./run_rscripts.sh --vendor=DEEPSEEK --size=LIGHT --type=DEFAULT --source=GOLDSTANDARDTEST

```

---

## Run Debug Scripts (run_debug_rscripts.sh)

디버깅된 DebugScripts R 스크립트를 실행하여 수정 결과를 확인합니다.

### 사용법
```bash
./run_debug_rscripts.sh \
  --vendor=OPENAI|GEMINI|DEEPSEEK|CLAUDE \
  --size=FLAGSHIP|LIGHT \
  --type=DEFAULT|PRIMARY \
  [--source=GOLDSTANDARD|GOLDSTANDARDTEST] \
  [--runner=R|Rscript]
```

### 경로 구조
| source | 입력 (DebugScripts) | 결과 (ResultDebugScripts) |
|--------|---------------------|---------------------------|
| GOLDSTANDARD | `public/DebugScripts/{type}/{vendor}_{size}/` | `public/ResultDebugScripts/{type}/{vendor}_{size}/` |
| GOLDSTANDARDTEST | `public/DebugScriptsTest/{type}/{vendor}_{size}/` | `public/ResultDebugScriptsTest/{type}/{vendor}_{size}/` |

### 예시
```bash
# goldStandard DebugScripts 실행
./run_debug_rscripts.sh --vendor=CLAUDE --size=LIGHT --type=DEFAULT

# goldStandardTest DebugScripts 실행
./run_debug_rscripts.sh --vendor=CLAUDE --size=LIGHT --type=DEFAULT --source=GOLDSTANDARDTEST
```

---

## Rule-Based Pipeline (StudyDTO -> R -> analysisSpecification.json)

Use this when you want deterministic, non-LLM generation from the gold standard StudyDTO JSON exports.

1) Generate R from a gold standard JSON export:
```
node --experimental-strip-types json2strategus.ts public/goldStandard/default/AntiVEGFKidney.ts --rule-based --output-json generated_json/analysisSpec/antivegfkidneyAnalysisSpecification.json
```
This writes:
- `generated_r/rule_based/antivegfkidney_CreateStrategusAnalysisSpecification_rulebased.R`

2) Run the generated R script to produce the JSON:
```
RENV_CONFIG_AUTOLOADER_ENABLED=FALSE \
R_LIBS_USER=renv/library/macos/R-4.5/aarch64-apple-darwin24.4.0 \
Rscript generated_r/rule_based/antivegfkidney_CreateStrategusAnalysisSpecification_rulebased.R
```
This writes:
- `generated_json/analysisSpec/antivegfkidneyAnalysisSpecification.json`

Notes:
- Omit `--output-json` to write to `inst/<studyName>/<studyName>AnalysisSpecification.json`.
- The R script fetches cohort definitions and negative control concepts from `https://atlas-demo.ohdsi.org/WebAPI`, so network access is required.

See the **[Using This Template.md](template_docs/UsingThisTemplate.md)** for more information on how to use this template.

----

An OHDSI study repository is expected to have a README.md file where the header conforms to a standard. A template README file is provided here:

**[README file template](template_docs/templateREADME.md)**

When initiating a repository, please copy this file, rename it to 'README.md', and fill in the fields as appropriate.

The information in the repository README file will be used to automatically update the [list of OHDSI research studies](https://data.ohdsi.org/OhdsiStudies/), so it is important to fill in the template accurately, and keep it up-to-date.

## Elements in the README template

| Element | Description |
| ------- | ----------- |
| [Study title]      | A meaningful title of the research project.            
| Study status badge | A badge indicating the study status. See [below](#study-status) for valid options. |
| Analytics use case | One or more analytics use cases included in the study (in a comma-separated list). See [below](#analytics-use-cases) for valid options. |
| Study type | The type of study. See [below](#study-types) for valid options. |
| Tags | Zero, one, or more additional keywords that can be used to filter the list of studies. The list of tags is not restricted, but be conservative in making up new tags. For example: `EHDEN` to identify studies that are part of the [EHDEN project](https://www.ehden.eu/). |
| Study lead | The name of the study lead.|
| Study lead forums tag | The OHDSI forums tag of the study lead, which can be used to contact the lead. It is recommended to make this a hyperlink to lead's forums profile |
| Study start date | When did work on the study commence? This date typically indicates when development of the protocol was initiated. Format: [Month] [Day], [Year] (e.g. May 1, 2019)|
| Study end date | When was the study completed? This typically indicates when the analyses were completed and the results have been collected. Do not enter future (planned) dates here. Format: [Month] [Day], [Year] (e.g. May 1, 2019)| 
| Protocol | A hyperlink to the protocol. The protocol is expected to be a document in the study repository itself. | 
| Publications | Zero, one or more hyperlinks to papers produced as part of the study (comma-separated). | 
| Results explorer | A hyperlink to a web app (e.g. a Shiny app) where the results of the study can be explored. |

### Study Status

Choose one of the following options:

| Badge             | Description                          |
| ----------------- | ------------------------------------ |
| <img src="https://img.shields.io/badge/Study%20Status-Repo%20Created-lightgray.svg" alt="Study Status: Repo Created"> | The study repository has just been created. Work has not yet commenced. | 
| <img src="https://img.shields.io/badge/Study%20Status-Started-blue.svg" alt="Study Status: Started"> | A first commit was made (to something else than the README file). Work has commenced. |
| <img src="https://img.shields.io/badge/Study%20Status-Design%20Finalized-brightgreen.svg" alt="Study Status: Design Finalized"> | The protocol and study code have been finalized. | 
| <img src="https://img.shields.io/badge/Study%20Status-Results%20Available-yellow.svg" alt="Study Status: Results Available"> | The study results are publicly available, for example in a paper or results explorer app. | 
| <img src="https://img.shields.io/badge/Study%20Status-Complete-orange.svg" alt="Study Status: Complete"> | The study is complete, no further dissemination planned. | 
| <img src="https://img.shields.io/badge/Study%20Status-Suspended-red.svg" alt="Study Status: Suspended"> | The study has been suspended, and may or may not be continued at a later point in time. | 

Copy the relevant markdown code from [this page](badgesMarkdownCode.md), and paste it in your README file, just below the study title.

### Analytics Use Cases

Choose one or more options from: 

- `Characterization`
- `Population-Level Estimation`, or
- `Patient-Level Prediction` 

See [the Data Analytics Use Cases chapter](https://ohdsi.github.io/TheBookOfOhdsi/DataAnalyticsUseCases.html) for more details.

### Study types

Can be either:

- `Methods Research` if the study explores a methodological question, for example an evaluation of various propensity score approaches. 
- `Clinical Application` if the study aims to answer a clinically relevant question, for example 'Does drug A cause outcome B?'.
