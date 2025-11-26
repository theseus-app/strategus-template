export const TEXTRapidCycleJanssen = `
Study Period 1: Individuals were included in these COVID-19 vaccine exposure cohorts if they had at least 365 days of observation before and at least one day of observation after this first COVID-19 vaccination starting on or after January 1st, 2021.

TAR 1-5: relative risks of outcomes of interest were assessed during four overlapping at-risk periods following vaccination (1–14, 1–28, 1–42 and 1–90 days) except for anaphylaxis, which used an at-risk window starting on the vaccination day (0–2 days).

PS settings 1: We apply variable ratio matching on the propensity score

Outcome Model: The comparative cohort analysis employed a Cox proportional hazards model to estimate the hazard ratio for a specified outcome.
`

export const JSONRapidCycleJanssen =
{
    getDbCohortMethodDataArgs: {
        studyPeriods: [
            {
                studyStartDate: 20210101,
                studyEndDate: null
            }
        ],
        restrictToCommonPeriod: true,
        firstExposureOnly: true,
        washoutPeriod: 365,
        removeDuplicateSubjects: "remove all",
        maxCohortSize: 0, //default
    },
    createStudyPopArgs: {
        censorAtNewRiskWindow: false, //default로 설정
        removeSubjectsWithPriorOutcome: true,
        priorOutcomeLookBack: 99999,
        timeAtRisks: [
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 14,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 28,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 42,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 90,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 0,
                startAnchor: "cohort start",
                riskWindowEnd: 2,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            }
        ]
    },
    propensityScoreAdjustment: {
        psSettings: [
            {
                matchOnPsArgs: {
                    maxRatio: 100,
                    caliper: 0.2, //default로 설정
                    caliperScale: "standardized logit"
                },
                stratifyByPsArgs: null,
            }
        ],
        createPsArgs: { //default
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
    fitOutcomeModelArgs: { //default except modelType
        modelType: "cox",
        stratified: true,
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
}