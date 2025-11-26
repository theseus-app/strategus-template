export const TEXTCEEAMOS =
    `
TAR: Our analysis considered the time-to-first event and was followed up to the earliest date among last date of assigned treatment, date of last observation in the database, date of occurrence of the endpoint, and date of censoring (as-treated [AT] approach).

PS Settings: The study populations were matched using variable-ratio PS matching with a maximum ratio of 10 (caliper = 0.2).

Outcome Model: Cox proportional hazard models were fitted to estimate the hazard ratios (HRs) and 95% CIs according to exposure using the CohortMethod21 R package for each data source.
`

export const JSONCEEAMOS = {
    maxCohortSize: 0, //default 추가
    createStudyPopArgs: {
        restrictToCommonPeriod: false, //default 추가
        firstExposureOnly: false, //default 추가
        washoutPeriod: 365,
        removeDuplicateSubjects: "remove all",
        censorAtNewRiskWindow: false, //default 추가
        removeSubjectsWithPriorOutcome: true,
        priorOutcomeLookBack: 365,
        timeAtRisks: {
            riskWindowStart: 1,
            startAnchor: "cohort start",
            riskWindowEnd: 0,
            endAnchor: "cohort end",
            minDaysAtRisk: 1 //default로 추가
        },
    },
    propensityScoreAdjustment: {
        psSettings: {
            matchOnPsArgs: {
                maxRatio: 10,
                caliper: 0.2,
                caliperScale: "standardized logit",
            },
            stratifyByPsArgs: null
        },
        //createPsArgs 전체 다 default로 추
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
    fitOutcomeModelArgs: { //modelType제외 default 추가
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
};
