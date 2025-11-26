export const TEXTCORAZON = `
Study Period 1-2: between 1 January 2010 (2012 for Longitudinal Patients Database France) and 31 December 2019.

TAR 1: Patients were followed from the index date until the occurrence of the study outcome, treatment discontinuation (allowing for 90-day gaps between consecutive prescriptions, with the date of treatment discontinuation being the end date of the last prescription [the “on-treatment” approach]), switching from the index medication to another oral anticoagulant (apixaban, dabigatran, edoxaban, rivaroxaban, or warfarin), death, or the end of the study period (31 December 2019), whichever came first.
TAR 2: We did additional sensitivity analyses in which the time at risk was not censored if a patient discontinued the index medication therapy or switched to another anticoagulant (analogue to the “intention-to-treat” approach).

PS Settings 1: Patients were stratified into 5 strata based on propensity score to estimate the average treatment effect.
PS Settings 2: We also repeated our analyses using propensity score matching at a variable-matching ratio as sensitivity analyses to estimate the average treatment effect on treated patients.

Outcome Model: Cox proportional hazards regression conditioned on the propensity score strata was applied to estimate the hazard ratio (HR) of the risk for outcomes`

export const JSONCORAZON = {
    getDbCohortMethodDataArgs: {
        studyPeriods: [
            {
                studyStartDate: "20100101",
                studyEndDate: "20191231",
            },
            {
                studyStartDate: "20120101",
                studyEndDate: "20191231",
            },
        ],
        restrictToCommonPeriod: false,
        firstExposureOnly: false,
        washoutPeriod: 0,
        removeDuplicateSubjects: "keep all",
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
                riskWindowEnd: 0,
                endAnchor: "cohort end",
                minDaysAtRisk: 1 //default 추가
            },
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 99999,
                endAnchor: "cohort start",
                minDaysAtRisk: 1 //default 추가
            },
        ],
    },
    propensityScoreAdjustment: {
        psSettings: [
            {
                matchOnPsArgs: null,
                stratifyByPsArgs: {
                    numberOfStrata: 5,
                    baseSelection: "all" //default 설정
                },
            },
            {
                matchOnPsArgs: {
                    maxRatio: 0,
                    caliper: 0.2, //default 설정
                    caliperScale: "standardized logit" //default 설정
                },
                stratifyByPsArgs: null
            },
        ],
        createPsArgs: { //createPsArgs default로 추가 
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
        stratified: true,
        useCovariates: false, //default
        inversePtWeighting: false, //default
        prior: { priorType: "laplace", useCrossValidation: true }, //default
        control: { //default
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
