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
