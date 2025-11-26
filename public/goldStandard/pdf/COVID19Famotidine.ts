export const JSONCOVID19Famotidine = {
    getDbCohortMethodDataArgs: {
        studyPeriods: [
            {
                studyStartDate: "20200201",
                studyEndDate: "20200530",
            },
        ],
        restrictToCommonPeriod: false, 
        firstExposureOnly: true, 
        washoutPeriod: 0, 
        removeDuplicateSubjects: "remove all",
        maxCohortSize: 0, //default
    },
    createStudyPopArgs: {
        censorAtNewRiskWindow: false, //default로 설정
        removeSubjectsWithPriorOutcome: false,
        priorOutcomeLookBack: 99999,
        timeAtRisks: [
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 30,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default 설정
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
                }
            },
            {
                matchOnPsArgs: {
                    maxRatio: 1,
                    caliper: 0.2, //default 설정
                    caliperScale: "standardized logit" //default 설정
                },
                stratifyByPsArgs: null
            },
        ],
        createPsArgs: {
            maxCohortSizeForFitting: 250000, //default 설정
            errorOnHighCorrelation: true, //default 설정
            prior: {
                priorType: "laplace",
                useCrossValidation: true,
            },
            control: {
                tolerance: 2e-7, //default 설정
                cvType: "auto", //default 설정
                fold: 10,
                cvRepetitions: 10, //default 설정
                noiseLevel: "silent", //default 설정
                resetCoefficients: true, //default 설정
                startingVariance: 0.01, //default 설정
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
