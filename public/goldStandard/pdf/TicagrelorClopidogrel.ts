export const JSONTicagrelorClopidogrel = {
    getDbCohortMethodDataArgs: {
        studyPeriods: [
            {
                studyStartDate: "20111101",
                studyEndDate: "20190331",
            },
            {
                studyStartDate: "20130301",
                studyEndDate: "20161231",
            },
        ],
        restrictToCommonPeriod: true, 
        firstExposureOnly: false, 
        washoutPeriod: 0, 
        removeDuplicateSubjects: "keep first",
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
                riskWindowEnd: 365,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 1825,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 0,
                endAnchor: "cohort end",
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 29,
                startAnchor: "cohort start",
                riskWindowEnd: 365,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 29,
                startAnchor: "cohort start",
                riskWindowEnd: 1825,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 29,
                startAnchor: "cohort start",
                riskWindowEnd: 0,
                endAnchor: "cohort end",
                minDaysAtRisk: 1, //default로 설정
            },
        ],
    },
    propensityScoreAdjustment: {
        psSettings: [
            {
                matchOnPsArgs: {
                    maxRatio: 1,
                    caliper: 0.2, //default로 설정
                    caliperScale: "standardized logit" //default로 설정
                },
                stratifyByPsArgs: null,
            },
            {
                matchOnPsArgs: {
                    maxRatio: 10,
                    caliper: 0.2, //default로 설정
                    caliperScale: "standardized logit" //default로 설정
                },
                stratifyByPsArgs: null,
            },
            {
                matchOnPsArgs: null,
                stratifyByPsArgs: {
                    numberOfStrata: 10,
                    baseSelection: "all" //default로 설정
                },
            },
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
};
