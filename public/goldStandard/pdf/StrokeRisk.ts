export const JSONStrokeRisk = {
  getDbCohortMethodDataArgs: {
    studyPeriods: [
      {
        studyStartDate: "20010101",
        studyEndDate: "20171231",
      },
      {
        studyStartDate: "20010101",
        studyEndDate: "20150930",
      },
    ],
    restrictToCommonPeriod: false, 
    firstExposureOnly: true, 
    washoutPeriod: 183, 
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
        riskWindowEnd: 0,
        endAnchor: "cohort end",
        minDaysAtRisk: 1,
      },
    ],
  },
  propensityScoreAdjustment: {
    psSettings: [
      {
        matchOnPsArgs: null,
        stratifyByPsArgs: null
      },
      {
        matchOnPsArgs: {
          maxRatio: 1,
          caliper: 0.05,
          caliperScale: "propensity score",
        },
        stratifyByPsArgs: null
      },
      {
        matchOnPsArgs: {
          maxRatio: 10,
          caliper: 0.2,
          caliperScale: "standardized logit",
        },
        stratifyByPsArgs: null
      },
    ],
    createPsArgs: {
      maxCohortSizeForFitting: 250000, //default로 설정
      errorOnHighCorrelation: true, //default로 설정
      prior: {
        priorType: "laplace",
        useCrossValidation: true // default로 설정
      },
      control: { //control 전부 default로 설정
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
