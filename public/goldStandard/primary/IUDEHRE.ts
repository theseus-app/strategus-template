export const TEXTIUDEHRE =
  `
Study Period: The study window was restricted to all IUD placements that occurred on or after January 1, 2003.

TAR: The time at risk was from 30 days to 15 years after IUD placement.

PS Settings: we used propensity score models with a regularized logistic regression. Patients were matched on the propensity score by 1:1 matching.

Outcome Model: used a Cox proportional hazards model to determine the relative risk for cervical neoplasms between the Cu IUD and LNG-IUS cohorts
`

export const JSONIUDEHRE = {
    getDbCohortMethodDataArgs: {
        studyPeriods: {
            studyStartDate: "20030101",
            studyEndDate: null,
        },
        maxCohortSize: 0, //default
    },
    createStudyPopArgs: {
        restrictToCommonPeriod: false, //default로 설정
        firstExposureOnly: true,
        washoutPeriod: 365,
        removeDuplicateSubjects: "keep all", //default로 설정
        censorAtNewRiskWindow: false, //default로 설정
        removeSubjectsWithPriorOutcome: true,
        priorOutcomeLookBack: 99999,
        timeAtRisks: {
            riskWindowStart: 30,
            startAnchor: "cohort start",
            riskWindowEnd: 5475,
            endAnchor: "cohort start",
            minDaysAtRisk: 1 //default 설정
        },
    },
    propensityScoreAdjustment: {
        psSettings: {
            matchOnPsArgs: {
                maxRatio: 1,
                caliper: 0.2, //default로 설정
                caliperScale: "standardized logit" //default로 설정
            },
            stratifyByPsArgs: null,
        },
    createPsArgs: { //laplace 제외하고 전부 default 설정
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
      }
    }
  },
  fitOutcomeModelArgs: { //modelType제외 default 설정
    modelType: "cox",
    stratified: false,
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
  }
}
