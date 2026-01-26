export const TEXTCystectomyTrimodality = `
Study Period 1: This retrospective study included patients with localised, cT2–T4N0M0 muscle-invasive urothelial carcinoma of the bladder who would have been eligible for both radical cystectomy and trimodality therapy, who were treated between Jan 1, 2005, and Dec 31, 2017

TAR 1: The analysis was performed as intention-to-treat.

PS Settings 1: PSM using logistic regression and 3:1 matching with replacement
PS Settings 2-4: Additionally, varying match ratios for the PSM analysis were explored, including 1:1, 1:2, and 1:4 matching.

Outcome Model: Differences in overall survival by treatment were estimated using doubly robust multivariable Cox proportional hazards models incorporating covariates used in propensity score calculation.
`

export const JSONCystectomyTrimodality = {
  getDbCohortMethodDataArgs: {
    studyPeriods: [
      {
        studyStartDate: "20050101",
        studyEndDate: "20171231",
      }
    ],
    restrictToCommonPeriod: true,
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
        riskWindowEnd: 99999,
        endAnchor: "cohort start",
        minDaysAtRisk: 1 //default 설정
      }
    ],
  },
  propensityScoreAdjustment: {
    psSettings: [
      {
        matchOnPsArgs: {
          maxRatio: 3,
          caliper: 0.2, //default 설정
          caliperScale: "standardized logit" //default 설정
        },
        stratifyByPsArgs: null
      },
      {
        matchOnPsArgs: {
          maxRatio: 1,
          caliper: 0.2, //default 설정
          caliperScale: "standardized logit" //default 설정
        },
        stratifyByPsArgs: null
      },
      {
        matchOnPsArgs: {
          maxRatio: 2,
          caliper: 0.2, //default 설정
          caliperScale: "standardized logit" //default 설정
        },
        stratifyByPsArgs: null
      },
      {
        matchOnPsArgs: {
          maxRatio: 4,
          caliper: 0.2, //default 설정
          caliperScale: "standardized logit" //default 설정
        },
        stratifyByPsArgs: null
      }      
    ],
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
  fitOutcomeModelArgs: { 
    modelType: "cox",
    stratified: true,
    useCovariates: true,
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
