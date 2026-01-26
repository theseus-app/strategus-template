export const TEXTMARS = `
Study Period 1: a prospective cohort study conducted between January 2011 and December 2013 in two tertiary academic center adult ICUs in the Netherlands (Amsterdam University Medical Center, location AMC, and University Medical Center Utrecht).

TAR 1: The follow-up period started after this 72-h exposure period and ended 90 days after ICU admission.

PS Settings 1: For PS matching, we used greedy matching with a caliper width of 0.2 times the standard deviation of the PS logit and matched treated patients to controls 1:1. 

Outcome Model: After PS matching, we created survival curves of the matched samples and estimated the hazard ratios and their standard errors by using Cox models with a robust variance estimator to account for the matched pairs.
`

export const JSONMARS = {
  getDbCohortMethodDataArgs: {
    studyPeriods: [
      {
        studyStartDate: "20110101",
        studyEndDate: "20131231",
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
        riskWindowStart: 3,
        startAnchor: "cohort start",
        riskWindowEnd: 90,
        endAnchor: "cohort start",
        minDaysAtRisk: 1 //default 설정
      }
    ],
  },
  propensityScoreAdjustment: {
    psSettings: [
      {
        matchOnPsArgs: {
          maxRatio: 1,
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
