export const TEXTStrokeRisk =
  `
Study Period: Included data available from January 1, 2001 through December 31, 2017

TAR: "On-treatment” time at risk in the cohort for which they first qualified, starting from the day after they entered that cohort until the first of: receiving a medication associated with the other cohort, having the study outcome (see in the following sections), having a gap of more than 30 days in the supply of the cohort-defining drug (more than 30 days from the end of the days’ supply one dispensing to the next dispensing), reaching the end of insurance enrollment, or reaching the end of the study.
For both cohorts being compared, patients were required to have at least 1 day of continuous observation after the time-at-risk start.

PS Settings: the large-scale regularized regression model involving PS estimation (adapted PS strategy) using predicted probability from a large-scale regularized logistic regression model, fit with a Laplace prior (LASSO) using all observed covariates, including a recent diagnosis of dementia as input and 1:10 variable ratio matching with a caliper of 0.2 of the standard deviation of the logit of PS.

Outcome Model: The HRs for the outcome during the time-at-risk was estimated by applying a Cox proportional hazards model conditioned on the PS matched sets. 
`

export const JSONStrokeRisk = {
  getDbCohortMethodDataArgs: {
    studyPeriods: {
      studyStartDate: "20010101",
      studyEndDate: "20171231",
    },
    maxCohortSize: 0, //default
  },
  createStudyPopArgs: {
    restrictToCommonPeriod: false, //default
    firstExposureOnly: false, //default
    washoutPeriod: 0, //default
    removeDuplicateSubjects: "keep all", //default
    censorAtNewRiskWindow: false, //default
    removeSubjectsWithPriorOutcome: true, //default
    priorOutcomeLookBack: 99999, //default
    timeAtRisks: {
      riskWindowStart: 1,
      startAnchor: "cohort start",
      riskWindowEnd: 0,
      endAnchor: "cohort end",
      minDaysAtRisk: 1,
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
