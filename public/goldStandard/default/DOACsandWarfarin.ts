export const TEXTDOACsandWarfarin = `
Study Period 1: The overall study period was from 19 October 2010 (coinciding with initial approval of dabigatran) to 31 December 2018.

TAR 1: The primary on-treatment time-at-risk period was defined as the time from 1 day after the anticoagulant index date to the end of inferred persistent exposure, the last day of observation, or the end of the study period, whichever came first. For DOACs, the final exposure record allowed for no more than a 3-day gap between successive exposure intervals (inferred by days’ supply) plus 5 days appended to the last exposure date. For warfarin, the final exposure record allowed for no more than a 7-day gap between successive exposure intervals (inferred by days’ supply) plus 5 days appended to the last exposure date.
TAR 2: the on-treatment time-at-risk period was similarly defined as in the primary analysis, but the final exposure record allowed for no more than a 30-day gap between successive exposure intervals (inferred by days’ supply) with no days appended to the last exposure date for DOACs and warfarin.
TAR 3: The second sensitivity analysis was an intent-to-treat (ITT) analysis, defined as the time from 1 day after the anticoagulant index date until the last day of observation or the end of the study period, whichever came first, regardless of the subsequent drug dispensing records.

PS Settings 1: Propensity scores (PSs) were used to reduce potential confounding due to an imbalance of observed patient characteristics at baseline and were calculated for each patient using the predicted probability from a regularized logistic regression model, fit with a Laplace prior (LASSO) and the regularization hyperparameter selected by optimizing the likelihood in a tenfold cross validation. The primary PS strategy matched the target to comparator patients in a 1:1 ratio
PS Settings 2: sensitivity PS strategy variably matched the target to comparator patients in a maximum of a 1:100 ratio

Outcome Model: 
`


export const JSONDOACsandWarfarin = {
  getDbCohortMethodDataArgs: {
    studyPeriods: [
      {
        studyStartDate: "20101019",
        studyEndDate: "20181231",
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
        riskWindowEnd: 5,
        endAnchor: "cohort end",
        minDaysAtRisk: 1 //default 설정
      },
      {
        riskWindowStart: 1,
        startAnchor: "cohort start",
        riskWindowEnd: 0,
        endAnchor: "cohort end",
        minDaysAtRisk: 1 //default 설정
      },
      {
        riskWindowStart: 1,
        startAnchor: "cohort start",
        riskWindowEnd: 99999,
        endAnchor: "cohort start",
        minDaysAtRisk: 1 //default 설정
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
          maxRatio: 100,
          caliper: 0.2, //default로 설정
          caliperScale: "standardized logit" //default로 설정
        },
        stratifyByPsArgs: null,
      },
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
  fitOutcomeModelArgs: { //modelType제외 default 설정
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
  }
}
