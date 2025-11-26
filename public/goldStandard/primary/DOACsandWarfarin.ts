export const TEXTDOACsandWarfarin =
  `
Study Period: The overall study period was from 19 October 2010 (coinciding with initial approval of dabigatran) to 31 December 2018.

TAR: The primary on-treatment time-at-risk period was defined as the time from 1 day after the anticoagulant index date to the end of inferred persistent exposure, the last day of observation, or the end of the study period, whichever came first. 

PS Settings:  Propensity scores (PSs) were used to reduce potential confounding due to an imbalance of observed patient characteristics at baseline and were calculated for each patient using the predicted probability from a regularized logistic regression model, fit with a Laplace prior (LASSO) and the regularization hyperparameter selected by optimizing the likelihood in a tenfold cross validation. The primary PS strategy matched the target to comparator patients in a 1:1 ratio

Outcome Model:
`

export const JSONDOACsandWarfarin = {
    getDbCohortMethodDataArgs: {
        studyPeriods: {
            studyStartDate: "20101019",
            studyEndDate: "20181231",
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
            riskWindowStart: 1,
            startAnchor: "cohort start",
            riskWindowEnd: 0,
            endAnchor: "cohort end",
            minDaysAtRisk: 1 //default 설정
        },
    },
    propensityScoreAdjustment: {
        psSettings: {
            matchOnPsArgs: {
                maxRatio: 1,
                caliper: 0.2,
                caliperScale: "standardized logit"
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
