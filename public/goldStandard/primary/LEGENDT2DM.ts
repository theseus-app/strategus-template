export const TEXTLEGENDT2DM =
  `
Study Period: In this study, we included 10 real-world data sources from the LEGEND-T2DM network, including 6 administrative claims and 4 electronic health record (EHR) databases across 4 countries from 1992 to 2021

TAR: We considered an on-treatment time-at-risk definition that follows a patient from treatment initiation to treatment discontinuation, which captures direct treatment effects while allowing for escalation with additional T2DM agents.

PS Settings: we preferred stratification 

Outcome Model: We then used Cox proportional hazards models to estimate HRs of each outcome for each comparison, conditional on PS stratification or variable-ratio patient matching.
`

export const JSONLEGENDT2DM = {
    getDbCohortMethodDataArgs: {
        studyPeriods: {
            studyStartDate: "19920101",
            studyEndDate: "20211231",
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
      matchOnPsArgs: null,
      stratifyByPsArgs: {
          numberOfStrata: 5,
          baseSelection: "all" //default로 설정
      },
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
