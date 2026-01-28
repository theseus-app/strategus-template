export const TEXTSGLT2iMetformin = `
Study Period 1-2: Before applying the eligibility criteria, we identified persons who filled a new prescription for first-line SGLT-2i (canagliflozin, empagliflozin, or dapagliflozin) or metformin between 1 April 2013 (consistent with the launch of SGLT2i in the United States) and 31 March 2020 in Optum (31 December 2018 in MarketScan and Medicare).

TAR 1: Follow-up began on the day after cohort entry and continued until the occurrence of a study outcome, death, treatment discontinuation (with an interval between prescription refills >60 days) (29), disenrollment, or end of the study period, whichever occurred first.
TAR 2: an intention-to-treat analysis was conducted to address potential informative censoring by carrying forward the initial exposure for 365 days without considering treatment discontinuation or the initiation of the comparator drug

PS Settings 1: We chose the ratio of 1:2 for matching to improve statistical efficiency because the number of metformin initiators was much larger
compared with SGLT-2i initiators. 

Outcome Model: Within each database, time block–specific PSmatched cohorts were aggregated for outcome regression and hazard ratios (HRs) with 95% CIs were estimated using proportional hazards models without further adjustment.
`

export const JSONSGLT2iMetformin = {
  getDbCohortMethodDataArgs: {
    studyPeriods: [
      {
        studyStartDate: "20130401",
        studyEndDate: "20200331",
      },
      {
        studyStartDate: "20130401",
        studyEndDate: "20181231",
      },
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
      }
    ],
  },
  propensityScoreAdjustment: {
    psSettings: [
      {
        matchOnPsArgs: {
          maxRatio: 2,
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
