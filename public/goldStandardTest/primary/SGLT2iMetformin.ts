export const TEXTSGLT2iMetformin = `
Study Period: Before applying the eligibility criteria, we identified persons who filled a new prescription for first-line SGLT-2i (canagliflozin, empagliflozin, or dapagliflozin) or metformin between 1 April 2013 (consistent with the launch of SGLT2i in the United States) and 31 March 2020 in Optum.

TAR: Follow-up began on the day after cohort entry and continued until the occurrence of a study outcome, death, treatment discontinuation (with an interval between prescription refills >60 days) (29), disenrollment, or end of the study period, whichever occurred first.

PS Settings: We chose the ratio of 1:2 for matching to improve statistical efficiency because the number of metformin initiators was much larger
compared with SGLT-2i initiators.

Outcome Model: Within each database, time block–specific PSmatched cohorts were aggregated for outcome regression and hazard ratios (HRs) with 95% CIs were estimated using proportional hazards models without further adjustment.
`

export const JSONSGLT2iMetformin = {
  getDbCohortMethodDataArgs: {
    studyPeriods: {
      studyStartDate: "20130401",
      studyEndDate: "20200331",
    },
    maxCohortSize: 0,
  },
  createStudyPopArgs: {
    restrictToCommonPeriod: true,
    firstExposureOnly: false,
    washoutPeriod: 0,
    removeDuplicateSubjects: "keep all",
    censorAtNewRiskWindow: false,
    removeSubjectsWithPriorOutcome: true,
    priorOutcomeLookBack: 99999,
    timeAtRisks: {
      riskWindowStart: 1,
      startAnchor: "cohort start",
      riskWindowEnd: 0,
      endAnchor: "cohort end",
      minDaysAtRisk: 1
    },
  },
  propensityScoreAdjustment: {
    psSettings: {
      matchOnPsArgs: {
        maxRatio: 2,
        caliper: 0.2,
        caliperScale: "standardized logit"
      },
      stratifyByPsArgs: null
    },
    createPsArgs: {
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
