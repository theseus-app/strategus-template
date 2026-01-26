export const TEXTGLP1RADepression = `
Study Period: We emulated the target trial using U.S. national Medicare administrative claims data between January 2013 and December 2020

TAR: The patients were followed from the index date until the diagnosis of depression; death; disenrollment from Medicare Part A, B, or D; up to 2 years of follow-up; or the end of the study (31 December 2020), whichever came first

PS Settings: The statistical analyses were the same as for the target trial except that 1:1 propensity score (PS) matching was used to mitigate potential confounding. The PS was estimated using multivariate logistic regression models incorporating baseline covariates. We applied nearest-neighbor matching without replacement, using a maximum caliper width of 0.05, to construct the matched cohort.

Outcome Model: To test the robustness of the study findings, we used a Fine–Gray subdistribution hazard model to address the competing risk for death, which allows a more accurate estimation of the cumulative incidence of the outcome in the presence of a competing event (death) that precludes the occurrence of the event of interest.
`

export const JSONGLP1RADepression = {
  getDbCohortMethodDataArgs: {
    studyPeriods: {
      studyStartDate: "20130101",
      studyEndDate: "20201231",
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
      riskWindowEnd: 730,
      endAnchor: "cohort start",
      minDaysAtRisk: 1
    },
  },
  propensityScoreAdjustment: {
    psSettings: {
      matchOnPsArgs: {
        maxRatio: 1,
        caliper: 0.05,
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
