export const TEXTCOVID19PPIandH2RA =
  `
Study Period: HIRA collected data on COVID-19 testing by the reverse transcriptase polymerase chain reaction method from 1 January to 15 May, 2020. 

TAR: Patients were censored if they were no longer observable in the database.

PS Settings: We used large-scale propensity score matching (PSM) with regularized logistic regression models to balance baseline characteristics of the study cohorts. propensity score stratification with five strata. 

Outcome Model: Cox proportional hazards regression models were used to model the time to the first outcome occurrence for the target relative to the comparator cohort in pairwise comparisons
`

export const JSONCOVID19PPIandH2RA = {
    getDbCohortMethodDataArgs: {
        studyPeriods: {
            studyStartDate: "20200101",
            studyEndDate: "20200515",
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
            riskWindowEnd: 99999,
            endAnchor: "cohort start",
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
