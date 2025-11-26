export const JSONUveitisSafety ={
    getDbCohortMethodDataArgs: {
      studyPeriods: [
        {
          studyStartDate: "",
          studyEndDate: "",
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
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 99999,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            }
        ]
    },
    propensityScoreAdjustment: {
        psSettings: [
            {
                matchOnPsArgs: {
                    maxRatio: 10,
                    caliper: 0.2,
                    caliperScale: "standardized logit"
                },
                stratifyByPsArgs: null,
            },
            {
                matchOnPsArgs: {
                    maxRatio: 1,
                    caliper: 0.2,
                    caliperScale: "standardized logit"
                },
                stratifyByPsArgs: null,
            }
        ],
        createPsArgs: {
            maxCohortSizeForFitting: 250000, //default로 설정
            errorOnHighCorrelation: true, //default로 설정
            prior: {
                priorType: "laplace",
                useCrossValidation: true
            },
            control: {
                tolerance: 2e-7,
                cvType: "auto",
                fold: 10,
                cvRepetitions: 10, //default로 설정
                noiseLevel: "silent", //default로 설정
                resetCoefficients: true, //default로 설정
                startingVariance: 0.01
            }
        }
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
    }
}