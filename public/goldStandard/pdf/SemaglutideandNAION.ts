export const JSONSemaglutideandNAION =
{
    getDbCohortMethodDataArgs: {
        studyPeriods: [
            {
                studyStartDate: 20171201,
                studyEndDate: 20231231
            }
        ],
        restrictToCommonPeriod: true, 
        firstExposureOnly: false, 
        washoutPeriod: 0, 
        removeDuplicateSubjects: "keep all",
        maxCohortSize: 0, //default
    },
    createStudyPopArgs: {
        censorAtNewRiskWindow: true, //default로 설정
        removeSubjectsWithPriorOutcome: true,
        priorOutcomeLookBack: 99999,
        timeAtRisks: [
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 0,
                endAnchor: "cohort end",
                minDaysAtRisk: 1, //default로 설정
            }
        ]
    },
    propensityScoreAdjustment: {
        psSettings: [
            {
                matchOnPsArgs: {
                    maxRatio: 1,
                    caliper: 0.2, //default로 설정
                    caliperScale: "standardized logit"
                },
                stratifyByPsArgs: null
            },
            {
                matchOnPsArgs: null,
                stratifyByPsArgs: {
                    numberOfStrata: 5, //default로 설정
                    baseSelection: "all"
                }
            }
        ],
        createPsArgs: {
            maxCohortSizeForFitting: 250000, //default로 설정
            errorOnHighCorrelation: true,
            prior: {
                priorType: "laplace",
                useCrossValidation: true
            },
            control: {
                tolerance: 2e-7, //default로 설정
                cvType: "auto",
                fold: 10, // default로 설정
                cvRepetitions: 10, //default로 설정
                noiseLevel: "silent",
                resetCoefficients: true,
                startingVariance: 0.01, //default로 설정
            }
        }
    },
    fitOutcomeModelArgs: { //default except modelType
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