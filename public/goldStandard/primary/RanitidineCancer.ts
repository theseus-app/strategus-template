export const TEXTRanitidineCancer =
    `
TAR: intention-to-treat (ITT), which followed patients until the end of data availability, with a 1-year lag;

PS Settings: Comparator cohorts were constructed by performing 1:1 PS matching with a caliper of 0.2 SDs of the logic of the PS. Database-specific PSs were estimated using L1 regularized logistic regression tuned by 10-fold cross-validation.

Outcome Model: Cox proportional hazard models were fitted to estimate the hazard ratios (HRs) and 95% CIs according to exposure using the CohortMethod21 R package for each data source.
`

export const JSONRanitidineCancer =
{
    maxCohortSize: 0, //default
    createStudyPopArgs: {
        restrictToCommonPeriod: false, //default
        firstExposureOnly: false, //default
        washoutPeriod: 365,
        removeDuplicateSubjects: "keep all", //default
        censorAtNewRiskWindow: false, //default
        removeSubjectsWithPriorOutcome: true,
        priorOutcomeLookBack: 365,
        timeAtRisks: {
            riskWindowStart: 365,
            startAnchor: "cohort start",
            riskWindowEnd: 99999,
            endAnchor: "cohort start",
            minDaysAtRisk: 1 //default로 설정
        },
    },
    propensityScoreAdjustment: {
        psSettings: {
            matchOnPsArgs: {
                maxRatio: 1,
                caliper: 0.2,
                caliperScale: "standardized logit",
            },
            stratifyByPsArgs: null, //default로 설정
        },
        createPsArgs: {
            maxCohortSizeForFitting: 250000, //default로 설정
            errorOnHighCorrelation: true, //default로 설정
            prior: {
                priorType: "laplace",
                useCrossValidation: true,
            },
            control: {
                tolerance: 2e-7, //default로 설정
                cvType: "auto", //default로 설정
                fold: 10,
                cvRepetitions: 10, //default로 설정
                noiseLevel: "silent", //default로 설정
                resetCoefficients: true, //default로 설정
                startingVariance: 0.01, //default로 설정
            },
        },
    },
    fitOutcomeModelArgs: { //modelType 제외 default 설정
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