export const TEXTTicagrelorClopidogrel =
    `
Study Period: from November 2011 to March 2019.

TAR: Patients were censored when they were no longer observed in the database, while they remained in the primary analysis if they discontinued the allocated drug or switched the drug within the first year.

PS Settings: The study populations were matched using one-to-one greedy matching of the propensity score.

Outcome Model: We used Cox proportional haz ard regression models to estimate the association of expo sures with outcomes
`

export const JSONTicagrelorClopidogrel = {
    getDbCohortMethodDataArgs: {
        studyPeriods: {
            studyStartDate: "20111101",
            studyEndDate: "20190331",
        },
        maxCohortSize: 0, //default
    },
    createStudyPopArgs: {
        restrictToCommonPeriod: false, //default
        firstExposureOnly: false, //default
        washoutPeriod: 365,
        removeDuplicateSubjects: "keep all", //default
        censorAtNewRiskWindow: false, //default
        removeSubjectsWithPriorOutcome: true,
        priorOutcomeLookBack: 99999,
        timeAtRisks: {
            riskWindowStart: 1,
            startAnchor: "cohort start",
            riskWindowEnd: 365,
            endAnchor: "cohort start",
            minDaysAtRisk: 1, //default로 설정
        },
    },
    propensityScoreAdjustment: {
        psSettings: {
            matchOnPsArgs: {
                maxRatio: 1,
                caliper: 0.2, //default로 설정
                caliperScale: "standardized logit" //default로 설정
            },
            stratifyByPsArgs: null,
        },
        createPsArgs: { //default
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
            },
        },
    },
    fitOutcomeModelArgs: { //default except modelType
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
    },
};
