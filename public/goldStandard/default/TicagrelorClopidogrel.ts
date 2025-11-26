export const TEXTTicagrelorClopidogrel = `
Study Period 1: from November 2011 to March 2019.
Study Period 2: we conducted an additional analysis to apply an identical study period to all databases, namely March 2013 and December2016,b

TAR 1: Patients were censored when they were no longer observed in the database, while they remained in the primary analysis if they discontinued the allocated drug or switched the drug within the first year.
TAR 2-3: 2 more time-at-risk windows were applied—a 5-year period and an on-treatment period. The on-treatment period was defined as the time from 1 day after the index date until the end of persistent exposure to the drug, allowing a 7-day gap or the end of a patient’s record.

PS Settings 1: The study populations were matched using one-to-one greedy matching of the propensity score.
PS Settings 2-3: In addition to one-to-one propensity score matching, 2 additional propensity score adjustments were performed: (1) variable-ratio propensity score matching with a maximum ratio of 10; and (2) propensity score stratification using deciles of the propensity score distribution.

Outcome Model: We used Cox proportional haz ard regression models to estimate the association of expo sures with outcomes
`

export const JSONTicagrelorClopidogrel = {
    getDbCohortMethodDataArgs: {
        studyPeriods: [
            {
                studyStartDate: "20111101",
                studyEndDate: "20190331",
            },
            {
                studyStartDate: "20130301",
                studyEndDate: "20161231",
            },
        ],
        restrictToCommonPeriod: true,
        firstExposureOnly: false,
        washoutPeriod: 0,
        removeDuplicateSubjects: "keep first",
        maxCohortSize: 0, //default
    },
    createStudyPopArgs: {
        censorAtNewRiskWindow: false, //default로 설정
        removeSubjectsWithPriorOutcome: false,
        priorOutcomeLookBack: 99999,
        timeAtRisks: [
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 365,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 1825,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 0,
                endAnchor: "cohort end",
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 29,
                startAnchor: "cohort start",
                riskWindowEnd: 365,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 29,
                startAnchor: "cohort start",
                riskWindowEnd: 1825,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 29,
                startAnchor: "cohort start",
                riskWindowEnd: 0,
                endAnchor: "cohort end",
                minDaysAtRisk: 1, //default로 설정
            },
        ],
    },
    propensityScoreAdjustment: {
        psSettings: [
            {
                matchOnPsArgs: {
                    maxRatio: 1,
                    caliper: 0.2, //default로 설정
                    caliperScale: "standardized logit" //default로 설정
                },
                stratifyByPsArgs: null,
            },
            {
                matchOnPsArgs: {
                    maxRatio: 10,
                    caliper: 0.2, //default로 설정
                    caliperScale: "standardized logit" //default로 설정
                },
                stratifyByPsArgs: null,
            },
            {
                matchOnPsArgs: null,
                stratifyByPsArgs: {
                    numberOfStrata: 10,
                    baseSelection: "all" //default로 설정
                },
            },
        ],
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
    },
};
