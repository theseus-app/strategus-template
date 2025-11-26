export const TEXTCOVID19Famotidine = `
Study Period 1: The study period started February 1, 2020, and ended May 30, 2020,

TAR 1: Time at risk was defined based on the intention-to-treat principle starting 1 day after admission and continuing up until the first of outcome of interest, loss to follow up, or 30 days after admission

PS Settings 1: To adjust for potential measured confounding and improve the balance between comparison cohorts, we built large-scale propensity score (PS) models for each comparison using regularized regression (13). We used a Laplace prior (LASSO) with the optimal hyperparameter to fit the model, determined through 10-fold cross validation in which the outcome is a binary indicator for the potential comparator. For the main analysis, we stratified into 5 PS strata
PS Settings 2: As a sensitivity analysis, we used a 1:1 PS matching.

Outcome Model: used conditional Cox proportional hazards models to estimate hazard ratios (HRs) between target and alternative comparator treatments for the risk of each outcome. The regression for the outcome models conditioned on the PS strata with treatment as the sole explanatory variable.
`

export const JSONCOVID19Famotidine = {
    getDbCohortMethodDataArgs: {
        studyPeriods: [
            {
                studyStartDate: "20200201",
                studyEndDate: "20200530",
            },
        ],
        restrictToCommonPeriod: false,
        firstExposureOnly: true,
        washoutPeriod: 0,
        removeDuplicateSubjects: "remove all",
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
                riskWindowEnd: 30,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default 설정
            },
        ],
    },
    propensityScoreAdjustment: {
        psSettings: [
            {
                matchOnPsArgs: null,
                stratifyByPsArgs: {
                    numberOfStrata: 5,
                    baseSelection: "all" //default 설정
                }
            },
            {
                matchOnPsArgs: {
                    maxRatio: 1,
                    caliper: 0.2, //default 설정
                    caliperScale: "standardized logit" //default 설정
                },
                stratifyByPsArgs: null
            },
        ],
        createPsArgs: {
            maxCohortSizeForFitting: 250000, //default 설정
            errorOnHighCorrelation: true, //default 설정
            prior: {
                priorType: "laplace",
                useCrossValidation: true,
            },
            control: {
                tolerance: 2e-7, //default 설정
                cvType: "auto", //default 설정
                fold: 10,
                cvRepetitions: 10, //default 설정
                noiseLevel: "silent", //default 설정
                resetCoefficients: true, //default 설정
                startingVariance: 0.01, //default 설정
            },
        },
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
    },
};
