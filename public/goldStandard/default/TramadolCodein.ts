export const TEXTTramadolCodein = `
TAR 1: The OT TAR was calculated from the index date of the first exposure to the end of treatment, based on days’ supply, allowing for up to 30-day gaps between the end of days’ supply and the start of the next prescription. This end date was censored if a subject died, left the database, reached the age of 89 years, reached 365 days after the index date, was exposed to the other opioid of interest
TAR 2: The ITT TAR started on the index date and continued until the target or comparator subject died, left the database, or experienced the outcome of interest

PS Settings 1: A PS was estimated for each subject using the predicted probability from a regularized logistic regression model, fit with a Laplace prior (least absolute shrinkage and selection operator), and the regularization hyperparameter selected by optimizing the likelihood in a ten-fold cross validation, using a starting variance of 0.01 and a tolerance of 2e−7. Subjects were matched on 1:1 ratio of target to comparator subjects. This approach used a greedy matching algorithm by applying a caliper of 0.2 of the standard deviation on the logit scale of the PS distribution

Outcome Model: In this study, we compared target cohorts with the comparator cohorts for the hazards of O1 or O2 during the TAR by applying a Cox proportional hazards model conditioned on PSs.
`

export const JSONTramadolCodein = {
    getDbCohortMethodDataArgs: {
        studyPeriods: [
            {
                studyStartDate: "",
                studyEndDate: "",
            },
        ],
        restrictToCommonPeriod: false,
        firstExposureOnly: false,
        washoutPeriod: 0,
        removeDuplicateSubjects: "keep all",
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
            },
        ],
    },
    propensityScoreAdjustment: {
        psSettings: [
            {
                matchOnPsArgs: {
                    maxRatio: 1,
                    caliper: 0.2,
                    caliperScale: "standardized logit",
                },
                stratifyByPsArgs: null,
            },
        ],
        createPsArgs: {
            maxCohortSizeForFitting: 250000, //default로 설정
            errorOnHighCorrelation: true, //default로 설정
            prior: {
                priorType: "laplace",
                useCrossValidation: true,
            },
            control: {
                tolerance: 2e-7,
                cvType: "auto", //default로 설정
                fold: 10,
                cvRepetitions: 10, //default로 설정
                noiseLevel: "silent", //default로 설정
                resetCoefficients: true, //default로 설정
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
