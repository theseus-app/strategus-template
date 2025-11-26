export const TEXTUveitisSafety =
    `
TAR: We defined the ‘on-treatment’ time-at-risk (TAR) as the day after index until the end of a period of inferred persistent exposure.

PS Settings: Our primary PS adjustment strategy matched target to comparator patients using a 1:10 maximum variable ratio matching approach and used a greedy matching algorithm that applied a caliper of 0.2 of the standard deviation on the logit scale of the PS distribution [48]. The PS was calculated for each patient as the predicted probability of target exposure status from an L1 regularized logistic regression model, fit with a Laplace prior where the regularization hyperparameter was selected by optimizing the likelihood in a 10-fold cross validation with a starting variance of 0.01 and a tolerance of 2*10− 7 [47].

Outcome Model: Within each database and study population, we fit a Cox proportional-hazards (PH) regression model conditioned on PS-matched sets with Remicade® treatment status as the explanatory variable to model the time to the first ever NIU occurrence relative to the comparator group.
`

export const JSONUveitisSafety =
{
    maxCohortSize: 0, //default
    createStudyPopArgs: {
        restrictToCommonPeriod: true,
        firstExposureOnly: true,
        washoutPeriod: 365,
        removeDuplicateSubjects: "keep all", //default
        censorAtNewRiskWindow: true,
        removeSubjectsWithPriorOutcome: true,
        priorOutcomeLookBack: 99999,
        timeAtRisks: {
            riskWindowStart: 1,
            startAnchor: "cohort start",
            riskWindowEnd: 0,
            endAnchor: "cohort end",
            minDaysAtRisk: 1, //default로 설정
        }
    },
    propensityScoreAdjustment: {
        psSettings: {
            matchOnPsArgs: {
                maxRatio: 10,
                caliper: 0.2,
                caliperScale: "standardized logit"
            },
            stratifyByPsArgs: null,
        },
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