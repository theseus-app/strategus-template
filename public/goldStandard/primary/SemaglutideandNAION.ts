export const TEXTSemaglutideandNAION =
    `
Study Period: Adults 18 years and older with T2D taking semaglutide (GLP-1RA), dulaglutide (GLP-1RA), exenatide (GLP-1RA), empagliflozin (sodium-glucose cotransporter 2 [SGLT2] inhibitor), sitagliptin (dipeptidyl peptidase 4 [DPP4] inhibitor), or glipizide (sulfonylurea) during the study period (December 1, 2017-December 31, 2023) were included.

TAR: The time-at-risk period began with medication initiation until the end of continuous drug exposure, defined as a gap in exposure of more than 30 days or the end of the continuous observation period.

PS Settings: Patients in each target and comparator exposure comparison (eg, semaglutide vs dulaglutide) were matched 1:1 using propensity scores

Outcome Model: Cox proportional hazards models estimated the HR of NAION from cohort entry to the outcome while taking treatment with each target and comparator T2D medication.
`

export const JSONSemaglutideandNAION =
{
    getDbCohortMethodDataArgs: {
        studyPeriods: {
            studyStartDate: 20171201,
            studyEndDate: 20231231
        },
        maxCohortSize: 0, //default
    },
    createStudyPopArgs: {
        restrictToCommonPeriod: false, //default
        firstExposureOnly: false, //default
        washoutPeriod: 365,
        removeDuplicateSubjects: "keep all", //default
        censorAtNewRiskWindow: false, //default
        removeSubjectsWithPriorOutcome: true, //default
        priorOutcomeLookBack: 99999, //default
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
                maxRatio: 1,
                caliper: 0.2, //default로 설정
                caliperScale: "standardized logit"
            },
            stratifyByPsArgs: null
        },
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