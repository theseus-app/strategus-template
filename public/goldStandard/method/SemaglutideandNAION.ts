export const TEXTSemaglutideandNAION =
    `
Methods
Study Design
We retrospectively analyzed 14 OHDSI databases (6 administrative claims and 8 electronic health records) using 2 approaches: (1) an active-comparator new-user cohort design to test whether NAION risk is higher with semaglutide compared with other GLP-1RA and non–GLP-1RAs and (2) a self-controlled case-series method to test whether NAION risk was higher with semaglutide exposure compared with nonexposure.15 ,16 Database details are in eTable 1 in Supplement 1. All participating sites had local institutional review board approvals where informed consent was waived because this was considered secondary research or exemptions. The study adhered to the tenets of the Declaration of Helsinki. This study followed the Strengthening the Reporting of Observational Studies in Epidemiology (STROBE) reporting guidelines.17

Participants and Exposure
Adults 18 years and older with T2D taking semaglutide (GLP-1RA), dulaglutide (GLP-1RA), exenatide (GLP-1RA), empagliflozin (sodium-glucose cotransporter 2 [SGLT2] inhibitor), sitagliptin (dipeptidyl peptidase 4 [DPP4] inhibitor), or glipizide (sulfonylurea) during the study period (December 1, 2017-December 31, 2023) were included. For the non–GLP-1RA comparator medications, the most commonly used within each drug class was chosen based on the utilization results from the Large-Scale Evidence Generation and Evaluation Across a Network of Databases for Type 2 Diabetes Mellitus (LEGEND-T2DM) study, an OHDSI network study examining second-line antihyperglycemic agents and cardiovascular outcomes.7,8,11 Participants self-identified races and ethnicities were categorized according to local database guidelines and could include Asian, Black or African American, Hispanic or Latino, not Hispanic or Latino, and White.

Time at Risk
The time-at-risk period began with medication initiation until the end of continuous drug exposure, defined as a gap in exposure of more than 30 days or the end of the continuous observation period.

Outcome
The outcome of interest was NAION, determined by diagnosis codes. A consortium of board-certified ophthalmologists including fellowship-trained neuro-ophthalmologists developed 2 definitions: the sensitive definition required 1 ischemic optic neuropathy diagnosis code, whereas the specific definition required a second confirmatory ischemic optic neuropathy diagnosis code within 90 days. For both definitions, patients with traumatic optic neuropathy or 2 diagnosis codes of giant cell arteritis were excluded. If patients had diagnosis codes within 60 days before the NAION diagnosis related to optic disc disorders, optic neuritis, or optic disc edema, the date of the NAION outcome was shifted to the earlier diagnosis because it was likely related to the NAION event.18 -20 Details are in eTables 2 and 3 in Supplement 1 and eTable 4 in Supplement 2.

The sensitivity, specificity, positive predictive value, and negative predictive value of both NAION definitions were evaluated on a subset of databases using PheValuator.21 PheValuator is an OHDSI tool that uses machine learning to compute a patient’s probability of having a disease of interest to identify a set of probabilistic gold standards, or patients with high likelihood of having the disease. The probabilistic gold standard dataset was used to determine the performance metrics of the consortium-developed rule-based definitions. Additional details are on the study protocol page.22

Negative Control Outcomes
In addition to the primary outcome, NAION, we included 97 outcomes believed to be causally unrelated to the exposures under investigation.22 To detect and adjust for systematic errors from residual confounding, both the cohort analysis and self-controlled case-series analysis were performed using these negative control outcomes.23,24

Statistical Analysis
All analyses were performed in R, version 4.2.3 (R Project for Statistical Computing).25,26 Analysis codes are available on Github.16 All P values were 2-sided, and P <.05 was considered statistically significant. Results can also be found on an interactive website.27

Characterization
We summarized the baseline characteristics of patients within each T2DM medication exposure cohort by database.28 These characteristics included demographics (age, sex, race, and ethnicity), Diabetes Comorbidity Severity Index score, Charlson Comorbidity Index-Romano adaptation, prior medical diagnoses (essential hypertension, hyperlipidemia, obstructive sleep apnea, chronic kidney disease, and anemia), and prior medication use (interferon, amiodarone, and phosphodiesterase inhibitor).29,30 NAION incidence proportion (number of patients with NAION divided by persons at risk) and incidence rate (number of patients with NAION during time-at-risk divided by person-days) were calculated.28,31

Active-Comparator New-User Cohort Analysis
An active-comparator new-user cohort design was used to estimate propensity score–adjusted hazard ratios (HRs) for NAION comparing new semaglutide users with users of comparator medications: GLP-1RAs (dulaglutide and exenatide) and non–GLP-1RA drugs (empagliflozin, sitagliptin, and glipizide).32 New users were defined as previously described.7,8,11 In brief, adults with T2D taking metformin monotherapy were included if they had at least 1 year of prior observation, initiated treatment with one of the medications of interest, had no prior exposure to a comparator diabetes medication, and had at most 30 days of prior insulin use.

Propensity scores were estimated using a large-scale propensity score approach, applying regularized regression over all baseline characteristics (eg, demographic characteristics, preexisting conditions, medications, and procedures).33,34 Patients in each target and comparator exposure comparison (eg, semaglutide vs dulaglutide) were matched 1:1 using propensity scores. Cox proportional hazards models estimated the HR of NAION from cohort entry to the outcome while taking treatment with each target and comparator T2D medication.

Sensitivity Analyses
We used 2 sensitivity analyses to test robustness. To assess sensitivity to the cohort definition, we created a second definition to include new users of each T2D medication regardless of prior exposure to a comparator drug. Details can be found in eTables 2 and 3 in Supplement 1 and eTable 4 in Supplement 2.22 We also stratified our propensity score–matched Cox proportional hazards models by calendar time (December 2017-January 2020, February 2020-June 2021, and July 2021-December 2023). This addressed potential biases due to decreased health care utilization from the COVID-19 pandemic, and the 60% increased prescription of semaglutide from 2021 to 2023 after FDA approval for obesity.22,35

Self-Controlled Case-Series Analysis
A self-controlled case series, where patients served as their own controls, was used to estimate the NAION incidence rate ratio (IRR) during each T2D medication exposure compared with nonexposure control time.36-38 Real-world data evaluation of study designs have found that this approach can accurately identify causal associations between drug exposures and health outcomes.36,38,39 We restricted the observation period to when patients had T2D and excluded the patients’ first 365 days in the database from the analysis to improve the identification of incident NAION. We used conditional Poisson regression models to compare the NAION IRRs.40 We defined a separate preexposure time window, during the control time, as the period of 30 days before treatment initiation and adjusted for it.40,41 The models also adjusted for potential effects of seasonality by including spline functions of calendar months.

Study Diagnostics and Meta-Analysis
For both cohort and self-controlled case-series analyses, we used a comprehensive set of study diagnostics, eg, preexposure patient characteristic balance, empirical clinical equipoise, minimum power requirement, and negative control experiments, to evaluate systematic error, residual bias, reliability, and the generalizability of our comparisons.23,24,36,42,43 Only databases that passed prespecified study diagnostics reported HR or IRR estimates.22 A bayesian random-effects meta-analysis combined each database’s estimates into a network-wide estimate.44,45
`

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