export const TEXTCEEAMOS =
    `
Methods
Data source
We performed a population-based, retrospective cohort study using two claims databases in the US and South Korea: US Open Claims and Health Insurance Review and Assessment Service National Claims (HIRA) (see eMethod 1 in Supplement 1 for database details). These databases were standardized using the Observational Medical Outcomes Partnership Common Data Model, version 5.3 [11].

Each data partner executes the package locally inside the firewall. The pre-designated statistical results (without patient-level information) were shared for interpretation and database-level meta-analyses. All partners received Institutional Review Board approval or exemption (IRB number: AJIRB-MED-MDB-21-274).

Study design
Active-comparator new-user designs were applied in our study to mitigate the methodological limitations of observational studies [12]. For new-user design, we identified patients who had newly initiated antihypertensive medications. For the active-comparator design, ACE inhibitors were compared with ARBs and thiazide diuretics (thiazide or thiazide-like diuretics), which are commonly used for the same indication and reported to be unrelated to the occurrence of schizophrenia [13]. We compared the incidence of outcomes between the two groups (ACE inhibitor vs. ARB and ACE inhibitor vs. thiazide diuretics).

We conducted distributed network analyses similar to previous studies [14, 15]. The statistical analytical protocol (see Supplement 2) was pre-specified before execution. According to this protocol, the study package for the entire process was built using the OHDSI Health Analytics Data-to-Evidence Suite in R; detailed study codes are available online at https://github.com/ohdsi-studies/Ceeamos. The study protocol was registered with the EU Post-Authorization Studies register under EUPAS42783.

Study population and exposure
We identified adult (aged ≥18 years) patients who were exposed to antihypertensive drugs (ACE inhibitors, ARBs, or thiazide diuretics) for the first time according to their medical history. Combination products of each ingredient were not included in this study. The index date was defined as the date of the first exposure to antihypertensive drugs. To avoid left censoring (i.e., incomplete data on patients who were already on antihypertensive treatment before entering the study), we excluded patients who were enrolled in the database for < 1 year before the index date. We excluded patients without a diagnosis 1 year before the index date. The other exclusion criteria were as follows: (1) a history of exposure to any hypertension treatment (prevalent user), (2) schizophrenia diagnosis and heart failure diagnosis at any time before the index date, (3) prescription of other blood pressure lowering medications (non-thiazide diuretics, beta blockers, and calcium channel blockers) and (4) prescription of the opposite drug (ARBs or thiazide diuretics for the ACE inhibitor group and vice versa) during the 7 days after the index date for ascertaining first-line treatment. Further details on cohort definitions are presented in Supplement 2.

Outcomes and follow-up
The primary outcome was a diagnosis of schizophrenia for the first time. To increase the specificity of the diagnosis, we applied a restricted definition of outcome, which included at least one diagnosis of schizophrenia, at least two prescriptions of antipsychotics, or at least two psychiatric procedures (electroconvulsive therapy and psychotherapy) at any time after the first diagnosis of schizophrenia. The secondary outcome was a specific definition of schizophrenia at the emergency department visit. Further details of the outcome definitions are provided in Supplement 2.

Our analysis considered the time-to-first event and was followed up to the earliest date among last date of assigned treatment, date of last observation in the database, date of occurrence of the endpoint, and date of censoring (as-treated [AT] approach). Each treatment was considered to be continued if the patient received a new prescription for the same treatment within 30 days of the last date of the previous prescription. Treatment discontinuation was defined as the last prescription with no further prescription within 30 days. Censoring events were defined as events in which patients were no longer under the observation due to another antihypertensive medications. (i.e., patients in the ACE inhibitor group were considered censored if they were exposed to ARBs or thiazide diuretics).

Statistical analysis
A large-scale propensity score (PS) adjustment [16] was performed using L1 penalized logistic regression, which used > 10 000 baseline patient characteristics between each of the two cohorts, including all available demographic characteristics, as well as the diagnosis, medication, and procedure history in each database. All variables were dichotomized, and missing variables were considered absent. The study populations were matched using variable-ratio PS matching with a maximum ratio of 10 (caliper = 0.2). Differences between the two matched cohorts were considered negligible when the absolute standardized mean differences (aSMDs) of all covariates were < 0.1 [17]. The incidence rates (IRs) per 1000 person-years (PY) were estimated. Cox proportional hazard models were used to estimate the association between exposure and outcomes. Next, we performed empirical calibration of all hazard ratio (HR) estimates, their 95% confidence intervals (CIs), and their 2-sided P values by fitting an empirical null distribution to point estimates of falsification end points [18]. We identified a total of 26 falsification endpoints to quantify systematic error (eTable 1 in Supplement 1) [19, 20]. These outcomes are not known to cause differences between antihypertensive drugs, such as ingrowing nails and fractures of the upper limb. Using calibrated estimates, we performed a random-effects meta-analysis to calculate the summary HR and 95% CI of the pooling effect estimates across the databases. The Kaplan–Meier method and log-rank tests were used to derive the cumulative incidence and comparative risk between-group differences. Statistical significance was set at a pre-specified two-sided P value < 0.05. We followed the Strengthening the Reporting of Observational Studies in Epidemiology (STROBE) reporting guidelines.

Sensitivity analyses
Multiple sensitivity analyses were conducted using different definitions of the study population, outcomes, and follow-up strategies. To examine the association with late-onset schizophrenia as previously described [21], we sub-grouped the study population according to age over 45 years. We also varied our follow-up strategy to intention-to-treat (ITT) to estimate the effect of being assigned to a given treatment regardless of non-adherence. Overall, 16 different analyses (two cohort definitions (by age) × two outcome definitions × two follow-up strategies × two comparison pairs) were performed.
`

export const JSONCEEAMOS = {
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
        removeDuplicateSubjects: "keep first",
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
                minDaysAtRisk: 1 //default로 추가
            },
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 99999,
                endAnchor: "cohort start",
                minDaysAtRisk: 1 //default로 추가
            },
        ],
    },
    propensityScoreAdjustment: {
        psSettings: [
            {
                matchOnPsArgs: {
                    maxRatio: 10,
                    caliper: 0.2,
                    caliperScale: "standardized logit",
                },
                stratifyByPsArgs: null
            },
        ],
        //createPsArgs 전체 다 default로 추
        createPsArgs: {
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
    fitOutcomeModelArgs: { //modelType제외 default 추가
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
