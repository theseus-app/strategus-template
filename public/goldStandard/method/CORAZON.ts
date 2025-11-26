export const TEXTCORAZON =
    `
Methods
Data Sources
This study used anonymized patient records from 5 electronic health databases in the distributed data network of the Observational Health Data Science and Informatics (OHDSI) program (12), an open-science, international, and interdisciplinary collaborative (12). All community members in OHDSI were invited to run the analyses and return the results for this study (13). In the end, IQVIA provided 5 electronic health databases from 4 countries: France (Longitudinal Patients Database France), Germany (Disease Analyzer Germany), the United Kingdom (U.K. IQVIA Medical Research Data), and the United States (U.S. Ambulatory Electronic Medical Records and U.S. Hospital Charge Data Master). The databases comprised 221 million people across primary care, outpatient, and hospital settings. Information, including demographics, drug prescriptions, and diagnosis records, is prospectively recorded in the databases as part of the routine clinical care of patients. All databases are standardized to the Observational Medical Outcomes Partnership Common Data Model (version 5) (14). The databases are quality-controlled for research purposes (15) and have been extensively used for high-quality and large-scale multinational drug surveillance studies (16–20). Details about the databases are given in Part 1 of the Supplement and previous publications (16–20). The data partner has obtained institutional review board approval (for U.K. IQVIA Medical Research Data) or exemption (for all other databases) for their participation in this study.
Study Design
This study used a new-user, active-comparator cohort design. We specified head-to-head target trials for each pairwise comparison of DOACs: apixaban versus dabigatran, apixaban versus rivaroxaban, apixaban versus edoxaban, dabigatran versus rivaroxaban, dabigatran versus edoxaban, and rivaroxaban versus edoxaban. The following sections and Part 2 of the Supplement describe the protocol components.
Eligibility Criteria
Eligible patients had AF, were aged 18 years or older, and had never used the DOAC pairs of interest. Patients were required to have at least 1 year of observation before the index date in the database to record medical history. To identify those with AF, we required patients to have a diagnosis of AF any time on or before the index date or within 90 days after the index date to account for any delay in recording the AF diagnosis. We excluded patients with a history of mitral stenosis, hyperthyroidism, or mechanical heart valve replacement (among whom DOACs might be contraindicated) or transient AF (that is, those who had undergone cardiac surgery or were diagnosed with myocarditis, pericarditis, or pulmonary embolism). Other exclusion criteria included a prescription of warfarin or other DOACs on or within 180 days before the index date; a prescription of another oral anticoagulant (other than the index anticoagulant) on the index date; and a history of an outcome of interest, to avoid its residual effects on future outcome events, which are difficult to control for in observational studies (Figure 1). The phenotype codes for clinical conditions, procedures, and drugs used in the study were compiled using a sequence of quality control procedures in the databases (Part 3 of the Supplement) and are listed in the study protocol and repository (21).
Treatment Groups and Follow-up
For each head-to-head comparison, patients were classified into a DOAC group based on their first prescription of a DOAC between 1 January 2010 (2012 for Longitudinal Patients Database France) and 31 December 2019. The index date was defined as the date of the first prescription. Patients were followed from the index date until the occurrence of the study outcome, treatment discontinuation (allowing for 90-day gaps between consecutive prescriptions, with the date of treatment discontinuation being the end date of the last prescription [the “on-treatment” approach]), switching from the index medication to another oral anticoagulant (apixaban, dabigatran, edoxaban, rivaroxaban, or warfarin), death, or the end of the study period (31 December 2019), whichever came first. For the databases with no death date available (Longitudinal Patients Database France and U.S. Ambulatory Electronic Medical Records), we used the date of last consultation instead of the date of death for censoring.
Outcomes
The following 4 outcomes were of interest: a composite of ischemic stroke and systemic embolism, intracranial hemorrhage (ICH), gastrointestinal bleeding (GIB), and all-cause mortality (available in Disease Analyzer Germany, U.K. IQVIA Medical Research Data, and U.S. Hospital Charge Data Master). The outcomes were identified on the basis of published code lists (Part 3 of the Supplement).
Statistical Analysis
To address any potential bias due to nonrandomized treatment allocation, we used propensity score modeling to compare patients who differed in anticoagulant treatment but were similar in other measured characteristics (22). The propensity score is defined as the probability of receiving the targeted treatment given the observed patient characteristics. We developed large-scale propensity score models for each comparison and database using a consistent, data-driven process through regularized logistic regression, which used a large set (>90 000) of predefined baseline patient characteristics (including age, sex, and other demographics); a unique identifier for care site (such as practice or hospital); and previous medical conditions, drug exposures, procedures, and health service use behaviors to provide the most accurate prediction of treatment and balance the patient cohorts across many characteristics (17, 23). All covariates were identified within the 365 days before and including the index date. The regularization propensity score method has been widely used for variable selection and confounding adjustment (16, 18, 20) and has consistently demonstrated equal or superior performance to traditional investigator-specified or high-dimensional propensity score approaches in both actual and simulation studies (23, 24).
Patients were stratified into 5 strata based on propensity score to estimate the average treatment effect. Standardized differences were used to assess the differences in patient characteristics between treatment groups before and after propensity score stratification. Proposed cutoffs for acceptable standardized differences range from 0.1 to 0.25 (3). Cox proportional hazards regression conditioned on the propensity score strata was applied to estimate the hazard ratio (HR) of the risk for outcomes in every pairwise DOAC comparison in each database. The HRs were pooled across the databases in a meta-analysis using a random-effects model.
In observational studies, residual bias could remain despite control for measured confounding through propensity scores. Therefore, to further reduce bias from unmeasured and systematic sources, we did empirical calibration of CIs (25, 26). For this, we used a data-rich algorithm (27) to identify 49 negative control outcomes (that is, events that are not known to be associated with DOAC use and thus have a null effect size) to construct an empirical null distribution and quantify systematic error (25, 26) (Supplement Figures 1 to 3). We then incorporated the error observed for negative controls into our results to take into account both systematic and random errors in the study. The full list of negative control outcomes is in Supplement Table 1.
In subgroup analyses, we restricted the analyses to patients who initiated a standard-dose regimen of DOACs (that is, apixaban, 5 mg twice daily; dabigatran, 150 mg twice daily; edoxaban, 60 mg once daily; or rivaroxaban, 20 mg once daily) and to those who initiated a reduced-dose DOAC regimen (that is, apixaban, 2.5 mg twice daily; dabigatran, 110 mg twice daily in Europe or 75 mg twice daily in the United States; edoxaban, 30 mg once daily; or rivaroxaban, 15 mg once daily). Additional analyses were done for the following 2 important subgroups that are often underrepresented in clinical trials: patients who were aged 80 years or older at cohort entry and patients with chronic kidney disease at cohort entry. Chronic kidney disease was defined as having a diagnosis of chronic kidney disease or a dialysis procedure, an algorithm used in the previous study in OHDSI (16). Part 4 of the Supplement presents all statistical analysis details.
We did additional sensitivity analyses in which the time at risk was not censored if a patient discontinued the index medication therapy or switched to another anticoagulant (analogue to the “intention-to-treat” approach). We also repeated our analyses using propensity score matching at a variable-matching ratio as sensitivity analyses to estimate the average treatment effect on treated patients (28). Overall, we specified 480 analyses per database (6 DOAC comparisons × 4 outcomes × 5 groups × 2 propensity score approaches × 2 time-at-risk definitions). For clarity, we present the result estimates from the on-treatment, propensity score stratification analyses here. The complete set of results is in the Supplement and an interactive website (https://data.ohdsi.org/corazon) (Part 5 of the Supplement).
All analyses were done using the R programming language, version 3.5.1 (R Foundation). The analysis packages were built on the open-source OHDSI CohortMethod R package and the Cyclops R package (21). The study protocol and all statistical analysis packages were prespecified before analysis execution. The study protocol and analysis codes are publicly available to enhance the transparency and reproducibility of the results (21). This study followed the STROBE (Strengthening the Reporting of Observational Studies in Epidemiology) reporting guideline.
`

export const JSONCORAZON = {
    getDbCohortMethodDataArgs: {
        studyPeriods: [
            {
                studyStartDate: "20100101",
                studyEndDate: "20191231",
            },
            {
                studyStartDate: "20120101",
                studyEndDate: "20191231",
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
        removeSubjectsWithPriorOutcome: true,
        priorOutcomeLookBack: 99999,
        timeAtRisks: [
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 0,
                endAnchor: "cohort end",
                minDaysAtRisk: 1 //default 추가
            },
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 99999,
                endAnchor: "cohort start",
                minDaysAtRisk: 1 //default 추가
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
                },
            },
            {
                matchOnPsArgs: {
                    maxRatio: 0,
                    caliper: 0.2, //default 설정
                    caliperScale: "standardized logit" //default 설정
                },
                stratifyByPsArgs: null
            },
        ],
        createPsArgs: { //createPsArgs default로 추가 
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
