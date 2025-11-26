export const TEXTRanitidineCancer =
    `
Methods
For this cohort study, each site received an institutional review board approval or obtained a waiver for the analysis of deidentified data according to institutional governance guidelines. The study is reported according to the Strengthening the Reporting of Observational Studies in Epidemiology (STROBE) reporting guideline.

Study Design and Data Sources
This federated international network cohort study was facilitated by the Observational Health Data Sciences and Informatics open science collaboration.11 The eligible individuals were identified by reviewing the routinely collected data in electronic health records and health claim data from the US, the UK, Germany, France, Spain, South Korea, and Taiwan. All data sources were standardized based on the Observational Medical Outcomes Partnership common data model, version 5.3.12 On the basis of a network of standardized databases, a series of distributed network analyses were conducted per previous studies.13,14 In accordance with a prespecified statistical analysis plan, an end-to-end R package was developed and distributed across the participating databases. This executable analytical software package is available elsewhere.15 For interpretation and database-level meta-analysis, only predesigned tabular statistical results from the data sources without patient-level information were shared with the coordinating centers. The predefined study protocol was registered in the European Union Post-Authorisation Studies Register.

The contributing data sources were the IQVIA US Ambulatory Electronic Medical Research (AmbEMR; US), Columbia University Irving Medical Center data warehouse (CUIMC; US), Stanford Medicine Research Data Repository (STARR; US), UK’s IQVIA Medical Research Data (IMRD; UK), IQVIA Disease Analyzer Germany (DA Germany; Germany), Information System for Research in Primary Care (SIDIAP; Spain), IQVIA Longitudinal Patient Database (LPD; France), Korean National Health Insurance System–National Sample Cohort (NHIS-NSC; South Korea), Ajou University School of Medicine (AUSOM; South Korea), Kandong Sacred Heart Hospital (KDH; South Korea), Hanyang University Medical Center (HUMIC; South Korea), and Taipei Medical University Clinical Research Database (TMUCRD; Taiwan). A detailed description of the databases is available in eAppendix 1 in Supplement 1.

Study Cohorts and Exposure
The study included adult patients aged 20 years or older who used ranitidine for more than 30 days with at least 1 year of exposure-free observation period prior to cohort entry. The comparator cohort was defined as adult patients who used other H2RAs (ie, famotidine, roxatidine, or lafutidine) with a 1-year washout period. Users of cimetidine and nizatidine were excluded from the primary comparator group, as previous studies have suggested that cimetidine may have anticancer effects16 and nizatidine is contaminated with NDMA.17,18 Patients with a history of cancer, exposure to other H2RAs for up to 1 year prior to cohort entry (for the target cohort), or ranitidine use for up to 1 year prior to cohort entry (for the comparator cohort) were excluded. Because of the use of ranitidine-containing combination drugs in Korea, patients exposed to sucralfate or bismuth within 1 month prior to the entry date were also excluded to minimize the imbalance between the ranitidine group and other H2RA groups.

The index date was defined as the date of H2RA treatment initiation. The end of the treatment duration was defined as the end of exposure to the drug of interest, allowing a 30-day gap between consecutive prescriptions. The study design is presented in eAppendix 2 in Supplement 1.

Outcomes
The primary outcome was the development of any cancer types, excluding nonmelanoma skin cancer. Secondary outcomes included the development of any cancer types (including nonmelanoma skin cancer and excluding thyroid cancer) or the 16 cancer subtypes analyzed separately (breast cancer; prostate cancer; lung cancer; colorectal cancer; bladder cancer; liver cancer; leukemia; pancreatic cancer; stomach cancer; lip, oral cavity, and pharynx cancer; thyroid cancer; corpus uteri cancer; ovary cancer; esophageal cancer; gall bladder and biliary tract cancer; and cervix uteri cancer). A list of the diagnostic codes used for outcome ascertainment is provided in eAppendix 3 in Supplement 1.

Statistical Analysis
Propensity score (PS) matching was performed to reduce potential confounding due to an imbalance in the baseline observed covariates between the target and the comparator cohorts. A large set of covariates was used to estimate the large-scale PSs, including age, sex, race, ethnicity, year and month of cohort entry, all recorded medications, medical history, procedures performed, and the Charlson Comorbidity Index score in the year prior to the index date. The information on race and ethnicity was derived from the databases. In AmbEMR, race was classified as Asian, White, and African American, with allowances for missing values; ethnicity was categorized as Hispanic or Latino, and Not Hispanic or Latino. Race and ethnicity were included in analysis because they may influence treatment decisions.

Comparator cohorts were constructed by performing 1:1 PS matching with a caliper of 0.2 SDs of the logic of the PS. Database-specific PSs were estimated using L1 regularized logistic regression tuned by 10-fold cross-validation.19,20 Cox proportional hazard models were fitted to estimate the hazard ratios (HRs) and 95% CIs according to exposure using the CohortMethod21 R package for each data source. A prespecified 2-sided P < .05 was considered significant.

For primary analysis, the result from each data source that passed the prespecified diagnostic criteria was aggregated using random-effects meta-analyses to calculate the summary HR.22 The study diagnostic criteria included empirical equipoise and sufficient balance of covariates after PS adjustment. Empirical equipoise was achieved if most patients’ preference scores (a transformation of the PS after adjusting for different prevalence of treatments) in both groups were between 0.3 and 0.7. If the absolute standardized mean difference of any covariate was greater than 0.1, the balance between the target and comparator cohorts was considered insufficient, and the analyses was excluded from primary meta-analysis.

Multiple sensitivity analyses were performed using different times of risk, definitions of the study population, outcomes, and statistical approaches. First, 4 different times of risk were defined: intention-to-treat (ITT), which followed patients until the end of data availability; ITT with a 1-year lag; during treatment, which followed people from 1 day after the index date until the completion of the treatment, allowing up to a 30-day gap between prescriptions; and during treatment with a 1-year lag. Second, 3 statistical models were applied in addition to the 1:1 PS matching: variable-ratio PS matching with a maximum ratio of 1:10, PS stratification into deciles, and unadjusted. The predefined setting for primary analysis was 1:1 PS matching with ITT and a 1-year lag. Third, negative control outcomes and empirical calibration were used to quantify and adjust for the impact of potential residual confounding due to unobserved covariates. Negative control outcomes were health events not causally associated with the target or comparator exposures, with an expected true HR equal to 1.23 A total of 119 negative control outcomes were considered (eAppendix 4 in Supplement 1). The comparative risks and CIs were empirically calibrated according to the empirical null distribution derived from the negative control outcomes.24 Fourth, for the analyses of the 18 secondary outcomes, statistical significance was reported after a Bonferroni correction to address multiplicity. Fifth, the interaction effects of cumulative drug dose were estimated.

All analyses were performed using R statistical software version 4.0.2 (R Project for Statistical Computing). Data were analyzed from April to September 2021.
`

export const JSONRanitidineCancer = {
    getDbCohortMethodDataArgs: {
        studyPeriods: [
            {
                studyStartDate: "",
                studyEndDate: "",
            },
        ],
        restrictToCommonPeriod: false, 
        firstExposureOnly: false, 
        washoutPeriod: 365, 
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
                riskWindowEnd: 99999,
                endAnchor: "cohort start",
                minDaysAtRisk: 1 //default로 설정
            },
            {
                riskWindowStart: 365,
                startAnchor: "cohort start",
                riskWindowEnd: 99999,
                endAnchor: "cohort start",
                minDaysAtRisk: 1 //default로 설정
            },
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 0,
                endAnchor: "cohort end",
                minDaysAtRisk: 1 //default로 설정
            },
            {
                riskWindowStart: 365,
                startAnchor: "cohort start",
                riskWindowEnd: 0,
                endAnchor: "cohort end",
                minDaysAtRisk: 1 //default로 설정
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
                stratifyByPsArgs: null, //default로 설정
            },
            {
                matchOnPsArgs: {
                    maxRatio: 10,
                    caliper: 0.2,
                    caliperScale: "standardized logit",
                },
                stratifyByPsArgs: null, //default로 설정
            },
            {
                matchOnPsArgs: null,
                stratifyByPsArgs: {
                    numberOfStrata: 10,
                    baseSelection: "all" //default로 설정
                },
            },
            {
                matchOnPsArgs: null,
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