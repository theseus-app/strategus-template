export const TEXTCOVID19PPIandH2RA =
  `
Methods
Data sources
A national claims database in South Korea that included COVID-19 testing data was used in this study [16]. The database was obtained from the Health Insurance Review and Assessment service (HIRA) which is the South Korean national institution for reviewing and assessing national health insurance claims. In response to the COVID-19 pandemic, HIRA collected data on COVID-19 testing by the reverse transcriptase polymerase chain reaction method from 1 January to 15 May, 2020. Notably, the collected data were converted into the Observational Medical Outcomes Partnership (OMOP) common data model (CDM), version 5, and released to the public. Hospitalization records were extracted for all patients involved in the study. This study was approved informed consent waiver by the institutional review board of the Kangdong Sacred Heart Hospital (no. 2020–04-001). All methods were carried out in accordance with relevant guidelines and regulations.

Study population and exposure
We identified patients aged over 18 years and diagnosed with COVID-19 disease. The cohort comprised patients prescribed acid-suppressants for 7 days or more. The index date was defined as the first day of drug treatment. The PPIs prescribed were defined as rabeprazole, pantoprazole, omeprazole, lansoprazole, ilaprazole, esomeprazole, and dexlansoprazole, while the H2RAs were defined as ranitidine, nizatidine, lafutidine, famotidine, and cimetidine. We excluded all patients prescribed any other primary or secondary medication(s) (i.e., PPIs and H2RAs) within 180 days before the index date. We defined the drug exposures as continuous exposure if the date gap between drug prescriptions was less than 30 days. Acid-suppressant non-users were defined as the patients who were not prescribed acid-suppressants and were not diagnosed with COVID-19 within 180 days before the index date.

Outcomes
The primary outcome was defined as diagnosis of COVID-19 and the secondary outcomes were defined as the complications of COVID-19, namely, (1) all-cause mortality, (2) hospitalization with at least one of the following diagnoses, i.e., pneumonia, acute respiratory disease syndrome (ARDS), sepsis, or acute kidney injury (AKI), and (3) requirement of intensive respiratory interventions such as mechanical ventilation, extracorporeal membrane oxygenation procedure (ECMO), or tracheostomy.

Statistical analyses
We used large-scale propensity score matching (PSM) with regularized logistic regression models to balance baseline characteristics of the study cohorts [17, 18]. Three different methods were used in the analysis, (1) propensity score unadjusted analysis, (2) one-to-four exact PSM with greedy nearest method, and (3) propensity score stratification with five strata [19]. The covariates included age, sex, all medication(s), medical procedure(s), disease history, and comorbidity index in the database. Cox proportional hazards regression models were used to estimate the association between exposures and outcomes. Patients were censored if they were no longer observable in the database. Data analyses were performed for three different cohort comparisons— (1) PPI users versus H2RA users, (2) PPI users versus non-users, and (3) H2RA users versus non-users.

During secondary analysis, only patients with a definite diagnosis of COVID-19 were included to measure complications due to COVID-19 disease among infected patients who had been prescribed acid-suppressive agents (i.e., PPIs and H2RAs). Moreover, we added the hospitalization criteria to investigate the clinical outcomes among COVID-19 patients with severe symptoms. Other analysis settings were identical to that used in primary analysis, i.e., PPI users, H2RA users, and non-users. Overall, six different cohort settings were applied in the secondary analysis (3 analyses among COVID-19 groups + 3 analyses among hospitalized COVID-19 groups).

Even though we utilized large-scale propensity score matching to balance between study groups and to minimize the unmeasured confounders, there still can be the residual bias in the observational studies [18]. To estimate the systematic error in the models, we employed 123 negative control outcomes to estimate systematic error in the models (Supplementary Table 1) [20, 21]. The negative control outcomes were found not to be affected by acid-suppressant use, hence, the negative control outcomes can show whether the model is influenced by unmeasured confounders or not. In this study, the final hazard ratio (HR) and 95% CIs were reported through empirical calibrations to adjust measured systematic errors from the analysis of 123 negative control outcomes [22].
`

export const JSONCOVID19PPIandH2RA = {
    getDbCohortMethodDataArgs: {
        studyPeriods: [
            {
                studyStartDate: "20200101",
                studyEndDate: "20200515",
            },
        ],
        restrictToCommonPeriod: false, 
        firstExposureOnly: true, 
        washoutPeriod: 180, 
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
        riskWindowEnd: 99999,
        endAnchor: "cohort start",
        minDaysAtRisk: 1 //default 설정
      }
    ],
  },
  propensityScoreAdjustment: {
    psSettings: [
        {
            matchOnPsArgs: null,
            stratifyByPsArgs: null,
        },
        {
            matchOnPsArgs: {
                maxRatio: 4,
                caliper: 0.2, //default로 설정
                caliperScale: "standardized logit" //default로 설정
            },
            stratifyByPsArgs: null,
        },
        {
            matchOnPsArgs: null,
            stratifyByPsArgs: {
                numberOfStrata: 5,
                baseSelection: "all" //default로 설정
            },
        },
    ],
    createPsArgs: { //laplace 제외하고 전부 default 설정
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
      }
    }
  },
  fitOutcomeModelArgs: { //modelType제외 default 설정
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
