export const TEXTStrokeRisk =
  `
METHODS
Data Sources
Data were obtained from the IBM MarketScan Medicare Supplemental Database (MDCR) that represents health services of retirees in the United States with employer-sponsored Medicare supplemental coverage through privately insured fee-for-service, point-of-service, or capitated health plans. The data include adjudicated health insurance claims including outpatient pharmacy dispensing claims coded with National Drug Codes, inpatient and outpatient medical claims including diagnosis codes coded in the International Classification of Diseases, Ninth Revision, Clinical Modification (ICD-9-CM) or ICD-10-CM, and procedure codes. The database was converted to the Observational Medical Outcomes Partnership (OMOP) Common Data Model (CDM). A database converted to the OMOP CDM is a transformation of the source structure and content to a standardized structure and vocabulary for encoding healthcare information (such as drug utilization and condition occurrences), which allows for consistent application of standardized analytics for research.19 As described by the Kaiser Family Foundation, employer-sponsored plans provide coverage to about 30% of traditional Medicare beneficiaries, or about 10 million persons. These beneficiaries tend to have both higher incomes and higher education levels, and are disproportionately white.20
The use of MDCR was reviewed by the New England Institutional Review Board and determined to be exempt from broad Institutional Review Board approval, as this research project did not involve human patient research.
Study Design and Study Population
This retrospective comparative cohort study included data available from January 1, 2001 through December 31, 2017 with the earliest cohort entry starting July 1, 2001, where prior data from 2001 contributed information on the conditions required for cohort inclusion. Thus, the start and end dates were similar to those of the Sentinel study (January 1, 2001–September 30, 2015).
The present study used databases that have been converted to the OMOP CDM. This model standardizes both the structure and content of data by mapping coding systems like ICD-9-CM to SNOMED-CT concepts by leveraging relationships that have been curated by the Unified Medical Language System.21 Typically, we define the cohorts in SNOMED-CT by walking up and down the comprehensive hierarchy. The relationships between SNOMED-CT and ICD-9-CM (and ICD-10-CM) dictate the codes that are included in the definition. In this case, we were limited to the ICD-9-CM codes used in the Sentinel study by Taylor et al.18 to define stroke. We used a similar approach and cross-walked from ICD-9-CM to SNOMED-CT. From there, we examined the relationships between SNOMED-CT and ICD-10-CM. We did not have a reference from Taylor et al.18 regarding the ICD-10-CM codes to be used and therefore relied on the mappings provided by the Unified Medical Language System. These ICD-10-CM codes were used for the incidence comparison (see Supplementary Figure 1). As these figures indicated a change in incidence estimates after September 30, 2015, we did a post-hoc analysis that was limited to the data through September 30, 2015 (see in the following sections).
New users of typical antipsychotics, haloperidol, or atypical antipsychotics who were ≥65 years old entered the study cohort when they first met all of the following conditions: Received a dispensing of a typical or atypical antipsychotic, had at least 183 days of continuous observation before that dispensing, during the 183 days up to and including the dispensing date had no diagnosis of stroke or cancer, and during the 183 days before that dispensing had received no dispensing of a typical or atypical antipsychotic. The dispensing date was the patient's index date. Patients who were exposed to typical and atypical antipsychotics on their index date were excluded.
The present study focused on two target cohorts (T1: new users of typical antipsychotics and T2: new users of haloperidol) and one comparator cohort (C1: new users of atypical antipsychotics). We compared the stroke risk in T1 cohort to the stroke risk in the C1 cohort, and the stroke risk in T2 cohort to the stroke risk in the C1 cohort. For each pairwise comparison, patients contributed “on-treatment” time at risk in the cohort for which they first qualified starting from the day after they entered that cohort until the first of: receiving a medication associated with the other cohort, having the study outcome (see in the following sections), having a gap of more than 30 days in the supply of the cohort-defining drug (more than 30 days from the end of the days’ supply one dispensing to the next dispensing), reaching the end of insurance enrollment, or reaching the end of the study. Stroke as the study outcome was defined by a diagnosis code for stroke (using the same code list as the Sentinel study18) as the primary condition in the insurance claim associated with an inpatient stay. The protocol was preregistered at https://clinicaltrials.gov/ct2/show/NCT04002700. The full protocol and an accompanying file of code lists used for identifying typical antipsychotics, haloperidol, atypical antipsychotics, and stroke are both publicly available at https://data.ohdsi.org/StrokeRiskInElderlyApUsers/.
Reimbursement coding requirements in the US changed from ICD-9-CM to ICD-10-CM on October 1, 2015 (see Supplementary Figure 1). We therefore calculated and visualized the yearly incidence rate per 1000 person-years of the stroke outcome in the database population. We observed a considerable decrease in the yearly incidence rate after 2015, reflective of the coding change and indicating that the ICD-9-CM codes used for stroke correspond to ICD-10-CM codes that identify a clinically different patient population. Because this change appeared likely to affect the incidence of stroke in the study population, we conducted a post-hoc analysis where the study end-date was redefined as September 30, 2015. That end-date for the post-hoc analysis was selected to reflect the end-date of the Sentinel study,18 whose end date coincided with the switch of many US health care databases from ICD-9-CM to ICD-10-CM.
Statistical Analysis
The crude incidence rate (IR) and incidence proportion (IP) of stroke were estimated within each exposure cohort and predefined gender subgroup. IRs were calculated as the number of patients with the outcome during each time-at-risk window divided by the total time-at-risk in years and were reported as IR/1000 person-years. IP was calculated as the number of patients with the outcome during each time-at-risk window divided by the total number of patients with time-at-risk and were reported as IP/1000 patients. Population-level effect estimation analyses were performed using a comparative cohort design including two pairwise comparisons: T1 versus C1 and T2 versus C1.
Propensity score (PS) matching was used to reduce potential confounding due to imbalance between the target and comparator cohorts in baseline covariates.22 Three PS adjustment approaches were used in the present study. First was the unadjusted (crude) estimate. Second was the Sentinel PS replication (Sentinel PS strategy), which involved 1:1 matching on a PS based on the 31 covariates used in the Sentinel study18 with a caliper of 0.05 on the PS scale. Third was the large-scale regularized regression model involving PS estimation (adapted PS strategy) using predicted probability from a large-scale regularized logistic regression model, fit with a Laplace prior (LASSO) using all observed covariates, including a recent diagnosis of dementia as input and 1:10 variable ratio matching with a caliper of 0.2 of the standard deviation of the logit of PS.23 Covariate balance was summarized by computing, before and after PS matching, the mean value and the associated standardized mean difference for each baseline covariate.
Negative control outcomes (outcomes that are not causally associated with any of the exposure cohorts) were used for empirical calibration. Positive control outcomes were based on these negative controls with the true effect size artificially increased to a desired effect size by addition of simulated outcomes. Using the negative and positive controls, a systematic error model was fitted against the control estimates and used to calibrate the hazard ratio (HR) and CI for the stroke estimate to account for potential unmeasured confounding.24
The HRs for the outcome during the time-at-risk was estimated by applying a Cox proportional hazards model conditioned on the PS matched sets. For each outcome model, the uncalibrated and empirically calibrated HR, 95% CI, and p-value25 were reported. For both cohorts being compared, patients were required to have at least 1 day of continuous observation after the time-at-risk start. The time-to-event of the outcome was determined by calculating the number of days from start of the time-at-risk window to the date of the outcome event.
`

export const JSONStrokeRisk = {
  getDbCohortMethodDataArgs: {
    studyPeriods: [
      {
        studyStartDate: "20010101",
        studyEndDate: "20171231",
      },
      {
        studyStartDate: "20010101",
        studyEndDate: "20150930",
      },
    ],
    restrictToCommonPeriod: false, 
    firstExposureOnly: true, 
    washoutPeriod: 183, 
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
        riskWindowEnd: 0,
        endAnchor: "cohort end",
        minDaysAtRisk: 1,
      },
    ],
  },
  propensityScoreAdjustment: {
    psSettings: [
      {
        matchOnPsArgs: null,
        stratifyByPsArgs: null
      },
      {
        matchOnPsArgs: {
          maxRatio: 1,
          caliper: 0.05,
          caliperScale: "propensity score",
        },
        stratifyByPsArgs: null
      },
      {
        matchOnPsArgs: {
          maxRatio: 10,
          caliper: 0.2,
          caliperScale: "standardized logit",
        },
        stratifyByPsArgs: null
      },
    ],
    createPsArgs: {
      maxCohortSizeForFitting: 250000, //default로 설정
      errorOnHighCorrelation: true, //default로 설정
      prior: {
        priorType: "laplace",
        useCrossValidation: true // default로 설정
      },
      control: { //control 전부 default로 설정
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
