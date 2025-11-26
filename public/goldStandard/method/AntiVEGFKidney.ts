export const TEXTAntiVEGFKidney =
  `
Methods
Study Design
This was a retrospective cohort study across 12 databases (6 administrative claims and 6 electronic health records) standardized to OHDSI’s OMOP CDM.22 In this CDM, local clinical concepts (e.g., International Classification of Diseases diagnosis codes or Current Procedural Terminology codes) are mapped to OMOP concepts through an extract-transform-load process. The OMOP CDM normalizes the structure and content of source data, which allows disparate health care databases to be queried in a standardized manner. Database details are included in the appendix (Table S1, available at www.ophthalmologyretina.org). All data partners had local institutional review board approval or exemption for their participation. The study adhered to the tenets of the Declaration of Helsinki and complied with Health Insurance Portability and Accountability Act.
Subjects/Exposure
Adult patients aged ≥ 18 years who were newly treated with monthly intravitreal anti-VEGF medications (ranibizumab, aflibercept, or bevacizumab) for ≥ 3 months for a blinding disease with ≥ 365 days of prior observation were included in the study. Blinding diseases included diagnoses for diabetic retinopathy, diabetic macular edema, exudative age-related macular degeneration, and retinal vein occlusion. The minimum number of anti-VEGF exposures was chosen based on prior studies demonstrating suppression of systemic plasma VEGF levels after 3 monthly intravitreal injections.10,11 Patients with cancer diagnoses (e.g., metastatic colorectal cancer) that could warrant systemic anti-VEGF were excluded. Patients with preexisting kidney failure (defined further below) were also excluded. If patients switched anti-VEGF medications, only the first exposure was included in the analysis, after which the patients were right-censored from the cohort. Cohorts were created using ATLAS, a publicly available web-based software application developed by the OHDSI community.23 Details of the cohorts and the codes used to define each phenotype can be found in Tables S2 and S3 (available at www.ophthalmologyretina.org).24,25
Time at Risk
Patients were assumed at-risk of kidney failure after the third anti-VEGF exposure until the end of continuous drug exposure or the end of the study period. The end of continuous exposure was defined as a gap of > 180 days between injections. The cutoff of 180 days was chosen because the need for anti-VEGF typically decreases over time. In the DRCR Retina Network’s clinical trials for treatment of diabetic macular edema, a median of 8 to 9 ranibizumab injections are given in the first year and that decreases to 2 to 3 in the second year across most studies.26 The cutoff of 180 days would categorize most patients with diabetic macular edema being treated per the DRCR protocol as having a “continuous” drug exposure in the first 2 years of treatment.
Outcome
The outcome of interest was kidney failure, or end-stage kidney disease in which the operational definitions were based on Kidney Disease Improving Global Outcomes stage 5 chronic kidney disease (CKD) treated with dialysis or kidney transplantation.27,28 Based on our previous research, we implemented and tested 2 versions of the kidney failure phenotype.29 The first kidney failure phenotype definition was aligned with Kidney Disease Improving Global Outcomes stage 5 CKD and was defined as having either a condition, observation, or procedure code related to end-stage kidney disease or kidney transplantation, 2 measurements of estimated glomerular filtration rate of < 15 ml/min/1.73m2 at least 90 days apart, or 2 observation or procedure codes of kidney dialysis spaced at least 90 days apart. Table S2 and S4 (available at www.ophthalmologyretina.org) contain the logic and full list of codes used to represent the definition. The second kidney failure phenotype definition used an administrative code-only limited definition that included the presence of a condition, observation, or procedure related to end-stage kidney disease. (Tables S2 and S4) The sensitivity, specificity, positive predictive value, and negative predictive value of the performance of each kidney failure definition was evaluated on a subset of databases using PheValuator, an R package for evaluating phenotype algorithms.29,30 In brief, PheValuator uses machine learning to develop predictive models that serve as a probabilistic reference standard, computes the probability of having a disease of interest for each subject in the cohort, and uses that result to determine the rule-based phenotype performance metrics including sensitivity, specificity, positive predictive value, and negative predictive value. Based on the output from PheValuator, it was determined that the second kidney failure phenotype had better positive predictive value with similar sensitivity to the “complex” definition; thus, the simpler kidney failure phenotype was used in this study (Table S5, available at www.ophthalmologyretina.org).
Statistical Analysis
All analyses were performed in R using the Strategus execution pipeline.31 Strategus is a standardized method to call Health Analytics Data-to-Evidence Suite open-source R packages developed by the OHDSI community for large-scale analytics.32,33 The prespecified study protocol and end-to-end open executable source code are available on GitHub.24 We developed an interactive website to promote transparency and allow for sharing and exploration of the results.34
(A). Incidence of Kidney Failure
Summary statistics were calculated to describe the baseline characteristics of patients in each anti-VEGF exposure cohort by database. These characteristics included demographic information (age, sex, race, and ethnicity), blinding diseases (retinopathy due to diabetes mellitus, retinal vascular occlusion, age-related macular degeneration, and macular retinal edema), kidney diseases (renal impairment, CKD, chronic disease of genitourinary system, and kidney disease), Diabetes Comorbidity Severity Index score, and Charlson Comorbidity Index-Romano adaptation.35,36 The baseline characteristics of patients were also stratified by the outcome of kidney failure. Differences between the groups were represented by the standardized mean difference and significant differences reported using a t test.37
The incidence proportion (number of outcomes divided by the total number of people at risk) and incidence rate (number of outcomes during the time-at-risk divided by the number of total person days) of kidney failure were calculated for each exposure cohort. Only kidney failure events while on treatment contributed to this analysis. Crude incidence proportions for each year were standardized to the 2015 US population by age and sex using direct standardization and then averaged.38,39
(B). Comparative Risk of Kidney Failure
A series of study diagnostics were performed and the meta-analysis of comparative risks only included the results from databases that passed all evaluations. The study diagnostics are described in detail elsewhere and the thresholds used in this study are located in the study GitHub.22,40 We used the large-scale propensity score method to match patients in each target and comparator exposure cohort comparison (aflibercept vs. ranibizumab, bevacizumab vs. ranibizumab, and bevacizumab vs. aflibercept) using 1:1 propensity score matching. The propensity score model included a large number of baseline covariates (e.g., demographic characteristics, preexisting conditions, measurements, and procedures) as potential confounders and used the L1-regularization technique to avoid model overfitting.41,42 The outcome was time from cohort entry to kidney failure while on treatment. Cox proportional hazards models were used to estimate the risk of kidney failure while on treatment. A random effects meta-analysis was performed to combine per site hazard ratio (HR) estimates into a single network-wide estimate.43
Sensitivity Analysis to Methodological Choices
Analyses were done to test the sensitivity of our design to methodological choices. In the main analysis, only kidney failure events that occurred while the patient was on treatment with intravitreal anti-VEGF contributed to the calculations. As a sensitivity analysis, all calculations were repeated under an intent-to-treat design, in which all kidney failure events after cohort entry contributed to the calculations even if they occurred while the patient was not receiving intravitreal anti-VEGF. There was also no right-censoring; patients who switched anti-VEGF medications were analyzed according to the first anti-VEGF they received. The intent-to-treat sensitivity analysis was chosen for several reasons. Because kidney failure is a long-term outcome, potential adverse kidney impacts of the intravitreal anti-VEGF could be cumulative and manifest years after medication exposure. Furthermore, it is possible that patients who experienced an adverse kidney failure, for example proteinuria, were preferentially switched to another anti-VEGF medication or stopped therapy altogether, thus biasing the main on-treatment study design. Because there was no right-censoring in the sensitivity analysis, patients who switched or stopped anti-VEGF were still included in the intent-to-treat analysis. The sensitivity analysis was performed on a subset of databases (Table S1).
`

export const JSONAntiVEGFKidney = {
    getDbCohortMethodDataArgs: {
        studyPeriods: [
          {
            studyStartDate: "",
            studyEndDate: "",
        },
        ],
        restrictToCommonPeriod: true, 
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
        minDaysAtRisk: 1 //default 설정
      },
      {
        riskWindowStart: 1,
        startAnchor: "cohort start",
        riskWindowEnd: 99999,
        endAnchor: "cohort start",
        minDaysAtRisk: 1 //default 설정
      },
    ],
  },
  propensityScoreAdjustment: {
    psSettings: [
      {
        matchOnPsArgs: {
          maxRatio: 1,
          caliper: 0.2, //default 설정
          caliperScale: "standardized logit" //default 설정
        },
        stratifyByPsArgs: null
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
