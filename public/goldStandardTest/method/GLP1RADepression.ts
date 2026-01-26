export const TEXTGLP1RADepression = `
Target Trial
We developed a hypothetical pragmatic trial (the target trial) to evaluate the effects of GLP-1RAs, compared with SGLT2is or DPP4is, on risk for depression. Table 1 summarizes the key components of this target trial. Eligible participants would be those aged 66 years or older with a diagnosis of T2D who have no prior diagnosis of type 1 diabetes, depression, or another mood disorder; no use of antidepressants; no contraindication to study drugs (end-stage renal disease or dialysis); and no use of GLP-1RAs, SGLT2is, or DPP4is within the past year. These broad inclusion criteria allow us to evaluate the real-world effectiveness and safety of GLP-1RAs across a wide spectrum of patients with T2D and increase the generalizability of the findings to clinical practice.
The participants would be randomly assigned to GLP-1RAs, SGLT2is, or DPP4is and would be aware of their assigned strategy. The primary end point would be the onset of depression. The causal contrast of interest would use the intention-to-treat effect. Participants would be followed from treatment assignment until the onset of depression, death, loss to follow-up, up to 2 years of follow-up, or the end of the study (31 December 2020), whichever came first.
Kaplan–Meier survival analysis of depression up to 2 years would be used to estimate the survival curves, and Cox proportional hazards regression models would be used to estimate hazard ratios (HRs) with 95% CIs by comparing participants assigned to GLP-1RAs versus SGLT2is and GLP-1RAs versus DPP4is. Their 6-month and 2-year risk differences and risk ratios with 95% CIs would be estimated.
Potential effect modification of GLP-1RAs on risk for depression would be evaluated in subgroups based on the following characteristics: age (≥75 vs. <75 years), sex (female vs. male), race and ethnicity (non-Hispanic White population vs. non-Hispanic Black population vs. Hispanic population vs. others), having a diagnosis of obesity at baseline (yes vs. no), insulin use at baseline (yes vs. no), treatment duration (<0.5 year vs. ≥0.5 year and <1 year vs. ≥1 year), and molecular structure of GLP-1RA (exenatide, dulaglutide, liraglutide, or semaglutide).

Target Trial Emulation
We emulated the target trial using U.S. national Medicare administrative claims data between January 2013 and December 2020 (5% random sample from January 2013 to December 2020 and 15% random sample from January 2016 to December 2020), accessed through the University of Florida. Medicare, the national health insurance program in the United States, primarily provides coverage for individuals aged 65 years and older. The claims data encompassed inpatient hospitalizations (Part A), outpatient services (Part B), and prescription drug claims (Part D), providing a comprehensive longitudinal record of health care use, including demographics, diagnoses, procedures, and pharmacy dispensing information.
We emulated the eligibility criteria of the target trial. Eligible patients should have a diagnosis of T2D identified using International Classification of Diseases codes that have been validated in Medicare claims data, yielding a positive predictive value from 71% to 96% (28). We compared patients initiating treatment with a GLP-1RA versus an SGLT2i or DPP4i between January 2014 (following the first SGLT2i approval in 2013) and December 2020. To obtain baseline covariates, we required patients to have at least 1 year of continuous enrollment in Medicare Parts A, B, and D before treatment initiation. We included patients aged 66 years or older. We excluded patients with a prior diagnosis of type 1 diabetes, end-stage renal disease (or receipt of dialysis), depression, or another mood disorder, as well as those who had received antidepressants in the previous year. We also excluded patients who concomitantly used GLP-1RAs and the comparator on the index date.
Among those eligible, patients were nonrandomly assigned to a GLP-1RA, SGLT2i, or DPP4i on the basis of their initial prescription for the respective drug. The index date (or “time zero”) was defined as the date when an eligible patient first received a prescription for any of these drugs. We chose SGLT2is and DPP4is as the active comparators to mitigate confounding by indication (or disease severity) based on the following reasons. First, during the study period, GLP-1RAs, SGLT2is, and DPP4is were all widely prescribed and recommended as second-line treatments of T2D, making SGLT2is and DPP4is comparable alternatives to GLP-1RAs (25, 26). Second, SGLT2is have a similar level of recommendation to GLP-1RAs, particularly for those with established or high risk for cardiovascular disease (25). Finally, DPP4is share similar mechanisms with GLP-1RAs, with both working through the incretin pathway (27), making DPP4is an appropriate comparator for evaluating the efficacy and safety profile of GLP-1RAs. To further ensure that the treatment groups were comparable and at a similar disease stage, we matched patients on demographic and clinical characteristics, as well as on baseline GLD regimen. This included matching on use of metformin, sulfonylureas, thiazolidinediones, and insulin, as well as on the total number of GLDs used at baseline (insulin use vs. no GLD use [excluding insulin] vs. 1 GLD [excluding insulin] vs. ≥2 GLDs [excluding insulin]). Exposures of interest and codes for inclusion and exclusion criteria are presented in Supplement Tables 1 and 2.
Consistent with the target trial, the study outcome was incident depression, identified as 1 or more diagnosis codes according to the chronic condition algorithm developed by the Chronic Conditions Data Warehouse (Supplement Table 2). We used an intention-to-treat analysis, the same as for the target trial, which refers to analysis based on the initial treatment assignment regardless of whether the patient received the treatment or adhered to the protocol. The patients were followed from the index date until the diagnosis of depression; death; disenrollment from Medicare Part A, B, or D; up to 2 years of follow-up; or the end of the study (31 December 2020), whichever came first. We chose this approach to minimize potential biases, such as maintaining the balance of confounders between treatment groups and accounting for nonadherence or protocol deviations, and to enhance the robustness and reliability of our findings by providing a more accurate reflection of the real-world effectiveness of the treatments under study (29).

Statistical Analysis
The statistical analyses were the same as for the target trial except that 1:1 propensity score (PS) matching was used to mitigate potential confounding. The PS was estimated using multivariate logistic regression models incorporating baseline covariates. The baseline covariates included demographic characteristics (such as age, sex, and race and ethnicity), comorbid conditions (such as obesity and chronic kidney disease), use of other GLDs (such as metformin and insulin), and use of non–diabetes-related medications (such as antihypertensives and statins), as outlined in Table 2. We applied nearest-neighbor matching without replacement, using a maximum caliper width of 0.05, to construct the matched cohort (30). The balance of baseline covariates before and after PS matching was assessed using standardized mean differences, with a value less than 0.1 indicating negligible imbalance (31).
To test the robustness of the study findings, we used a Fine–Gray subdistribution hazard model to address the competing risk for death (32), which allows a more accurate estimation of the cumulative incidence of the outcome in the presence of a competing event (death) that precludes the occurrence of the event of interest. All statistical analyses were done using SAS, version 9.4 (SAS Institute), and the survival curves were produced using R software (R Foundation), package “survminer.” A P value less than 0.05 was considered statistically significant.

Role of the Funding Source
The National Institute of Diabetes and Digestive and Kidney Diseases had no role in the design, conduct, or analysis of the study or in the decision to submit the manuscript for publication.
`

export const JSONGLP1RADepression = {
  getDbCohortMethodDataArgs: {
    studyPeriods: [
      {
        studyStartDate: "20130101",
        studyEndDate: "20201231",
      }
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
        riskWindowEnd: 730,
        endAnchor: "cohort start",
        minDaysAtRisk: 1 //default 설정
      }
    ],
  },
  propensityScoreAdjustment: {
    psSettings: [
      {
        matchOnPsArgs: {
          maxRatio: 1,
          caliper: 0.05, //default 설정
          caliperScale: "standardized logit" //default 설정
        },
        stratifyByPsArgs: null
      }
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
