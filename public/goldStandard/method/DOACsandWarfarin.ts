export const TEXTDOACsandWarfarin =
  `
2 Methods
2.1 Study Design
This retrospective, observational, comparative, new-user cohort study evaluated data from adult women who were newly exposed to DOACs or warfarin in a population of insured patients in the USA (Fig. 1). The study was pre-registered at ClinicalTrials.gov (NCT04394234), and the complete specification of all analyses is available in the protocol at https://github.com/ohdsi-studies/DoacsWarfarinSub/tree/master/Protocol.
The overall study period was from 19 October 2010 (coinciding with initial approval of dabigatran) to 31 December 2018. In any pairwise comparison, the study period was restricted to calendar time when both exposures (drugs) were observed in each data source.

2.2 Data Sources
This distributed database network study was executed using patient-level data from four US de-identified, administrative claims databases. All four databases have been standardized into the Observational Medical Outcomes Partnership Common Data Model (https://ohdsi.github.io/CommonDataModel/background.html) [13]. The databases included: (1) IBM MarketScan® Commercial Database (CCAE), a medical and drug insurance claims database of active employees, early retirees, Consolidated Omnibus Budget Reconciliation Act continuers, and their dependents insured by employer-sponsored plans; (2) IBM MarketScan® Multi-state Medicaid (MDCD), an administrative claims database containing the pooled healthcare experience of Medicaid enrollees from multiple states; (3) IBM MarketScan® Medicare Supplemental Beneficiaries (MDCR), an administrative health claims database for Medicare-eligible active and retired employees and their Medicare-eligible dependents from employer-sponsored supplemental plans; and (4) Optum’s Clinformatics® Data Mart Database (Optum), an administrative health claims database for members who are fully insured in commercial plans or in administrative services only and commercial Medicare. Analyses of de-identified publicly available data do not constitute human subjects research and, as such, do not require institutional review board review/approval.

All analyses were performed independently within each of these four databases and no patient-level data were pooled across the databases for any analysis. Instead, meta-analysis estimates were computed based on per-database aggregate statistics as described below.

2.3 Study Population
Cohorts of mutually exclusive, adult (aged ≥ 18 years) female new users of rivaroxaban, apixaban, dabigatran, and warfarin with ≥ 183 days of prior continuous database observation were identified. The date of new anticoagulant dispensing was the index date. Patients were excluded for edoxaban exposure; exposure to other drugs of interest (e.g., DOAC users were excluded for the warfarin exposure cohort); hysterectomy; vaginal bleed; and medical, surgical, or transfusion management for vaginal bleeding any time prior to or on the index date. From these base exposure populations, additional inclusion criteria were added to construct new user cohorts with the following prior indications with consideration of the number of days prior to and including the index date: (1) AF population: AF diagnosis any time prior and no VTE in the past 183 days and no total knee or hip replacement surgery in the past 35 days; and (2) VTE population: VTE in the past 183 days, no AF diagnosis any time prior, and no total knee or hip replacement surgery in the past 35 days. All inclusion criteria are based on information referenced to the index date, such that “any time prior” means all observed time before and including the index date. In total, 12 new user cohorts were constructed for the three indications across four treatments. New user cohort definition details are available in the protocol Section 7.3 including code sets in Annex A and an example in Annex B (https://github.com/ohdsi-studies/DoacsWarfarinSub/tree/master/Protocol/Tables_Annex).

2.4 Outcome Assessment
Patients with severe uterine bleeding were identified by having vaginal bleeding with a blood transfusion on the same day or surgical management on or within the subsequent 60 days, based on diagnostic and procedure codes recorded. Outcome ascertainment details are available in the protocol Section 7.4 and codes for the outcome definitions are available in Annex A (https://github.com/ohdsi-studies/DoacsWarfarinSub/tree/master/Protocol/Tables_Annex). Cases were identified based on patients’ records provided by the source data and no additional case adjudication was performed.

Time-at-risk is the interval relative to the index date during which the outcome can be ascertained. We defined time-at-risk three ways in the study. The primary on-treatment time-at-risk period was defined as the time from 1 day after the anticoagulant index date to the end of inferred persistent exposure, the last day of observation, or the end of the study period, whichever came first. For DOACs, the final exposure record allowed for no more than a 3-day gap between successive exposure intervals (inferred by days’ supply) plus 5 days appended to the last exposure date. This definition was intended to reflect the short half-life (approximately 12 h) of DOACs. For warfarin, the final exposure record allowed for no more than a 7-day gap between successive exposure intervals (inferred by days’ supply) plus 5 days appended to the last exposure date. This definition was intended to reflect the longer half-life (20–60 h) of warfarin.

Two sensitivity analyses were conducted using variations of time-at-risk periods to provide an increased time to observe severe uterine bleeding events. In the first sensitivity analysis, the on-treatment time-at-risk period was similarly defined as in the primary analysis, but the final exposure record allowed for no more than a 30-day gap between successive exposure intervals (inferred by days’ supply) with no days appended to the last exposure date for DOACs and warfarin. This definition was intended to reflect the dispensing behavior in routine clinical practice. The second sensitivity analysis was an intent-to-treat (ITT) analysis, defined as the time from 1 day after the anticoagulant index date until the last day of observation or the end of the study period, whichever came first, regardless of the subsequent drug dispensing records. All time-at-risk period definitions required that patients have at least 1 day of time-at-risk after the index date.

2.5 Statistical Analysis
The unadjusted (crude) incidence rates (IRs) of severe uterine bleeding within each exposure cohort were reported and calculated as the number of persons with the outcome during each time-at-risk period, divided by the total time-at-risk (IR/1000 PY). Incidence proportion, defined as the number of persons with an outcome during the time-at-risk period divided by the total number of persons at risk, was also calculated. The number of severe uterine bleeding events observed during each time-at-risk period, total time-at-risk in years, and the IR and incidence proportion in the exposure cohorts for DOACs and warfarin were reported for each indication population in each database. The crude IRs do not reflect any statistical adjustment and are not intended for comparative purposes.

For population-level effect estimation, all pairwise comparisons of DOACs (rivaroxaban, apixaban, dabigatran) and warfarin were assessed nested within each of the two indication groups (AF or VTE). Propensity scores (PSs) were used to reduce potential confounding due to an imbalance of observed patient characteristics at baseline and were calculated for each patient using the predicted probability from a regularized logistic regression model, fit with a Laplace prior (LASSO) and the regularization hyperparameter selected by optimizing the likelihood in a tenfold cross validation [14,15,16]. The primary PS strategy matched the target to comparator patients in a 1:1 ratio and a sensitivity PS strategy variably matched the target to comparator patients in a maximum of a 1:100 ratio. Both approaches used a greedy matching algorithm with a caliper of 0.2 of the standard deviation on the logit scale of the PS distribution. Covariates used as input to the PS model included all available baseline covariates on demographics; prior conditions (including possible confounders such as cardiovascular comorbidities, polyp of cervix, endometriosis, other cancers, and pregnancy); medication use (including hormonal therapy drugs); procedures; and the risk scores, including the Charlson Comorbidity Index and the stroke risk index (CHA2DS2-VASc), during the 183 days prior to or on the index date. Propensity score matching was evaluated based on a covariate balance before and after PS matching and considered successful when all standardized differences in means were < 0.1 [17]. Study diagnostics (PS distribution, covariate balance, and empirical null distribution) for the primary analysis of each pairwise comparison for each database in AF and VTE populations are reported in the Electronic Supplementary Material (ESM). Because of limited dabigatran exposures in VTE, no formal comparisons were performed.

Cox proportional hazards regression models were used to model the time to the first outcome occurrence for the target relative to the comparator cohort in pairwise comparisons while accounting for the baseline covariate imbalance using the two PS matching strategies (1:1, unconditional; and 1:100, conditional) [18, 19]. Empirical calibration using 127 negative control outcomes (ESM) was performed to account for residual error unaddressed by PS matching [20, 21]. In addition to negative control outcomes, we also generated and included synthetic positive control outcomes. Positive control outcomes were developed on the basis of the negative controls, but where the effect size was artificially increased to a predefined effect size by injection of additional simulated outcomes [21]. For each indication, estimates of severe uterine bleeding risk were reported as the empirically calibrated hazard ratio (cHR) with associated 95% calibrated CIs and p values. For each indication-target-comparator-analysis combination, heterogeneity of the HRs across the four databases was estimated, using I2 as a metric [22]. When there was sufficient homogeneity across sources (I2 < 40%) [23], database-specific estimates were pooled through a random effect meta-analysis using the Hartung–Knapp–Sidik–Jonkman inverse-variance method [24]. Pooled results including p values corrected for multiple testing using Hochberg’s step-up procedure are provided. We conducted this study using open-source software developed by the OHDSI community, CohortMethod (https://ohdsi.github.io/CohortMethod/) and Cyclops (https://ohdsi.github.io/Cyclops/) [25].
`

export const JSONDOACsandWarfarin = {
    getDbCohortMethodDataArgs: {
        studyPeriods: [
          {
            studyStartDate: "20101019",
            studyEndDate: "20181231",
        },
        ],
        restrictToCommonPeriod: false, 
        firstExposureOnly: true, 
        washoutPeriod: 0, 
        removeDuplicateSubjects: "remove all",
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
        riskWindowEnd: 5,
        endAnchor: "cohort end",
        minDaysAtRisk: 1 //default 설정
      },
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
                caliper: 0.2, //default로 설정
                caliperScale: "standardized logit" //default로 설정
            },
            stratifyByPsArgs: null,
        },
        {
            matchOnPsArgs: {
                maxRatio: 100,
                caliper: 0.2, //default로 설정
                caliperScale: "standardized logit" //default로 설정
            },
            stratifyByPsArgs: null,
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
