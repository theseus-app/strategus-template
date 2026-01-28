export const TEXTSGLT2iMetformin = `
Study Data
We used data from 2 large commercial U.S. health insurance databases—Optum Clinformatics Data Mart and
IBM MarketScan—and Medicare fee for service. The
commercial databases included persons with employersponsored health insurance or a Medicare Advantage
insurance plan across the United States. The Medicare
database included beneficiaries aged 65 years and
older. These databases contained deidentified individual-level, longitudinal information on demographics,
diagnoses, and procedures, and outpatient prescription
dispensations recorded during billing of all health care
encounters. The study was approved by the Mass General
Brigham Institutional Review Board before data collection;
licensing agreements were in place. Access to the data
and analytics infrastructure can be shared for relevant
requests.
Study Design and Eligibility Criteria
We designed an observational study to emulate a
target trial comparing the risk for cardiovascular events
associated with first-line SGLT-2i versus metformin in
real-world patients with T2D, using U.S. claims data (see
Supplement Table 1 [available at Annals.org] for the
simulated trial design framework). Eligible persons were
aged 18 years and older (>65 years in Medicare), had
at least 1 diagnosis of T2D (inpatient or outpatient
International Classification of Diseases, Ninth Revision,
Clinical Modification [ICD-9-CM] codes 250.x0 or 250.
x2 through 30 September 2015, or Tenth Revision,
Clinical Modification [ICD-10-CM] code E11.xxx thereafter) at any point before cohort entry (26, 27), and had
continuous health care insurance enrollment with complete medical coverage and pharmacy benefits for at
least 365 days before cohort entry. We excluded persons with missing information on age, sex, and region.
To ensure the identification of first-line use, we excluded
persons who used any antidiabetic drugs at any point
before or on cohort entry. To reduce surveillance variability, we also excluded those who did not have at least
1 prescription or physician visit in both of 2 periods
(365 days to 183 days and 182 days to 1 day)
before cohort entry (28). Other ineligible persons had a
history of gestational or secondary diabetes, polycystic
ovary syndrome, organ transplant, end-stage renal disease, HIV/AIDS, or nursing home admission at any point
before cohort entry (Figure 1; Supplement Figure 1,
available at Annals.org).
Exposure and Follow-up
Before applying the eligibility criteria, we identified
persons who filled a new prescription for first-line SGLT-2i
(canagliflozin, empagliflozin, or dapagliflozin) or metformin
between 1 April 2013 (consistent with the launch of SGLT2i in the United States) and 31 March 2020 in Optum (31
December 2018 in MarketScan and Medicare). The first
dispensing date of the study drugs was defined as cohort
entry. We excluded persons who initiated treatment with
SGLT-2i and metformin simultaneously on cohort entry.
Follow-up began on the day after cohort entry and
continued until the occurrence of a study outcome, death,
treatment discontinuation (with an interval between prescription refills >60 days) (29), disenrollment, or end of
the study period, whichever occurred first (Figure 1;
Supplement Figure 1).
Outcomes
Primary outcomes were a composite of hospitalization for acute MI, hospitalization for ischemic or hemorrhagic stroke, or all-cause mortality (hereafter called
“mortality”) (MI/stroke/mortality), and a composite of
HHF or all-cause mortality (HHF/mortality). Secondary
outcomes were the individual components of the primary outcomes and a composite of MI/stroke/HHF/mortality. Safety events included acute kidney injury, bone
fractures, genital infections, severe hypoglycemia, severe
urinary tract infections, diabetic ketoacidosis, and lowerlimb amputations. Genital infections also functioned as a
positive tracer outcome to assess the validity of the study
results (30–32).
The outcomes were identified by using validated
ICD-9/10-CM procedural and diagnosis codes (Supplement
Table 2, available at Annals.org). Validation studies for the
claims-based algorithms for the cardiovascular end
points showed positive predictive values above 80%
(33–36). All-cause mortality was ascertained from 4 sources for Clinformatics: Centers for Medicare & Medicaid
Services data, the Social Security Administration Death
Master File (37), hospital discharge status indicating
death, and death as a reason for insurance coverage
discontinuation. All-cause mortality was ascertained
through hospital discharge status indicating death for
MarketScan (38) and through the Master Beneficiary
summary file for Medicare (39). Validation studies for
the claims-based algorithms for the safety outcomes—
acute kidney injury, bone fractures, severe hypoglycemia, and diabetic ketoacidosis—also showed positive
predictive values above 80% (40–45). For the safety outcomes—genital infections, severe urinary tract infections, and lower-limb amputations—without a validation
study, we adapted definition codes from other's studies
(32, 46–48).
Patient Characteristics
Patient characteristics were selected based on subject knowledge about confounders and predictors of the
outcomes; they were measured during the 365 days
before or on cohort entry. These included demographics,
diabetes-related and other comorbidities, concomitant
medications, and measures of health care use (Supplement
Table 3, available at Annals.org). Laboratory test results
were available for approximately 15% of the population
through linkage with national laboratory test provider
chains.
Statistical Analysis
To emulate randomization, we used propensity
score (PS) matching. We chose the ratio of 1:2 for
matching to improve statistical efficiency because
the number of metformin initiators was much larger
compared with SGLT-2i initiators. The ratio also allowed
us to retain as many SGLT-2i initiators as possible (92%)
because increasing the matching ratio would result in
excluding more SGLT-2i initiators with a caliper. Therefore,
the estimand of this study was an on-treatment estimate,
with a grace period of 60 days between prescription refills,
among the initiators receiving SGLT-2i or metformin who
were well balanced on all measured potential confounders.
To mitigate the potential channeling bias due to the
selective prescription of SGLT-2i that changed over time
since market launch, the study period was stratified into
4 consecutive calendar time blocks (T1, April 2013 through
December 2014; T2, January 2015 through June 2016; T3,
July 2016 through December 2017; and T4, January 2018
through March 2020) (49, 50). Within each database, time
block–specific predicted probabilities of receiving first-line
SGLT-2i versus metformin as treatment of T2D were estimated from logistic regression models (51) that included
all prespecified baseline covariates except for laboratory
values, which were not available for all patients (see
Supplement Tables 4 to 6 [available at Annals.org] for the
results of the PS models). The missing-indicator method
was used to treat missing values for the race variable in
Optum, assuming missingness was conditionally independent of the outcomes (52). Within strata of time block,
to reduce residual confounding and retain the same persons for the main and subgroup analyses of CVD, we then
1:2 matched patients on the PS using a caliper width of
0.001 of the SD of the logit of the PS and baseline CVD
(53–55). During the baseline period, CVD was defined as a
history of MI, stable or unstable angina, other ischemic
heart diseases, transient ischemic attack, stroke, atherosclerotic peripheral vascular disease, or heart failure, based
on the treatment guidelines for T2D (9).
Within each database, covariate balance between
the exposure groups before and after PS matching was
assessed using standardized differences greater than 0.1,
defined as meaningful imbalances for confounding a treatment effect association (56, 57). We pooled patient characteristics across 3 databases and calculated standardized
differences. The balance in laboratory test results was also
examined after PS matching to evaluate the potential for
residual confounding because laboratory test results
were not included in the PS models. We calculated database-specific unadjusted and PS-matched numbers of
events, incidence rates (IRs), and IR differences (IRDs) per
1000 person-years (1000 PYs) with 95% CIs for all outcomes. Within each database, time block–specific PSmatched cohorts were aggregated for outcome regression
(58), and hazard ratios (HRs) with 95% CIs were estimated
using proportional hazards models without further
adjustment (59). Database-specific HRs were combined
using a fixed-effect meta-analysis (60). Subgroup analyses by baseline CVD were conducted, applying a x2 test
for homogeneity (61).
We performed several sensitivity analyses to demonstrate the robustness of our findings (see the Supplement
[available at Annals.org] for more details): i) the first study
time block (April 2013 to December 2014) was excluded
from the analysis because of the lack of adequate equipoise between the treatment groups in the early phase of
postmarketing (62); ii) the study cohorts were restricted to
persons with continuous health insurance enrollment for at
least 2 years before cohort entry to assess the effect of the
probable inclusion of prior antidiabetic drug users; iii) an
intention-to-treat analysis was conducted to address potential informative censoring by carrying forward the initial
exposure for 365 days without considering treatment
discontinuation or the initiation of the comparator drug
(63); iv) in addition, we censored persons on initiation
of treatment with the comparator drug during the follow-up period to evaluate potential exposure misclassification because 16% of SGLT-2i initiators started treatment
with metformin, whereas 2% of metformin initiators started
treatment with SGLT-2i after cohort entry (data not shown);
v) for a subset of the study population with baseline hemoglobin A1c (HbA1c) levels available, we reestimated the PS,
further conditioning on HbA1c levels, to adjust for baseline
glucose control; vi) we quantified the bias associated with
the imbalance in baseline HbA1c levels across treatment
groups after PS matching (64); vii) to assess the effect of
unmeasured socioeconomic status (20), we evaluated cardiovascular end points of first-line SGLT-2i and metformin
against first-line dipeptidyl peptidase-4 inhibitors (DPP-4i),
which cost more than metformin without a known effect on
the cardiovascular outcomes of interest among adults with
T2D (9); viii) we reestimated the PSs after replacing the 4
census regions (northeast, midwest, south, and west) of the
primary analysis with 50 states and 1 federal district in the
PS models to explore the presence of potential residual
confounding because of geographic variation in clinical
care; and ix) we evaluated the effect of individual SGLT-2i
versus metformin on the primary cardiovascular outcomes.
All analyses were performed using R version 3.6.2
(65), with analytic files generated using the Aetion
Evidence Platform v4.10 (66–68).
Role of the Funding Source
The funder had no role in the design, conduct, or
analysis of the study or in the decision to submit the
manuscript for publication
`

export const JSONSGLT2iMetformin = {
  getDbCohortMethodDataArgs: {
    studyPeriods: [
      {
        studyStartDate: "20130401",
        studyEndDate: "20200331",
      },
      {
        studyStartDate: "20130401",
        studyEndDate: "20181231",
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
      }
    ],
  },
  propensityScoreAdjustment: {
    psSettings: [
      {
        matchOnPsArgs: {
          maxRatio: 2,
          caliper: 0.2, //default 설정
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
