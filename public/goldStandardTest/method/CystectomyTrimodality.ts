export const TEXTCystectomyTrimodality = `
Methods
Study design and participants
This retrospective study included patients with localised,
cT2–T4N0M0 muscle-invasive urothelial carcinoma of
the bladder who would have been eligible for both radical
cystectomy and trimodality therapy, who were treated
between Jan 1, 2005, and Dec 31, 2017, at Massachusetts
General Hospital, Boston, MA, USA; Princess Margaret
Cancer Centre, University Health Network, Toronto, ON,
Canada; or University of Southern California, Los Angeles,
CA, USA. Institutional research ethics board approvals
were obtained. Due to the retrospective nature of this
study, written informed consent was not required
from patients. Patients underwent radical cystectomy
in Toronto and Los Angeles, and trimodality therapy in
Toronto and Boston.
Inclusion criteria for trimodality therapy candidates
were (1) cT2–T4N0M0 muscle-invasive bladder cancer
tumours less than 7 cm in their greatest dimension,
(2) solitary tumours, (3) no or only unilateral hydronephrosis, (4) adequate bladder function, and (5) no
extensive or multifocal carcinoma in situ. Patients with
limited carcinoma in situ adjacent to the primary tumour,
patients with unilateral hydronephrosis, and patients at
high risk of morbidity and mortality from radical
cystectomy were not excluded from consideration for
trimodality therapy. Patients with urothelial carcinoma
and histological variants were included both for
trimodality therapy and radical cystectomy.16 Exclusion
criteria were (1) not fulfilling the inclusion criteria,
(2) primary non-urothelial cancers, (3) contraindications
to radiation (eg, previous pelvic radiation, history of
inflammatory bowel disease), and (4) concomitant upper
tract urothelial cancer.
Decision to pursue trimodality therapy versus radical
cystectomy was based on the patient’s choice after
multidisciplinary discussions of treatment options.
Patients were clinically staged at each institution
and underwent a maximal transurethral resection of
bladder tumour (TURBT) as a component of trimodality
therapy. As completeness of a TURBT has been found
to be prognostic, repeat resection was performed for
cases when tumour was still visible macroscopically.7
Muscle invasion was pathologically confirmed on
diagnostic TURBT specimens in all participants. All
patients had cross-sectional imaging and none with
node-positive disease or metastatic disease before
treatment were included.
Patients then received concurrent radiosensitising
chemotherapy and radiotherapy. Details regarding
radiotherapy dose, design, and number of fractions, as
well as type of chemotherapy, have been previously
reported for the Toronto and Boston sites, and are based
on previous protocols from NRG Oncology and
RTOG.6,7,17 All patients treated with trimodality therapy
received radiosensitising chemotherapy. Of note, for
those who received chemotherapy in addition to their
radiosensitising chemotherapy, neoadjuvant chemotherapy was given in Toronto, whereas adjuvant
chemotherapy was given in Boston. All patients met the
inclusion criteria for trimodality therapy before receiving
neoadjuvant chemotherapy. Regarding the surgical
approach, in summary, radical cystectomy consisted of
cystoprostatectomy in men or anterior exenteration in
women with bilateral pelvic lymph node dissection
and urinary diversion. Patients with a history of pelvic
radiotherapy who underwent radical cystectomy were
excluded because they would not have been eligible
for both treatment modalities. All patients were
followed up with surveillance imaging and patients
who received trimodality therapy were also followed
up with cystoscopy, as previously reported,6,7 according
to established international guidelines and protocols.
Follow-up was not different between centres.
Outcomes
The predefined primary endpoint was metastasis-free
survival (combined distant metastases and regional
pelvic or nodal failure). Secondary endpoints were overall
survival, cancer-specific survival, distant metastatic
failure-free survival (any recurrence outside of the pelvis),
regional failure-free survival (nodal recurrence within
the pelvis), and disease-free survival (regional and distant
failure and cancer-specific mortality). All-cause mortality
was considered a competing risk for metastasis-free
survival, distant metastatic failure-free survival, and
regional failure-free survival. Death unrelated to bladder
cancer was considered a competing risk for cancerspecific survival.
Metastases and nodal recurrences were confirmed
either by biopsy or by cross-sectional imaging. Local
recurrences were defined as recurrence in the urethra or
bladder bed in the radical cystectomy group, and within
the bladder (either non-muscle invasive or muscleinvasive) in the trimodality therapy group. Recurrences
in the ureter were considered a second primary in both
groups. All endpoints were measured from the date of
diagnosis to the date of the first documented event.
Causes of death were determined by the original
investigator, independently reviewed by the investigators
of this study, and reviewed by a committee among the
authors when deemed necessary. Patients who did not
experience the event during follow-up were censored at
the date of last follow-up.
Statistical analysis
Baseline characteristics were summarised using descriptive statistics, then compared between groups using
the Mann-Whitney U and χ² tests. All statistical comparisons were two-sided, and a p value of less than 0·05
was considered statistically significant. Stata version 16.1
and R version 4.1.0 were used to conduct the analyses.
The analysis was performed as intention-to-treat. Two
independent statisticians (KL and AN) at different 
institutions analysed the data with different propensity
score techniques. Missing data were imputed using
multiple imputation chained equations, and imputed
data were pooled using Rubin’s rules.
To directly compare outcomes between the radical
cystectomy and trimodality therapy groups, propensity
score matching (PSM) methods were used to match
patients in the radical cystectomy cohort to patients in
the trimodality therapy cohort. Propensity score was
calculated using a logistic regression model with the
following predictors: age (continuous), sex (male vs
female), carcinoma in situ (yes vs no), clinical T-stage
(cT2 vs cT3 or cT4), Eastern Cooperative Oncology Group 
(ECOG) performance status, BMI (<30 kg/m² vs
≥30 kg/m²),3
 hydronephrosis (no vs unilateral), peritreatment (either neoadjuvant or adjuvant) chemotherapy,
and smoking history (never smoked vs current or
former smoker). Missing covariates were imputed using
multiple imputation methods. Propensity scores were
incorporated in two ways: (1) PSM using logistic
regression and 3:1 matching with replacement; and
(2) inverse probability treatment weighting (IPTW). For
PSM, trimodality therapy patients were matched to
radical cystectomy patients with replacement using a
3:1 ratio with nearest-neighbour matching and caliper of
0·25 times the standard deviation of the propensity
score’s logit using the same ratio as recently reported.18
Trimodality therapy patients for whom three radical
cystectomy patients could not be matched because
propensity score fell beyond the boundaries were
retained along with the radical cystectomy patients that
were matched. Distribution of propensity scores and
covariates after matching were reviewed to assess the
quality of match. For IPTW, stabilised weights (defined
as the weight multiplied by the probability of receiving
the actual treatment received) were calculated from
the propensity score and used as weights.19 Distribution
of covariates after weighting were evaluated. For
both methods, the absolute value of the standardised
differences less than 0·1 were considered acceptable.
Differences in overall survival by treatment were
estimated using doubly robust multivariable Cox
proportional hazards models incorporating covariates
used in propensity score calculation. Cancer-specific
survival, metastasis-free survival, disease-free survival,
distant metastatic failure-free survival, and regional
failure-free survival by treatment group were compared
using doubly robust multivariable Fine and Gray
regression models, thus allowing estimation of the subdistribution hazard function, which models the hazard
function in the presence of competing risks. Death due
to causes other than bladder cancer was considered a
competing risk for cancer-specific survival, and all-cause
mortality was considered a competing risk for all other
aforementioned endpoints. In the PSM analysis, all
models incorporated the covariates used in the propensity)
score. In the IPTW analysis, all models incorporated
both the covariates and stabilised weights. All outcomes
were displayed as adjusted survival curves, and the 5-year
survival for each treatment group was estimated.
Sensitivity analyses were performed to assess any
potential differences in modality outcomes between
centres. IPTW cancer-specific survival and metastasisfree survival stratified by treatment and centre were
estimated, and differences between centres for each
modality were assessed using the multivariable weighted
Fine and Gray regression models. Sensitivity analyses
were repeated with the subset of patients with cT2
disease only. Additionally, varying match ratios for the
PSM analysis were explored, including 1:1, 1:2, and
1:4 matching.
Given level 1 evidence supporting the role of chemotherapy in radical cystectomy,1
 we performed a sensitivity
analysis of radical cystectomy plus neoadjuvant or adjuvant
chemotherapy versus trimodality therapy, as well as
radical cystectomy plus neoadjuvant chemotherapy versus
trimodality therapy. Propensity scores were recalculated
for this subset using the same covariates used in the main
radical cystectomy versus trimodality therapy analysis
(excluding neoadjuvant or adjuvant chemotherapy), and
IPTW was used to incorporate the propensity scores.
Adjusted and weighted cancer-specific survival, metastasisfree survival, and disease-free survival were estimated and
plotted, and the 5-year survivals were estimated.
For radical cystectomy, surgical characteristics were
described using descriptive statistics. Cross-tabulations of
clinical T-stage and pathological T-stage were performed.
Patients were categorised into groups using pathological
T-stage and hydronephrosis status. Differences in
metastasis-free survival by pathological stage and
hydronephrosis status (present or absent) were assessed
using cumulative incidence plots. Patients were stratified
according to pathological T-stage and hydronephrosis
group and the cumulative incidence of metastasis over
time was plotted. Differences in the cumulative incidence
were assessed using the Fine-Gray test.
For trimodality therapy, time to non-muscle-invasive
and muscle-invasive failures were estimated using
cumulative incidence functions, with death serving as a
competing risk. Cancer-specific survival, stratified by
salvage cystectomy status, was estimated and compared
using multivariable Fine and Gray models. Metastasisfree survival by peri-treatment chemotherapy status
was formally evaluated using propensity score methods.
The propensity score for receipt of peri-treatment
chemotherapy was estimated using logistic regression
models incorporating age (continuous), sex, carcinoma
in situ, clinical T-stage (cT2 vs cT3 or cT4), BMI
(<30 kg/m² vs ≥30 kg/m²), hydronephrosis, and
smoking history. IPTW was used with stabilised
weights. Weighted cumulative incidence curves were
generated and differences in metastasis-free survival by
peri-treatment chemotherapy were evaluated using
multivariable and weighted Fine and Gray regression
models incorporating the aforementioned covariates. 
Role of the funding source
The funders of the study had no role in study design,
data collection, data analysis, data interpretation, or
writing of the report.
`

export const JSONCystectomyTrimodality = {
  getDbCohortMethodDataArgs: {
    studyPeriods: [
      {
        studyStartDate: "20050101",
        studyEndDate: "20171231",
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
          maxRatio: 3,
          caliper: 0.2, //default 설정
          caliperScale: "standardized logit" //default 설정
        },
        stratifyByPsArgs: null
      },
      {
        matchOnPsArgs: {
          maxRatio: 1,
          caliper: 0.2, //default 설정
          caliperScale: "standardized logit" //default 설정
        },
        stratifyByPsArgs: null
      },
      {
        matchOnPsArgs: {
          maxRatio: 2,
          caliper: 0.2, //default 설정
          caliperScale: "standardized logit" //default 설정
        },
        stratifyByPsArgs: null
      },
      {
        matchOnPsArgs: {
          maxRatio: 4,
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
  fitOutcomeModelArgs: { 
    modelType: "cox",
    stratified: true,
    useCovariates: true,
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
