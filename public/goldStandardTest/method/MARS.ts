export const TEXTMARS = `
Methods
The MARS cohort
The MARS study (Molecular Assessment and Risk Stratification in Sepsis; ClinicalTrials.gov Identifier: NCT01905033) was a prospective cohort study conducted between January 2011 and December 2013 in two tertiary academic center adult ICUs in the Netherlands (Amsterdam University Medical Center, location AMC, and University Medical Center Utrecht). All admitted patients with an expected length of stay greater than 24 h were included via an opt-out consent procedure approved by both institutional medical ethics committees (IRB no. 10-056C). A more extensive description of this cohort can be found in Additional file 1: Methods and prior publications from our group [24,25,26].

Clinical variables and definitions
Sepsis was defined as infection with a likelihood of possible, probable or definite diagnosed within 24 h after ICU admission [27, 28], and a modified sequential organ failure assessment (mSOFA) score (excluding the central nervous system component) of two or higher, consistent with sepsis-3 criteria [1]. In case of a missing SOFA score on admission, the presence of acute kidney injury (AKI) or ARDS upon ICU admission was considered a surrogate for a SOFA score of 2 or higher, thereby indicating eligibility for the study (3/705 [0.4%] patients in the final cohort). Other definitions can be found in Additional file 1: Methods.

All individual medication administrations during the study period were prospectively registered in MetaVision (iMDsoft, Israel). From these data, we identified whether patients received erythromycin (and other macrolides) and calculated the total administered dose, the duration of treatment and the total number of courses. We defined a new course of low-dose erythromycin as starting erythromycin again after at least 48 h of not receiving erythromycin.

Study design, patient selection and outcomes
We designed this observational cohort study as a “target trial,” an emulation of the ideal RCT that could be used to answer the causal question of interest, within the constraints of the available data [29]. Explicitly specifying the study design this manner theoretically reduces the influence of biases common in non-randomized studies of interventions [29,30,31]. Additional file 1: Table 1 provides a side-by-side comparison of the target trial and its emulation described herein.

Patients were eligible for inclusion in this study if they met the criteria for sepsis within 24 h of ICU admission. Patients were ineligible if they had been readmitted following a previous ICU admission within the study period or if they were transferred from another hospital (unless this was on the first day of ICU admission). Figure 1A depicts the study design. All patients had to be alive and in the ICU during an exposure period of 72 h after ICU admission to prevent immortal time bias [32]. Patients were assigned to the erythromycin group if they had received erythromycin at least once at a low-dose (125–250 mg) within these 72 h or to the control group if they had not. The follow-up period started after this 72-h exposure period and ended 90 days after ICU admission.
The treatment strategy of interest was low-dose erythromycin (up to 600 mg per day, divided over 2–4 doses), administered as a prokinetic agent (i.e., to alleviate gastrointestinal dysmotility) during the first 72 h in ICU. We chose this indication to minimize the antimicrobial effects of erythromycin and consequently increase the likelihood that any remaining difference between groups could be attributed to its immunomodulatory effects. Previous studies on macrolide treatment in acute inflammation have focused more on azithromycin and clarithromycin prescribed at higher doses as antibiotics—often inferring immunomodulatory benefits from improvements in clinical outcomes despite causative microorganisms being macrolide-resistant [18, 19, 22]. We focused on low-dose erythromycin to study immunomodulatory macrolides in critically ill patients with sepsis for several reasons: our group previously demonstrated reduced 30-day mortality in patients with ARDS treated with low-dose erythromycin [23]; erythromycin improves outcomes in animal models relevant to sepsis [15,16,17]; while subtle differences have been reported, the effects of immunomodulatory macrolides are highly comparable and erythromycin, clarithromycin and azithromycin can often be used interchangeably [8, 33]; and the immunomodulatory effects of macrolides, at least in chronic use, occur at lower doses than the antimicrobial effects [8, 34, 35]. Azithromycin and clarithromycin were also occasionally administered in the participating ICUs during the study period, but we excluded patients using these drugs during the exposure period (n = 8), because azithromycin and clarithromycin were not used for the same indication, and together represented only 4.4% of the total individual macrolide administrations during the study period (523 out of 11,797).

Patients were excluded if they started high-dose erythromycin (500–1000 mg per administration), azithromycin or clarithromycin during the 72-h exposure period. In the per protocol analysis patients in the control group were excluded if they started low-dose erythromycin after 72 h during the same ICU admission, but these patients were included in an intention-to-treat sensitivity analysis. We did not exclude patients from either group who started high-dose erythromycin, azithromycin or clarithromycin after 72 h as part of their normal care.

The primary outcome was mortality rate up to 90 days. 30-day mortality was a secondary outcome. Secondary clinical outcomes indicative of the duration of symptoms were change in mSOFA score from admission until day 4 (“ΔSOFA”); ICU and hospital length of stay; and duration of mechanical ventilation. Secondary clinical outcomes indicative of ICU-acquired complications (occurring ≥ 72 h after ICU admission, after the exposure period) were the incidence of secondary infections, AKI and ARDS.

Host response biomarker assays
Host response biomarkers were measured at admission (within 16 h of presentation) and day 4 in all sepsis patients of the MARS cohort with a likelihood of probable or definite enrolled during the first 2.5 years, as previously described [36]. Additional information pertaining to these measurements is provided in Additional file 1: Methods and Additional file 1: Table 2.

Statistical methods
Categorical data are presented as count (percentage), normally distributed or non-normally distributed continuous data are presented as mean (standard deviation) or median [interquartile range], respectively. Baseline variables in the unadjusted table were compared between the erythromycin and control group using either Welch’s t-test or Wilcoxon’s rank-sum test (for normally or non-normally distributed continuous variables, respectively), or Fisher’s exact test (for categorical data). Tests were two-sided throughout, and a P value < 0.05 was considered statistically significant. All analyses were performed in the R statistical framework (version 4.1.2, Vienna, Austria).

Missing data
Variables with ≤ 5% overall missing data were considered missing completely at random and hence were not imputed. The fraction of missing information for the covariates used in the propensity score (PS) estimation (listed below) was low, with values in any of the covariates missing in 18/705 (2.6%) patients. We therefore used a listwise deletion approach in which 5/235 (2.1%) patients in the erythromycin group and 13/470 (2.8%) patients in the control group were excluded from the analyses (Additional file 1: Fig. 1). A detailed overview of missing data is provided in Additional file 1: Methods and Additional file 1: Tables 3 and 4.

Estimation of the propensity score
Treatment with erythromycin is dependent on baseline covariates linked to mortality. To deal with this confounding by indication (and thereby increase the likelihood of ignorability [37]), we used both PS matching and inverse probability of treatment weighting (IPTW; referred to as “weighting” or “weighted” throughout the manuscript) using the PS. These methods are commonly used to estimate different treatment effects: PS matching estimates the average treatment effect for the treated (ATT), the effect of the treatment for patients similar to those already being treated; IPTW estimates the average treatment effect (ATE), the effect of treatment if it were applied to the entire population under study [38].

For each patient, we estimated the PS—the probability of receiving the treatment given the covariates used in the model [39]—using logistic regression, with treatment exposure set as the dependent variable and baseline covariates as independent variables. Based on the pathophysiology of gastrointestinal dysmotility in the critically ill [7, 40, 41], we selected covariates either related to both receiving erythromycin and the primary outcome of 90-day mortality (true confounders), or related to 90-day mortality and possibly to erythromycin exposure (potential confounders). Variables only related to receiving erythromycin but not to the outcome were not included in the model [42]. Additional file 1: Fig. 2 depicts a directed acyclic graph (DAG) of the assumed causal relationships between the treatment, the outcome and the baseline (admission) covariates. The model included the following covariates (measured at ICU admission): age, sex, body mass index, hospital of admission, postsurgical admission, source of infection (abdominal, pulmonary, urinary, cardiovascular, skin, central nervous system or other/unknown [24]), Charlson comorbidity index score (without age), any malignancy (solid or hematological), Acute Physiology And Chronic Health Evaluation (APACHE) IV score [43], mSOFA score, Gastrointestinal Failure score—an ordinal scale ranging from “normal gastrointestinal function” to “abdominal compartment syndrome” [44]—dichotomized to absent (score of 0) or present (score of 1 or higher), septic shock, ARDS, AKI and use of mechanical ventilation.

For PS matching, we used greedy matching with a caliper width of 0.2 times the standard deviation of the PS logit [45] and matched treated patients to controls 1:1. For IPTW, we capped weights above 10 at 10 to limit excessive influence on the results induced by extremes of the PS. We assessed the balance in distribution of covariates before and after both PS matching and weighting by examining the standardized mean difference (SMD) for all variables, and the distribution of variances or interquartile ranges for continuous variables. SMDs should ideally be < 0.1 for all covariates used in the model, but we accepted SMDs up to 0.2. Variance ratios should ideally be 1, but values < 2 were considered acceptable [46].

Estimation of the treatment effects
In the unadjusted sample, we compared mortality up to 90 days with Kaplan–Meier curves and estimated hazard ratios for mortality using Cox proportional hazard models. After PS matching, we created survival curves of the matched samples and estimated the hazard ratios and their standard errors by using Cox models with a robust variance estimator to account for the matched pairs [38]. After PS weighting, we created weighted survival curves and estimated hazard ratios using Cox models. We calculated the standard errors for the weighted hazard ratios as the standard deviation of the distribution of bootstrapped hazard ratios, specifically by re-estimating the weights and fitting the Cox model in 1000 bootstrap samples [47]. To assess the influence of residual confounding, we calculated E-values [48] as described in Additional file 1: Methods. Based on an earlier report [20], we also calculated hazard ratios for the period from 30 to 90 days after admission in the matched and weighted populations. Secondary clinical outcomes were compared using statistical tests appropriate for matched and weighted data, as described in Additional file 1: Methods.

Analysis of host response biomarkers
We analyzed host response biomarker levels and trajectories in PS matched patients, using linear mixed models on log2-transformed values, as described in Additional file 1: Methods.

Sensitivity analyses
We performed three sensitivity analyses to test whether findings in the primary outcome were robust to changes in the study design: (1) an intention-to-treat analysis where control patients in whom low-dose erythromycin was initiated more than 72 h after ICU admission were included (as excluding patients based on events that occur after follow-up has started may lead to selection bias); (2) an analysis with different exposure periods, in which we varied the duration of the period during which patients could be included (and had to be alive) from 72 to 48 h or 96 h; (3) a competing risk analysis in which we considered ICU discharge as a competing risk for mortality (see Additional file 1: Methods for details).
`

export const JSONMARS = {
  getDbCohortMethodDataArgs: {
    studyPeriods: [
      {
        studyStartDate: "20110101",
        studyEndDate: "20131231",
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
        riskWindowStart: 3,
        startAnchor: "cohort start",
        riskWindowEnd: 90,
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
