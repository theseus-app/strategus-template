export const TEXTTramadolCodein =
    `
2 Material and Methods
2.1 Study Design
This was a retrospective, observational, comparative cohort design study [17]. The protocol and code were publicly available prior to study execution on GitHub [18], the study was registered with the European Union PAS register (EUPAS36038) [19], and the results were kept blinded until diagnostics were reviewed. Blinding in this setting means the effect size of interest was not revealed until after diagnostics were reviewed and the protocol was finalized. Blinding avoids p-hacking or adjusting an analysis to achieve a desired result.

2.2 Study Populations
We employed two variants of the target (tramadol; T1 and T2) and comparator (codeine; C1 and C2) cohorts among subjects aged 50–89 years with 365 days of continuous observable time prior to the index date. The index date in all cases was the first exposure to either tramadol or codeine that met all cohort criteria.

The first cohort variants (T1, C1) were similar to the cohorts employed by Wei et al. [6]. Subjects were excluded if they were exposed to tramadol, codeine, or other opioids in the 365 days prior to index date. Additionally, subjects were excluded if they experienced hip fracture, cancer, or were diagnosed with opioid abuse in the 365 days prior. As noted above, these cohort definitions may lead to confounding by indication because codeine is also used to treat cough, and we expected that the analysis would show this. The assumption was that patients who received codeine for cough would be younger and healthier than patients receiving tramadol for pain.

The objective of the second cohort variants (T2, C2) was to make the target and comparator cohorts more comparable and this was done in two ways. First, by excluding subjects diagnosed with cough or cold in the 30 days prior to initial exposure to the opioids of interest and second, by excluding subjects who were prescribed cold or cough medications, antibiotics, or antihistamines in the 30 days prior to initial exposure of the opioids of interest. Code lists can be accessed in the Electronic Supplementary Material (ESM) and detailed descriptions of the cohorts can be found in Tables 1–4 of the ESM.

2.3 Databases
Data from an electronic health record database from the UK and three administrative claims databases from the USA were used in our analyses. The UK database was the CPRD, which is similar to the The Health Improvement Network database used by Wei et al. [6]. The US databases included the IBM® MarketScan® Medicare Supplemental Database (MDCR), the IBM® MarketScan® Multi-State Medicaid Database (MDCD), and Optum’s De-identified Clinformatics® Data Mart—Date of Death (OPTUM_DOD). Additional information on these four databases can be found in Table 5 of the ESM. Each database was standardized to the Observational Medical Outcomes Partnership Common Data Model, version 5.3.1 [20,21,22].

Of note, the OPTUM_DOD censors subjects at 90 years of age. This means that 90-year-old subjects in this database could actually be older. Therefore, for all cohorts, across all databases, we decided to censor at age 89 years, while Wei et al. [6] censored at age 90 years.

2.4 Time-at-Risk and Outcome Definitions
For this study, two time-at-risk (TAR) definitions were used: on-treatment (OT) and intent-to-treat (ITT). The OT TAR was calculated from the index date of the first exposure to the end of treatment, based on days’ supply, allowing for up to 30-day gaps between the end of days’ supply and the start of the next prescription. This end date was censored if a subject died, left the database, reached the age of 89 years, reached 365 days after the index date, was exposed to the other opioid of interest (i.e., was censored when patients in the tramadol cohort were exposed to codeine or when patients in the codeine cohort were exposed to tramadol), or experienced the outcome of interest.

The ITT TAR started on the index date and continued until the target or comparator subject died, left the database, or experienced the outcome of interest. The ITT TAR is more sensitive to potentially longer lasting effects of exposure compared with the OT TAR.

The outcome of interest was hip fracture, which was defined in two different ways. Outcome Definition 1 (O1) was specifically designed for the CPRD analysis. This definition included the first occurrence of either a diagnosis of a hip fracture or a procedure associated with treatment of hip fracture among subjects aged 50–89 years with 365 days of continuous observable time prior to the index date (see Table 6 of the ESM for a detailed description). While the analysis was conducted on Observational Medical Outcomes Partnership Common Data Models that leveraged standard terminologies, in an attempt to replicate the Wei et al. study’s definition [6, 23], hip fracture O1 was defined via READ codes (which was the coding system used in the study). Because standardized terminologies were not used, O1 will only work on databases that use the READ code system.

The second definition of hip fracture was Outcome Definition 2 (O2), which was tailored to the US claims databases (see Table 7 of the ESM). This outcome was developed based on the algorithms from Ray et al. [1] and Nair et al. [24] and was defined as the first hip fracture in a subject’s medical history, among subjects aged 50–89 years with 365 days of observable time prior to the index date. The O2 definition leveraged standard terminologies found in the Observational Medical Outcomes Partnership Common Data Model. Because of the differences in how the outcome definitions were defined we decided, a priori, to perform the analysis for risk separately (one for CPRD while performing individual assessments of and a meta-analysis across the three administrative claims databases from the USA) [18, 19].

Negative control outcomes were also assessed in our study. Negative controls are exposure-outcome pairs where no causal relationship is believed to exist between the exposure and the outcome, and therefore the true relative risk is assumed to be 1. To account for the remaining systematic bias after PS matching, negative controls can be used to calibrate the results of an observational study [25]. The negative control outcomes were selected by choosing conditions with no evidence of an exposure-associated outcome in products labels or adverse event reporting, or the literature (process previously outlined in detail [26, 27]). A total of 221 possible negative controls were reviewed by two physicians, who ultimately agreed on 101 of the outcomes, which became our negative controls (see Table 8 of the ESM).

2.5 Statistical Analyses
2.5.1 Model Specification
In this study, we compared target cohorts with the comparator cohorts for the hazards of O1 or O2 during the TAR by applying a Cox proportional hazards model conditioned on PSs. Empirical calibration based on the negative controls was used to minimize any potential residual confounding with empirically calibrated HR (CHR), 95% calibrated confidence interval (CCI), and calibrated p-values [25, 28]. The number of subjects, days of TAR, and outcome events in each cohort, in each pairwise comparison after PS adjustment, were also reported. The time to event for O1 or O2 among subjects in the target (T1, T2) and comparator (C1, C2) cohorts was determined by calculating the number of days from the start of the TAR window (index date), until the first occurrence of the outcome, and it was right-censored at the end of the TAR window.

Propensity scores were used in the analyses to reduce potential confounding due to an imbalance of baseline subject characteristics between the target and comparator cohorts in the pairwise comparisons. The PS is the probability that a subject received the target exposure versus the comparator exposure, given a set of observed covariates. The covariates were chosen via a data-driven approach, which does not rely on clinical expertise, but instead uses a model, which can include 10,000–100,000 unique characteristics [29]. The types of baseline covariates used to fit the PS model are found in Table 9 of the ESM.

A PS was estimated for each subject using the predicted probability from a regularized logistic regression model, fit with a Laplace prior (least absolute shrinkage and selection operator), and the regularization hyperparameter selected by optimizing the likelihood in a ten-fold cross validation, using a starting variance of 0.01 and a tolerance of 2e−7 [30, 31]. Covariates that occurred in less than 0.1% of the combined target and comparator cohorts in a pairwise comparison were excluded prior to model fitting. Subjects were matched on 1:1 ratio of target to comparator subjects. This approach used a greedy matching algorithm by applying a caliper of 0.2 of the standard deviation on the logit scale of the PS distribution [32].

2.5.2 Evidence Evaluation
For each population-level effect estimate generated by the study, i.e., each target-comparator-outcome-analysis-database combination, diagnostics were conducted to understand its potential for bias and threat to a valid interpretation. The diagnostics included both PS distribution and covariate balance (before and after PS matching).

Once the PS model was fitted for each pairwise comparison, the preference score for the target and comparator cohorts was plotted to evaluate the comparability of the two cohorts (T1 vs C1, T2 vs C2) before matching. If the proportion of subjects was in clinical equipoise (i.e., the subjects with a PS between 0.3 and 0.7, was less than 50%), then the estimate was not reported [33].

Covariate balance was evaluated by computing the standardized mean difference of each covariate before PS matching against the standardized mean difference after PS matching. After matching, standardized mean differences with values <0.1 indicate negligible group differences [34].

2.5.3 Analyses Performed
Six pairwise analysis groups, comparing a tramadol cohort (T1 or T2) with a codeine cohort (C1 or C2), were performed across the TARs (OT or ITT) for a total of 24 analyses (Table 1). Three of the analysis groups (Analyses 101, 201, and 301) were performed using the CPRD for a total of six analyses. The other three analysis groups (Analyses 102, 202, and 302) were performed using the three US claims databases for a total of 18 analyses. Additionally, for the US claims database analyses, there was a pre-planned aggregation of the results using a random-effects meta-analysis. This meta-analysis did not combine the results for the CPRD and US databases because of differences in the outcome definition. In summary, Analyses 101 and 102 were meant as a replication of Wei et al. [6], Analyses 201 and 202 were a second replication that excluded some variables in the PS model that were highly correlated with exposure, and Analyses 301 and 302 excluded subjects with cough and/or prescribed cough, antibiotic, cold and cough medications, and antihistamines in the last 30 days, which resulted in the tramadol and codeine cohorts being more comparable, i.e., not confounded by differences in indication.

Additionally, a priori, we were concerned that the doses of tramadol and codeine might differ in terms of MMEs [35] as the risk of an adverse event can be dose dependent. We characterized the initial dose of both tramadol and codeine and compared the results before and after PS matching. This was done by computing the daily dose as the number of oral tablets times the number of milligrams (mg) of tramadol or codeine per tablet divided by the days’ supply. It should be noted that days’ supply was not always provided by the CPRD; however, for the tramadol and codeine exposures it was missing less than 0.2% of the time [36, 37]. For the analysis, exposure to tramadol or codeine was used regardless of its days’ supply. The MME was calculated by multiplying this daily dose by a conversion factor (0.1 for tramadol and 0.15 for codeine).
`

export const JSONTramadolCodein = {
    getDbCohortMethodDataArgs: {
        studyPeriods: [
          {
            studyStartDate: "",
            studyEndDate: "",
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
          removeSubjectsWithPriorOutcome: false,
          priorOutcomeLookBack: 99999,
        timeAtRisks: [
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 0,
                endAnchor: "cohort end",
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 99999,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
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
                tolerance: 2e-7,
                cvType: "auto", //default로 설정
                fold: 10,
                cvRepetitions: 10, //default로 설정
                noiseLevel: "silent", //default로 설정
                resetCoefficients: true, //default로 설정
                startingVariance: 0.01,
            },
        },
    },
    fitOutcomeModelArgs: { //default except modelType
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
    },
};
