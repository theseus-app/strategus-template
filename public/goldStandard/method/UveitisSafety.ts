export const TEXTUveitisSafety =
    `
Methods
We designed and conducted an active comparator, new user, PS-matched cohort study [24, 25] to estimate the risk of NIU among new users of Remicade®. The pre-specified protocol and complete source code for this study are available at https://github.com/ohdsi-studies/UveitisSafetyEstimation/tree/master/Documents and https://github.com/ohdsi-studies/UveitisSafetyEstimation/. Our observational study adhered to principles of target trial emulation [26, 27] and standardized, comprehensive analyses intended to reduce observational study biases [28]. This was a multi-database study that allows for analysis of diverse patient populations, rare exposures and outcomes, and supports replicability and generalizability [29]. Further, evidence from multi-database studies is strengthened by assessing results consistency across databases.

Data sources
We conducted the study in five databases, three administrative claims and two electronic health record (EHR) databases, all from the United States (US). The claims databases included Merative™ MarketScan® Commercial Database (CCAE), Optum® de-Identified Clinformatics® Data Mart Database (Clinformatics®), IQVIA Pharmetrics Plus (Pharmetrics). The EHR databases included Optum® de-identified Electronic Health Record Dataset (Optum® EHR) and IQVIA Ambulatory EMR (Amb EMR). These five US databases provide multiple perspectives on the study populations given variation in population composition and data capture process by database. Detailed database descriptions are available in Appendix 1.

The databases were standardized in structure and content into the Observational Medical Outcomes Partnership (OMOP) Common Data Model (CDM) [30, 31] which is maintained by the Observational Health Data Sciences and Informatics (OHDSI) community. This standardization allows the strictly consistent application of analytic routines across multiple, disparate databases that eliminates variability of cohort definitions, variable definitions, and analytic implementation.

Study populations
We assessed four non-mutually exclusive populations that are indicated for Remicade®: patients with inflammatory bowel conditions Crohn’s disease or ulcerative colitis (IBD), ankylosing spondylitis (AS), psoriatic conditions plaque psoriasis or psoriatic arthritis (PsO/PsA), and rheumatoid arthritis (RA). The indication cohort definitions are fully specified in Appendix 2. Comprehensive clinical characterization of the Remicade®-indicated study populations is available for review at an interactive web application at https://results.ohdsi.org/app/15_UveitisSafetyIndications.

Exposures
Within each study population we compared new users of a target exposure to new users of comparator exposures, that we refer to as the target and comparator cohorts. In the IBD, AS, PsO/PsA study populations, the target cohorts consisted of patients newly exposed to Remicade®. In the RA study population, the target cohort consisted of patients newly exposed to Remicade® concurrently exposed to methotrexateFootnote2 [32]. We compared the Remicade® target cohorts to the comparator cohorts, which differed by study population. We defined the comparator cohorts by new use of one of several alternative therapies indicated for IBD, AS, PsO/PsA, or RA and are listed in Table 1. The comparator exposures are biologics approved by the US Food and Drug Administration (FDA) for treatment of the indication study populations. Further, we excluded specific exposures for which there exists evidence of an increased or decreased risk for NIU. Specifically, we excluded etanercept and adalimumab from all comparator cohorts. Etanercept is known to increase the risk of uveitis and adalimumab is approved as a treatment for uveitis, as well as known to decrease the risk of uveitis [33,34,35].

Table 1 presents the comparator drugs used as the reference to which the target cohorts were compared for each indication. The target cohort population was limited to index exposures after the earliest date of approval by the FDA for the drugs included in the comparator cohort. Patients aged at least 18 years at the time of index and with at least 365 days of prior observation were eligible to participate in both the target and comparator cohorts. Additionally, target cohort patients were required to be naïve to biologics and infliximab biosimilars. All patients in the target and comparator cohorts were required to have no previous exposure to the medications listed as restrictions in Table 2. The detailed target and comparator cohort definitions are in Appendix 2.

Remicade® dosage varies by indicationFootnote3. Because our study is stratified by indication, it is unlikely Remicade® dosage variation will violate the consistency assumption for causality (i.e., each patient receives the same version of treatment, or if multiple versions of a treatment do exist, then they have the same effect on the outcome) [36].

Outcomes
Phenotyping is the process by which the physiological, clinical description of a medical condition is translated into a computable algorithm designed to identify patients with the condition from an observational data source [37, 38]. We applied a novel phenotyping [39] and outcome validation [40, 41] approach to developing and evaluating a phenotype algorithm for patients with NIU with intent to minimize misclassification. In studies that use ratio effect estimates such as ours, low outcome sensitivity is tolerable provided specificity is high to obtain an unbiased estimate of treatment effect [42].

The novel outcome validation method we used builds a probabilistic reference standard rather than using deterministic medical chart adjudication. We fit a diagnostic predictive model that assigns case probabilities to a large reference set against which we compared patients returned by our candidate outcome phenotype algorithms. Case probabilities are assigned to cases and non-cases which allowed us to populate a full confusion matrix with the sums of conditional probabilities to compute all misclassification metrics. Briefly, NIU is intraocular inflammation, characterized by inflammation of the uvea in the absence of infection. We developed and evaluated three outcome definitions:

Broad – first occurrence of a NIU code.

Narrow – first occurrence of a NIU code with a second NIU code occurrence between 31 days and 365 days relative to first occurrence.

Primary – [first occurrence of a NIU code with a second NIU code occurrence between 31 days and 365 days relative to first occurrence] OR [first occurrence of a NIU code during an ophthalmology visit].

We ultimately used the primary definition in our comparative study given its high specificity and its favorable tradeoff between sensitivity and patient count compared to the other definitions. The full clinical description of NIU, the full code list and temporal logic specifications of our three candidate algorithms, and the results of our phenotyping development and evaluation are reported in Appendix 3. We comprehensively characterized our candidate NIU definitions which are available at https://results.ohdsi.org/app/14_UveitisSafetyOutcomes. Misclassification errors for the primary outcome definition is reported in Table 3. It was on the basis of the phenotype evaluation results reported in Appendix 3 that we decided to use the primary NIU definition.

Cohort study analysis specifications
We fit a large-scale PS model (LSPS) [43, 44] to ensure baseline balance on directly and indirectly measured covariates [45, 46] between the target and comparator cohorts. The PS was calculated for each patient as the predicted probability of target exposure status from an L1 regularized logistic regression model, fit with a Laplace prior where the regularization hyperparameter was selected by optimizing the likelihood in a 10-fold cross validation with a starting variance of 0.01 and a tolerance of 2*10− 7 [47]. PS model input covariates included demographics, several risk indices, and code occurrence-based, baseline covariates for all medical diagnoses, drug exposures, procedure occurrences, device use, and laboratory measurements (Appendix 4). Our primary PS adjustment strategy matched target to comparator patients using a 1:10 maximum variable ratio matching approach and used a greedy matching algorithm that applied a caliper of 0.2 of the standard deviation on the logit scale of the PS distribution [48].

We defined the ‘on-treatment’ time-at-risk (TAR) as the day after index until the end of a period of inferred persistent exposure. This allowed no more than a 90-day persistence window between successive exposures plus a 90-day added surveillance to the last exposure date. We chose this persistence window based on recommended administration frequency [49] and an empirical assessment of the durations between subsequent administrations for the drugs in three data sources included in this study. The days distribution between successive exposures showed that 75% of sequential administration records occurred within 90 days for all drugs in all databases except for ustekinumab in Optum EHR®. Further, 90% of exposure records occurred within 90 days for most drugs except for ustekinumab, which may have time-at-risk right-censored early for approximately 10% of patients. Appendix 5 reports time distributions between subsequent exposures for the study drugs. This approach was consistent with safety follow-up in registry regulatory safety studies for biologics marketed by the sponsor [49]. Additionally, we right-censored ‘on-treatment’ TAR at an exposure to a comparison drug, adalimumab, or etanercept; for the target cohorts, exposure was censored on other TNF alpha inhibitor (TNFαi) or interleukin inhibitors and the comparator cohorts, exposure was censored at the exposures listed in Table 2.

Within each database and study population, we fit a Cox proportional-hazards (PH) regression model conditioned on PS-matched sets with Remicade® treatment status as the explanatory variable to model the time to the first ever NIU occurrence relative to the comparator group. This requirement excluded patients with a pre-index NIU event from the analysis.

In addition to the NIU outcome of interest, we also executed each study comparison against a set of 86 negative control outcomes to identify and correct for unobserved confounding and design or analytic misspecification [50]. Negative control outcomes are conditions known not to be causally associated with the target or comparator cohort exposures. Negative controls were selected by a semi-automated process that identifies conditions with no evidence of causal drug effects per spontaneous reports, published literature, and product labels [51]. Because of the a priori assertion of no target or comparator effect on the negative control outcomes, we assume the difference between hypothetical null (hazard ratio [HR] = 1) and the observed effect on a negative control is considered residual systematic error from unmeasured sources. The set of negative controls outcomes are in Appendix 6. We calibrated the NIU hazard ratios against the empirical null distribution to adjust for observed residual bias and reported calibrated hazard ratios (cHR) as the effect estimate.

In addition to the primary analysis described above, we included secondary analyses with 1:1 PS matching and an ‘intention-to-treat’ (ITT) TAR. The ITT TAR began on the day after index and ended at the end of observation time in the database and was not right censored at discontinuation or exposure to other drugs.

Our two PS matching strategies (1:10, 1:1), two TAR risk definitions (‘on-treatment’, ‘ITT’), four comparisons (Remicade® vs. comparator in IBD, AS, PsO/PsA, and RA study populations), by five databases (CCAE, Clinformatics®, Pharmetrics, Optum® EHR, and Amb EMR) resulted in 80 individual analyses, each intended to produce a single effect estimate. Twenty of these analyses were designated as primary (1:10 matching strategy, ‘on-treatment’ TAR, by four comparisons, and by five databases).

For each study population comparative analysis that passed diagnostics, we calculated the heterogeneity of hazard ratios across databases using the I2 metric and performed a meta-analysis using a DerSimonian-Laird estimate of the random effects variance [52]. We computed meta-analytic effect estimates when estimates of heterogeneity across databases were sufficiently low (I2 < 40%). Meta-analytic results from our primary analysis were our main source of statistical inference from which we drew causal inference conclusions. Where meta-analytic estimates were unavailable because of failing diagnostics or unacceptable heterogeneity, we reported and interpreted database-specific estimates.

Evidence validity diagnostics
The target estimand in this study is the average treatment effect in the overlap (ATO) population [53]. One key assumption for causal inference from a potential outcomes framework is exchangeability [54]. In the context of estimating the average treatment effect among the treated (ATT) (and the ATO using PS matching with a caliper) we assume partial exchangeability, the potential outcome under no treatment must be unrelated to treatment assignment conditional on measured covariates [36, 53]. The PS is a balancing score, such that the distribution of observed baseline covariates will be equivalent between target and comparator patients with similar PS values, and if strong ignorability with partial exchangeability holds then treatment assignment is unrelated to the potential outcome under no treatment conditional on the PS [43]. PS matching is used to approximate exchangeability between exposed patient cohorts that have been selectively assigned treatments during routine clinical care. Exchangeable target and comparator cohorts are those where exposure status is the only difference between them, where we can attribute any difference in outcome occurrence to exposure status only [55].

For each analysis in each database intended to generate an effect estimate, we applied the following validity diagnostics to determine whether we could report the result as reliable.

Empirical equipoise
Empirical equipoise is a diagnostic related to partial exchangeability. Specifically, target and comparator cohorts with similar PS distributions, or a high degree of overlap, will have similar baseline covariate distributions on average. Further, these patient cohorts will resemble each other on observed baseline covariates including confounders, thereby making it more likely that the partial exchangeability assumption has been met. After fitting the PS model, plotting the PS distribution stratified by exposure status can help assess partial exchangeability. By calculating the proportion of study population patients with PS overlap near equipoise (PS = 0.5), we can appraise comparability appropriateness before applying any statistical balancing techniques to strengthen exchangeability. A patient is in empirical equipoise if their preference score (a transformation of the PS that normalizes for exposure cohort size imbalances) is between 0.3 and 0.7 of the preference score distribution [56]. If the proportion of patients in empirical equipoise was less than 35% in an analysis, then it failed the equipoise diagnostic. We were more liberal than the 50% threshold proposed by Walker [56] because we prioritized bias reduction and internal validity over initial comparability assessment.

Covariate balance
Covariate balance is another diagnostic related to partial exchangeability. Conditional on the PS, patients of different exposure status should have similar distributions of baseline covariates. This assertion requires empirical confirmation to meet the assumption that treatment effect estimates are only valid only if patients in the two exposure cohorts have similar distributions of observed baseline covariates [57]. In the sample of PS-matched patients, we assessed baseline covariate distribution similarity by calculating and plotting the absolute standardized difference (ASD) [57] of every covariate before and after applying PS matching. For binary covariates, the ASD is the absolute prevalence difference of a covariate in units of the pooled standard deviation and is insensitive to sample size. We considered after matching ASDs less than 0.1 to indicate a negligible difference between cohorts in a pairwise comparison [57]. If any covariate in a comparison had an ASD greater than or equal to 0.1, then the analysis failed the covariate balance diagnostic.

Expected absolute standardized error
The expected absolute standardized error (EASE) metric detects and quantifies residual bias from unobserved sources, which relates to the assumption of partial exchangeability. To compute EASE, we first generate a residual systematic error distribution using effect size estimates for negative controls, assuming this distribution follows a normal distribution. We fit this distribution similarly to the random-effects component in a meta-analysis, capturing deviations from the null that are not attributed to random error (as indicated by estimated systematic errors) [58, 59]. EASE then summarizes this systematic error distribution by integrating over its absolute values. An EASE of 0 suggests that the variance in negative control estimates is fully explained by random error, indicating the absence of systematic error. We considered analyses where EASE was greater than 0.25 to have failed the diagnostic. When EASE = 0.25 and systematic error is centered on 0, a true relative risk of 1 has a 95% probability of being observed between 0.54 and 1.85 due to systematic error. Although empirical calibration could statistically restore nominal operating characteristics, we decided EASE > 0.25 identified unacceptable design operating characteristics even after PS adjustment.

Non-zero event counts
For a HR to be estimated from a Cox PH model, outcome occurrences during the TAR for both target and comparator cohorts in the analysis had to be greater than zero. Otherwise, the HR would approach negative or positive infinity, which is not a valid estimate of a causal effect. Analyses where target and/or comparator TAR outcome occurrence counts were zero failed this diagnostic.

Representativeness
In establishing valid causal estimates from observational data, patient restriction from the original study population is sometimes required, for example, when patients are excluded after PS matching or for having an outcome occur before index. This practice is often necessary to ensure the interval validity of the study, but it can be at odds with representativeness. While it has been argued that representativeness may not be essential for scientific study [60], the extent to which the characteristics distribution of the restricted analytic cohort differ from that of the original study population can be assessed empirically. We assessed the extent to which baseline characteristics of the after-matching target cohort are like those of the initial target cohort. We evaluated covariate similarity between the two cohorts by plotting the prevalence of all baseline covariates and calculating ASDs [61]. Note that representativeness was assessed qualitatively with no set threshold for meeting a representativeness criterion.

We set the thresholds for the empirical equipoise, covariate balance, and expected absolute standardized error somewhat arbitrarily, but we assert that a critical feature of validity diagnostics is to set thresholds a priori and adhere to them strictly to avoid investigator bias from post-hoc analyses conditional on preliminary results. Our pre-specified protocol was posted at: https://github.com/ohdsi-studies/UveitisSafetyEstimation/tree/master/Documents.
`

export const JSONUveitisSafety ={
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
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 99999,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            }
        ]
    },
    propensityScoreAdjustment: {
        psSettings: [
            {
                matchOnPsArgs: {
                    maxRatio: 10,
                    caliper: 0.2,
                    caliperScale: "standardized logit"
                },
                stratifyByPsArgs: null,
            },
            {
                matchOnPsArgs: {
                    maxRatio: 1,
                    caliper: 0.2,
                    caliperScale: "standardized logit"
                },
                stratifyByPsArgs: null,
            }
        ],
        createPsArgs: {
            maxCohortSizeForFitting: 250000, //default로 설정
            errorOnHighCorrelation: true, //default로 설정
            prior: {
                priorType: "laplace",
                useCrossValidation: true
            },
            control: {
                tolerance: 2e-7,
                cvType: "auto",
                fold: 10,
                cvRepetitions: 10, //default로 설정
                noiseLevel: "silent", //default로 설정
                resetCoefficients: true, //default로 설정
                startingVariance: 0.01
            }
        }
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
    }
}