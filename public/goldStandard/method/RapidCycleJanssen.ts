export const TEXTRapidCycleJanssen =
    `
2. Methods
Following the first Ad26.COV2·S authorization, Janssen launched an initial version of this near real-time real world safety monitoring framework in February 2021 with monitoring in a single US healthcare claims data source and expanded to include four US claims databases in February 2022. Whenever data was refreshed, another ‘look’ [5] at the data was performed. In total, the data were refreshed four times.
2.1. Data Sources
Janssen first used the HealthVerity COVID-19 Vaccine database because the latency and periodic updates of this data overcame lags in COVID-19 vaccine exposure capture in US closed claims data. From February 2022 forward, Janssen added licensed closed claims data sources including Merative™ (CCAE) MarketScan® Commercial Database, Optum's de-identified Clinformatics® Data Mart Database, and IQVIA™ Adjudicated Health Plan Claims Data. In late 2022, the HealthVerity COVID-19 Vaccine dataset was removed when licensing ended and was replaced by the IQVIA Pharmetrics Plus Adjudicated Health Plan Claims Data when it was licensed and observed to have sufficient Janssen COVID-19 vaccine exposure. Appendix 1 includes full details of these databases, including descriptions of the data and high-level database characterizations.
When data owners provide updates to databases, they are converted to Observational Medical Outcomes Partnership (OMOP) Common Data Model (CDM) v5.4 [6] utilizing the Observational Health Data Sciences and Informatics (OHDSI) standardized vocabularies prior to analysis [7]. Databases are characterized and evaluated for quality (Fig. 1). The OHDSI Data Quality Dashboard [8] is used to perform over 3000 data quality checks.
The use of the Merative Marketscan and Optum claims databases was reviewed by the New England Institutional Review Board (IRB) and was determined to be exempt from broad IRB approval, as this research project did not involve human subjects research. Like those databases HealthVerity is deidentified and is HIPAA-compliant, therefore making it non-human subjects research.
2.2. Exposures of Interest
To allow for comparative assessment, three COVID-19 vaccine exposure cohorts were defined based on the first COVID-19 vaccine exposure to either Janssen Ad26.COV2·S (target exposure), Moderna mRNA-1273 (comparator exposure), or Pfizer mRNA-BNT162b2 (comparator exposure). Individuals were included in these COVID-19 vaccine exposure cohorts if they had at least 365 days of observation before and at least one day of observation after this first COVID-19 vaccination starting on or after January 1st, 2021. Cohort entry date was the day of the first vaccine exposure.
2.3. Outcomes of Interest
The outcome list was built using the Safety Platform for Emergency vACcines (SPEAC) COVID-19, [9] and the Biologics Effectiveness and Safety (BEST) Initiative lists of adverse events of special interest (AESI), [10] adding any outcome identified as a potential safety signal in the framework of pharmacovigilance activities. A total of 56 outcomes across multiple body systems (blood / lymphatic, cardiac, immune, and nervous / central) were included and are listed in Table 1.
To develop computable phenotype algorithms for outcomes of interest, a consistent process was followed that relied on clinical descriptions for each outcome and utilized validated definitions identified through systematic literature reviews, when available. When available, existing case definitions and diagnostic codes, such as those from the Brighton Collaboration, US FDA or US Centers for Disease Control and Prevention (CDC) were considered. An internal panel of clinicians, epidemiologists, and informaticians (AS, PR, CW, GR, RM) reviewed code lists used for outcome phenotype algorithms. The OHDSI CohortDiagnostics 3.2.5 R package [11,12] was employed to review code lists, identify overlaps between different outcome phenotypes within data sources, and generate detailed clinical profile summaries for individuals meeting outcome definitions in the data sources, which helped in the evaluation phenotype algorithm validity.
When possible, performance characteristics for algorithms were estimated with the PheValuator R package, which estimates the sensitivity, specificity, and positive predicted value of a specific phenotype algorithm against a diagnostic predictive model as the gold standard [13]. Prior research suggests Phevaluator can produce results similar to case review [14,15]. Appendix 2 provides the steps followed to develop and evaluate outcome phenotype algorithms and further details about outcomes, phenotype algorithms, codes, and performance characteristics.
In the remaining part of this manuscript, Guillain-Barré syndrome (GBS), an adverse drug reaction that was added to the Ad26.CoV2·S product information in August 2021 16, is used as the example to illustrate the methods and the results of this rapid analytic framework.
2.4. Negative Control Outcomes
Negative controls were used to assess the validity of the results and to account for residual bias. Negative controls are outcomes believed not to be caused by any of the COVID-19 vaccines and therefore likely would not be flagged as a signal by this safety surveillance system. Effect size estimates for negative controls ideally should be close to the null. Negative control outcomes were defined as the first occurrence of the corresponding negative control SNOMED concept or any of its descendants. The 93 negative controls (Appendix 3) employed have been used previously [17,18].
2.5. Analytic Methods
Over multiple looks at the data, the analysis workflow employed both self-controlled case series (SCCS) and comparative cohort designs (Fig. 2). In both designs, relative risks of outcomes of interest were assessed during four overlapping at-risk periods following vaccination (1–14, 1–28, 1–42 and 1–90 days) except for anaphylaxis, which used an at-risk window starting on the vaccination day (0–2 days). Negative control outcome estimates were used to derive systematic error distribution, which were subsequently applied to empirically calibrate estimates and confidence intervals for Outcomes of Interest. The analytical code to characterize outcome phenotype definitions and to conduct analyses can be found here: https://github.com/ohdsi-studies/CovidVaccineRapidCycleAnalyses.
2.6. Self-Controlled Case Series (SCCS)
SCCS methodology [19], using conditional Poisson regression with covariates for at-risk status (exposure), pre-exposure (30 days prior to vaccination), and calendar time, was employed to assess the relative incidence of each specified outcome. This assessment contrasts outcomes occurring during an at-risk period immediately following Janssen COVID-19 vaccination (target Exposure) with outcome events happening in a not at-risk period, which can include time before or after vaccine exposure but excludes the defined at-risk period. Multiple SCCS analyses were conducted for each outcome with variations in analytic period start dates, use of only post-exposure data, and the period defined as at-risk (Appendix 3). All SCCS analyses used splines to adjust for calendar time.
2.7. Comparative Cohort
The comparative cohort analysis employed a Cox proportional hazards model to estimate the hazard ratio for a specified outcome. This estimation involves separate comparisons of the first dose of Janssen vaccination (target exposure) to the first doses of the Moderna and the Pfizer Covid-19 vaccines, which serve as two separate comparator exposure cohorts. For each outcome we defined a prior outcome lookback period, and one or more risk window start and end dates (Appendix 3). To adjust for potential confounding, we employ Large-Scale Propensity Scores (LSPS), a data-driven approach that includes all baseline covariates (between 60,000 and 100,000 variables) observed in the data including all demographics, conditions, drug exposures, and procedures observed in the year prior to vaccination in a regularized regression. Prior research suggests LSPS can identify potential confounders missed by experts [20], is superior to other data-driven approaches such as High-Dimensional Propensity Scores (HDPS) [20], and can adjust for confounders that are only indirectly measured [21]. To avoid including strong instrumental variables (IVs) we evaluate univariable correlations. Moreover, our prior research suggests including IVs is unlikely to lead to bias [22]. We apply variable ratio matching on the propensity score [23].
2.8. Evidence Interpretation Strategies
This work relies on two distinct evidence interpretation strategies (Discovery and Estimation) to support two use cases (Table 2).
The Discovery evidence interpretation strategy supports the use case of discovering potentially unknown associations for further investigation. The analysis relies on a large, prespecified set of target exposure and outcome of interest combinations. To limit the number of analyses included in Discovery, a single at-risk period for each outcome was considered. Because of uncertainty in possible time-to-onset, for a limited set of outcomes (GBS, myocarditis, myocarditis or pericarditis, and pericarditis) two at-risk periods were considered (Appendix 3). To correct for multiple testing, a Bonferroni correction was applied across outcomes, analyses, and databases. To correct for sequential testing across multiple looks at the data, the Maximized Sequential Probability Ratio Test (MaxSPRT) combined with empirical calibration by negative controls were applied [24]. A threshold was computed aiming at a family-wise type 1 error rate of 50 % to minimize the number of false negatives. Only those analyses that exceeded the predefined Discovery threshold were reviewed.
The Estimation evidence interpretation strategy fits the use case of quantifying the strength of association for a specific target exposure-outcome of interest pair and assesses statistical uncertainty. Estimation results were only reviewed for exposure-outcome pairs for which either Discovery or external evidence such as spontaneous reports suggested a potential effect. Effect-size estimates were combined across databases using Bayesian random-effects meta-analysis with non-normal likelihood approximation to avoid bias due to small counts [25]. Empirical calibration for the Estimation strategy was performed by 1) computing meta-analytic estimates for all negative control outcomes, 2) using these estimates to fit empirical null distributions, and 3) applying these to calibrate the meta-analytic point estimates, confidence intervals, and p-values for the outcomes of interest. Only effect-size estimates that passed pre-determined analytic diagnostic thresholds (see Analytic Diagnostics section) were included in the meta-analysis.
2.9. Analytic Diagnostics
For results to be unblinded, analyses had to pass a set of pre-determined, standardized diagnostics. Each diagnostic has a failure threshold, which prevents unblinding if not met. Diagnostics are briefly described below and detailed further in Appendix 4.
All analyses underwent the following analytic diagnostics:
1.
Power: The Minimum Detectable Relative Risk (MDRR) for a given observed sample size (after applying analytic exclusions) using an α = 0.05, β = 0.20 was calculated. The diagnostic failure threshold is an MDRR value of 10 or higher, rejecting outcomes with so little data that there's less than 20 % chance of identifying a relative risk of 10 (at a regular significance level of 5 %).
2.
Systematic error: A systematic error distribution using negative control estimates [26] was fit and summarized as the Expected Absolute Systematic Error (EASE) (see Appendix 4). An EASE of 0 means all variation in negative control estimates can be explained by random error. The diagnostic failure threshold is an EASE value of 0.25 or higher. When EASE = 0.25 and systematic error is centered on 0, a true relative risk of 1 has a 95 % probability of appearing to be anywhere between 0.54 and 1.85 due to systematic error.
For the SCCS design, these additional diagnostics were employed:
1.
Reverse causality: We tested if the risk of the outcome was already increased immediately prior to vaccine exposure (30 days before versus 30 days after). The diagnostic failure threshold for reverse causality test is a p-value <0.01, meaning there is less than 1 % chance of seeing such a high rate of the outcome prior to vaccination (relative to after vaccination) when the rates before and after are in fact the same.
2.
Time Trend: Within-subject confounding can occur when the rate of an outcome changes as a function of calendar time, which can occur due to data capture lags (incurred but not documented events are expected to occur close to the date of data extraction). This diagnostic assesses whether adjustment using a spline function over calendar time in the SCCS design was sufficient to control for these time trends. Monthly outcome rates after spline adjustment were computed and separately compared to the unadjusted mean monthly rate. A family-wise alpha of 0.05 was used, and a Bonferroni correction for the number of months was applied. If at least one monthly rate was statistically different from the mean monthly rate, then this diagnostic failed.
For the comparative cohort design, the following additional diagnostic were used:
1.
Attrition: Subjects could be excluded from the Janssen COVID-19 vaccine cohort, for example because they also received the comparator vaccine, did not have a match in the comparator cohort, or did not have the required exposure clean window. The attrition diagnostic would fail if more than 50 % of the Janssen COVID-19 cohort was excluded.
2.
Equipoise: A preference score, the linear transformation of the propensity score [27], was calculated. Then an equipoise measure was computed as the percent of the population with a preference score between 0.3 and 0.7. The diagnostic failure threshold is an equipoise value of less than 10 %, meaning less than 10 % of the population was somewhat likely to receive either vaccine based on their baseline characteristics
3.
Covariate Balance: We computed the standardized difference of mean (SDM) for every covariate used to balance any two exposure groups. The diagnostic failed when the absolute SDM of any covariate was 0.1 or higher, meaning we would stop if any of the 60,000 to 100,000 covariates was out of balance between the two exposure groups after propensity score matching.
`

export const JSONRapidCycleJanssen =
{
    getDbCohortMethodDataArgs: {
        studyPeriods: [
            {
                studyStartDate: 20210101,
                studyEndDate: null
            }
        ],
        restrictToCommonPeriod: true, 
        firstExposureOnly: true, 
        washoutPeriod: 365, 
        removeDuplicateSubjects: "remove all",
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
                riskWindowEnd: 14,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 28,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 42,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 90,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 0,
                startAnchor: "cohort start",
                riskWindowEnd: 2,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            }
        ]
    },
    propensityScoreAdjustment: {
        psSettings: [
            {
                matchOnPsArgs: {
                    maxRatio: 100,
                    caliper: 0.2, //default로 설정
                    caliperScale: "standardized logit"
                },
                stratifyByPsArgs: null,
            }
        ],
        createPsArgs: { //default
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
            },
        },
    },
    fitOutcomeModelArgs: { //default except modelType
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
    },
}