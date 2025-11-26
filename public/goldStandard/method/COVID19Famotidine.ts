export const TEXTCOVID19Famotidine =
    `
METHODS
We conducted a prevalent-user comparative retrospective cohort study measuring the association between famotidine use and severity of COVID-19 outcomes among patients hospitalized with COVID-19 in the United States. This study was approved under protocol (CCSDIH002924; https://github.com/ohdsi-studies/Covid19EstimationFamotidine/blob/master/Protocol/Covid19EstimationFamotidineProtocol.pdf).

Data source
The study population comprised hospitalized patients with a diagnosis of COVID-19 available in the COVID-19 Premier Hospital Database (PHD). The PHD contains complete clinical coding, hospital cost, and patient billing data from approximately 700 hospitals throughout the United States. It captures from 20% to 25% of all inpatient admissions in the United States. Premier collects deidentified data from participating hospitals in its healthcare alliance. The hospitals included are nationally representative based on bed size, geographic region, location (urban/rural), and teaching hospital status. The database contains medications administered during the hospitalization; laboratory, diagnostic, and therapeutic services; and primary and secondary diagnoses for each patient's hospitalization. Identifier-linked enrollment files provide demographic and payor information. Detailed service-level information for each hospital day is recorded; this includes details on medication and devices received. All data were standardized to the Observational Health and Data Sciences and Informatics Observational Medical Outcomes Partnership Common Data Model version 5.3. The full description of the extract, transform, and load of the data can be found here: https://github.com/OHDSI/ETL-CDMBuilder/blob/master/man/PREMIER/Premier_ETL_CDM_V5_3.doc. PHD contains deidentified patient information, is Health Insurance Portability and Accountability Act compliant, and is considered exempt from institutional review board approval (10).

Study period and follow-up
The study period started February 1, 2020, and ended May 30, 2020, the latest available date for all data in 2020. Figure 1 illustrates the retrospective study design schematic. As illustrated in Figure 1, follow-up for each of the cohorts started at an index date defined by the first inpatient admission (day 0). Time at risk was defined based on the intention-to-treat principle starting 1 day after admission and continuing up until the first of outcome of interest, loss to follow up, or 30 days after admission.

Study population
We included patients aged 18 years or older with an inpatient visit occurring after February 1, 2020, with a condition, measurement, or observation indicative of COVID-19 during or within 21 days before admission. Patients with evidence of intensive services (i.e., mechanical ventilation, tracheostomy, or extracorporeal membrane oxygenation) at or within 30 days before admission were excluded.

Exposures, outcomes, and confounders
Using a “nonusers” (i.e., unexposed group) group as a comparator can potentially increase the risk of confounding since the persons who used Famotidine are likely different than those who did not on several factors (11). To mitigate that risk, we included 2 active comparator groups. PPI users are expected to be clinically similar to famotidine users, given that both drugs have similar indications. However, whether PPI happens to have its own effect on COVID-19 outcome, an artificial relative difference may be observed. We included hydroxychloroquine users as another comparative group, given the accumulating evidence on the null effect of hydroxychloroquine on COVID-19 outcome, making it a possible ideal negative control for the analysis (12).

Famotidine, PPI, and hydroxychloroquine exposure groups were defined as patients dispensed any medication containing 1 of the 3 drugs of interest—as an ingredient—on the day of admission. Patients who received both famotidine and any of the comparator drugs on day of admission were excluded. The famotidine nonuser group was derived from the same source population (participants) with no history of exposure to any drug with famotidine as an active ingredient before or on the day of admission.

Outcomes of interest were death and death or intensive services (combined). Death was identified based on patient discharge status within admission records and International Classification of Diseases, Ninth Revision diagnosis codes provided by the source data. Only deaths that occur during hospitalization were captured. No additional death adjudication was performed.

Intensive services were defined as any condition, procedure, or observation code indicative of mechanical ventilation, tracheostomy, or extracorporeal membrane oxygenation. The code list used to identify study participants, exposures, and outcomes can be found at: https://github.com/ohdsi-studies/Covid19EstimationFamotidine/blob/master/Protocol/Annex%20I%20-%20Concept%20Set%20Expressions.xlsx.

Statistical methods
To adjust for potential measured confounding and improve the balance between comparison cohorts, we built large-scale propensity score (PS) models for each comparison using regularized regression (13). We used a Laplace previously (LASSO) with the optimal hyperparameter to fit the model, determined through 10-fold cross validation in which the outcome is a binary indicator for the potential comparator. This process used a large set of predefined baseline patient characteristics, including patient demographics (i.e., gender, age, and index month) and all observed conditions within 30 days before or on admission. For computational efficiency, we excluded all features that occurred in fewer than 0.1% of patients within the target and comparator cohorts before PS model fitting. For the main analysis, we stratified into 5 PS strata and used conditional Cox proportional hazards models to estimate hazard ratios (HRs) between target and alternative comparator treatments for the risk of each outcome. The regression for the outcome models conditioned on the PS strata with treatment as the sole explanatory variable.

As a sensitivity analysis, we used a 1:1 PS matching and used an unconditional Cox proportional hazards model to estimate HRs in the matched set. We declared a HR as significantly different from no effect when its P < 0.05 without correcting for multiple testing.

Blinded to the results, the study team evaluated study diagnostics for these treatment comparisons to assess whether they were likely to yield unbiased estimates. The suite of diagnostics included (i) minimum detectible risk ratio, (ii) preference score (a transformation of the PS that adjusts for prevalence differences between populations) distributions to evaluate empirical equipoise (14) and population generalizability, and (iii) extensive patient characteristics to evaluate cohort balance before and after PS-adjustment. We defined target and comparator cohorts to achieve sufficient balance if all after-adjustment baseline characteristics return absolute standardized mean differences (SMD) <0.1 (15).

We conducted this study using the open-source Observational Health and Data Sciences and Informatics CohortMethod R package (https://ohdsi.github.io/CohortMethod/) with large-scale analytics made possible through the Cyclops R package (https://ohdsi.github.io/Cyclops/) (16). The prespecified protocol and start-to-finish open and executable source code are available at: https://github.com/ohdsi-studies/Covid19EstimationFamotidine. To promote transparency and facilitate sharing and exploration of the complete result set, an interactive web application (https://data.ohdsi.org/Covid19EstimationFamotidine/) serves up study diagnostics and results for all study effects.
`
export const JSONCOVID19Famotidine = {
    getDbCohortMethodDataArgs: {
        studyPeriods: [
            {
                studyStartDate: "20200201",
                studyEndDate: "20200530",
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
                riskWindowEnd: 30,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default 설정
            },
        ],
    },
    propensityScoreAdjustment: {
        psSettings: [
            {
                matchOnPsArgs: null,
                stratifyByPsArgs: {
                    numberOfStrata: 5,
                    baseSelection: "all" //default 설정
                }
            },
            {
                matchOnPsArgs: {
                    maxRatio: 1,
                    caliper: 0.2, //default 설정
                    caliperScale: "standardized logit" //default 설정
                },
                stratifyByPsArgs: null
            },
        ],
        createPsArgs: {
            maxCohortSizeForFitting: 250000, //default 설정
            errorOnHighCorrelation: true, //default 설정
            prior: {
                priorType: "laplace",
                useCrossValidation: true,
            },
            control: {
                tolerance: 2e-7, //default 설정
                cvType: "auto", //default 설정
                fold: 10,
                cvRepetitions: 10, //default 설정
                noiseLevel: "silent", //default 설정
                resetCoefficients: true, //default 설정
                startingVariance: 0.01, //default 설정
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
