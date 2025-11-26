export const TEXTTicagrelorClopidogrel =
    `
Methods
Study Design and Data
This retrospective observational study, which used 2 electronic health record (EHR)-based databases in the United States (US Optum EHR; US IQVIA Hospital) and 1 nationwide administrative claims database in South Korea (Health Insurance Review and Assessment service [HIRA]) from November 2011 to March 2019, was approved by the institutional review board of Ajou University (AJIRB-MED-MDB-17-289). Written informed consent was waived due to the deidentified nature of the databases. Analyses of deidentified data were performed in accordance with local laws and regulation with approvals from respective scientific and ethics committees. All 3 databases were standardized to the Observational Medical Outcomes Partnership common data model, version 5 (eMethod 1 in Supplement 1).13

We performed distributed network analyses.14 The statistical analytic protocol (Supplement 2) was prespecified before execution. According to this protocol, the study package for the entire process was built on the OHDSI Methods Library in R and released with a Docker image providing a computational reproducible environment.15 This package was executed locally inside a firewall for each database. Next, the predesignated statistical results (without patient-level information) were shared for interpretation and database-level meta-analysis.

Study Population and Exposure
We identified adult patients (aged ≥20 years) who underwent PCI for the first time (according to their medical history) following a diagnosis of ACS. The index date was defined as the date of PCI. To avoid left censoring, we excluded patients who had been enrolled in the database for less than 1 year before the index date. The other exclusion criteria were a history of ischemic stroke, hemorrhagic stroke, or gastrointestinal bleeding any time before the index date; and a prescription of prasugrel, clopidogrel (for the ticagrelor group), or ticagrelor (for the clopidogrel group) during the 30 days preceding the index date. Further details of the cohort definition are presented in eMethod 2 in Supplement 1. Ticagrelor or clopidogrel use was ascertained from prescription records within the 7 days before the index date.

Outcomes
The primary outcome was net adverse clinical events (NACE), which included ischemic events (recurrent acute myocardial infarction [AMI], revascularization, or ischemic stroke) and hemorrhagic events (hemorrhagic stroke or gastrointestinal bleeding) from day 1 to 365 days after PCI. The secondary outcomes consisted of an extended definition of NACE that included mortality (NACE or mortality), all-cause mortality, composite ischemic events (recurrent AMI, revascularization, or ischemic stroke), composite hemorrhagic events (hemorrhagic stroke or gastrointestinal bleeding), and individual components of the primary outcome within 1 year. The 1-year risk of dyspnea, a well-known adverse event of ticagrelor, was also evaluated.

As a post hoc analysis, we compared the occurrence of cardiovascular-related mortality and major adverse cardiovascular events (MACE, including cardiovascular mortality, recurrent AMI, and stroke) in the Optum EHR and HIRA databases (this information was not available in the IQVIA database). Cardiovascular-related mortality was identified by a death record with at least 1 cardiovascular-related diagnosis (AMI, stroke, sudden cardiac death, or hospitalization for heart failure) in the 30 days before death as described previously.14 Further details of the outcome definitions, including International Statistical Classification of Diseases and Related Health Problems, Tenth Revision; International Classification of Diseases, Ninth Revision, Clinical Modification; and Current Procedural Terminology (CPT)-4 codes, and the results of validation are provided in eMethod 3 in Supplement 1.

Statistical Analysis
We used regularized logistic regression16,17 to estimate the propensity scores, which used more than 10 000 baseline patient characteristics between the ticagrelor and clopidogrel groups, including all the available demographic characteristics, as well as the medical, medication, procedure, and device exposure history in each database. For the Optum EHR, we also included race and baseline laboratory values in the logistic regression model because they were available and can be associated with the selection of the drug and clinical outcomes. Categories of race were determined according to self-reported responses in the EHR: African American, Asian, White, and Other/Unknown. However, due to statistical deidentification rules for race based on geography, if there was a small number of one group, such as Asians in a particular region, they were categorized as Other/Unknown.

All variables except laboratory values were binary (yes/no) and all missing binary variables were considered as not present and coded as no. The missingness in laboratory values were matched between the ticagrelor and clopidogrel groups and missing values were not imputed. Next, the study populations were matched using one-to-one greedy matching of the propensity score.18 We used Cox proportional hazard regression models to estimate the association of exposures with outcomes. The Cox proportionality assumption was tested based on the Schoenfeld residuals, and no relevant violations were found for the primary outcome. Patients were censored when they were no longer observed in the database, while they remained in the primary analysis if they discontinued the allocated drug or switched the drug within the first year. We then performed random-effects meta-analysis to calculate summary hazard ratio (HR) pooling effect estimates across the databases.19

Sensitivity Analyses
To assess the robustness of the findings, a large set of sensitivity analyses were conducted using differing definitions of the at-risk time window, the statistical approaches, the study population, and the outcomes. First, in addition to one-to-one propensity score matching, 2 additional propensity score adjustments were performed: (1) variable-ratio propensity score matching with a maximum ratio of 10; and (2) propensity score stratification using deciles of the propensity score distribution. Second, 2 more time-at-risk windows were applied—a 5-year period and an on-treatment period. The on-treatment period was defined as the time from 1 day after the index date until the end of persistent exposure to the drug, allowing a 7-day gap or the end of a patient’s record. Third, in every analysis, we added sensitivity analyses with a “blanking period” rule that excluded the occurrence of the outcome within the initial 28 days after PCI because such early outcomes may reflect duplicated diagnoses, either due to transfer between hospitals or clinical coding practices.20

Fourth, since a diagnosis code–based outcome definition may include false-positive cases, we applied a restricted definition of outcome that only considered primary diagnoses. Fifth, we conducted an additional analysis to apply an identical study period to all databases, namely March 2013 and December 2016, because ticagrelor was covered by national insurance in South Korea from March 2013. Sixth, as a post hoc analysis and since the individual events in NACE are not equivalent, we calculated the weighted incidence of NACE, weighted by the HR of individual events, for 1-year mortality after an event relative to death after ischemic stroke (eMethod 4 in Supplement 1).

In addition, we employed a total of 96 falsification end points using a data-rich algorithm to quantify systematic error (eMethod 5 in Supplement 1).21,22 These outcomes are not known to be related to either ticagrelor or clopidogrel (eg, chalazion and pneumothorax). We performed empirical calibration of the CIs by fitting an empirical null distribution to point estimates of these falsification end points.23 Overall, 144 different analyses (3 statistical approaches × 3 time windows × 2 blanking period rules × 2 outcome definitions × 2 study periods × 2 empirical calibrations) were performed for each outcome.

To assess the association of ticagrelor vs clopidogrel with clinical outcomes in the United States, we also performed a post hoc meta-analysis that was restricted to the 2 US databases. To measure adherence, the medication possession ratio was calculated by summing days supplied of the allocated drug divided by days in the time-at-risk period (ie, a value of 1 indicates 100% adherence) using the HIRA database, which provides the complete longitudinal drug usage history after the index date.24 We compared the incidences of individual ischemic or hemorrhagic outcomes in the ticagrelor group from our study with those from the recent head-to-head randomized trial (TICA-KOREA).25 A prespecified 2-sided P value of less than .05 was considered to indicate statistical significance. Because of the potential for type 1 error due to multiple comparisons, findings for analyses of secondary outcomes should be interpreted as exploratory. All analyses were performed using R programming language version 3.5.1. We followed the Strengthening the Reporting of Observational Studies in Epidemiology (STROBE) reporting guideline.
`

export const JSONTicagrelorClopidogrel = {
    getDbCohortMethodDataArgs: {
        studyPeriods: [
            {
                studyStartDate: "20111101",
                studyEndDate: "20190331",
            },
            {
                studyStartDate: "20130301",
                studyEndDate: "20161231",
            },
        ],
        restrictToCommonPeriod: true, 
        firstExposureOnly: false, 
        washoutPeriod: 0, 
        removeDuplicateSubjects: "keep first",
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
                riskWindowEnd: 365,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 1825,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 0,
                endAnchor: "cohort end",
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 29,
                startAnchor: "cohort start",
                riskWindowEnd: 365,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 29,
                startAnchor: "cohort start",
                riskWindowEnd: 1825,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 29,
                startAnchor: "cohort start",
                riskWindowEnd: 0,
                endAnchor: "cohort end",
                minDaysAtRisk: 1, //default로 설정
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
                    maxRatio: 10,
                    caliper: 0.2, //default로 설정
                    caliperScale: "standardized logit" //default로 설정
                },
                stratifyByPsArgs: null,
            },
            {
                matchOnPsArgs: null,
                stratifyByPsArgs: {
                    numberOfStrata: 10,
                    baseSelection: "all" //default로 설정
                },
            },
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
};
