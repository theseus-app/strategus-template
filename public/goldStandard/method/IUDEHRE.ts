export const TEXTIUDEHRE =
  `
Observational Health Data Sciences and Informatics is an international, open-science collaborative of more than 220 health care organizations with a mission to improve health through the use of large-scale observational research. Observational Health Data Sciences and Informatics maintains the Observational Medical Outcome Partnership Common Data Model, which is a deep informational model that specifies how to encode and store clinical data in a standard format, enabling standardized analysis methods on data within the Observational Health Data Sciences and Informatics network. The model's schema represents structured data such as patient demographics, visits, conditions, procedures, laboratory results, vitals, and medications. Observational Health Data Sciences and Informatics maintains more than 100 vocabularies and the mappings between them to encode all clinical data. Previous studies have evaluated various clinical data models and have determined Observational Medical Outcome Partnership the “best of breed” for comparative effectiveness research.5 The Columbia University Irving Medical Center participates in Observational Health Data Sciences and Informatics and provided an Observational Medical Outcome Partnership database for this analysis. We leveraged the open-source tools provided by Observational Health Data Sciences and Informatics to perform a comparative effectiveness analysis of Cu IUDs to LNG-IUSs with respect to cervical neoplasm incidence.6–10

Columbia University Irving Medical Center's anonymized Observational Medical Outcome Partnership database comprises a mixture of inpatient and outpatient visits, spans a time period of four decades (1980s–present), and represents a population of 6 million patients. The data in the Observational Medical Outcome Partnership database were extracted from Columbia University Irving Medical Center and New York-Presbyterian Hospital's electronic health record systems. The Columbia University Irving Medical Center Observational Medical Outcome Partnership database used in this analysis was version 5.2 of the Observational Medical Outcome Partnership Common Data Model. The Columbia University Irving Medical Center has institutional approval for use of the Observational Health Data Sciences and Informatics tools (IRB#AAAO7805), however additional IRB approval is not necessary to access anonymized data.

We implemented a retrospective, observational, cohort study that compared a target cohort of Cu IUD to LNG-IUS users. We used the date of a first IUD placement as the index date for the study. All patients had continuous observation in our database for at least 365 days before IUD insertion. We restricted our cohorts to female patients who were 45 years or younger at the time of IUD placement. We excluded women with a history of endometrial or cervical neoplasms or who had a prior IUD placement. By default, women were in the Cu IUD cohort unless documentation of an LNG-IUS appeared in the database. We used a collection of procedure codes, such as SNOMED 65200003 (“Insertion of intrauterine contraceptive device”) or CPT 58300 (“Insertion of intrauterine device [IUD])” to identify IUD placement. Whether the IUD placed was a Cu IUD or an LNG-IUS was determined by whether an LNG-IUS was identified by RxNorm codes, such as RxNorm 807283 (“Levonorgestrel 0.000833 MG/HR Intrauterine System”).

The outcome was a high-grade cervical neoplasm diagnosis. We chose primary cervical neoplasms that were malignant or had a high association with malignancy, such as cervical intraepithelial neoplasia grade II or III. The outcome of a high-grade cervical neoplasm diagnosis was identified by a condition code, such as SNOMED code 372024009 (“Primary malignant neoplasm of uterine cervix”). We excluded cervical polyps, cervical intraepithelial neoplasia grade I, and metastatic spread of a neoplasm to the cervix. As part of our data-validation process, we correlated the condition code with a biopsy diagnosis.

We used a collection of procedure codes such as SNOMED 171149006 (“Screening for malignant neoplasm of cervix”) to identify subsequent cervical screening and HCPCS Q0091 (“Screening Papanicolaou smear; obtaining, preparing and conveyance of cervical or vaginal smear to laboratory”) to identify preventive screening visits.

The time at risk was from 30 days to 15 years after IUD placement. The study window was restricted to all IUD placements that occurred on or after January 1, 2003, to account for a lag between U.S. Food and Drug Administration approval of the LNG-IUS in 2000 and its regular use in our clinical practice. We used an any-use design with no censoring events. Follow-up time was defined by continued observation of the patient in the Columbia University Irving Medical Center database. Observation ended either when a patient developed the outcome or had no further observation data in the Columbia University Irving Medical Center database, which contains observations though December 2018. We excluded any woman who had both a Cu IUD and an LNG-IUS placed during the study period. We also performed a subgroup analysis including only those cervical neoplasm diagnoses that occurred at least 1 year after IUD placement.

To reduce potential confounding due to imbalance between the Cu IUD and LNG-IUS cohorts in baseline covariates, we used propensity score models with a regularized logistic regression. This algorithm determined which among more than 10,000 baseline covariates should be included in the propensity score model. The covariates represented demographic characteristics, prior conditions, drug dispensing, procedures, and visit counts. We stratified or matched patients by propensity score and used a Cox proportional hazards model to determine the relative risk for cervical neoplasms between the Cu IUD and LNG-IUS cohorts. Our methods for reducing confounding are similar to what has been described recently by our research community.11

We executed diagnostics to determine whether the analysis could be appropriately conducted. The diagnostics included propensity score distribution, covariate balance before and after propensity score matching, and estimation for negative controls to assess residual error. Additionally, negative control diagnoses that were unrelated to the exposures were used to evaluate the potential effect of residual systematic error in the study design, and to facilitate empirical calibration of the P-value and CI for the exposures and outcome of interest.

Negative control diagnoses were unrelated to IUD exposure and were assumed to be equally distributed between the cohorts. The distribution of effect estimates across all negative controls was used to fit an empirical null distribution, which modeled the observed residual systematic error. The empirical null distribution was applied to the Cu IUD and LNG-IUS exposure and cervical neoplasm outcome to calibrate the P-value. We selected 123 negative control outcomes. We fit a systematic error model and performed CI calibration.12

We compared the Cu IUD cohort to the LNG-IUS cohort for the hazards of cervical neoplasm during the time-at-risk by applying a Cox proportional hazards model. Patients were matched on the propensity score by two methods: stratification and 1:1 matching.13,14 Propensity score distributions and Kaplan-Meier estimates were plotted.

To validate our findings, we did a manual chart review of a sample of an identified patient database. In accordance with Columbia University IRB approval (Protocol# AAAS5403), we selected 115 patients who had high-grade cervical neoplasms in the Cu IUD and LNG-IUS cohorts in our institution to validate that we had identified Cu IUD users, LNG-IUS users, and high-grade cervical neoplasms accurately. Eighty cases were in the Cu IUD cohort and 35 cases were in the LNG-IUS cohort.
`

export const JSONIUDEHRE = {
    getDbCohortMethodDataArgs: {
        studyPeriods: [
            {
                studyStartDate: "20030101",
                studyEndDate: null,
            },
        ],
        restrictToCommonPeriod: false, 
        firstExposureOnly: true, 
        washoutPeriod: 365, 
        removeDuplicateSubjects: "remove all",
        maxCohortSize: 0, //default
    },
    createStudyPopArgs: {
        censorAtNewRiskWindow: false, //default로 설정
        removeSubjectsWithPriorOutcome: false,
        priorOutcomeLookBack: 99999,
    timeAtRisks: [
      {
        riskWindowStart: 30,
        startAnchor: "cohort start",
        riskWindowEnd: 5475,
        endAnchor: "cohort start",
        minDaysAtRisk: 1 //default 설정
      },
      {
        riskWindowStart: 365,
        startAnchor: "cohort start",
        riskWindowEnd: 5475,
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
            matchOnPsArgs: null,
            stratifyByPsArgs: {
                numberOfStrata: 5,
                baseSelection: "all" //default로 설정
            },
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
