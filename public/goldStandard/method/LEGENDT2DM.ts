export const TEXTLEGENDT2DM =
  `
Methods
Data sources
In this study, we included 10 real-world data sources from the LEGEND-T2DM network, including 6 administrative claims and 4 electronic health record (EHR) databases across 4 countries from 1992 to 2021 (Central Illustration). These represent data from 6 national-level and 4 health-system data sets from the United States, as well as data sources from Germany, Spain, and the United Kingdom. The vast majority of patient records span from the mid-2000s to the present, covering 2 decades of T2DM treatment as well as the introduction of many second-line antihyperglycemic agents. All LEGEND-T2DM data sources were previously standardized to the Observational Health Data Sciences and Informatics Observational Medical Outcomes Partnership (OMOP) common data model version 5,28 which mapped international coding systems into standard vocabulary concepts. The use of the OMOP common data model allows a federated analysis of data, without patient-level data sharing, using consistent cohort definitions and study design. All data partners received institutional approval or exemption for their participation. Details of data sources are presented in Supplemental Tables 1 and 2.
Study design
We identified patients with T2DM and established cardiovascular disease (CVD) on metformin monotherapy who initiated on any of the drug ingredients within one of the SGLT2i, GLP-1 RA, DPP4i, or SU drug classes (Supplemental Methods, Supplemental Table 3).18 Each exposure cohort thus consisted of new users of each drug class. We require patients with T2DM and CVD to have at least 1 year of prior observation in the database, with at least 3 months of prior metformin use before initiating a second-line agent, and no prior exposure to a comparator second-line or other antihyperglycemic agent or >30 days of insulin exposure (Supplemental Methods). To ensure statistical power, we executed analyses for comparisons and data sources with at least 1,000 patients in each arm. To evaluate the comparative effectiveness of antihyperglycemic agents, we constructed target-comparator-database combinations, in which we compared one antihyperglycemic agent (target) with another agent (comparator) across data sources.
For this, we used a new-user cohort design for target and comparator agents within each data source in order to emulate the hypothetical target trial.29,30 Methodological principles in the study design were carefully constructed on the basis of the evidence by experts and were leveraged previously to minimize bias and improve reproducibility.27,31
Study outcomes
Across all data sources and pairwise exposure cohorts, we assessed the relative risks for 2 primary and 4 secondary cardiovascular outcomes. The 2 primary outcomes of interest were: 1) 3-point MACE, including acute MI, stroke, and sudden cardiac death (SCD); and 2) 4-point MACE, which additionally included hospitalization for HF. Secondary outcomes of interest included the 4 individual MACE components. We constructed outcome cohorts on the basis of previously developed phenotypes validated and tested in prior work (Supplemental Methods).31-36
For each outcome cohort, we included patients with no events prior to treatment initiation and defined continuous drug exposure as consecutive drug prescriptions with <30-day prescription gaps. We considered an on-treatment time-at-risk definition that follows a patient from treatment initiation to treatment discontinuation, which captures direct treatment effects while allowing for escalation with additional T2DM agents.
Statistical analysis
We used a systematic federated analytical framework to address residual confounding, publication bias, and P hacking.27,37 This framework uses data-driven, large-scale propensity score (PS) adjustment for measured confounding,38 a large set of negative control outcome experiments to address unmeasured confounding and systematic bias,39-41 study diagnostics to ensure validity and generalizability,41,42 and a principled meta-analysis approach to aggregate evidence across data sources. We used standardized vocabularies to construct consistent computable definitions of all study cohorts, covariates, and outcomes. We provide full disclosure of all hypotheses investigated and prespecify and report all analytical procedures in the published protocol of LEGEND-T2DM.27 To promote open science and avoid publication bias, we have disseminated all results in a publicly available R ShinyApp,43 and all analytical code is publicly available on GitHub.44
Within each data source, we estimated the relative risks for all 6 outcomes between each pair of new-user cohorts, taking one exposure cohort as the target and the other as a comparator. For each pairwise comparison and each data source, we adjusted for measured confounding and improved balance between cohorts by matching and stratifying on PS.45 We estimated the PS using a data-driven approach that adjusts for a broad range of predefined baseline patient characteristics through regularized regression.38 These characteristics included demographics, comorbidities, concomitant medication use, and health care use in the period before the initiation of the second-line antihyperglycemic agent. The choice of PS stratification vs matching was based on the approach that achieves a standardized mean difference (SMD) <0.15 across all covariates.46 When both stratification and matching provided sufficient balance, we preferred stratification over matching and thus reported results on the basis of stratification when available, as the former improves patient inclusion and, therefore, generalizability. We then used Cox proportional hazards models to estimate HRs of each outcome for each comparison, conditional on PS stratification or variable-ratio patient matching.
We also sought to address residual bias, which might persist in observational studies even after PS adjustment that controls for measured confounding.39,40 For this, we conducted negative control (falsification) outcome experiments for each comparison and outcome, in which the null hypothesis of no differential effect (ie, HR: 1) is believed to be true for each outcome. We selected 100 negative controls through a data-driven algorithm that identifies OMOP condition concept occurrences with similar prevalence to the outcomes of interest that lack evidence of association with exposures in published literature, drug-product labels, and spontaneous reports, which we then confirmed by expert review (Supplemental Methods).47 We list these negative controls in Supplemental Table 4. From these negative control experiments, we learned an empirical null distribution that informs residual study bias; that is, a deviation from the empirical null across all outcomes represents a quantitative surrogate for the residual bias. We calibrated each original HR estimate to compute a calibrated HR estimate and 95% CI.39 We declared an HR as significantly different from the null if the calibrated P value is <0.05 without considering multiple testing corrections.
We assessed, while blinded to the results, study diagnostics to ensure reliability and generalizability for all comparisons and report only estimates that pass the diagnostics.41,42 These study diagnostics included: 1) the minimum detectable risk ratio as a metric for statistical power; 2) preference score distributions between the target and comparator cohorts to evaluate empirical equipoise and population generalizability; and 3) cohort balance before and after PS adjustment, defined by the absolute SMDs on extensive patient characteristics. A study passed diagnostics if the minimum detectable risk ratio was <4 and >25% of patients had preference scores between 0.3 and 0.7 in both arms and a maximum SMD <0.15 after PS adjustment. Additional diagnostics for visual examination included: 4) calibration plots on negative control outcomes to examine residual bias; and 5) Kaplan-Meier plots, all publicly available on the R ShinyApp,43 to visually inspect proportionality assumptions for the Cox models.
We reported all HR estimates, their 95% CIs, and P values postcalibration for studies that passed diagnostics. To aggregate evidence across nonoverlapping data sources, we combined all calibrated HR estimates for each comparison using a random-effects meta-analysis approach.48–50 Furthermore, for each comparison, we performed leave-one-out meta-analyses by calculating the calibrated risk estimates while systematically removing one data source at a time.
`

export const JSONLEGENDT2DM = {
    getDbCohortMethodDataArgs: {
        studyPeriods: [
            {
                studyStartDate: "19920101",
                studyEndDate: "20211231",
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
    ],
  },
  propensityScoreAdjustment: {
    psSettings: [
        {
            matchOnPsArgs: {
                maxRatio: 100,
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
