export const TEXTAlzheimerDonepezil = `
This study was a secondary data analysis using existing data, the study was approved by the University of Florida Institutional Review Board (IRB201902362).

The target Alzheimer’s disease (AD) trial and its characteristics
Although there is no cure for AD yet, the U.S. FDA approved two classes of medications: (1) cholinesterase inhibitors and (2) memantine, to treat the symptoms of dementia. Donepezil (Aricept®), a cholinesterase inhibitor, was the most widely tested AD drug and approved for all stages of AD. The target trial NCT0047820528 is a Phase III double-blind, double-dummy, parallel-group comparison of 23 mg donepezil sustained release (SR) with the 10 mg donepezil immediate release (IR) formulation (marketed as the SOC) in patients with moderate to severe AD. Patients who have been taking 10 mg IR (or a bioequivalent generic) for at least 3 months prior to screening were recruited. The original trial consisted of 24 weeks of daily administration of study medication, with clinic visits at screening, baseline, 3 weeks (safety only), 6 weeks, 12 weeks, 18 weeks, and 24 weeks, or early termination. Patients received either 10 mg donepezil IR in combination with the placebo corresponding to 23 mg donepezil SR, or 23 mg donepezil SR in combination with the placebo corresponding to 10 mg donepezil IR. A total of 471 and 963 patients were enrolled from approximately 200 global sites (Asia, Oceania, Europe, India, Israel, North America, South Africa, and South America). The results of the original trial yielded that donepezil 23 mg/d was associated with greater benefits in cognition compared with donepezil 10 mg/d and led to the FDA approval of the new 23 mg dose form for the treatment of AD in 201029, despite the debate on whether the 2.2 point of cognition improvement (on a 100 point scale) over the 10 mg dose form is sufficient22,30.

In our simulation, we followed the detailed study procedures outlined by Farlow et al.29 to formulate our simulation protocol, including the treatment regimen, population eligibility, and follow-up assessments for SAEs. Table 4 describes how the original trial design was followed in our simulation.

Real-world patient data (RWD) from the OneFlorida network
The OneFlorida data contain robust longitudinal and linked patient-level RWD of ~15 million (>60%) Floridians, including data from Medicaid claims, cancer registries, vital statistics, and EHRs from its clinical partners. As one of the PCORI-funded clinical research networks in the national PCORnet, OneFlorida includes 12 healthcare organizations that provide care through 4100 physicians, 914 clinical practices, and 22 hospitals, covering all 67 Florida counties. The OneFlorida data is a Health Insurance Portability and Accountability Act (HIPAA) limited data set (i.e., data are not shifted and location data are available) that contains detailed patient characteristics and clinical variables, including demographics, encounters, diagnoses, procedures, vitals, medications, and labs31. We focused on the structured data immediately available to us formatted according to the PCORnet common data model (PCORnet CDM)32.

Cohort identification: the target population, the study population, and the trial not eligible population
From the OneFlorida data, we identified three populations: the target population (TP), the study population (SP), and the trial not eligible population (NEP) for the target trial following the process shown in Fig. 1a, and the relationship between these populations are displayed in Fig. 1b. The true TP should be those that will benefit from the drug, thus, should be broader as patients with AD in general. However, as patients who were not treated with donepezil in real world would not have any safety or effectiveness data of the drug in RWD, the effective TP of interest is a constrained subset: patients who (1) had the disease of interest (i.e., AD), and (2) had used the study drug (i.e., donepezil) for a specific time period according to the study protocol. The 10 mg donepezil is only in IR form while the 23 mg donepezil is exclusively in SR form, so we used the corresponding RxNorm concept unique identifier (RXCUI) and the National Drug Code (NDC) to identify the two groups (i.e., 10 mg vs. 23 mg) of patients in our data25,26. We then identified the SP (i.e., patients who met both the TP criteria and the trial eligibility criteria) and NEP (i.e., patients who meet the TP criteria but do not meet the trial eligibility criteria) by applying the eligibility criteria of the target trial to the TP. To do so, we analyzed the target trial’s eligibility criteria and determined the computability of each criterion. A criterion is computable when its required data elements are available and clearly defined in the target patient database (i.e., the OneFlorida data in our study). Then, we manually translated the computable criteria into database queries against the OneFlorida database. We assumed that all patients met the non-computable criteria (e.g., “written informed consent”), which is a limitation of our study. The full list of eligibility criteria and their computability are listed in Supplementary Table 2. We first decomposed each criterion (e.g., “Patients with dementia complicated by other organic disease or Alzheimer’s disease with delirium”) into smaller study traits (e.g., “dementia complicated by other organic disease” and “Alzheimer’s disease with delirium”). We then checked whether each of the study trait is computable based on the OneFlorida data as shown in Supplementary Table 2. We then used the computable study traits to determine patients’ eligibility. Many of the incomputable study traits are not clinically relevant for our studies (e.g., “No caregiver available to meet the inclusion criteria for caregivers.”). Nevertheless, how computability of these study traits affects the trial simulation results—a limitation of our current study—warrant further investigations in future studies.

Definition and identification of serious adverse events (SAE) from EHRs
The target trial used Severe Impairment Battery (SIB) and the Clinician’s Interview-Based Impression of Change Plus Caregiver Input scale (CIBIC+; global function rating) to assess the efficacy of donepezil in AD patients. Because these effectiveness data are not readily available in the structured EHR data, we focused on assessing drug safety in terms of the occurrences of SAEs. To define an SAE, we followed the FDA33 definition of SAEs and the Common Terminology Criteria for Adverse Events (CTCAE) version 5—a descriptive terminology for Adverse Event (AE) reporting. In CTCAE, an AE is any “unfavorable and unintended sign, symptom, or disease temporally associated with the use of a medical treatment or procedure that may or may not be considered related to the medical treatment or procedure,” and the AEs are organized based on the System Organ Class (SOC) defined in Medical Dictionary for Regulatory Activities (MedDRA34). CTCAE also provides a grading scale for each AE into Grade 1 (mild), Grade 2 (moderate), Grade 3 (severe or medically significant but not immediately life-threatening), Grade 4 (life-threatening consequences), and Grade 5 (death).

We mapped each reported SAE in the trial results section of the target trial NCT00478205 on ClinicalTrails.gov at https://www.clinicaltrials.gov/ct2/show/results/NCT00478205 to the CTCAE term and identified the severity based on the CTCAE grading scale. We considered an AE as SAE if it meets the criteria for Grade 3/4 (results in hospitalization), and Grade 5 (death). As shown in Fig. 2, to count as an SAE related to donepezil, the SAE event has to occur within 24 weeks after the first donepezil prescription (which is the same follow-up period as the original trial). Note that we excluded chronic conditions that happened before the study, for example, different types of cancer.

Trial simulation
Table 4 shows our design of the simulated trial corresponding to the original target trial. Based on the calculation from the original trial27, a sample size of 400 and 800 were needed for the 10 and 23 mg arms, respectively. We first simulated the control arm of the standard therapy (i.e., the 10 mg arm of the original trial), where we have a sufficiently large sample size from the OneFlorida data. We designed our simulation based on the sample size of the arm in the original trial (N = 400), and tested two different sampling approaches: (1) random sampling, and (2) proportional sampling controlling for race distribution.

Even though we did not find a sufficient number of patients who took 23 mg donepezil in our data, we still simulated both case–control arms using the same sampling strategy in the one-arm simulation that yielded the closest effect sizes compared with the original trial. We explored two different scenarios with different sample sizes: (1) the ratio of the number of subjects in the 23 mg arm to the 10 mg arm was set as 1:1; and (2) the ratio was set as 1:3. Because of the limited number of individuals who took the 23 mg form, we can only increase the number of subjects in the 10 mg arm in the second sample size scenario. We used PSM to simulate randomization. The variables used for PSM included age, gender, race, and CCI (i.e., as a proxy for baseline overall health of the patient) prior to baseline. Specifically, we fitted a logistic regression model using different treatments (i.e., case vs. control) as the outcome variable and age, gender, race, and CCI as covariates to generate the logistic probabilities of propensity scores of individuals in the two comparison groups and then used the nearest neighbor method to carry out the mapping process. The two arms were matched with the propensity scores with a 1:1 or 1:3 ratio.

Specifically, we first used proportional sampling to extract a sample of patients for the 23 mg SP using the same race distribution as in the original trial, and then identified a matched sample for the 10 mg SP using PSM. We then calculated the SAEs in the 10 mg vs. 23 mg arms as the safety outcomes. The simulation process was performed 1000 times with bootstrap sampling with replacement, and the mean value and 95% CI of each bootstrap sample were calculated to generate the overall estimates. We focused on comparing the average number of SAE per patient, the overall SAE rates (i.e., how many patients had SAEs), and stratified the analysis by major SAE categories according to the CTCAE guideline. The effects of PSM were evaluated by examining the distributions of propensity scores using jitter plot (Supplementary Table 1 and Supplementary Fig. 1).
`

export const JSONAlzheimerDonepezil = {
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
        riskWindowEnd: 180,
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
      },
      {
        matchOnPsArgs: {
          maxRatio: 3,
          caliper: 0.2, //default 설정
          caliperScale: "standardized logit" //default 설정
        },
        stratifyByPsArgs: null
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
    modelType: "logistic",
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
