export type Anchor = "cohort start" | "cohort end";
export type RemoveDuplicate = "keep all" | "keep first" | "remove all";
export type CaliperScale = "propensity score" | "standardized" | "standardized logit";
export type BaseSelection = "all" | "target" | "comparator";
export type ModelType = "logistic" | "poisson" | "cox";
export type CvType = "auto";
export type NoiseLevel = "silent" | "quiet" | "noisy";

export interface MatchOnPsArgs {
  maxRatio: number;
  caliper: number;
  caliperScale: CaliperScale;
}
export interface StratifyByPsArgs {
  numberOfStrata: number;
  baseSelection: BaseSelection;
}
export interface Prior {
  priorType: "laplace";
  useCrossValidation: boolean;
}
export interface Control {
  tolerance: number;
  cvType: CvType;
  fold: number;
  cvRepetitions: number;
  noiseLevel: NoiseLevel;
  resetCoefficients: boolean;
  startingVariance: number;
}
export interface PsSetting {
  description: string;
  matchOnPsArgs: MatchOnPsArgs | null;
  stratifyByPsArgs: StratifyByPsArgs | null;
}

export type StudyDTO = {
  name: string;
  cohortDefinitions: {
    targetCohort: { id: number | null; name: string };
    comparatorCohort: { id: number | null; name: string };
    outcomeCohort: { id: number | null; name: string }[];
  };
  negativeControlConceptSet: { id: number | null; name: string };
  covariateSelection: {
    conceptsToInclude: { id: number | null; name: string }[];
    conceptsToExclude: { id: number | null; name: string }[];
  };
  getDbCohortMethodDataArgs: {
    studyPeriods: {
      studyStartDate: string | number | null;
      studyEndDate: string | number | null;
    }[];
    maxCohortSize: number;
  };
  createStudyPopArgs: {
    restrictToCommonPeriod: boolean;
    firstExposureOnly: boolean;
    washoutPeriod: number;
    removeDuplicateSubjects: RemoveDuplicate;
    censorAtNewRiskWindow: boolean;
    removeSubjectsWithPriorOutcome: boolean;
    priorOutcomeLookBack: number;
    timeAtRisks: {
      description?: string;
      riskWindowStart: number;
      startAnchor: Anchor;
      riskWindowEnd: number;
      endAnchor: Anchor;
      minDaysAtRisk: number;
    }[];
  };
  propensityScoreAdjustment: {
    psSettings: PsSetting[];
    createPsArgs: {
      maxCohortSizeForFitting: number;
      errorOnHighCorrelation: boolean;
      prior: Prior | null;
      control: Control | null;
    };
  };
  fitOutcomeModelArgs: {
    modelType: ModelType;
    stratified: boolean;
    useCovariates: boolean;
    inversePtWeighting: boolean;
    prior: Prior | null;
    control: Control | null;
  };
};

export const defaultDTO: StudyDTO = {
  name: "",
  cohortDefinitions: {
    targetCohort: { id: null, name: "" },
    comparatorCohort: { id: null, name: "" },
    outcomeCohort: [{ id: null, name: "" }],
  },
  negativeControlConceptSet: { id: null, name: "" },
  covariateSelection: {
    conceptsToInclude: [{ id: null, name: "" }],
    conceptsToExclude: [{ id: null, name: "" }],
  },
  getDbCohortMethodDataArgs: {
    studyPeriods: [
      {
        studyStartDate: null,
        studyEndDate: null,
      },
    ],
    maxCohortSize: 0,
  },
  createStudyPopArgs: {
    restrictToCommonPeriod: false,
    firstExposureOnly: false,
    washoutPeriod: 0,
    removeDuplicateSubjects: "keep all",
    censorAtNewRiskWindow: false,
    removeSubjectsWithPriorOutcome: true,
    priorOutcomeLookBack: 99999,
    timeAtRisks: [
      {
        description: "",
        riskWindowStart: 1,
        startAnchor: "cohort start",
        riskWindowEnd: 0,
        endAnchor: "cohort end",
        minDaysAtRisk: 1,
      },
    ],
  },
  propensityScoreAdjustment: {
    psSettings: [
      {
        description: "PS 1",
        matchOnPsArgs: {
          maxRatio: 1,
          caliper: 0.2,
          caliperScale: "standardized logit",
        },
        stratifyByPsArgs: null,
      },
    ],
    createPsArgs: {
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
  },
};

export function fillWithDefaults<T>(base: T, gold: any): T {
  if (gold === undefined) return base;
  if (gold === null) return gold as T;

  if (Array.isArray(base)) {
    if (Array.isArray(gold)) return gold as T;
    return base;
  }

  if (typeof base === "object" && base !== null) {
    const out: any = {};
    const keys = new Set([...Object.keys(base as any), ...Object.keys(gold ?? {})]);
    for (const k of keys) {
      const bVal = (base as any)[k];
      const gHas = Object.prototype.hasOwnProperty.call(gold, k);
      const gVal = gHas ? (gold as any)[k] : undefined;
      if (gHas) {
        if (
          bVal &&
          typeof bVal === "object" &&
          !Array.isArray(bVal) &&
          gVal &&
          typeof gVal === "object" &&
          !Array.isArray(gVal)
        ) {
          out[k] = fillWithDefaults(bVal, gVal);
        } else if (Array.isArray(bVal)) {
          out[k] = Array.isArray(gVal) ? gVal : bVal;
        } else {
          out[k] = gVal;
        }
      } else {
        out[k] = bVal;
      }
    }
    return out as T;
  }

  return gold as T;
}
