export interface ScenarioCommands {
  build: string;
  lint: string;
  test: string;
}

export interface ScenarioConstraints {
  noAny: boolean;
  singleCodeBlock: boolean;
}

export interface ScenarioConfig {
  id: string;
  category: string;
  description: string;
  promptFile: string;
  templateDir: string;
  solutionFiles: string[];
  commands: ScenarioCommands;
  constraints: ScenarioConstraints;
}

export interface RunResult {
  model: string;
  scenarioId: string;
  compileOk: boolean;
  lintErrors: number;
  lintWarnings: number;
  testsPassed: number;
  testsFailed: number;
  instructionViolations: string[];
  latencyMs: number;
}

export interface ModelSummary {
  model: string;
  compileRate: number;
  lintCleanRate: number;
  testPassRate: number;
  instructionScore: number;
  medianLatencyMs: number;
  accuracyScore: number;
}

export interface PullModelsOptions {
  modelsFile: string;
  delayMs: number;
}

export interface RunOptions {
  modelsFile: string;
  scenariosDir: string;
  outFile: string;
  concurrency: number;
  filterModel?: string;
  filterScenario?: string;
}
