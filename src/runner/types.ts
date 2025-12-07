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
  // Language specific scores
  cppScore: number;
  rustScore: number;
  haskellScore: number;
  scalaScore: number;
  javaScore: number;
  csharpScore: number;
  goScore: number;
  dartScore: number;
  tsScore: number; // Rank 8
  pyScore: number; // Rank 9
  rubyScore: number;
  phpScore: number;
  bashScore: number;
  htmlScore: number;
  sqlScore: number;
}

export interface PullModelsOptions {
  modelsFile: string;
  delayMs: number;
  limit?: number;
}

export interface RunOptions {
  modelsFile: string;
  scenariosDir: string;
  outFile: string;
  concurrency: number;
  filterModel?: string;
  filterScenario?: string;
  sequentialModels?: boolean;
  limit?: number;
}
export interface SystemInfo {
  platform: string;
  release: string;
  arch: string;
  cpuModel: string;
  cpuSpeed: number;
  cpuCores: number;
  totalMemory: number;
}

export interface BenchmarkReport {
  system: SystemInfo;
  results: RunResult[];
}
