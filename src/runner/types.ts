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
  cppScore: number | null;
  rustScore: number | null;
  haskellScore: number | null;
  scalaScore: number | null;
  javaScore: number | null;
  csharpScore: number | null;
  goScore: number | null;
  dartScore: number | null;
  tsScore: number | null;
  pyScore: number | null;
  rubyScore: number | null;
  phpScore: number | null;
  bashScore: number | null;
  htmlScore: number | null;
  sqlScore: number | null;
  ocamlScore: number | null;
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
  numGpu?: number;
  mainGpu?: number;
  numCtx?: number;
  numThread?: number;
  iterations?: number;
}
export interface SystemInfo {
  platform: string;
  release: string;
  arch: string;
  totalMemory: number;
  freeMemory: number;
  cpuModel: string;
  cpuCores: number;
  gpuInfo?: string;
}

export interface BenchmarkReport {
  system: SystemInfo;
  results: RunResult[];
}
