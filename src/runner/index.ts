import fs from 'fs/promises';
import { constants as fsConstants } from 'fs';
import { summarizeResults } from './summarizer.js';
import path from 'path';
import { loadScenarios } from './scenarioLoader.js';
import { RunOptions, RunResult, BenchmarkReport, SystemInfo } from './types.js';
import { createWorkspace, runCommand } from './workspace.js';
import { generate } from './modelClient.js';
import { extractCode } from './metrics.js';
import { execa } from 'execa';
import os from 'os';

async function fileExists(path: string): Promise<boolean> {
  try {
    await fs.access(path, fsConstants.F_OK);
    return true;
  } catch {
    return false;
  }
}

async function prepareBaseTemplate(): Promise<string> {
  const baseDir = path.resolve('base-template');
  const nodeModules = path.join(baseDir, 'node_modules');

  if (!(await fileExists(nodeModules))) {
    console.log('Installing dependencies in base-template...');
    await execa('npm', ['install'], { cwd: baseDir, stdio: 'inherit' });
  }
  return baseDir;
}

export async function runAll(options: RunOptions): Promise<void> {
  const baseDir = await prepareBaseTemplate();

  const rawModels = await fs.readFile(options.modelsFile, 'utf8');
  const modelList = JSON.parse(rawModels) as unknown;

  if (!Array.isArray(modelList) || modelList.some((item) => typeof item !== 'string')) {
    throw new Error(`Expected ${options.modelsFile} to contain an array of model names.`);
  }

  const modelPattern = options.filterModel ? new RegExp(options.filterModel) : null;
  const scenarioPattern = options.filterScenario ? new RegExp(options.filterScenario) : null;

  const initialModels = options.limit ? modelList.slice(0, options.limit) : modelList;
  const models = modelPattern ? initialModels.filter((name) => modelPattern.test(name)) : initialModels;
  const scenarios = await loadScenarios(options.scenariosDir);
  const filteredScenarios = scenarioPattern
    ? scenarios.filter((scenario) => scenarioPattern.test(scenario.config.id))
    : scenarios;

  // Sort scenarios from hardest to easiest for reporting consistency
  const difficultyOrder = [
    'cpp', 'rs', 'hs', 'ml', 'scala', 'java', 'cs', 'go', 'dart', 'ts', 'py', 'rb', 'php', 'sh', 'html', 'sql'
  ];

  const getPrefix = (id: string) => id.split('-')[0];

  filteredScenarios.sort((a, b) => {
    const pA = getPrefix(a.config.id);
    const pB = getPrefix(b.config.id);
    const iA = difficultyOrder.indexOf(pA);
    const iB = difficultyOrder.indexOf(pB);

    // If both are known, sort by index
    if (iA !== -1 && iB !== -1) {
      if (iA !== iB) return iA - iB;
      // same language, sort by id
      return a.config.id.localeCompare(b.config.id);
    }
    // If one is unknown, put it at end
    if (iA !== -1) return -1;
    if (iB !== -1) return 1;
    // Both unknown, sort string
    return pA.localeCompare(pB) || a.config.id.localeCompare(b.config.id);
  });

  const results: RunResult[] = [];

  // Stats tracking
  const modelStats = new Map<string, { total: number; completed: number; durationsMs: number[] }>();

  const iterations = options.iterations || 1;

  for (const model of models) {
    modelStats.set(model, {
      total: filteredScenarios.length * iterations,
      completed: 0,
      durationsMs: []
    });
  }

  function formatDuration(ms: number): string {
    const s = Math.round(ms / 1000);
    if (s < 60) return `${s}s`;
    const m = Math.floor(s / 60);
    const rs = s % 60;
    return `${m}m ${rs}s`;
  }

  /* Create task helper */
  const createRunTask = (model: string, scenario: typeof filteredScenarios[0], iterationIndex: number = 0) => async () => {
    const stats = modelStats.get(model)!;
    const startTime = Date.now();

    let etaMsg = '';
    if (stats.completed > 0 && stats.durationsMs.length > 0) {
      const avg = stats.durationsMs.reduce((a, b) => a + b, 0) / stats.durationsMs.length;
      const remaining = stats.total - stats.completed;
      const eta = avg * remaining;
      etaMsg = ` (Avg: ${formatDuration(avg)}, ETA: ${formatDuration(eta)})`;
    }

    const iterSuffix = iterations > 1 ? ` (iter ${iterationIndex + 1}/${iterations})` : '';
    console.log(`[${stats.completed + 1}/${stats.total}] Running ${model} on ${scenario.config.id}${iterSuffix}...${etaMsg}`);

    const isTs = scenario.config.id.startsWith('ts-');

    // 1. Setup Workspace
    // Use iterationIndex plus a timestamp-random suffix to be super safe
    const suffix = iterations > 1 ? `iter${iterationIndex}-${Date.now().toString().slice(-6)}` : undefined;
    const relativeWorkspacePath = await createWorkspace(model, scenario.config.id, suffix);
    const workspacePath = path.resolve(relativeWorkspacePath);

    // Copy base template (excluding node_modules) ONLY for TS
    if (isTs) {
      await fs.cp(path.join(baseDir, 'package.json'), path.join(workspacePath, 'package.json'));
      await fs.cp(path.join(baseDir, 'tsconfig.json'), path.join(workspacePath, 'tsconfig.json'));
      await fs.cp(path.join(baseDir, 'vitest.config.ts'), path.join(workspacePath, 'vitest.config.ts'));
      await fs.cp(path.join(baseDir, '.eslintrc.cjs'), path.join(workspacePath, '.eslintrc.cjs'));
      await fs.cp(path.join(baseDir, '.prettierrc'), path.join(workspacePath, '.prettierrc'));

      // Symlink node_modules
      await fs.symlink(
        path.join(baseDir, 'node_modules'),
        path.join(workspacePath, 'node_modules'),
        'junction'
      );
    }

    // Copy scenario template
    const templateDir = path.resolve(scenario.config.templateDir);
    // Copy contents of templateDir to workspacePath
    await fs.cp(templateDir, workspacePath, { recursive: true, force: true });

    // 2. Generate
    let generateResult;
    try {
      generateResult = await generate({
        model,
        prompt: scenario.promptText,
        options: {
          num_gpu: options.numGpu,
          main_gpu: options.mainGpu,
          num_ctx: options.numCtx,
          num_thread: options.numThread,
        }
      });
    } catch (e) {
      console.error(`Generation failed for ${model}/${scenario.config.id}:`, e);
      results.push({
        model,
        scenarioId: scenario.config.id,
        compileOk: false,
        lintErrors: 0,
        lintWarnings: 0,
        testsPassed: 0,
        testsFailed: 0,
        instructionViolations: ['Generation failed'],
        latencyMs: 0
      });
      return;
    }

    const { code, instructionViolations } = extractCode(generateResult.response, scenario.config.constraints);

    // Write code to solution file
    // Default to first solution file
    const targetFile = scenario.config.solutionFiles[0];
    const fullTargetPath = path.join(workspacePath, targetFile);
    await fs.writeFile(fullTargetPath, code);

    // 3. Run Checks
    // Build
    const buildRes = await runCommand(scenario.config.commands.build, workspacePath);
    const compileOk = buildRes.ok;

    // Lint
    const lintRes = await runCommand(scenario.config.commands.lint, workspacePath);

    let lintErrors = 0;
    let lintWarnings = 0;

    if (isTs) {
      // Parse lint output (eslint)
      const lintMatch = lintRes.stdout.match(/(\d+) problems? \((\d+) errors?, (\d+) warnings?\)/);
      if (lintMatch) {
        lintErrors = parseInt(lintMatch[2], 10);
        lintWarnings = parseInt(lintMatch[3], 10);
      } else if (lintRes.exitCode !== 0) {
        lintErrors = (lintRes.stdout.match(/error/gi) || []).length;
        lintWarnings = (lintRes.stdout.match(/warning/gi) || []).length;
      }
    } else {
      // Python lint (pylint) simplistics
      if (lintRes.exitCode !== 0) {
        // If it failed, assume errors. 
        // Count lines containing ': error'?
        lintErrors = (lintRes.stdout.match(/error/gi) || []).length;
        if (lintErrors === 0) lintErrors = 1; // fallback
      }
    }

    // Test
    // Vitest output: "Tests  2 failed | 1 passed (3)"
    // or "Tests  3 passed (3)"
    const testRes = await runCommand(scenario.config.commands.test, workspacePath);

    let testsPassed = 0;
    let testsFailed = 0;

    const passedMatch = testRes.stdout.match(/(\d+) passed/);
    const failedMatch = testRes.stdout.match(/(\d+) failed/);

    if (passedMatch) testsPassed = parseInt(passedMatch[1], 10);
    if (failedMatch) testsFailed = parseInt(failedMatch[1], 10);

    // If compilation failed, tests probably failed or didn't run.
    if (!compileOk && testsPassed === 0 && testsFailed === 0) {
      // Check if build error output suggests anything
    }

    results.push({
      model,
      scenarioId: scenario.config.id,
      compileOk,
      lintErrors,
      lintWarnings,
      testsPassed,
      testsFailed,
      instructionViolations,
      latencyMs: generateResult.latencyMs
    });

    // Cleanup? 
    // For debugging we might want to keep it. But it fills disk.
    // await fs.rm(workspacePath, { recursive: true, force: true });

    // Update stats
    const duration = Date.now() - startTime;
    stats.durationsMs.push(duration);
    stats.completed++;
  };

  /* ... */
  // Capture System Info UP FRONT so we can save it incrementally
  /* ... */
  // Capture System Info UP FRONT so we can save it incrementally
  const cpus = os.cpus();
  const cpuModel = cpus.length > 0 ? cpus[0].model : 'Unknown';

  let gpuInfo = 'Unknown';
  try {
    if (os.platform() === 'darwin') {
      const { stdout } = await execa('system_profiler', ['SPDisplaysDataType', '-json']);
      const data = JSON.parse(stdout);
      const gpus = data.SPDisplaysDataType;
      if (Array.isArray(gpus)) {
        gpuInfo = gpus.map((g: any) => g.sppci_model || g._name).join(', ');
      }
    } else {
      // Linux/Windows fallback
      try {
        const { stdout } = await execa('nvidia-smi', ['-L']);
        gpuInfo = stdout.trim();
      } catch {
        // Ignore
      }
    }
  } catch (e) {
    // console.warn('Failed to fetch GPU info', e); 
  }

  const systemInfo: SystemInfo = {
    platform: os.platform(),
    release: os.release(),
    arch: os.arch(),
    cpuModel,
    cpuCores: cpus.length,
    totalMemory: os.totalmem(),
    freeMemory: os.freemem(),
    gpuInfo
  };

  // Function to save progress safely
  let isSaving = false;
  const saveProgress = async () => {
    if (isSaving) return; // simple mutex, skip if busy
    isSaving = true;
    try {
      const report: BenchmarkReport = {
        system: systemInfo,
        results
      };
      await fs.writeFile(options.outFile, JSON.stringify(report, null, 2));
      // We can also update the README here, but let's be careful about console spam
      // await summarizeResults(options.outFile); 
      // Actually user wants to see results per model. 
      // summarizer prints to console too.
    } catch (err) {
      console.error('Failed to save progress:', err);
    } finally {
      isSaving = false;
    }
  };

  // Execute with concurrency
  const concurrency = options.concurrency || 1;

  const runQueue = async (tasksToRun: (() => Promise<void>)[]) => {
    const activeWorkers: Promise<void>[] = [];
    // Use a simple iterator
    const queue = [...tasksToRun];

    const worker = async () => {
      while (queue.length > 0) {
        const task = queue.shift();
        if (task) {
          await task();
          await saveProgress();
        }
      }
    };

    for (let i = 0; i < concurrency; i++) {
      activeWorkers.push(worker());
    }

    await Promise.all(activeWorkers);
  };

  if (options.sequentialModels) {
    for (const model of models) {
      const modelTasks = [];
      for (const s of filteredScenarios) {
        for (let k = 0; k < iterations; k++) {
          modelTasks.push(createRunTask(model, s, k));
        }
      }
      await runQueue(modelTasks);
    }
  } else {
    // Flatten all
    const allTasks: (() => Promise<void>)[] = [];
    for (const model of models) {
      for (const s of filteredScenarios) {
        for (let k = 0; k < iterations; k++) {
          allTasks.push(createRunTask(model, s, k));
        }
      }
    }
    await runQueue(allTasks);
  }

  // Final save and summary
  await saveProgress();
  console.log(`Wrote ${results.length} results to ${options.outFile}`);

  console.log('\n--- Benchmark Summary ---\n');
  await summarizeResults(options.outFile);
}
