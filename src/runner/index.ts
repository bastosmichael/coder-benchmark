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

  const models = modelPattern ? modelList.filter((name) => modelPattern.test(name)) : modelList;
  const scenarios = await loadScenarios(options.scenariosDir);
  const filteredScenarios = scenarioPattern
    ? scenarios.filter((scenario) => scenarioPattern.test(scenario.config.id))
    : scenarios;

  const results: RunResult[] = [];

  // Create a queue of tasks
  const tasks: (() => Promise<void>)[] = [];

  for (const model of models) {
    for (const scenario of filteredScenarios) {
      tasks.push(async () => {
        console.log(`Running ${model} on ${scenario.config.id}...`);

        const isTs = scenario.config.id.startsWith('ts-');

        // 1. Setup Workspace
        const relativeWorkspacePath = await createWorkspace(model, scenario.config.id);
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
            prompt: scenario.promptText
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
      });
    }
  }

  /* ... */
  // Execute with concurrency
  const concurrency = options.concurrency || 1;
  const activeWorkers: Promise<void>[] = [];

  // Use a simple iterator
  const queue = [...tasks];

  const worker = async () => {
    while (queue.length > 0) {
      const task = queue.shift();
      if (task) await task();
    }
  };

  for (let i = 0; i < concurrency; i++) {
    activeWorkers.push(worker());
  }

  await Promise.all(activeWorkers);

  // Capture System Info
  const cpus = os.cpus();
  const cpuModel = cpus.length > 0 ? cpus[0].model : 'Unknown';
  // Speed is often 0 on some systems/VMs if not reported per core correctly
  const cpuSpeed = cpus.length > 0 ? cpus[0].speed : 0;

  const systemInfo: SystemInfo = {
    platform: os.platform(),
    release: os.release(),
    arch: os.arch(),
    cpuModel,
    cpuSpeed,
    cpuCores: cpus.length,
    totalMemory: os.totalmem(),
  };

  const report: BenchmarkReport = {
    system: systemInfo,
    results
  };

  await fs.writeFile(options.outFile, JSON.stringify(report, null, 2));
  console.log(`Wrote ${results.length} results to ${options.outFile}`);

  console.log('\n--- Benchmark Summary ---\n');
  await summarizeResults(options.outFile);
}
