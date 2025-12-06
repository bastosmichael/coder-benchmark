import fs from 'fs/promises';
import { constants as fsConstants } from 'fs';
import path from 'path';
import { loadScenarios } from './scenarioLoader.js';
import { RunOptions, RunResult, ScenarioConfig } from './types.js';
import { createWorkspace, runCommand } from './workspace.js';
import { generate } from './modelClient.js';
import { extractTsCode } from './metrics.js';
import { execa } from 'execa';

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

        // 1. Setup Workspace
        const relativeWorkspacePath = await createWorkspace(model, scenario.config.id);
        const workspacePath = path.resolve(relativeWorkspacePath);

        // Copy base template (excluding node_modules)
        // We use fs.cp if available (Node 16.7+), otherwise fs.copyFile loop or execa 'cp'
        // Assuming modern node, but let's be safe and copy specific folders if cp is flaky?
        // fs.cp is stable in Node 20.
        // We'll use execa for recursion to be safe and simple
        await execa('cp', ['-r', path.join(baseDir, 'package.json'), workspacePath]);
        await execa('cp', ['-r', path.join(baseDir, 'tsconfig.json'), workspacePath]);
        await execa('cp', ['-r', path.join(baseDir, 'vitest.config.ts'), workspacePath]);
        await execa('cp', ['-r', path.join(baseDir, '.eslintrc.cjs'), workspacePath]);
        await execa('cp', ['-r', path.join(baseDir, '.prettierrc'), workspacePath]);

        // Symlink node_modules
        await fs.symlink(
          path.join(baseDir, 'node_modules'),
          path.join(workspacePath, 'node_modules')
        );

        // Copy scenario template
        const templateDir = path.resolve(scenario.config.templateDir);
        // Copy contents of templateDir to workspacePath
        // cp -r templateDir/* workspacePath
        await execa('cp', ['-R', '.', workspacePath], { cwd: templateDir });

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

        const { code, instructionViolations } = extractTsCode(generateResult.response, scenario.config.constraints);

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
        // Parse lint output (eslint)
        // output: "5 problems (2 errors, 3 warnings)"
        const lintMatch = lintRes.stdout.match(/(\d+) problems? \((\d+) errors?, (\d+) warnings?\)/);
        let lintErrors = 0;
        let lintWarnings = 0;
        if (lintMatch) {
          lintErrors = parseInt(lintMatch[2], 10);
          lintWarnings = parseInt(lintMatch[3], 10);
        } else if (lintRes.exitCode !== 0) {
          // Maybe it just says "error"
          // Eslint with default formatter usually prints stats info at end.
          // If parse fails but exit code != 0, assume some errors.
          // Let's count occurences of "error" and "warning" in lines?
          // Fallback
          lintErrors = (lintRes.stdout.match(/error/gi) || []).length;
          lintWarnings = (lintRes.stdout.match(/warning/gi) || []).length;
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

  await fs.writeFile(options.outFile, JSON.stringify(results, null, 2));
  console.log(`Wrote ${results.length} results to ${options.outFile}`);
}
