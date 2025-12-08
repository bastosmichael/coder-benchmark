#!/usr/bin/env node
import { program } from 'commander';
import { pullModelsCommand } from '../runner/pullModels.js';
import { runAll } from '../runner/index.js';
import { summarizeResults } from '../runner/summarizer.js';

program
  .command('prepare-models [limit]')
  .option('--models <file>', 'Models file', 'models.json')
  .option('--delay-ms <number>', 'Delay between pulls in milliseconds', '10000')
  .option('--limit <number>', 'Limit number of models to prepare')
  .action(async (limitArg, options) => {
    const delay = Number.parseInt(options.delayMs, 10);
    const limitVal = limitArg || options.limit;
    const limit = limitVal ? Number.parseInt(limitVal, 10) : undefined;
    await pullModelsCommand({
      modelsFile: options.models,
      delayMs: Number.isNaN(delay) ? 10000 : delay,
      limit: limit && !Number.isNaN(limit) ? limit : undefined,
    });
  });

program
  .command('run [limit]')
  .option('--models <file>', 'Model list JSON', 'models.json')
  .option('--scenarios <dir>', 'Scenarios directory', 'scenarios')
  .option('--out <file>', 'Output results JSON', 'results.json')
  .option('--concurrency <n>', 'Concurrency level', '1')
  .option('--filter-model <pattern>', 'Only run models matching pattern')
  .option('--filter-scenario <pattern>', 'Only run scenarios matching pattern')
  .option('--sequential-models', 'Run models sequentially, parallelizing scenarios within each model')
  .option('--limit <number>', 'Limit number of models to run')
  .option('--num-gpu <number>', 'Number of layers to offload to GPU')
  .option('--main-gpu <number>', 'Index of main GPU to use')
  .option('--num-ctx <number>', 'Context window size')
  .option('--num-ctx <number>', 'Context window size')
  .option('--num-thread <number>', 'Number of threads to use')
  .option('--iterations <number>', 'Number of times to run each scenario', '3')
  .action(async (limitArg, options) => {
    const concurrency = Number.parseInt(options.concurrency, 10);
    const limitVal = limitArg || options.limit;
    const limit = limitVal ? Number.parseInt(limitVal, 10) : undefined;
    const numGpu = options.numGpu ? Number.parseInt(options.numGpu, 10) : undefined;
    const mainGpu = options.mainGpu ? Number.parseInt(options.mainGpu, 10) : undefined;
    const numCtx = options.numCtx ? Number.parseInt(options.numCtx, 10) : undefined;
    const numThread = options.numThread ? Number.parseInt(options.numThread, 10) : undefined;
    const iterations = options.iterations ? Number.parseInt(options.iterations, 10) : undefined;

    await runAll({
      modelsFile: options.models,
      scenariosDir: options.scenarios,
      outFile: options.out,
      concurrency: Number.isNaN(concurrency) ? 1 : concurrency,
      filterModel: options.filterModel,
      filterScenario: options.filterScenario,
      sequentialModels: options.sequentialModels,
      limit: limit && !Number.isNaN(limit) ? limit : undefined,
      numGpu: numGpu && !Number.isNaN(numGpu) ? numGpu : undefined,
      mainGpu: mainGpu && !Number.isNaN(mainGpu) ? mainGpu : undefined,
      numCtx: numCtx && !Number.isNaN(numCtx) ? numCtx : undefined,
      numThread: numThread && !Number.isNaN(numThread) ? numThread : undefined,
      iterations: iterations && !Number.isNaN(iterations) ? iterations : undefined,
    });
  });

program
  .command('summarize')
  .argument('<file>', 'Results file to summarize')
  .action(async (file) => {
    await summarizeResults(file);
  });

program.parseAsync(process.argv);
