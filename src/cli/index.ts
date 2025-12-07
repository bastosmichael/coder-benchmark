#!/usr/bin/env node
import { program } from 'commander';
import { pullModelsCommand } from '../runner/pullModels.js';
import { runAll } from '../runner/index.js';
import { summarizeResults } from '../runner/summarizer.js';

program
  .command('prepare-models')
  .option('--models <file>', 'Models file', 'models.json')
  .option('--delay-ms <number>', 'Delay between pulls in milliseconds', '10000')
  .option('--limit <number>', 'Limit number of models to prepare')
  .action(async (options) => {
    const delay = Number.parseInt(options.delayMs, 10);
    const limit = options.limit ? Number.parseInt(options.limit, 10) : undefined;
    await pullModelsCommand({
      modelsFile: options.models,
      delayMs: Number.isNaN(delay) ? 10000 : delay,
      limit: limit && !Number.isNaN(limit) ? limit : undefined,
    });
  });

program
  .command('run')
  .option('--models <file>', 'Model list JSON', 'models.json')
  .option('--scenarios <dir>', 'Scenarios directory', 'scenarios')
  .option('--out <file>', 'Output results JSON', 'results.json')
  .option('--concurrency <n>', 'Concurrency level', '1')
  .option('--filter-model <pattern>', 'Only run models matching pattern')
  .option('--filter-scenario <pattern>', 'Only run scenarios matching pattern')
  .option('--sequential-models', 'Run models sequentially, parallelizing scenarios within each model')
  .option('--limit <number>', 'Limit number of models to run')
  .action(async (options) => {
    const concurrency = Number.parseInt(options.concurrency, 10);
    const limit = options.limit ? Number.parseInt(options.limit, 10) : undefined;

    await runAll({
      modelsFile: options.models,
      scenariosDir: options.scenarios,
      outFile: options.out,
      concurrency: Number.isNaN(concurrency) ? 1 : concurrency,
      filterModel: options.filterModel,
      filterScenario: options.filterScenario,
      sequentialModels: options.sequentialModels,
      limit: limit && !Number.isNaN(limit) ? limit : undefined,
    });
  });

program
  .command('summarize')
  .argument('<file>', 'Results file to summarize')
  .action(async (file) => {
    await summarizeResults(file);
  });

program.parseAsync(process.argv);
