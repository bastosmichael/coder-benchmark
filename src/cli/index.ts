#!/usr/bin/env node
import { program } from 'commander';
import { pullModelsCommand } from '../runner/pullModels.js';
import { runAll } from '../runner/index.js';
import { summarizeResults } from '../runner/summarizer.js';

program
  .command('prepare-models')
  .option('--models <file>', 'Models file', 'models.json')
  .option('--delay-ms <number>', 'Delay between pulls in milliseconds', '10000')
  .action(async (cmd) => {
    const options = cmd.opts<{ models: string; delayMs: string }>();
    const delay = Number.parseInt(options.delayMs, 10);
    await pullModelsCommand({
      modelsFile: options.models,
      delayMs: Number.isNaN(delay) ? 10000 : delay,
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
  .action(async (cmd) => {
    const options = cmd.opts<{
      models: string;
      scenarios: string;
      out: string;
      concurrency: string;
      filterModel?: string;
      filterScenario?: string;
    }>();

    const concurrency = Number.parseInt(options.concurrency, 10);

    await runAll({
      modelsFile: options.models,
      scenariosDir: options.scenarios,
      outFile: options.out,
      concurrency: Number.isNaN(concurrency) ? 1 : concurrency,
      filterModel: options.filterModel,
      filterScenario: options.filterScenario,
    });
  });

program
  .command('summarize')
  .argument('<file>', 'Results file to summarize')
  .action(async (file) => {
    await summarizeResults(file);
  });

program.parseAsync(process.argv);
