import fs from 'fs/promises';
import { execa } from 'execa';
import { PullModelsOptions } from './types.js';

async function sleep(ms: number): Promise<void> {
  await new Promise((resolve) => setTimeout(resolve, ms));
}

export async function pullModelsCommand(options: PullModelsOptions): Promise<void> {
  const rawModels = await fs.readFile(options.modelsFile, 'utf8');
  const models = JSON.parse(rawModels) as unknown;

  if (!Array.isArray(models) || models.some((item) => typeof item !== 'string')) {
    throw new Error(`Expected ${options.modelsFile} to contain an array of model names.`);
  }

  const modelsToPull = options.limit ? models.slice(0, options.limit) : models;

  for (const [index, model] of modelsToPull.entries()) {
    console.log(`Pulling model ${model} (${index + 1}/${modelsToPull.length})...`);
    try {
      await execa('ollama', ['pull', model], { stdio: 'inherit' });
    } catch (error) {
      const message = error instanceof Error ? error.message : 'Unknown error';
      console.warn(`Failed to pull ${model}: ${message}`);
    }

    if (index < models.length - 1 && options.delayMs > 0) {
      await sleep(options.delayMs);
    }
  }
}
