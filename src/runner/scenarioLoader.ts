import fs from 'fs/promises';
import path from 'path';
import { ScenarioConfig } from './types.js';

export interface LoadedScenario {
  config: ScenarioConfig;
  promptText: string;
}

export async function loadScenarios(dir: string): Promise<LoadedScenario[]> {
  const entries = await fs.readdir(dir, { withFileTypes: true });
  const scenarios: LoadedScenario[] = [];

  for (const entry of entries) {
    if (!entry.isFile() || !entry.name.endsWith('.json')) {
      continue;
    }

    const filePath = path.join(dir, entry.name);
    const rawConfig = await fs.readFile(filePath, 'utf8');
    const config: ScenarioConfig = JSON.parse(rawConfig);

    const promptPath = path.resolve(dir, config.promptFile);
    let promptText = '';
    try {
      promptText = await fs.readFile(promptPath, 'utf8');
    } catch {
      promptText = '';
    }

    scenarios.push({ config, promptText });
  }

  return scenarios;
}
