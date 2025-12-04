import fs from 'fs/promises';
import { RunResult } from './types.js';

export async function summarizeResults(file: string): Promise<void> {
  try {
    const content = await fs.readFile(file, 'utf8');
    const results = JSON.parse(content) as RunResult[];
    console.log(`Loaded ${results.length} results from ${file}`);
  } catch (error) {
    const message = error instanceof Error ? error.message : 'Unknown error';
    console.warn(`Could not read results from ${file}: ${message}`);
  }
}
