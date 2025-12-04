import { RunOptions } from './types.js';

export async function runAll(options: RunOptions): Promise<void> {
  console.log('Received run options:', options);
}
