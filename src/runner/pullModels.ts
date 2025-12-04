import { PullModelsOptions } from './types.js';

export async function pullModelsCommand(options: PullModelsOptions): Promise<void> {
  console.log(
    `Would pull models listed in ${options.modelsFile} with delay ${options.delayMs}ms between pulls.`
  );
}
