export interface GenerateOptions {
  model: string;
  prompt: string;
}

export interface GenerateResult {
  response: string;
  latencyMs: number;
}

export async function generate(options: GenerateOptions): Promise<GenerateResult> {
  const start = Date.now();
  const response = `Stubbed response for model ${options.model}`;
  const latencyMs = Date.now() - start;

  return { response, latencyMs };
}
