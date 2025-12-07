export interface GenerateOptions {
  model: string;
  prompt: string;
  options?: {
    num_gpu?: number;
    main_gpu?: number;
    num_ctx?: number;
    num_thread?: number;
  };
}

export interface GenerateResult {
  response: string;
  latencyMs: number;
}

export async function generate(options: GenerateOptions): Promise<GenerateResult> {
  const maxRetries = 3;
  let attempt = 0;

  while (true) {
    try {
      const start = Date.now();
      // Increase timeout by passing an abort signal with a longer timeout if needed, 
      // but native fetch timeout depends on node version/undici default.
      // Undici default is usually 300s, but headers timeout can be shorter.
      // We'll rely on the retry.

      const res = await fetch('http://localhost:11434/api/generate', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          model: options.model,
          prompt: options.prompt,
          stream: false,
          options: options.options,
        }),
      });

      if (!res.ok) {
        throw new Error(`Ollama generate failed with status ${res.status}`);
      }

      const data = (await res.json()) as { response?: string };
      const latencyMs = Date.now() - start;

      return { response: data.response ?? '', latencyMs };
    } catch (error: any) {
      attempt++;
      if (attempt >= maxRetries) {
        throw error;
      }
      console.warn(`[Retry ${attempt}/${maxRetries}] Failed for ${options.model}: ${error.message}. Retrying in 2s...`);
      await new Promise(r => setTimeout(r, 2000));
    }
  }
}
