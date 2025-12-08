import fs from 'fs/promises';
import { RunResult } from './types.js';
import { summarizeModelResults } from './metrics.js';

export async function summarizeResults(file: string, quiet: boolean = false): Promise<void> {
  try {
    const content = await fs.readFile(file, 'utf8');

    // Attempt parse as BenchmarkReport, fallback to array for backward compat if needed
    const json = JSON.parse(content);
    let results: RunResult[];
    let system: any = null;

    if (Array.isArray(json)) {
      results = json;
    } else {
      results = json.results;
      system = json.system;
    }

    const summaries = summarizeModelResults(results);

    // Format for display
    // Format for display
    const fmt = (val: number | null) => val !== null ? val.toFixed(0) : '-';

    const tableData = summaries.map(s => ({
      'Model': s.model,
      'Score': s.accuracyScore.toFixed(1),
      'C++': fmt(s.cppScore),
      'Rust': fmt(s.rustScore),
      'Hs': fmt(s.haskellScore),
      'OCaml': fmt(s.ocamlScore),
      'Scala': fmt(s.scalaScore),
      'Java': fmt(s.javaScore),
      'C#': fmt(s.csharpScore),
      'Go': fmt(s.goScore),
      'Dart': fmt(s.dartScore),
      'TS': fmt(s.tsScore),
      'Py': fmt(s.pyScore),
      'Ruby': fmt(s.rubyScore),
      'PHP': fmt(s.phpScore),
      'Bash': fmt(s.bashScore),
      'HTML': fmt(s.htmlScore),
      'SQL': fmt(s.sqlScore),
      'Latency': s.medianLatencyMs
    }));

    if (!quiet) {
      if (system) {
        console.log(`System: ${system.platform} ${system.release} (${system.arch})`);
        console.log(`CPU: ${system.cpuModel} (${system.cpuCores} cores)`);
        console.log(`Memory: ${(system.totalMemory / 1024 / 1024 / 1024).toFixed(2)} GB`);
        if (system.gpuInfo) console.log(`GPU: ${system.gpuInfo}`);
        console.log('');
      }

      console.table(tableData);
    }

    // Generate Markdown Table
    let markdown = '';

    if (system) {
      markdown += `** System Environment **\n`;
      markdown += `- ** OS **: ${system.platform} ${system.release} (${system.arch}) \n`;
      markdown += `- ** CPU **: ${system.cpuModel} (${system.cpuCores} cores) \n`;
      markdown += `- ** Memory **: ${(system.totalMemory / 1024 / 1024 / 1024).toFixed(2)} GB\n`;
      if (system.gpuInfo) markdown += `- ** GPU **: ${system.gpuInfo}\n`;
      markdown += `\n`;
    }

    markdown += '\n| Model | Score | C++ | Rust | Hs | OCaml | Scala | Java | C# | Go | Dart | TS | Py | Ruby | PHP | Bash | HTML | SQL | Latency (ms) |\n';
    markdown += '|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|\n';

    for (const s of summaries) {
      const f = (v: number | null) => v !== null ? v.toFixed(0) : '-';
      markdown += `| ${s.model} | ${s.accuracyScore.toFixed(1)} | ${f(s.cppScore)} | ${f(s.rustScore)} | ${f(s.haskellScore)} | ${f(s.ocamlScore)} | ${f(s.scalaScore)} | ${f(s.javaScore)} | ${f(s.csharpScore)} | ${f(s.goScore)} | ${f(s.dartScore)} | ${f(s.tsScore)} | ${f(s.pyScore)} | ${f(s.rubyScore)} | ${f(s.phpScore)} | ${f(s.bashScore)} | ${f(s.htmlScore)} | ${f(s.sqlScore)} | ${s.medianLatencyMs} |\n`;
    }

    // Read README
    const readmePath = 'README.md';
    let readme = await fs.readFile(readmePath, 'utf8');

    const header = '## Benchmark Summary';
    const regex = new RegExp(`${header} [\\s\\S] * $`, 'i');

    if (regex.test(readme)) {
      readme = readme.replace(regex, `${header} \n\nLast updated: ${new Date().toISOString()} \n\n${markdown} `);
    } else {
      readme += `\n\n${header} \n\nLast updated: ${new Date().toISOString()} \n\n${markdown} `;
    }

    await fs.writeFile(readmePath, readme);
    if (!quiet) {
      console.log(`Updated ${readmePath} with new results.`);
    }

  } catch (error) {
    const message = error instanceof Error ? error.message : 'Unknown error';
    console.warn(`Could not read results from ${file}: ${message} `);
  }
}

