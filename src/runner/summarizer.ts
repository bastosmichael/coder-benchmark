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
    const tableData = summaries.map(s => ({
      'Model': s.model,
      'Score': s.accuracyScore.toFixed(1),
      'C++': s.cppScore.toFixed(0),
      'Rust': s.rustScore.toFixed(0),
      'Hs': s.haskellScore.toFixed(0),
      'Scala': s.scalaScore.toFixed(0),
      'Java': s.javaScore.toFixed(0),
      'C#': s.csharpScore.toFixed(0),
      'Go': s.goScore.toFixed(0),
      'Dart': s.dartScore.toFixed(0),
      'TS': s.tsScore.toFixed(0),
      'Py': s.pyScore.toFixed(0),
      'Ruby': s.rubyScore.toFixed(0),
      'PHP': s.phpScore.toFixed(0),
      'Bash': s.bashScore.toFixed(0),
      'HTML': s.htmlScore.toFixed(0),
      'SQL': s.sqlScore.toFixed(0),
      'Latency': s.medianLatencyMs
    }));

    if (!quiet) {
      if (system) {
        console.log(`System: ${system.platform} ${system.release} (${system.arch})`);
        console.log(`CPU: ${system.cpuModel} (${system.cpuCores} cores)`);
        console.log(`Memory: ${(system.totalMemory / 1024 / 1024 / 1024).toFixed(2)} GB`);
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
      markdown += `- ** Memory **: ${(system.totalMemory / 1024 / 1024 / 1024).toFixed(2)} GB\n\n`;
    }

    markdown += '\n| Model | Score | C++ | Rust | Hs | Scala | Java | C# | Go | Dart | TS | Py | Ruby | PHP | Bash | HTML | SQL | Latency (ms) |\n';
    markdown += '|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|\n';

    for (const s of summaries) {
      markdown += `| ${s.model} | ${s.accuracyScore.toFixed(1)} | ${s.cppScore.toFixed(0)} | ${s.rustScore.toFixed(0)} | ${s.haskellScore.toFixed(0)} | ${s.scalaScore.toFixed(0)} | ${s.javaScore.toFixed(0)} | ${s.csharpScore.toFixed(0)} | ${s.goScore.toFixed(0)} | ${s.dartScore.toFixed(0)} | ${s.tsScore.toFixed(0)} | ${s.pyScore.toFixed(0)} | ${s.rubyScore.toFixed(0)} | ${s.phpScore.toFixed(0)} | ${s.bashScore.toFixed(0)} | ${s.htmlScore.toFixed(0)} | ${s.sqlScore.toFixed(0)} | ${s.medianLatencyMs} |\n`;
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

