import fs from 'fs/promises';
import { RunResult } from './types.js';
import { summarizeModelResults } from './metrics.js';

export async function summarizeResults(file: string): Promise<void> {
  try {
    const content = await fs.readFile(file, 'utf8');
    const results = JSON.parse(content) as RunResult[];

    const summaries = summarizeModelResults(results);

    // Format for display
    const tableData = summaries.map(s => ({
      'Model': s.model,
      'Score': s.accuracyScore.toFixed(1),
      'C++': s.cppScore.toFixed(1),
      'Rust': s.rustScore.toFixed(1),
      'Hs': s.haskellScore.toFixed(1),
      'Scala': s.scalaScore.toFixed(1),
      'Java': s.javaScore.toFixed(1),
      'C#': s.csharpScore.toFixed(1),
      'Go': s.goScore.toFixed(1),
      'TS': s.tsScore.toFixed(1),
      'Py': s.pyScore.toFixed(1),
      'Ruby': s.rubyScore.toFixed(1),
      'PHP': s.phpScore.toFixed(1),
      'Bash': s.bashScore.toFixed(1),
      'HTML': s.htmlScore.toFixed(1),
      'SQL': s.sqlScore.toFixed(1),
      'Latency': s.medianLatencyMs
    }));

    console.table(tableData);

    // Generate Markdown Table
    let markdown = '\n| Model | Score | C++ | Rust | Hs | Scala | Java | C# | Go | TS | Py | Ruby | PHP | Bash | HTML | SQL | Latency (ms) |\n';
    markdown += '|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|\n';

    for (const s of summaries) {
      markdown += `| ${s.model} | ${s.accuracyScore.toFixed(1)} | ${s.cppScore.toFixed(1)} | ${s.rustScore.toFixed(1)} | ${s.haskellScore.toFixed(1)} | ${s.scalaScore.toFixed(1)} | ${s.javaScore.toFixed(1)} | ${s.csharpScore.toFixed(1)} | ${s.goScore.toFixed(1)} | ${s.tsScore.toFixed(1)} | ${s.pyScore.toFixed(1)} | ${s.rubyScore.toFixed(1)} | ${s.phpScore.toFixed(1)} | ${s.bashScore.toFixed(1)} | ${s.htmlScore.toFixed(1)} | ${s.sqlScore.toFixed(1)} | ${s.medianLatencyMs} |\n`;
    }

    // Read README
    const readmePath = 'README.md';
    let readme = await fs.readFile(readmePath, 'utf8');

    const header = '## Benchmark Summary';
    const regex = new RegExp(`${header}[\\s\\S]*$`, 'i');

    if (regex.test(readme)) {
      readme = readme.replace(regex, `${header}\n\nLast updated: ${new Date().toISOString()}\n${markdown}`);
    } else {
      readme += `\n\n${header}\n\nLast updated: ${new Date().toISOString()}\n${markdown}`;
    }

    await fs.writeFile(readmePath, readme);
    console.log(`Updated ${readmePath} with new results.`);

  } catch (error) {
    const message = error instanceof Error ? error.message : 'Unknown error';
    console.warn(`Could not read results from ${file}: ${message}`);
  }
}
