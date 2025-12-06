import { ScenarioConstraints, RunResult, ModelSummary } from './types.js';

export interface ExtractionResult {
  code: string;
  instructionViolations: string[];
}

export function extractTsCode(
  response: string,
  constraints: ScenarioConstraints
): ExtractionResult {
  let code = response;
  const violations: string[] = [];

  // Simple markdown code block extraction
  const codeBlockRegex = /```(?:typescript|ts)?\s*([\s\S]*?)```/i;
  const match = response.match(codeBlockRegex);

  if (match) {
    code = match[1];
    if (constraints.singleCodeBlock) {
      // Check if there are multiple blocks
      const allMatches = [...response.matchAll(/```/g)];
      // each block has 2 ```, so if > 2 matches, likely multiple blocks (or nested, which is weird)
      if (allMatches.length > 2) {
        violations.push('Multiple code blocks detected when only one was expected');
      }
    }
  } else {
    // If no code block, we might consider it a violation or just take the whole text if it looks like code?
    // For now, let's assume if no code block, it's failed to follow instructions if we strictly expect markdown.
    // But maybe the model just output code. Let's try to detect.
    if (response.trim().startsWith('import') || response.trim().startsWith('export') || response.trim().startsWith('function') || response.trim().startsWith('class')) {
      // likely raw code
    } else {
      violations.push('No markdown code block found');
    }
  }

  return {
    code: code.trim(),
    instructionViolations: violations,
  };
}

export function summarizeModelResults(results: RunResult[]): ModelSummary[] {
  const models = [...new Set(results.map(r => r.model))];

  const summaries = models.map(model => {
    const modelResults = results.filter(r => r.model === model);
    const total = modelResults.length;

    const compileOk = modelResults.filter(r => r.compileOk).length;
    const lintClean = modelResults.filter(r => r.lintErrors === 0 && r.lintWarnings === 0).length;
    const allTestsPassed = modelResults.filter(r => r.testsPassed > 0 && r.testsFailed === 0).length;

    // Simple aggregations
    const totalTests = modelResults.reduce((sum, r) => sum + r.testsPassed + r.testsFailed, 0);
    const totalPassed = modelResults.reduce((sum, r) => sum + r.testsPassed, 0);

    const passedRate = totalTests > 0 ? totalPassed / totalTests : 0;

    // Median latency
    const latencies = modelResults.map(r => r.latencyMs).sort((a, b) => a - b);
    const medianLatency = latencies.length > 0 ? latencies[Math.floor(latencies.length / 2)] : 0;

    // Accuracy Score (weighted)
    // 30% compile, 20% lint, 50% test pass rate
    // This is arbitrary, users can adjust.
    const baseScore = (
      (compileOk / total) * 30 +
      (lintClean / total) * 20 +
      passedRate * 50
    );

    // Speed bonus: higher is better. 
    // If latency is 100ms -> +20 points.
    // If latency is 1000ms -> +2 points.
    // If latency is 10000ms -> +0.2 points.
    // This differentiates models that otherwise fail everything uniquely.
    const speedBonus = medianLatency > 0 ? (2000 / (medianLatency + 100)) : 0;

    return {
      model,
      compileRate: total > 0 ? (compileOk / total) * 100 : 0,
      lintCleanRate: total > 0 ? (lintClean / total) * 100 : 0,
      testPassRate: passedRate * 100,
      instructionScore: 100,
      medianLatencyMs: medianLatency,
      accuracyScore: baseScore + speedBonus
    };
  });

  return summaries.sort((a, b) => b.accuracyScore - a.accuracyScore);
}
