import { ScenarioConstraints, RunResult, ModelSummary } from './types.js';

export interface ExtractionResult {
  code: string;
  instructionViolations: string[];
}

export function extractCode(
  response: string,
  constraints: ScenarioConstraints
): ExtractionResult {
  let code = response;
  const violations: string[] = [];

  // specific language blocks or generic
  const codeBlockRegex = /```(?:typescript|ts|python|py|cpp|c\+\+|c|rust|rs|haskell|hs|ocaml|ml|scala|java|csharp|cs|go|ruby|rb|php|bash|sh|shell|html|css|sql|dart)?\s*([\s\S]*?)```/i;
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
    if (response.trim().startsWith('import') || response.trim().startsWith('export') || response.trim().startsWith('function') || response.trim().startsWith('class') || response.trim().startsWith('def')) {
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

    const calculateScore = (items: RunResult[]): number | null => {
      if (items.length === 0) return null;
      const total = items.length;
      const compileOk = items.filter(r => r.compileOk).length;
      const lintClean = items.filter(r => r.lintErrors === 0 && r.lintWarnings === 0).length;

      const totalTests = items.reduce((sum, r) => sum + r.testsPassed + r.testsFailed, 0);
      const totalPassed = items.reduce((sum, r) => sum + r.testsPassed, 0);
      const passedRate = totalTests > 0 ? totalPassed / totalTests : 0;

      const latencies = items.map(r => r.latencyMs).sort((a, b) => a - b);
      const medianLatency = latencies.length > 0 ? latencies[Math.floor(latencies.length / 2)] : 0;

      const baseScore = (
        (compileOk / total) * 30 +
        (lintClean / total) * 20 +
        passedRate * 50
      );

      const speedBonus = medianLatency > 0 ? (2000 / (medianLatency + 100)) : 0;
      return baseScore + speedBonus;
    };

    // ... (lines 72-84 same) ...
    // Total stats
    const total = modelResults.length;
    const compileOk = modelResults.filter(r => r.compileOk).length;
    const lintClean = modelResults.filter(r => r.lintErrors === 0 && r.lintWarnings === 0).length;

    const totalTests = modelResults.reduce((sum, r) => sum + r.testsPassed + r.testsFailed, 0);
    const totalPassed = modelResults.reduce((sum, r) => sum + r.testsPassed, 0);
    const passedRate = totalTests > 0 ? totalPassed / totalTests : 0;

    const latencies = modelResults.map(r => r.latencyMs).sort((a, b) => a - b);
    const medianLatency = latencies.length > 0 ? latencies[Math.floor(latencies.length / 2)] : 0;

    // Calculate per-language scores
    const cppScore = calculateScore(modelResults.filter(r => r.scenarioId.startsWith('cpp-')));
    const rustScore = calculateScore(modelResults.filter(r => r.scenarioId.startsWith('rs-')));
    const haskellScore = calculateScore(modelResults.filter(r => r.scenarioId.startsWith('hs-')));
    const scalaScore = calculateScore(modelResults.filter(r => r.scenarioId.startsWith('scala-')));
    const javaScore = calculateScore(modelResults.filter(r => r.scenarioId.startsWith('java-')));
    const csharpScore = calculateScore(modelResults.filter(r => r.scenarioId.startsWith('cs-')));
    const goScore = calculateScore(modelResults.filter(r => r.scenarioId.startsWith('go-')));
    const dartScore = calculateScore(modelResults.filter(r => r.scenarioId.startsWith('dart-')));
    const tsScore = calculateScore(modelResults.filter(r => r.scenarioId.startsWith('ts-')));
    const pyScore = calculateScore(modelResults.filter(r => r.scenarioId.startsWith('py-')));
    const rubyScore = calculateScore(modelResults.filter(r => r.scenarioId.startsWith('rb-')));
    const phpScore = calculateScore(modelResults.filter(r => r.scenarioId.startsWith('php-')));
    const bashScore = calculateScore(modelResults.filter(r => r.scenarioId.startsWith('sh-')));
    const htmlScore = calculateScore(modelResults.filter(r => r.scenarioId.startsWith('html-')));
    const sqlScore = calculateScore(modelResults.filter(r => r.scenarioId.startsWith('sql-')));
    const ocamlScore = calculateScore(modelResults.filter(r => r.scenarioId.startsWith('ml-')));

    let sumScores = 0;
    let countLangs = 0;

    const checkLang = (prefix: string, score: number | null) => {
      if (score !== null) {
        sumScores += score;
        countLangs++;
      }
    };

    checkLang('cpp-', cppScore);
    checkLang('rs-', rustScore);
    checkLang('hs-', haskellScore);
    checkLang('scala-', scalaScore);
    checkLang('java-', javaScore);
    checkLang('cs-', csharpScore);
    checkLang('go-', goScore);
    checkLang('dart-', dartScore);
    checkLang('ts-', tsScore);
    checkLang('py-', pyScore);
    checkLang('rb-', rubyScore);
    checkLang('php-', phpScore);
    checkLang('sh-', bashScore);
    checkLang('html-', htmlScore);
    checkLang('sql-', sqlScore);
    checkLang('ml-', ocamlScore);

    const overallScore = countLangs > 0 ? sumScores / countLangs : 0;

    return {
      model,
      compileRate: total > 0 ? (compileOk / total) * 100 : 0,
      lintCleanRate: total > 0 ? (lintClean / total) * 100 : 0,
      testPassRate: passedRate * 100,
      instructionScore: 100,
      medianLatencyMs: medianLatency,
      accuracyScore: overallScore,
      cppScore,
      rustScore,
      haskellScore,
      scalaScore,
      javaScore,
      csharpScore,
      goScore,
      dartScore,
      tsScore,
      pyScore,
      rubyScore,
      phpScore,
      bashScore,
      htmlScore,
      sqlScore,
      ocamlScore
    };
  });

  return summaries.sort((a, b) => b.accuracyScore - a.accuracyScore);
}
