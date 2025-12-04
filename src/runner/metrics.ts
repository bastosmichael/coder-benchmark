import { ScenarioConstraints, RunResult, ModelSummary } from './types.js';

export interface ExtractionResult {
  code: string;
  instructionViolations: string[];
}

export function extractTsCode(
  response: string,
  _constraints: ScenarioConstraints
): ExtractionResult {
  return {
    code: response,
    instructionViolations: [],
  };
}

export function summarizeModelResults(_results: RunResult[]): ModelSummary[] {
  return [];
}
