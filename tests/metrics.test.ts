
import { describe, it, expect } from 'vitest';
import { extractCode, summarizeModelResults } from '../src/runner/metrics.js';
import { RunResult } from '../src/runner/types.js';

describe('metrics', () => {
    describe('extractCode', () => {
        it('extracts typescript code block', () => {
            const input = 'Here is the code:\n```typescript\nconsole.log("hello");\n```';
            const result = extractCode(input, { noAny: false, singleCodeBlock: true });
            expect(result.code).toBe('console.log("hello");');
            expect(result.instructionViolations).toHaveLength(0);
        });

        it('extracts python code block', () => {
            const input = '```python\nprint("hello")\n```';
            const result = extractCode(input, { noAny: false, singleCodeBlock: true });
            expect(result.code).toBe('print("hello")');
        });

        it('extracts dart code block', () => {
            const input = '```dart\nvoid main() {}\n```';
            const result = extractCode(input, { noAny: false, singleCodeBlock: true });
            expect(result.code).toBe('void main() {}');
        });

        it('handles raw code with imports if no markdown found', () => {
            const input = 'import fs from "fs";\nconsole.log("test");';
            const result = extractCode(input, { noAny: false, singleCodeBlock: true });
            expect(result.code).toBe(input);
            // It might warn, but checks for raw code
        });

        it('detects multiple blocks if disallowed', () => {
            const input = '```ts\n1\n```\n```ts\n2\n```';
            const result = extractCode(input, { noAny: false, singleCodeBlock: true });
            expect(result.instructionViolations).toContain('Multiple code blocks detected when only one was expected');
        });
    });

    describe('summarizeModelResults', () => {
        it('calculates scores correctly', () => {
            const mockResults: RunResult[] = [
                {
                    model: 'test-model',
                    scenarioId: 'ts-simple',
                    compileOk: true,
                    lintErrors: 0,
                    lintWarnings: 0,
                    testsPassed: 1,
                    testsFailed: 0,
                    instructionViolations: [],
                    latencyMs: 1000
                },
                {
                    model: 'test-model',
                    scenarioId: 'py-simple',
                    compileOk: true,
                    lintErrors: 0,
                    lintWarnings: 0,
                    testsPassed: 1,
                    testsFailed: 0,
                    instructionViolations: [],
                    latencyMs: 1000
                },
                // Dart fail
                {
                    model: 'test-model',
                    scenarioId: 'dart-simple',
                    compileOk: false,
                    lintErrors: 0,
                    lintWarnings: 0,
                    testsPassed: 0,
                    testsFailed: 1,
                    instructionViolations: [],
                    latencyMs: 1000
                }
            ];

            const summaries = summarizeModelResults(mockResults);
            expect(summaries).toHaveLength(1);
            const s = summaries[0];
            expect(s.model).toBe('test-model');

            // Verification of language scores
            // TS: 30 (compile) + 20 (lint) + 50 (test) = 100 + speed bonus
            // Speed bonus: 2000 / (1000 + 100) = 1.81
            expect(s.tsScore).toBeCloseTo(101.8, 0);

            // Dart: 0 + 20 (lint clean even if compile fail? check logic) + 0 = 20 + speed bonus
            // wait, lintClean logic: items.filter(r => r.lintErrors === 0 && r.lintWarnings === 0).length
            // So yes, lint clean. 
            // Base = 0 + 20 + 0 = 20.
            expect(s.dartScore).toBeCloseTo(21.8, 0);

            // Overall: (101.8 + 101.8 + 21.8) / 3 ? No, only 3 langs present?
            // ts, py, dart.
            // py should be same as ts.
            // 101.8 + 101.8 + 21.8 = 225.4
            // 225.4 / 3 = 75.1
            expect(s.accuracyScore).toBeGreaterThan(0);
        });
    });
});
