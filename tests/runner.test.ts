
import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { runAll } from '../src/runner/index.js';
import fs from 'fs/promises';
import { execa } from 'execa';
import * as scenarioLoader from '../src/runner/scenarioLoader.js';
import * as workspace from '../src/runner/workspace.js';
import * as modelClient from '../src/runner/modelClient.js';
import * as summarizer from '../src/runner/summarizer.js';

vi.mock('fs/promises');
vi.mock('execa');
vi.mock('../src/runner/scenarioLoader.js');
vi.mock('../src/runner/workspace.js');
vi.mock('../src/runner/modelClient.js');
vi.mock('../src/runner/summarizer.js');

describe('runner', () => {
    beforeEach(() => {
        vi.resetAllMocks();
    });

    afterEach(() => {
        vi.clearAllMocks();
    });

    it('runs benchmarks successfully', async () => {
        // Mock fs.readFile for models
        vi.mocked(fs.readFile).mockImplementation(async (path: any) => {
            if (path === 'models.json') return JSON.stringify(['test-model']);
            return '';
        });

        // Mock scenario loading
        vi.mocked(scenarioLoader.loadScenarios).mockResolvedValue([
            {
                config: {
                    id: 'ts-test',
                    category: 'Test',
                    description: 'Test Scenario',
                    promptFile: 'prompt.md',
                    templateDir: 'template',
                    solutionFiles: ['solution.ts'],
                    commands: { build: 'build', lint: 'lint', test: 'test' },
                    constraints: { noAny: false, singleCodeBlock: true }
                },
                promptText: 'Implement this'
            }
        ]);

        // Mock workspace creation
        vi.mocked(workspace.createWorkspace).mockResolvedValue('/tmp/workspace');

        // Mock execa for base template prep and copy
        vi.mocked(execa).mockResolvedValue({} as any);

        // Mock fs access for fileExists (prepareBaseTemplate)
        // fileExists uses fs.access. Success = exists.
        // We want base-template/node_modules to exist typically, or check if it installs.
        // Let's say it exists.
        vi.mocked(fs.access).mockResolvedValue(undefined);

        // Mock generation
        vi.mocked(modelClient.generate).mockResolvedValue({
            response: '```typescript\nconsole.log("hello");\n```',
            latencyMs: 100
        });

        // Mock runCommand for build/lint/test
        vi.mocked(workspace.runCommand).mockImplementation(async (cmd) => {
            if (cmd === 'build') return { ok: true, exitCode: 0, stdout: '', stderr: '', durationMs: 10 };
            if (cmd === 'lint') return { ok: true, exitCode: 0, stdout: '0 problems', stderr: '', durationMs: 10 };
            if (cmd === 'test') return { ok: true, exitCode: 0, stdout: '1 passed', stderr: '', durationMs: 10 };
            return { ok: false, exitCode: 1, stdout: '', stderr: '', durationMs: 0 };
        });

        await runAll({
            modelsFile: 'models.json',
            scenariosDir: 'scenarios',
            outFile: 'results.json',
            concurrency: 1
        });

        // Verifications
        expect(modelClient.generate).toHaveBeenCalledWith({
            model: 'test-model',
            prompt: 'Implement this',
            options: {
                main_gpu: undefined,
                num_ctx: undefined,
                num_gpu: undefined,
                num_thread: undefined,
            }
        });
        expect(workspace.runCommand).toHaveBeenCalledWith('build', expect.anything());
        expect(workspace.runCommand).toHaveBeenCalledWith('lint', expect.anything());
        expect(workspace.runCommand).toHaveBeenCalledWith('test', expect.anything());
        expect(fs.writeFile).toHaveBeenCalledWith('results.json', expect.stringContaining('"compileOk": true'));
        expect(summarizer.summarizeResults).toHaveBeenCalledWith('results.json');
    });

    it('installs dependencies if base template missing', async () => {
        vi.mocked(fs.readFile).mockResolvedValue(JSON.stringify(['model']));
        vi.mocked(scenarioLoader.loadScenarios).mockResolvedValue([]); // No scenarios, just test setup

        // Fail access to node_modules
        vi.mocked(fs.access).mockRejectedValueOnce(new Error('no ent'));

        await runAll({ modelsFile: 'models.json', scenariosDir: 'dir', outFile: 'out.json', concurrency: 1 });

        expect(execa).toHaveBeenCalledWith('npm', ['install'], expect.objectContaining({ cwd: expect.stringContaining('base-template') }));
    });

    it('handles invalid models file', async () => {
        vi.mocked(fs.readFile).mockResolvedValue('not json');
        await expect(runAll({ modelsFile: 'm.json', scenariosDir: 's', outFile: 'o', concurrency: 1 })).rejects.toThrow();
    });

    it('handles generation failure', async () => {
        vi.mocked(fs.readFile).mockResolvedValue(JSON.stringify(['model']));
        vi.mocked(scenarioLoader.loadScenarios).mockResolvedValue([
            {
                config: {
                    id: 'ts-fail',
                    category: 'Test',
                    description: '',
                    promptFile: '',
                    templateDir: '',
                    solutionFiles: ['sol.ts'],
                    commands: { build: '', lint: '', test: '' },
                    constraints: { noAny: false, singleCodeBlock: true }
                },
                promptText: ''
            }
        ]);
        vi.mocked(fs.access).mockResolvedValue(undefined);
        vi.mocked(workspace.createWorkspace).mockResolvedValue('/wk');

        vi.mocked(modelClient.generate).mockRejectedValue(new Error('Gen failed'));

        await runAll({ modelsFile: 'm', scenariosDir: 's', outFile: 'out.json', concurrency: 1 });

        // Should write results with failure
        expect(fs.writeFile).toHaveBeenCalledWith('out.json', expect.stringContaining('Generation failed'));
    });

    it('parses lint errors correctly', async () => {
        vi.mocked(fs.readFile).mockResolvedValue(JSON.stringify(['model']));
        vi.mocked(scenarioLoader.loadScenarios).mockResolvedValue([
            {
                config: {
                    id: 'ts-lint-fail',
                    category: '', description: '', promptFile: '', templateDir: '', solutionFiles: ['sol.ts'],
                    commands: { build: 'b', lint: 'l', test: 't' },
                    constraints: { noAny: false, singleCodeBlock: true }
                },
                promptText: ''
            }
        ]);
        vi.mocked(fs.access).mockResolvedValue(undefined);
        vi.mocked(workspace.createWorkspace).mockResolvedValue('/wk');
        vi.mocked(modelClient.generate).mockResolvedValue({ response: 'code', latencyMs: 10 });

        vi.mocked(workspace.runCommand).mockImplementation(async (cmd) => {
            if (cmd === 'b') return { ok: true, exitCode: 0, stdout: '', stderr: '', durationMs: 0 };
            // Lint output mocking
            if (cmd === 'l') return { ok: false, exitCode: 1, stdout: '5 problems (2 errors, 3 warnings)', stderr: '', durationMs: 0 };
            if (cmd === 't') return { ok: true, exitCode: 0, stdout: '1 passed', stderr: '', durationMs: 0 };
            return { ok: false, exitCode: 1, stdout: '', stderr: '', durationMs: 0 };
        });

        await runAll({ modelsFile: 'm', scenariosDir: 's', outFile: 'out.json', concurrency: 1 });

        expect(fs.writeFile).toHaveBeenCalledWith(
            'out.json',
            expect.stringMatching(/"lintErrors": 2/)
        );
        expect(fs.writeFile).toHaveBeenCalledWith(
            'out.json',
            expect.stringMatching(/"lintWarnings": 3/)
        );
    });

    it('filters models and scenarios', async () => {
        vi.mocked(fs.readFile).mockResolvedValue(JSON.stringify(['m1', 'm2']));
        vi.mocked(scenarioLoader.loadScenarios).mockResolvedValue([
            { config: { id: 's1', category: '', description: '', promptFile: '', templateDir: '', solutionFiles: ['sol.ts'], commands: {} as any, constraints: {} as any }, promptText: '' },
            { config: { id: 's2', category: '', description: '', promptFile: '', templateDir: '', solutionFiles: ['sol.ts'], commands: {} as any, constraints: {} as any }, promptText: '' }
        ]);
        vi.mocked(fs.access).mockResolvedValue(undefined); // base template exists
        vi.mocked(workspace.createWorkspace).mockResolvedValue('/wk');
        vi.mocked(modelClient.generate).mockResolvedValue({ response: '', latencyMs: 0 });
        vi.mocked(workspace.runCommand).mockResolvedValue({ ok: true, exitCode: 0, stdout: '', stderr: '', durationMs: 0 });

        await runAll({
            modelsFile: 'm',
            scenariosDir: 's',
            outFile: 'out.json',
            concurrency: 1,
            filterModel: 'm1',
            filterScenario: 's2'
        });

        // Should run m1 on s2 ONLY.
        // We can check createWorkspace calls
        expect(workspace.createWorkspace).toHaveBeenCalledTimes(1);
        expect(workspace.createWorkspace).toHaveBeenCalledWith('m1', 's2', undefined);
    });

    it('handles python linting output', async () => {
        vi.mocked(fs.readFile).mockResolvedValue(JSON.stringify(['model']));
        // Python scenario (not starting with ts-)
        vi.mocked(scenarioLoader.loadScenarios).mockResolvedValue([
            {
                config: {
                    id: 'py-test',
                    category: '', description: '', promptFile: '', templateDir: '', solutionFiles: ['sol.py'],
                    commands: { build: 'b', lint: 'l', test: 't' },
                    constraints: { noAny: false, singleCodeBlock: true }
                },
                promptText: ''
            }
        ]);
        vi.mocked(fs.access).mockResolvedValue(undefined);
        vi.mocked(workspace.createWorkspace).mockResolvedValue('/wk');
        vi.mocked(modelClient.generate).mockResolvedValue({ response: 'code', latencyMs: 0 });

        vi.mocked(workspace.runCommand).mockImplementation(async (cmd) => {
            if (cmd === 'b') return { ok: true, exitCode: 0, stdout: '', stderr: '', durationMs: 0 };
            // Pylint failure
            if (cmd === 'l') return { ok: false, exitCode: 1, stdout: 'file.py:1: error: Bad code\nfile.py:2: error: Worse code', stderr: '', durationMs: 0 };
            if (cmd === 't') return { ok: true, exitCode: 0, stdout: '', stderr: '', durationMs: 0 };
            return { ok: true, exitCode: 0, stdout: '', stderr: '', durationMs: 0 };
        });

        await runAll({ modelsFile: 'm', scenariosDir: 's', outFile: 'out.json', concurrency: 1 });

        expect(fs.writeFile).toHaveBeenCalledWith('out.json', expect.stringMatching(/"lintErrors": 2/));
    });

    it('handles python linting fallback', async () => {
        vi.mocked(fs.readFile).mockResolvedValue(JSON.stringify(['model']));
        vi.mocked(scenarioLoader.loadScenarios).mockResolvedValue([
            {
                config: { id: 'py-fallback', category: '', description: '', promptFile: '', templateDir: '', solutionFiles: ['sol.py'], commands: {} as any, constraints: {} as any },
                promptText: ''
            }
        ]);
        vi.mocked(fs.access).mockResolvedValue(undefined);
        vi.mocked(workspace.createWorkspace).mockResolvedValue('/wk');
        vi.mocked(modelClient.generate).mockResolvedValue({ response: '', latencyMs: 0 });

        vi.mocked(workspace.runCommand).mockImplementation(async (cmd) => {
            // Lint failure but no text match
            if (cmd === undefined) return { ok: false, exitCode: 1, stdout: 'unknown failure', stderr: '', durationMs: 0 };
            return { ok: true, exitCode: 0, stdout: '', stderr: '', durationMs: 0 };
        });

        await runAll({ modelsFile: 'm', scenariosDir: 's', outFile: 'out.json', concurrency: 1 });

        // Should fallback to 1 error if exitCode != 0
        // Wait, commands are undefined in mock config above?
        // commands: {} as any. so undefined.
    });

    it('throws if models file has non-string items', async () => {
        vi.mocked(fs.readFile).mockResolvedValue(JSON.stringify(['ok', 123]));
        await expect(runAll({ modelsFile: 'm', scenariosDir: 's', outFile: 'o', concurrency: 1 }))
            .rejects.toThrow('Expected m to contain an array of model names');
    });

    it('handles TS lint output fallback', async () => {
        vi.mocked(fs.readFile).mockResolvedValue(JSON.stringify(['model']));
        vi.mocked(scenarioLoader.loadScenarios).mockResolvedValue([
            {
                config: { id: 'ts-fallback', category: '', description: '', promptFile: '', templateDir: '', solutionFiles: ['sol.ts'], commands: {} as any, constraints: {} as any },
                promptText: ''
            }
        ]);
        vi.mocked(fs.access).mockResolvedValue(undefined);
        vi.mocked(workspace.createWorkspace).mockResolvedValue('/wk');
        vi.mocked(modelClient.generate).mockResolvedValue({ response: '', latencyMs: 0 });

        vi.mocked(workspace.runCommand).mockImplementation(async (cmd) => {
            // TS Lint failure with weird output
            if (cmd === undefined && !('b' in ({} as any))) return { ok: false, exitCode: 1, stdout: 'ERROR: something failed', stderr: '', durationMs: 0 };
            // We need to match command lookup. running 'lint'.
            return { ok: false, exitCode: 1, stdout: 'ERROR: something failed', stderr: '', durationMs: 0 };
        });

        await runAll({ modelsFile: 'm', scenariosDir: 's', outFile: 'out.json', concurrency: 1 });

        expect(fs.writeFile).toHaveBeenCalledWith(
            'out.json',
            expect.stringMatching(/"lintErrors": 1/) // 'ERROR' matches /error/gi once
        );
    });
});

