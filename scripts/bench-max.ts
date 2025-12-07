import { execa } from 'execa';
import os from 'os';
import process from 'process';

async function main() {
    const isWindows = os.platform() === 'win32';

    // Configuration
    const defaults = {
        LIMIT: 10,
        CONCURRENCY: 10,
        NUM_GPU: 999,
        MAIN_GPU: 0,
        PARALLEL_SLOTS: 10,
    };

    const args = process.argv.slice(2);
    const config: Record<string, string | number> = {};
    const passthroughArgs: string[] = [];

    // Parse args manually to handle key=value and standard flags
    for (let i = 0; i < args.length; i++) {
        const arg = args[i];
        if (arg.includes('=')) {
            const [key, value] = arg.split('=');
            if (key === 'limit') config.LIMIT = Number(value);
            else if (key === 'concurrency') config.CONCURRENCY = Number(value);
            else if (key === 'main-gpu' || key === 'mainGpu') config.MAIN_GPU = Number(value);
            else if (key === 'num-gpu' || key === 'numGpu') config.NUM_GPU = Number(value);
            else if (key === 'parallel') config.PARALLEL_SLOTS = Number(value);
            else passthroughArgs.push(arg);
        } else if (arg.startsWith('--')) {
            const key = arg.replace(/^--/, '');
            const value = args[i + 1];

            // Heuristic: if next arg is a number, consume it
            const isNextNumber = value && !Number.isNaN(Number(value)) && !value.startsWith('-');

            if (key === 'limit' && isNextNumber) { config.LIMIT = Number(value); i++; }
            else if (key === 'concurrency' && isNextNumber) { config.CONCURRENCY = Number(value); i++; }
            else if (key === 'main-gpu' && isNextNumber) { config.MAIN_GPU = Number(value); i++; }
            else if (key === 'num-gpu' && isNextNumber) { config.NUM_GPU = Number(value); i++; }
            else { passthroughArgs.push(arg); }
        } else if (!Number.isNaN(Number(arg)) && i === 0) {
            // Assume first positional number is limit (backward compat / convenient)
            config.LIMIT = Number(arg);
        } else {
            passthroughArgs.push(arg);
        }
    }

    const LIMIT = config.LIMIT ?? defaults.LIMIT;
    const CONCURRENCY = config.CONCURRENCY ?? defaults.CONCURRENCY;
    const NUM_GPU = config.NUM_GPU ?? defaults.NUM_GPU;
    const MAIN_GPU = config.MAIN_GPU ?? defaults.MAIN_GPU;
    const PARALLEL_SLOTS = config.PARALLEL_SLOTS ?? defaults.PARALLEL_SLOTS;

    console.log('Stopping any existing Ollama instance...');
    try {
        if (isWindows) {
            await execa('taskkill', ['/F', '/IM', 'ollama.exe', '/T'], { reject: false });
        } else {
            await execa('pkill', ['ollama'], { reject: false });
        }
    } catch (e) {
        // Ignore if not found
    }

    // Wait loop to ensure it's actually dead
    console.log('Waiting for port 11434 to be free...');
    const maxRetries = 10;
    for (let i = 0; i < maxRetries; i++) {
        try {
            // Check if something is listening on 11434
            // On mac/linux: lsof -i :11434
            // On win: netstat -ano | findstr :11434
            // Simpler: just try to connect? Or just sleep longer and retry kill?
            // Let's just robustly retry kill and sleep.
            if (i > 0) {
                if (isWindows) {
                    await execa('taskkill', ['/F', '/IM', 'ollama.exe', '/T'], { reject: false });
                } else {
                    await execa('pkill', ['-9', 'ollama'], { reject: false });
                }
            }
            await new Promise(r => setTimeout(r, 1000));
        } catch (e) { }
    }

    console.log(`Starting dedicated Ollama instance with OLLAMA_NUM_PARALLEL=${PARALLEL_SLOTS}...`);

    const env = {
        ...process.env,
        OLLAMA_NUM_PARALLEL: String(PARALLEL_SLOTS),
        OLLAMA_MAX_LOADED_MODELS: '1'
    };

    let ollamaProcess;
    if (isWindows) {
        // On Windows, 'ollama serve' might need to be run directly
        ollamaProcess = execa('ollama', ['serve'], {
            env,
            detached: true,
            stdio: 'ignore'
        });
    } else {
        ollamaProcess = execa('ollama', ['serve'], {
            env,
            detached: true,
            stdio: 'ignore'
        });
    }

    ollamaProcess.unref();

    console.log('Waiting for Ollama to start...');
    await new Promise(r => setTimeout(r, 5000));

    console.log('Running benchmark...');
    try {
        await execa('npm', [
            'run', 'bench', '--',
            '--limit', String(LIMIT),
            '--sequential-models',
            '--concurrency', String(CONCURRENCY),
            '--main-gpu', String(MAIN_GPU),
            '--num-gpu', String(NUM_GPU),
            ...passthroughArgs
        ], { stdio: 'inherit' });
    } catch (e) {
        console.error('Benchmark failed or was interrupted');
        process.exitCode = 1;
    } finally {
        console.log('Benchmark complete. Cleaning up...');
        if (isWindows) {
            await execa('taskkill', ['/F', '/IM', 'ollama.exe', '/T'], { reject: false });
        } else {
            // Since we detached, we might need to pkill again to be sure
            await execa('pkill', ['ollama'], { reject: false });
        }
    }
}

main().catch(console.error);
