import fs from 'fs/promises';
import path from 'path';
import { execa } from 'execa';

export interface CommandResult {
  ok: boolean;
  exitCode: number | null;
  stdout: string;
  stderr: string;
  durationMs: number;
}

export async function createWorkspace(model: string, scenarioId: string): Promise<string> {
  const sanitizedModel = model.replace(/[^a-zA-Z0-9_-]/g, '_');
  const workspacePath = path.join('.workdir', `${sanitizedModel}-${scenarioId}-${Date.now()}`);
  await fs.mkdir(workspacePath, { recursive: true });
  return workspacePath;
}

export async function runCommand(command: string, cwd: string): Promise<CommandResult> {
  const start = Date.now();
  try {
    const { stdout, stderr, exitCode } = await execa(command, { shell: true, cwd });
    return {
      ok: true,
      exitCode: exitCode ?? 0,
      stdout,
      stderr,
      durationMs: Date.now() - start,
    };
  } catch (error) {
    const commandError = error as { stdout?: string; stderr?: string; exitCode?: number };
    return {
      ok: false,
      exitCode: typeof commandError.exitCode === 'number' ? commandError.exitCode : null,
      stdout: commandError.stdout ?? '',
      stderr: commandError.stderr ?? '',
      durationMs: Date.now() - start,
    };
  }
}
