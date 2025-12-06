
import { defineConfig } from 'vitest/config';

export default defineConfig({
    test: {
        include: ['tests/**/*.test.ts'],
        exclude: ['**/node_modules/**', '**/dist/**', '**/.workdir/**', '**/base-template/**', '**/scenarios/**'],
        coverage: {
            include: ['src/**/*.ts'],
            exclude: ['src/cli/index.ts', 'src/runner/types.ts'],
            reporter: ['text', 'json', 'html'],
            thresholds: {
                statements: 99,
                branches: 89,
                functions: 96,
                lines: 100
            }
        },
    },
});
