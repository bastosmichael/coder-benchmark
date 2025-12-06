
import tseslint from 'typescript-eslint';
import js from '@eslint/js';

export default tseslint.config(
    js.configs.recommended,
    ...tseslint.configs.recommended,
    {
        rules: {
            '@typescript-eslint/no-explicit-any': 'off',
            '@typescript-eslint/no-unused-vars': ['warn', { argsIgnorePattern: '^_' }],
            'no-undef': 'off', // TypeScript handles this
        },
        languageOptions: {
            ecmaVersion: 2022,
            sourceType: 'module',
        },
    }
);
