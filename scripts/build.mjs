#!/usr/bin/env node
import { build } from 'esbuild';

const watch = process.argv.includes('--watch');
const production = process.argv.includes('--production');

const config = {
    entryPoints: ['src/extension.ts'],
    bundle: true,
    outfile: 'dist/extension.js',
    platform: 'node',
    target: 'node20',
    format: 'cjs',
    sourcemap: !production,
    minify: production,
    external: ['vscode'],
    logLevel: 'info',
};

if (watch) {
    const ctx = await (await import('esbuild')).context(config);
    await ctx.watch();
    console.log('esbuild: watching for changes…');
} else {
    await build(config);
}
