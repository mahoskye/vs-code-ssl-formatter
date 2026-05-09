#!/usr/bin/env node
import { build } from 'esbuild';
import { mkdirSync, writeFileSync } from 'node:fs';

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

// The repo's top-level package.json declares "type": "module", which makes
// Node treat any sibling .js as ESM. esbuild emits CJS, so drop a sentinel
// package.json next to the bundle to override the module type for this dir.
function writeDistManifest() {
    mkdirSync('dist', { recursive: true });
    writeFileSync('dist/package.json', JSON.stringify({ type: 'commonjs' }, null, 2) + '\n');
}

if (watch) {
    const ctx = await (await import('esbuild')).context(config);
    await ctx.watch();
    writeDistManifest();
    console.log('esbuild: watching for changes…');
} else {
    await build(config);
    writeDistManifest();
}
