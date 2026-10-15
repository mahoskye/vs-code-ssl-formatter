#!/usr/bin/env node
/**
 * Syncs syntaxes/ssl.tmLanguage.json from the canonical TextMate grammar in
 * ssl-style-guide (ssl-style-guide/ssl.tmLanguage.updated.json, decided in
 * mahoskye/ssl-style-guide#91). The extension never edits its copy by hand;
 * grammar changes go upstream first (#113).
 *
 *   node scripts/sync-grammar.mjs [ref]   copy the grammar at <ref> (default:
 *                                         origin/main of ../ssl-style-guide)
 *   node scripts/sync-grammar.mjs --check fail if the shipped grammar differs
 *                                         from the commit it records
 *
 * The copy records its source commit the way VS Code's built-in grammars do,
 * in `information_for_contributors` and `version`. `--check` reads that commit
 * from the sibling checkout when it is present (override the path with
 * SSL_STYLE_GUIDE) and from GitHub otherwise, so it also runs in CI. It also
 * notes, without failing, when the sibling's origin/main has a different grammar.
 */
import { execFileSync } from 'child_process';
import { existsSync, readFileSync, writeFileSync } from 'fs';
import { dirname, resolve } from 'path';
import { fileURLToPath } from 'url';

const PROJECT_ROOT = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const UPSTREAM_REPO = 'mahoskye/ssl-style-guide';
const UPSTREAM_PATH = 'ssl-style-guide/ssl.tmLanguage.updated.json';
const SIBLING = process.env.SSL_STYLE_GUIDE || resolve(PROJECT_ROOT, '..', 'ssl-style-guide');
const TARGET = resolve(PROJECT_ROOT, 'syntaxes/ssl.tmLanguage.json');
const COMMIT_URL = `https://github.com/${UPSTREAM_REPO}/commit/`;

const git = (...args) => execFileSync('git', ['-C', SIBLING, ...args], {
    encoding: 'utf-8',
    stdio: ['ignore', 'pipe', 'pipe'],
    maxBuffer: 16 * 1024 * 1024,
});

const hasSibling = () => existsSync(resolve(SIBLING, '.git'));

/** The upstream grammar at `sha`, from the sibling checkout or GitHub. */
async function upstreamGrammar(sha) {
    if (hasSibling()) {
        try {
            return JSON.parse(git('show', `${sha}:${UPSTREAM_PATH}`));
        } catch {
            // The sibling may not have fetched this commit; fall back to GitHub.
        }
    }
    const url = `https://raw.githubusercontent.com/${UPSTREAM_REPO}/${sha}/${UPSTREAM_PATH}`;
    const response = await fetch(url);
    if (!response.ok) {
        throw new Error(`GET ${url}: ${response.status} ${response.statusText}`);
    }
    return JSON.parse(await response.text());
}

/** The grammar without the provenance keys this script adds. */
function stripProvenance(grammar) {
    const { information_for_contributors: _info, version: _version, ...rest } = grammar;
    return rest;
}

function render(grammar, sha) {
    const withProvenance = {
        information_for_contributors: [
            `This file is generated from ${UPSTREAM_REPO}/${UPSTREAM_PATH}, the canonical SSL TextMate grammar.`,
            'Do not edit it here. Change the grammar upstream, then run `npm run sync:grammar` (see #113).',
        ],
        version: COMMIT_URL + sha,
        ...stripProvenance(grammar),
    };
    return JSON.stringify(withProvenance, null, 2) + '\n';
}

async function sync(ref) {
    if (!hasSibling()) {
        console.error(`No ssl-style-guide checkout at ${SIBLING}. Set SSL_STYLE_GUIDE or clone it there.`);
        process.exit(2);
    }
    let sha;
    try {
        sha = git('rev-parse', '--verify', `${ref}^{commit}`).trim();
    } catch (err) {
        console.error(`Unknown ref "${ref}" in ${SIBLING}: ${err.stderr || err.message}`);
        console.error('Run `git -C ../ssl-style-guide fetch` first.');
        process.exit(2);
    }
    writeFileSync(TARGET, render(await upstreamGrammar(sha), sha), 'utf-8');
    console.log(`Synced syntaxes/ssl.tmLanguage.json from ${UPSTREAM_REPO}@${sha.slice(0, 7)} (${ref}).`);
}

async function check() {
    const shipped = JSON.parse(readFileSync(TARGET, 'utf-8'));
    const sha = typeof shipped.version === 'string' && shipped.version.startsWith(COMMIT_URL)
        ? shipped.version.slice(COMMIT_URL.length)
        : '';
    if (!/^[0-9a-f]{40}$/.test(sha)) {
        console.error('syntaxes/ssl.tmLanguage.json does not record its upstream commit. Run `npm run sync:grammar`.');
        process.exit(1);
    }
    const expected = render(await upstreamGrammar(sha), sha);
    if (readFileSync(TARGET, 'utf-8') !== expected) {
        console.error(`syntaxes/ssl.tmLanguage.json differs from ${UPSTREAM_REPO}@${sha.slice(0, 7)}.`);
        console.error('Do not edit it by hand: change the grammar upstream, then run `npm run sync:grammar`.');
        process.exit(1);
    }
    console.log(`Grammar matches ${UPSTREAM_REPO}@${sha.slice(0, 7)}.`);

    if (hasSibling()) {
        try {
            const latest = git('rev-parse', 'origin/main').trim();
            const latestGrammar = stripProvenance(await upstreamGrammar(latest));
            if (JSON.stringify(latestGrammar) !== JSON.stringify(stripProvenance(shipped))) {
                console.log(`Note: ${UPSTREAM_REPO} origin/main (${latest.slice(0, 7)}) has a different grammar. If upstream has moved on, run \`npm run sync:grammar\` to pick it up.`);
            }
        } catch {
            // No origin/main in the sibling; nothing to compare.
        }
    }
}

const arg = process.argv[2];
if (arg === '--check') {
    await check();
} else {
    await sync(arg || 'origin/main');
}
