#!/usr/bin/env bun
/**
 * Corpus regression harness for the bundled starlims-lsp binary.
 *
 * Runs the shipped formatter and validator over a large body of real SSL
 * (thousands of .srvscr/.ds files) and asserts three invariants:
 *
 *   1. Formatting never fails.
 *   2. Formatting is idempotent — a second pass changes nothing.
 *   3. Formatting never introduces diagnostics — no file may come out of the
 *      formatter with more errors than it went in with. A known server defect
 *      can be accepted by recording its file count in the baseline
 *      (`filesGainingErrors`); the check then fails only when that count
 *      changes, and still lists the files on every run.
 *
 * It then diffs the corpus-wide diagnostic counts (by rule slug) against
 * tests/corpus-baseline.json, which is how an LSP bundle bump gets reviewed:
 * the deltas tell you exactly which rules changed behaviour on real code.
 *
 * Opt-in. Not part of `npm test` or CI — the corpus is a separate, private
 * repository of real customer scripts and must never be copied into this one.
 * The canonical corpus is starlims-corpus's script-codebase/, checked out
 * beside this repository; the baseline records which corpus it came from.
 *
 *   bun scripts/corpus-test.ts                  # check against the baseline
 *   bun scripts/corpus-test.ts --update-baseline
 *   SSL_CORPUS=/path/to/corpus bun scripts/corpus-test.ts
 *
 * Flags:
 *   --update-baseline  Rewrite tests/corpus-baseline.json from this run.
 *   --keep             Leave the formatted working copy on disk and print it.
 *
 * Exit codes: 0 pass, 1 invariant violation or baseline drift, 2 setup error.
 */
import { mkdtempSync, mkdirSync, rmSync, existsSync, readdirSync } from "fs";
import { copyFile, readFile, writeFile } from "fs/promises";
import { execFile } from "child_process";
import { createHash } from "crypto";
import * as os from "os";
import * as path from "path";
import { fileURLToPath } from "url";

// Node APIs throughout rather than the Bun globals: the script is run with
// bun, but staying on the standard library keeps it typechecking in editors
// without pulling in @types/bun.
const PROJECT_ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const BASELINE_PATH = path.join(PROJECT_ROOT, "tests", "corpus-baseline.json");
/** The canonical corpus, relative to the directory holding this checkout. */
const CANONICAL_CORPUS = "starlims-corpus/script-codebase";
const SSL_EXTENSIONS = [".ssl", ".srvscr", ".ds", ".ssl.txt", ".ds.txt"];

/** How many paths to hand a single binary invocation. */
const CHUNK_SIZE = 200;

const args = new Set(process.argv.slice(2));
const UPDATE_BASELINE = args.has("--update-baseline");
const KEEP_WORKDIR = args.has("--keep");

type Severity = "error" | "warning" | "info" | "hint";

interface Diagnostic {
    line: number;
    column: number;
    severity: Severity;
    message: string;
    source?: string;
    code?: string;
}

interface ValidateResult {
    file: string;
    valid: boolean;
    diagnostics: Diagnostic[] | null;
}

interface Baseline {
    lspVersion: string;
    /**
     * Which corpus produced the counts: its path relative to the directory
     * holding this checkout, or "SSL_CORPUS" for one outside it. Never an
     * absolute path, so no local layout leaks into the baseline.
     */
    corpus?: string;
    corpusFiles: number;
    /**
     * Files that come out of the formatter with more errors than they went in
     * with: a known server defect accepted until it is fixed. Absent means 0.
     */
    filesGainingErrors?: number;
    totals: Partial<Record<Severity, number>>;
    codes: Record<string, Partial<Record<Severity, number>>>;
}

function fail(message: string): never {
    console.error(`\n✗ ${message}`);
    process.exit(1);
}

function setupError(message: string): never {
    console.error(`\n✗ ${message}`);
    process.exit(2);
}

/**
 * Platform-specific binary name. Mirrors getServerBinaryName() in
 * src/lspClient.ts, which is not exported and pulls in the vscode module.
 */
function serverBinaryName(): string {
    const platform = os.platform();
    const arch = os.arch();

    const platformStr =
        platform === "win32" ? "windows" : platform === "darwin" ? "darwin" : "linux";
    // Only an amd64 build is published for Windows; it runs fine under
    // emulation on arm64.
    const archStr = platform === "win32" ? "amd64" : arch === "arm64" ? "arm64" : "amd64";
    const ext = platform === "win32" ? ".exe" : "";

    return `starlims-lsp-${platformStr}-${archStr}${ext}`;
}

function resolveCorpus(): string {
    // An empty SSL_CORPUS counts as unset rather than as a path to nowhere.
    const explicit = process.env.SSL_CORPUS?.trim() || undefined;
    const candidate = explicit ?? path.resolve(PROJECT_ROOT, "..", CANONICAL_CORPUS);

    if (!existsSync(candidate)) {
        if (explicit) {
            setupError(`SSL_CORPUS points at ${explicit}, which does not exist.`);
        }
        console.log(
            "Corpus not found — skipping. Check out starlims-corpus beside this " +
                `repository (expected at ${candidate}), or set SSL_CORPUS.`
        );
        process.exit(0);
    }
    return path.resolve(candidate);
}

/** The baseline's label for a corpus root (see Baseline.corpus). */
function corpusLabel(corpusRoot: string): string {
    const relative = path.relative(path.resolve(PROJECT_ROOT, ".."), corpusRoot);
    return relative && !relative.startsWith("..") && !path.isAbsolute(relative)
        ? relative.split(path.sep).join("/")
        : "SSL_CORPUS";
}

function collectSslFiles(root: string): string[] {
    const out: string[] = [];

    const walk = (dir: string) => {
        for (const entry of readdirSync(dir, { withFileTypes: true })) {
            if (entry.name === ".git" || entry.name === "node_modules") {
                continue;
            }
            const full = path.join(dir, entry.name);
            if (entry.isDirectory()) {
                walk(full);
            } else if (entry.isFile() && SSL_EXTENSIONS.some((e) => entry.name.endsWith(e))) {
                out.push(full);
            }
        }
    };

    walk(root);
    return out.sort();
}

function chunk<T>(items: T[], size: number): T[][] {
    const out: T[][] = [];
    for (let i = 0; i < items.length; i += size) {
        out.push(items.slice(i, i + size));
    }
    return out;
}

function runBinary(
    binary: string,
    argv: string[]
): Promise<{ exitCode: number; stdout: string; stderr: string }> {
    return new Promise((resolve) => {
        execFile(binary, argv, { maxBuffer: 1 << 28 }, (error, stdout, stderr) => {
            // A non-zero exit surfaces as `error`; the validator uses exit 1
            // for "found errors", so the code is returned rather than thrown.
            const exitCode = error ? ((error as NodeJS.ErrnoException & { code?: number }).code ?? 1) : 0;
            resolve({ exitCode: typeof exitCode === "number" ? exitCode : 1, stdout, stderr });
        });
    });
}

/** Copy the corpus into a scratch directory so the originals are never touched. */
async function stageWorkingCopy(files: string[], corpusRoot: string, workDir: string) {
    for (const file of files) {
        const dest = path.join(workDir, path.relative(corpusRoot, file));
        mkdirSync(path.dirname(dest), { recursive: true });
        await copyFile(file, dest);
    }
}

async function hashFiles(files: string[]): Promise<Map<string, string>> {
    const hashes = new Map<string, string>();
    for (const file of files) {
        hashes.set(file, createHash("sha1").update(await readFile(file)).digest("hex"));
    }
    return hashes;
}

/** Format every file in place, failing the run if the binary errors out. */
async function formatInPlace(binary: string, files: string[]): Promise<void> {
    for (const group of chunk(files, CHUNK_SIZE)) {
        const { exitCode, stderr } = await runBinary(binary, ["--format", "--write", ...group]);
        if (exitCode !== 0) {
            fail(`Formatter exited ${exitCode}:\n${stderr.trim().slice(0, 4000)}`);
        }
    }
}

/**
 * Validate, preserving input order. The binary reports only basenames, which
 * collide across directories, so results are paired with inputs positionally
 * and the basename is checked as a guard against drift in that contract.
 */
async function validate(binary: string, files: string[]): Promise<ValidateResult[]> {
    const results: ValidateResult[] = [];

    for (const group of chunk(files, CHUNK_SIZE)) {
        // Exit code 1 just means some file has errors, which is expected.
        const { exitCode, stdout, stderr } = await runBinary(binary, ["--validate", ...group]);
        if (exitCode > 1) {
            fail(`Validator exited ${exitCode}:\n${stderr.trim().slice(0, 4000)}`);
        }

        let parsed: ValidateResult[];
        try {
            parsed = JSON.parse(stdout);
        } catch (error) {
            fail(`Could not parse validator output: ${(error as Error).message}`);
        }
        if (parsed.length !== group.length) {
            fail(`Validator returned ${parsed.length} results for ${group.length} files.`);
        }
        parsed.forEach((result, i) => {
            if (result.file !== path.basename(group[i])) {
                fail(
                    `Validator output is out of order: expected ${path.basename(group[i])}, ` +
                        `got ${result.file}.`
                );
            }
        });
        results.push(...parsed);
    }

    return results;
}

function severityCounts(result: ValidateResult): Record<string, number> {
    const counts: Record<string, number> = {};
    for (const diagnostic of result.diagnostics ?? []) {
        counts[diagnostic.severity] = (counts[diagnostic.severity] ?? 0) + 1;
    }
    return counts;
}

function buildBaseline(
    lspVersion: string,
    corpus: string,
    results: ValidateResult[],
    filesGainingErrors: number
): Baseline {
    const totals: Partial<Record<Severity, number>> = {};
    const codes: Record<string, Partial<Record<Severity, number>>> = {};

    for (const result of results) {
        for (const diagnostic of result.diagnostics ?? []) {
            const severity = diagnostic.severity;
            totals[severity] = (totals[severity] ?? 0) + 1;

            const code = diagnostic.code ?? "(uncoded)";
            const bucket = (codes[code] ??= {});
            bucket[severity] = (bucket[severity] ?? 0) + 1;
        }
    }

    // Sorted so the checked-in baseline diffs cleanly.
    const sortedCodes: Baseline["codes"] = {};
    for (const code of Object.keys(codes).sort()) {
        sortedCodes[code] = codes[code];
    }

    return {
        lspVersion,
        corpus,
        corpusFiles: results.length,
        ...(filesGainingErrors > 0 ? { filesGainingErrors } : {}),
        totals,
        codes: sortedCodes,
    };
}

function countOf(bucket: Partial<Record<Severity, number>> | undefined, severity: string): number {
    return (bucket as Record<string, number> | undefined)?.[severity] ?? 0;
}

/** Returns a human-readable list of differences, empty when the two agree. */
function diffBaselines(expected: Baseline, actual: Baseline): string[] {
    const lines: string[] = [];
    const severities: Severity[] = ["error", "warning", "info", "hint"];

    for (const severity of severities) {
        const before = countOf(expected.totals, severity);
        const after = countOf(actual.totals, severity);
        if (before !== after) {
            lines.push(`  total ${severity}: ${before} → ${after} (${after - before >= 0 ? "+" : ""}${after - before})`);
        }
    }

    for (const code of [...new Set([...Object.keys(expected.codes), ...Object.keys(actual.codes)])].sort()) {
        for (const severity of severities) {
            const before = countOf(expected.codes[code], severity);
            const after = countOf(actual.codes[code], severity);
            if (before !== after) {
                lines.push(`  ${code} (${severity}): ${before} → ${after} (${after - before >= 0 ? "+" : ""}${after - before})`);
            }
        }
    }

    return lines;
}

async function main() {
    const binary = path.join(PROJECT_ROOT, "server", serverBinaryName());
    if (!existsSync(binary)) {
        setupError(`Bundled server binary not found at ${binary}. Run \`npm run fetch-lsp\` first.`);
    }

    const corpusRoot = resolveCorpus();
    const originals = collectSslFiles(corpusRoot);
    if (originals.length === 0) {
        setupError(`No SSL files (${SSL_EXTENSIONS.join(", ")}) found under ${corpusRoot}.`);
    }

    const { stdout: versionOut } = await runBinary(binary, ["--version"]);
    const lspVersion = versionOut.match(/version\s+(\S+)/)?.[1] ?? "unknown";

    console.log(`corpus:  ${corpusRoot}`);
    console.log(`files:   ${originals.length}`);
    console.log(`server:  ${path.basename(binary)} (${lspVersion})\n`);

    const workDir = mkdtempSync(path.join(os.tmpdir(), "ssl-corpus-"));
    let failures = 0;

    try {
        await stageWorkingCopy(originals, corpusRoot, workDir);
        const working = originals.map((f) => path.join(workDir, path.relative(corpusRoot, f)));

        // Invariant 1: formatting never fails.
        console.log("pass 1: formatting…");
        const beforeFormat = await hashFiles(working);
        await formatInPlace(binary, working);
        const afterFirst = await hashFiles(working);
        const reformatted = working.filter((f) => beforeFormat.get(f) !== afterFirst.get(f));
        console.log(`  reformatted ${reformatted.length}/${working.length} files`);

        // Invariant 2: formatting is idempotent.
        console.log("pass 2: re-formatting to check idempotency…");
        await formatInPlace(binary, working);
        const afterSecond = await hashFiles(working);
        const unstable = working.filter((f) => afterFirst.get(f) !== afterSecond.get(f));
        if (unstable.length > 0) {
            failures++;
            console.error(`\n✗ ${unstable.length} file(s) changed on the second format pass:`);
            for (const f of unstable.slice(0, 20)) {
                console.error(`    ${path.relative(workDir, f)}`);
            }
            if (unstable.length > 20) {
                console.error(`    …and ${unstable.length - 20} more`);
            }
        } else {
            console.log("  ✓ idempotent");
        }

        // Invariant 3: formatting never introduces diagnostics.
        console.log("validating originals and formatted output…");
        const before = await validate(binary, originals);
        const after = await validate(binary, working);

        const regressions: string[] = [];
        for (let i = 0; i < before.length; i++) {
            const errorsBefore = severityCounts(before[i]).error ?? 0;
            const errorsAfter = severityCounts(after[i]).error ?? 0;
            if (errorsAfter > errorsBefore) {
                regressions.push(
                    `${path.relative(corpusRoot, originals[i])}: ${errorsBefore} → ${errorsAfter} errors`
                );
            }
        }
        const knownRegressions = existsSync(BASELINE_PATH)
            ? ((JSON.parse(await readFile(BASELINE_PATH, "utf8")) as Baseline).filesGainingErrors ?? 0)
            : 0;
        if (regressions.length > 0) {
            const accepted = !UPDATE_BASELINE && regressions.length === knownRegressions;
            if (!accepted && !UPDATE_BASELINE) {
                failures++;
            }
            const mark = accepted || UPDATE_BASELINE ? "!" : "✗";
            const note = accepted
                ? " (matches the baseline's known count)"
                : UPDATE_BASELINE
                  ? " (recorded in the baseline as a known defect)"
                  : ` (baseline expects ${knownRegressions})`;
            console.error(`\n${mark} formatting introduced errors in ${regressions.length} file(s)${note}:`);
            for (const line of regressions.slice(0, 20)) {
                console.error(`    ${line}`);
            }
            if (regressions.length > 20) {
                console.error(`    …and ${regressions.length - 20} more`);
            }
        } else {
            console.log("  ✓ no file gained errors from formatting");
            if (knownRegressions > 0 && !UPDATE_BASELINE) {
                failures++;
                console.error(
                    `\n✗ The baseline expects ${knownRegressions} file(s) to gain errors, and none did. ` +
                        "If the server fixed it, re-baseline with `npm run test:corpus:update`."
                );
            }
        }

        // Baseline: diagnostic counts on the unformatted corpus.
        const actual = buildBaseline(lspVersion, corpusLabel(corpusRoot), before, regressions.length);

        if (UPDATE_BASELINE) {
            await writeFile(BASELINE_PATH, JSON.stringify(actual, null, 4) + "\n");
            console.log(`\nwrote baseline → ${path.relative(PROJECT_ROOT, BASELINE_PATH)}`);
            console.log(
                `  ${Object.entries(actual.totals)
                    .map(([k, v]) => `${v} ${k}`)
                    .join(", ")} across ${actual.corpusFiles} files`
            );
        } else if (!existsSync(BASELINE_PATH)) {
            failures++;
            console.error(
                `\n✗ No baseline at ${path.relative(PROJECT_ROOT, BASELINE_PATH)}. ` +
                    "Create one with `npm run test:corpus:update`."
            );
        } else {
            const expected: Baseline = JSON.parse(await readFile(BASELINE_PATH, "utf8"));

            if ((expected.corpus ?? "unrecorded") !== actual.corpus) {
                failures++;
                console.error(
                    `\n✗ The baseline came from a different corpus (${expected.corpus ?? "unrecorded"}) ` +
                        `than this run (${actual.corpus}), so diagnostic counts are not comparable. ` +
                        `Run against ${CANONICAL_CORPUS}, or re-baseline with \`npm run test:corpus:update\`.`
                );
            } else if (expected.corpusFiles !== actual.corpusFiles) {
                failures++;
                console.error(
                    `\n✗ Corpus changed size (${expected.corpusFiles} → ${actual.corpusFiles} files), ` +
                        "so diagnostic counts are not comparable. Re-baseline with " +
                        "`npm run test:corpus:update`."
                );
            } else {
                const differences = diffBaselines(expected, actual);
                if (differences.length > 0) {
                    failures++;
                    const versionNote =
                        expected.lspVersion === actual.lspVersion
                            ? "on the same LSP version — this is a regression"
                            : `across ${expected.lspVersion} → ${actual.lspVersion} — review, then ` +
                              "re-baseline with `npm run test:corpus:update`";
                    console.error(`\n✗ Diagnostic counts changed ${versionNote}:`);
                    for (const line of differences) {
                        console.error(line);
                    }
                } else {
                    console.log(`  ✓ diagnostics match baseline (${actual.lspVersion})`);
                }
            }
        }

        if (KEEP_WORKDIR) {
            console.log(`\nformatted working copy kept at ${workDir}`);
        }
    } finally {
        if (!KEEP_WORKDIR) {
            rmSync(workDir, { recursive: true, force: true });
        }
    }

    if (failures > 0) {
        console.error(`\n${failures} check(s) failed.`);
        process.exit(1);
    }
    console.log("\nAll corpus checks passed.");
}

await main();
