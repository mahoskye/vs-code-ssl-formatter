# Corpus Regression Testing

`npm run test:corpus` runs the bundled `starlims-lsp` binary over a large body
of **real** SSL — thousands of production `.srvscr` and `.ds` files — and checks
that a new server build does not change how it behaves on that code.

Hand-written unit and integration tests cover the rules we thought to write
down. The corpus covers the code people actually ship: legacy comment blocks,
hand-wrapped SQL, decades-old formatting conventions, and constructs nobody
would think to invent for a test.

This is **opt-in and local**. It is deliberately not wired into `npm test` or
CI, because the corpus lives in a separate repository of real customer scripts.

> **Never copy corpus files, diffs, or excerpts into this repository.** They
> contain customer names, ticket numbers, and site-specific logic. This
> repository is public, so every test fixture is written synthetically: no
> identifiers, table or column names, comments or other text taken from a
> real deployment, edited or not. Only aggregate counts (the baseline below)
> may come from the corpus.

## Running it

```bash
npm run test:corpus
```

The corpus defaults to `../starlims-corpus/script-codebase`: the
`script-codebase/` tree of the private `starlims-corpus` repository, checked
out beside this one. It is the canonical real-world corpus for the STARLIMS
projects: a full production export (about 10,000 server scripts and data
sources), larger than the older `misc-test-files` snapshot, and the corpus the
classification and contract tooling works from. Use it so baseline counts stay
comparable from one bundle bump to the next. Point the harness elsewhere with
`SSL_CORPUS`:

```bash
SSL_CORPUS=/path/to/corpus npm run test:corpus
```

If no corpus is found the run exits 0 with a notice, so it is safe to invoke
from a machine that does not have one.

The script requires [bun](https://bun.sh) (it runs the TypeScript directly),
and uses the same platform-specific `server/starlims-lsp-*` binary the
extension ships.

## What it checks

The corpus is copied to a temp directory first; the originals are never
touched.

1. **Formatting never fails.** `--format --write` must exit 0 for every file.
2. **Formatting is idempotent.** A second pass must not change a single byte.
   Files that change again are listed by path.
3. **Formatting never introduces errors.** Every file is validated before and
   after formatting; no file may come out with more errors than it went in with.
4. **Diagnostic counts match the baseline.** Corpus-wide counts, bucketed by
   rule slug and severity, are diffed against `tests/corpus-baseline.json`.

Exit codes: `0` pass, `1` an invariant failed or the baseline drifted, `2` a
setup problem (missing binary, `SSL_CORPUS` pointing nowhere).

## The baseline

`tests/corpus-baseline.json` records the LSP version, which corpus produced
it, the corpus file count, and diagnostic counts per rule slug:

```json
{
    "lspVersion": "0.28.2",
    "corpus": "starlims-corpus/script-codebase",
    "corpusFiles": 10170,
    "totals": { "warning": 5355, "hint": 1721, "error": 325 },
    "codes": {
        "sql_injection": { "warning": 3022 }
    }
}
```

`corpus` is the corpus's path relative to the directory holding this checkout,
or `SSL_CORPUS` for one outside it, so no local path is recorded. Everything in
the file is an aggregate count; it never names a file.

`filesGainingErrors` (absent means 0) accepts a known server defect that
makes formatting add errors to some files. The check then fails only when that
number changes, and still lists the files on every run; once the server is
fixed, the check fails until you re-baseline, which drops the field. It is
absent since v0.28.2: v0.28.0 gave 2 files more errors (mahoskye/starlims-lsp#351,
fixed in v0.28.1, and mahoskye/starlims-lsp#355, fixed in v0.28.2).

Regenerate it after reviewing a change:

```bash
npm run test:corpus:update
```

The baseline is tied to a specific corpus. If the corpus or its file count
differs, the run fails with a re-baseline prompt rather than reporting
misleading deltas — counts from different corpora are not comparable.

## Using it for an LSP bundle bump

This is the harness's main job. After `npm run fetch-lsp` pulls a new server
build:

```bash
npm run test:corpus
```

A clean run means the new build formats and diagnoses thousands of real files
exactly as the previous one did. A failing run prints per-rule deltas:

```
✗ Diagnostic counts changed across 0.18.0 → 0.19.0 — review, then re-baseline:
  total warning: 7485 → 7702 (+217)
  sql_injection (warning): 1331 → 1548 (+217)
```

That tells you precisely which rule changed and by how much before it reaches
users. Once the deltas are understood and intended, re-baseline and commit the
updated `tests/corpus-baseline.json` alongside the bundle bump.

## Inspecting failures

`--keep` leaves the formatted working copy in place so you can diff individual
files against the corpus:

```bash
bun scripts/corpus-test.ts --keep
```

## Scope

The harness exercises the **shipped** LSP formatter and validator, which are
the only formatter and validator the extension has. There is deliberately no
native fallback formatter: a corpus run of the old TypeScript one found it
non-idempotent on 18% of real files and appending stray semicolons to SQL in
`.ds` documents, so it was removed rather than maintained as a second
implementation that drifts from the server's.

The SQL formatters that remain in `src/formatting/` back the **Format SQL**
commands, which reflow one SQL string on request and are not exercised here.
