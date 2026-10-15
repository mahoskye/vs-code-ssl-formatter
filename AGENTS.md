# AGENTS.md

Guidance for agents and contributors working in this repository.

## What this is

`vs-code-ssl-formatter` is the VS Code extension for STARLIMS Scripting
Language (SSL), published as `mahoskye.vs-code-ssl-formatter`. SSL here means
the language as documented in the
[SSL reference](https://github.com/mahoskye/starlims-ssl-reference): written for
STARLIMS 11, verified on STARLIMS 12.

The extension is a thin client for the bundled
[starlims-lsp](https://github.com/mahoskye/starlims-lsp) server, plus editor
features the server does not provide:

| From the server (`server/`, started by `src/lspClient.ts`) | From the extension (`src/`) |
| --- | --- |
| Completion, hover, signature help, inlay hints | Code actions: rule fixes and Suppress / Show docs (`sslCodeActionProvider.ts`) |
| Definition, references, rename, document and workspace symbols, folding | CodeLens reference counts (`sslCodeLensProvider.ts`) |
| Formatting (document and range), embedded SQL formatting | Document highlights (`sslDocumentHighlightProvider.ts`) |
| Diagnostics, including `ssl.diagnostics.rules` overrides and suppression comments | Call hierarchy (`sslCallHierarchyProvider.ts`) |
| | Block closer (`sslBlockCloser.ts`), comment toggling (`sslCommentController.ts`) |
| | Format SQL commands (`commands/formatSql.ts`, `formatting/`) |
| | Status bar (`sslStatusBar.ts`), retired-setting migration (`utils/settingsMigration.ts`) |

Syntax highlighting comes from `syntaxes/ssl.tmLanguage.json`, a synced copy of
ssl-style-guide's canonical TextMate grammar.

## Decisions to keep

- **No native fallback for language features** (#106). The extension once
  re-implemented the server in TypeScript as a fallback; it drifted from the
  server and the SSL reference, and was removed. If the server cannot start,
  the user gets an error with Show Output and Retry. Do not reintroduce a
  second implementation of a feature the server provides.
- **No native formatter.** Formatting is the server's. A second formatter
  that drifts from it damages files.
- **The grammar is canonical upstream** (#113). Never edit
  `syntaxes/ssl.tmLanguage.json` by hand: change
  `ssl-style-guide/ssl.tmLanguage.updated.json` in
  [ssl-style-guide](https://github.com/mahoskye/ssl-style-guide), then run
  `npm run sync:grammar`. CI (`npm run check:grammar`) fails on a hand edit.
- **The server owns diagnostics.** It applies `ssl.diagnostics.rules` and
  suppression comments; the client only forwards settings (#107). Settings
  reach the server only as the tree `getSSLConfiguration()` builds.
- **Settings stay consistent.** Every contributed setting is in
  `src/constants/config.ts` and is read somewhere, and every setting the code
  reads is contributed; `tests/config.test.ts` enforces it. Removing a value
  from a setting's enum needs a migration entry in
  `src/utils/settingsMigration.ts` (see `compact`, #115).

## Build and test

```bash
npm ci
npm run lint            # ESLint (flat config, eslint.config.mjs)
npm run check-types     # tsc --noEmit
npm run knip            # unused files, exports and dependencies
npm run check:grammar   # grammar matches its recorded upstream commit
npm run test:unit       # mocha unit tests, including grammar tests
npm run test:integration  # VS Code integration tests against the real server
```

- Unit tests (`tests/*.test.ts`) run under an ESM loader with a `vscode` mock
  (`tests/setup.ts`, `tests/helpers/mockVSCode.ts`). Mark type-only imports
  `type`; ESLint enforces it, and the loader keeps any import it is not told
  is a type.
- Integration tests (`tests/integration/`) download VS Code into
  `.vscode-test/` and run the bundled extension with the real server.
- `npm run test:corpus` is an opt-in regression run of the bundled server over
  the private corpus; see [docs/CORPUS_TESTING.md](docs/CORPUS_TESTING.md).

## Data rules (this repository is public)

- Never copy code, file names, identifiers, comments or other text from a real
  STARLIMS deployment into this repository, its issues or its PRs. Test
  fixtures are written synthetically.
- The only material from the private corpus allowed here is aggregate counts:
  `tests/corpus-baseline.json` holds per-rule counts and names no file.
- Reproductions for upstream bugs are synthetic too.

## Sibling repositories

Checked out beside this one (`../<name>`):

| Repository | What this repo takes from it |
| --- | --- |
| `starlims-lsp` | The server binaries (`npm run fetch-lsp -- vX.Y.Z`) and the diagnostic codes (`npm run generate:slugs`, read at the bundled tag recorded in `server/LSP_VERSION`) |
| `ssl-style-guide` | The TextMate grammar (`npm run sync:grammar`) |
| `starlims-corpus` (private) | The corpus for `npm run test:corpus` (`script-codebase/`) |
| `starlims-program` | The program map and cross-project data boundaries |

## Release process

Most releases bundle a new starlims-lsp release.

1. `npm run fetch-lsp -- vX.Y.Z`: downloads the binaries, verifies their
   SHA-256 against the release digests, and records the tag in
   `server/LSP_VERSION` and `server/DOCUMENTATION.md`.
2. `npm run generate:slugs`: regenerates `src/constants/diagnosticSlugs.ts`
   and the `ssl.diagnostics.rules` key enum from that tag.
3. Contribute any new server setting in `package.json`, send it from
   `getSSLConfiguration()` in `src/lspClient.ts` under the server's key, and
   add it to `src/constants/config.ts`.
4. `npm run test:corpus`, review the deltas, then `npm run test:corpus:update`.
5. Bump the version in `package.json` and `package-lock.json`, and add a
   CHANGELOG entry (formatting changes need the reformat note: one
   reformat-only commit, `.git-blame-ignore-revs`, then format-on-save).
6. Branch `X.Y.Z-bundle-lsp-vA.B.C`, open a PR, and merge (merge commit) once
   CI is green.
7. Tag `vX.Y.Z` on the merge commit, push it, and publish a GitHub release
   (not a pre-release). `.github/workflows/publish.yml` packages every
   platform, smoke-tests the linux-x64 package, and publishes to the
   Marketplace; it refuses a tag that does not match `package.json`.

Afterwards, starlims-lsp's `docs/configuration/CONFIGURATION.md` §10.2 maps
this extension's setting names to server keys; update it when settings change.
