# Architecture

The extension is a language client for the bundled
[starlims-lsp](https://github.com/mahoskye/starlims-lsp) server, plus a few
editor features of its own. This page describes how the pieces fit; the
decisions behind them are in [AGENTS.md](../AGENTS.md).

## Activation

`src/extension.ts` runs when an SSL file opens:

1. Initializes the logger (`utils/logger.ts`, the **SSL Extension** output
   channel).
2. Migrates retired setting values (`utils/settingsMigration.ts`).
3. Registers commands, the status bar and the comment controller.
4. Starts the language client (`lspClient.ts`). If the server fails to start,
   an error offers **Show Output** and **Retry**, and the status bar shows
   `SSL · server stopped`.
5. Registers the extension's own providers.

## Language client (`src/lspClient.ts`)

- **Server binary**: `server/starlims-lsp-<os>-<arch>` for the host, run over
  stdio. Windows always uses the `amd64` build.
- **Settings**: `getSSLConfiguration()` maps the VS Code `ssl.*` settings to the
  server's settings tree (for example `ssl.format.wrapLength` →
  `ssl.format.maxLineLength`, `ssl.styleGuide.limitBlockDepth` →
  `ssl.diagnostics.maxBlockDepth`). The tree goes to the server in
  `initializationOptions` and, on any `ssl` or `editor.tabSize` change, in a
  `workspace/didChangeConfiguration` notification. The raw `ssl` section is
  never sent, because its key names are the extension's, not the server's.
- **Tracing and output**: `ssl.trace.server` sets the client's message tracing.
  Server output and traces go to one **SSL Language Server** log channel that
  the extension creates once and shares with every client, so a restart
  neither closes it nor adds another.
- **Restarts**: a client that fails to start is dropped, so **SSL: Restart
  Language Server** or **Retry** creates a fresh one.

The server provides completion, hover, signature help, inlay hints,
definition, references, rename, document and workspace symbols, folding,
formatting and diagnostics. It applies `ssl.diagnostics.rules` and suppression
comments itself.

## The extension's own features

| Feature | Source | Notes |
| --- | --- | --- |
| Quick fixes and universal actions | `sslCodeActionProvider.ts` | Keyed on the rule slugs in `constants/diagnosticSlugs.ts`, which `scripts/generate-diagnostic-slugs.mjs` generates from the bundled server's tag. |
| CodeLens reference counts | `sslCodeLensProvider.ts` | Counts direct references and `DoProc`/`ExecFunction` string targets in the open document. |
| Document highlights | `sslDocumentHighlightProvider.ts` | |
| Call hierarchy | `sslCallHierarchyProvider.ts` | |
| Block closer | `sslBlockCloser.ts` | Text-event based; inserts `:ENDIF;`, `:NEXT;` and so on after Enter on an opener. |
| Comment toggling | `sslCommentController.ts`, `utils/commentToggle.ts` | |
| Format SQL commands | `commands/formatSql.ts`, `formatting/`, `parsing/sql*.ts` | Reflow one SQL string on request. Document formatting, including embedded SQL, is the server's. |
| Status bar | `sslStatusBar.ts`, `utils/statusRenderer.ts` | Shows the server's state and the version it reports. |

## Settings

`package.json` contributes the settings; `constants/config.ts` lists the same
keys and defaults. `tests/config.test.ts` fails if the two differ, if the code
reads a setting that is not contributed, or if a contributed setting is never
read. A value removed from a setting's enum gets an entry in
`utils/settingsMigration.ts`, which rewrites stored values at activation.

## Grammar

`syntaxes/ssl.tmLanguage.json` is a copy of ssl-style-guide's canonical
TextMate grammar. `scripts/sync-grammar.mjs` writes it from a given upstream
commit and records that commit in the file; `npm run check:grammar` (run in
CI) fails if the copy differs from the recorded commit.

## Tests

- **Unit** (`tests/*.test.ts`): mocha under an ESM loader, with a `vscode` mock
  (`tests/setup.ts`, `tests/helpers/mockVSCode.ts`). `tests/grammar.test.ts`
  tokenizes with vscode-textmate and checks that every keyword, function,
  class and operator the bundled server exports is highlighted.
- **Integration** (`tests/integration/`): runs in a downloaded VS Code with the
  bundled extension and the real server.
- **Corpus** (`npm run test:corpus`, opt-in): see
  [CORPUS_TESTING.md](CORPUS_TESTING.md).

## Packaging

`scripts/build.mjs` bundles `src/extension.ts` with esbuild into
`dist/extension.js`. `scripts/package-platforms.mjs` builds one `.vsix` per
platform with only that platform's server binary, plus a universal package
with all of them for targets without their own build. `scripts/smoke-vsix.mjs`
installs a package into a clean VS Code profile, opens an SSL file, and fails
if the extension host logs an activation failure.
