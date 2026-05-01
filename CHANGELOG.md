# Change Log

All notable changes to the "STARLIMS Scripting Language" extension will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.10.2] - 2026-05-01

### Fixed
- **LSP startup deadlock on Windows.** Bundled `starlims-lsp` v0.7.2 fixes a
  pre-existing self-deadlock in the `initialized` notification handler. The
  handler synchronously called `client/registerCapability` to register
  file watchers — but glsp dispatches notifications on the same goroutine
  that reads incoming messages, so the server was blocked waiting for the
  client's response on the same goroutine that would have to deliver it.
  Manifested as `fatal error: all goroutines are asleep - deadlock!` and
  `Cannot call write after a stream was destroyed` on the client side. The
  registration now runs in a background goroutine. The bug had always been
  theoretically broken; v1.10.0 evidently shifted the Windows timing
  enough to make it deterministic.

## [1.10.1] - 2026-05-01

### Fixed
- **LSP server no longer dies on internal panics.** A v1.10.0 Windows user
  reported `fatal error: all goroutines are asleep - deadlock` after
  upgrading; the symptom matched a panic in a diagnostic check function
  killing the LSP process. Bundled `starlims-lsp` v0.7.1 wraps the
  diagnostic-collection path in `recover()` so any future panic surfaces
  as a single `internal_error` diagnostic plus a stack trace in the LSP
  output channel — the editor stays usable and bug reports include
  actionable detail.
- **Statically link the Linux amd64 server binary.** The host build was
  linking against the build runner's glibc; cross-compiled targets were
  already static. All targets now build with `CGO_ENABLED=0` so users on
  systems with older glibc no longer hit silent startup failures.

## [1.10.0] - 2026-05-01

### Added
- **`procedure_declaration_syntax` rule.** Bundled `starlims-lsp` v0.7.0 ships
  a new error-level diagnostic that catches two common procedure-declaration
  typos:
  - `PROCEDURE Name(...)` (missing the leading colon) — previously misfired
    `direct_procedure_call` on the procedure name. The new rule explains the
    actual problem: definitions are `:PROCEDURE Name;` (colon prefix, trailing
    semicolon, no parens; arguments via `:PARAMETERS`).
  - `:PROCEDURE Name(...)` — parens after a valid keyword. Same fix, same
    message.
  Slug `procedure_declaration_syntax` is now in `ssl.diagnostics.rules`.

### Changed
- **`mixed_type_operator` no longer false-fires** on three common patterns
  that the v0.6.0 inferencer mishandled:
  - **Uppercase-leading identifiers.** `DCUparseCat` (capital `D` is the
    start of an acronym) was being read as a `d`-prefixed Hungarian date.
    Type inference now requires a strict-case lowercase prefix before an
    uppercase rune; the lenient match still drives the `hungarian_notation`
    enforcer rule for `Sname`-style mistakes.
  - **Indexed access (`arr[i]`).** Was typed as the array, not the element.
    Now treated as opaque element type — no warning.
  - **Member access (`Me:Foo`, `obj:bar`).** Same treatment — opaque.
  Regression-guard: `"abc" + 5` still emits the warning, so the rule isn't
  silenced for genuine literal mismatches.
- **`class_member_order` no longer enforces Constructor position.** Real
  legacy classes routinely place the constructor first; the rule now only
  enforces `:INHERIT` < `:DECLARE` < methods.

### Removed
- **`skipped_param_spacing` rule.** Pure stylistic noise about whether
  `{a, , b}` should be `{a,,b}`. Removed in `starlims-lsp` v0.7.0; the slug
  is also gone from `ssl.diagnostics.rules`.

### Internal
- `npm run smoke` fixture updated to valid SSL (`:PROCEDURE Smoke; …
  :ENDPROC;`); the old fixture would now (correctly) trip the new
  `procedure_declaration_syntax` rule and confuse the smoke check.
- Bundled LSP binaries refreshed to v0.7.0.

## [1.9.1] - 2026-05-01

### Fixed
- **Packaged VSIX missing `vscode-languageclient`.** `.vscodeignore` had a
  blanket `node_modules/**` entry which excluded all runtime dependencies
  from the published `.vsix`. On a fresh install this surfaced as
  `Cannot find module 'vscode-languageclient/node'` from `lspClient.js`,
  preventing the extension from activating. Removed the blanket ignore;
  `vsce` now bundles production dependencies (and only those) as intended.

## [1.9.0] - 2026-04-30

### Added
- **Universal suppression code actions.** Every LSP-emitted diagnostic with
  a known rule slug now offers two extra lightbulb entries:
  - *"Suppress 'X' on this line"* — inserts `/* @ssl-disable-next-line X; */`
    on the line above, indented to match.
  - *"Suppress 'X' for this file"* — inserts `/* @ssl-disable X; */` at the
    top of the document.
  The LSP (v0.5.0+) honors both directives natively, so the diagnostic
  disappears on the next publishDiagnostics round. Legacy native codes
  (`ssl-*` kebab) skip these actions because the directives don't apply
  to them.
- **"Show docs for 'X'" code action.** When a diagnostic message names an
  SSL element (function, class, etc.), the lightbulb offers a jump straight
  to the corresponding ssl-docs page. The category is inferred from the
  rule slug; if the inference can't be made the action is omitted rather
  than shipping a broken link.

### Tests
- 4 new code-action unit tests covering line-scope suppress, file-scope
  suppress, the legacy-code skip path, and the docs-link URL shape.

## [1.8.0] - 2026-04-30

### Added
- **starlims-lsp bumped to v0.6.0.** Hover for built-in functions and classes
  now shows the documented exception triggers + canonical exception messages,
  caveats, and "Don't" anti-pattern lists straight from `ssl-docs`. Hovering
  `ExecFunction`, for example, surfaces *"Please provide at least one
  parameter for ExecFunction"* and *"Wrong parameters for {functionName}"*
  inline with the signature. The metadata covers 263 elements with
  exceptions, 343 with caveats, and 397 with do/don't lists.

This change requires no extension-side wiring — the LSP serves the enriched
hover content directly. The bundled `--export-signatures` payload also
carries the new fields if you want to consume them via tooling.

## [1.7.0] - 2026-04-30

### Added
- **Nine new quick-fix code actions** keyed on stable LSP rule slugs:
  `class_instantiation_curly`, `dot_property_access`, `step_spacing`,
  `comment_termination`, `redeclare_is_noop`, `equals_vs_strict_equals`,
  `parameters_first`, `default_after_parameters`, and `nested_iif`. The
  last one is offered as a `Refactor → Rewrite` action; the rest are
  `QuickFix`-kind. Combined with the four shipped in 1.6.0, the lightbulb
  now offers fixes for fourteen distinct LSP-emitted rules.
- **`ssl.diagnostics.rules` autocomplete.** The settings UI now lists the
  98 known rule slugs from the bundled LSP as `propertyNames.enum`, so
  users get IntelliSense on rule names when configuring per-rule severity
  overrides.
- New `npm run generate:slugs` script (sources from
  `../starlims-lsp/internal/providers/diagnostic_codes.go`) generates
  `src/constants/diagnosticSlugs.ts` and patches `package.json`'s
  `propertyNames.enum`. Run after bumping the bundled LSP.

### Changed
- **README** Feature Highlights now documents quick fixes (with a slug
  table), per-rule overrides, source-level `@ssl-disable` suppression
  comments, and the status-bar item.
- The status-bar renderer (`src/utils/statusRenderer.ts`) was extracted
  from `sslStatusBar.ts` so it can be unit-tested without resolving
  `vscode-languageclient` through the ts-node loader chain.
- The diagnostic-rule-overrides middleware moved to
  `src/utils/ruleOverrides.ts` for the same reason. `lspClient.ts` now
  imports it.

### Removed
- `INTEGRATION_STATUS.md` — planning artifact from the 1.4.0 LSP
  integration push, superseded by `CHANGELOG.md` and
  `docs/ARCHITECTURE.md`.

### Tests
- 9 new code-action unit tests covering each new handler.
- 5 new `applyRuleOverrides` middleware tests covering pass-through,
  drop-on-`off`, severity remap for `info`/`warn`/`error`, unknown
  override values, and codeless diagnostics.
- 4 new status-bar renderer tests covering all four state combinations.
- 1 new end-to-end integration test exercising LSP → middleware →
  code-action chain via `udobject_array_in_clause`.

## [1.6.0] - 2026-04-30

### Added
- **starlims-lsp bumped to v0.5.0.** Adds server-side support for the new
  `ssl.diagnostics.rules` setting, source-level suppression comments, and
  three formatter options that were previously fallback-only on the client:
  `ssl.format.trimTrailingWhitespace`, `ssl.format.maxConsecutiveBlankLines`,
  and `ssl.format.builtinFunctionCase`.
- **Suppression comments** (LSP-side, surfaced through the extension):
  `/* @ssl-disable <rule>; */` (file-scope) and
  `/* @ssl-disable-next-line <rule>; */` (line-scope) silence matching
  diagnostics. Comma-separated rule lists and the `*` wildcard are accepted.
- **Quick-fix code actions for LSP-emitted diagnostics.** Keyed on the stable rule slugs introduced by `starlims-lsp` v0.4.0:
  - `udobject_array_in_clause` — *Extract '<expr>' to local '<aLocal>' before SQL call*: inserts `:DECLARE aLocal;` and `aLocal := <expr>;` and rewrites the SQL placeholder.
  - `keyword_uppercase` — *Uppercase keyword: <KW>*.
  - `not_preferred_operator` — *Replace '<>' with '!='* (or `#` → `!=`).
  - `prefer_exitcase` — *Insert ':EXITCASE;' before next :CASE/:ENDCASE*.
- **`ssl.diagnostics.rules` setting.** Object map of rule slug → `"off" | "info" | "warn" | "error"`. Diagnostics matching a rule set to `off` are dropped on the client; other values remap severity. Forwarded to the LSP via `initializationOptions` for future server-side support, with client-side filtering as the immediate enforcement path.
- **Status-bar indicator.** When an SSL document is the active editor, a status-bar item displays `SSL · LSP <version> · <fns> fns · <classes> classes` (or `SSL · native` when running without the LSP). Click opens the SSL output channel.

### Changed
- The diagnostic middleware now applies `ssl.diagnostics.rules` overrides to every batch of LSP diagnostics before VS Code surfaces them.

## [1.5.1] - 2026-04-30

### Changed
- **starlims-lsp bumped to v0.4.0.** The LSP now populates the LSP `Diagnostic.code` field on every emitted diagnostic with a stable, machine-readable slug (e.g. `parameters_first`, `prefer_exitcase`, `udobject_array_in_clause`, `exitfor_in_finally`). Slugs match `ssl-style-guide.schema.yaml` `lints` rule names where the schema defines them; parser/lexer-level findings carry slugs derived from the producing check. This unblocks future client-side quick-fix code actions, suppression comments, and per-rule severity overrides keyed on `diagnostic.code`.
- **`InitializeResult.serverInfo.version`** now reports the real build version (was previously hardcoded to "0.2.0" regardless of the actual binary).

## [1.5.0] - 2026-04-30

### Added
- **starlims-lsp v0.3.0**: Refreshed bundled language-server binaries (Linux/macOS/Windows, amd64/arm64) from `v0.2.0` to `v0.3.0`. The previous bundle was a `v0.2.0-19-gd23fca8-dirty` development build dated 2026-04-06.
- **Class hover**: hovering a built-in class now lists its constructors, properties, and methods (LSP-provided).
- **Type and special-form hover**: the 8 SSL value types (`array`, `boolean`, `codeblock`, `date`, `netobject`, `number`, `object`, `string`) and the 6 special forms (`access-modifiers`, `base`, `code-block`, `code-organization`, `constructor`, `me`) now show inline reference content (LSP-provided).
- **Constructor signature help**: cursor inside `<ClassName>{...}` now triggers signature help for each published constructor form.
- **Context-aware completions**:
  - `<BuiltInClass>{` — constructor signatures
  - `Me:` / `Base:` inside a `:CLASS Foo;` file — `Foo`'s methods and properties
  - `<BuiltInClass>:` — that class's methods and properties
- **New diagnostic — UDObject array in IN-clause**: warns when a UDObject array property is used inside a SQL `IN (?obj:Prop?)` placeholder, which causes the runtime error *"The current array has more than 1 dimmension."*. Fix: copy the array to a local variable first. Emitted by the LSP.
- **New diagnostic — class-name collision**: warns when `:CLASS Foo;` declares a class whose name shadows a built-in (e.g. `Email`, `SQLConnection`).
- **Class inventory (LSP)**: exposes 7 classes that were previously hidden — `CDataColumn`, `CDataColumns`, `CDataField`, `CDataRow`, `SQLConnection`, `SSLError`, `SSLSQLError`.
- **Native fallback inventory now sourced from the bundled LSP.** On activation, the extension runs `<bundled-lsp> --export-signatures` once and feeds the result (330 functions, 29 classes, 38 keywords) to the native completion, hover, signature help, inlay hints, and diagnostic providers. When the LSP is enabled this is invisible — the LSP itself serves user-facing features. When `ssl.languageServer.enabled` is `false` (or the LSP fails to launch), the native providers see the same authoritative catalog instead of the small hardcoded subset. If the export call fails (binary missing, spawn error, malformed output), providers transparently fall back to the hardcoded subset in `src/constants/language.ts`.

### Changed
- **Diagnostic codes (native fallback only)** aligned to `ssl-style-guide.schema.yaml` rule slugs where an equivalent exists. The bundled LSP does not populate the LSP `code` field on its diagnostics, so this rename only affects native-emitted diagnostics:
  | Old | New |
  | --- | --- |
  | `ssl-block-depth` | `ssl-max-block-depth` |
  | `ssl-max-params` | `ssl-max-params-warning` |
  | `ssl-keyword-case` | `ssl-keyword-uppercase` |
  | `sql-sql-injection` | `ssl-sql-injection` |
  Other extension diagnostic codes have no schema equivalent and are unchanged.
- **Setting descriptions** clarified for client-only formatter options that have no LSP equivalent (`ssl.format.builtinFunctionCase`, `ssl.format.formatOnSave`, `ssl.format.trimTrailingWhitespace`, `ssl.format.maxConsecutiveBlankLines`, `ssl.format.sql.concatOperator`) — these apply only when `ssl.languageServer.enabled` is `false`.

### Removed (LSP behavior)
- The LSP's built-in function inventory shrank from 354 → 330. Calls to these legacy / licensing helpers now show "unknown function" diagnostics: `LPrint`, `TraceOn`, `TraceOff`, `SqlTraceOn`, `SqlTraceOff`, `StationName`, `UndeclaredVars`, `In64BitMode`, `NetFrameworkVersion`, `GetExecutionTrace`, `SetLocationOracle`, `SetLocationSQLServer`, `GetForbiddenAppIDs`, `GetForbiddenDesignerAppIDs`, `IsFeatureAuthorized`, `IsFeatureBasedLicense`, `IsDemoLicense`, `GetLicenseInfoAsText`, `ResetFeatures`, `GetInstallationKey`, `GetFeaturesAndNumbers`, `GetNumberOfInstrumentConnections`, `GetNumberOfNamedConcurrentUsers`, `GetNumberOfNamedUsers`. If your codebase still calls any of these, treat them as project-local declarations.

### Documentation
- `docs/ssl-sqlexecute-parameter-substitution.md`: added caveat block about UDObject array properties in `IN` clauses (this caveat tracks the same condition the LSP now flags).
- `docs/feature-roadmap.md`: added "Upstream Proposals" section listing client-only formatter settings to migrate to the LSP.

## [1.4.0] - 2026-03-28

### Added
- **LSP Integration**: Embedded the `starlims-lsp` language server (v0.2.0) with cross-platform binaries for Linux, macOS, and Windows (amd64/arm64).
  - When enabled (default), the LSP provides: completion, hover, definition, references, document symbols, folding ranges, signature help, formatting, diagnostics, rename, inlay hints, and workspace symbols.
  - Native TypeScript providers serve as automatic fallback when the LSP is disabled or fails to start.
  - New setting `ssl.languageServer.enabled` to toggle LSP on/off.
  - New setting `ssl.trace.server` to trace LSP communication for debugging.
  - New command `SSL: Restart Language Server` for troubleshooting.
- **LSP Configuration Sync**: Extension settings for formatting, diagnostics, and inlay hints are automatically synced to the LSP server via `initializationOptions` and `workspace/didChangeConfiguration`.
- **Integration Tests**: Added LSP integration tests (rename, inlay hints, workspace symbols, formatting, definitions, symbols, config sync) and fallback integration tests for native provider mode.

### Changed
- Rename provider, inlay hints provider, and workspace symbol provider are now delegated to the LSP when active (previously always used native providers).

## [1.3.3] - 2026-01-09

### Fixed
- Fixed CodeLens "0 references" issue for procedures called via `DoProc` and `ExecFunction` by correctly scanning string literals.
- **Issue #52**: Fixed comment terminator detection - semicolons inside comments now correctly trigger errors when followed by code.
- **Issue #53**: Fixed built-in function calls being incorrectly marked as undefined variables by:
  - Renaming diagnostic code from `ssl-undefined-variable` to `ssl-undeclared-variable` for clarity.
  - Tracking dynamically assigned variables (LHS of `:=`) as defined even without `:DECLARE`.
  - Only showing `ssl-invalid-sql-param` error for truly undefined variables in SQL parameters.

## [1.3.2] - 2025-12-09

### Fixed

#### Diagnostics & Validation
- **Missing Semicolon Fixes**: Resolved false positive `ssl-missing-semicolon` errors for:
  - Multi-line statements using assignment operators (`:=`, `+=`, etc.) as continuation.
  - Multi-line function calls where arguments start on a new line with `(`, `{`, or `,`.
  - Statements ending inside multi-line strings (e.g. `SQLExecute` calls).
- **Comment Syntax**: Fixed `Invalid syntax: '*'` diagnostic flagging lines that start with `*` inside a block comment.
- **Undefined Variable Check**: Resolved false positive `ssl-undefined-variable` errors for dot-separated operators/literals (e.g., `.AND.`, `.OR.`, `.NOT.`, `.T.`, `.F.`).
- **SQL Parameter Validation**: Updated `ssl-invalid-sql-param` diagnostic:
  - Changed severity from **Warning** to **Error**.
  - Simplified message to remove prescriptive variable naming advice.

## [1.3.1] - 2025-12-08

### Added

#### Integration Testing Framework
- **Comprehensive Integration Tests**: Added a robust integration test suite covering Diagnostics, Formatting, Hover, and Completions in a real VS Code environment.
  - `tests/integration/diagnostics.test.ts`: Validates error reporting for invalid SQL usage and ensures no false positives for `ALen`/`AEval`.
  - `tests/integration/formatting.test.ts`: Verifies indentation logic and SQL string formatting.
  - `tests/integration/hover.test.ts`: Checks tooltip documentation for built-in functions.
  - `tests/integration/completion.test.ts`: Tests procedure and keyword completion.
- **CI Integration**: Updated GitHub Actions workflow to run integration tests (using `xvfb` on Linux).

### Fixed

#### Parsing & Formatting
- **Block Indentation Bug**: Fixed a critical issue where `IF` blocks (and other keywords without colons) were not indenting correctly.
  - This was due to the Lexer misidentifying keywords like `IF` as identifiers.
  - Updated `Lexer` to check identifiers against `SSL_KEYWORDS` list to correctly classify them as `TokenType.Keyword`.
- **Inlay Hints Refresh**: Fixed an issue where inlay hints would not update immediately when moving the cursor.

#### Diagnostics & Validation
- **False Positive Diagnostics**: Resolved multiple false positive warnings:
  - `values` keyword in `SQLExecute` strings no longer flagged as invalid procedure call.
  - Multi-line `SQLExecute` strings with placeholders (`?var?`) at line ends no longer flagged.
  - Variables declared in multi-line `:DECLARE` statements are now correctly recognized.
  - `:PUBLIC` variable declarations are now fully supported.

### Changed
- **Performance**: Optimized `SSLCompletionProvider` to better handle local procedure lookups.

## [1.3.0] - 2025-12-08

### Added

#### Core Architecture
- **Tree-sitter Grammar**: Introduced a robust Tree-sitter based grammar for potentially more accurate parsing and syntax highlighting (internal).
- **SSL Style Guide**: Added comprehensive integration with the SSL Style Guide, providing stricter validation and formatting rules.

#### SQL Formatter
- **Dedicated SQL Engine**: Implemented a dedicated SQL formatting engine capable of complex indentation, subquery alignment, and handling of various SQL dialects (Oracle/SQL Server style nuances).
- **Advanced Wrapping**: Improved line wrapping for complex SQL statements, including correct handling of `VALUES` lists, `INSERT` columns, and string concatenations.

### Fixed

#### Document Formatter & Tests
- **Comment Syntax Validation**: Updated test fixtures to strictly enforce standard SSL block comment syntax (`/* ... ;`) and removed conflicting legacy test cases.
- **Test Harness Restoration**: Repaired corrupted structure in `tests/formatter.test.ts` to ensure complete coverage of comment and style guide fixtures.
- **Project Hygiene**: Resolved ESLint warnings in formatter logic.

#### Formatting Refinements
- **Comprehensive Fixes**: Verified and finalized fixes for reported regressions:
  - Array bracket parsing and alignment
  - proper parameter list wrapping
  - SQL `UPDATE` and `INSERT` statement indentation
  - Logical operator spacing matches

#### Code Quality / Refactoring
- **SQL Formatter**: Extracted helper methods for casing, preprocessing, and table.column handling.
- **Statement Printer**: Extracted `SPECIAL_FUNCTIONS` constant, `isSpecialFunctionFirstParam`, `calculateSqlIndent`, and `handleOperatorSpacing` helpers.
- **Whitespace Manager**: Extracted `normalizeKeyword` helper for repeated text normalization.
- **SQL Context**: Replaced hardcoded SQL function names with dynamic regex using `SQL_CONTEXT_FUNCTIONS` constant.
- **Parser**: Extracted `handleBlockStart`, `handleBlockEnd`, `handleBlockMiddle`, and `isStatementContinuation` helpers.
- **Lexer**: Removed dead code and improved string tokenization.
- **Formatter**: Split `visit` method into smaller visitor methods.


## [1.2.5] - 2025-12-05

### Fixed

#### Document Formatter
- **Operator Spacing**: Fixed issue where operators near string literals weren't spaced (e.g., `='VALUE'` now formats to `= 'VALUE'`)
  - Implemented masking strategy to safely format operators without affecting string contents
- **Unary Operator Spacing**: Fixed incorrect space insertion after unary `!` operator
  - `! Empty` is now correctly formatted as `!Empty`
- **Multi-line Comment Spacing**: Fixed bug where blank lines were incorrectly inserted inside multi-line comments
- **Commented Code Protection**: SQL formatting no longer attempts to format commented-out SQL code
- **String Literal Protection**: Fixed regression where `substr` was capitalized to `SubStr` inside string literals
  - enhanced multi-line string state tracking in `normalizeBuiltinFunctionCase`

## [1.2.4] - 2025-12-05

### Fixed

#### Document Formatter
- **String Concatenation Wrapping**: Fixed bug where trailing `+` operators were dropped when wrapping long concatenated strings
  - Ensures valid syntax is preserved when reformatting multi-line string concatenations

## [1.2.3] - 2025-12-05

### Changed

#### Document Formatter - SQL Handling
- **SQL Formatting Enabled by Default**: `ssl.format.sql.enabled` now defaults to `true`
- **Multi-line SQL String Support**: SQL strings spanning multiple lines are now properly captured and formatted
  - Regex now matches across newlines using `[\s\S]*?`
  - Processes entire text to handle SQL spanning multiple physical lines
- **Context-Preserving Indentation**: Continuation lines align with the opening quote position
  - Maintains visual context within SSL code structure

#### Document Formatter - Line Wrapping
- **Expanded Wrapping Patterns**: Now handles more line types:
  - Standalone function calls (e.g., `SQLExecute(...)`)
  - String concatenations with `+` operator
  - Logical expressions with `.AND.`/`.OR.` operators
- **Fixed Indentation Preservation**: Wrapped lines now correctly maintain original indentation
  - Helper functions return unindented lines
  - Caller applies originalIndent to all wrapped lines
  - Proper continuation alignment for function arguments

## [1.2.2] - 2025-12-05

### Fixed

#### Document Formatter
- **Multi-line SQL String Indentation**: Fixed incorrect bracket counting when multi-line SQL strings were present
  - Parentheses inside SQL strings were incorrectly counted as code, causing subsequent lines to be over-indented
  - Added `stripAllStringContent()` to properly handle partial strings that start but don't end on a line
  - Lines that end multi-line strings now correctly update bracket depth for code after the closing quote

#### SQL Formatting Styles
- **Canonical Compact Style**: Rewrote formatting to match documented examples in `sql-formats.md`
  - SELECT columns now stay on same line (wrap at 80 characters) instead of one per line
  - Added proper WHERE clause line breaks (was getting merged with ON conditions)
  - Fixed hanging operators: AND at 2-space indent, OR at 7-space alignment

## [1.2.0] - 2025-12-05

### Added

#### SQL Formatting (Issue #10)
- **Format SQL Command** with 5 customizable formatting styles:
  - `compact` - Single-line clauses with minimal line breaks
  - `expanded` - Multi-line with vertical column alignment
  - `hangingOperators` - AND/OR at line start with reduced indent
  - `knr` - K&R style with parenthesized condition blocks
  - `knrCompact` - K&R style with compact SELECT lists
- Context menu integration for quick style selection
- Configurable default style via `ssl.format.sql.style` setting

#### SQL Parameter Hover Hints (Issues #13, #15)
- Positional `?` placeholders now show parameter index and resolved value
- Named `?PARAM?` placeholders show variable value and declaration line
- Support for indexed array access like `?aCountries[2]?`
- Detects SQL function (RunSQL, LSearch, SQLExecute, etc.) and extracts parameters

#### UDObject IntelliSense (Issue #18)
- Built-in UDObject methods: `AddProperty`, `IsProperty`, `AddMethod`, `IsMethod`, `Clone`, `Serialize`, `Deserialize`
- Built-in `xmltype` property with documentation
- Scans for properties added via `AddProperty()` calls
- Scans for methods added via `AddMethod()` calls
- Proper sorting: built-in members before user-defined

#### Namespace-based File Linking (Issue #16)
- IntelliSense completions for DoProc/ExecFunction string arguments
- Current file procedures shown with priority sorting
- Workspace procedures with qualified namespace names (e.g., `Validation.ValidateInput`)

#### Project Assets
- New flask/star logo for the extension

### Fixed
- **#39**: Inlay hints no longer appear inside SQL string literals
- **#40**: Rename operations now correctly limited to procedure scope

### Changed
- ESLint configuration updated to allow underscore prefix for unused parameters

## [1.1.0] - 2025-11-19

### Added
- Command palette entry **SSL: Configure Document Namespaces** to help map namespace aliases to workspace folders without hand-editing `settings.json`.
- Diagnostic (`ssl-invalid-exec-target`) that warns when `ExecFunction` strings omit the required `Namespace.Script.Procedure` segments, preventing broken navigation strings.
- Diagnostic enforcement so SQL helpers that require positional `?` placeholders (RunSQL, LSearch, GetDataSet variants, etc.) flag accidental usage of `?param?` tokens, and vice versa for `SQLExecute`.

### 🎉 Major Bug Fix Release

This release fixes **16 critical bugs** across all major feature areas with **100% test coverage**. All fixes have been thoroughly tested and verified with 103 passing unit tests.

See [detailed release notes](docs/releases/RELEASE_NOTES_v1.1.0.md) for comprehensive information.

### Fixed

#### String Literal Protection (Critical)
- **#28**: SQL formatter incorrectly modified spacing and casing inside string literals (⚠️ data corruption risk)
  - Formatter no longer changes `AND (` to `AND(` or `substr` to `SubStr` inside SQL strings
- **#27**: SQL functions inside strings triggered hover hints for unrelated language functions
  - Hover hints now correctly excluded for content inside strings and comments
- **#24**: Syntax highlighting incorrectly applied to content inside string literals
  - Regex patterns and SQL syntax inside strings now treated as literal text
- **#21**: Parentheses inside comments were incorrectly syntax-highlighted
  - Grammar updated to prevent nested tokenization within comments

#### Formatter Architecture
- **#31**: Line breaking engine lost indentation when wrapping long lines
  - Wrapped lines now maintain block-level indentation
- **#33**: Formatter collapsed intentionally structured multi-line function calls
  - Multi-line expressions and array literals now preserve intentional structure
- **#32**: `:RETURN` not indented inside `:IF/:ENDIF` and other control blocks
  - `:RETURN` now indents correctly based on block depth

#### Essential UX
- **#14**: Language features unavailable in untitled/temporary files (⚠️ critical UX issue)
  - IntelliSense, hover, diagnostics, and all features now work before first save
- **#34**: Comment toggling (Ctrl+/) added extra semicolons and behaved inconsistently
  - SSL-aware comment toggling without duplication or syntax errors

#### Linter Accuracy
- **#36**: Procedure reference mapping triggered false positives outside valid execution contexts
  - "Find All References" now shows only actual procedure definitions and invocations
- **#26**: False "missing semicolon" warnings on multi-line function calls with array literals
  - Multi-line DoProc/ExecFunction calls no longer flagged incorrectly
- **#22**: False "undeclared variable" warnings for object property access
  - Object property access (`oUser:sValue`, `Me:sProperty`) now recognized correctly
- **#25**: Incorrect SQL parameter casing errors
  - SQL parameter validation now case-insensitive (`?SESSIONID?` matches `sessionId`)

#### Polish
- **#35**: Inlay hints inconsistently applied and disappeared unpredictably
  - Parameter hints now consistent across all visible lines
- **#37**: Hover hints displayed unnecessary "Usage Frequency: Moderate" line
  - Removed distracting frequency information for cleaner tooltips
- **#30**: `DoProc` hover hint missing required procedure name parameter in signature
  - Shows complete signature: `DoProc(string procedureName, object[] parameters)`

### Testing & Quality

- **Test Suite**: Expanded from 88 to 103 unit tests (+15 regression tests)
- **Regression Coverage**: 100% for all 25 fixed bugs (16 new + 9 previously closed)
- **Test Status**: ✅ 103/103 passing (0 failures)
- **New Test Files**: 7 new test files covering all feature areas
  - `tests/symbolProvider.test.ts` - Document symbol provider (8 tests)
  - `tests/hover.test.ts` - Hover provider string/comment exclusion (12 tests)
  - `tests/commentToggle.test.ts` - Comment toggling (5 tests)
  - `tests/diagnosticProvider.test.ts` - Diagnostic provider accuracy (10 tests)
  - `tests/referenceProvider.test.ts` - Reference provider (3 tests)
  - `tests/inlayHintsProvider.test.ts` - Inlay hints (1 test)
  - `tests/extension.test.ts` - Document selectors (1 test)

### Added

- **Mock Infrastructure**: Added `MockSymbolKind` and `MockDocumentSymbol` to test helpers
- **Document Selectors**: Canonical helper for both `file://` and `untitled://` schemes
- **Comment Controller**: Dedicated SSL-aware comment toggling command

### Documentation

- **Project Documentation**: Moved to `docs/project/` directory
  - Bug fix progress tracking
  - Testing verification reports
  - Regression test status
  - Completion summaries
- **Release Notes**: Detailed notes in `docs/releases/RELEASE_NOTES_v1.1.0.md`

### Upgrade Notes

**No Breaking Changes**: This is a pure bug fix release restoring correct behavior.

**Behavior Changes**:
1. `:RETURN` now indents inside control blocks (correct behavior)
2. String literals never modified by formatter (correct behavior)
3. Fewer false positive diagnostics
4. Language features work immediately in new files

**Recommended Actions**:
1. Re-save SSL files to apply corrected `:RETURN` indentation
2. Test untitled file workflow - IntelliSense should work immediately
3. Review disabled diagnostics - many false positives are now fixed

## [1.0.2] - 2025-11-15

### Added

- **Automatic Line Wrapping**: Comprehensive implementation of automatic line wrapping for long lines
  - Intelligently wraps lines exceeding the configured `ssl.format.wrapLength` (default: 90 characters)
  - Preserves logical structure and readability when breaking lines
  - Proper indentation for continuation lines

### Fixed

- **Comment Formatting**: Enhanced comment preservation during formatting
  - Fixed preservation of inline comments on the same line as code
  - Fixed preservation of spacing and indentation inside multi-line comments
  - Comments now maintain their position and formatting during code transformations

- **Diagnostics Improvements**: More accurate and cleaner diagnostic reporting
  - Fixed duplicate diagnostics for undeclared variables
  - Diagnostic ranges now exclude leading whitespace for clearer highlighting
  - Improved accuracy of diagnostic positioning

- **Symbol Recognition**: Better understanding of SSL language constructs
  - Fixed CLASS symbol tracking and nesting in document outline
  - CLASS 'Me' property access now properly recognized without false warnings
  - Multi-line expression continuation with logical operators (AND, OR, NOT) now correctly identified

## [1.0.1] - 2025-11-14

### Fixed

- **String Parsing**: Fixed incorrect escape sequence handling in string parsing and syntax highlighting
  - SSL does not support escape sequences - backslashes are literal characters, not escape characters
  - Removed incorrect `prevChar !== '\\'` check in `sslFormattingProvider.ts` that was treating backslashes as escape characters
  - Removed escape sequence patterns from TextMate grammar (`ssl.tmLanguage.json`) that were incorrectly highlighting `\s`, `\"`, etc. as escape sequences
  - A quote character always ends a string in SSL (e.g., `"\"` is a string containing a single backslash, followed by end-of-string)
  - String examples that now work correctly:
    - `"\"` - String containing a single backslash
    - `"\" for "/"` - Two separate strings: `"\"` and `"/"`
    - `"C:\string\"` - String containing `C:\string\` (backslashes are literal, quote ends the string)
    - `"test\"` - String containing `test\` (backslash is literal, quote ends the string)
  - Updated comments in `formatters.ts` and `sslFoldingProvider.ts` to clarify that SSL has no escape sequences

## [1.0.0] - 2025-11-14

### 🎉 Production Release

This marks the first production-ready release of the SSL VS Code extension. The extension now provides enterprise-grade language support with comprehensive testing, CI/CD automation, and cross-platform compatibility.

### 🚀 CI/CD & Infrastructure

- **GitHub Actions Workflows**: Automated testing on Ubuntu, Windows, and macOS with Node.js 18 & 20
- **CodeQL Security Scanning**: Weekly vulnerability analysis and security monitoring
- **Automated Publishing**: Streamlined releases to VS Code Marketplace
- **Package Optimization**: Reduced extension size from 524KB to 190KB (63% reduction)
- **Cross-Platform Testing**: Verified compatibility across all major operating systems

### 🧪 Testing & Quality Assurance

- **Comprehensive Test Suite**: 28 unit tests covering all major features
- **Mock VSCode API**: Isolated testing environment for reliable test execution
- **Style Guide Test Fixtures**: 21 test fixtures validating formatting rules
- **Test Coverage Documentation**: Detailed analysis of test coverage and gaps
- **Automated Test Scripts**: Scripts for format verification and comment preservation
- **Professional Logging System**: Structured logging with configurable levels (replaces console.log)

### 🎨 Enhanced Formatting

- **Operator Spacing Normalization**: Consistent spacing around all operators
- **Continuation Line Support**: Proper indentation for multi-line expressions
- **Multi-line Comment Handling**: Correct formatting within comment blocks
- **Multi-line String Support**: Improved handling of strings spanning multiple lines
- **Statement Splitting**: Automatic separation of multiple statements on one line
- **CASE/OTHERWISE Formatting**: Proper indentation for case statements
- **Configurable Tab Size**: Respects editor's tab size configuration
- **Comment Preservation**: Maintains comment positioning during formatting

### 🔍 Advanced Diagnostics

- **SQL Injection Detection**: Enhanced detection of non-parameterized queries
- **SQL Parameter Validation**: Validates that SQL parameters are declared variables
- **DoProc Parameter Checking**: Ensures correct parameter counts in procedure calls
- **Undeclared Variable Detection**: Identifies variables used before declaration
- **Scope-Aware Analysis**: Distinguishes between global and local variable scopes
- **Hungarian Notation Validation**: Enforces variable naming conventions
- **Block Depth Checking**: Warns about excessive nesting (configurable limit)
- **Parameter Count Validation**: Flags procedures with too many parameters

### 💡 IntelliSense Improvements

- **Enhanced DoProc Support**: Smart completion for procedure names in DoProc calls
- **Array Parameter Handling**: Proper signature help for array parameters
- **Inlay Hints Refresh**: Dynamic updates on cursor movement
- **Object Member Completion**: Completion after `:` for object properties
- **User-Defined Procedures**: Hover support for custom procedures
- **Built-in Classes**: Support for Email, SSLRegex, and other SSL classes
- **Extended Function Library**: Added ARRAYNEW, STR, GETLASTSSLERROR, and more

### 🏗️ Code Structure & Maintainability

- **Centralized Constants**: Organized language constants, patterns, and diagnostics
- **Modular Architecture**: Separated concerns into dedicated modules
- **Utility Functions**: Reusable formatting and validation utilities
- **Type Safety**: Improved TypeScript type definitions
- **Code Documentation**: Comprehensive inline documentation
- **Pattern Library**: Centralized regex patterns for consistent parsing

### 📚 Documentation

- **CI/CD Setup Guide**: Complete guide for GitHub Actions workflows
- **Test Suite Documentation**: Comprehensive test documentation and guides
- **Style Guide Integration**: Full integration with SSL Style Guide
- **Developer Documentation**: Architecture and contribution guidelines
- **Quick Start Guides**: Easy onboarding for new contributors

### 🐛 Bug Fixes

- **Multi-line Comment Detection**: Fixed comment boundary detection in references
- **:DEFAULT Syntax**: Corrected parameter separator syntax
- **:ENDCLASS Recognition**: Fixed class end detection
- **Import Paths**: Resolved module import issues
- **Comment Indentation**: Fixed indentation within comment blocks
- **Line Ending Compatibility**: Normalized line endings for cross-platform tests
- **Windows PATH Issues**: Fixed NODE_OPTIONS for Windows compatibility

### 🔧 Configuration

All existing configuration options remain available with improved defaults:

- `ssl.trace.server`: Enable logging output (off/messages/verbose)
- `ssl.format.*`: Comprehensive formatting options
- `ssl.naming.*`: Hungarian notation validation settings
- `ssl.styleGuide.*`: Code quality and style enforcement
- `ssl.security.*`: SQL injection prevention settings
- `ssl.intellisense.*`: IntelliSense and code intelligence features

### 📦 Package Contents

- **Extension Size**: 190KB (optimized)
- **Supported Files**: `.ssl`, `.srvscr`, `.ds`, `.ssl.txt`, `.ds.txt`
- **VS Code Version**: ^1.105.0+
- **Node.js Support**: 18.x, 20.x

---

## [0.4.0] - 2025-11-12

### Added

- **Workspace Symbol Search** (`Ctrl+T`): Search for procedures, classes, and regions across entire workspace
- **Document Highlight**: Automatic highlighting of symbol occurrences with read/write/definition distinction
- **Call Hierarchy**: Visual procedure call tree showing incoming and outgoing calls
- **Inlay Hints** (Experimental): Parameter name hints displayed inline in function calls
- **Configuration**:
  - `ssl.intellisense.inlayHints.enabled`: Enable inlay hints (default: false)
  - `ssl.intellisense.inlayHints.parameterNames`: Show parameter names (default: false)

### Changed

- Enhanced workspace-wide features for multi-file projects
- Improved symbol recognition across different file types

## [0.3.0] - 2025-11-12

### Added

- **Navigation & Code Intelligence**:
  - Go to Definition (F12) for procedures and variables
  - Find All References (Shift+F12) for symbols
  - Rename Symbol (F2) with Hungarian notation validation
  - Peek Definition (Alt+F12) for inline preview
- **Signature Help**: Parameter hints while typing function calls
- **CodeLens**: Reference counts displayed above procedures
- **Code Actions & Quick Fixes**:
  - Add missing semicolon
  - Fix keyword casing to UPPERCASE
  - Add missing :OTHERWISE clause to CASE statements
  - Lightbulb menu for quick access
- **Configuration**:
  - `ssl.intellisense.codeLens.enabled`: Toggle CodeLens display
  - `ssl.intellisense.signatureHelp.enabled`: Toggle signature help

### Changed

- Enhanced extension activation with all language features
- Improved README with navigation and quick fix documentation

## [0.2.0] - 2025-11-12

### Added

- **Enhanced TextMate Grammar**: Comprehensive SSL v11 syntax support
- **Code Formatting**: Full document and range formatting
  - Keyword casing normalization (UPPERCASE by default)
  - Built-in function casing (PascalCase by default)
  - Operator spacing normalization
  - Configurable indentation (tab/space)
  - Format on save support
- **IntelliSense Support**:
  - Code completion for keywords, functions, and snippets
  - Parameter hints for built-in functions
  - Smart code snippets (procedures, loops, error handling)
- **Hover Information**: Documentation on hover for keywords and functions
- **Document Symbols**: Outline view and breadcrumb navigation
- **Code Quality Diagnostics**:
  - Block depth checking
  - Parameter count validation
  - Hungarian notation validation
  - SQL injection prevention warnings
  - Missing semicolon detection
  - CASE statement completeness checking
- **Extended File Support**: `.srvscr`, `.ds`, `.ds.txt` extensions
- **Comprehensive Configuration**: 15+ customizable settings
- **SSL Style Guide Integration**: Based on SSL v11 style guide

### Changed

- Enhanced folding provider with better block detection
- Improved language configuration with auto-closing pairs
- Updated README with comprehensive documentation

## [0.1.0] - 2024-10-10

### Added

- Initial release
- Syntax highlighting for SSL files
- Code folding for SSL-specific constructs
- Special highlighting for folds in the minimap
- Basic language configuration

---

## Upgrade Notes

### Upgrading to 1.0.0

**Breaking Changes**: None. All existing configurations and features remain compatible.

**Recommended Actions**:
1. Review logging configuration: `ssl.trace.server` for debugging
2. Enable automated testing in your development workflow
3. Consider enabling format-on-save: `ssl.format.formatOnSave`

**New Features to Try**:
- Run tests locally: `npm run test:unit`
- Create VSIX package: `npm run package`
- View CI results on GitHub Actions

### Support

For issues, feature requests, or questions:
- [GitHub Issues](https://github.com/mahoskye/vs-code-ssl-formatter/issues)
- [GitHub Repository](https://github.com/mahoskye/vs-code-ssl-formatter)

---

**[Unreleased]**: https://github.com/mahoskye/vs-code-ssl-formatter/compare/v1.1.0...HEAD
**[1.1.0]**: https://github.com/mahoskye/vs-code-ssl-formatter/compare/v1.0.2...v1.1.0
**[1.0.2]**: https://github.com/mahoskye/vs-code-ssl-formatter/compare/v1.0.1...v1.0.2
**[1.0.1]**: https://github.com/mahoskye/vs-code-ssl-formatter/compare/v1.0.0...v1.0.1
**[1.0.0]**: https://github.com/mahoskye/vs-code-ssl-formatter/releases/tag/v1.0.0
