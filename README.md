# STARLIMS Scripting Language (SSL) Extension for VS Code

![CI](https://github.com/mahoskye/vs-code-ssl-formatter/workflows/CI/badge.svg)
[![Version](https://img.shields.io/visual-studio-marketplace/v/mahoskye.vs-code-ssl-formatter.svg)](https://marketplace.visualstudio.com/items?itemName=mahoskye.vs-code-ssl-formatter)
[![Installs](https://img.shields.io/visual-studio-marketplace/i/mahoskye.vs-code-ssl-formatter.svg)](https://marketplace.visualstudio.com/items?itemName=mahoskye.vs-code-ssl-formatter)

A comprehensive Visual Studio Code extension that brings first-class language tooling—syntax highlighting, formatting, IntelliSense, diagnostics, and navigation—to STARLIMS Scripting Language (SSL).

## Overview

- Built-in formatter keeps SSL code aligned with internal STARLIMS style guidelines.
- IntelliSense exposes keyword, function, and snippet completions plus parameter hints.
- Linter and diagnostics catch style, safety, and maintainability issues early.
- Navigation helpers (definitions, references, call hierarchy, CodeLens) make large codebases approachable.
- Editor polish such as folding, outline view, breadcrumbs, and auto-closing improve everyday ergonomics.

See [CHANGELOG.md](CHANGELOG.md) for release history and bug-fix transparency.

## Installation

### Visual Studio Marketplace

1. Install [Visual Studio Code](https://code.visualstudio.com/).
2. Open the Extensions view (`Ctrl+Shift+X` / `Cmd+Shift+X`).
3. Search for **"STARLIMS Scripting Language"** or `mahoskye.vs-code-ssl-formatter`.
4. Click **Install** and reload VS Code when prompted.

### Manual Installation

1. Download the latest `.vsix` package from the [GitHub releases](https://github.com/mahoskye/vs-code-ssl-formatter/releases).
2. In VS Code open the command palette (`Ctrl+Shift+P` / `Cmd+Shift+P`), run `Extensions: Install from VSIX...`, and select the downloaded file.
3. Reload VS Code.

### From Source

```bash
git clone https://github.com/mahoskye/vs-code-ssl-formatter.git
cd vs-code-ssl-formatter
npm install
npm run package
code --install-extension vs-code-ssl-formatter-*.vsix
```

## Getting Started

1. Open or create a file with one of the [supported extensions](#supported-file-types).
2. Run `Format Document` (`Shift+Alt+F` / `Shift+Option+F`) to normalize indentation, spacing, and keyword casing.
3. Use IntelliSense features (`Ctrl+Space`, signature help, hover) to explore built-in keywords and procedures.
4. Fix diagnostics via the Problems panel or use `Ctrl+.` to apply quick fixes.
5. Customize behavior under **Settings → Extensions → SSL**.

## Feature Highlights

### Syntax & Editing Experience

- Full TextMate grammar for SSL, including keywords with colon prefixes, built-in functions, string literals (double, single, and bracket notation), SQL snippets inside strings, and both single-line and multi-line comments.
- Folding ranges for procedure blocks, loops, conditional structures, exception handling, regions, and comment blocks to simplify navigation.
- Hover documentation for keywords, language constructs, Hungarian notation hints, and inline SQL placeholder guidance (`?param?` vs positional `?`).
- Namespace-aware `Go to Definition`/hover for `DoProc`/`ExecFunction` strings when `ssl.documentNamespaces` is configured.

### Formatting & Style Guide

- `Format Document` and `Format Selection` commands with consistent indentation (tabs or spaces), operator spacing, trailing whitespace trimming, and enforced final newline.
- Keyword and built-in function casing normalization (preserve, UPPERCASE, lowercase, PascalCase) with per-feature configuration.
- Optional format-on-save, wrap length enforcement, and support for formatting only selected code.
- Inline SQL formatting (optional) reflows `SQLExecute`/`RunSQL` string literals with configurable keyword casing and clause indentation.

### IntelliSense & Navigation

- Keyword and function completion items with summaries, signature help, and snippet templates for common constructs (procedures, IF/ELSE, loops, TRY/CATCH, regions, CASE).
- Go to Definition, Peek Definition, Find All References, Rename Symbol, workspace symbol search, and document highlights with read/write distinctions.
- Call Hierarchy view to inspect incoming/outgoing procedure calls and experimental inlay hints that surface parameter names inline.
- CodeLens displays reference counts above procedures for quick usage checks.

### Diagnostics & Quick Fixes

- Live validation for block depth, procedure parameter count, Hungarian notation adherence, SQL injection risks, missing semicolons, missing OTHERWISE clauses, and other style guide rules.
- Configurable severity levels (error, warning, info) and strict style-guide mode to convert warnings to errors.
- **Per-rule overrides**: `ssl.diagnostics.rules` accepts a slug → severity map (`"off" | "info" | "warn" | "error"`). Settings UI offers slug autocomplete from the bundled LSP's published rule list (98 slugs as of v1.7.0).
- **Source-level suppression**: `/* @ssl-disable <slug>; */` silences the named rule for the rest of the file; `/* @ssl-disable-next-line <slug>; */` silences only the line that follows. `*` matches every coded diagnostic.
- **Quick fixes** keyed on stable rule slugs:
  - `udobject_array_in_clause` — extract the UDObject property to a local variable and rewrite the `IN (?...?)` placeholder.
  - `keyword_uppercase` — uppercase the flagged keyword.
  - `not_preferred_operator` — replace `<>` or `#` with `!=`.
  - `prefer_exitcase` — insert `:EXITCASE;` before the next `:CASE` / `:ENDCASE`.
  - `class_instantiation_curly` — rewrite `Foo(...)` as `Foo{...}`.
  - `dot_property_access` — replace `.` with `:` for property access.
  - `step_spacing` — insert the missing space before `:STEP`.
  - `comment_termination` — append `;` to terminate a comment.
  - `redeclare_is_noop` — drop the redundant `:DECLARE` line.
  - `equals_vs_strict_equals` — replace `=` with `==` for exact string match.
  - `parameters_first` — move `:PARAMETERS` directly after `:PROCEDURE`.
  - `default_after_parameters` — relocate `:DEFAULT` to follow `:PARAMETERS`.
  - `nested_iif` — refactor an `IIF(...)` assignment into `:IF / :ELSE / :ENDIF` (offered as a `Refactor → Rewrite` action).
  - Plus the legacy `ssl-invalid-direct-call` fix for native fallback.

### Editor Enhancements

- Outline view, breadcrumbs, bracket matching, and auto-closing pairs tuned to SSL syntax.
- Comment toggling (`Ctrl+/`) for SSL block and line comments.
- Breadcrumbs and symbol highlighting to understand scope and structure quickly.
- Region markers (`:REGION` / `:ENDREGION` or `/* region`) for manual code organization.
- **Status-bar item** (right side, when an SSL file is active) showing `SSL · LSP <version> · <fns> fns · <classes> classes` — or `SSL · native` when the language server is disabled. Click to open the SSL output channel.

## Commands

| Command | Description |
| --- | --- |
| `Format Document` | Formats the active SSL file according to configured style settings. |
| `Format Selection` | Formats only the highlighted block. |
| `Toggle Line Comment` / `Toggle Block Comment` | Adds or removes SSL-specific comment markers. |
| `Go to Definition`, `Find All References`, `Rename Symbol` | Navigation helpers for procedures, variables, and regions. |
| `Call Hierarchy: Show Incoming/Outgoing Calls` | Visualizes call relationships inside complex procedures. |

Use the Command Palette (`Ctrl+Shift+P` / `Cmd+Shift+P`) to access any command directly.

## Supported File Types

The extension activates for files ending in:

- `.ssl`
- `.srvscr`
- `.ds`
- `.ssl.txt`
- `.ds.txt`

To force SSL for other names, set `"files.associations": { "*.xyz": "ssl" }` in VS Code settings.

## Configuration

Open **Settings → Extensions → SSL** or edit `settings.json` directly. A few frequently used options:

### General

- `ssl.strictStyleGuideMode`: Elevates all style warnings to errors (default: `false`).
- `ssl.maxNumberOfProblems`: Limits diagnostics reported per file (default: `100`).
- `ssl.globals`: Array of variable names that should always be treated as declared globals (default: `[]`).

### Formatting

- `ssl.format.indentStyle`: `tab` or `space` (default: `tab`).
- `ssl.format.indentWidth`: Number of tabs/spaces per indent (default: `1`).
- `ssl.format.builtinFunctionCase`: `PascalCase` or `preserve` (default: `PascalCase`). Normalizes built-in functions to their canonical casing, or preserves user's choice.
- `ssl.format.wrapLength`: Maximum line length before wrapping (default: `90`).
- `ssl.format.operatorSpacing`: Insert spaces around operators (default: `true`).
- `ssl.format.commaSpacing`: Insert spaces after commas (default: `true`).
- `ssl.format.semicolonEnforcement`: Ensure statements end with semicolons (default: `true`).
- `ssl.format.blankLinesBetweenProcs`: Blank lines between procedures (default: `1`).
- `ssl.format.formatOnSave`: Automatically format on save (default: `false`).
- `ssl.format.trimTrailingWhitespace`: Remove trailing whitespace (default: `true`).
- `ssl.format.sql.enabled`: Format inline SQL string literals (default: `true`).
- `ssl.format.sql.keywordCase`: `preserve`, `upper`, or `lower` for SQL keywords (default: `upper`).
- `ssl.format.sql.indentSpaces`: Spaces to indent formatted SQL clauses (default: `4`).
- `ssl.format.sql.style`: SQL layout style. Options: `standard`, `canonicalCompact`, `compact`, `expanded` (default: `standard`). See `sql-formats.md` for examples of each style, and change the default in your user/workspace settings.

**Note:** SSL keywords (`:IF`, `:WHILE`, `:PROCEDURE`, etc.) are always normalized to UPPERCASE per the SSL style guide. This is not configurable.

### Naming & Style

- `ssl.naming.hungarianNotation.enabled`: Enforce Hungarian notation (default: `true`).
- `ssl.naming.hungarianNotation.severity`: `warn`, `error`, or `info` (default: `warn`).
- `ssl.naming.hungarianNotation.prefixes`: Allowed Hungarian prefixes (default: `s, n, b, d, a, o, u`).
- `ssl.styleGuide.limitBlockDepth`: Maximum nested block depth (default: `4`, `0` disables).

- `ssl.styleGuide.maxParamsPerProcedure`: Limit on procedure parameters (default: `8`, `0` disables).

### Security & Diagnostics

- `ssl.security.preventSqlInjection`: Warn about concatenated SQL queries (default: `true`).
- `ssl.security.requireParameterizedQueries`: Require `?PARAM?` placeholders (default: `true`).
- Placeholder enforcement: SQL helpers that expect positional arrays (`RunSQL`, `GetDataSet`, etc.) warn if your query uses named `?param?` tokens, while `SQLExecute` warns if you try to pass positional `?` markers.

### IntelliSense

- `ssl.intellisense.enabled`: Master toggle for IntelliSense (default: `true`).
- `ssl.intellisense.codeLens.enabled`: Show reference counts above procedures (default: `true`).
- `ssl.intellisense.signatureHelp.enabled`: Show parameter hints while typing (default: `true`).
- `ssl.intellisense.inlayHints.enabled`: Enable experimental inline parameter hints (default: `false`).
- `ssl.intellisense.inlayHints.parameterNames`: Show parameter names inline (default: `false`).
- `ssl.intellisense.completion.enableKeywords` / `.enableBuiltinFunctions` / `.enableBuiltinClasses` / `.enableSnippets`: Toggle individual completion categories (default: `true`).
- `ssl.intellisense.customFunctions`: Add or override built-in functions for completion, hover details, diagnostics, and formatting (default: `[]`). Each entry supports `name`, `description`, `params`, `signature`, `returnType`, `category`, and `untypedSignature`.
- `ssl.intellisense.customClasses`: Add or override built-in classes for completion and member lists (default: `[]`). Each entry supports `name`, `description`, `instantiation`, `usage`, `methods`, and `properties`.

### Navigation

- `ssl.documentNamespaces`: Object mapping namespace prefixes (as they appear in `ExecFunction`/`DoProc` strings) to workspace-relative folders. Example:
  ```jsonc
  {
    "ssl.documentNamespaces": {
      "Services": "server/services",
      "API": "src/api"
    }
  }
  ```
  With this mapping, strings like `ExecFunction("Services.Orders.ProcessOrder")` resolve directly to `server/services/Orders.ssl`.
  Use the command palette entry **“SSL: Configure Document Namespaces”** to interactively add aliases without hand-editing `.vscode/settings.json`.

### Example `settings.json`

```jsonc
{
  "ssl.strictStyleGuideMode": false,
  "ssl.format.formatOnSave": true,
  "ssl.format.builtinFunctionCase": "PascalCase",
  "ssl.naming.hungarianNotation.enabled": true,
  "ssl.naming.hungarianNotation.prefixes": ["s", "n", "b", "d", "a", "o", "u"],
  "ssl.styleGuide.limitBlockDepth": 4,
  "ssl.security.preventSqlInjection": true,
  "ssl.globals": ["sUserName"],
  "ssl.intellisense.customFunctions": [
    { "name": "MyHelper", "description": "Project helper", "params": "(value)" }
  ],
  "ssl.intellisense.customClasses": [
    { "name": "ApiClient", "methods": ["Get", "Post"], "properties": ["BaseUrl"] }
  ]
}
```

## Documentation & Support

- User documentation: this README.
- Developer documentation: [docs/README.md](docs/README.md) with architecture, CI, and grammar details.
- Release notes and bug history: [CHANGELOG.md](CHANGELOG.md).
- SQL placeholder reference: [docs/sql-parameters.md](docs/sql-parameters.md).
- Issues & feature requests: [GitHub Issues](https://github.com/mahoskye/vs-code-ssl-formatter/issues).

Contributions are welcome—see [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) and follow the testing instructions before opening a pull request.
