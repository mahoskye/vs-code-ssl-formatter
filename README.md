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
- Quick fixes for missing semicolons, keyword casing, built-in function casing, missing OTHERWISE clauses, and comment toggling.

### Editor Enhancements

- Outline view, breadcrumbs, bracket matching, and auto-closing pairs tuned to SSL syntax.
- Comment toggling (`Ctrl+/`) for SSL block and line comments.
- Breadcrumbs and symbol highlighting to understand scope and structure quickly.
- Region markers (`:REGION` / `:ENDREGION` or `/* region`) for manual code organization.

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
- `ssl.format.formatOnSave`: Automatically format on save (default: `false`).
- `ssl.format.trimTrailingWhitespace`: Remove trailing whitespace (default: `true`).
- `ssl.format.sql.enabled`: Format inline SQL string literals (default: `false`).
- `ssl.format.sql.keywordCase`: `preserve`, `upper`, or `lower` for SQL keywords (default: `upper`).
- `ssl.format.sql.indentSpaces`: Spaces to indent formatted SQL clauses (default: `4`).

**Note:** SSL keywords (`:IF`, `:WHILE`, `:PROCEDURE`, etc.) are always normalized to UPPERCASE per the SSL style guide. This is not configurable.

### Naming & Style

- `ssl.naming.hungarianNotation.enabled`: Enforce Hungarian notation (default: `true`).
- `ssl.naming.hungarianNotation.severity`: `warn`, `error`, or `info` (default: `warn`).
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
  "ssl.styleGuide.limitBlockDepth": 4,
  "ssl.security.preventSqlInjection": true
}
```

## Documentation & Support

- User documentation: this README.
- Developer documentation: [docs/README.md](docs/README.md) with architecture, CI, and grammar details.
- Release notes and bug history: [CHANGELOG.md](CHANGELOG.md).
- SQL placeholder reference: [docs/sql-parameters.md](docs/sql-parameters.md).
- Issues & feature requests: [GitHub Issues](https://github.com/mahoskye/vs-code-ssl-formatter/issues).

Contributions are welcome—see [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) and follow the testing instructions before opening a pull request.
