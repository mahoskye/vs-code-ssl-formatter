# STARLIMS Scripting Language (SSL) Extension for VS Code

![CI](https://github.com/mahoskye/vs-code-ssl-formatter/workflows/CI/badge.svg)
[![Version](https://img.shields.io/visual-studio-marketplace/v/mahoskye.vs-code-ssl-formatter.svg)](https://marketplace.visualstudio.com/items?itemName=mahoskye.vs-code-ssl-formatter)
[![Installs](https://img.shields.io/visual-studio-marketplace/i/mahoskye.vs-code-ssl-formatter.svg)](https://marketplace.visualstudio.com/items?itemName=mahoskye.vs-code-ssl-formatter)

Language support for STARLIMS Scripting Language (SSL) in VS Code: syntax
highlighting, formatting, completion, hover, navigation, diagnostics and quick
fixes.

The language features come from the bundled
[starlims-lsp](https://github.com/mahoskye/starlims-lsp) language server, which
models SSL as the [SSL reference](https://github.com/mahoskye/starlims-ssl-reference)
documents it: written for STARLIMS 11, verified on STARLIMS 12. The extension
adds editor features on top: quick fixes, CodeLens, highlights, call hierarchy,
the block closer and the Format SQL commands.

See [CHANGELOG.md](CHANGELOG.md) for release history.

## Installation

### Visual Studio Marketplace

1. Open the Extensions view (`Ctrl+Shift+X` / `Cmd+Shift+X`).
2. Search for **"STARLIMS Scripting Language"** or `mahoskye.vs-code-ssl-formatter`.
3. Click **Install**.

The Marketplace serves a package for your platform (Windows, macOS and Linux,
x64 and arm64), each carrying its own build of the language server.

### Manual installation

Download a `.vsix` from the [GitHub releases](https://github.com/mahoskye/vs-code-ssl-formatter/releases),
then run **Extensions: Install from VSIX...** from the Command Palette.

## Features

### From the language server

- **Completion, hover and signature help** for keywords, built-in functions and
  classes, and your own procedures. Hover renders the SSL reference, including
  documented exceptions, caveats and do/don't guidance.
- **Navigation**: Go to Definition (including `DoProc`/`ExecFunction` targets
  across the workspace), Find All References, Rename, document and workspace
  symbols, and folding.
- **Inlay hints** with parameter names.
- **Formatting** of the document or a selection, including SQL embedded in
  strings. The formatter writes SSL keywords in uppercase.
- **Diagnostics**: syntax errors, run-time pitfalls verified on STARLIMS 12, and
  style-guide rules, each with a stable rule slug.

### From the extension

- **Syntax highlighting** from ssl-style-guide's canonical TextMate grammar,
  including SQL placeholders (`?nId?`) and ODBC escapes inside strings.
- **Quick fixes** (`Ctrl+.`) keyed on rule slugs:
  - `direct_procedure_call`: rewrite a call to a procedure in the same file as
    `DoProc("Name", {args})`.
  - `udobject_array_in_clause`: extract the UDObject property to a local and
    rewrite the `IN (?...?)` placeholder.
  - `keyword_uppercase`: uppercase the keyword.
  - `not_preferred_operator`: replace `<>` or `#` with `!=`.
  - `prefer_exitcase`: insert `:EXITCASE;` before the next `:CASE` / `:ENDCASE`.
  - `class_instantiation_curly`: rewrite `Foo(...)` as `Foo{...}`.
  - `dot_property_access`: replace `.` with `:` for property access.
  - `step_spacing`: insert the missing space before `:STEP`.
  - `comment_termination`: append `;` to terminate a comment.
  - `redeclare_is_noop`: drop the redundant `:DECLARE` line.
  - `equals_vs_strict_equals`: replace `=` with `==` for an exact string match.
  - `parameters_first`: move `:PARAMETERS` directly after `:PROCEDURE`.
  - `default_after_parameters`: move `:DEFAULT` to follow `:PARAMETERS`.
  - `nested_iif`: rewrite an `IIF(...)` assignment as `:IF / :ELSE / :ENDIF`
    (a **Refactor → Rewrite** action).
  - `trailing_skip_commas`, `exitcase_after_return` and
    `invalid_limstypeex_comparison` have fixes too.
- **Universal actions** on any rule:
  - *Suppress '\<rule\>' on this line* inserts `/* @ssl-disable-next-line <rule>;`.
  - *Suppress '\<rule\>' for this file* inserts `/* @ssl-disable <rule>;` at the top.
  - *Show docs for '\<element\>'* opens the SSL reference page for the named
    function, class, keyword or operator.
- **CodeLens** reference counts above procedures, counting `DoProc` and
  `ExecFunction` string targets.
- **Document highlights** and **Call Hierarchy** for procedures.
- **Block closer**: press Enter after `:IF cond;`, `:WHILE`, `:FOR`,
  `:BEGINCASE`, `:TRY`, `:PROCEDURE` or `:REGION` and the matching `:ENDIF;`,
  `:ENDWHILE;`, `:NEXT;`, `:ENDCASE;`, `:ENDTRY;`, `:ENDPROC;` or `:ENDREGION;`
  is inserted below. `:CLASS` gets none: a class runs to the end of its file.
- **Comment toggling** (`Ctrl+/` / `Cmd+/`) with SSL's `/* ... ;` comments.
- **Format SQL** commands that reflow a SQL string in a chosen style.
- **Status bar** item showing `SSL · LSP <version>`, or `SSL · server stopped`
  when the language server is not running. Click it to open the output channel.

### Diagnostics in detail

- **Per-rule overrides**: `ssl.diagnostics.rules` maps a rule slug to `"off"`,
  `"hint"`, `"info"`, `"warn"` or `"error"`. The settings editor autocompletes
  the slugs from the bundled server.
- **Suppression comments**: `/* @ssl-disable <rule>;` silences a rule for the
  rest of the file, and `/* @ssl-disable-next-line <rule>;` for the next line
  only. `/* @ssl-disable *;` silences every rule, and a reason may follow the
  rule after `--`, `—` or `:`. An SSL comment ends at its first `;`, so there is
  no closing `*/`: text after the `;` would be code.
- **Info tier**: advisory observations (size reviews, idiom notes) are hidden
  unless `ssl.diagnostics.infoDiagnostics` is on or the rule is listed in
  `ssl.diagnostics.rules`.

## Commands

| Command | Description |
| --- | --- |
| **SSL: Toggle Comment** (`Ctrl+/` / `Cmd+/`) | Comments or uncomments the selected lines. |
| **SSL: Format SQL** | Reflows the selected SQL string, or the one on the cursor's line, in `ssl.format.sql.style`. Also in the editor context menu. |
| **SSL: Format SQL (Pick Style)** | The same, choosing the style first. |
| **SSL: Toggle Inlay Hints** | Turns `ssl.intellisense.inlayHints.enabled` on or off. |
| **SSL: Restart Language Server** | Restarts the server, or starts it if it is not running. |

Formatting, navigation and refactoring use VS Code's standard commands
(**Format Document**, **Go to Definition**, **Rename Symbol** and so on).

## Supported file types

`.ssl`, `.srvscr`, `.ds`, `.ssl.txt` and `.ds.txt`. For other names, set
`"files.associations": { "*.xyz": "ssl" }`.

## Settings

Open **Settings → Extensions → SSL**, or edit `settings.json`.

### Formatting

| Setting | Default | Description |
| --- | --- | --- |
| `ssl.format.indentStyle` | `tab` | `tab` or `space`. |
| `ssl.format.indentWidth` | `1` | Spaces per indent level in space mode; tab mode uses `editor.tabSize`. |
| `ssl.format.wrapLength` | `90` | Line length before wrapping, for SSL and embedded SQL. |
| `ssl.format.builtinFunctionCase` | `PascalCase` | `PascalCase` (canonical casing) or `preserve`. |
| `ssl.format.operatorSpacing` | `true` | Spaces around operators. |
| `ssl.format.commaSpacing` | `true` | A space after commas. |
| `ssl.format.semicolonEnforcement` | `true` | End statements with `;`. |
| `ssl.format.blankLinesBetweenProcs` | `1` | Blank lines between procedures. |
| `ssl.format.maxConsecutiveBlankLines` | `2` | Collapse longer runs of blank lines; `0` disables. |
| `ssl.format.trimTrailingWhitespace` | `true` | Remove trailing whitespace. |
| `ssl.format.formatOnSave` | `false` | Format SSL files on save (like `editor.formatOnSave`). |
| `ssl.format.sql.enabled` | `true` | Format SQL inside strings passed to `SQLExecute`, `RunSQL` and the like. |
| `ssl.format.sql.style` | `canonicalCompact` | `standard`, `canonicalCompact` (the style guide's) or `expanded`. |
| `ssl.format.sql.keywordCase` | `upper` | `upper`, `lower` or `preserve`. |
| `ssl.format.sql.identifierCase` | `preserve` | `preserve`, `lower` or `upper` for table, column and alias names. |
| `ssl.format.sql.indentSpaces` | `4` | Indent of SQL clauses. |

### Diagnostics

| Setting | Default | Description |
| --- | --- | --- |
| `ssl.diagnostics.rules` | `{}` | Per-rule severity overrides (see above). |
| `ssl.diagnostics.infoDiagnostics` | `false` | Show the info tier. |
| `ssl.diagnostics.unusedVariables` | `false` | Report variables that are declared but never used (hint). |
| `ssl.diagnostics.endpointPatterns` | `[]` | Path substrings that mark endpoint scripts, which get `Request` and `Response` as predeclared. |
| `ssl.diagnostics.maxProcedureCodeLines` | `80` | Code lines before `procedure_length_review` suggests a review; `0` disables. |
| `ssl.diagnostics.maxScriptCodeLines` | `400` | Code lines before `script_size_review` suggests a review; `0` disables. |
| `ssl.diagnostics.maxScriptProcedures` | `12` | Local procedures before `script_size_review` suggests a review; `0` disables. |
| `ssl.globals` | `[]` | Names always treated as declared globals. |
| `ssl.naming.hungarianNotation.enabled` | `true` | Check Hungarian prefixes. |
| `ssl.naming.hungarianNotation.checkTypes` | `true` | Warn when a prefix promises one type and the assigned value has another. |
| `ssl.naming.hungarianNotation.prefixes` | `s, n, b, d, a, o, fn, v` | Allowed prefixes. |
| `ssl.styleGuide.limitBlockDepth` | `4` | Nesting depth before `max_block_depth` (info tier) reports; `0` disables. |

### Editor

| Setting | Default | Description |
| --- | --- | --- |
| `ssl.editor.autoInsertBlockClosers` | `true` | The block closer described above. |
| `ssl.intellisense.codeLens.enabled` | `true` | CodeLens reference counts. |
| `ssl.intellisense.inlayHints.enabled` | `true` | Parameter-name inlay hints. |
| `ssl.intellisense.inlayHints.minParameterCount` | `2` | Fewest parameters a call needs to get hints. |
| `ssl.intellisense.signatureHelp.autoTrigger` | `false` | Open signature help automatically while typing a call; otherwise use `Ctrl+Shift+Space`. Takes effect after a reload. |
| `ssl.trace.server` | `off` | `messages` or `verbose` traces the language server in the **SSL Language Server** output channel and adds detail to the **SSL Extension** log. |

### Example

```jsonc
{
  "ssl.format.formatOnSave": true,
  "ssl.format.sql.style": "canonicalCompact",
  "ssl.diagnostics.rules": { "max_block_depth": "warn" },
  "ssl.globals": ["gCurrentUser"]
}
```

## Troubleshooting

If the status bar shows **SSL · server stopped**, the language server did not
start: run **SSL: Restart Language Server**, and see the **SSL Extension** output
channel for the reason. Without the server there is no completion, hover,
navigation, formatting or diagnostics.

## Contributing

See [AGENTS.md](AGENTS.md) for how the extension is put together, how to build
and test it, and how releases are made, and [docs/](docs/README.md) for the
architecture and corpus testing. Issues and feature requests go to
[GitHub Issues](https://github.com/mahoskye/vs-code-ssl-formatter/issues).
