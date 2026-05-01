# STARLIMS LSP Developer Guide

This guide complements `README.md` with deeper usage, configuration, and troubleshooting details for the STARLIMS Language Server Protocol (LSP) implementation.

> **Bundled version:** `starlims-lsp` v0.7.1 (built 2026-05-01). Run `./starlims-lsp-<platform>-<arch> --version` to confirm. Source: https://github.com/mahoskye/starlims-lsp.

## Who this is for

- Editors or plugin authors integrating `starlims-lsp`
- Developers configuring formatting and diagnostics options
- Contributors validating behavior and compatibility

## Quickstart

1. Install the server (pre-built binary, Go install, or build from source).
2. Configure your editor to run `starlims-lsp --stdio` for `.ssl` files.
3. Open an SSL file and confirm features like completion or hover.

## Command-line interface

```
starlims-lsp --stdio
starlims-lsp --version
starlims-lsp --help
```

The server communicates over stdin/stdout using LSP. `--stdio` is the default.

## Capabilities at a glance

- Completion for keywords, built-ins, procedures, and variables
- Hover information for symbols
- Signature help for built-in functions
- Definition and references for procedures/variables
- Document symbols and folding ranges
- Formatting (whole document and range)
- Diagnostics for common SSL structural issues

### Workspace scope notes

Workspace symbols are limited to open documents. The server does not index the entire workspace yet.

## Editor integration

### VS Code

Use the companion extension `vs-code-ssl-formatter` and set the server binary path if needed. The extension is responsible for launching `starlims-lsp`.

### Neovim (nvim-lspconfig)

```lua
local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

if not configs.starlims_lsp then
  configs.starlims_lsp = {
    default_config = {
      cmd = { 'starlims-lsp', '--stdio' },
      filetypes = { 'ssl' },
      root_dir = function(fname)
        return lspconfig.util.find_git_ancestor(fname) or vim.fn.getcwd()
      end,
      settings = {},
    },
  }
end

lspconfig.starlims_lsp.setup{}
```

### Other editors

Any LSP-compatible editor can use the server. Configure it to:

1. Start `starlims-lsp --stdio`
2. Associate `.ssl` files with the server
3. Send `workspace/didChangeConfiguration` settings (optional)

## Configuration

Settings are provided via `workspace/didChangeConfiguration` under the `ssl` key.

### Example settings

```json
{
  "ssl": {
    "format": {
      "indentStyle": "tab",
      "indentSize": 4,
      "maxLineLength": 90,
      "operatorSpacing": true,
      "commaSpacing": true,
      "semicolonEnforcement": true,
      "blankLinesBetweenProcs": 1,
      "sql": {
        "enabled": true,
        "style": "standard",
        "keywordCase": "upper",
        "indentSize": 4,
        "maxLineLength": 90
      }
    },
    "diagnostics": {
      "hungarianNotation": false,
      "hungarianPrefixes": ["s", "n", "b", "d", "a", "o", "u"],
      "globals": []
    }

  }
}
```

### Formatting options

- `indentStyle`: `tab` or `space`
- `indentSize`: number of spaces when `indentStyle` is `space` (tab width when `tab`)
- `maxLineLength`: wrap threshold (0 disables wrapping)
- `operatorSpacing`: insert spaces around operators
- `commaSpacing`: insert spaces after commas
- `semicolonEnforcement`: ensure statements end with semicolons
- `blankLinesBetweenProcs`: blank lines between procedures

### SQL formatting options

- `enabled`: enable SQL formatting inside SSL
- `style`: `standard`, `canonicalCompact`, `compact`, `expanded`
- `keywordCase`: `upper`, `lower`, `preserve`
- `indentSize`: spaces per SQL indent level
- `maxLineLength`: wrap threshold for SQL clauses

### Diagnostics options

- `hungarianNotation`: enable Hungarian notation warnings
- `hungarianPrefixes`: allowable prefixes for warnings
- `globals`: list of variables treated as declared globals

Defaults match the STARLIMS style guide and mirror the values above.

## Formatting behavior

- Document formatting replaces the entire file with formatted output.
- Range formatting expands to whole lines to preserve statement structure.
- SQL formatting applies only when SQL is detected in SSL strings.

## Troubleshooting

- **No LSP features**: ensure the editor is launching `starlims-lsp --stdio` and filetype is `ssl`.
- **Settings not applying**: verify your editor sends `workspace/didChangeConfiguration` with a top-level `ssl` key.
- **Workspace symbols missing**: only open documents are scanned.
- **Formatting looks off**: check `indentStyle`/`indentSize` and whether SQL formatting is enabled.

## Development tips

- Run `make test` to validate lexer, parser, and provider changes.
- Use `make fmt` before submitting PRs.
- The server is built on `github.com/tliron/glsp`.
