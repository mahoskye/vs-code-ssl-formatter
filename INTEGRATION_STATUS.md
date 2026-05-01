# VS Code SSL Extension - LSP Integration Status

**Date:** 2026-03-28
**Status:** Phase 4 Complete

---

## Summary

The VS Code SSL extension now integrates with the starlims-lsp language server. When the LSP is enabled (default), core language features are provided by the LSP server. Native TypeScript providers serve as fallback when the LSP is disabled or fails to start.

---

## Recent Changes

### 1. Added LSP Toggle Configuration

New setting in `package.json`:

```json
"ssl.languageServer.enabled": {
    "type": "boolean",
    "default": true,
    "description": "Use the starlims-lsp language server for core features..."
}
```

### 2. Conditional Provider Registration

Modified `src/extension.ts` to conditionally register providers:

**When LSP is active (`ssl.languageServer.enabled: true`):**
- Native providers for these features are **skipped**:
  - Completion
  - Hover
  - Definition
  - References
  - Document Symbols
  - Folding Ranges
  - Signature Help
  - Formatting
  - Diagnostics
  - Rename
  - Inlay Hints
  - Workspace Symbols

**When LSP is disabled or fails:**
- All native providers are registered as fallback

**Features that always stay in extension (regardless of LSP):**
- Code Actions (quick fixes)
- CodeLens (reference counts)
- Call Hierarchy
- Document Highlight
- TextMate grammar (syntax highlighting)
- Commands (format SQL, configure namespaces, toggle inlay hints)

### 3. Platform Binaries (v0.2.0)

The `server/` directory contains all 5 platform binaries (rebuilt 2026-03-28):
- `starlims-lsp-linux-amd64`
- `starlims-lsp-linux-arm64`
- `starlims-lsp-darwin-amd64`
- `starlims-lsp-darwin-arm64`
- `starlims-lsp-windows-amd64.exe`

### 4. LSP Configuration Sync Improvements

- Added `ssl.format.blankLinesBetweenProcs` setting for LSP formatting.
- Hungarian notation settings now sync to LSP diagnostics.
- Configuration changes for `ssl.naming` now trigger LSP updates.

### 5. Restart Language Server Command

- Added `SSL: Restart Language Server` command for troubleshooting.
- Improved LSP startup error messaging with Output panel quick access.

### 6. LSP Test Coverage & Fallback Runner

- Added LSP-focused integration tests (definitions, symbols, folding, signature help, config updates).
- Added fallback integration runner that disables LSP via dedicated user-data/workspace settings.

### 7. Document Symbol Range Fix (starlims-lsp)

- Adjusted document symbol ranges to always contain selection ranges to satisfy VS Code validation.
- Updated Linux LSP binary in `server/` after the fix.

---

## Files Changed

| File | Change |
|------|--------|
| `package.json` | Added `ssl.languageServer.enabled` setting and restart command |
| `src/extension.ts` | Conditional provider registration based on LSP status |
| `src/lspClient.ts` | Configuration sync, restart logic, and error messaging |
| `tests/integration/lsp.test.ts` | LSP integration coverage |
| `tests/integration-fallback/fallback.test.ts` | Native fallback coverage |
| `tests/runFallbackTest.ts` | Fallback runner for LSP-disabled mode |
| `server/*` | Updated Linux binary after symbol range fix |

---

## Configuration

### LSP Settings

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| `ssl.languageServer.enabled` | boolean | `true` | Enable/disable LSP server |
| `ssl.trace.server` | string | `"off"` | Trace LSP communication (`off`, `messages`, `verbose`) |

### Formatting Settings (synced to LSP)

These settings are passed to the LSP server via `initializationOptions` and `workspace/didChangeConfiguration`:

| Setting | Maps to LSP |
|---------|-------------|
| `ssl.format.indentStyle` | `ssl.format.indentStyle` |
| `ssl.format.indentWidth` (space) / `editor.tabSize` (tab) | `ssl.format.indentSize` |
| `ssl.format.wrapLength` | `ssl.format.maxLineLength` |
| `ssl.format.operatorSpacing` | `ssl.format.operatorSpacing` |
| `ssl.format.commaSpacing` | `ssl.format.commaSpacing` |
| `ssl.format.semicolonEnforcement` | `ssl.format.semicolonEnforcement` |
| `ssl.format.blankLinesBetweenProcs` | `ssl.format.blankLinesBetweenProcs` |
| `ssl.format.sql.enabled` | `ssl.format.sql.enabled` |
| `ssl.format.sql.style` | `ssl.format.sql.style` |
| `ssl.format.sql.keywordCase` | `ssl.format.sql.keywordCase` |
| `ssl.format.sql.indentSpaces` | `ssl.format.sql.indentSize` |

### Diagnostics Settings (synced to LSP)

| Setting | Maps to LSP |
|---------|-------------|
| `ssl.naming.hungarianNotation.enabled` | `ssl.diagnostics.hungarianNotation` |
| `ssl.naming.hungarianNotation.prefixes` | `ssl.diagnostics.hungarianPrefixes` |
| `ssl.globals` | `ssl.diagnostics.globals` |

### Inlay Hint Settings (synced to LSP)

| Setting | Maps to LSP |
|---------|-------------|
| `ssl.intellisense.inlayHints.enabled` | `ssl.inlayHints.enabled` |
| `ssl.intellisense.inlayHints.parameterNames` | `ssl.inlayHints.parameterNames` |

---

## Testing

### Unit Tests
```bash
npm test
```

Current test count: 282 tests (all passing)

### Integration Tests
```bash
npm run test:integration
npm run test:integration-fallback
```

Latest run (2026-01-11): both suites passing.

### Automated LSP Integration Coverage

Integration tests now exercise LSP-backed completion, hover, definitions, signature help, symbols, folding, formatting, diagnostics, configuration updates, and the restart command. Fallback coverage runs with `ssl.languageServer.enabled: false` via `npm run test:integration-fallback`.

### Manual Testing Checklist

| Feature | How to Test | Status |
|---------|-------------|--------|
| LSP Starts | Check Output > SSL Language Server | Pending |
| Completion | Type `:` in .ssl file → keywords appear | Pending |
| Hover | Hover over `SQLExecute` → docs shown | Pending |
| Go to Definition | Ctrl+Click on procedure name | Pending |
| Find References | Right-click → Find All References | Pending |
| Document Symbols | Ctrl+Shift+O → outline appears | Pending |
| Folding | Click fold icons next to procedures | Pending |
| Signature Help | Type `(` after function name | Pending |
| Diagnostics | Type unclosed `:IF` → error appears | Pending |
| Format Document | Shift+Alt+F → proper indentation | Pending |
| SQL Formatting | Format document with SQL strings | Pending |
| Config Change | Change `ssl.format.indentStyle` → immediate effect | Pending |
| Fallback Mode | Set `ssl.languageServer.enabled: false` → native providers work | Pending |

---

## Next Steps

### High Priority
1. **Manual testing in VS Code** - verify all features work correctly with LSP
2. **Platform-specific packaging** - use `vsce package --target` to ship per-platform VSIXes (~7MB each vs ~35MB)

### Medium Priority
3. **Manual fallback verification** - confirm native providers work with LSP disabled in a real workspace
4. **Performance profiling** - compare LSP vs native provider performance

### Low Priority
5. **Range formatting** - LSP range formatting is implemented but needs manual verification

---

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    VS Code Extension                         │
├─────────────────────────────────────────────────────────────┤
│  extension.ts                                                │
│  ├── If LSP enabled → startClient()                         │
│  │   └── Skip native providers for LSP-handled features     │
│  └── If LSP disabled → register all native providers        │
├─────────────────────────────────────────────────────────────┤
│  lspClient.ts                                                │
│  ├── Platform detection (win32/darwin/linux, amd64/arm64)   │
│  ├── Binary path: server/starlims-lsp-{platform}-{arch}     │
│  ├── Spawn: starlims-lsp --stdio                            │
│  └── Configuration sync: workspace/didChangeConfiguration   │
├─────────────────────────────────────────────────────────────┤
│  Native Providers (fallback or extension-only)              │
│  ├── sslCodeActionProvider.ts    (always active)            │
│  ├── sslCodeLensProvider.ts      (always active)            │
│  ├── sslCallHierarchyProvider.ts (always active)            │
│  ├── sslDocumentHighlightProvider.ts (always active)        │
│  ├── sslRenameProvider.ts        (fallback only)            │
│  ├── sslInlayHintsProvider.ts    (fallback only)            │
│  ├── sslWorkspaceSymbolProvider.ts (fallback only)          │
│  ├── sslCompletionProvider.ts    (fallback only)            │
│  ├── sslHoverProvider.ts         (fallback only)            │
│  ├── sslDefinitionProvider.ts    (fallback only)            │
│  └── ...                                                     │
└─────────────────────────────────────────────────────────────┘
                              │
                              │ stdio (JSON-RPC)
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                    starlims-lsp Server                       │
│  server/starlims-lsp-{platform}-{arch}                      │
├─────────────────────────────────────────────────────────────┤
│  Capabilities:                                               │
│  ├── textDocument/completion                                 │
│  ├── textDocument/hover                                      │
│  ├── textDocument/definition                                 │
│  ├── textDocument/references                                 │
│  ├── textDocument/documentSymbol                             │
│  ├── textDocument/foldingRange                               │
│  ├── textDocument/signatureHelp                              │
│  ├── textDocument/formatting                                 │
│  ├── textDocument/publishDiagnostics                         │
│  ├── textDocument/rename                                     │
│  ├── textDocument/inlayHint                                  │
│  └── workspace/symbol                                        │
└─────────────────────────────────────────────────────────────┘
```

---

## Troubleshooting

### LSP Not Starting

1. Check Output panel: View → Output → SSL Language Server
2. Verify binary exists: `ls server/starlims-lsp-*`
3. Check binary permissions: `chmod +x server/starlims-lsp-*`
4. Try disabling: Set `ssl.languageServer.enabled: false`

### Duplicate Completions/Diagnostics

This shouldn't happen with the current implementation. If it does:
1. Check that `ssl.languageServer.enabled` is `true`
2. Reload the window: Ctrl+Shift+P → "Developer: Reload Window"
3. Check logs for "Using LSP for:" message

### Formatting Not Working

1. Check `ssl.format.*` settings are correctly configured
2. Verify LSP is running (check Output panel)
3. Try formatting a simple file first
4. Check for syntax errors that might prevent formatting

---

## Related Files

- **LSP Server:** `/home/maho/dev/starlims-projects/starlims-lsp/`
- **LSP integration doc:** `/home/maho/dev/starlims-projects/starlims-lsp/INTEGRATION_STATUS.md`
- **Full roadmap:** `/home/maho/dev/starlims-projects/starlims-lsp/ROADMAP.md`
