# SSL Extension Feature Roadmap (Q3 2025)

This document tracks the remaining GitHub feature issues and outlines how we'll deliver them.

## Issue Tracker Snapshot

| # | Title | Type | Notes |
| - | - | - | - |
| 38 | Feature: Add a project logo | Branding | Improve VS Marketplace presence |
| 29 | Feature Request: Improve indentation for multi-line argument lists in function calls | Formatter | Preserve intentional alignments |
| 23 | Feature Request: Add support for global variables via configuration | Configuration | Reduce false positives |
| 19 | Feature: Add configurable minimap highlighting with region support | UX | Aid large-file navigation |
| 18 | Feature: Improve IntelliSense for `CreateUDObject()` with proper member filtering | IntelliSense | Precise completion lists |
| 17 | Research: Document which SQL functions support `?PARAMS?` vs `?` placeholder syntax | Research | Foundational reference |
| 16 | Feature: Add namespace-based file linking for `ExecFunction` and `DoProc` | Navigation | Cross-file intelligence |
| 15 | Feature: Add hover hints for named SQL parameters (`?PARAMS?`) | Hover | Build on #17 |
| 13 | Feature: Add hover hints for SQL `?` placeholders | Hover | Build on #17 |
| 10 | Feature: Add SQL formatting support with customizable styles | Formatter | Inline SQL awareness |

## Workstreams

### 1. Branding & Polish
- **Issue**: #38
- **Deliverables**: VS Code icon (SVG+PNG), marketplace hero image updates, `package.json` icon entries.
- **Validation**: Local preview (`vsce package`) and manual inspection.

### 2. Formatter Enhancements
- **Issues**: #29, #10
- **Plan**:
  - Extend formatter AST to recognize multi-line argument lists and maintain relative indentation.
  - Introduce SQL formatting subsystem that extracts SQL strings, formats via internal rules, reinserts safely.
- **Tests**: New fixtures under `tests/fixtures/style-guide`, unit coverage in `tests/formatter.test.ts`.

### 3. Configuration & IDE Experience
- **Issues**: #23, #19
- **Plan**:
  - Add `ssl.globals` configuration, update diagnostics/reference providers to whitelist configured symbols.
  - Implement region-aware minimap highlights using decorations and new setting (blocked by VS Code minimap API limits).
 - **Tests**: `tests/diagnosticProvider.test.ts`, `tests/commentToggle.test.ts` (for region markers).

### 4. IntelliSense & Hover Improvements
- **Issues**: #18, #15, #13
- **Plan**:
  - Expand metadata for `CreateUDObject()` to infer returned object's members based on arguments/config files.
  - Provide hover text for SQL placeholders, referencing research output.
- **Tests**: `tests/hover.test.ts`, `tests/inlayHintsProvider.test.ts`, new fixture files.

### 5. Navigation & Linking
- **Issue**: #16
- **Plan**:
  - Introduce namespace-aware file resolver (leveraging new `ssl.documentNamespaces` setting).
  - Update definition/reference providers to resolve `ExecFunction`/`DoProc` across workspace.
- **Tests**: `tests/referenceProvider.test.ts`, `tests/symbolProvider.test.ts`, integration scenarios.

### 6. Research Documentation
- **Issue**: #17
- **Plan**:
  - Audit STARLIMS SQL functions (docs/code references) for placeholder support.
  - Publish findings in `docs/sql-parameters.md` and summarize in README/CHANGELOG.
- **Dependency**: informs hover/formatter improvements.

## Execution Order & Status

| Order | Issue(s) | Status | Notes |
| - | - | - | - |
| 1 | #38 | ☑ Completed | Icon packaged in `media/icon.png` |
| 2 | #17 | ☑ Completed | Reference published in `docs/sql-parameters.md` (validated against `ssl_agent_instructions.md`) |
| 3 | #29, #10 | ☑ Completed | Multi-line arg indentation + SQL literal formatting |
| 4 | #23, #19 | ☐ In progress | Globals config shipped; minimap highlight blocked by VS Code API |
| 5 | #18, #15, #13 | ☑ Completed | CreateUDObject class index + SQL placeholder hovers |
| 6 | #16 | ☑ Completed | Namespace-aware ExecFunction/DoProc linking (`ssl.documentNamespaces`) |

## Validation Checklist

- [ ] `npm run lint`
- [ ] `npm run test:unit`
- [ ] Update README/CHANGELOG per feature set
- [ ] Run formatting regression suite after formatter changes

## Notes

- Keep CHANGELOG entries grouped by workstream for the next release.
- Consider bundling related features into minor releases (e.g., Formatter v1.2, IntelliSense v1.3).
- Track manual verification steps in future PR descriptions to maintain history without reintroducing removed docs.

## Upstream Proposals (starlims-lsp)

### Settings that have no LSP equivalent

These extension settings currently apply only to the native fallback path
(when `ssl.languageServer.enabled` is `false`). Propose upstream so the LSP
can own the behavior end-to-end:

- `ssl.format.builtinFunctionCase` (`preserve` | `PascalCase`) — normalize
  built-in function name casing during format.
- `ssl.format.formatOnSave` — superseded by `editor.formatOnSave` when LSP is
  active; consider removing from the extension once everyone is on LSP.
- `ssl.format.trimTrailingWhitespace` — LSP already trims internally; expose a
  setting to opt out if needed.
- `ssl.format.maxConsecutiveBlankLines` — collapse runs of blank lines.
- `ssl.format.sql.concatOperator` (`||` | `+`) — choose Oracle/Postgres vs SQL
  Server concatenation when wrapping long string literals.

### Diagnostic protocol gaps

- **Add a `Code` field to the LSP `Diagnostic` struct.** Currently the LSP
  emits diagnostics with only `Range`, `Severity`, `Message`, and
  `Source: "ssl-lsp"`. Without a stable code, clients cannot reliably wire
  quick-fix code actions, suppression comments, or per-rule severity
  overrides — they would have to match on message text, which is brittle.
  When added, prefer slugs from `ssl-style-guide.schema.yaml` `lints` (e.g.
  `parameters_first`, `prefer_exitcase`, `udobject_array_in_clause`).
- **Honor documented exceptions per element.** The `ssl-element-reference`
  and the style guide call out element-specific exceptions and edge cases
  (e.g. functions that *do* accept named SQL placeholders, keywords whose
  context rules differ inside data sources, classes whose members bypass
  underscore-private convention). Diagnostic checks should consult those
  exception lists before flagging — currently several rules are applied
  uniformly. Action: enumerate the exception fields already present in
  `ssl-element-reference.json` and route them into the relevant diagnostic
  checks.

## CI / build hygiene

- **GitHub Actions Node.js 24 migration.** The publish workflow logs
  warnings that `actions/checkout@v4` and `actions/setup-node@v4` (and
  any other v4 actions in `.github/workflows/*.yml`) run on Node.js 20,
  which is being deprecated. Timeline:
  - **2026-06-02** — Node 24 becomes the default runner runtime; v4
    actions still work but emit warnings.
  - **2026-09-16** — Node.js 20 is removed entirely from runners. Any
    action that hasn't shipped a Node 24-compatible release will break.
  Action: bump every `uses: actions/...@v4` to a Node 24-supporting
  version by Sep 2026. Until then, optionally set
  `FORCE_JAVASCRIPT_ACTIONS_TO_NODE24=true` to opt into Node 24 early
  and surface compatibility issues now.
