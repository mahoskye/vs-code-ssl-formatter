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
