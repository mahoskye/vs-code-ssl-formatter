# Change Log

All notable changes to the "STARLIMS Scripting Language" extension will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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
