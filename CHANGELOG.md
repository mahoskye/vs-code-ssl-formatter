# Change Log

All notable changes to the "STARLIMS Scripting Language" extension will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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

**[Unreleased]**: https://github.com/mahoskye/vs-code-ssl-formatter/compare/v1.0.0...HEAD
**[1.0.0]**: https://github.com/mahoskye/vs-code-ssl-formatter/releases/tag/v1.0.0
