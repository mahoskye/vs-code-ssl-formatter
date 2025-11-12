# Change Log

All notable changes to the "STARLIMS Scripting Language Formatter" extension will be documented in this file.

## [0.3.0] - 2025-11-12

### Added

-   **Navigation & Code Intelligence**:
    -   Go to Definition (F12) for procedures and variables
    -   Find All References (Shift+F12) for symbols
    -   Rename Symbol (F2) with Hungarian notation validation
    -   Peek Definition (Alt+F12) for inline preview
-   **Signature Help**: Parameter hints while typing function calls (triggered by `(` and `,`)
-   **CodeLens**: Reference counts displayed above procedures (clickable to show references)
-   **Code Actions & Quick Fixes**:
    -   Add missing semicolon
    -   Fix keyword casing to UPPERCASE
    -   Add missing :OTHERWISE clause to CASE statements
    -   Lightbulb menu for quick access to fixes
-   **Configuration**:
    -   `ssl.intellisense.codeLens.enabled`: Toggle CodeLens display
    -   `ssl.intellisense.signatureHelp.enabled`: Toggle signature help

### Changed

-   Enhanced extension activation with all language features
-   Improved README with navigation and quick fix documentation

## [0.2.0] - 2025-11-12

### Added

-   **Enhanced TextMate Grammar**: Updated syntax highlighting with comprehensive SSL v11 support
-   **Code Formatting**: Full document and range formatting with configurable style options
    -   Keyword casing normalization (UPPERCASE by default)
    -   Built-in function casing (PascalCase by default)
    -   Operator spacing normalization
    -   Configurable indentation (tab/space)
    -   Format on save support
-   **IntelliSense Support**:
    -   Code completion for keywords, functions, and snippets
    -   Parameter hints for built-in functions
    -   Smart code snippets (procedures, loops, error handling, etc.)
-   **Hover Information**: Documentation on hover for keywords and functions
-   **Document Symbols**: Outline view and breadcrumb navigation for procedures, regions, and variables
-   **Code Quality Diagnostics**:
    -   Block depth checking (max nesting level)
    -   Parameter count validation
    -   Hungarian notation validation
    -   SQL injection prevention warnings
    -   Missing semicolon detection
    -   CASE statement completeness checking
-   **Extended File Support**: Added `.srvscr`, `.ds`, and `.ds.txt` extensions
-   **Comprehensive Configuration**: 15+ settings for customizing formatting, diagnostics, and code quality
-   **SSL Style Guide Integration**: Based on comprehensive SSL v11 style guide

### Changed

-   Enhanced folding provider with better block detection
-   Improved language configuration with additional auto-closing pairs
-   Updated README with comprehensive documentation

## [0.1.0] - 2024-10-10

### Added

-   Initial release
-   Syntax highlighting for SSL files
-   Code folding for SSL-specific constructs
-   Special highlighting for folds in the minimap
