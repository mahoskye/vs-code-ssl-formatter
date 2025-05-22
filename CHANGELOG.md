# Change Log

All notable changes to the "STARLIMS Scripting Language Formatter" extension will be documented in this file.

## [0.4.0] - 2025-05-21

### Added

-   Full language server capabilities with proper parser implementation
-   Error detection and diagnostics based on SSL EBNF grammar
-   Semantic highlighting for improved code visualization
-   Dedicated tokenizer for SSL language features
-   Comprehensive comment handling support
-   Parser validation against formal SSL grammar specification
-   Abstract Syntax Tree (AST) generation for SSL code
-   Array access validation (supporting both arr[1,2] and arr[1][2] notations)
-   Property and method access validation (object:property syntax)
-   Intelligent error recovery during parsing
-   Support for all SSL literals (string, number, boolean, nil, array, date)
-   Proper validation of SSL-specific language constructs

### Changed

-   Improved token-based syntax highlighting
-   Better error reporting with precise location information
-   Enhanced handling of control structures
-   Updated extension UI with improved diagnostic capabilities

## [0.3.0] - 2025-05-21

### Added

-   Enhanced syntax highlighting based on complete EBNF grammar
-   Support for scientific notation in number literals
-   Highlighting for logical operators (.AND., .OR., .NOT.)
-   Specialized highlighting for different function types (DB, special functions)
-   Property access highlighting with object:property syntax
-   SQL parameter highlighting
-   Bitwise function highlighting

### Changed

-   Updated keyword list to include all SSL v11 keywords
-   Improved regex patterns for better syntax recognition
-   Added folding rules for inline code blocks

## [0.2.0] - 2024-10-12

### Added

-   Auto-completion support for SSL files
-   Custom completion items configurable through VS Code settings
-   Command to reload completion items

### Changed

-   Improved code structure and error handling

## [0.1.0] - 2024-10-10

### Added

-   Initial release
-   Syntax highlighting for SSL files
-   Code folding for SSL-specific constructs
-   Special highlighting for folds in the minimap
