# Change Log

All notable changes to the "STARLIMS Scripting Language Formatter" extension will be documented in this file.

# Changelog

## [0.5.0-formatter] - In Development

### Added

-   Formatter implementation (next phase)

## [0.4.0] - 2025-06-08

### Added

-   Complete SSL parser implementation
-   AST (Abstract Syntax Tree) node definitions
-   Comprehensive parser for all SSL language constructs including:
    -   Expressions and statements
    -   Control flow structures (if/else, while, for loops)
    -   Class definitions and procedures
    -   Try/catch statements and case statements
    -   SQL statements and utilities
-   Extensive parser test suite with EBNF compliance validation

## [0.3.0] - 2025-06-07

### Added

-   Complete SSL tokenizer implementation
-   Token type definitions and lexical analysis
-   Comprehensive tokenizer test suite

## [0.2.0] - 2025-06-04

### Added

-   EBNF Grammar guide for SSL in `docs/ssl-ebnf-grammar-complete.md`
-   New version of `development_plan.md` to help focus the project
-   Initial implementation of JEST tests for the extension

### Changed

-   Revised the `syntaxes/ssl.tmLanguage.json` to improve syntax highlighting
-   Improved `language-configuration.json` to better handle comments and brackets

### Removed

-   Removed code folding for SSL-specific constructs. Wasn't happy with the implementation and it was causing issues with the minimap.
-   Removed special highlighting for folds in the minimap. Folding and highlighting will be reintroduced in a future release with a more robust implementation.

## [0.1.0] - 2024-10-10

### Added

-   Initial release
-   Syntax highlighting for SSL files
-   Code folding for SSL-specific constructs
-   Special highlighting for folds in the minimap
