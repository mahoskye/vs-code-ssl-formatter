# Change Log

All notable changes to the "STARLIMS Scripting Language Formatter" extension will be documented in this file.

## [0.3.0] - 2025-05-21

### Added

- Enhanced syntax highlighting based on complete EBNF grammar
- Support for scientific notation in number literals
- Highlighting for logical operators (.AND., .OR., .NOT.)
- Specialized highlighting for different function types (DB, special functions)
- Property access highlighting with object:property syntax
- SQL parameter highlighting
- Bitwise function highlighting

### Changed

- Updated keyword list to include all SSL v11 keywords
- Improved regex patterns for better syntax recognition
- Added folding rules for inline code blocks

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
