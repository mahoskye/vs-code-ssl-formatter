# VS Code SSL Extension: Complete Feature List

This document provides a comprehensive list of all features available in the STARLIMS Scripting Language (SSL) extension for Visual Studio Code.

## 1. Syntax & Highlighting

- **Full Syntax Highlighting**: Provides comprehensive TextMate grammar for all SSL language constructs, including keywords, operators, literals, and comments.
- **Case-Insensitive Recognition**: Correctly highlights all SSL keywords and operators, regardless of casing.
- **Comment Highlighting**: Differentiates and correctly highlights SSL block comments (`/* ... ;`).
- **Grammar Scope**: `source.ssl`

## 2. Editor Features

- **Code Folding**: Allows collapsing and expanding of code blocks for:
    - Procedure blocks (`:PROCEDURE`)
    - Loops (`:FOR`, `:WHILE`)
    - Conditionals (`:IF...:ELSE`, `:BEGINCASE`)
    - Exception handling (`:TRY...:CATCH`)
    - Keyword-based regions (`:REGION` / `:ENDREGION`)
    - Comment-based regions (`/* region ... ;` and `/* endregion;`)
- **Bracket Matching**: Automatically highlights matching pairs of `()`, `[]`, and `{}`.
- **Auto-Closing Pairs**: Automatically inserts the closing character for:
    - Brackets: `()`, `[]`, `{}`
    - Quotes: `""`, `''`
    - Comments: `/* ;`
- **Smart Indentation**: Automatically adjusts indentation based on SSL's block structure (e.g., inside a `PROCEDURE` or `IF` statement).
- **Symbol Highlighting**: Automatically highlights all occurrences of the symbol currently under the cursor.

## 3. Code Intelligence

- **Outline View**: Provides a hierarchical, symbolic view of the current file in the Explorer panel, showing:
    - Procedures (`:PROCEDURE`)
    - Variable declarations
    - :REGIONs
- **Go to Definition (F12)**: Jump directly from a symbol's usage to its declaration.
- **Find All References (Shift+F12)**: Find and list all usages of a variable or procedure within the current file.
- **Hover Information**: Displays a tooltip with symbol details on mouseover, including:
    - Symbol type (e.g., `VARIABLE`, `:PROCEDURE`)
    - Declaration location
- **Code Completion**: Provides context-aware suggestions for:
    - SSL keywords
    - Operators
    - Variables defined in the current scope
    - Built-in functions and literals.
- **Procedure Header Snippets**: Provides a snippet to quickly insert the standard procedure header comment block, helping to enforce documentation standards.

## 4. Code Quality & Diagnostics

- **Real-time Diagnostics**: Provides instant feedback and error reporting as you type.
- **Error Detection**: Identifies and reports a wide range of issues, including:
    - **Symbol Errors**: Undefined symbols, duplicate declarations, and scope violations.
    - **Type Consistency**: Basic checks for type mismatches.
    - **Style Guide Adherence**: The extension can be configured to enforce additional style guide rules, such as:
        - Limiting block depth (`limit_block_depth`).
        - Limiting parameters per procedure (`max_params_per_procedure`).
        - Discouraging global variables (`disallow_globals`).
        - Requiring an `:OTHERWISE` clause in `:BEGINCASE` blocks.
- **Configurable Problem Limits**: `ssl.maxNumberOfProblems` setting to control the maximum number of reported diagnostics.

## 5. Code Formatting

- **Format Document (Shift+Alt+F)**: Reformats an entire SSL file according to a comprehensive set of style rules.
- **Format Selection**: Reformats only the selected portion of code.
- **Comprehensive Formatting Rules**:
    - **Consistent Indentation**: Enforces a consistent indentation size for all code blocks.
    - **Operator Spacing**: Normalizes spacing around assignment, comparison, and logical operators.
    - **Keyword Casing**: Enforces consistent casing for all SSL keywords.
    - **Whitespace Management**: Trims trailing whitespace and ensures single statements per line.
    - **Blank Line Control**: Manages blank lines between major code blocks (e.g., `:PROCEDURE` blocks).
- **Configurable Formatting**:
    - `ssl.format.indentSize`: Set the number of spaces for an indentation level.

## 6. Configuration

- **Extension Settings**: Users can configure the extension via VS Code settings (`settings.json`).
- **Strict Style Guide Mode**:
    - `ssl.strictStyleGuideMode`: A boolean flag to enable or disable stricter style guide rules, such as warnings for Hungarian notation.
- **Server Tracing**:
    - `ssl.trace.server`: A setting for developers to trace communication with the language server for debugging purposes.

## 7. Supported File Types

The extension automatically activates for files with the following extensions:
- `.ssl`
- `.srvscr`
- `.ds`
- `.ssl.txt`
- `.ds.txt`
