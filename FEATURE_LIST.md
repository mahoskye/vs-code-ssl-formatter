# VS Code SSL Extension: Complete Feature List

This document provides a comprehensive list of all features available in the STARLIMS Scripting Language (SSL) extension for Visual Studio Code.

## 1. Syntax & Highlighting

- **Full Syntax Highlighting**: Provides comprehensive TextMate grammar for all SSL language constructs, including keywords, operators, literals, and comments.
- **Keyword & Identifier Casing**: 
    - **Keywords**: SSL keywords (`:IF`, `:WHILE`, `:PROCEDURE`, etc.) are **case-sensitive** and must be matched exactly with their colon prefix. The formatter normalizes keywords to UPPERCASE by default (configurable).
    - **Identifiers**: Variable and procedure/function names are **case-insensitive** and are highlighted regardless of their casing. The formatter can optionally enforce canonical casing for identifiers via settings.
    - **Built-in Functions**: Function names are case-insensitive (e.g., `SQLExecute`, `sqlexecute`, `SQLEXECUTE` are all valid). The formatter normalizes built-in function names to PascalCase for consistency (e.g., `SQLExecute`, `CreateUdoObject`, `BuildString`).
- **Comment Highlighting**: Differentiates and correctly highlights SSL block comments (`/* ... ;`). Comments start with `/*` and terminate at the next semicolon `;`.
- **Grammar Scope**: `source.ssl`
- **Common Function Recognition**: Highlights frequently used SSL functions including:
    - High-frequency functions (1000+ uses): `SQLExecute`, `DOPROC`, `EXECFUNCTION`, `EMPTY`, `LEN`, `USRMES`, `CHR`, `AADD`, `ALLTRIM`, `AT`, `NOW`, `TODAY`
    - Common functions (100+ uses): `CREATEUDOBJECT`, `BUILDSTRING`, `ASCAN`, `ALEN`, `ARRAYCALC`, `BUILDARRAY`, `DIRECTORY`, `CREATEGUID`, and others

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
- **Comment Toggling**: Press `Ctrl+/` (or `Cmd+/` on macOS) to quickly add or remove `/* ... ;` comments from selected lines. Essential for rapid code commenting/uncommenting during development and debugging.
- **Breadcrumbs Navigation**: Navigation bar at the top of the editor showing the current file path and position within the code structure (e.g., `file.ssl > CalculateTotal > IF block`). Click any breadcrumb level to navigate quickly through the code hierarchy.

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
 - **Signature Help / Parameter Hints**: Shows procedure parameter lists and inline hints while typing calls.
 - **Rename / PrepareRename & Refactor**: Rename symbols (local and workspace-aware), and provide basic refactor primitives. `PrepareRename` verifies rename target validity before editing.
 - **CodeLens & Reference Counts**: Optional inline CodeLens that shows reference counts above procedures and other top-level symbols.
 - **Inlay Hints (Optional)**: Experimental inlay hints for parameter names and inferred types to improve readability.
 - **Call Hierarchy**: View a tree showing which procedures call the current one (incoming calls) and which procedures it calls (outgoing calls). Provides visual understanding of code flow and dependencies.
 - **Workspace Symbol Search**: Press `Ctrl+T` (or `Cmd+T` on macOS) and type any procedure or variable name to instantly jump to it anywhere in the workspace. Enables fast navigation across large multi-file projects.
 - **Code Snippets**: Provides templates for common code patterns including:
     - **Procedure Header**: Standard documentation header template:
       ```
       /************************************************************
       Description.. : [Brief description of procedure purpose]
       Parameters... : [Parameter descriptions]
       Returns...... : [Return value description]
       Author....... : [Author initials]
       Date......... : YYYY-MM-DD
       ************************************************************/
       ```
     - Procedure skeleton with `:PARAMETERS`, `:DEFAULT`, `:TRY...:CATCH`, and `:RETURN`
     - Loop structures (`:FOR...:TO...:STEP...:NEXT`, `:WHILE...:ENDWHILE`)
     - Conditional blocks (`:IF...:ELSE...:ENDIF`, `:BEGINCASE...:CASE...:OTHERWISE...:ENDCASE`)
     - Exception handling (`:TRY...:CATCH...:FINALLY...:ENDTRY`)
     - Region markers (`:REGION name;...:ENDREGION;`)
     - Array literals (`{expr1, expr2, expr3}`)
     - Code blocks (`{|param1, param2| expression}`)
 - **Include/Import Support**: IntelliSense and navigation support for `:INCLUDE` statements, enabling cross-file code resolution and Go to Definition for included SSL files. Enforces include order (external before local, placed at top of file after header comments).

## 4. Code Quality & Diagnostics

- **Real-time Diagnostics**: Provides instant feedback and error reporting as you type.
- **Error Detection**: Identifies and reports a wide range of issues, including:
    - **Symbol Errors**: Undefined symbols, duplicate declarations, and scope violations.
    - **Type Consistency**: Basic checks for type mismatches.
    - **Syntax Errors**: Mismatched block keywords (`:IF` without `:ENDIF`), missing semicolons, unclosed strings/brackets.
    - **Style Guide Adherence**: The extension can be configured to enforce additional style guide rules, such as:
        - **Block Depth Limiting** (`limit_block_depth: 4`): Warns when nesting exceeds 4 levels.
        - **Parameter Count** (`max_params_per_procedure: 8`): Flags procedures with more than 8 parameters.
        - **Global Variable Usage** (`disallow_globals: true`): Discourages use of global variables.
        - **Case Statement Coverage** (`require_otherwise_in_case: true`): Requires an `:OTHERWISE` clause in `:BEGINCASE` blocks.
        - **Hungarian Notation** (`hungarian_notation.enabled: true`, `severity: warn`): Recommends proper prefixes (s=string, n=numeric, b=boolean, l=logical, d=date, a=array, o=object) with warnings for non-compliance. Exceptions allowed for loop counters (i, j, k) and SSL constants (NIL, .T., .F.).
        - **Positive Logic** (`prefer_positive_logic: true`): Encourages positive conditionals over negative ones.
        - **Ternary Nesting** (`no_nested_ternaries: true`): Prevents nested ternary expressions.
    - **Security Diagnostics**:
        - **SQL Injection Prevention** (`prevent_sql_injection: true`): Warns about non-parameterized SQL queries.
        - **Parameterized Queries** (`parameterized_queries_required: true`): Requires use of `?PARAM?` or `?` placeholders in SQL.
        - **Database Parameter Validation** (`validate_database_parameters: warn`): Validates parameter count matches placeholders.
    - **Performance Diagnostics**:
        - SQL optimization hints (prefer `EXISTS` over `DISTINCT`, use `BETWEEN` for ranges, avoid `SELECT *`).
- **Configurable Problem Limits**: `ssl.maxNumberOfProblems` setting to control the maximum number of reported diagnostics.

- **Code Actions / Quick Fixes**: Diagnostic-based code actions and quick fixes for common problems (for example: add missing `:END` / `:ENDPROCEDURE`, add a missing variable declaration, suggest canonical keyword casing). These appear in the lightbulb menu and the Quick Fix command.
- **Auto-Fix / Fix All**: Configurable auto-fix on save and "Fix All" behavior for certain rule classes. Controlled by settings such as `ssl.autoFix.onSave` and `ssl.autoFix.onSave.rules`.
- **Suppression & Severity Levels**: Rules can be configured to be errors, warnings, or info; diagnostics can be locally suppressed via inline annotations (if supported) and globally via settings.

## 5. Code Formatting

- **Format Document (Shift+Alt+F)**: Reformats an entire SSL file according to a comprehensive set of style rules.
- **Format Selection**: Reformats only the selected portion of code.
- **Comprehensive Formatting Rules**:
    - **Consistent Indentation**: Uses tab-based indentation (configurable). Default: 1 tab per indent level with 1 tab continuation indent.
    - **Operator Spacing**: Normalizes spacing around operators:
        - Space around assignment operators (`:=`, `+=`, `-=`, `*=`, `/=`, `^=`, `%=`)
        - Space around comparison operators (`==`, `!=`, `<>`, `#`, `<`, `>`, `<=`, `>=`, `=`)
        - Space around logical operators (`.AND.`, `.OR.`, `.NOT.`)
        - Space after commas
        - No space before semicolons
        - No space inside parentheses, brackets, or braces
    - **Keyword Casing**: Normalizes all SSL keywords to UPPERCASE (e.g., `:IF`, `:WHILE`, `:PROCEDURE`). Keywords are case-sensitive and must include the colon prefix.
    - **Built-in Function Casing**: Normalizes built-in function names to PascalCase (e.g., `SQLExecute`, `CreateUdoObject`, `BuildString`, `DoProc`, `ExecFunction`).
    - **Identifier Casing**: Enforces naming conventions:
        - Procedures: `PascalCase`
        - Classes: `PascalCase`
        - Variables: `camelCase` with Hungarian notation prefixes
        - Constants: `UPPER_SNAKE_CASE`
    - **Whitespace Management**: Trims trailing whitespace and ensures single statements per line.
    - **Blank Line Control**: 
        - 1 blank line between top-level blocks (procedures, classes, regions)
        - 1 blank line before `:REGION` and `:PROCEDURE` blocks
    - **Line Length**: Soft wrap at 90 characters (configurable), breaking on commas or before operators.
    - **Statement Termination**: All statements must end with semicolon `;`
    - **Final Newline**: Ensures files end with a newline character.
- **Configurable Formatting**:
    - `ssl.format.indentStyle`: `tab` (default) or `space` — indentation character type.
    - `ssl.format.indentWidth`: Number of tabs or spaces per indent level (default: 1 for tabs, 4 for spaces).

- **Format-on-save / Format-on-type / Format-on-paste**: Optional behaviors to run the formatter automatically:
    - `ssl.format.formatOnSave` (boolean) — run formatting when the document is saved.
    - `ssl.format.formatOnType` (boolean) — format as you type (for small, incremental edits).
    - `ssl.format.formatOnPaste` (boolean) — format pasted code blocks.

- **Additional Formatting Options**:
    - `ssl.format.keywordCase`: `preserve | upper | lower` — preferred keyword casing (default: `upper`).
    - `ssl.format.builtinFunctionCase`: `preserve | PascalCase | lowercase | UPPERCASE` — built-in function casing (default: `PascalCase`).
    - `ssl.format.identifierCasing`: `preserve | camelCase | PascalCase` — optional identifier canonicalization.
    - `ssl.format.wrapLength`: integer — maximum line length before wrapping (default: 90).
    - `ssl.format.trimTrailingWhitespace`: boolean — explicitly control trimming trailing whitespace (default: true).
    - `ssl.format.blankLinesBetweenProcedures`: integer — number of blank lines between top-level blocks (default: 1).
    - `ssl.format.finalNewline`: boolean — ensure file ends with newline (default: true).
    - `ssl.format.lineEndings`: `lf | crlf | auto` — line ending style (default: `lf`).
    - `ssl.format.encoding`: string — file encoding (default: `utf-8`).
    - `ssl.format.enableSqlFormatting`: boolean — when true, attempt targeted SQL formatting inside known SQL/SQL-like string blocks with UPPERCASE keywords.

## 6. Configuration

- **Extension Settings**: Users can configure the extension via VS Code settings (`settings.json`).
- **Strict Style Guide Mode**:
    - `ssl.strictStyleGuideMode`: A boolean flag to enable or disable stricter style guide rules, such as **requiring** Hungarian notation (errors instead of warnings), preventing nested ternaries, requiring `:OTHERWISE` in case statements, and SQL injection prevention.
- **Hungarian Notation Settings**:
    - `ssl.naming.hungarianNotation.enabled`: boolean — enable Hungarian notation validation (default: true).
    - `ssl.naming.hungarianNotation.strictMode`: boolean — when true, produces errors; when false, produces warnings (default: false).
    - `ssl.naming.hungarianNotation.severity`: `warn | error` — severity level for violations (default: warn).
    - `ssl.naming.hungarianNotation.prefixes`: object — custom prefix mappings (s=string, n=numeric, b=boolean, etc.).
    - `ssl.naming.hungarianNotation.exceptions`: array — allowed exceptions like loop counters [i, j, k], SSL constants [NIL, .T., .F.], abbreviations [ID, SQL, URL, XML, HTML].
- **Naming Convention Settings**:
    - `ssl.naming.procedures`: `PascalCase | preserve` — procedure naming style (default: PascalCase).
    - `ssl.naming.classes`: `PascalCase | preserve` — class naming style (default: PascalCase).
    - `ssl.naming.variables`: `camelCase | preserve` — variable naming style (default: camelCase).
    - `ssl.naming.constants`: `UPPER_SNAKE_CASE | preserve` — constant naming style (default: UPPER_SNAKE_CASE).
    - `ssl.naming.maxVariableLength`: integer — maximum variable name length (default: 20).
    - `ssl.naming.maxFunctionLength`: integer — maximum function name length (default: 30).
- **Server Tracing**:
    - `ssl.trace.server`: A setting for developers to trace communication with the language server for debugging purposes.

- **Telemetry & Privacy**:
    - `ssl.telemetry.enabled`: boolean — telemetry is opt-in and disabled by default; the extension will document what is collected and why.

- **Localization / i18n Readiness**:
    - All user-facing strings are prepared for localization and the extension is structured so translations can be added later.

- **Multi-root Workspace Support**:
    - The extension fully supports VS Code multi-root workspaces, allowing you to work with multiple SSL projects simultaneously with independent configuration and symbol indexing per workspace folder.

- **Theme & Color Customization**:
    - Semantic token support enables custom color themes to style SSL keywords, variables, functions, and other language elements.
    - Users can customize syntax highlighting colors via VS Code's `editor.tokenColorCustomizations` setting.
    - Extension provides semantic token modifiers for enhanced theming (e.g., `readonly`, `declaration`, `modification`).

- **Security & Performance Settings**:
    - `ssl.security.preventSqlInjection`: boolean — warn about non-parameterized SQL (default: true).
    - `ssl.security.requireParameterizedQueries`: boolean — require `?PARAM?` in SQL strings (default: true).
    - `ssl.security.validateDatabaseParameters`: `off | warn | error` — validate parameter count (default: warn).
    - `ssl.performance.preferExistsOverDistinct`: boolean — SQL optimization hint (default: true).
    - `ssl.performance.avoidSelectStar`: boolean — warn against `SELECT *` (default: true).

- **Other Useful Settings**:
    - `ssl.autoFix.onSave` — enable/disable auto fix on save (default: false).
    - `ssl.autoFix.onSave.rules` — array of rule IDs to auto-fix on save.
    - `ssl.performance.indexing.maxFiles` — tune indexing limits for very large workspaces (default: 10000).
    - `ssl.files.encoding`: string — default file encoding (default: utf-8).
    - `ssl.files.lineEndings`: `lf | crlf | auto` — line ending preference (default: lf).
    - `ssl.files.oneProcedurePerFile`: boolean — enforce single procedure per file (default: false).
## 7. Commands & Keybindings

- The extension contributes several commands to the Command Palette (Command+Shift+P) and reasonable default keybindings which can be changed by users. Examples:
    - `ssl.formatDocument` — Format Document (default: Shift+Alt+F)
    - `ssl.formatSelection` — Format Selection
    - `ssl.toggleStrictStyleGuideMode` — Toggle strict style guide mode
    - `ssl.insertProcedureHeader` — Insert standard procedure header snippet
    - `ssl.runWorkspaceFormat` — Run formatter across the workspace (use with care)

Default keybindings are documented but intentionally minimal so users can customize them via the standard VS Code keyboard shortcuts UI.

## 8. Performance, Indexing & Tests

- **Workspace Indexing & Caching**: The language features rely on workspace indexing for fast cross-file features (Go to Definition, Find All References, rename). The implementation supports incremental indexing and on-disk caching to reduce startup cost for large projects.
- **Performance Expectations**: The extension aims to provide near-interactive responses for single-file operations and efficient, batched updates for workspace-wide analyses. Large workspaces can be tuned with indexing and memory-related settings.
- **Tests & CI**: The repository includes unit and integration tests (see `test/`) that cover formatter behavior, language features, and provider contracts. CI is expected to run these tests for PR validation.

## 9. User Interface Elements

- **Status Bar Integration**: Displays current SSL context information in the VS Code status bar, including:
    - Current procedure name
    - Document statistics (line count, procedure count)
    - Language mode indicator
    - Optional formatting status and diagnostics summary

## 10. Extension Metadata & Discoverability

- **Marketplace Presence**: The extension is published with comprehensive metadata for easy discovery:
    - Categories: Programming Languages, Formatters, Linters
    - Tags: SSL, STARLIMS, scripting, laboratory, LIMS
    - Custom icon representing the SSL/STARLIMS brand
    - Gallery banner with appropriate theming
    - Screenshots and animated GIFs demonstrating key features
- **Documentation**: Comprehensive README with feature overview, installation instructions, usage examples, and configuration guidance.

## 11. Supported File Types

The extension automatically activates for files with the following extensions:
- `.ssl`
- `.srvscr` (server scripts)
- `.ds` (data scripts)
- `.ssl.txt`
- `.ds.txt`

## 12. SSL Language Specifics

The extension correctly handles SSL's unique syntax characteristics:

- **One-based Indexing**: Arrays and strings use 1-based indexing (first element is `[1]`, not `[0]`).
- **Comment Syntax**: Comments begin with `/*` and end at the next semicolon `;` (not `*/`).
- **Property/Method Access**: Uses colon `:` not dot `.` for object property/method access (e.g., `object:property`, `object:method(args)`).
- **Boolean Literals**: `.T.` (true) and `.F.` (false) with periods.
- **NIL Literal**: `NIL` represents null/undefined values.
- **Assignment Operators**: Primary assignment uses `:=`; compound assignments include `+=`, `-=`, `*=`, `/=`, `^=`, `%=`.
- **String Delimiters**: Supports double quotes `"text"`, single quotes `'text'`, and brackets `[text]`.
- **String Concatenation**: Uses `+` operator; no implicit concatenation.
- **Database Parameter Placeholders**: Named `?PARAM_NAME?` or positional `?` within SQL strings.
- **Array Literals**: Use braces `{expr1, expr2, expr3}`; empty slots allowed for parameter skipping `{val1,,val3}`.
- **Code Block Literals**: Lambda-style `{|param1, param2| expression}`.
- **Increment/Decrement**: Supports both prefix and postfix `++` and `--`.
- **Bitwise Operations**: Via functions `_AND()`, `_OR()`, `_XOR()`, `_NOT()` (not operators).
- **Dynamic Code Execution**: `eval()`, `GetRegion()`, `GetInlineCode()` functions.

## 13. Future/Excluded Features

The following features are explicitly **not** included in the current version but may be considered for future releases:

- **Built-in STARLIMS Function Knowledge**: Limited IntelliSense for common STARLIMS API functions. The extension recognizes high-frequency functions (SQLExecute, DOPROC, etc.) but doesn't provide complete API documentation to avoid confidentiality concerns. Focuses primarily on user-written code.
- **User-provided Signature Files**: No support for loading external function definition files yet, though the architecture allows for this to be added later.
- **Semantic Highlighting**: Advanced coloring based on variable scope or type (parameters vs locals vs globals) is not yet implemented.
- **Advanced Refactoring**: Operations like "Extract Procedure" or "Change Signature" beyond basic rename are not yet available.
- **Debugging Support**: No debug adapter protocol implementation for stepping through SSL code execution.
- **STARLIMS Integration**: No direct integration with STARLIMS server for deployment, compilation, or runtime debugging.

