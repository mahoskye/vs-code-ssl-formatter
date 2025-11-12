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

- **Code Folding**: Comprehensive folding support with intelligent block detection that allows collapsing and expanding of code structures for improved readability and navigation:
    - **Procedure Blocks** (`:PROCEDURE...;:ENDPROCEDURE;`): Collapse entire procedure definitions including parameters, defaults, body, and return statements. Fold markers appear in the gutter next to procedure declarations.
    - **Loop Structures**:
        - `:FOR...;:TO...;:STEP...;:NEXT;` — Collapse for-loops with all their iterations
        - `:WHILE...;:ENDWHILE;` — Collapse while-loop blocks
        - `:FOREACH...;:IN...;:NEXT;` — Collapse foreach-loop blocks
    - **Conditional Blocks**:
        - `:IF...;:ELSEIF...;:ELSE...;:ENDIF;` — Collapse entire if-else chains or individual branches
        - `:BEGINCASE...;:CASE...;:OTHERWISE...;:ENDCASE;` — Collapse case statements and individual case branches
        - Supports nested conditionals with independent fold controls
    - **Exception Handling** (`:TRY...;:CATCH...;:FINALLY...;:ENDTRY;`): Collapse try-catch-finally blocks independently or as a whole
    - **Explicit Region Markers**:
        - **Keyword-based regions**: `:REGION region_name;...;:ENDREGION;` — Create custom collapsible sections for organizing related code blocks (e.g., "Initialization", "Data Validation", "Cleanup")
        - **Comment-based regions**: `/* region region_name;...;/* endregion;` — Alternative region syntax using SSL comments for compatibility
        - Region names appear in the folded placeholder text for easy identification
    - **Multi-line Comments**: `/* ... ;` — Fold large comment blocks to reduce visual clutter
    - **Array and Object Literals**: Fold multi-line array `{...}` and code block `{|...|...}` literals
    - **Fold All / Unfold All**: Use `Ctrl+K Ctrl+0` to collapse all foldable regions, `Ctrl+K Ctrl+J` to expand all
    - **Fold Level Commands**: `Ctrl+K Ctrl+1` through `Ctrl+K Ctrl+7` to fold to specific nesting levels
    - **Fold Recursively**: `Ctrl+K Ctrl+[` to fold the current region and all nested regions
    - **Visual Indicators**: Subtle fold icons in the editor gutter; folded regions show `...` with context preview on hover

- **Minimap Highlighting**: Enhanced minimap integration for rapid visual navigation and code overview:
    - **Syntax-aware Colorization**: The minimap on the right side of the editor displays a scaled-down, syntax-highlighted view of the entire file, making it easy to scan for procedures, comments, and code blocks at a glance
    - **Current Viewport Indicator**: Highlighted rectangle in the minimap shows the currently visible portion of the document; click or drag to navigate instantly to any part of the file
    - **Symbol Markers**: Key language constructs are visually emphasized in the minimap:
        - `:PROCEDURE` blocks appear as distinct color blocks for quick procedure location
        - `:REGION` sections are visually demarcated
        - Comments and documentation blocks are distinguished from code
    - **Search Result Highlights**: Active search matches are highlighted in the minimap, allowing you to see the distribution of search results across the entire file
    - **Diagnostic Markers**: Errors and warnings appear as colored bars (red for errors, yellow for warnings, blue for information) at their line positions in the minimap
    - **Selection Highlighting**: Current selection and all matching occurrences of the selected text are highlighted in the minimap
    - **Customizable Size**: Minimap width and character rendering can be configured via VS Code settings:
        - `editor.minimap.enabled`: Toggle minimap on/off (default: true)
        - `editor.minimap.size`: Control minimap size (`proportional`, `fill`, `fit`)
        - `editor.minimap.scale`: Adjust minimap zoom level (1-3)
        - `editor.minimap.showSlider`: Control viewport slider visibility (`always`, `mouseover`)
        - `editor.minimap.maxColumn`: Limit maximum column rendered in minimap (default: 120)
    - **Performance Optimized**: Efficiently renders even for large SSL files (1000+ lines) with minimal performance impact

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

The SSL extension provides extensive configuration options accessible through VS Code's settings UI (`Ctrl+,` or `Cmd+,`) or directly in `settings.json`. Settings can be configured at the User level (global), Workspace level (project-specific), or Folder level (for multi-root workspaces).

### 6.1 General Extension Settings

- **Extension Settings**: Users can configure the extension via VS Code settings (`settings.json`) or the Settings UI.
- **Strict Style Guide Mode**:
    - `ssl.strictStyleGuideMode`: boolean — Master switch to enable stricter style guide rules. When enabled, converts many warnings to errors and enforces stricter coding standards including **required** Hungarian notation, preventing nested ternaries, requiring `:OTHERWISE` in case statements, and SQL injection prevention (default: false).
    - Example: `"ssl.strictStyleGuideMode": true`

### 6.2 Formatting Configuration

Comprehensive control over code formatting behavior and style preferences:

- **Indentation Style**:
    - `ssl.format.indentStyle`: `tab | space` — Character type for indentation (default: `tab`).
    - `ssl.format.indentWidth`: integer — Number of tabs or spaces per indent level (default: 1 for tabs, 4 for spaces).
    - `ssl.format.continuationIndent`: integer — Additional indentation for wrapped lines (default: 1).
    - Example: `"ssl.format.indentStyle": "space", "ssl.format.indentWidth": 4`

- **Keyword & Identifier Casing**:
    - `ssl.format.keywordCase`: `preserve | upper | lower` — Preferred casing for SSL keywords (default: `upper`).
    - `ssl.format.builtinFunctionCase`: `preserve | PascalCase | lowercase | UPPERCASE` — Casing for built-in function names (default: `PascalCase`).
    - `ssl.format.identifierCasing`: `preserve | camelCase | PascalCase` — Optional canonicalization of user-defined identifiers (default: `preserve`).
    - Example: `"ssl.format.keywordCase": "upper", "ssl.format.builtinFunctionCase": "PascalCase"`

- **Line Length & Wrapping**:
    - `ssl.format.wrapLength`: integer — Maximum line length before wrapping (soft limit) (default: 90, range: 60-200).
    - `ssl.format.wrapAtComma`: boolean — Prefer breaking lines at commas in argument lists (default: true).
    - `ssl.format.wrapBeforeOperator`: boolean — When wrapping, place operators at the start of the new line (default: true).
    - Example: `"ssl.format.wrapLength": 100`

- **Whitespace & Spacing**:
    - `ssl.format.trimTrailingWhitespace`: boolean — Remove trailing whitespace from all lines (default: true).
    - `ssl.format.spaceAroundOperators`: boolean — Enforce spaces around assignment and comparison operators (default: true).
    - `ssl.format.spaceAfterComma`: boolean — Enforce space after commas in parameter lists (default: true).
    - `ssl.format.spaceInsideParentheses`: boolean — Add spaces inside parentheses (default: false).
    - `ssl.format.spaceInsideBrackets`: boolean — Add spaces inside brackets (default: false).
    - Example: `"ssl.format.spaceAroundOperators": true`

- **Blank Lines**:
    - `ssl.format.blankLinesBetweenProcedures`: integer — Number of blank lines between top-level blocks (default: 1, range: 0-5).
    - `ssl.format.blankLinesBeforeRegion`: integer — Blank lines before `:REGION` markers (default: 1, range: 0-3).
    - `ssl.format.maxConsecutiveBlankLines`: integer — Maximum allowed consecutive blank lines (default: 2, range: 1-5).
    - Example: `"ssl.format.blankLinesBetweenProcedures": 2`

- **File Endings**:
    - `ssl.format.finalNewline`: boolean — Ensure file ends with a newline character (default: true).
    - `ssl.format.lineEndings`: `lf | crlf | auto` — Line ending style: Unix (`lf`), Windows (`crlf`), or automatic detection (default: `lf`).
    - `ssl.format.encoding`: string — File encoding (default: `utf-8`). Supported: `utf-8`, `utf-16le`, `utf-16be`, `iso-8859-1`, `windows-1252`.
    - Example: `"ssl.format.lineEndings": "crlf"`

- **Special Formatting Features**:
    - `ssl.format.enableSqlFormatting`: boolean — When true, apply targeted SQL formatting inside SQL string literals with UPPERCASE keywords (default: true).
    - `ssl.format.alignAssignments`: boolean — Vertically align consecutive assignment operators (default: false).
    - `ssl.format.sortIncludes`: boolean — Automatically sort `:INCLUDE` statements (external before local) (default: true).
    - Example: `"ssl.format.enableSqlFormatting": true`

- **Format Triggers**:
    - `ssl.format.formatOnSave`: boolean — Automatically format document when saving (default: false).
    - `ssl.format.formatOnType`: boolean — Format incrementally as you type (default: false).
    - `ssl.format.formatOnPaste`: boolean — Format pasted code blocks (default: false).
    - Example: `"ssl.format.formatOnSave": true`

### 6.3 Naming Convention Settings

Control identifier naming rules and Hungarian notation enforcement:

- **Hungarian Notation**:
    - `ssl.naming.hungarianNotation.enabled`: boolean — Enable Hungarian notation validation (default: true).
    - `ssl.naming.hungarianNotation.strictMode`: boolean — When true, produces errors; when false, produces warnings (default: false).
    - `ssl.naming.hungarianNotation.severity`: `warn | error | info` — Severity level for violations (default: `warn`).
    - `ssl.naming.hungarianNotation.prefixes`: object — Custom prefix mappings. Default:
        ```json
        {
          "s": "string",
          "n": "numeric",
          "b": "boolean",
          "l": "logical",
          "d": "date",
          "a": "array",
          "o": "object",
          "u": "udo"
        }
        ```
    - `ssl.naming.hungarianNotation.exceptions`: array — Allowed exceptions (default: `["i", "j", "k", "NIL", ".T.", ".F.", "ID", "SQL", "URL", "XML", "HTML", "API", "UID", "GUID"]`).
    - Example: 
        ```json
        "ssl.naming.hungarianNotation.enabled": true,
        "ssl.naming.hungarianNotation.strictMode": false,
        "ssl.naming.hungarianNotation.exceptions": ["i", "j", "k", "index", "temp"]
        ```

- **Identifier Casing Conventions**:
    - `ssl.naming.procedures`: `PascalCase | preserve | camelCase` — Procedure naming style (default: `PascalCase`).
    - `ssl.naming.classes`: `PascalCase | preserve` — Class naming style (default: `PascalCase`).
    - `ssl.naming.variables`: `camelCase | preserve | PascalCase` — Variable naming style (default: `camelCase`).
    - `ssl.naming.constants`: `UPPER_SNAKE_CASE | preserve | UPPERCASE` — Constant naming style (default: `UPPER_SNAKE_CASE`).
    - `ssl.naming.privateMembers`: `_camelCase | camelCase | preserve` — Private member naming (default: `_camelCase`).
    - Example: `"ssl.naming.procedures": "PascalCase", "ssl.naming.variables": "camelCase"`

- **Length Restrictions**:
    - `ssl.naming.maxVariableLength`: integer — Maximum variable name length (default: 20, range: 10-50).
    - `ssl.naming.maxFunctionLength`: integer — Maximum function/procedure name length (default: 30, range: 15-60).
    - `ssl.naming.minVariableLength`: integer — Minimum meaningful variable name length (default: 2, range: 1-5).
    - Example: `"ssl.naming.maxVariableLength": 25`

- **Naming Pattern Validation**:
    - `ssl.naming.disallowAbbreviations`: boolean — Discourage ambiguous abbreviations in identifiers (default: false).
    - `ssl.naming.requireDescriptiveNames`: boolean — Enforce descriptive names over single letters (except loop counters) (default: true).
    - Example: `"ssl.naming.requireDescriptiveNames": true`

### 6.4 Diagnostic & Code Quality Settings

Configure linting rules, error detection, and code quality checks:

- **General Diagnostics**:
    - `ssl.maxNumberOfProblems`: integer — Maximum number of diagnostics to report per file (default: 100, range: 10-1000).
    - `ssl.diagnostics.enabled`: boolean — Enable/disable all diagnostic checks (default: true).
    - `ssl.diagnostics.onType`: boolean — Show diagnostics as you type (default: true) vs. only on save (false).
    - Example: `"ssl.maxNumberOfProblems": 200`

- **Style Guide Rules**:
    - `ssl.styleGuide.limitBlockDepth`: integer — Maximum nesting level for control structures (default: 4, range: 2-10, 0=disabled).
    - `ssl.styleGuide.maxParamsPerProcedure`: integer — Maximum procedure parameter count (default: 8, range: 3-15, 0=disabled).
    - `ssl.styleGuide.disallowGlobals`: boolean — Discourage use of global variables (default: false).
    - `ssl.styleGuide.requireOtherwiseInCase`: boolean — Require `:OTHERWISE` clause in `:BEGINCASE` blocks (default: true).
    - `ssl.styleGuide.preferPositiveLogic`: boolean — Encourage positive conditionals over negative (default: true).
    - `ssl.styleGuide.noNestedTernaries`: boolean — Prevent nested ternary expressions (default: true).
    - `ssl.styleGuide.requireProcedureHeaders`: boolean — Require documentation header on all procedures (default: false).
    - Example: `"ssl.styleGuide.limitBlockDepth": 5, "ssl.styleGuide.maxParamsPerProcedure": 6`

### 6.5 Security Settings

SQL injection prevention and secure coding practices:

- **SQL Security**:
    - `ssl.security.preventSqlInjection`: boolean — Warn about non-parameterized SQL queries (default: true).
    - `ssl.security.requireParameterizedQueries`: boolean — Require `?PARAM?` or `?` placeholders in SQL strings (default: true).
    - `ssl.security.validateDatabaseParameters`: `off | warn | error` — Validate parameter count matches placeholders (default: `warn`).
    - `ssl.security.disallowDynamicSql`: boolean — Flag dynamic SQL construction via string concatenation (default: false).
    - `ssl.security.warnUnsafeEval`: boolean — Warn about use of `eval()` and similar dynamic execution (default: true).
    - Example: 
        ```json
        "ssl.security.preventSqlInjection": true,
        "ssl.security.requireParameterizedQueries": true,
        "ssl.security.validateDatabaseParameters": "error"
        ```

- **Data Sanitization**:
    - `ssl.security.requireInputValidation`: boolean — Encourage validation of external input (default: true).
    - `ssl.security.warnMissingErrorHandling`: boolean — Flag procedures without `:TRY...:CATCH` blocks (default: false).

### 6.6 Performance Settings

Optimization hints and performance-related diagnostics:

- **SQL Performance**:
    - `ssl.performance.preferExistsOverDistinct`: boolean — Suggest `EXISTS` over `SELECT DISTINCT` for existence checks (default: true).
    - `ssl.performance.avoidSelectStar`: boolean — Warn against `SELECT *` queries (default: true).
    - `ssl.performance.useBetweenForRanges`: boolean — Suggest `BETWEEN` for range queries (default: true).
    - `ssl.performance.warnMissingIndexHints`: boolean — Hint when queries might benefit from indexes (default: false).
    - Example: `"ssl.performance.avoidSelectStar": true`

- **Code Performance**:
    - `ssl.performance.warnNestedLoops`: boolean — Flag potentially inefficient nested loops (default: false).
    - `ssl.performance.suggestArrayPreallocation`: boolean — Suggest preallocating arrays for large datasets (default: true).

- **Indexing & Caching**:
    - `ssl.performance.indexing.enabled`: boolean — Enable workspace symbol indexing (default: true).
    - `ssl.performance.indexing.maxFiles`: integer — Maximum files to index (default: 10000, range: 100-50000).
    - `ssl.performance.indexing.maxFileSize`: integer — Skip files larger than this (KB) (default: 1024, range: 100-10240).
    - `ssl.performance.indexing.excludePatterns`: array — Glob patterns to exclude from indexing (default: `["**/node_modules/**", "**/temp/**", "**/.git/**"]`).
    - `ssl.performance.caching.enabled`: boolean — Enable on-disk caching for faster startup (default: true).
    - `ssl.performance.caching.invalidateOnChange`: boolean — Auto-invalidate cache when files change (default: true).
    - Example: `"ssl.performance.indexing.maxFiles": 20000`

### 6.7 File & Workspace Settings

File handling, encoding, and workspace organization:

- **File Encoding & Line Endings**:
    - `ssl.files.encoding`: string — Default file encoding (default: `utf-8`). Supported: `utf-8`, `utf-16le`, `utf-16be`, `iso-8859-1`, `windows-1252`.
    - `ssl.files.lineEndings`: `lf | crlf | auto` — Line ending preference (default: `lf`).
    - `ssl.files.autoDetectEncoding`: boolean — Automatically detect file encoding (default: true).
    - Example: `"ssl.files.encoding": "utf-8", "ssl.files.lineEndings": "lf"`

- **File Organization**:
    - `ssl.files.oneProcedurePerFile`: boolean — Enforce single procedure per file (default: false).
    - `ssl.files.matchFilenameToProcedure`: boolean — Require filename to match procedure name (default: false).
    - `ssl.files.includeOrder.enforceExternal First`: boolean — Enforce external includes before local (default: true).
    - `ssl.files.includeOrder.alphabetize`: boolean — Sort includes alphabetically within groups (default: true).
    - Example: `"ssl.files.oneProcedurePerFile": true`

- **Workspace Settings**:
    - `ssl.workspace.multiRoot.enabled`: boolean — Enable multi-root workspace support (default: true).
    - `ssl.workspace.excludePatterns`: array — Exclude patterns for file search and indexing (default: `["**/node_modules/**", "**/temp/**", "**/*.log"]`).
    - `ssl.workspace.followSymlinks`: boolean — Follow symbolic links during indexing (default: false).

### 6.8 IntelliSense & Code Intelligence

Control code completion, hover, and signature help behavior:

- **Code Completion**:
    - `ssl.intellisense.enabled`: boolean — Enable IntelliSense features (default: true).
    - `ssl.intellisense.completionDetail`: `minimal | full` — Amount of detail in completion items (default: `full`).
    - `ssl.intellisense.autoImport`: boolean — Automatically add `:INCLUDE` for external symbols (default: true).
    - `ssl.intellisense.suggestBuiltins`: boolean — Include built-in functions in completions (default: true).
    - `ssl.intellisense.suggestKeywords`: boolean — Include SSL keywords in completions (default: true).
    - `ssl.intellisense.triggerCharacters`: array — Characters that trigger completion (default: `[":", ".", "("]`).
    - Example: `"ssl.intellisense.autoImport": true`

- **Hover & Signatures**:
    - `ssl.intellisense.hover.enabled`: boolean — Show hover information (default: true).
    - `ssl.intellisense.hover.documentation`: boolean — Include documentation in hover (default: true).
    - `ssl.intellisense.signatureHelp.enabled`: boolean — Show parameter hints (default: true).
    - `ssl.intellisense.signatureHelp.activeParameter`: boolean — Highlight active parameter (default: true).

- **Advanced Features**:
    - `ssl.intellisense.codeLens.enabled`: boolean — Show CodeLens reference counts (default: true).
    - `ssl.intellisense.codeLens.showReferences`: boolean — Show reference counts above symbols (default: true).
    - `ssl.intellisense.inlayHints.enabled`: boolean — Show inlay hints for parameters/types (default: false).
    - `ssl.intellisense.inlayHints.parameterNames`: boolean — Show parameter name hints (default: false).

### 6.9 Auto-Fix & Code Actions

Configure automatic fixes and quick fix behavior:

- **Auto-Fix on Save**:
    - `ssl.autoFix.onSave`: boolean — Enable auto-fix when saving files (default: false).
    - `ssl.autoFix.onSave.rules`: array — Specific rule IDs to auto-fix on save (default: `[]`). Examples: `["missing-semicolon", "keyword-casing", "trailing-whitespace"]`.
    - `ssl.autoFix.timeout`: integer — Maximum time (ms) to spend on auto-fix (default: 5000, range: 1000-30000).
    - Example: 
        ```json
        "ssl.autoFix.onSave": true,
        "ssl.autoFix.onSave.rules": ["keyword-casing", "trailing-whitespace", "missing-semicolon"]
        ```

- **Code Actions**:
    - `ssl.codeActions.enabled`: boolean — Enable code action quick fixes (default: true).
    - `ssl.codeActions.showDocumentation`: boolean — Include documentation links in code actions (default: true).
    - `ssl.codeActions.sortOrder`: `severity | alphabetical` — How to order quick fixes (default: `severity`).

### 6.10 Debug & Developer Settings

Settings for extension developers and troubleshooting:

- **Server Tracing**:
    - `ssl.trace.server`: `off | messages | verbose` — Trace communication with the language server for debugging (default: `off`).
    - `ssl.trace.logFile`: string — Path to log file for trace output (default: empty, logs to output channel).
    - Example: `"ssl.trace.server": "verbose"`

- **Developer Options**:
    - `ssl.developer.enableExperimentalFeatures`: boolean — Enable experimental/beta features (default: false).
    - `ssl.developer.showParseTree`: boolean — Show AST/parse tree in developer tools (default: false).
    - `ssl.developer.logPerformance`: boolean — Log performance metrics (default: false).

- **Telemetry & Privacy**:
    - `ssl.telemetry.enabled`: boolean — Opt-in telemetry for extension improvement (default: false). Documents what is collected: usage patterns, error rates, feature adoption (no code content).
    - `ssl.telemetry.level`: `off | error | usage | all` — Granularity of telemetry data (default: `off`).

### 6.11 UI & Visual Settings

Control visual elements and user interface preferences:

- **Theme & Color Customization**:
    - Semantic token support enables custom color themes to style SSL keywords, variables, functions, and other language elements.
    - Users can customize syntax highlighting colors via VS Code's `editor.tokenColorCustomizations` setting:
        ```json
        "editor.tokenColorCustomizations": {
          "[Your Theme Name]": {
            "textMateRules": [
              {
                "scope": "keyword.control.ssl",
                "settings": { "foreground": "#569CD6", "fontStyle": "bold" }
              },
              {
                "scope": "entity.name.function.ssl",
                "settings": { "foreground": "#DCDCAA" }
              }
            ]
          }
        }
        ```
    - Extension provides semantic token modifiers for enhanced theming: `readonly`, `declaration`, `modification`, `static`, `deprecated`.

- **Editor Integration**:
    - `ssl.ui.showStatusBarItem`: boolean — Show SSL context in status bar (default: true).
    - `ssl.ui.statusBarPosition`: `left | right` — Status bar item position (default: `left`).
    - `ssl.ui.breadcrumbs.enabled`: boolean — Show breadcrumb navigation (default: true).
    - `ssl.ui.outline.enabled`: boolean — Show outline view (default: true).

### 6.12 Localization & Multi-Root Workspace

- **Localization / i18n Readiness**:
    - `ssl.localization.locale`: string — Preferred display language (default: `auto`, uses VS Code's display language).
    - All user-facing strings are prepared for localization. Currently supports: English (`en`). Additional languages can be contributed.

- **Multi-root Workspace Support**:
    - The extension fully supports VS Code multi-root workspaces, allowing you to work with multiple SSL projects simultaneously with independent configuration and symbol indexing per workspace folder.
    - Settings can be configured at workspace folder level to override workspace or user settings.
    - Each workspace folder maintains its own symbol index and diagnostic state.

### 6.13 Configuration Examples

Complete `settings.json` examples for common scenarios:

**Strict Enterprise Configuration:**
```json
{
  "ssl.strictStyleGuideMode": true,
  "ssl.naming.hungarianNotation.enabled": true,
  "ssl.naming.hungarianNotation.strictMode": true,
  "ssl.styleGuide.limitBlockDepth": 4,
  "ssl.styleGuide.maxParamsPerProcedure": 6,
  "ssl.styleGuide.requireOtherwiseInCase": true,
  "ssl.security.preventSqlInjection": true,
  "ssl.security.requireParameterizedQueries": true,
  "ssl.security.validateDatabaseParameters": "error",
  "ssl.files.oneProcedurePerFile": true,
  "ssl.format.formatOnSave": true,
  "ssl.format.keywordCase": "upper",
  "ssl.format.builtinFunctionCase": "PascalCase"
}
```

**Relaxed Development Configuration:**
```json
{
  "ssl.strictStyleGuideMode": false,
  "ssl.naming.hungarianNotation.enabled": true,
  "ssl.naming.hungarianNotation.severity": "info",
  "ssl.styleGuide.limitBlockDepth": 0,
  "ssl.security.preventSqlInjection": true,
  "ssl.security.validateDatabaseParameters": "warn",
  "ssl.format.formatOnSave": false,
  "ssl.maxNumberOfProblems": 50
}
```

**Performance-Optimized for Large Workspaces:**
```json
{
  "ssl.performance.indexing.maxFiles": 50000,
  "ssl.performance.indexing.maxFileSize": 2048,
  "ssl.performance.caching.enabled": true,
  "ssl.diagnostics.onType": false,
  "ssl.intellisense.codeLens.enabled": false,
  "ssl.workspace.excludePatterns": [
    "**/node_modules/**",
    "**/archive/**",
    "**/backup/**",
    "**/*.log"
  ]
}
```
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

