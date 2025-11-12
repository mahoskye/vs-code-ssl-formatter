# STARLIMS Scripting Language (SSL) Extension for VS Code

A comprehensive Visual Studio Code extension providing syntax highlighting, code formatting, IntelliSense, and code quality tools for STARLIMS Scripting Language (SSL).

## Features

### Syntax Highlighting

- **Full Syntax Support**: Comprehensive TextMate grammar for all SSL language constructs including keywords, operators, literals, and comments
- **Keyword Recognition**: Case-sensitive keyword highlighting (`:IF`, `:WHILE`, `:PROCEDURE`, etc.) with colon prefix
- **Built-in Functions**: Recognition and highlighting of common SSL functions (SQLExecute, DOPROC, EMPTY, LEN, etc.)
- **Comment Support**: Proper highlighting of SSL block comments (`/* ... ;`)
- **String Literals**: Support for double quotes, single quotes, and bracket notation
- **SQL Highlighting**: Embedded SQL keyword highlighting within string literals

### Code Folding

Comprehensive folding support for improved code navigation:

- **Procedure Blocks**: `:PROCEDURE...;:ENDPROC;`
- **Loop Structures**: `:FOR...;:NEXT;`, `:WHILE...;:ENDWHILE;`, `:FOREACH...;:NEXT;`
- **Conditional Blocks**: `:IF...;:ENDIF;`, `:BEGINCASE...;:ENDCASE;`
- **Exception Handling**: `:TRY...;:CATCH...;:ENDTRY;`
- **Region Markers**: `:REGION...;:ENDREGION;` and `/* region...;/* endregion;`
- **Multi-line Comments**: Collapsible comment blocks

### Code Formatting

Automatic code formatting according to SSL style guide:

- **Format Document** (`Shift+Alt+F`): Format entire file
- **Format Selection**: Format selected code
- **Keyword Casing**: Normalize keywords to UPPERCASE (configurable)
- **Function Casing**: Normalize built-in functions to PascalCase (configurable)
- **Operator Spacing**: Consistent spacing around operators
- **Indentation**: Configurable tab or space-based indentation
- **Line Management**: Trim trailing whitespace, ensure final newline
- **Format on Save**: Optional automatic formatting when saving

### IntelliSense & Code Completion

Smart code completion for faster development:

- **Keyword Completion**: All SSL keywords with descriptions
- **Function Completion**: Built-in functions with parameter hints
- **Code Snippets**: Templates for common patterns:
  - Procedure with documentation header
  - IF/ELSE blocks
  - FOR/WHILE loops
  - TRY/CATCH error handling
  - REGION markers
  - CASE statements

### Hover Information

Detailed information on hover:

- **Keyword Documentation**: Description of SSL keywords
- **Function Signatures**: Parameter lists and return types
- **Hungarian Notation Hints**: Type information for properly prefixed variables

### Code Quality & Diagnostics

Real-time code analysis and style enforcement:

- **Block Depth Checking**: Warns when nesting exceeds configured limit (default: 4)
- **Parameter Count**: Flags procedures with too many parameters (default: 8)
- **Hungarian Notation**: Validates variable naming conventions (s=string, n=numeric, b=boolean, etc.)
- **SQL Injection Prevention**: Warns about non-parameterized SQL queries
- **Style Guide Enforcement**: Missing semicolons, nested ternaries, missing OTHERWISE clauses
- **Configurable Severity**: Choose between error, warning, or info levels

### Editor Features

- **Outline View**: Hierarchical view of procedures, regions, and variables
- **Breadcrumbs**: Navigation bar showing current position in code
- **Symbol Highlighting**: Automatic highlighting of symbol occurrences
- **Bracket Matching**: Matching pairs for `()`, `[]`, `{}`
- **Auto-Closing Pairs**: Automatic insertion of closing brackets and quotes
- **Comment Toggling**: `Ctrl+/` to toggle comments

## Supported File Types

The extension activates for files with the following extensions:

- `.ssl`
- `.srvscr` (server scripts)
- `.ds` (data scripts)
- `.ssl.txt`
- `.ds.txt`

## Configuration

The extension provides extensive configuration options. Access settings via `Ctrl+,` (or `Cmd+,` on macOS) and search for "SSL".

### General Settings

- `ssl.strictStyleGuideMode`: Enable stricter enforcement (converts warnings to errors)
- `ssl.maxNumberOfProblems`: Maximum diagnostics per file (default: 100)

### Formatting Settings

- `ssl.format.indentStyle`: `tab` or `space` (default: `tab`)
- `ssl.format.indentWidth`: Number of tabs/spaces per indent (default: 1)
- `ssl.format.keywordCase`: `preserve`, `upper`, or `lower` (default: `upper`)
- `ssl.format.builtinFunctionCase`: `preserve`, `PascalCase`, `lowercase`, or `UPPERCASE` (default: `PascalCase`)
- `ssl.format.wrapLength`: Maximum line length (default: 90)
- `ssl.format.formatOnSave`: Auto-format on save (default: false)
- `ssl.format.trimTrailingWhitespace`: Remove trailing whitespace (default: true)

### Naming Convention Settings

- `ssl.naming.hungarianNotation.enabled`: Enable Hungarian notation validation (default: true)
- `ssl.naming.hungarianNotation.severity`: `warn`, `error`, or `info` (default: `warn`)

### Style Guide Settings

- `ssl.styleGuide.limitBlockDepth`: Maximum nesting level (default: 4, 0=disabled)
- `ssl.styleGuide.maxParamsPerProcedure`: Maximum parameters (default: 8, 0=disabled)

### Security Settings

- `ssl.security.preventSqlInjection`: Warn about SQL injection risks (default: true)
- `ssl.security.requireParameterizedQueries`: Require ?PARAM? placeholders (default: true)

### IntelliSense Settings

- `ssl.intellisense.enabled`: Enable IntelliSense features (default: true)

### Example Configuration

```json
{
  "ssl.strictStyleGuideMode": false,
  "ssl.format.formatOnSave": true,
  "ssl.format.keywordCase": "upper",
  "ssl.format.builtinFunctionCase": "PascalCase",
  "ssl.naming.hungarianNotation.enabled": true,
  "ssl.styleGuide.limitBlockDepth": 4,
  "ssl.security.preventSqlInjection": true
}
```

## SSL Language Specifics

The extension correctly handles SSL's unique characteristics:

- **One-based Indexing**: Arrays and strings start at index 1
- **Comment Syntax**: Comments begin with `/*` and end at the next semicolon `;`
- **Property Access**: Uses colon `:` not dot (e.g., `object:property`)
- **Boolean Literals**: `.T.` (true) and `.F.` (false)
- **NIL Literal**: Represents null/undefined values
- **Assignment**: Primary assignment uses `:=`
- **String Delimiters**: Supports `"text"`, `'text'`, and `[text]`
- **Database Parameters**: Named `?PARAM?` or positional `?`
- **Array Literals**: `{expr1, expr2, expr3}`
- **Code Blocks**: Lambda-style `{|param1, param2| expression}`

## Installation

### From VS Code Marketplace

1. Open Visual Studio Code
2. Press `Ctrl+P` (or `Cmd+P` on macOS)
3. Type `ext install mahoskye.vs-code-ssl-formatter`
4. Press Enter

### From VSIX File

1. Download the `.vsix` file
2. Open VS Code
3. Go to Extensions view (`Ctrl+Shift+X`)
4. Click the "..." menu at the top
5. Select "Install from VSIX..."
6. Choose the downloaded file

## Building from Source

To build and package the extension:

```bash
# Clone the repository
git clone https://github.com/mahoskye/vs-code-ssl-formatter.git
cd vs-code-ssl-formatter

# Install dependencies
npm install

# Compile TypeScript
npm run compile

# Package the extension
npm run package
```

The packaged `.vsix` file can be installed manually in VS Code.

## Usage Tips

1. **Quick Format**: Press `Shift+Alt+F` to format your entire SSL file
2. **Code Snippets**: Type `procedure`, `if`, `for`, etc., and press Tab for quick templates
3. **Outline Navigation**: Use the Outline view (Explorer panel) to navigate large files
4. **Problem Panel**: View all diagnostics in the Problems panel (`Ctrl+Shift+M`)
5. **Hover for Help**: Hover over keywords and functions for documentation
6. **Format on Save**: Enable `ssl.format.formatOnSave` for automatic formatting

## SSL Style Guide

This extension enforces the STARLIMS SSL Style Guide which includes:

- **Hungarian Notation**: Variables prefixed with type indicators (s, n, b, l, d, a, o)
- **PascalCase**: Procedures and classes
- **UPPERCASE**: Keywords (`:IF`, `:WHILE`, etc.)
- **Tab Indentation**: One tab per indent level
- **90 Character Lines**: Soft wrap at 90 characters
- **Parameterized SQL**: Use ?PARAM? placeholders to prevent SQL injection
- **Block Depth Limit**: Maximum 4 levels of nesting
- **Parameter Limit**: Maximum 8 parameters per procedure

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests on [GitHub](https://github.com/mahoskye/vs-code-ssl-formatter).

## License

This extension is licensed under the [MIT License](LICENSE).

## Changelog

See [CHANGELOG.md](CHANGELOG.md) for version history and updates.

## Support

For issues, feature requests, or questions:

- [GitHub Issues](https://github.com/mahoskye/vs-code-ssl-formatter/issues)
- [GitHub Repository](https://github.com/mahoskye/vs-code-ssl-formatter)

## Acknowledgments

Based on comprehensive analysis of STARLIMS Scripting Language (SSL) v11 documentation and the STARLIMS SSL Style Guide.
