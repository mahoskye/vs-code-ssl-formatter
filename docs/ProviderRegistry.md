# SSL Provider Registry Documentation

The SSL Provider Registry is a centralized system for managing VS Code language service providers for the STARLIMS Scripting Language (SSL). It provides a clean, maintainable way to register and manage all SSL-related providers according to the SSL EBNF grammar specification.

## Overview

The provider registry system consists of:

-   **`SSLProviderRegistry`**: Main registry class for managing providers
-   **Provider Classes**: Individual language service providers for SSL
-   **Configuration System**: Flexible configuration for different use cases
-   **Automatic Cleanup**: Proper resource management and disposal

## Architecture

### Core Components

1. **Provider Registry (`SSLProviderRegistry`)**

    - Singleton pattern for centralized management
    - Handles provider instantiation and registration
    - Manages VS Code API interactions
    - Provides cleanup and disposal

2. **SSL Language Providers**

    - `SSLFoldingRangeProvider`: Code folding for SSL constructs
    - `SSLDocumentFormattingProvider`: Code formatting
    - `SSLCompletionProvider`: Auto-completion
    - `SSLHoverProvider`: Documentation on hover
    - `SSLDocumentSymbolProvider`: Code outline and navigation
    - `SSLDiagnosticsProvider`: Error detection and validation
    - `SSLBracketMatchingProvider`: Bracket and keyword matching

3. **Configuration System**
    - Default configuration for full SSL support
    - Selective configuration for specific providers
    - Custom configuration for advanced scenarios

## SSL EBNF Grammar Compliance

The provider registry system is designed to fully support the SSL EBNF grammar specification:

### Supported Language Constructs

1. **Procedure Statements**

    - `:PROCEDURE` / `:ENDPROC` blocks
    - Parameter declarations (`:PARAMETERS`)
    - Default parameters (`:DEFAULT`)

2. **Conditional Statements**

    - `:IF` / `:ELSE` / `:ENDIF` blocks
    - Nested conditionals

3. **Loop Statements**

    - `:WHILE` / `:ENDWHILE` loops
    - `:FOR` / `:NEXT` loops
    - Loop control (`:EXITWHILE`, `:EXITFOR`, `:LOOP`)

4. **Switch Case Statements**

    - `:BEGINCASE` / `:ENDCASE` blocks
    - `:CASE` and `:OTHERWISE` clauses
    - `:EXITCASE` statements

5. **Error Handling**

    - `:TRY` / `:CATCH` / `:FINALLY` / `:ENDTRY` blocks
    - `:ERROR` blocks

6. **Object-Oriented Programming**

    - `:CLASS` definitions
    - `:INHERIT` statements
    - Method declarations

7. **Special Blocks**

    - `:REGION` / `:ENDREGION` blocks
    - `:BEGININLINECODE` / `:ENDINLINECODE` blocks

8. **Data Structures**

    - Array literals (`{...}`)
    - Code block literals (`{|...|...}`)
    - Multi-dimensional arrays

9. **Expressions and Operators**

    - Arithmetic operators (`+`, `-`, `*`, `/`, `^`)
    - Comparison operators (`=`, `==`, `!=`, `<`, `>`, `<=`, `>=`)
    - Logical operators (`.AND.`, `.OR.`, `.NOT.`)
    - Assignment operators (`:=`, `+=`, `-=`, `*=`, `/=`, `^=`)

10. **SSL-Specific Features**
    - Hungarian notation support
    - Property access with colon (`:`)
    - Boolean literals (`.T.`, `.F.`)
    - SQL integration (`SqlExecute`, `LSearch`)
    - Function calls (`DoProc`, `ExecFunction`, `ExecUDF`)

## Usage Examples

### Basic Usage (Default Configuration)

```typescript
import { SSLProviderRegistry } from "./providers/providerRegistry";

export function activate(context: vscode.ExtensionContext) {
    const registry = SSLProviderRegistry.getInstance();
    const registration = registry.registerProviders(SSLProviderRegistry.getDefaultConfig());

    context.subscriptions.push(...registration.disposables);
}

export function deactivate() {
    const registry = SSLProviderRegistry.getInstance();
    registry.unregisterProviders();
}
```

### Selective Provider Registration

```typescript
// Register only essential providers
const config = SSLProviderRegistry.createSelectiveConfig({
    foldingRange: true,
    completion: true,
    diagnostics: true,
});

const registration = registry.registerProviders(config);
```

### Custom Configuration

```typescript
const customConfig: ProviderRegistrationConfig = {
    documentSelector: [
        { scheme: "file", language: "ssl" },
        { scheme: "file", pattern: "**/*.ssl.txt" },
    ],
    registerAll: false,
    specificProviders: {
        foldingRange: true,
        documentFormatting: true,
        completion: true,
        diagnostics: true,
    },
};
```

## Provider Details

### Folding Range Provider

Provides code folding for SSL language constructs:

-   **Procedure blocks**: `:PROCEDURE` to `:ENDPROC`
-   **Conditional blocks**: `:IF` to `:ENDIF`
-   **Loop blocks**: `:WHILE` to `:ENDWHILE`, `:FOR` to `:NEXT`
-   **Switch blocks**: `:BEGINCASE` to `:ENDCASE`
-   **Try-catch blocks**: `:TRY` to `:ENDTRY`
-   **Class definitions**: `:CLASS` to end of script
-   **Region blocks**: `:REGION` to `:ENDREGION`
-   **Comment regions**: `/* region` to `/* endregion`
-   **Multi-line constructs**: Arrays, code blocks, SQL strings

### Document Formatting Provider

Provides SSL code formatting according to style guidelines:

-   **Indentation**: Consistent indentation for nested blocks
-   **Spacing**: Proper spacing around operators and keywords
-   **Alignment**: Parameter alignment in multi-line declarations
-   **Line breaking**: Intelligent line breaking for long statements
-   **SQL formatting**: Special handling for embedded SQL queries

### Completion Provider

Provides auto-completion for SSL language elements:

-   **Keywords**: SSL keywords like `:PROCEDURE`, `:IF`, `:WHILE`
-   **Functions**: Built-in SSL functions and procedures
-   **Operators**: Logical operators like `.AND.`, `.OR.`, `.T.`, `.F.`
-   **Snippets**: Common SSL code patterns and constructs

**Trigger Characters:**

-   `:` - SSL keywords
-   `(` - Function calls
-   `.` - Logical operators

### Hover Provider

Provides documentation and help information:

-   **Keyword documentation**: Help for SSL keywords and constructs
-   **Function signatures**: Parameter information for SSL functions
-   **Type information**: Data type details and usage examples
-   **Best practices**: SSL coding guidelines and recommendations

### Document Symbol Provider

Provides code outline and navigation:

-   **Procedures**: Function and procedure definitions
-   **Classes**: Class declarations and methods
-   **Variables**: Variable declarations
-   **Labels**: Label definitions for branching
-   **Regions**: Region markers for code organization

### Diagnostics Provider

Provides error detection and validation:

-   **Syntax errors**: Invalid SSL syntax detection
-   **Semantic errors**: Type mismatches and undefined references
-   **Bracket matching**: Unmatched SSL keyword pairs
-   **Best practice warnings**: Code quality suggestions
-   **EBNF compliance**: Grammar rule validation

### Bracket Matching Provider

Provides bracket and keyword pair matching:

-   **SSL keywords**: Matching pairs like `:IF`/`:ENDIF`
-   **Standard brackets**: `()`, `[]`, `{}`
-   **Navigation**: Jump to matching bracket/keyword
-   **Highlighting**: Visual indication of matching pairs

## Commands

The provider registry automatically registers these SSL-specific commands:

### `ssl-formatter.showBracketInfo`

Shows information about all bracket pairs in the current SSL document.

### `ssl-formatter.showBracketInfoAtCursor`

Shows bracket information at the current cursor position.

### `ssl-formatter.findMatchingBracket`

Navigates to the matching bracket or SSL keyword pair.

## Event Listeners

The registry sets up automatic document event listeners:

-   **`onDidChangeTextDocument`**: Updates diagnostics on document changes (debounced)
-   **`onDidOpenTextDocument`**: Analyzes SSL documents when opened
-   **`onDidCloseTextDocument`**: Cleans up diagnostics when documents are closed

## Configuration Options

### Document Selector

The registry uses a document selector to identify SSL files:

```typescript
{ scheme: "file", language: "ssl" }
```

This matches files with the language ID "ssl" in the file system.

### Registration Modes

1. **Register All** (default): Registers all available providers
2. **Selective**: Registers only specified providers
3. **Custom**: Allows complete customization of registration

## Best Practices

### Extension Activation

1. Always use the singleton instance
2. Register providers early in activation
3. Add all disposables to extension context
4. Handle registration errors gracefully

### Extension Deactivation

1. Always call `unregisterProviders()` in deactivate
2. Let VS Code handle disposable cleanup
3. Log deactivation for debugging

### Error Handling

```typescript
try {
    const registration = registry.registerProviders(config);
    context.subscriptions.push(...registration.disposables);
} catch (error) {
    console.error("Failed to register SSL providers:", error);
    vscode.window.showErrorMessage("SSL language support failed to activate");
}
```

### Performance Considerations

1. Use selective registration for lightweight extensions
2. Consider user preferences for optional providers
3. Debounce diagnostics updates to avoid excessive processing
4. Cache provider instances when possible

## Troubleshooting

### Common Issues

1. **Providers not working**: Check language ID matches "ssl"
2. **Double registration error**: Ensure `unregisterProviders()` is called before re-registering
3. **Missing features**: Verify all required providers are registered
4. **Performance issues**: Consider selective registration or debouncing

### Debugging

Enable debug logging to troubleshoot issues:

```typescript
console.log("Registering SSL providers...");
const registration = registry.registerProviders(config);
console.log("Registered providers:", Object.keys(registration.providers));
```

## Integration with SSL EBNF Grammar

The provider registry ensures full compliance with the SSL EBNF grammar by:

1. **Supporting all language constructs**: Every grammar rule is supported by appropriate providers
2. **Maintaining consistency**: All providers use the same tokenizer and parser
3. **Following conventions**: SSL-specific naming and formatting conventions are enforced
4. **Providing comprehensive coverage**: From basic syntax to advanced features

## Migration from Direct Registration

If migrating from direct provider registration, follow these steps:

1. Replace individual provider imports with registry import
2. Remove manual provider instantiation
3. Replace individual registrations with registry call
4. Update disposal logic to use registry cleanup
5. Test all functionality to ensure compatibility

### Before (Direct Registration)

```typescript
const foldingProvider = new SSLFoldingRangeProvider();
const foldingDisposable = vscode.languages.registerFoldingRangeProvider(selector, foldingProvider);
context.subscriptions.push(foldingDisposable);
```

### After (Registry)

```typescript
const registry = SSLProviderRegistry.getInstance();
const registration = registry.registerProviders(SSLProviderRegistry.getDefaultConfig());
context.subscriptions.push(...registration.disposables);
```

## Future Enhancements

The provider registry system is designed to be extensible:

1. **Additional providers**: New language service providers can be easily added
2. **Configuration options**: More granular configuration options
3. **Performance optimization**: Lazy loading and caching improvements
4. **Multi-language support**: Support for SSL variants or related languages
5. **Advanced diagnostics**: Enhanced error detection and suggestions
