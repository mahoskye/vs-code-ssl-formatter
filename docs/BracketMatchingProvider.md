# SSL Bracket Matching Provider

The SSL Bracket Matching Provider implements comprehensive bracket matching for SSL (STARLIMS Scripting Language) keyword pairs based on the official EBNF grammar specification.

## Features

### SSL Keyword Pair Matching

The provider supports matching for all SSL keyword pairs defined in the EBNF grammar:

#### Control Flow Structures

-   `:PROCEDURE` ↔ `:ENDPROC` - Procedure definitions
-   `:IF` ↔ `:ENDIF` - Conditional statements (with `:ELSE` support)
-   `:WHILE` ↔ `:ENDWHILE` - While loops
-   `:FOR` ↔ `:NEXT` - For loops
-   `:BEGINCASE` ↔ `:ENDCASE` - Switch/case statements (with `:CASE` and `:OTHERWISE`)

#### Error Handling

-   `:TRY` ↔ `:ENDTRY` - Try-catch blocks (with `:CATCH` and `:FINALLY`)
-   `:ERROR` - Error handling blocks (no explicit end keyword)

#### Code Organization

-   `:REGION` ↔ `:ENDREGION` - Code regions
-   `:BEGININLINECODE` ↔ `:ENDINLINECODE` - Inline code blocks
-   `:CLASS` ↔ End of script - Class definitions (classes end with the script)

#### Comment Regions

-   `/* region */` ↔ `/* endregion */` - Comment-based regions

### Standard Bracket Support

The provider also handles standard programming brackets:

-   `()` - Parentheses for function calls and expressions
-   `[]` - Square brackets for array access
-   `{}` - Curly braces for array literals and code blocks
-   `{||}` - Code block literals

## Commands

The extension provides several commands for bracket matching:

### `ssl-formatter.showBracketInfo`

Shows all bracket pairs in the current SSL document with their line positions.

### `ssl-formatter.showBracketInfoAtCursor`

Shows bracket information for the bracket pair at the current cursor position.

### `ssl-formatter.findMatchingBracket`

Jumps the cursor to the matching bracket/keyword for the current position.

## Features

### Intelligent Position Matching

The provider uses intelligent position matching that:

-   Recognizes when the cursor is anywhere on an SSL keyword (e.g., anywhere on `:PROCEDURE`)
-   Handles multi-character keywords correctly
-   Supports both opening and closing keywords

### Comprehensive Validation

The provider validates bracket pairs and reports:

-   Unmatched opening keywords
-   Unmatched closing keywords
-   Misplaced intermediate keywords (e.g., `:ELSE` without `:IF`)
-   Context validation for keywords that must appear within specific blocks

### Nested Structure Support

The provider correctly handles:

-   Nested control structures
-   Mixed bracket types
-   Complex nested expressions
-   Class methods within class definitions

### Error Diagnostics

Real-time diagnostics provide:

-   Error highlighting for unmatched brackets
-   Context-aware error messages
-   Suggestions for proper keyword placement

## Usage Examples

### Basic Procedure Matching

```ssl
:PROCEDURE MyProc;     ← Opening keyword
    /* procedure body */
:ENDPROC;              ← Matching closing keyword
```

### Nested Structures

```ssl
:PROCEDURE ComplexProc;
    :IF condition;     ← Opening IF
        :WHILE loop;   ← Nested WHILE
            /* content */
        :ENDWHILE;     ← Matching ENDWHILE
    :ELSE;             ← Intermediate keyword
        /* else branch */
    :ENDIF;            ← Matching ENDIF
:ENDPROC;              ← Matching ENDPROC
```

### Class Definitions

```ssl
:CLASS MyClass;        ← Class begins
    :PROCEDURE Method;
        /* method body */
    :ENDPROC;
/* End of script */    ← Class ends with script
```

### Comment Regions

```ssl
/* region Helper Functions */
    /* helper code */
/* endregion */
```

## Configuration

The bracket matching provider works automatically with SSL files and requires no configuration. It integrates with:

-   VS Code's built-in bracket matching highlighting
-   The extension's diagnostic system
-   The folding range provider for code folding

## Technical Implementation

### Architecture

-   Uses the SSL tokenizer for accurate token recognition
-   Implements a stack-based algorithm for bracket pair matching
-   Provides comprehensive validation with detailed error reporting
-   Handles SSL-specific syntax like class definitions that end with the script

### Performance

-   Efficient tokenization and parsing
-   Incremental updates on document changes
-   Optimized for large SSL files
-   Minimal memory footprint

### EBNF Compliance

The implementation strictly follows the SSL EBNF grammar specification:

-   All keyword pairs defined in the grammar are supported
-   Intermediate keywords are properly handled
-   Special cases like class definitions are implemented according to spec
-   Comment syntax follows SSL conventions

## Integration

The bracket matching provider integrates seamlessly with other extension features:

-   **Folding Provider**: Shares bracket pair information for code folding
-   **Formatter**: Uses bracket structure for proper indentation
-   **Diagnostics**: Provides real-time validation of bracket pairs
-   **Language Server**: Could be extended to provide more advanced features

## Error Handling

The provider is designed to be robust:

-   Handles malformed SSL code gracefully
-   Provides meaningful error messages
-   Continues to work even with syntax errors in the document
-   Offers fallback behavior for edge cases

## Future Enhancements

Planned improvements include:

-   Integration with VS Code's bracket pair colorization
-   Advanced navigation commands (go to next/previous bracket pair)
-   Semantic bracket matching (understanding procedure calls)
-   Enhanced diagnostics with fix suggestions
