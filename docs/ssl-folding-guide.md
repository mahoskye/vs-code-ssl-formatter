# SSL Code Folding Documentation

This document describes the comprehensive code folding functionality implemented for the SSL (STARLIMS Scripting Language) VS Code extension.

## Overview

The SSL folding provider supports all major language constructs defined in the SSL EBNF grammar, enabling users to collapse and expand code sections for better readability and navigation.

## Supported Folding Constructs

### 1. Regions

#### Keyword Regions
```ssl
:REGION RegionName;
    /* Code content */
:ENDREGION;
```

#### Comment Regions
```ssl
/* region Region Name;
    /* Code content */
/* endregion;
```

### 2. Procedures and Classes

#### Procedure Definitions
```ssl
:PROCEDURE ProcedureName;
:PARAMETERS param1, param2;
    /* Procedure body */
:ENDPROC;
```

#### Class Definitions
```ssl
:CLASS ClassName;
:INHERIT BaseClass;
    /* Class members and methods */
```

### 3. Control Structures

#### Conditional Statements
```ssl
:IF condition;
    /* True branch */
:ELSE;
    /* False branch */
:ENDIF;
```

#### Loops

**For Loops:**
```ssl
:FOR variable := start :TO end;
    /* Loop body */
:NEXT;
```

**While Loops:**
```ssl
:WHILE condition;
    /* Loop body */
:ENDWHILE;
```

### 4. Switch Statements

```ssl
:BEGINCASE;
:CASE condition1;
    /* Case 1 */
:CASE condition2;
    /* Case 2 */
:OTHERWISE;
    /* Default case */
:ENDCASE;
```

### 5. Error Handling

#### Structured Exception Handling
```ssl
:TRY;
    /* Code that might fail */
:CATCH;
    /* Error handling */
:FINALLY;
    /* Cleanup code */
:ENDTRY;
```

#### Legacy Error Handling
```ssl
:ERROR;
    /* Error handling code */
```

### 6. Inline Code Blocks

```ssl
:BEGININLINECODE "BlockName";
    /* Inline code content */
:ENDINLINECODE;
```

### 7. Comments

Multi-line comment blocks are automatically folded:
```ssl
/* This is a multi-line comment
   that spans several lines
   and will be folded;
```

## Advanced Features

### 1. Nested Structure Support

The folding provider correctly handles deeply nested structures:

```ssl
:PROCEDURE ComplexProcedure;
    :IF condition;
        :FOR i := 1 :TO 10;
            :BEGINCASE;
            :CASE i = 1;
                :TRY;
                    /* Nested try block */
                :CATCH;
                    /* Nested catch */
                :ENDTRY;
            :OTHERWISE;
                /* Default case */
            :ENDCASE;
        :NEXT;
    :ENDIF;
:ENDPROC;
```

### 2. String Delimiter Handling

The folding provider correctly ignores SSL keywords inside strings, supporting all three SSL string delimiter types:

- Double quotes: `"string content with :IF keyword"`
- Single quotes: `'string content with :PROCEDURE keyword'`
- Square brackets: `[string content with :WHILE keyword]`

### 3. Complex Control Flow

#### Multiple ELSE Statements
```ssl
:IF condition1;
    /* Branch 1 */
:ELSE;
    :IF condition2;
        /* Branch 2 */
    :ELSE;
        /* Default branch */
    :ENDIF;
:ENDIF;
```

#### Case Transitions
Individual `:CASE` and `:OTHERWISE` blocks within switch statements are properly folded:

```ssl
:BEGINCASE;
:CASE value = 1;
    /* This case block folds */
    DoSomething();
    DoSomethingElse();
:CASE value = 2;
    /* This case block also folds */
    DoAnotherThing();
:OTHERWISE;
    /* Default case folds too */
    DoDefaultAction();
:ENDCASE;
```

### 4. Try-Catch-Finally Complexity

The provider correctly handles the complex relationships in error handling:

- The `:TRY` block folds from `:TRY` to the line before `:CATCH`
- The `:CATCH` block folds from `:CATCH` to the line before `:FINALLY` (if present) or `:ENDTRY`
- The `:FINALLY` block folds from `:FINALLY` to the line before `:ENDTRY`
- The entire structure is also foldable as one unit

## Implementation Details

### Stack-Based Parsing

The folding provider uses a stack-based approach to track nested structures:

1. **Push Context**: When a fold-start keyword is encountered, a context is pushed onto the stack
2. **Pop Context**: When a fold-end keyword is encountered, the matching context is popped
3. **Special Handling**: Intermediate keywords (like `:CATCH`, `:ELSE`) are handled with special logic

### String State Tracking

The provider maintains state for string literals to avoid false positives:

- Tracks whether currently inside a string
- Remembers the delimiter type (", ', or ])
- Handles string escapes and multi-line strings correctly

### Comment Handling

Comments are processed with special consideration for:

- Multi-line comment blocks
- Region markers within comments
- Proper termination with semicolons

## Performance Considerations

The folding provider is optimized for large files:

1. **Cancellation Support**: Respects VS Code's cancellation tokens for large files
2. **Single Pass**: Processes the entire file in a single pass
3. **Efficient Stack Operations**: Uses targeted stack searches for complex structures
4. **Minimal Memory Footprint**: Stores only essential context information

## Testing

The folding functionality has been tested with:

1. **Simple Structures**: Basic procedures, classes, and control statements
2. **Complex Nesting**: Deeply nested control structures with multiple levels
3. **Edge Cases**: String handling, comment regions, and malformed code
4. **Performance**: Large files with thousands of lines
5. **Real-world Code**: Actual SSL scripts from production environments

## Future Enhancements

Potential improvements for future versions:

1. **Semantic Folding**: Fold related statements based on semantic analysis
2. **Custom Fold Markers**: Support for user-defined folding regions
3. **Fold Persistence**: Remember fold states across VS Code sessions
4. **Smart Folding**: Automatic folding of certain patterns (e.g., long parameter lists)

## Troubleshooting

### Common Issues

1. **Incomplete Folding**: Ensure all block structures have proper end markers
2. **False Positives**: Check for keywords inside string literals
3. **Performance**: For very large files, consider breaking them into smaller modules

### Debug Mode

To enable debug logging for the folding provider, add this to your VS Code settings:

```json
{
    "ssl.folding.debug": true
}
```

This will output detailed information about fold detection to the developer console.
