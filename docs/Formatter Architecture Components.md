## Formatter Architecture Components

### 1. Core Infrastructure Components

#### 1.1 Formatter Visitor Base

**File**: `src/formatter/visitor.ts`

-   Abstract base visitor class implementing the visitor pattern
-   Core traversal logic for AST nodes
-   State management (output builder, indentation level, options)

#### 1.2 Formatter Options

**File**: `src/formatter/options.ts`

-   Configuration interface for formatting rules
-   Default formatting options
-   User-configurable settings (indent size, tabs vs spaces, line length)

#### 1.3 Output Builder

**File**: `src/formatter/outputBuilder.ts`

-   Efficient string building with indentation management
-   Methods for writing lines with proper indentation
-   Blank line management

### 2. Node-Specific Formatters

#### 2.1 Declaration Formatters

**File**: `src/formatter/declarations.ts`

-   Format `:DECLARE` statements
-   Format `:PARAMETERS` statements
-   Format `:DEFAULT` statements
-   Format `:PUBLIC` statements
-   Format `:INCLUDE` statements

#### 2.2 Control Flow Formatters

**File**: controlFlow.ts

-   Format `:IF`/`:ELSE`/`:ENDIF` blocks
-   Format `:WHILE`/`:ENDWHILE` loops
-   Format `:FOR`/`:NEXT` loops
-   Format exit statements (`:EXITWHILE`, `:EXITFOR`)

#### 2.3 Procedure Formatters

**File**: `src/formatter/procedures.ts`

-   Format `:PROCEDURE`/`:ENDPROC` blocks
-   Handle parameter lists and default parameters
-   Manage procedure body indentation

#### 2.4 Expression Formatters

**File**: `src/formatter/expressions.ts`

-   Format binary expressions with proper spacing
-   Format unary expressions
-   Format property access (`:` operator)
-   Format array access and subscripts

#### 2.5 Literal Formatters

**File**: `src/formatter/literals.ts`

-   Format string literals (preserve quotes)
-   Format number literals (including scientific notation)
-   Format boolean literals (`.T.`, `.F.`)
-   Format array literals with proper spacing
-   Format date literals

### 3. Specialized Formatters

#### 3.1 Comment Formatter

**File**: `src/formatter/comments.ts`

-   Handle block comments (`/* comment ;`)
-   Handle single-line comments
-   Handle region comments (`/* region`, `/* endregion`)
-   Comment association and placement logic

#### 3.2 Class Formatter

**File**: `src/formatter/classes.ts`

-   Format `:CLASS` declarations
-   Format `:INHERIT` statements
-   Format class members and methods

#### 3.3 Error Handling Formatters

**File**: `src/formatter/errorHandling.ts`

-   Format `:TRY`/`:CATCH`/`:FINALLY`/`:ENDTRY` blocks
-   Format `:ERROR` blocks
-   Proper indentation for error handling structures

#### 3.4 Switch Case Formatters

**File**: `src/formatter/switchCase.ts`

-   Format `:BEGINCASE`/`:ENDCASE` blocks
-   Format `:CASE` statements
-   Format `:OTHERWISE` blocks
-   Format `:EXITCASE` statements

#### 3.5 SQL Formatters

**File**: `src/formatter/sql.ts`

-   Format `SqlExecute` calls
-   Format `LSearch` calls
-   Handle SQL parameter formatting (`?param?`, `?`)

### 4. Utility Components

#### 4.1 Spacing Manager

**File**: `src/formatter/spacing.ts`

-   Rules for spacing around operators
-   Rules for spacing in lists (commas, parameters)
-   Handle special cases like skipped parameters (`param1,,param3`)

#### 4.2 Line Breaking Manager

**File**: `src/formatter/lineBreaking.ts`

-   Determine when to break long lines
-   Handle line breaking in function calls
-   Handle line breaking in expressions

#### 4.3 Comment Association

**File**: `src/formatter/commentAssociation.ts`

-   Associate comments with AST nodes
-   Determine leading vs trailing comments
-   Preserve comment positioning

### 5. Main Formatter Entry Point

#### 5.1 SSL Formatter

**File**: index.ts

-   Main `SSLFormatter` class
-   Public API for formatting SSL code
-   Integration with VS Code formatting provider
-   Orchestrates the visitor pattern execution

## Suggested Implementation Order

I recommend implementing these components in the following order:

1. **Core Infrastructure** (1.1-1.3) - Foundation for all formatting
2. **Expression Formatters** (2.4) - Basic expression handling
3. **Literal Formatters** (2.5) - Simple literal formatting
4. **Declaration Formatters** (2.1) - Basic statement formatting
5. **Spacing Manager** (4.1) - Essential for consistent formatting
6. **Control Flow Formatters** (2.2) - Core language structures
7. **Procedure Formatters** (2.3) - Function/procedure handling
8. **Comment Formatter** (3.1) - Comment preservation
9. **Error Handling Formatters** (3.3) - Exception handling
10. **Switch Case Formatters** (3.4) - Case statement handling
11. **Class Formatter** (3.2) - Object-oriented features
12. **SQL Formatters** (3.5) - Database integration
13. **Line Breaking Manager** (4.2) - Advanced formatting
14. **Comment Association** (4.3) - Advanced comment handling
15. **Main Formatter** (5.1) - Integration and public API
