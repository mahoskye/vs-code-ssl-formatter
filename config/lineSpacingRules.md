# VS Code Line Spacing Formatter Generation Prompt

You will assist me in creating a TypeScript module for VS Code line spacing formatting. I will provide rules in this markdown format:

```markdown
[Rule category]:

1. [Rule description]
2. [Rule description]
   ...
```

## Interaction Guidelines

1. Wait for explicit instructions before proceeding with each step
2. Only provide code or implementation details that are specifically requested
3. Ask for clarification if the request is ambiguous
4. Do not attempt to generate the entire plugin at once

## Development Process

For each development task, we will:

1. First, analyze the provided rule set and identify:

    - Rule categories (e.g., block spacing, inline spacing)
    - Required whitespace patterns
    - Conditional logic requirements
    - Edge cases and exceptions

2. Create the following TypeScript interfaces based on the rule analysis:

    - Rule configuration interfaces
    - Line context interfaces for pattern matching
    - Any helper interfaces needed for complex patterns

3. Implement the LineSpacingFormatter function with this signature:

```typescript
export function LineSpacingFormatter(text: string): string;
```

4. Break down the implementation into these components:

    - Line parser to extract context and patterns
    - Rule matcher to determine which rules apply
    - Spacing calculator to determine required whitespace
    - Edit generator to create VS Code TextEdit objects

5. Include error handling for:

    - Invalid document states
    - Conflicting rules
    - Performance optimization for large files

6. Add documentation for:
    - Each interface and type
    - The main formatter function
    - Rule resolution logic
    - Example usage scenarios

Each phase will be implemented only when explicitly requested, and we will focus on one component at a time.

FUNDAMENTAL RULES:

1. All keyword matching must be case insensitive
2. No more than two consecutive blank lines anywhere in file
3. File must NOT begin with a blank line
4. File must end with exactly one blank line

DECLARATION SPACING:

1. No blank lines between consecutive declarations of same type unless developer has explicitly separated them
2. One blank line between different declaration types
3. Two blank lines after last declaration before main code begins

PROCEDURE BLOCK SPACING:

1. Two blank lines before `:PROCEDURE` declaration
2. One blank line after `:PROCEDURE` declaration
3. One blank line before `:ENDPROC`
4. Two blank lines after `:ENDPROC`

ERROR BLOCK SPACING:

1. Two blank lines before `:ERROR` block
2. One blank line after `:ERROR` declaration
3. One blank line before `:RESUME`
4. One blank line after `:RESUME`

REGION SPACING:

1. One blank line before `/* region` or `/*region` comments
2. One blank line after `/* region` or `/*region` comments
3. One blank line before `/* endregion` or `/*endregion` comments
4. Two blank lines after `/* endregion` or `/*endregion` comments

CONTROL STRUCTURE SPACING:

1. One blank line after `:BEGINCASE`
2. One blank line before `:ENDCASE`
3. No blank line after `:CASE` or `:OTHERWISE` statements when followed by code
4. One blank line before and after `:EXITCASE`
5. One blank line before `:ELSE` or `:ENDIF`

COMMENT SPACING:

1. One blank line before any comment
2. Single line comments (starting with `/*`) should be followed by one blank line unless:
    - They are directly followed by a related code statement
    - They are part of a consecutive comment block
3. Block comments (starting with `/**`) must maintain their exact spacing and must be followed by two blank lines
4. Comments that describe a following code block should not have a blank line after

STATEMENT BLOCK SPACING:

1. One blank line after statement that ends a logical block
2. No blank line between tightly coupled statements
3. One blank line before and after SQL statements
4. One blank line between unrelated statement groups
5. One blank line before and after blocks of related function calls
6. One blank line before early `:RETURN` statements (returns not at end of procedure)
7. Two blank lines after early `:RETURN` statements

PROCESSING ORDER:

1. Preserve block comments (starting with `/**`)
2. Remove any leading blank lines
3. Remove excess blank lines (> 2)
4. Apply region spacing
5. Apply procedure block spacing
6. Apply error block spacing
7. Apply control structure spacing
8. Apply declaration block spacing
9. Apply statement block spacing
10. Apply comment line spacing
11. Ensure single ending blank line
12. Final check for max two consecutive blank lines

Notes:

-   Everything up to a semicolon `;` is considered part of the same code block/statement
-   Comments begin with `/*` and end at a semicolon `;`
-   Block comments begin with `/**` and should never be modified
-   A developer is allowed up to two blank lines to help separate logic groups
