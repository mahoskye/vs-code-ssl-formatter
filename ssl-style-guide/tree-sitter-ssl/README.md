# Tree-sitter SSL (STARLIMS Scripting Language)

**Status:** Complete grammar for SSL v11 (parses all major SSL statements, expressions, comments, and language constructs). Based on comprehensive analysis of SSL documentation, style guides, and language specifications.

## Complete Language Coverage

- **Colon-prefixed keywords** like `:IF`, `:DECLARE`, `:PROCEDURE`, etc., are case-insensitive and must end their statements with a semicolon `;`.
- **Comments** begin with `/*` and end at the next semicolon `;` (faithful to SSL syntax).
- **Strings** support `"double"`, `'single'`, and bracketed `[text]` literals, with database parameter patterns `?param?`.
- **Numbers** allow integers, decimals, and scientific notation (following SSL v11 rules).
- **Arrays** use `{ ... }`; indexers support `[i]`, `[i,j]`, and chained forms with 1-based indexing.
- **Object access** uses `object:property` and `object:method(args)`.
- **Control structures** include `:IF/:ELSE/:ENDIF`, `:WHILE/:ENDWHILE`, `:FOR ... :NEXT`, `:BEGINCASE/:CASE/:OTHERWISE/:ENDCASE`, and structured exceptions `:TRY/:CATCH/:FINALLY/:ENDTRY`.
- **Loop control** includes `:EXITFOR`, `:EXITWHILE`, `:LOOP`, and `:RESUME` statements.
- **Procedures and classes** with full parameter handling, default values, and inheritance support.
- **Database integration** with parameterized queries and function call patterns.

## Files and Structure

```
tree-sitter-ssl/
├── grammar.js              → main Tree-sitter grammar
├── package.json            → package metadata
├── queries/
│   ├── highlights.scm      → syntax highlighting captures
│   ├── locals.scm          → local variable scope detection
│   └── injections.scm      → SQL highlighting within strings
└── README.md               → this file
```

## Query Files

- **highlights.scm**: Main syntax highlighting for SSL constructs
- **locals.scm**: Local variable and parameter scope detection
- **injections.scm**: SQL code highlighting within database strings

## Getting Started

```bash
npm i
npm run gen
```

Use `tree-sitter parse` on SSL files to validate parsing. The grammar is based on the complete EBNF specification and should handle all standard SSL constructs.

## Grammar Validation

This grammar has been validated against:
- Complete EBNF grammar specification
- SSL v11 syntax rules
- STARLIMS v10 Code Convention and Style Guide
- Keyword and operator lists
- Number and string format specifications

## Known Implementation Notes

- **Database parameters**: Strings can contain `?param?` patterns for database parameterization
- **Scientific notation**: Follows strict SSL rules (requires decimal point for e-notation)
- **Hungarian notation**: Variable naming conventions are preserved in parsing
- **Block structure**: All control structures require proper termination with keywords
- **Comments**: All comments must be terminated with semicolon (SSL-specific)

## Testing

Test with real SSL scripts to ensure proper parsing of:
- Complex procedure definitions with parameters and defaults
- Class inheritance and method definitions
- Database function calls with parameterized queries
- Nested control structures and error handling
- Array operations and object property access