# SSL Style Guide Test Fixtures

This directory contains comprehensive test fixtures for the STARLIMS SSL Style Guide. Each test fixture is a pair of files:

- `*-bad.ssl` - Source code containing style guide violations
- `*-expected.ssl` - Expected output after formatting according to the style guide

## Test Coverage

**Total Fixtures:** 27 test rules across 14 categories

### Categories

#### Formatting (3 tests)
- **01-formatting-indentation** - Tab indentation rules
- **02-formatting-spacing-around-operators** - Spacing around operators (`:=`, `+`, `-`, `*`, `/`, `==`, etc.)
- **03-formatting-spacing-after-comma** - Spaces after commas in parameters and arrays
- **04-formatting-blank-lines** - Blank lines between procedures and blocks

#### Keywords (1 test)
- **05-keywords-case** - Keywords must be UPPERCASE with `:` prefix

#### Naming Conventions (3 tests)
- **06-naming-hungarian-notation** - Hungarian notation prefixes (s, n, b, a, d, o, l)
- **07-naming-procedure-pascal-case** - Procedure names in PascalCase
- **08-naming-variable-camel-case** - Variable names in camelCase with Hungarian prefix

#### Control Flow (4 tests)
- **09-control-flow-if-else** - IF/ELSE block indentation and structure
- **10-control-flow-for-loop** - FOR loop indentation
- **11-control-flow-while-loop** - WHILE loop indentation
- **12-control-flow-case-statement** - CASE/BEGIN CASE indentation

#### Error Handling (1 test)
- **13-error-handling-try-catch** - TRY/CATCH/FINALLY structure and indentation

#### Statements (1 test)
- **14-statements-one-per-line** - One statement per line rule

#### Comments (1 test)
- **15-comments-block-style** - Block comment style with semicolon terminator

#### Function Calls (2 tests)
- **16-function-calls-no-space-before-paren** - No space before function call parentheses
- **17-function-calls-builtin-pascal-case** - Built-in functions in PascalCase

#### Object Access (1 test)
- **18-object-property-access-no-space** - No spaces around colon in object:property syntax

#### Operators & Expressions (2 tests)
- **19-logical-operators-spacing** - Spacing around .AND., .OR., .NOT.
- **20-arrays-literal-syntax** - Array literal spacing

#### String Operations (1 test)
- **21-string-concatenation-explicit-plus** - Use explicit `+` operator for string concatenation

#### Classes (1 test)
- **23-classes-structure** - Class definition structure and member indentation

#### Documentation (1 test)
- **24-documentation-header** - Standard procedure documentation header format

#### Module Structure (2 tests)
- **25-module-structure** - Directives order (Parameters -> Defaults -> Declarations)
- **26-declarations-formatting** - Formatting of declaration keywords and lists

#### SQL (2 tests)
- **27-sql-formatting-select** - Formatting of SELECT statements in SQLExecute
- **28-sql-formatting-update** - Formatting of UPDATE statements in RunSQL

## Testing

To generate a report of all fixtures:

```bash
node out/tests/fixtures-loader.js
```

To generate markdown documentation:

```bash
node out/tests/fixtures-loader.js --markdown
```

## Running Tests

```bash
npm test
```

Tests validate that:
1. Both `-bad` and `-expected` files exist for each test
2. Both files contain valid SSL code
3. The `-bad` file contains style violations
4. The `-expected` file represents the properly formatted version

## Style Guide Reference

All fixtures validate compliance with the SSL Style Guide defined in:
- `dev docs/ssl-style-guide/ssl-style-guide.schema.yaml`

### Key Style Guide Rules

| Rule | Details |
|------|---------|
| **Indentation** | Use tabs (1 per level) |
| **Line Length** | 90 character max |
| **Keywords** | UPPERCASE with `:` prefix |
| **Functions** | PascalCase for builtins |
| **Variables** | camelCase with Hungarian prefix |
| **Procedures** | PascalCase |
| **Statements** | One per line, terminated with `;` |
| **Comments** | Block style: `/* comment; */` |
| **Operators** | Spaces around: `:=`, `+`, `-`, `*`, `/`, `==` |
| **Commas** | Space after (except before ``) |
| **Blank Lines** | One between top-level blocks |

## Adding New Tests

To add a new test fixture:

1. Create a pair of files following the naming pattern:
   - `NN-category-rule-bad.ssl` (with style violations)
   - `NN-category-rule-expected.ssl` (formatted correctly)

2. Where `NN` is a sequential number (01, 02, 03, ...)

3. Update this README to document the new test

Example:

```bash
# Create the bad version with violations
touch 99-custom-category-my-rule-bad.ssl

# Create the expected version (properly formatted)
touch 99-custom-category-my-rule-expected.ssl
```

## Fixture Statistics

- Total Test Rules: 21
- Total Categories: 10
- Average Bad File Size: 218 bytes
- Average Expected File Size: 225 bytes
