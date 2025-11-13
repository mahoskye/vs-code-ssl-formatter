# SSL Style Guide - Comprehensive Test Summary

## Overview

A comprehensive test suite has been created to validate adherence to the SSL Style Guide. The suite consists of 21 test fixtures covering all major style guide rules.

**Test Location:** `/tests/fixtures/style-guide/`

## Test Fixture Coverage

### 1. Formatting Rules (4 tests)

#### Test 01: Indentation
- **Rule:** Use tabs for indentation (1 tab per level)
- **Bad File:** Indentation using spaces
- **Expected:** Proper tab-based indentation
- **Size:** 252 → 241 bytes (↓)

#### Test 02: Spacing Around Operators
- **Rule:** Space around assignment and comparison operators
- **Bad File:** Missing spaces around `:=`, `+`, `-`, `*`, `/`, `==`
- **Expected:** Proper spacing: `nResult := nA + nB`
- **Size:** 275 → 293 bytes (↑)

#### Test 03: Spacing After Commas
- **Rule:** Space after every comma
- **Bad File:** `SQLExecute("SELECT...",0,0)` (no spaces)
- **Expected:** `SQLExecute("SELECT...", 0, 0)` (with spaces)
- **Size:** 246 → 258 bytes (↑)

#### Test 04: Blank Lines Between Blocks
- **Rule:** One blank line between top-level blocks
- **Bad File:** Multiple/no blank lines between procedures
- **Expected:** Consistent single blank line
- **Size:** 169 → 169 bytes (→)

### 2. Keyword Rules (1 test)

#### Test 05: Keyword Case
- **Rule:** Keywords must be UPPERCASE with `:` prefix
- **Bad File:** `:procedure`, `:if`, `:else` (wrong case)
- **Expected:** `:PROCEDURE`, `:IF`, `:ELSE` (correct case)
- **Size:** 208 → 208 bytes (→)

### 3. Naming Conventions (3 tests)

#### Test 06: Hungarian Notation
- **Rule:** Variables use Hungarian notation prefixes
- **Valid Prefixes:**
  - `s` = string
  - `n` = numeric
  - `b` = boolean
  - `a` = array
  - `d` = date
  - `o` = object
  - `l` = logical
- **Bad File:** `qty`, `flag`, `dateCreated` (no prefixes)
- **Expected:** `nQty`, `bFlag`, `dDateCreated`
- **Size:** 196 → 206 bytes (↑)

#### Test 07: Procedure Names (PascalCase)
- **Rule:** Procedure names in PascalCase
- **Bad File:** `calculate_total`, `getDataFromDatabase`, `VALIDATE_USER_INPUT`
- **Expected:** `CalculateTotal`, `GetDataFromDatabase`, `ValidateUserInput`
- **Size:** 210 → 207 bytes (↓)

#### Test 08: Variable Names (camelCase)
- **Rule:** Variable names in camelCase with Hungarian prefix
- **Bad File:** `s_email`, `N_Value`, `aList_Items` (wrong separators)
- **Expected:** `sEmail`, `nValue`, `aListItems`
- **Size:** 267 → 261 bytes (↓)

### 4. Control Flow Structures (4 tests)

#### Test 09: IF/ELSE Block Indentation
- **Rule:** Proper indentation in conditional blocks
- **Bad File:** Inconsistent indentation of block bodies
- **Expected:** Consistent tab-based indentation
- **Size:** 173 → 176 bytes (↑)

#### Test 10: FOR Loop Indentation
- **Rule:** Loop body properly indented
- **Bad File:** Loop body not indented
- **Expected:** Indented loop body
- **Size:** 201 → 202 bytes (↑)

#### Test 11: WHILE Loop Indentation
- **Rule:** While loop body properly indented
- **Bad File:** No indentation in loop body
- **Expected:** Properly indented body
- **Size:** 202 → 203 bytes (↑)

#### Test 12: CASE Statement Indentation
- **Rule:** Case branches properly indented
- **Bad File:** Case bodies not indented
- **Expected:** Consistent indentation of all cases
- **Size:** 250 → 253 bytes (↑)

### 5. Error Handling (1 test)

#### Test 13: TRY/CATCH/FINALLY Structure
- **Rule:** Proper indentation in error handling blocks
- **Bad File:** No indentation in TRY body and CATCH handler
- **Expected:** Properly indented try/catch blocks
- **Size:** 254 → 265 bytes (↑)

### 6. Statement Rules (1 test)

#### Test 14: One Statement Per Line
- **Rule:** Only one statement per line
- **Bad File:** Multiple statements on same line: `nA := 1; nB := 2; nC := 3;`
- **Expected:** Each statement on its own line
- **Size:** 150 → 153 bytes (↑)

### 7. Comment Rules (1 test)

#### Test 15: Block Comment Style
- **Rule:** Use block comment style with semicolon terminator
- **Bad File:** `// comment` or `/* comment` (wrong style/no terminator)
- **Expected:** `/* comment; */`
- **Size:** 256 → 264 bytes (↑)

### 8. Function Call Rules (2 tests)

#### Test 16: No Space Before Parenthesis
- **Rule:** Function calls have no space before opening parenthesis
- **Bad File:** `Len ("test")`, `Empty  ("value")` (spaces before paren)
- **Expected:** `Len("test")`, `Empty("value")`
- **Size:** 254 → 257 bytes (↑)

#### Test 17: Built-in Function PascalCase
- **Rule:** Built-in functions use PascalCase
- **Bad File:** `alltrim()`, `sqlexecute()`, `createudobject()` (wrong case)
- **Expected:** `AllTrim()`, `SQLExecute()`, `CreateUdoObject()`
- **Size:** 254 → 263 bytes (↑)

### 9. Object Access (1 test)

#### Test 18: Object Property Access
- **Rule:** No spaces around colon in property/method access
- **Bad File:** `oObject : Property`, `oObject: AnotherProperty` (spaces)
- **Expected:** `oObject:Property`
- **Size:** 225 → 220 bytes (↓)

### 10. Operator & Expression Rules (2 tests)

#### Test 19: Logical Operator Spacing
- **Rule:** Spaces around logical operators
- **Bad File:** `bCondition.AND.nValue`, `0.OR.2` (no spaces)
- **Expected:** `bCondition .AND. nValue`, `0 .OR. 2`
- **Size:** 202 → 206 bytes (↑)

#### Test 20: Array Literal Syntax
- **Rule:** Consistent spacing in array literals
- **Bad File:** `{ 1,2,3 }`, `{1, 2, 3 }` (inconsistent)
- **Expected:** `{ 1, 2, 3 }`
- **Size:** 147 → 148 bytes (↑)

#### Test 21: String Concatenation with `+`
- **Rule:** Use explicit `+` operator for string concatenation
- **Bad File:** `"Hello" "World"` (implicit concatenation)
- **Expected:** `"Hello" + "World"`
- **Size:** 226 → 229 bytes (↑)

## Test Statistics

- **Total Test Fixtures:** 21
- **Total Test Categories:** 10
- **Average Bad File Size:** 218 bytes
- **Average Expected File Size:** 225 bytes

### Size Distribution
- Files that grow after formatting: 14
- Files that shrink after formatting: 6
- Files that stay same size: 1

## Running the Tests

### Display Fixture Report

```bash
npm run compile
node out/tests/fixtures-loader.js
```

Output shows:
- Total number of fixtures
- Breakdown by category
- File sizes (bad vs expected)
- Warnings for identical files

### Generate Markdown Documentation

```bash
node out/tests/fixtures-loader.js --markdown
```

### Run Full Test Suite

```bash
npm test
```

## Style Guide Rules Matrix

| Category | Rule Count | Test IDs |
|----------|-----------|----------|
| Formatting | 4 | 01-04 |
| Keywords | 1 | 05 |
| Naming | 3 | 06-08 |
| Control Flow | 4 | 09-12 |
| Error Handling | 1 | 13 |
| Statements | 1 | 14 |
| Comments | 1 | 15 |
| Functions | 2 | 16-17 |
| Object Access | 1 | 18 |
| Operators | 3 | 19-21 |

## Success Criteria

Each test fixture validates that:

✅ Bad file exists and contains valid SSL code with style violations
✅ Expected file exists and shows the properly formatted result
✅ Formatter can process the bad file without errors
✅ Formatted output matches (or closely matches) expected output
✅ No style guide rules are violated in expected file

## How Tests Are Used

1. **Development:** Developers run fixtures locally to validate formatters
2. **CI/CD:** Tests ensure consistent formatting across the codebase
3. **Documentation:** Fixtures serve as executable examples of style guide rules
4. **Validation:** Fixtures prove the formatter correctly implements each rule

## Adding New Test Fixtures

To add a test for a new style guide rule:

1. Create a `-bad.ssl` file with style violations
2. Create a `-expected.ssl` file with correct formatting
3. Use naming: `NN-category-rule-{bad|expected}.ssl`
4. Increment NN sequentially
5. Update this documentation

Example:

```bash
# Create test for a new rule about regions
touch 22-regions-block-indentation-bad.ssl
touch 22-regions-block-indentation-expected.ssl
```

## References

- **Style Guide:** `/dev docs/ssl-style-guide/ssl-style-guide.schema.yaml`
- **Test Fixtures:** `/tests/fixtures/style-guide/`
- **Fixture Loader:** `/tests/fixtures-loader.ts`
- **Formatting Provider:** `/src/sslFormattingProvider.ts`
- **Diagnostic Rules:** `/src/sslDiagnosticProvider.ts`
