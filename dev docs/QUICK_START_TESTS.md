# Quick Start Guide - Style Guide Tests

## 🎯 Quick Reference

### View All Fixtures
```bash
npm run compile
node out/tests/fixtures-loader.js
```

### Expected Output
Shows 21 test fixtures organized into 10 categories:
- **Formatting** (4 tests)
- **Keywords** (1 test)
- **Naming** (3 tests)
- **Control Flow** (4 tests)
- **Error Handling** (1 test)
- **Statements** (1 test)
- **Comments** (1 test)
- **Functions** (2 tests)
- **Object Access** (1 test)
- **Operators** (3 tests)

### Test Directory
```
tests/fixtures/style-guide/
```

21 test pairs (bad + expected files), each demonstrating a specific style guide rule.

## 📝 Test File Naming

```
NN-category-rule-{bad|expected}.ssl

NN         = Sequential number (01-21)
category   = Style rule category
rule       = Specific rule name
type       = "bad" (violations) or "expected" (correct)
```

### Example
- `06-naming-hungarian-notation-bad.ssl` - Variables without Hungarian prefix
- `06-naming-hungarian-notation-expected.ssl` - Variables with correct prefixes

## 🧪 What Each Test Validates

| Test # | Category | Rule | Purpose |
|--------|----------|------|---------|
| 01 | Formatting | Indentation | Tabs vs spaces |
| 02 | Formatting | Operator Spacing | Spaces around `:=`, `+`, `-`, `==` |
| 03 | Formatting | Comma Spacing | Space after commas |
| 04 | Formatting | Blank Lines | Lines between procedures |
| 05 | Keywords | Case | UPPERCASE with `:` prefix |
| 06 | Naming | Hungarian | Variable prefixes (s, n, b, a, d, o) |
| 07 | Naming | Procedures | PascalCase names |
| 08 | Naming | Variables | camelCase names |
| 09 | Control Flow | IF/ELSE | Indentation in conditionals |
| 10 | Control Flow | FOR | Indentation in loops |
| 11 | Control Flow | WHILE | While loop indentation |
| 12 | Control Flow | CASE | Case statement indentation |
| 13 | Error Handling | TRY/CATCH | Try block indentation |
| 14 | Statements | One Per Line | No multiple statements/line |
| 15 | Comments | Block Style | `/* comment; */` format |
| 16 | Functions | No Space | No space before `(` |
| 17 | Functions | PascalCase | Built-in function naming |
| 18 | Object Access | Property | No spaces around `:` |
| 19 | Operators | Logical | Spaces around `.AND.`, `.OR.` |
| 20 | Operators | Arrays | Array literal spacing |
| 21 | Operators | String Concat | Explicit `+` operator |

## 💾 How Fixtures Work

Each test pair shows the **before** and **after** of applying style guide rules:

### Example: Test 06 (Hungarian Notation)

**Bad File Content:**
```ssl
/* Hungarian notation should be used;
:PROCEDURE CalculateTotal;
:PARAMETERS qty, price;
:DECLARE result, flag, dateCreated;
	:IF flag;
		result := qty * price;
	:ENDIF;
:RETURN result;
:ENDPROC;
```

**Expected File Content:**
```ssl
/* Hungarian notation should be used;
:PROCEDURE CalculateTotal;
:PARAMETERS nQty, nPrice;
:DECLARE nResult, bFlag, dDateCreated;
	:IF bFlag;
		nResult := nQty * nPrice;
	:ENDIF;
:RETURN nResult;
:ENDPROC;
```

**Differences:**
- `qty` → `nQty` (numeric prefix)
- `price` → `nPrice` (numeric prefix)
- `result` → `nResult` (numeric prefix)
- `flag` → `bFlag` (boolean prefix)
- `dateCreated` → `dDateCreated` (date prefix)

## 📊 Test Coverage Statistics

- **21** total tests
- **42** test files (21 pairs)
- **10** style guide categories
- **~200+** individual style rule checks

## 🔧 Adding New Tests

To add a test for a new style guide rule:

1. **Create the bad file** (with violations):
   ```bash
   touch tests/fixtures/style-guide/22-category-rule-bad.ssl
   ```

2. **Create the expected file** (correct format):
   ```bash
   touch tests/fixtures/style-guide/22-category-rule-expected.ssl
   ```

3. **Fill with examples** following the pattern

4. **Verify** by running:
   ```bash
   npm run compile
   node out/tests/fixtures-loader.js
   ```

## 📚 Related Documentation

- **Full Test Details:** `STYLE_GUIDE_TESTS.md`
- **Fixture README:** `tests/fixtures/style-guide/README.md`
- **Style Guide:** `dev docs/ssl-style-guide/ssl-style-guide.schema.yaml`

## ✨ Key Points

✅ Tests are ready to use as-is
✅ Can be extended with new rules
✅ Serve as executable documentation
✅ Validate formatter correctness
✅ Can be integrated into CI/CD
