# Comment Indentation Enhancement

## Summary
Modified the SSL formatter to properly indent comments based on their surrounding code context. Comments now move to match the indentation level of the block they appear in.

## Changes Made

### File: `src/sslFormattingProvider.ts`
**Method:** `normalizeBlockIndentation()` (lines 357-378)

#### Before
Comments were skipped entirely during block indentation pass:
```typescript
// Skip empty lines or return original comment lines
if (!trimmed || trimmed.startsWith("/*") || trimmed.startsWith("*")) {
    return line;
}
```

#### After
Single-line comments (`/* ... ;`) are now indented to match the current indentation level. Lines starting with `*` are NOT treated as comment markers and retain their original indentation:
```typescript
// Skip empty lines
if (!trimmed) {
    return line;
}

// Handle single-line comments (but NOT lines that start with * - those are not comment markers)
if (trimmed.startsWith("/*")) {
    // Indent comments to match the current indentation level
    const indented = indentChar.repeat(indentLevel) + trimmed;
    return indented;
}

// Lines starting with * but not part of a multi-line comment are NOT comment lines
// (they're likely continuation of something else), so skip them
if (trimmed.startsWith("*")) {
    return line;
}
```

## Behavior

### Before Formatting
```ssl
:PROCEDURE TestProcedure;
:PARAMETERS nValue;
/* Comment describing parameters;
:IF nValue > 100;
/* Comment inside IF;
nResult := nValue * 2;
:ENDIF;
:ENDPROC;
```

### After Formatting
```ssl
:PROCEDURE TestProcedure;
	:PARAMETERS nValue;
	/* Comment describing parameters;
	:IF nValue > 100;
		/* Comment inside IF;
		nResult := nValue * 2;
	:ENDIF;
:ENDPROC;
```

## Details

1. **Single-line comments** (`/* ... ;` on one line) are indented to match the current `indentLevel`
2. **Lines starting with `*`** are NOT treated as comment markers and keep their original indentation
3. **Multi-line comments** starting with `/*` but not ending with `;` on the same line are preserved as-is (internal lines don't get re-indented)
4. **Empty lines** are preserved without changes
5. Comments are processed **after** calculating block indentation levels, so they correctly track:
   - Procedure/class bodies (+1 level)
   - Control structures like IF/WHILE/FOR (+1 level each)
   - Nested structures (cumulative indentation)

## Edge Cases Handled

- Comments at top-level (indent level 0) are not indented
- Nested block comments preserve their structure
- Multi-line comment content is preserved without modification
- Comments within strings are not affected (handled by multi-line comment/string tracking)

## Testing

The change was tested with:
- Various control structures (IF/ELSE, FOR, WHILE, TRY/CATCH, etc.)
- Nested blocks
- Comments at different indentation levels
- Multi-line comments

The formatter still properly:
- Splits multiple statements
- Normalizes keyword case
- Normalizes operator spacing
- Trims trailing whitespace
