/**
 * Utility functions for formatting: string-aware replacements and operator spacing normalization
 *
 * The implementation below is deliberately conservative: it never modifies lines
 * that look like comments (block-start '/*' or leading '*') or lines inside a
 * multi-line comment. Replacements are done only on code lines and are applied
 * outside string literals.
 *
 * Note: SSL does not support escape sequences. Backslashes are literal characters,
 * not escape characters. A quote character always ends a string.
 */


export interface Segment {
    text: string;
    inString: boolean;
    inComment: boolean;
}

export function parseSegments(line: string): Segment[] {
    const segments: Segment[] = [];
    let current = '';
    let inString = false;
    let inComment = false;
    let stringChar: string | null = null;

    for (let i = 0; i < line.length; i++) {
        const char = line[i];
        const nextChar = i + 1 < line.length ? line[i + 1] : '';

        // Handle start of comment (/* ... syntax)
        if (!inString && !inComment && char === '/' && nextChar === '*') {
            if (current) {
                segments.push({ text: current, inString: false, inComment: false });
                current = '';
            }
            inComment = true;
            current = char + nextChar;
            i++; // Skip the next character since we've already processed it
            continue;
        }

        // Handle start of string (double quote, single quote, or bracket)
        if (!inString && !inComment && (char === '"' || char === "'" || char === '[')) {
            // Special check for [ - it might be array access OR string
            // If it's array access, we treat it as code (not string start)
            if (char === '[') {
                // Look backwards for identifier or closing paren/bracket
                let prevIndex = i - 1;
                while (prevIndex >= 0 && /\s/.test(line[prevIndex])) {
                    prevIndex--;
                }
                const isArrayAccess = prevIndex >= 0 && /[a-zA-Z0-9_\)\]]/.test(line[prevIndex]);

                if (isArrayAccess) {
                    current += char;
                    continue; // Treat as normal code character
                }
            }

            if (current) {
                segments.push({ text: current, inString: false, inComment: false });
                current = '';
            }
            inString = true;
            stringChar = char === '[' ? ']' : char; // For brackets, look for closing ]
            current = char;
            continue;
        }

        if (inString) {
            current += char;
            // String end detection: matching delimiter ends the string (no escape sequences in SSL)
            if (char === stringChar) {
                segments.push({ text: current, inString: true, inComment: false });
                current = '';
                inString = false;
                stringChar = null;
            }
            continue;
        }

        if (inComment) {
            current += char;
            // Comment end detection: semicolon ends SSL comments
            if (char === ';') {
                segments.push({ text: current, inString: false, inComment: true });
                current = '';
                inComment = false;
            }
            continue;
        }

        current += char;
    }

    if (current) {
        segments.push({ text: current, inString, inComment });
    }

    return segments;
}

export function replaceOutsideStrings(line: string, pattern: RegExp, replacement: string): string {
    const segments = parseSegments(line);
    const maskedSegments = segments.map((seg, i) => {
        if (seg.inString || seg.inComment) {
            return `__MASKED_${i}__`;
        }
        return seg.text;
    });

    const maskedLine = maskedSegments.join('');
    // If the pattern doesn't match the masked line, return the original segments joined
    if (!pattern.test(maskedLine)) {
        return segments.map(s => s.text).join('');
    }

    // Perform replacement on masked line
    let result = maskedLine.replace(pattern, replacement);

    // Restore content
    segments.forEach((seg, i) => {
        if (seg.inString || seg.inComment) {
            const placeholder = `__MASKED_${i}__`;
            // Use global replacement for the placeholder
            result = result.split(placeholder).join(seg.text);
        }
    });

    return result;
}


/**
 * Convert implicit string concatenation to explicit with + operator
 * SSL allows "string1" "string2" but style guide requires "string1" + "string2"
 */
function replaceImplicitStringConcatenation(line: string): string {
    // Match: closing quote, whitespace, opening quote (same type)
    // We need to handle both double quotes and single quotes
    // Pattern: (["'])  \s+  (["'])
    // But we need to make sure the closing and opening quotes match
    // Also ensure we don't match single strings like " " by requiring
    // non-whitespace before the first quote and after the second quote

    // Match double-quoted strings followed by whitespace and another double-quoted string
    // Require non-whitespace before first quote and after second quote to avoid matching " "
    let result = line.replace(/([^\s])"\s+"([^\s])/g, '$1" + "$2');

    // Match single-quoted strings followed by whitespace and another single-quoted string
    result = result.replace(/([^\s])'\s+'([^\s])/g, "$1' + '$2");

    return result;
}

export function normalizeOperatorSpacing(text: string): string {
    const lines = text.split('\n');
    let inBlockComment = false;
    let inMultiLineString = false;
    let stringDelimiter = '';

    const multiCharRep: Array<[RegExp, string]> = [
        [/\:\s*=/g, ':='],
        [/\+\s*=/g, '+='],
        [/-\s*=/g, '-='],
        [/\*\s*=/g, '*='],
        [/\/\s*=/g, '/='],
        [/\^\s*=/g, '^='],
        [/\%\s*=/g, '%='],
        [/=\s*=/g, '=='],
        [/!\s*=/g, '!='],
        [/<=\s*/g, '<='],
        [/>\s*=\s*/g, '>='],
        [/<\s*>\s*/g, '<>'],
    ];

    // Helper to process code content (clean lines or remainders)
    const processCode = (code: string): string => {
        // Fix missing spaces around + operators that are adjacent to strings
        const segments = parseSegments(code);
        for (let i = 0; i < segments.length; i++) {
            const seg = segments[i];
            if (!seg.inString && !seg.inComment) {
                // Check for + at start of code segment (preceded by string)
                if (i > 0 && segments[i - 1].inString) {
                    if (/^\s*\+/.test(seg.text)) {
                        seg.text = seg.text.replace(/^\s*\+\s*/, ' + ');
                    }
                }
                // Check for + at end of code segment (followed by string)
                if (i < segments.length - 1 && segments[i + 1].inString) {
                    if (/\+\s*$/.test(seg.text)) {
                        seg.text = seg.text.replace(/\s*\+\s*$/, ' + ');
                    }
                }
            }
        }
        let out = segments.map(s => s.text).join('');

        for (const [pat, rep] of multiCharRep) {
            out = replaceOutsideStrings(out, pat, rep);
        }

        // Standard replacements
        out = replaceOutsideStrings(out, /\s*:=\s*/g, ' := ');
        out = replaceOutsideStrings(out, /\s*\+=\s*/g, ' += ');
        out = replaceOutsideStrings(out, /\s*-=\s*/g, ' -= ');
        out = replaceOutsideStrings(out, /\s*\*=\s*/g, ' *= ');
        out = replaceOutsideStrings(out, /\s*\/=\s*/g, ' /= ');
        out = replaceOutsideStrings(out, /\s*\^=\s*/g, ' ^= ');
        out = replaceOutsideStrings(out, /\s*%=\s*/g, ' %= ');

        // Expanded character set for operands to include quotes, brackets, and parameters
        const operand = `[A-Za-z0-9_\\)\\]\\"\\'\\?]`;
        const operandStart = `[A-Za-z0-9_\\(\\[\\"\\'\\?\\.]`;

        // Arithmetic
        out = replaceOutsideStrings(out, new RegExp(`(${operand})\\s*\\+\\s*(${operandStart})`, 'g'), '$1 + $2');
        out = replaceOutsideStrings(out, new RegExp(`(${operand})\\s*-\\s*(${operandStart})`, 'g'), '$1 - $2');
        out = replaceOutsideStrings(out, new RegExp(`(${operand})\\s*\\*\\s*(${operandStart})`, 'g'), '$1 * $2');
        out = replaceOutsideStrings(out, new RegExp(`(${operand})\\s*\\/\\s*(${operandStart})`, 'g'), '$1 / $2');

        out = replaceOutsideStrings(out, /\s*==\s*/g, ' == ');
        out = replaceOutsideStrings(out, /\s*!=\s*/g, ' != ');
        out = replaceOutsideStrings(out, /\s*<>\s*/g, ' <> ');
        out = replaceOutsideStrings(out, /\s*<=\s*/g, ' <= ');
        out = replaceOutsideStrings(out, /\s*>=\s*/g, ' >= ');

        // Assignment / Equality
        // Note: SSL uses = for both assignment and equality depending on context,
        // but we handle := separately. Here we handle = used as operator.
        out = replaceOutsideStrings(out, new RegExp(`(${operand})\\s*=\\s*(${operandStart})`, 'g'), '$1 = $2');

        out = replaceOutsideStrings(out, /([^<>=])\s*<\s*([^>=])/g, '$1 < $2');
        out = replaceOutsideStrings(out, /([^<>=])\s*>\s*([^=])/g, '$1 > $2');

        out = replaceOutsideStrings(out, /\s*\.AND\.\s*/gi, ' .AND. ');
        out = replaceOutsideStrings(out, /\s*\.OR\.\s*/gi, ' .OR. ');
        out = replaceOutsideStrings(out, /\s*\.NOT\.\s*/gi, ' .NOT. ');

        out = replaceOutsideStrings(out, /([a-zA-Z0-9_\)])\s*!/g, '$1 !');
        out = replaceOutsideStrings(out, /!\s*([a-zA-Z0-9_\(])/g, '!$1');

        out = replaceOutsideStrings(out, /\s+,/g, ',');
        out = replaceOutsideStrings(out, /,\s*([^\s,])/g, ', $1');
        out = out.replace(/,"/g, ', "').replace(/,'/g, ", '");

        out = replaceOutsideStrings(out, /\s+;/g, ';');

        out = replaceOutsideStrings(out, /([a-zA-Z0-9_])\s+\(/g, '$1(');

        out = replaceOutsideStrings(out, /([a-zA-Z0-9_\)])\s*:\s*([a-zA-Z_\(])/g, '$1:$2');

        const keywords = ['TO', 'FOR', 'IF', 'ELSE', 'ELSEIF', 'ENDIF', 'WHILE', 'ENDWHILE',
            'NEXT', 'STEP', 'CASE', 'BEGINCASE', 'ENDCASE', 'OTHERWISE',
            'TRY', 'CATCH', 'FINALLY', 'ENDTRY', 'PROCEDURE', 'ENDPROC',
            'PARAMETERS', 'DECLARE', 'RETURN', 'PUBLIC', 'DEFAULT'];
        for (const kw of keywords) {
            const kwPattern = new RegExp(`(\\w):(${kw})\\b`, 'g');
            out = replaceOutsideStrings(out, kwPattern, `$1 :$2`);
        }

        // Collapse empty braces
        out = replaceOutsideStrings(out, /\{\s+\}/g, '{}');

        out = replaceImplicitStringConcatenation(out);
        return out;
    };

    // Helper to update multi-line string state based on line content
    const updateState = (text: string) => {
        const segments = parseSegments(text);
        if (segments.length === 0) return;

        const lastSeg = segments[segments.length - 1];
        if (lastSeg.inString) {
            inMultiLineString = true;
            const startChar = lastSeg.text[0];
            stringDelimiter = startChar === '[' ? ']' : startChar;
        }
    };

    return lines
        .map((line) => {
            try {
                const trimmed = line.trim();

                // Track block comment start/end using the project's convention (/* ... ;)
                if (!inBlockComment && trimmed.startsWith('/*') && !trimmed.endsWith(';')) {
                    inBlockComment = true;
                }
                if (inBlockComment || trimmed.startsWith('/*') || trimmed.startsWith('*')) {
                    if (inBlockComment && trimmed.endsWith(';')) {
                        inBlockComment = false;
                    }
                    return line;
                }

                // Handle multi-line string state
                if (inMultiLineString) {
                    // Look for the closing delimiter
                    const closeIdx = line.indexOf(stringDelimiter);
                    if (closeIdx === -1) {
                        // Still inside string
                        return line;
                    }

                    // Found close: split and process remainder
                    const preString = line.substring(0, closeIdx + 1);
                    const remainder = line.substring(closeIdx + 1);

                    // Reset string state
                    inMultiLineString = false;
                    stringDelimiter = '';

                    if (!remainder.trim()) {
                        return line; // Nothing after string
                    }

                    // Process remainder as code
                    let processedRemainder = processCode(remainder);

                    // Manual fix: if remainder starts with +, ensure space before it (since preString was string)
                    if (/^\s*\+/.test(processedRemainder)) {
                        processedRemainder = processedRemainder.replace(/^\s*\+\s*/, ' + ');
                    }
                    else if (!/^\s/.test(processedRemainder) && processedRemainder.length > 0) {
                        // pass
                    }

                    // Update state for next line based on remainder
                    // Use the processed remainder to determine if we opened a new string
                    updateState(processedRemainder);

                    const result = preString + processedRemainder;
                    return result;
                }

                if (!trimmed) {
                    return line;
                }

                // Normal processing
                let out = processCode(line);

                // Update state
                updateState(line); // Check original line (or out?) for unclosed string
                // Better to check 'line' because 'out' has replacements that shouldn't affect structure.
                // Actually 'processCode' preserves strings. So 'out' is safe.
                // Using 'line' is safer to avoid issues if I messed up replacements.
                // But wait, if I use 'line', I might detect a string that 'processCode' removed? (No, doesn't remove).
                // Use 'line'.

                return out;
            } catch (error) {
                console.log(`[DEBUG] Error processing line: "${line}": ${error}`);
                return line;
            }
        })
        .join('\n');
}

/**
 * Minimal indentation normalizer used by test scripts. It's intentionally
 * conservative: it does not modify comment content (only may normalize
 * leading whitespace) and otherwise returns the input unchanged. This keeps
 * the test focused on comment-content preservation.
 */
export function normalizeIndentation(text: string, indentStyle: string, indentWidth: number, tabSize: number): string {
    // Minimal no-op implementation for tests. If you need a full reimplementation
    // of the provider's indentation logic here, we can port it carefully and add
    // unit tests. For now, keep indentation stable so comment-preservation tests
    // focus on operator spacing.
    return text;
}
