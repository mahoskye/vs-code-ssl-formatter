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

export function replaceOutsideStrings(line: string, pattern: RegExp, replacement: string): string {
    const segments: { text: string; inString: boolean; inComment: boolean }[] = [];
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

    return segments
        .map((seg) => (seg.inString || seg.inComment ? seg.text : seg.text.replace(pattern, replacement)))
        .join('');
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

    return lines
        .map((line) => {
            const trimmed = line.trim();

            // Track block comment start/end using the project's convention (/* ... ;)
            if (!inBlockComment && trimmed.startsWith('/*') && !trimmed.endsWith(';')) {
                inBlockComment = true;
            }
            if (inBlockComment || trimmed.startsWith('/*') || trimmed.startsWith('*')) {
                if (inBlockComment && trimmed.endsWith(';')) {
                    inBlockComment = false;
                }
                return line; // do not modify comment content or let quotes toggle string state
            }

            // If we're already inside a multi-line string, only look for its end
            if (inMultiLineString) {
                const delimiterCount = (line.match(new RegExp(stringDelimiter === '"' ? '"' : (stringDelimiter === "'" ? "'" : '\\]'), 'g')) || []).length;
                if (delimiterCount % 2 !== 0 || (stringDelimiter === ']' && delimiterCount > 0)) {
                    inMultiLineString = false;
                    stringDelimiter = '';
                }
                return line; // do not modify string content
            }

            if (!trimmed) {
                return line;
            }

            let out = line;

            // Repair obvious broken multi-char operators first, but only outside strings
            for (const [pat, rep] of multiCharRep) {
                out = replaceOutsideStrings(out, pat, rep);
            }

            // Normalize spacing around common operators (done outside strings)
            out = replaceOutsideStrings(out, /\s*:=\s*/g, ' := ');
            out = replaceOutsideStrings(out, /\s*\+=\s*/g, ' += ');
            out = replaceOutsideStrings(out, /\s*-=\s*/g, ' -= ');
            out = replaceOutsideStrings(out, /\s*\*=\s*/g, ' *= ');
            out = replaceOutsideStrings(out, /\s*\/=\s*/g, ' /= ');
            out = replaceOutsideStrings(out, /\s*\^=\s*/g, ' ^= ');
            out = replaceOutsideStrings(out, /\s*%=\s*/g, ' %= ');

            // Arithmetic operators: add spaces around binary operators
            // Match: identifier/number/paren followed by operator followed by identifier/number/paren
            // This avoids matching unary operators like -5 or +5
            out = replaceOutsideStrings(out, /([A-Za-z0-9_\)])\s*\+\s*([A-Za-z0-9_\(])/g, '$1 + $2');
            out = replaceOutsideStrings(out, /([A-Za-z0-9_\)])\s*-\s*([A-Za-z0-9_\(])/g, '$1 - $2');
            out = replaceOutsideStrings(out, /([A-Za-z0-9_\)])\s*\*\s*([A-Za-z0-9_\(])/g, '$1 * $2');
            out = replaceOutsideStrings(out, /([A-Za-z0-9_\)])\s*\/\s*([A-Za-z0-9_\(])/g, '$1 / $2');

            // Comparison operators: normalize to have spaces on both sides
            out = replaceOutsideStrings(out, /\s*==\s*/g, ' == ');
            out = replaceOutsideStrings(out, /\s*!=\s*/g, ' != ');
            out = replaceOutsideStrings(out, /\s*<>\s*/g, ' <> ');
            out = replaceOutsideStrings(out, /\s*<=\s*/g, ' <= ');
            out = replaceOutsideStrings(out, /\s*>=\s*/g, ' >= ');

            // Ensure single-equals are only spaced when they look like assignment/comparison
            out = replaceOutsideStrings(out, /([A-Za-z0-9_\)])\s*=\s*([A-Za-z0-9_\(])/g, '$1 = $2');

            // Gentle spacing for < and > (avoid touching multi-char tokens already handled)
            out = replaceOutsideStrings(out, /([^<>=])\s*<\s*([^>=])/g, '$1 < $2');
            out = replaceOutsideStrings(out, /([^<>=])\s*>\s*([^=])/g, '$1 > $2');

            // Logical operators
            out = replaceOutsideStrings(out, /\s*\.AND\.\s*/gi, ' .AND. ');
            out = replaceOutsideStrings(out, /\s*\.OR\.\s*/gi, ' .OR. ');
            out = replaceOutsideStrings(out, /\s*\.NOT\.\s*/gi, ' .NOT. ');

            // Unary ! operator
            out = replaceOutsideStrings(out, /([a-zA-Z0-9_\)])\s*!/g, '$1 !');
            out = replaceOutsideStrings(out, /!\s*([a-zA-Z0-9_\(])/g, '! $1');

            // No space before commas, space after commas
            out = replaceOutsideStrings(out, /\s+,/g, ',');
            out = replaceOutsideStrings(out, /,\s*(\S)/g, ', $1');
            // Handle comma followed by string delimiter (which is in a different segment)
            out = out.replace(/,"/g, ', "').replace(/,'/g, ", '");

            // No space before semicolons
            out = replaceOutsideStrings(out, /\s+;/g, ';');

            // No space before opening parenthesis in function calls
            // Match: identifier followed by spaces and opening paren
            out = replaceOutsideStrings(out, /([a-zA-Z0-9_])\s+\(/g, '$1(');

            // Object property/method access: no spaces around colon
            // Match: identifier/paren followed by optional spaces, colon, optional spaces, identifier/paren
            // This matches: obj:Prop or obj :Prop or obj: Prop or obj : Prop
            // But NOT: :FOR or :TO (which have space/start before the colon)
            // The key: we require an identifier character immediately before the space-colon sequence
            out = replaceOutsideStrings(out, /([a-zA-Z0-9_\)])\s*:\s*([a-zA-Z_\(])/g, '$1:$2');

            // Ensure space before colon-prefixed keywords (like :TO, :FOR, :STEP, etc.)
            // This corrects cases where the above rule removed space before keywords
            const keywords = ['TO', 'FOR', 'IF', 'ELSE', 'ELSEIF', 'ENDIF', 'WHILE', 'ENDWHILE',
                              'NEXT', 'STEP', 'CASE', 'BEGINCASE', 'ENDCASE', 'OTHERWISE',
                              'TRY', 'CATCH', 'FINALLY', 'ENDTRY', 'PROCEDURE', 'ENDPROC',
                              'PARAMETERS', 'DECLARE', 'RETURN', 'PUBLIC', 'DEFAULT'];
            for (const kw of keywords) {
                const kwPattern = new RegExp(`(\\w):(${kw})\\b`, 'g');
                out = replaceOutsideStrings(out, kwPattern, `$1 :$2`);
            }

            // Convert implicit string concatenation to explicit with +
            // Match: string literal followed by whitespace and another string literal
            out = replaceImplicitStringConcatenation(out);

            // Track start of multi-line strings for subsequent lines
            const doubleQuoteCount = (line.match(/"/g) || []).length;
            const singleQuoteCount = (line.match(/'/g) || []).length;
            const bracketOpenCount = (line.match(/\[/g) || []).length;
            const bracketCloseCount = (line.match(/\]/g) || []).length;

            if (doubleQuoteCount % 2 !== 0) {
                inMultiLineString = true;
                stringDelimiter = '"';
            } else if (singleQuoteCount % 2 !== 0) {
                inMultiLineString = true;
                stringDelimiter = "'";
            } else if (bracketOpenCount !== bracketCloseCount) {
                inMultiLineString = true;
                stringDelimiter = ']';
            }

            return out;
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
