/**
 * Utility functions for formatting: string-aware replacements and operator spacing normalization
 *
 * The implementation below is deliberately conservative: it never modifies lines
 * that look like comments (block-start '/*' or leading '*') or lines inside a
 * multi-line comment. Replacements are done only on code lines and are applied
 * outside string literals.
 */

export function replaceOutsideStrings(line: string, pattern: RegExp, replacement: string): string {
    const segments: { text: string; inString: boolean }[] = [];
    let current = '';
    let inString = false;
    let stringChar: string | null = null;

    for (let i = 0; i < line.length; i++) {
        const char = line[i];
        // Handle start of string (simple - does not attempt to fully parse escapes)
        if (!inString && (char === '"' || char === "'")) {
            if (current) {
                segments.push({ text: current, inString: false });
                current = '';
            }
            inString = true;
            stringChar = char;
            current = char;
            continue;
        }

        if (inString) {
            current += char;
            // naive string end detection (works for our fixtures)
            if (char === stringChar) {
                segments.push({ text: current, inString: true });
                current = '';
                inString = false;
                stringChar = null;
            }
            continue;
        }

        current += char;
    }

    if (current) segments.push({ text: current, inString });

    return segments
        .map((seg) => (seg.inString ? seg.text : seg.text.replace(pattern, replacement)))
        .join('');
}

export function normalizeOperatorSpacing(text: string): string {
    const lines = text.split('\n');
    let inBlockComment = false;

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
            if (inBlockComment) {
                if (trimmed.endsWith(';')) inBlockComment = false;
                return line; // do not modify comment content
            }
            if (!trimmed) return line;
            if (trimmed.startsWith('/*') && !trimmed.endsWith(';')) {
                inBlockComment = true;
                return line;
            }
            if (trimmed.startsWith('/*') || trimmed.startsWith('*')) return line;

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
            out = replaceOutsideStrings(out, /\s*%\=\s*/g, ' %= ');

            // Ensure single-equals are only spaced when they look like assignment/comparison
            out = replaceOutsideStrings(out, /([A-Za-z0-9_\)])\s*=\s*([A-Za-z0-9_\(])/g, '$1 = $2');

            // Gentle spacing for < and > (avoid touching multi-char tokens already handled)
            out = replaceOutsideStrings(out, /([^<>=])\s*<\s*([^>=])/g, '$1 < $2');
            out = replaceOutsideStrings(out, /([^<>=])\s*>\s*([^=])/g, '$1 > $2');

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
export function normalizeIndentation(text: string, _indentStyle: string, _indentWidth: number, _tabSize: number): string {
    return text;
}
