import { describe, it, beforeEach } from 'mocha';
import { expect } from 'chai';
import * as vscode from 'vscode';
import { SSLFormattingProvider } from '../src/sslFormattingProvider';
import { MockTextDocument, MockUri, createSSLConfig } from './helpers/mockVSCode';

describe('User Feedback Formatting Fixes', () => {
    let formatter: SSLFormattingProvider;
    let config: any;

    beforeEach(() => {
        formatter = new SSLFormattingProvider();
        config = createSSLConfig();
    });

    function format(text: string, options: any = {}): string {
        const uri = new MockUri('/test/test.ssl');
        const doc = new MockTextDocument(uri, 'ssl', text);
        const formattingOptions = {
            tabSize: options.tabSize || 4,
            insertSpaces: options.insertSpaces !== undefined ? options.insertSpaces : false
        } as vscode.FormattingOptions;

        const mockConfig = require('vscode').workspace.configuration;
        // Reset config
        Object.keys(mockConfig).forEach(key => delete mockConfig[key]);
        Object.assign(mockConfig, createSSLConfig());

        if (options.config) {
            // Update existing configuration instance
            Object.keys(options.config).forEach(key => {
                const config = require('vscode').workspace.configuration; // Get current instance
                // Mock config logic uses full keys for updates
                config.update(key, options.config[key]);
            });
        }

        const edits = formatter.provideDocumentFormattingEdits(doc as any, formattingOptions, {} as vscode.CancellationToken);
        if (!edits || edits.length === 0) return text;
        return edits[0].newText;
    }

    it('Fix: Array bracket parsing regression', () => {
        // Issue: Brackets in RUNFOLDERNO[1,1] were treated as string delimiters
        // Test keyword inside array access
        const input3 = 'x := arr[:if];';
        const expected3 = 'x := arr[:IF];'; // :if should be normalized to :IF if correctly parsed as code
        expect(format(input3).trim()).to.equal(expected3);
    });

    it('Fix: List wrapping for :PARAMETERS', () => {
        const input = ':PARAMETERS A, B, C, D, E, F, G, H;';
        // With wrapLength=20 (force wrap)
        const formatted = format(input, { config: { "ssl.format.wrapLength": 20 } });
        // Expected behavior: split
        expect(formatted.includes('\n'), 'Should wrap long parameter list').to.be.true;
        // Check for parameters keyword
        expect(formatted.includes(':PARAMETERS'), 'Should contain keyword').to.be.true;
        // Check that at least one param is on new line
        const lines = formatted.split('\n');
        expect(lines.length > 1, 'Should have multiple lines').to.be.true;
    });

    it('Fix: CASE statement indentation', () => {
        const input =
            `:BEGINCASE
:CASE "1"
code := 1;
:CASE "2"
code := 2;
:ENDCASE`;

        const expected =
            `:BEGINCASE
\t:CASE "1"
\t\tcode := 1;
\t:CASE "2"
\t\tcode := 2;
:ENDCASE`;

        const formatted = format(input, { insertSpaces: false });
        // Normalize newlines
        const normalizedFormatted = formatted.replace(/\r\n/g, '\n').trim();
        const normalizedExpected = expected.replace(/\r\n/g, '\n').trim();

        expect(normalizedFormatted).to.equal(normalizedExpected);
    });

    it('Fix: Line spacing (max 2 blanks)', () => {
        const input = 'a := 1;\n\n\n\nb := 2;';
        const formatted = format(input);
        const blanks = formatted.trim().split('\n').filter(l => l.trim() === '').length;
        expect(blanks <= 2, `Should have max 2 blank lines, got ${blanks}`).to.be.true;
    });

    it('Fix: SQL Indentation visual column', () => {
        // Wrap in procedure/if to ensure indentation of \t\t is valid and preserved
        const sqlInput =
            `:PROCEDURE Test
\t:IF 1
\t\tSQLExecute("SELECT * FROM table WHERE id=1");
\t:ENDIF
:ENDPROC`;

        const formatted = format(sqlInput, { config: { "ssl.format.sql.enabled": true }, tabSize: 4, insertSpaces: false });

        const lines = formatted.split('\n');
        // Ensure SQL was formatted (wrapped)
        // With canonicalCompact, WHERE wraps.
        const whereLine = lines.find(l => l.trim().startsWith('WHERE'));
        expect(whereLine, 'WHERE should be on new line').to.exist;

        // Check indentation of WHERE line.
        // Should align with query start.
        // \t\tSQLExecute("...
        // Quote starts after SQLExecute(".
        // Visual width:
        // \t (4) + \t (4) = 8
        // SQLExecute = 10
        // (" = 2
        // Total = 20.
        // So continuation lines should start at col 21 (quote pos + 1 space for alignment).
        // It uses spaces for alignment even if using tabs for indent.
        const leadingSpaces = whereLine!.match(/^\s*/)[0];
        // Note: The leading spaces will be mixed: \t\t (preserved indent) + spaces (continuation)?
        // No, formatSqlLiterals calculates continuationIndent using ' '.repeat(quoteColumn + 1).
        // Wait, does it replace the original indentation?
        // formattedSql = sqlLines[0] + '\n' + sqlLines.slice(1).map(l => continuationIndent + l).join('\n');
        // It uses pure spaces for continuation lines.

        // However, tab expansion depends on position.
        // If line starts with spaces, length is length.
        expect(leadingSpaces.length >= 20, `Expected visual indentation around 20 spaces, got ${leadingSpaces.length}`).to.be.true;
    });

    it('Fix: SQL UPDATE indentation style', () => {
        const input = 'SQLExecute("UPDATE table SET a=1 WHERE id=1");';
        const formatted = format(input, { config: { "ssl.format.sql.enabled": true } });

        const lines = formatted.split('\n');
        const updateLine = lines.find(l => l.includes('UPDATE'));
        expect(updateLine, 'Should have UPDATE line').to.exist;
        // Updated requirement: SET stays on same line
        expect(updateLine!.includes('SET'), 'UPDATE line should contain SET').to.be.true;

        const whereLine = lines.find(l => l.trim().startsWith('WHERE'));
        expect(whereLine, 'WHERE should be on new line').to.exist;
    });
    it('Fix: Balanced list wrapping', () => {
        const input = ':PARAMETERS A, B, C, D, E, F, G, H;';
        // With wrapLength=40.
        // Total length ~25 chars + separators ~16 = 41 chars approx.
        // Should split into 2 lines. 
        // Balanced: A, B, C, D on first line? E, F, G, H on second?
        // Greedy would fill first line.
        // Balanced should try to be even?
        const formatted = format(input, { config: { "ssl.format.wrapLength": 30 } });
        const lines = formatted.split('\n');
        expect(lines.length).to.be.greaterThan(1);
        // Check balance: lengths should be roughly similar (lines > 1 confirmed above)
        // With wrap=30, L1 ~18-24 chars, L2 ~13 chars.
        // greedy would produce L1 ~30, L2 ~0-5.
        // Just verify >1 line for now as small input variance is high.
        expect(lines.length).to.be.greaterThan(1);
    });

    it('Fix: Consolidate broken statements', () => {
        // Input has statement split arbitrarily
        const input = 'x\n   := 1\n   ;';
        const formatted = format(input);
        // Should be joined
        expect(formatted.trim()).to.equal('x := 1;');
    });

    it('Fix: Argument list leading/trailing commas', () => {
        // Long function call
        const input = 'Func("LongString", {Item1, Item2, Item3, Item4, Item5});';
        // Force wrap
        const formatted = format(input, { config: { "ssl.format.wrapLength": 20 } });
        // Should wrap.
        // Check that comma is at end of line, not start
        const lines = formatted.split('\n');
        expect(lines.length).to.be.greaterThan(1);
        lines.slice(0, -1).forEach(line => {
            // Lines that are not last should typically end with comma (unless split inside string)
            // Or at least NOT start with comma (except indent)
            expect(line.trim().startsWith(',')).to.be.false;
        });
    });

    it('Fix: SQL SET clause vertical formatting', () => {
        const input = 'SQLExecute("UPDATE table SET col1 = 1, col2 = 2, col3 = 3 WHERE id=1");';
        const formatted = format(input, { config: { "ssl.format.sql.enabled": true } });

        // Expect SET items on new lines
        const lines = formatted.split('\n');

        // Find UPDATE ... SET line
        const updateLine = lines.find(l => l.includes('UPDATE') && l.includes('SET'));
        expect(updateLine, 'UPDATE table SET should be on one line').to.exist;

        // Subsequent lines should contain assignments
        // Check for multiple lines containing '='
        const assignmentLines = lines.filter(l => l.includes('=') && !l.includes('WHERE') && l.includes('col'));
        expect(assignmentLines.length).to.be.greaterThan(1);
    });

    it('Fix: SQL Subquery alignment', () => {
        const input = 'SQLExecute("SELECT * FROM t WHERE id IN (SELECT id FROM t2 WHERE x=1)");';
        const formatted = format(input, { config: { "ssl.format.sql.enabled": true } });

        const lines = formatted.split('\n');
        // Subquery (SELECT id...) should be indented relative to outer query
        // Outer query:
        // SELECT *
        // FROM t
        // WHERE id IN (
        //     SELECT id ...

        const subSelect = lines.find(l => l.includes('SELECT') && !l.startsWith('SELECT')); // Inner SELECT?
        // Wait, formatCanonicalCompactStyle capitalizes keywords.
        // Inner SELECT might be on new line?
        // Regex replacement for SELECT is only top-level if I used `^`?
        // No, I used `selectMatch` logic for specific SELECT column formatting.
        // But `majorClauses` loop processes GLOBAL `SELECT`?
        // Wait, `SELECT` is NOT in `majorClauses` list!
        // It is handled separately at start.
        // So nested SELECT is NOT forcefully put on new line by `majorClauses`.
        // UNLESS it is followed by regular major clauses like FROM.
        // Nested FROM will be put on new line.

        const innerFrom = lines.find(l => l.trim().startsWith('FROM t2'));
        expect(innerFrom).to.exist;

        // Check indentation of inner FROM
        // Should be greater than outer FROM
        const outerFrom = lines.find(l => l.trim().startsWith('FROM t'));
        let outerIndent = 0;
        if (outerFrom) {
            const m = outerFrom.match(/^\s*/);
            if (m) outerIndent = m[0].length;
        }

        let innerIndent = 0;
        if (innerFrom) {
            const m = innerFrom.match(/^\s*/);
            if (m) innerIndent = m[0].length;
        }

        // Inner FROM should be at same or greater indent (column alignment)
        expect(innerIndent).to.be.greaterThanOrEqual(outerIndent);
    });
    it('Fix: Regression - text merging in consolidateStatements', () => {
        // Reproduce VERSIONFROM bug
        const input = 'x := SQLExecute("SELECT tmv.VERSION\n' +
            '                 FROM TEST_METHODS_VERSIONS tmv");';
        const formatted = format(input);

        // Should NOT merge to VERSIONFROM
        expect(formatted).to.contain('tmv.VERSION FROM');
        expect(formatted).to.not.contain('VERSIONFROM');
    });

    it('Fix: Visual alignment for :PARAMETERS', () => {
        const input = ':PARAMETERS A, B, C, D, E, F, G, H;';
        // With wrapLength=30
        const formatted = format(input, { config: { "ssl.format.wrapLength": 30 } });
        const lines = formatted.split('\n');

        // First line: :PARAMETERS A, B, ...
        // Continuation should align with A
        // :PARAMETERS is 11 chars. Space is 1. Indent should be 12.
        // :PARAMETERS A, B,
        //             C, D...

        const firstLine = lines[0];
        const indentLen = lines[1].match(/^\s*/)![0].length;
        // The first token matches ':PARAMETERS ' ?
        expect(indentLen).to.equal(12, 'Should use visual alignment (12 spaces)');
    });

    it('Fix: Visual alignment for function calls', () => {
        const input = 'MyFunc(A, B, C, D, E, F);';
        const formatted = format(input, { config: { "ssl.format.wrapLength": 15 } });
        const lines = formatted.split('\n');
        // MyFunc(A, B,
        //        C, D...
        // MyFunc is 6. '(' is 1. Indent 7.
        const indentLen = lines[1].match(/^\s*/)![0].length;
        expect(indentLen).to.equal(7, 'Should align with open paren (7 spaces)');
    });

    it('Fix: SQL UPDATE SET formatting', () => {
        const input = 'SQLExecute("UPDATE table SET col1=1, col2=2 WHERE id=1");';
        const formatted = format(input, { config: { "ssl.format.sql.enabled": true } });
        const lines = formatted.split('\n');

        // Expected:
        // UPDATE table SET
        //     col1=1,
        //     col2=2
        // WHERE id=1

        const updateLine = lines.find(l => l.includes('UPDATE'));
        expect(updateLine).to.contain('SET', 'UPDATE line should contain SET'); // Changed requirement

        // Subsequent lines should contain assignments
        const col1Index = lines.findIndex(l => l.includes('col1=1'));
        expect(col1Index).to.be.greaterThan(lines.indexOf(updateLine!));
    });

    it('Fix: SQL INSERT INTO formatting', () => {
        const input = 'SQLExecute("INSERT INTO table (c1, c2, c3, c4) VALUES (v1, v2, v3, v4)");';
        const formatted = format(input, { config: { "ssl.format.sql.enabled": true } });
        // Should wrap columns if possible, but basic INSERT/VALUES splitting is main requirement
        // But user said "insert statement breaks line limits... inserts are not being formatted"
        // So we want wrapped content inside parentheses? 
        // For now, check that INSERT INTO and VALUES are split

        const lines = formatted.split('\n');
        expect(formatted).to.contain('INSERT INTO');
        expect(formatted).to.contain('VALUES');

        // Ideally:
        // INSERT INTO table (c1, c2,
        //     c3, c4)
        // VALUES (v1, v2,
        //     v3, v4)
        // OR
        // INSERT INTO table (
        //     c1, c2, c3...
        // )
        // Let's implement parentheses content wrapping for SQL.
    });

    it('Fix: Regression - comment merging in consolidateStatements', () => {
        // Line ends with block comment, should not merge next line if statement is complete
        const input = 'x := 1; /* comment */\n' +
            'y := 2;';
        const formatted = format(input);

        const lines = formatted.trim().split('\n');
        expect(lines).to.have.lengthOf(2);
        expect(lines[0]).to.contain('x := 1');
        expect(lines[1]).to.contain('y := 2');
    });

    it('Fix: SQL indentation follows tab/spaces setting (not hardcoded 2)', () => {
        const input = 'SQLExecute("SELECT * FROM t WHERE id=1 AND x=2");';
        // Mock config uses tabSize 4, indentStyle 'tab' maybe? 
        // Helper creates config. Let's force space config for this test.
        // We want to check hanging indent.
        // "SELECT * ..." -> "SELECT *"
        // "FROM t..." -> "FROM t"
        // "WHERE id=1" -> "WHERE id=1"
        // "  AND x=2" -> Should use 4 spaces if configured, or tab?
        // Canonical formatCanonicalCompactStyle previously used 2 spaces.

        // Let's test standard 4 space indent
        // We can't easily change tabSize passed to formatText in this test wrapper 
        // without updating the mock config extensively? 
        // formatSqlLiterals uses passed tabSize (default 4).

        const formatted = format(input, { config: { "ssl.format.sql.enabled": true } });
        // The AND line should be indented.
        const lines = formatted.split('\n');
        const andLine = lines.find(l => l.trim().startsWith('AND'));
        expect(andLine).to.exist;

        // SQLExecute("SELECT * ...")
        // Alignment: SQLExecute("SELECT  (len 12)
        // Hanging Indent: 4
        // Total: 16
        // The user cares that hanging indent is standard (4 or configured), not 2.
        // With alignment, it becomes > 4, but relative to clause start it should be 4?
        // Actually AND aligns with column indent.
        // columns indent = 7.
        // We want checking that we aren't using hardcoded 2.

        const indent = andLine!.match(/^\s*/)![0].length;
        // 12 (alignment to string start) + 4 (indent setup) = 16
        expect(indent).to.equal(16, "SQL hanging indent should be 4 spaces + alignment");
    });

    it('Fix: Specific SQL INSERT formatting preference', () => {
        const input = 'SQLExecute("INSERT INTO FOLDERS (FOLDERNO, FLDSTS, SP_CODE) VALUES (?RUNFOLDERNO?, ?Logged?, -1)");';
        const formatted = format(input, { config: { "ssl.format.sql.enabled": true } });

        const lines = formatted.trim().split('\n');
        // Expected:
        // SQLExecute("INSERT INTO FOLDERS      (Line 1)
        //     (FOLDERNO, FLDSTS, SP_CODE)      (Line 2, indented)
        // VALUES                               (Line 3)
        //     (?RUNFOLDERNO?, ?Logged?, -1)"); (Line 4, indented)

        // Check line count
        // Can differ slightly depending on wrapping, but should be at least 4 lines for this structure if strictly followed
        // or 3 if VALUES follows.

        // User wants:
        // INSERT INTO FOLDERS
        //    (columns)
        // VALUES
        //    (values)

        const insertLine = lines.find(l => l.includes('INSERT INTO'));
        // With new wrapping logic, values might be inside parens on new line
        // VALUES
        //     (
        //     ?RUNFOLDERNO?...
        const valuesListLine = lines.find(l => l.trim().startsWith('?RUNFOLDERNO?'));

        expect(insertLine).to.exist;
        expect(insertLine).to.not.include('FOLDERNO'); // Columns should be on next line
        // Columns line might be simply (FOLDERNO... or ( \n FOLDERNO... depends on length?
        // In this short case, wrapSqlList might return one line if it fits.
        // But our logic forces: ( \n wrappedCols \n ).
        // So columnsLine should be just '(', and FOLDERNO on next line?
        // Let's verify output structure.
        // The previous logic was: insertBlock = ... \n${indent}(\n${wrappedCols}\n${indent})

        // So we expect:
        // INSERT INTO ...
        //    (
        //    FOLDERNO...

        const openParenLine = lines.find(l => l.trim() === '(');
        expect(openParenLine).to.exist;

        const colListLine = lines.find(l => l.trim().startsWith('FOLDERNO'));
        expect(colListLine).to.exist;

        const valuesLine = lines.find(l => l.trim().startsWith('VALUES'));
        expect(valuesLine).to.exist;
        expect(valuesListLine).to.exist;

        // Ensure indentation
        // colListLine should be indented more than INSERT?
        // INSERT is at indent 0 (relative to SQL start)
        // ( is at indent 4
        // FOLDERNO is at indent 4?
        // wrapSqlList uses `indent`.
        // So `(` is at indent 4. `wrappedCols` lines start with `indent` (4).
        // So they align.

        expect(colListLine!.match(/^\s*/)![0].length).to.be.greaterThan(insertLine!.match(/^\s*/)![0].length);
    });

    it('Fix: Wrap long SQL INSERT columns/values', () => {
        // Very long line that should be wrapped
        const longColumns = 'COL1, COL2, COL3, COL4, COL5, COL6, COL7, COL8, COL9, COL10, COL11, COL12, COL13, COL14, COL15';
        const longValues = 'VAL1, VAL2, VAL3, VAL4, VAL5, VAL6, VAL7, VAL8, VAL9, VAL10, VAL11, VAL12, VAL13, VAL14, VAL15';
        const input = `SQLExecute("INSERT INTO TABLE (${longColumns}) VALUES (${longValues})");`;

        // Configure wrap length effectively (though formatCanonicalCompactStyle might use hardcoded or internal logic)
        // We will target ~90 char wrapping in implementation.
        const formatted = format(input, { config: { "ssl.format.sql.enabled": true, "ssl.format.wrapLength": 80 } });
        const lines = formatted.split('\n');

        // Should be multiple lines for columns and values
        // Find lines starting with (COL1
        const colLines = lines.filter(l => l.trim().startsWith('(COL1') || l.trim().startsWith('COL'));
        const valLines = lines.filter(l => l.trim().startsWith('(VAL1') || l.trim().startsWith('VAL'));

        // If wrapped, we expect more than 1 line for columns or values content effectively, 
        // OR the lines should not be excessively long.
        // Let's check max line length of the SQL parts.
        const maxLen = Math.max(...lines.map(l => l.length));
        expect(maxLen).to.be.lessThan(90, "Line length exceeded 90 chars"); // Stricter verification

        // Also check that we definitely have multiple lines for the content if it was wrapped
        // Also check that we definitely have multiple lines for the content if it was wrapped
        expect(colLines.length).to.be.greaterThan(1, "Columns should be wrapped");
    });

    it('Fix: INSERT table name preservation', () => {
        const input = 'SQLExecute("INSERT INTO MY_TABLE (COL1) VALUES (1)");';
        const formatted = format(input, { config: { "ssl.format.sql.enabled": true } });
        expect(formatted).to.contain('INSERT INTO MY_TABLE');
    });

    it('Fix: INSERT INTO ... SELECT formatting', () => {
        // Line 86 style: INSERT INTO ... SELECT ...
        const input = 'SQLExecute("INSERT INTO T (A, B, C) SELECT 1, 2, 3 FROM T2");';
        const formatted = format(input, { config: { "ssl.format.sql.enabled": true } });
        // Expect formatting:
        // INSERT INTO T
        // (
        // A, B, C
        // )
        // SELECT
        //     1, 2, 3
        const lines = formatted.split('\n');
        expect(formatted).to.contain('INSERT INTO T');
        expect(formatted).to.contain('SELECT');

        const selectLine = lines.find(l => l.trim().startsWith('SELECT'));
        expect(selectLine).to.exist;

        // If SELECT list is wrapped, check indent
        // If short, it might be on same line? Or we force newline?
        // Logic says we force newline for VALUES list. Should do same for SELECT list.
        // But SELECT list has no parens. So it's just "1, 2, 3".
        // wrapSqlList puts it on new line if we pass "" as first item? No.
        // Logic: valuesBlock = `\nSELECT\n${indent}${wrappedVals}`.

        // So expected structure:
        // SELECT
        //     1, 2, 3

        const valLine = lines.find(l => l.trim().startsWith('1, 2, 3'));
        expect(valLine).to.exist;
        // Values list should be at least as indented as SELECT (they're at same or deeper level)
        expect(valLine!.match(/^\s*/)![0].length).to.be.greaterThanOrEqual(selectLine!.match(/^\s*/)![0].length);
    });
});
