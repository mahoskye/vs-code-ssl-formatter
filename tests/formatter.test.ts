/**
 * SSL Formatter Unit Tests
 * Tests the formatting provider using mock VSCode API
 */

import { describe, it } from 'mocha';
import { expect } from 'chai';
import * as fs from 'fs';
import * as path from 'path';
import { SSLFormattingProvider } from '../src/sslFormattingProvider';
import {
	createDocument,
	applyEdits,
	createFormattingOptions
} from './helpers/mockVSCode';

describe('SSL Formatter - Keyword Casing', () => {
	const formatter = new SSLFormattingProvider();
	const options = createFormattingOptions();

	it('should normalize lowercase keywords to UPPERCASE', () => {
		const input = ':procedure TestFunc;\n:parameters nValue;\n:if nValue > 0;\n:endif;\n:endproc;';
		const expected = ':PROCEDURE TestFunc;\n:PARAMETERS nValue;\n\t:IF nValue > 0;\n\t:ENDIF;\n:ENDPROC;\n';

		const doc = createDocument(input);
		const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
		const formatted = applyEdits(input, edits as any[]);

		expect(formatted).to.equal(expected, 'Keywords should be normalized to UPPERCASE');
	});

	it('should handle mixed case keywords', () => {
		const input = ':PrOcEdUrE Test;\n:PaRaMeTeRs x;\n:ReTuRn x;\n:EnDpRoC;';
		const expected = ':PROCEDURE Test;\n:PARAMETERS x;\n:RETURN x;\n:ENDPROC;\n';

		const doc = createDocument(input);
		const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
		const formatted = applyEdits(input, edits as any[]);

		expect(formatted).to.equal(expected);
	});

	it('should normalize all block keywords', () => {
		const keywords = [
			[':if;', ':IF;\n'],
			[':else;', ':ELSE;\n'],
			[':endif;', ':ENDIF;\n'],
			[':while;', ':WHILE;\n'],
			[':endwhile;', ':ENDWHILE;\n'],
			[':for;', ':FOR;\n'],
			[':next;', ':NEXT;\n'],
			[':begincase;', ':BEGINCASE;\n'],
			[':case;', ':CASE;\n'],
			[':otherwise;', ':OTHERWISE;\n'],
			[':endcase;', ':ENDCASE;\n'],
			[':try;', ':TRY;\n'],
			[':catch;', ':CATCH;\n'],
			[':finally;', ':FINALLY;\n'],
			[':endtry;', ':ENDTRY;\n']
		];

		keywords.forEach(([input, expected]) => {
			const doc = createDocument(input);
			const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
			const formatted = applyEdits(input, edits as any[]);
			expect(formatted).to.equal(expected, `Failed to normalize: ${input}`);
		});
	});
});

describe('SSL Formatter - Indentation', () => {
	const formatter = new SSLFormattingProvider();
	const options = createFormattingOptions();

	it('should convert spaces to tabs', () => {
		const input = ':PROCEDURE Test;\n    nValue := 1;\n:ENDPROC;';
		const expected = ':PROCEDURE Test;\n\tnValue := 1;\n:ENDPROC;\n';

		const doc = createDocument(input);
		const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
		const formatted = applyEdits(input, edits as any[]);

		expect(formatted).to.equal(expected, 'Should convert leading spaces to tabs');
	});

	it('should indent IF blocks consistently', () => {
		const input = ':IF x > 0;\nnValue := 1;\n:ENDIF;';
		const expected = ':IF x > 0;\n\tnValue := 1;\n:ENDIF;\n';

		const doc = createDocument(input);
		const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
		const formatted = applyEdits(input, edits as any[]);

		expect(formatted).to.equal(expected);
	});

	it('should handle nested indentation', () => {
		const input = ':IF a;\n:IF b;\nx := 1;\n:ENDIF;\n:ENDIF;';
		const expected = ':IF a;\n\t:IF b;\n\t\tx := 1;\n\t:ENDIF;\n:ENDIF;\n';

		const doc = createDocument(input);
		const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
		const formatted = applyEdits(input, edits as any[]);

		expect(formatted).to.equal(expected, 'Nested blocks should have 2 levels of indentation');
	});
});

describe('SSL Formatter - Operator Spacing', () => {
	const formatter = new SSLFormattingProvider();
	const options = createFormattingOptions();

	it('should add spaces around assignment operator', () => {
		const input = 'nValue:=123;';
		const expected = 'nValue := 123;\n';

		const doc = createDocument(input);
		const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
		const formatted = applyEdits(input, edits as any[]);

		expect(formatted).to.equal(expected);
	});

	it('should add spaces around comparison operators', () => {
		const tests = [
			['a==b', 'a == b;\n'],
			['a!=b', 'a != b;\n'],
			['a<>b', 'a <> b;\n'],
			['a<b', 'a < b;\n'],
			['a>b', 'a > b;\n'],
			['a<=b', 'a <= b;\n'],
			['a>=b', 'a >= b;\n']
		];

		tests.forEach(([input, expected]) => {
			const doc = createDocument(input + ';');
			const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
			const formatted = applyEdits(input + ';', edits as any[]);
			expect(formatted).to.include(expected.trim().replace(';\n', ''), `Failed for: ${input}`);
		});
	});

	it('should add spaces around logical operators', () => {
		const input = 'bResult := a.AND.b.OR.c;';
		const expected = 'bResult := a .AND. b .OR. c;\n';

		const doc = createDocument(input);
		const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
		const formatted = applyEdits(input, edits as any[]);

		expect(formatted).to.equal(expected);
	});
});

describe('SSL Formatter - Builtin Functions', () => {
	const formatter = new SSLFormattingProvider();
	const options = createFormattingOptions();

	it('should normalize builtin functions to canonical casing', () => {
		const tests = [
			['result := sqlexecute(sql);', 'result := SQLExecute(sql);\n'],
			['result := SQLEXECUTE(sql);', 'result := SQLExecute(sql);\n'],
			['result := createudobject();', 'result := CreateUdObject();\n'],
			['result := CREATEUDOBJECT();', 'result := CreateUdObject();\n'],
			['result := doproc(name);', 'result := DoProc(name);\n'],
			['result := DOPROC(name);', 'result := DoProc(name);\n'],
			['result := buildstring(fmt);', 'result := buildstring(fmt);\n'],
			['result := alltrim(sText);', 'result := AllTrim(sText);\n'],
			['result := aadd(arr, val);', 'result := aadd(arr, val);\n']
		];

		tests.forEach(([input, expected]) => {
			const doc = createDocument(input);
			const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
			const formatted = applyEdits(input, edits as any[]);
			expect(formatted).to.equal(expected, `Failed to normalize: ${input}`);
		});
	});
});

describe('SSL Formatter - Comment Preservation', () => {
	const formatter = new SSLFormattingProvider();
	const options = createFormattingOptions();

	it('should preserve inline comments after statements', () => {
		const input = 'dropSql := "drop table " + tempTable; /*drop temp table if created;';
		const expected = 'dropSql := "drop table " + tempTable; /*drop temp table if created;\n';

		const doc = createDocument(input);
		const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
		const formatted = applyEdits(input, edits as any[]);

		expect(formatted).to.equal(expected, 'Inline comments should stay on the same line');
	});

	it('should preserve multiple inline comments', () => {
		const tests = [
			['count := count + 1;  /*increment counter;', 'count := count + 1; /*increment counter;\n'],
			['isValid := FALSE;    /*reset validation flag;', 'isValid := FALSE; /*reset validation flag;\n'],
			['result := Calculate(x, y);  /*perform calculation;', 'result := Calculate(x, y); /*perform calculation;\n']
		];

		tests.forEach(([input, expected]) => {
			const doc = createDocument(input);
			const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
			const formatted = applyEdits(input, edits as any[]);
			expect(formatted).to.equal(expected, `Failed to preserve comment for: ${input}`);
		});
	});

	it('should still split multiple statements when no inline comment present', () => {
		const input = ':ENDCASE; nProcessed := nProcessed + 1;';
		const expected = ':ENDCASE;\nnProcessed := nProcessed + 1;\n';

		const doc = createDocument(input);
		const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
		const formatted = applyEdits(input, edits as any[]);

		expect(formatted).to.equal(expected, 'Multiple statements without comments should still be split');
	});
});

describe('SSL Formatter - Line Wrapping', () => {
	const formatter = new SSLFormattingProvider();
	const options = createFormattingOptions();

	it('should wrap long string at word boundaries', () => {
		const input = `sMessage := "This is a very long message that exceeds the ninety character line limit and should be wrapped at word boundaries";`;
		const doc = createDocument(input);
		const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
		const formatted = applyEdits(input, edits as any[]);

		// Should break at spaces, not mid-word
		expect(formatted).to.include('+');
		expect(formatted).to.not.match(/\w"-/); // No word broken with quote-dash
		expect(formatted.split('\n').every(line => line.length <= 92)).to.be.true; // Allow some margin
	});

	it('should not break very long unbreakable words', () => {
		// A URL or very long word with no spaces - should keep whole
		const input = `sUrl := "https://verylongdomainname.example.com/path/to/resource/with/many/segments/that/exceeds/ninety/characters/total";`;
		const doc = createDocument(input);
		const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
		const formatted = applyEdits(input, edits as any[]);

		// Should NOT break the URL, even though it's long
		expect(formatted.split('\n').length).to.equal(2); // Original + newline, no wrapping
	});

	it('should wrap bracketed list in condensed mode for short items', () => {
		const input = `aColors := {"red", "green", "blue", "yellow", "orange", "purple", "pink", "brown", "black", "white"};`;
		const doc = createDocument(input);
		const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
		const formatted = applyEdits(input, edits as any[]);

		// Should break into multiple lines but keep multiple items per line
		const lines = formatted.trim().split('\n');
		expect(lines.length).to.be.greaterThan(2); // Multiple lines
		// In condensed mode, should have fewer lines than items (multiple items per line)
		expect(lines.length).to.be.lessThan(11); // 10 items, should condense to fewer lines
	});

	it('should wrap bracketed list in expanded mode for long items', () => {
		const input = `result := SomeFunction(veryLongParameterNameOne, veryLongParameterNameTwo, veryLongParameterNameThree, veryLongParameterNameFour);`;
		const doc = createDocument(input);
		const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
		const formatted = applyEdits(input, edits as any[]);

		// Should break with one item per line
		const lines = formatted.trim().split('\n');
		expect(lines.length).to.be.greaterThan(3); // At least 4 lines for 4 params
		expect(formatted).to.include('veryLongParameterNameOne,');
		expect(formatted).to.include('veryLongParameterNameTwo,');
	});

	it('should wrap logical expressions at operators', () => {
		const input = `bCondition := (a .AND. b .AND. c .AND. d) .OR. (e .AND. f .AND. g .AND. h) .OR. (i .AND. j .AND. k);`;
		const doc = createDocument(input);
		const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
		const formatted = applyEdits(input, edits as any[]);

		// Should break at .OR. and .AND. operators
		expect(formatted).to.include('.OR.');
		const lines = formatted.trim().split('\n');
		expect(lines.length).to.be.greaterThan(2); // Multiple lines
	});

	it('should not wrap lines already under 90 characters', () => {
		const input = `sShort := "This is short";`;
		const doc = createDocument(input);
		const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
		const formatted = applyEdits(input, edits as any[]);

		// Should not add any line breaks
		expect(formatted.trim().split('\n').length).to.equal(1);
	});

	it('should not wrap single-line comments even if they exceed 90 characters', () => {
		const input = `/* This is a very long single-line comment that exceeds the ninety character line limit but should not be wrapped at all;`;
		const doc = createDocument(input);
		const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
		const formatted = applyEdits(input, edits as any[]);

		// Should keep comment as single line
		expect(formatted.trim().split('\n').length).to.equal(1);
		expect(formatted).to.include('This is a very long single-line comment');
	});

	it('should not wrap multi-line comment blocks', () => {
		const input = `/*
This is a multi-line comment block with some very long lines that exceed the ninety character limit
    And some indented lines with detailed explanations that are also quite long and exceed the limit
    Another long line that should not be wrapped even though it is very long and exceeds ninety characters
;`;
		const doc = createDocument(input);
		const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
		const formatted = applyEdits(input, edits as any[]);

		// Should preserve all comment lines exactly as they are
		expect(formatted).to.include('This is a multi-line comment block');
		expect(formatted).to.include('And some indented lines');
		expect(formatted).to.include('Another long line');
		// Should not add any concatenation operators to comments
		expect(formatted).to.not.include('/*' + ' +'); // No comment splitting
	});

	it('should not wrap inline comments after code', () => {
		const input = `nValue := 42; /* This is an inline comment that is very long and exceeds the ninety character line limit;`;
		const doc = createDocument(input);
		const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
		const formatted = applyEdits(input, edits as any[]);

		// Should preserve inline comment on same line
		expect(formatted).to.include('nValue := 42;');
		expect(formatted).to.include('This is an inline comment');
		// Should not break the line
		expect(formatted.trim().split('\n').length).to.equal(1);
	});
});

describe('SSL Formatter - Multi-line Comment Formatting', () => {
	const formatter = new SSLFormattingProvider();
	const options = createFormattingOptions();

	it('should preserve indentation inside multi-line comments', () => {
		const input = `/*
// This is a comment block
//     Indented explanation or pseudo-code
//     - Bullet 1
//     - Bullet 2
;`;
		const expected = `/*
// This is a comment block
//     Indented explanation or pseudo-code
//     - Bullet 1
//     - Bullet 2
;\n`;

		const doc = createDocument(input);
		const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
		const formatted = applyEdits(input, edits as any[]);

		expect(formatted).to.equal(expected, 'Indentation inside comments should be preserved');
	});

	it('should preserve spacing in ASCII diagrams within comments', () => {
		const input = `/*
    +---+
    | A |
    +---+
      |
    +---+
    | B |
    +---+
;`;
		const expected = `/*
    +---+
    | A |
    +---+
      |
    +---+
    | B |
    +---+
;\n`;

		const doc = createDocument(input);
		const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
		const formatted = applyEdits(input, edits as any[]);

		expect(formatted).to.equal(expected, 'ASCII diagrams should maintain exact spacing');
	});

	it('should preserve aligned parameter documentation in comments', () => {
		const input = `/*
Parameters:
    nValue   - The input value
    sName    - The name string
    bActive  - Active flag
;`;
		const expected = `/*
Parameters:
    nValue   - The input value
    sName    - The name string
    bActive  - Active flag
;\n`;

		const doc = createDocument(input);
		const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
		const formatted = applyEdits(input, edits as any[]);

		expect(formatted).to.equal(expected, 'Aligned documentation should be preserved');
	});
});

describe('SSL Formatter - Style Guide Fixtures', () => {
	const formatter = new SSLFormattingProvider();
	const options = createFormattingOptions();

	const fixtureDir = path.join(__dirname, 'fixtures', 'style-guide');

	// Skip if fixture directory doesn't exist
	if (!fs.existsSync(fixtureDir)) {
		it.skip('Fixture directory not found', () => {});
		return;
	}

	const fixtures = fs.readdirSync(fixtureDir)
		.filter(f => f.endsWith('-bad.ssl'))
		.map(f => f.replace('-bad.ssl', ''));

	fixtures.forEach(fixtureName => {
		it(`should format ${fixtureName} correctly`, () => {
			const badPath = path.join(fixtureDir, `${fixtureName}-bad.ssl`);
			const expectedPath = path.join(fixtureDir, `${fixtureName}-expected.ssl`);

			if (!fs.existsSync(badPath) || !fs.existsSync(expectedPath)) {
				return; // Skip if files don't exist
			}

			const input = fs.readFileSync(badPath, 'utf-8');
			const expected = fs.readFileSync(expectedPath, 'utf-8');

			// Actually call the formatter!
			const doc = createDocument(input);
			const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
			const formatted = applyEdits(input, edits as any[]);

			// Normalize line endings for cross-platform compatibility
			const normalizeLineEndings = (str: string) => str.replace(/\r\n/g, '\n');

			// Compare actual output to expected output
			expect(normalizeLineEndings(formatted)).to.equal(normalizeLineEndings(expected),
				`Formatter output for ${fixtureName} doesn't match expected.\n` +
				`Input file: ${badPath}\n` +
				`Expected file: ${expectedPath}`
			);
		});
	});

	it('should have found test fixtures', () => {
		expect(fixtures.length).to.be.greaterThan(0, 'Should have found at least one test fixture');
	});
});
