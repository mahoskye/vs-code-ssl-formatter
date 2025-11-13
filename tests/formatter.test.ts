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

			// Compare actual output to expected output
			expect(formatted).to.equal(expected,
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
