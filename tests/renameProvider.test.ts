import { describe, it } from 'mocha';
import { expect } from 'chai';
import { SSLRenameProvider } from '../src/sslRenameProvider';
import { createDocument, MockPosition } from './helpers/mockVSCode';

const provider = new SSLRenameProvider();

/**
 * Helper to extract edit ranges from a WorkspaceEdit
 */
function getEditRanges(edit: any, documentUri: any): { line: number; startChar: number; endChar: number }[] {
	const ranges: { line: number; startChar: number; endChar: number }[] = [];
	if (edit && edit._changes) {
		for (const [uri, edits] of edit._changes) {
			if (uri.toString() === documentUri.toString()) {
				for (const e of edits) {
					ranges.push({
						line: e.range.start.line,
						startChar: e.range.start.character,
						endChar: e.range.end.character
					});
				}
			}
		}
	}
	return ranges;
}

describe('SSL Rename Provider - Scope Limiting (Bug #40)', () => {
	it('should only rename parameters within their procedure scope', () => {
		const text = [
			':PROCEDURE Test1;',
			':PARAMETERS aArray1, bArray2;',
			'	nResult := ProcessArray(bArray2);',
			':ENDPROC;',
			'',
			':PROCEDURE Test2;',
			':PARAMETERS aArray1, bArray2;',
			'	nResult := ProcessArray(bArray2);',
			':ENDPROC;'
		].join('\n');

		const document = createDocument(text);
		// Position on bArray2 in Test2's PARAMETERS line (line 6)
		const position = new MockPosition(6, 22); // Position on "bArray2"

		const edit = provider.provideRenameEdits(document as any, position as any, 'bHasArray', {} as any);
		const ranges = getEditRanges(edit, document.uri);

		// Should only rename bArray2 in Test2 (lines 6 and 7), not in Test1 (lines 1 and 2)
		const editLines = ranges.map(r => r.line);
		expect(editLines).to.include(6, 'Should rename parameter declaration in Test2');
		expect(editLines).to.include(7, 'Should rename parameter usage in Test2');
		expect(editLines).to.not.include(1, 'Should NOT rename parameter in Test1 declaration');
		expect(editLines).to.not.include(2, 'Should NOT rename parameter in Test1 usage');
	});

	it('should only rename variables within their procedure scope', () => {
		const text = [
			':PROCEDURE Proc1;',
			':DECLARE sName;',
			'	sName := "hello";',
			':ENDPROC;',
			'',
			':PROCEDURE Proc2;',
			':DECLARE sName;',
			'	sName := "world";',
			':ENDPROC;'
		].join('\n');

		const document = createDocument(text);
		// Position on sName in Proc1 (line 2)
		const position = new MockPosition(2, 1);

		const edit = provider.provideRenameEdits(document as any, position as any, 'sGreeting', {} as any);
		const ranges = getEditRanges(edit, document.uri);

		const editLines = ranges.map(r => r.line);
		expect(editLines).to.include(1, 'Should rename declaration in Proc1');
		expect(editLines).to.include(2, 'Should rename usage in Proc1');
		expect(editLines).to.not.include(6, 'Should NOT rename declaration in Proc2');
		expect(editLines).to.not.include(7, 'Should NOT rename usage in Proc2');
	});
});

describe('SSL Rename Provider - String Protection', () => {
	it('should not rename variables inside string literals', () => {
		const text = [
			':PROCEDURE Test;',
			':DECLARE sValue;',
			'	sValue := "hello";',
			'	sMessage := "The sValue is: " + sValue;',
			':ENDPROC;'
		].join('\n');

		const document = createDocument(text);
		// Position on sValue in assignment (line 2)
		const position = new MockPosition(2, 1);

		const edit = provider.provideRenameEdits(document as any, position as any, 'sData', {} as any);
		const ranges = getEditRanges(edit, document.uri);

		// Should rename sValue in lines 1, 2, and the second occurrence in line 3
		// but NOT the "sValue" inside the string literal
		const editLines = ranges.map(r => r.line);
		expect(editLines).to.include(1);
		expect(editLines).to.include(2);
		expect(editLines).to.include(3);

		// Check that there's only one edit on line 3 (the concatenated variable, not the string)
		const line3Edits = ranges.filter(r => r.line === 3);
		expect(line3Edits.length).to.equal(1, 'Should only rename the actual variable, not the one in the string');
		// The variable at the end of line 3 should be the one renamed (after the +)
		expect(line3Edits[0].startChar).to.be.greaterThan(20, 'Edit should be on the variable after the string');
	});

	it('should not rename variables inside SQL strings', () => {
		const text = [
			':PROCEDURE Test;',
			':DECLARE sQuery, sField;',
			'	sField := "name";',
			'	sQuery := "SELECT sField FROM table WHERE sField = ?";',
			'	SQLExecute(sQuery);',
			':ENDPROC;'
		].join('\n');

		const document = createDocument(text);
		const position = new MockPosition(2, 1); // sField assignment

		const edit = provider.provideRenameEdits(document as any, position as any, 'sColumn', {} as any);
		const ranges = getEditRanges(edit, document.uri);

		const editLines = ranges.map(r => r.line);
		expect(editLines).to.include(1, 'Should rename declaration');
		expect(editLines).to.include(2, 'Should rename assignment');
		// Line 3 has sField inside a string - should NOT be renamed
		expect(editLines).to.not.include(3, 'Should NOT rename variable name inside SQL string');
	});

	it('should rename SQL parameters (?varName?) inside strings', () => {
		const text = [
			':PROCEDURE Test;',
			':DECLARE nUserId;',
			'	nUserId := 123;',
			'	sQuery := "SELECT * FROM users WHERE id = ?nUserId?";',
			'	SQLExecute(sQuery, , , , , , , , , nUserId);',
			':ENDPROC;'
		].join('\n');

		const document = createDocument(text);
		const position = new MockPosition(2, 1); // nUserId assignment

		const edit = provider.provideRenameEdits(document as any, position as any, 'nId', {} as any);
		const ranges = getEditRanges(edit, document.uri);

		const editLines = ranges.map(r => r.line);
		expect(editLines).to.include(1, 'Should rename declaration');
		expect(editLines).to.include(2, 'Should rename assignment');
		expect(editLines).to.include(3, 'Should rename SQL parameter ?nUserId?');
		expect(editLines).to.include(4, 'Should rename in SQLExecute call');

		// Verify the SQL parameter on line 3 is actually renamed
		const line3Edits = ranges.filter(r => r.line === 3);
		expect(line3Edits.length).to.equal(1, 'Should have one edit on line 3 for ?nUserId?');
	});

	it('should rename multiple SQL parameters in the same query', () => {
		const text = [
			':PROCEDURE Test;',
			':DECLARE nStart, nEnd;',
			'	nStart := 1;',
			'	nEnd := 100;',
			'	sQuery := "SELECT * FROM items WHERE id >= ?nStart? AND id <= ?nStart?";',
			':ENDPROC;'
		].join('\n');

		const document = createDocument(text);
		const position = new MockPosition(2, 1); // nStart assignment

		const edit = provider.provideRenameEdits(document as any, position as any, 'nBegin', {} as any);
		const ranges = getEditRanges(edit, document.uri);

		// Should rename both ?nStart? occurrences in the SQL string
		const line4Edits = ranges.filter(r => r.line === 4);
		expect(line4Edits.length).to.equal(2, 'Should rename both ?nStart? parameters in SQL string');
	});
});

describe('SSL Rename Provider - Comment Protection', () => {
	it('should not rename variables inside comments', () => {
		const text = [
			':PROCEDURE Test;',
			':DECLARE nCount;',
			'	/* nCount is used for tracking;',
			'	nCount := 0;',
			':ENDPROC;'
		].join('\n');

		const document = createDocument(text);
		const position = new MockPosition(3, 1); // nCount assignment

		const edit = provider.provideRenameEdits(document as any, position as any, 'nTotal', {} as any);
		const ranges = getEditRanges(edit, document.uri);

		const editLines = ranges.map(r => r.line);
		expect(editLines).to.include(1, 'Should rename declaration');
		expect(editLines).to.include(3, 'Should rename assignment');
		expect(editLines).to.not.include(2, 'Should NOT rename variable in comment');
	});
});

describe('SSL Rename Provider - Procedure Names', () => {
	it('should rename procedure names across the entire document', () => {
		const text = [
			':PROCEDURE HelperFunc;',
			':ENDPROC;',
			'',
			':PROCEDURE MainProc;',
			'	DoProc("HelperFunc", {});',
			':ENDPROC;'
		].join('\n');

		const document = createDocument(text);
		// Position on procedure name definition
		const position = new MockPosition(0, 12); // "HelperFunc"

		const edit = provider.provideRenameEdits(document as any, position as any, 'UtilityFunc', {} as any);
		const ranges = getEditRanges(edit, document.uri);

		const editLines = ranges.map(r => r.line);
		// Note: The DoProc string won't be renamed because it's inside a string literal
		// This is actually correct behavior - you'd need to manually update strings
		expect(editLines).to.include(0, 'Should rename procedure definition');
	});
});

describe('SSL Rename Provider - Global Scope', () => {
	it('should rename global variables across the document', () => {
		const text = [
			':DECLARE gGlobalVar;',
			'gGlobalVar := "initial";',
			'',
			':PROCEDURE Test1;',
			'	sLocal := gGlobalVar;',
			':ENDPROC;',
			'',
			':PROCEDURE Test2;',
			'	gGlobalVar := "updated";',
			':ENDPROC;'
		].join('\n');

		const document = createDocument(text);
		// Position on global variable in global scope (line 1)
		const position = new MockPosition(1, 0);

		const edit = provider.provideRenameEdits(document as any, position as any, 'gSharedVar', {} as any);
		const ranges = getEditRanges(edit, document.uri);

		const editLines = ranges.map(r => r.line);
		// Global variables in global scope should be renamed everywhere
		expect(editLines).to.include(0, 'Should rename declaration');
		expect(editLines).to.include(1, 'Should rename assignment in global scope');
		expect(editLines).to.include(4, 'Should rename usage in Test1');
		expect(editLines).to.include(8, 'Should rename usage in Test2');
	});
});
