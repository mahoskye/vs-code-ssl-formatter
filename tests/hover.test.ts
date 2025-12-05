/**
 * SSL Hover Provider Unit Tests
 * Tests the hover provider using mock VSCode API
 */

import { describe, it } from 'mocha';
import { expect } from 'chai';
import { SSLHoverProvider } from '../src/sslHoverProvider';
import { createDocument, MockPosition, MockRange } from './helpers/mockVSCode';
import { ProcedureIndex, ProcedureInfo } from '../src/utils/procedureIndex';
import * as vscode from 'vscode';

const hoverText = (hover: any): string => {
	if (!hover || !hover.contents) {
		return '';
	}
	if (typeof hover.contents.toString === 'function') {
		return hover.contents.toString();
	}
	return String(hover.contents);
};

describe('SSL Hover Provider - String Literal Exclusion (Bug #27)', () => {
	const hoverProvider = new SSLHoverProvider();

	it('should not provide hover hints for SQL functions inside string literals', () => {
		const code = `sql := "SELECT substr(col_name, 1, 3) FROM table_name";`;
		const doc = createDocument(code);

		// Position of "substr" inside the string (line 0, character ~15)
		const position = new MockPosition(0, 15);

		const hover = hoverProvider.provideHover(doc as any, position as any, null as any);

		// Should NOT provide hover for functions inside strings
		expect(hover).to.be.null;
	});

	it('should not provide hover hints for SQL keywords inside string literals', () => {
		const code = `sql := "SELECT * FROM table WHERE id = 1";`;
		const doc = createDocument(code);

		// Position of "SELECT" inside the string
		const position = new MockPosition(0, 9);

		const hover = hoverProvider.provideHover(doc as any, position as any, null as any);

		// Should NOT provide hover for keywords inside strings
		expect(hover).to.be.null;
	});

	it('should not provide hover hints for functions in multi-line SQL strings', () => {
		const code = `query := SQLExecute("
    SELECT col1,
        CASE
            WHEN substr(colX, 1, 1) = 'A' THEN 'TypeA'
            ELSE 'Other'
        END AS category
    FROM table1
");`;
		const doc = createDocument(code);

		// Position of "substr" inside the multi-line string (line 3, character ~18)
		const position = new MockPosition(3, 18);

		const hover = hoverProvider.provideHover(doc as any, position as any, null as any);

		// Should NOT provide hover for functions inside multi-line strings
		expect(hover).to.be.null;
	});

	it('should not provide hover hints for functions mentioned in comments', () => {
		const code = `/* This query uses substr() to extract part of a string;`;
		const doc = createDocument(code);

		// Position of "substr" inside the comment
		const position = new MockPosition(0, 20);

		const hover = hoverProvider.provideHover(doc as any, position as any, null as any);

		// Should NOT provide hover for functions inside comments
		expect(hover).to.be.null;
	});

	it('should not provide hover for regex patterns inside strings', () => {
		const code = `pattern := "REGEXP_REPLACE(col, '[(|)]', '')";`;
		const doc = createDocument(code);

		// Position inside the regex pattern
		const position = new MockPosition(0, 30);

		const hover = hoverProvider.provideHover(doc as any, position as any, null as any);

		// Should NOT provide hover inside strings
		expect(hover).to.be.null;
	});

	it('should provide hover for functions OUTSIDE string literals', () => {
		const code = `result := AllTrim("  test  ");`;
		const doc = createDocument(code);

		// Position of "AllTrim" outside the string
		const position = new MockPosition(0, 12);

		const hover = hoverProvider.provideHover(doc as any, position as any, null as any);

		// SHOULD provide hover for actual code outside strings
		expect(hover).to.not.be.undefined;
		if (hover) {
			expect(hover).to.have.property('contents');
			const content = hoverText(hover);
			expect(content).to.not.include('Usage Frequency');
		}
	});

	it('should provide hover for DoProc outside strings but not for "DoProc" mentioned in strings', () => {
		const code = `result := DoProc("ProcName", {}); /*Call DoProc with args;`;
		const doc = createDocument(code);

		// Position of actual DoProc call (outside string)
		const position = new MockPosition(0, 12);
		const hoverOutside = hoverProvider.provideHover(doc as any, position as any, null as any);

		// SHOULD provide hover for actual DoProc
		expect(hoverOutside).to.not.be.undefined;
		if (hoverOutside) {
			const content = hoverText(hoverOutside);
			expect(content).to.contain('DoProc(string procedureName, object[] parameters)');
		}

		// Position of "DoProc" inside comment
		const positionInComment = new MockPosition(0, 41);
		const hoverInComment = hoverProvider.provideHover(doc as any, positionInComment as any, null as any);

		// Should NOT provide hover inside comment
		expect(hoverInComment).to.be.null;
	});

	it('should handle single-quoted strings', () => {
		const code = `sql := 'SELECT substr(col, 1, 3) FROM table';`;
		const doc = createDocument(code);

		// Position of "substr" inside single-quoted string
		const position = new MockPosition(0, 15);

		const hover = hoverProvider.provideHover(doc as any, position as any, null as any);

		// Should NOT provide hover for functions inside single-quoted strings
		expect(hover).to.be.null;
	});

	it('should handle bracket-quoted strings', () => {
		const code = `sql := [SELECT substr(col, 1, 3) FROM table];`;
		const doc = createDocument(code);

		// Position of "substr" inside bracket-quoted string
		const position = new MockPosition(0, 15);

		const hover = hoverProvider.provideHover(doc as any, position as any, null as any);

		// Should NOT provide hover for functions inside bracket-quoted strings
		expect(hover).to.be.null;
	});
});

class StubProcedureIndex implements ProcedureIndex {
	private info?: ProcedureInfo;

	constructor(info?: ProcedureInfo) {
		this.info = info;
	}

	async initialize(): Promise<void> {
		return;
	}

	dispose(): void {
		// no-op
	}

	getProceduresByName(): ProcedureInfo[] {
		return this.info ? [this.info] : [];
	}

	resolveProcedureLiteral(): ProcedureInfo | undefined {
		return this.info;
	}
}

describe('SSL Hover Provider - Workspace procedure resolution', () => {
const stubInfo: ProcedureInfo = {
	name: 'ProcessOrder',
	uri: vscode.Uri.file('/workspace/services/Orders.ssl'),
	range: new MockRange(new MockPosition(10, 0), new MockPosition(10, 12)) as unknown as vscode.Range,
	fileBaseName: 'orders',
	scriptKeys: ['services.orders', 'orders'],
	declarationText: ':PROCEDURE ProcessOrder;',
	parameters: ['nOrderId', 'sStatus']
};

	const hoverProvider = new SSLHoverProvider(undefined, new StubProcedureIndex(stubInfo));

	it('provides hover for ExecFunction strings using workspace index', () => {
		const code = `ExecFunction("Services.Orders.ProcessOrder", { nOrderId });`;
		const doc = createDocument(code);
		const position = new MockPosition(0, code.indexOf('ProcessOrder'));

		const hover = hoverProvider.provideHover(doc as any, position as any, null as any);
		expect(hover).to.not.be.null;
		if (hover) {
			const content = hoverText(hover);
			expect(content).to.include('ProcessOrder');
			expect(content).to.include('Located in');
			expect(content).to.include('nOrderId');
			expect(content).to.include('sStatus');
		}
	});
});

describe('SSL Hover Provider - SQL placeholder hints', () => {
	const hoverProvider = new SSLHoverProvider();

	it('provides hover for named placeholders', () => {
		const code = `sql := "SELECT * FROM Patients WHERE PatientID = ?sPatientId?";`;
		const doc = createDocument(code);
		const column = code.indexOf('?sPatientId?') + 2;
		const position = new MockPosition(0, column);

		const hover = hoverProvider.provideHover(doc as any, position as any, null as any);
		expect(hover).to.not.be.null;
		if (hover) {
			const content = hoverText(hover);
			expect(content).to.contain('Named SQL parameter');
			expect(content).to.contain('sPatientId');
		}
	});

	it('provides hover for positional placeholders', () => {
		const code = `RunSQL("UPDATE Foo SET Bar = ? WHERE Baz = ?", , , { sBar, sBaz });`;
		const doc = createDocument(code);
		const firstQuestion = code.indexOf('?');
		const position = new MockPosition(0, firstQuestion);

		const hover = hoverProvider.provideHover(doc as any, position as any, null as any);
		expect(hover).to.not.be.null;
		if (hover) {
			const content = hoverText(hover);
			expect(content).to.contain('Positional SQL placeholder');
		}
	});
});

describe('SSL Hover Provider - Enhanced SQL Parameter Hints (Issues #13, #15)', () => {
	const hoverProvider = new SSLHoverProvider();

	it('shows parameter index for positional placeholders', () => {
		const code = `RunSQL("SELECT * FROM Customers WHERE Country = ? AND City = ?", , , { "Germany", "Berlin" });`;
		const doc = createDocument(code);
		// Second ? placeholder
		const secondQuestion = code.indexOf('?', code.indexOf('?') + 1);
		const position = new MockPosition(0, secondQuestion);

		const hover = hoverProvider.provideHover(doc as any, position as any, null as any);
		expect(hover).to.not.be.null;
		if (hover) {
			const content = hoverText(hover);
			expect(content).to.contain('Parameter 2');
		}
	});

	it('shows value for positional placeholders from RunSQL parameters array', () => {
		const code = `RunSQL("SELECT * FROM Customers WHERE Country = ?", , , { "Germany" });`;
		const doc = createDocument(code);
		const questionPos = code.indexOf('?');
		const position = new MockPosition(0, questionPos);

		const hover = hoverProvider.provideHover(doc as any, position as any, null as any);
		expect(hover).to.not.be.null;
		if (hover) {
			const content = hoverText(hover);
			expect(content).to.contain('Value:');
			expect(content).to.contain('"Germany"');
		}
	});

	it('shows value for named placeholder with variable lookup', () => {
		const code = `:DECLARE sCountry;
sCountry := "Germany";
sql := "SELECT * FROM Customers WHERE Country = ?sCountry?";`;
		const doc = createDocument(code);
		const paramPos = code.indexOf('?sCountry?') + 2;
		const position = new MockPosition(2, paramPos - code.lastIndexOf('\n', paramPos) - 1);

		const hover = hoverProvider.provideHover(doc as any, position as any, null as any);
		expect(hover).to.not.be.null;
		if (hover) {
			const content = hoverText(hover);
			expect(content).to.contain('Named SQL parameter');
			expect(content).to.contain('sCountry');
			expect(content).to.contain('Value:');
			expect(content).to.contain('"Germany"');
		}
	});

	it('shows declaration line for named placeholder', () => {
		const code = `:DECLARE sId;
sId := 123;
sql := "SELECT * FROM Users WHERE Id = ?sId?";`;
		const doc = createDocument(code);
		const lines = code.split('\n');
		const lastLine = lines[lines.length - 1];
		const paramPos = lastLine.indexOf('?sId?') + 2;
		const position = new MockPosition(2, paramPos);

		const hover = hoverProvider.provideHover(doc as any, position as any, null as any);
		expect(hover).to.not.be.null;
		if (hover) {
			const content = hoverText(hover);
			expect(content).to.contain('Declared at line 1');
		}
	});

	it('shows array element value for indexed named placeholder', () => {
		const code = `:DECLARE aCountries;
aCountries := { "Germany", "France", "Spain" };
sql := "SELECT * FROM Customers WHERE Country = ?aCountries[2]?";`;
		const doc = createDocument(code);
		const lines = code.split('\n');
		const lastLine = lines[lines.length - 1];
		const paramPos = lastLine.indexOf('?aCountries[2]?') + 2;
		const position = new MockPosition(2, paramPos);

		const hover = hoverProvider.provideHover(doc as any, position as any, null as any);
		expect(hover).to.not.be.null;
		if (hover) {
			const content = hoverText(hover);
			expect(content).to.contain('aCountries[2]');
			expect(content).to.contain('Value:');
			expect(content).to.contain('"France"');
		}
	});

	it('warns when variable not found for named placeholder', () => {
		const code = `sql := "SELECT * FROM Users WHERE Id = ?nUndeclaredVar?";`;
		const doc = createDocument(code);
		const paramPos = code.indexOf('?nUndeclaredVar?') + 2;
		const position = new MockPosition(0, paramPos);

		const hover = hoverProvider.provideHover(doc as any, position as any, null as any);
		expect(hover).to.not.be.null;
		if (hover) {
			const content = hoverText(hover);
			expect(content).to.contain('not found');
		}
	});

	it('shows LSearch function name for positional placeholder', () => {
		const code = `LSearch("SELECT * FROM Users WHERE Id = ?", { nUserId });`;
		const doc = createDocument(code);
		const questionPos = code.indexOf('?');
		const position = new MockPosition(0, questionPos);

		const hover = hoverProvider.provideHover(doc as any, position as any, null as any);
		expect(hover).to.not.be.null;
		if (hover) {
			const content = hoverText(hover);
			expect(content).to.contain('LSearch');
		}
	});

	it('warns when more placeholders than parameters', () => {
		const code = `RunSQL("SELECT * FROM T WHERE A = ? AND B = ? AND C = ?", , , { "x" });`;
		const doc = createDocument(code);
		// Third ? placeholder
		let pos = code.indexOf('?');
		pos = code.indexOf('?', pos + 1);
		pos = code.indexOf('?', pos + 1);
		const position = new MockPosition(0, pos);

		const hover = hoverProvider.provideHover(doc as any, position as any, null as any);
		expect(hover).to.not.be.null;
		if (hover) {
			const content = hoverText(hover);
			expect(content).to.contain('No matching parameter');
		}
	});
});

describe('SSL Hover Provider - Comment Exclusion (Bug #21)', () => {
	const hoverProvider = new SSLHoverProvider();

	it('should not provide hover for keywords inside comments', () => {
		const code = `/* Use IF statement to check condition;`;
		const doc = createDocument(code);

		// Position of "IF" inside comment
		const position = new MockPosition(0, 7);

		const hover = hoverProvider.provideHover(doc as any, position as any, null as any);

		// Should NOT provide hover for keywords inside comments
		expect(hover).to.be.null;
	});

	it('should not provide hover for parentheses content inside comments', () => {
		const code = `/*
Parameters:
    bLog - Whether to log operations (.T. or .F.)
;`;
		const doc = createDocument(code);

		// Position of ".T." inside comment
		const position = new MockPosition(2, 35);

		const hover = hoverProvider.provideHover(doc as any, position as any, null as any);

		// Should NOT provide hover inside comments
		expect(hover).to.be.null;
	});

	it('should not provide hover for variable names mentioned in comments', () => {
		const code = `/* The variable nCount is used for tracking;`;
		const doc = createDocument(code);

		// Position of "nCount" inside comment
		const position = new MockPosition(0, 18);

		const hover = hoverProvider.provideHover(doc as any, position as any, null as any);

		// Should NOT provide hover for variables inside comments
		expect(hover).to.be.null;
	});
});
