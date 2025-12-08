import { describe, it } from 'mocha';
import { expect } from 'chai';
import { SSLInlayHintsProvider } from '../src/sslInlayHintsProvider';
import { createDocument, MockRange, MockPosition } from './helpers/mockVSCode';
import { ProcedureIndex, ProcedureInfo } from '../src/utils/procedureIndex';
import * as vscode from 'vscode';

const provider = new SSLInlayHintsProvider();

const fullRangeFor = (lineCount: number, lastLineLength: number) =>
	new MockRange(new MockPosition(0, 0), new MockPosition(lineCount - 1, lastLineLength));

describe('SSL Inlay Hints Provider', () => {
	it('provides hints for all lines within the requested range', () => {
		const document = createDocument(`:PROCEDURE Example;
result := AllTrim(sValue);
DoProc("HelperProc", {sValue, nCount});
:ENDPROC;

:PROCEDURE HelperProc;
:PARAMETERS sValue, nCount;
:ENDPROC;`);
		const range = fullRangeFor(document.lineCount, document.lineAt(document.lineCount - 1).text.length);
		const hints = provider.provideInlayHints(document as any, range as any, {} as any);
		const labels = hints.map(hint => hint.label).filter(Boolean) as string[];
		expect(labels).to.include('source:');
		expect(labels).to.include('sValue:');
		expect(labels).to.include('nCount:');
	});
});

class StubProcedureIndex implements ProcedureIndex {
	private readonly info?: ProcedureInfo;

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

	getAllProcedures(): ProcedureInfo[] {
		return this.info ? [this.info] : [];
	}

	resolveProcedureLiteral(): ProcedureInfo | undefined {
		return this.info;
	}
}

describe('SSL Inlay Hints Provider - ExecFunction resolution', () => {
	it('labels ExecFunction parameter array using resolved procedure signature', () => {
		const stubInfo: ProcedureInfo = {
			name: 'ProcessOrder',
			uri: vscode.Uri.file('/workspace/services/OrderPipeline.ssl'),
			range: new MockRange(new MockPosition(5, 0), new MockPosition(5, 12)) as unknown as vscode.Range,
			fileBaseName: 'orderpipeline',
			scriptKeys: ['services.orderpipeline', 'orderpipeline'],
			declarationText: ':PROCEDURE ProcessOrder;',
			parameters: ['nOrderId', 'sStatus']
		};
		const execProvider = new SSLInlayHintsProvider(new StubProcedureIndex(stubInfo));
		const document = createDocument(`ExecFunction("Services.OrderPipeline.ProcessOrder", { nOrderId, sStatus });`);
		const range = fullRangeFor(document.lineCount, document.lineAt(document.lineCount - 1).text.length);
		const hints = execProvider.provideInlayHints(document as any, range as any, {} as any);
		const labels = hints.map(hint => hint.label).filter(Boolean) as string[];
		expect(labels).to.include('nOrderId:');
		expect(labels).to.include('sStatus:');
	});

	it('keeps positional hint labels for builtins even when arguments are omitted', () => {
		const document = createDocument(`RunSQL(sSQL, , { nOrderId });`);
		const range = fullRangeFor(document.lineCount, document.lineAt(document.lineCount - 1).text.length);
		const hints = provider.provideInlayHints(document as any, range as any, {} as any);
		const labels = hints.map(hint => hint.label).filter(Boolean) as string[];
		expect(labels).to.include('commandString:');
		expect(labels).to.include('friendlyName:');
		expect(labels).to.include('arrayOfValues:');
	});
});

describe('SSL Inlay Hints Provider - String Exclusion (Bug #39)', () => {
	it('should not provide hints for function-like patterns inside double-quoted strings', () => {
		const document = createDocument(`sQuery := "SELECT Len(field) FROM table";`);
		const range = fullRangeFor(document.lineCount, document.lineAt(document.lineCount - 1).text.length);
		const hints = provider.provideInlayHints(document as any, range as any, {} as any);
		// Len() inside the SQL string should not get hints
		expect(hints).to.have.lengthOf(0);
	});

	it('should not provide hints for function-like patterns inside single-quoted strings', () => {
		const document = createDocument(`sQuery := 'SELECT Len(field) FROM table';`);
		const range = fullRangeFor(document.lineCount, document.lineAt(document.lineCount - 1).text.length);
		const hints = provider.provideInlayHints(document as any, range as any, {} as any);
		expect(hints).to.have.lengthOf(0);
	});

	it('should not provide hints for function-like patterns inside bracket strings', () => {
		const document = createDocument(`sQuery := [SELECT Len(field) FROM table];`);
		const range = fullRangeFor(document.lineCount, document.lineAt(document.lineCount - 1).text.length);
		const hints = provider.provideInlayHints(document as any, range as any, {} as any);
		expect(hints).to.have.lengthOf(0);
	});

	it('should provide hints for real function calls but not for patterns in SQL strings', () => {
		const document = createDocument(`nLength := Len(sValue);
sQuery := "SELECT COUNT(*), Len(name) FROM users";`);
		const range = fullRangeFor(document.lineCount, document.lineAt(document.lineCount - 1).text.length);
		const hints = provider.provideInlayHints(document as any, range as any, {} as any);
		// Should have exactly 1 hint for the real Len() call
		expect(hints).to.have.lengthOf(1);
		expect(hints[0].label).to.equal('source:');
	});

	it('should handle multiple function-like patterns in a single SQL string', () => {
		const document = createDocument(`sQuery := "SELECT Len(name), Upper(city), AllTrim(zip) FROM addresses";`);
		const range = fullRangeFor(document.lineCount, document.lineAt(document.lineCount - 1).text.length);
		const hints = provider.provideInlayHints(document as any, range as any, {} as any);
		// None of the SQL functions should get hints
		expect(hints).to.have.lengthOf(0);
	});

	it('should correctly handle mixed real calls and SQL strings on the same line', () => {
		const document = createDocument(`sResult := AllTrim(sInput) + " SELECT Upper(field) FROM table";`);
		const range = fullRangeFor(document.lineCount, document.lineAt(document.lineCount - 1).text.length);
		const hints = provider.provideInlayHints(document as any, range as any, {} as any);
		// Should only have hint for the real AllTrim() call
		expect(hints).to.have.lengthOf(1);
		expect(hints[0].label).to.equal('source:');
	});

	it('should handle DoProc calls with SQL in the parameters correctly', () => {
		const document = createDocument(`:PROCEDURE Test;
DoProc("Helper", { "SELECT id FROM table" });
:ENDPROC;

:PROCEDURE Helper;
:PARAMETERS sQuery;
:ENDPROC;`);
		const range = fullRangeFor(document.lineCount, document.lineAt(document.lineCount - 1).text.length);
		const hints = provider.provideInlayHints(document as any, range as any, {} as any);
		// Should have hint for the DoProc parameter
		const labels = hints.map(h => h.label);
		expect(labels).to.include('sQuery:');
	});
});
