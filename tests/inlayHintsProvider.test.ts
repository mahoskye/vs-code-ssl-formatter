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
		expect(labels).to.include('string:');
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
		const document = createDocument(`RunSQL(sSQL, , , { nOrderId });`);
		const range = fullRangeFor(document.lineCount, document.lineAt(document.lineCount - 1).text.length);
		const hints = provider.provideInlayHints(document as any, range as any, {} as any);
		const labels = hints.map(hint => hint.label).filter(Boolean) as string[];
		expect(labels).to.include('query:');
		expect(labels).to.include('connectionName:');
		expect(labels).to.include('returnRecords:');
		expect(labels).to.include('parameters:');
	});
});
