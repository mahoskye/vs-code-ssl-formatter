import { describe, it } from 'mocha';
import { expect } from 'chai';
import { SSLReferenceProvider } from '../src/sslReferenceProvider';
import { createDocument, MockPosition, MockRange } from './helpers/mockVSCode';
import { ProcedureIndex, ProcedureInfo } from '../src/utils/procedureIndex';
import * as vscode from 'vscode';

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

const provider = new SSLReferenceProvider();

const procedureFixture = [
	':PROCEDURE MainProc;',
	'\tDoProc("HelperProc", {nValue});',
	'\tExecFunction("Utilities.HelperProc", {nValue});',
	'\t/* DoProc("HelperProc", {nValue});',
	'\tsMessage := "HelperProc in plain string";',
	':ENDPROC;',
	'',
	':PROCEDURE HelperProc;',
	':ENDPROC;',
	'',
	'sHelperProc := "value";'
].join('\n');

const getLines = (text: string) => text.split('\n');

describe('SSL Reference Provider - Procedures', () => {
	it('returns only procedure definition and valid DoProc/ExecFunction references', () => {
		const document = createDocument(procedureFixture);
		const lines = getLines(procedureFixture);
		const definitionLine = lines.findIndex(line => line.includes(':PROCEDURE HelperProc;'));
		const doProcLine = lines.findIndex(line => line.includes('DoProc("HelperProc"'));
		const execLine = lines.findIndex(line => line.includes('ExecFunction("Utilities.HelperProc"'));
		const position = new MockPosition(definitionLine, lines[definitionLine].indexOf('HelperProc'));
		const references = provider.provideReferences(document as any, position as any, {} as any, {} as any);
		const referenceLines = references.map(loc => loc.range.start.line).sort();
		expect(referenceLines).to.have.members([definitionLine, doProcLine, execLine]);
	});

	it('identifies procedure references when invoked from within DoProc strings', () => {
		const document = createDocument(procedureFixture);
		const lines = getLines(procedureFixture);
		const doProcLine = lines.findIndex(line => line.includes('DoProc("HelperProc"'));
		const startCharacter = lines[doProcLine].indexOf('HelperProc');
		const position = new MockPosition(doProcLine, startCharacter);
		const references = provider.provideReferences(document as any, position as any, {} as any, {} as any);
		expect(references.length).to.equal(3);
		const refLines = references.map(loc => loc.range.start.line);
		expect(refLines).to.include(doProcLine);
	});
});

describe('SSL Reference Provider - Workspace references', () => {
	it('adds cross-file definition when ExecFunction literal uses namespace', () => {
		const stubInfo: ProcedureInfo = {
			name: 'ProcessOrder',
			uri: vscode.Uri.file('/workspace/services/Orders.ssl'),
			range: new MockRange(new MockPosition(10, 0), new MockPosition(10, 12)) as unknown as vscode.Range,
			fileBaseName: 'orders',
			scriptKeys: ['services.orders', 'orders', 'lib.orders'],
			declarationText: ':PROCEDURE ProcessOrder;',
			parameters: ['nOrderId']
		};
		const referenceProvider = new SSLReferenceProvider(new StubProcedureIndex(stubInfo));
		const text = 'ExecFunction("Services.Orders.ProcessOrder", { nOrderId });';
		const document = createDocument(text);
		const position = new MockPosition(0, text.indexOf('ProcessOrder') + 2);

		const references = referenceProvider.provideReferences(document as any, position as any, {} as any, {} as any);
		const hasWorkspaceLocation = references.some(loc => loc.uri.fsPath === stubInfo.uri.fsPath);
		expect(hasWorkspaceLocation).to.be.true;
	});
});

describe('SSL Reference Provider - Variables', () => {
	it('continues to return standard references for variables', () => {
		const text = [
			':PROCEDURE Example;',
			':DECLARE sValue;',
			'sValue := "start";',
			'CallSomething(sValue);',
			'/* sValue should be ignored ;',
			':ENDPROC;'
		].join('\n');
		const document = createDocument(text);
		const lines = getLines(text);
		const firstUsageLine = lines.findIndex(line => line.includes('sValue := '));
		const position = new MockPosition(firstUsageLine, lines[firstUsageLine].indexOf('sValue'));
		const references = provider.provideReferences(document as any, position as any, {} as any, {} as any);
		const referenceLines = references.map(loc => loc.range.start.line);
		expect(referenceLines).to.include(firstUsageLine);
		expect(referenceLines).to.include(lines.findIndex(line => line.includes('CallSomething(sValue)')));
		const commentLine = lines.findIndex(line => line.includes('ignored'));
		expect(referenceLines).to.not.include(commentLine);
	});
});
