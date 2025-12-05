import { describe, it } from 'mocha';
import { expect } from 'chai';
import { SSLDefinitionProvider } from '../src/sslDefinitionProvider';
import { createDocument, MockPosition, MockRange } from './helpers/mockVSCode';
import { ProcedureIndex, ProcedureInfo } from '../src/utils/procedureIndex';
import * as vscode from 'vscode';

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

	getAllProcedures(): ProcedureInfo[] {
		return this.info ? [this.info] : [];
	}

	resolveProcedureLiteral(): ProcedureInfo | undefined {
		return this.info;
	}
}

describe('SSL Definition Provider - Workspace namespace resolution', () => {
	it('resolves ExecFunction literals across files', () => {
		const stubInfo: ProcedureInfo = {
			name: 'ProcessOrder',
			uri: vscode.Uri.file('/workspace/services/Orders.ssl'),
			range: new MockRange(new MockPosition(12, 0), new MockPosition(12, 12)) as unknown as vscode.Range,
			fileBaseName: 'orders',
			scriptKeys: ['services.orders', 'orders'],
			declarationText: ':PROCEDURE ProcessOrder;',
			parameters: ['nOrderId']
		};

		const definitionProvider = new SSLDefinitionProvider(new StubProcedureIndex(stubInfo));
		const code = `ExecFunction("Services.Orders.ProcessOrder", { nOrderId });`;
		const doc = createDocument(code);
		const position = new MockPosition(0, code.indexOf('ProcessOrder') + 2);

		const definition = definitionProvider.provideDefinition(doc as any, position as any, null as any);
		expect(definition).to.not.be.null;
		if (definition && definition instanceof vscode.Location) {
			expect(definition.uri.fsPath).to.equal(stubInfo.uri.fsPath);
		}
	});
});
