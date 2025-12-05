import { describe, it } from 'mocha';
import { expect } from 'chai';
import { SSLCompletionProvider } from '../src/sslCompletionProvider';
import { createDocument, MockPosition, MockRange } from './helpers/mockVSCode';
import { ClassIndex, ClassMembers } from '../src/utils/classIndex';
import { ProcedureIndex, ProcedureInfo } from '../src/utils/procedureIndex';
import * as vscode from 'vscode';

const createMockClassIndex = (members: ClassMembers | undefined): ClassIndex => ({
	getClassMembers: (className: string) => {
		if (members && members.className.toUpperCase() === className.toUpperCase()) {
			return members;
		}
		return undefined;
	}
});

class StubProcedureIndex implements ProcedureIndex {
	private readonly procedures: ProcedureInfo[];

	constructor(procedures: ProcedureInfo[] = []) {
		this.procedures = procedures;
	}

	async initialize(): Promise<void> {
		return;
	}

	dispose(): void {
		// no-op
	}

	getProceduresByName(name: string): ProcedureInfo[] {
		return this.procedures.filter(p => p.name.toLowerCase() === name.toLowerCase());
	}

	getAllProcedures(): ProcedureInfo[] {
		return this.procedures;
	}

	resolveProcedureLiteral(literal: string): ProcedureInfo | undefined {
		const parts = literal.split('.');
		const procName = parts[parts.length - 1];
		return this.procedures.find(p => p.name.toLowerCase() === procName.toLowerCase());
	}
}

describe('SSL Completion Provider - CreateUDObject member filtering', () => {
	it('returns class members from the workspace index', () => {
		const mockMembers: ClassMembers = {
			className: 'OrderLogger',
			methods: ['Initialize', 'Save'],
			properties: ['sName']
		};
		const completionProvider = new SSLCompletionProvider(createMockClassIndex(mockMembers));

		const code = `oLogger := CreateUdObject("Namespace.OrderLogger");
oLogger:
`;
		const doc = createDocument(code);
		const position = new MockPosition(1, 8); // After 'oLogger:'
		const context = { triggerCharacter: ':', triggerKind: 1 } as any;

		const completions = completionProvider.provideCompletionItems(doc as any, position as any, null as any, context) as any[];
		const labels = completions.map(item => item.label);

		expect(labels).to.include('Initialize');
		expect(labels).to.include('Save');
		expect(labels).to.include('sName');
	});
});

describe('SSL Completion Provider - Anonymous UDObject members (Issue #18)', () => {
	const completionProvider = new SSLCompletionProvider();

	it('returns built-in UDObject methods for anonymous objects', () => {
		const code = `oUser := CreateUDObject();
oUser:
`;
		const doc = createDocument(code);
		const position = new MockPosition(1, 6); // After 'oUser:'
		const context = { triggerCharacter: ':', triggerKind: 1 } as any;

		const completions = completionProvider.provideCompletionItems(doc as any, position as any, null as any, context) as any[];
		const labels = completions.map(item => item.label);

		// Built-in methods
		expect(labels).to.include('AddProperty');
		expect(labels).to.include('IsProperty');
		expect(labels).to.include('AddMethod');
		expect(labels).to.include('IsMethod');
		expect(labels).to.include('Clone');
		expect(labels).to.include('Serialize');
		expect(labels).to.include('Deserialize');

		// Built-in property
		expect(labels).to.include('xmltype');
	});

	it('returns user-defined properties assigned to the object', () => {
		const code = `oConfig := CreateUDObject();
oConfig:Host := "localhost";
oConfig:Port := 8080;
oConfig:
`;
		const doc = createDocument(code);
		const position = new MockPosition(3, 8); // After 'oConfig:'
		const context = { triggerCharacter: ':', triggerKind: 1 } as any;

		const completions = completionProvider.provideCompletionItems(doc as any, position as any, null as any, context) as any[];
		const labels = completions.map(item => item.label);

		expect(labels).to.include('Host');
		expect(labels).to.include('Port');
	});

	it('returns properties added via AddProperty()', () => {
		const code = `oConfig := CreateUDObject();
oConfig:AddProperty("Timeout", 30);
oConfig:AddProperty("MaxRetries", 5);
oConfig:
`;
		const doc = createDocument(code);
		const position = new MockPosition(3, 8);
		const context = { triggerCharacter: ':', triggerKind: 1 } as any;

		const completions = completionProvider.provideCompletionItems(doc as any, position as any, null as any, context) as any[];
		const labels = completions.map(item => item.label);

		expect(labels).to.include('Timeout');
		expect(labels).to.include('MaxRetries');
	});

	it('returns methods added via AddMethod()', () => {
		const code = `oHandler := CreateUDObject();
oHandler:AddMethod("Connect", "ConnectToServer");
oHandler:AddMethod("Disconnect", "DisconnectFromServer");
oHandler:
`;
		const doc = createDocument(code);
		const position = new MockPosition(3, 9);
		const context = { triggerCharacter: ':', triggerKind: 1 } as any;

		const completions = completionProvider.provideCompletionItems(doc as any, position as any, null as any, context) as any[];
		const labels = completions.map(item => item.label);

		expect(labels).to.include('Connect');
		expect(labels).to.include('Disconnect');
	});

	it('does not show global variables as object members', () => {
		const code = `:DECLARE sGlobalVar;
sGlobalVar := "test";
oUser := CreateUDObject();
oUser:Name := "John";
oUser:
`;
		const doc = createDocument(code);
		const position = new MockPosition(4, 6);
		const context = { triggerCharacter: ':', triggerKind: 1 } as any;

		const completions = completionProvider.provideCompletionItems(doc as any, position as any, null as any, context) as any[];
		const labels = completions.map(item => item.label);

		// Should include object members
		expect(labels).to.include('Name');
		expect(labels).to.include('AddProperty');

		// Should NOT include global variables
		expect(labels).to.not.include('sGlobalVar');
	});

	it('sorts built-in members before user-defined members', () => {
		const code = `oObj := CreateUDObject();
oObj:CustomProp := "value";
oObj:
`;
		const doc = createDocument(code);
		const position = new MockPosition(2, 5);
		const context = { triggerCharacter: ':', triggerKind: 1 } as any;

		const completions = completionProvider.provideCompletionItems(doc as any, position as any, null as any, context) as any[];

		// Built-in members should have sortText starting with '0_'
		const addPropertyItem = completions.find((c: any) => c.label === 'AddProperty');
		const customPropItem = completions.find((c: any) => c.label === 'CustomProp');

		expect(addPropertyItem.sortText).to.match(/^0_/);
		expect(customPropItem.sortText).to.match(/^1_/);
	});
});

describe('SSL Completion Provider - DoProc/ExecFunction procedure completions (Issue #16)', () => {
	it('returns current file procedures for DoProc completion', () => {
		const completionProvider = new SSLCompletionProvider();
		const code = `:PROCEDURE MainProc;
	DoProc("
:ENDPROC;

:PROCEDURE HelperProc;
:ENDPROC;

:PROCEDURE AnotherHelper;
:ENDPROC;
`;
		const doc = createDocument(code);
		// Position is inside DoProc("
		const position = new MockPosition(1, 9);
		const context = { triggerCharacter: '"', triggerKind: 1 } as any;

		const completions = completionProvider.provideCompletionItems(doc as any, position as any, null as any, context) as any[];
		const labels = completions.map(item => item.label);

		expect(labels).to.include('MainProc');
		expect(labels).to.include('HelperProc');
		expect(labels).to.include('AnotherHelper');
	});

	it('returns current file procedures for ExecFunction completion', () => {
		const completionProvider = new SSLCompletionProvider();
		const code = `:PROCEDURE MainProc;
	result := ExecFunction("
:ENDPROC;

:PROCEDURE ProcessData;
:ENDPROC;
`;
		const doc = createDocument(code);
		// Position is inside ExecFunction(" - the line is `\tresult := ExecFunction("`
		// Tab(0) r(1)e(2)s(3)u(4)l(5)t(6) (7):=(8)(9) (10)E(11)x(12)e(13)c(14)F(15)u(16)n(17)c(18)t(19)i(20)o(21)n(22)((23)"(24)
		const position = new MockPosition(1, 25);
		const context = { triggerCharacter: '"', triggerKind: 1 } as any;

		const completions = completionProvider.provideCompletionItems(doc as any, position as any, null as any, context) as any[];
		const labels = completions.map(item => item.label);

		expect(labels).to.include('MainProc');
		expect(labels).to.include('ProcessData');
	});

	it('returns workspace procedures with qualified names', () => {
		const workspaceProc: ProcedureInfo = {
			name: 'ValidateInput',
			uri: vscode.Uri.file('/workspace/utils/Validation.ssl'),
			range: new MockRange(new MockPosition(5, 0), new MockPosition(5, 13)) as unknown as vscode.Range,
			fileBaseName: 'validation',
			scriptKeys: ['utils.validation', 'validation'],
			declarationText: ':PROCEDURE ValidateInput;',
			parameters: ['sInput', 'bStrict']
		};

		const procedureIndex = new StubProcedureIndex([workspaceProc]);
		const completionProvider = new SSLCompletionProvider(undefined, procedureIndex);

		const code = `:PROCEDURE MainProc;
	result := ExecFunction("
:ENDPROC;
`;
		const doc = createDocument(code);
		// Position after ExecFunction(" - see comment in previous test
		const position = new MockPosition(1, 25);
		const context = { triggerCharacter: '"', triggerKind: 1 } as any;

		const completions = completionProvider.provideCompletionItems(doc as any, position as any, null as any, context) as any[];
		const labels = completions.map(item => item.label);

		// Should include workspace procedure with qualified name
		expect(labels).to.include('Validation.ValidateInput');
	});

	it('sorts current file procedures before workspace procedures', () => {
		const workspaceProc: ProcedureInfo = {
			name: 'ExternalHelper',
			uri: vscode.Uri.file('/workspace/lib/Helpers.ssl'),
			range: new MockRange(new MockPosition(0, 0), new MockPosition(0, 14)) as unknown as vscode.Range,
			fileBaseName: 'helpers',
			scriptKeys: ['lib.helpers', 'helpers'],
			declarationText: ':PROCEDURE ExternalHelper;',
			parameters: []
		};

		const procedureIndex = new StubProcedureIndex([workspaceProc]);
		const completionProvider = new SSLCompletionProvider(undefined, procedureIndex);

		const code = `:PROCEDURE LocalProc;
	DoProc("
:ENDPROC;
`;
		const doc = createDocument(code);
		const position = new MockPosition(1, 9);
		const context = { triggerCharacter: '"', triggerKind: 1 } as any;

		const completions = completionProvider.provideCompletionItems(doc as any, position as any, null as any, context) as any[];

		const localItem = completions.find((c: any) => c.label === 'LocalProc');
		const workspaceItem = completions.find((c: any) => c.label === 'Helpers.ExternalHelper');

		// Local procedures should sort before workspace procedures
		expect(localItem.sortText).to.match(/^0_/);
		expect(workspaceItem.sortText).to.match(/^1_/);
	});
});
