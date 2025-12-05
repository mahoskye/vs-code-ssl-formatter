import { describe, it } from 'mocha';
import { expect } from 'chai';
import { SSLCompletionProvider } from '../src/sslCompletionProvider';
import { createDocument, MockPosition } from './helpers/mockVSCode';
import { ClassIndex, ClassMembers } from '../src/utils/classIndex';

const createMockClassIndex = (members: ClassMembers | undefined): ClassIndex => ({
	getClassMembers: (className: string) => {
		if (members && members.className.toUpperCase() === className.toUpperCase()) {
			return members;
		}
		return undefined;
	}
});

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
