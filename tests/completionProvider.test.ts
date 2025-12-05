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
