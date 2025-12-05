import { describe, it, beforeEach } from 'mocha';
import { expect } from 'chai';
import * as vscode from 'vscode';
import { SSLCompletionProvider } from '../src/sslCompletionProvider';
import { createDocument } from './helpers/mockVSCode';

const positionAt = (line: number, character: number) => new vscode.Position(line, character);

describe('SSL Completion Provider - Category toggles', () => {
	const provider = new SSLCompletionProvider();
	const baseDoc = createDocument(':PROCEDURE Test;\n:PARAMETERS a;\n'); // simple document

	beforeEach(() => {
		// Reset toggles to defaults before each test
		const config = vscode.workspace.getConfiguration('ssl');
		config.update('ssl.intellisense.completion.enableKeywords', true);
		config.update('ssl.intellisense.completion.enableBuiltinFunctions', true);
		config.update('ssl.intellisense.completion.enableBuiltinClasses', true);
		config.update('ssl.intellisense.completion.enableSnippets', true);
	});

	it('respects keyword toggle', () => {
		const config = vscode.workspace.getConfiguration('ssl');
		config.update('ssl.intellisense.completion.enableKeywords', false);
		const completions = provider.provideCompletionItems(baseDoc as any, positionAt(0, 0), {} as any, {} as any);
		const keywordItems = completions.filter(item => item.kind === vscode.CompletionItemKind.Keyword);
		expect(keywordItems.length).to.equal(0);
	});

	it('respects function toggle', () => {
		const config = vscode.workspace.getConfiguration('ssl');
		config.update('ssl.intellisense.completion.enableBuiltinFunctions', false);
		const completions = provider.provideCompletionItems(baseDoc as any, positionAt(0, 0), {} as any, {} as any);
		const functionItems = completions.filter(item => item.kind === vscode.CompletionItemKind.Function);
		expect(functionItems.length).to.equal(0);
	});

	it('respects class toggle', () => {
		const config = vscode.workspace.getConfiguration('ssl');
		config.update('ssl.intellisense.completion.enableBuiltinClasses', false);
		const completions = provider.provideCompletionItems(baseDoc as any, positionAt(0, 0), {} as any, {} as any);
		const classItems = completions.filter(item => item.kind === vscode.CompletionItemKind.Class);
		expect(classItems.length).to.equal(0);
	});

	it('respects snippets toggle', () => {
		const config = vscode.workspace.getConfiguration('ssl');
		config.update('ssl.intellisense.completion.enableSnippets', false);
		const completions = provider.provideCompletionItems(baseDoc as any, positionAt(0, 0), {} as any, {} as any);
		const snippetItems = completions.filter(item => item.kind === vscode.CompletionItemKind.Snippet);
		expect(snippetItems.length).to.equal(0);
	});
});

describe('SSL Completion Provider - Custom definitions', () => {
	const provider = new SSLCompletionProvider();
	const config = vscode.workspace.getConfiguration('ssl');

	afterEach(() => {
		config.update('ssl.intellisense.customFunctions', []);
		config.update('ssl.intellisense.customClasses', []);
	});

	it('adds custom functions from configuration', () => {
		config.update('ssl.intellisense.customFunctions', [
			{ name: 'CustomFunc', description: 'Project helper', params: '(value)' }
		]);

		const completions = provider.provideCompletionItems(
			createDocument('') as any,
			positionAt(0, 0),
			{} as any,
			{} as any
		);

		const customItems = completions.filter(item => item.label === 'CustomFunc' && item.kind === vscode.CompletionItemKind.Function);
		expect(customItems).to.have.length(1);
		expect(customItems[0].detail).to.equal('Project helper');
	});

	it('overrides built-in metadata when names match', () => {
		config.update('ssl.intellisense.customFunctions', [
			{ name: 'SQLExecute', description: 'Overrides default SQL helper', params: '(query)' }
		]);

		const completions = provider.provideCompletionItems(
			createDocument('') as any,
			positionAt(0, 0),
			{} as any,
			{} as any
		);

		const sqlItems = completions.filter(item => item.label === 'SQLExecute');
		expect(sqlItems).to.have.length(1);
		expect(sqlItems[0].detail).to.equal('Overrides default SQL helper');
	});

	it('exposes custom classes and their members', () => {
		config.update('ssl.intellisense.customClasses', [
			{ name: 'MyClass', description: 'Custom class', methods: ['Ping'], properties: ['State'] }
		]);

		const completions = provider.provideCompletionItems(
			createDocument('') as any,
			positionAt(0, 0),
			{} as any,
			{} as any
		);

		expect(completions.some(item => item.label === 'MyClass' && item.kind === vscode.CompletionItemKind.Class)).to.be.true;

		const memberDoc = createDocument('oObj := MyClass{};\noObj:');
		const memberCompletions = provider.provideCompletionItems(
			memberDoc as any,
			positionAt(1, 5),
			{} as any,
			{ triggerCharacter: ':' } as any
		);

		expect(memberCompletions.some(item => item.label === 'Ping')).to.be.true;
		expect(memberCompletions.some(item => item.label === 'State')).to.be.true;
	});
});
