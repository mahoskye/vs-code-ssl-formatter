import { describe, it } from 'mocha';
import { expect } from 'chai';
import { SSLSymbolProvider } from '../src/sslSymbolProvider';
import { createDocument, MockSymbolKind } from './helpers/mockVSCode';

describe('SSL Document Symbol Provider (Bug #12)', () => {
	const provider = new SSLSymbolProvider();

	it('should provide symbols for procedures', () => {
		const document = createDocument(`:PROCEDURE Initialize;
	sName := "Test";
	nCount := 0;
:ENDPROC;`);

		const symbols = provider.provideDocumentSymbols(document as any, null as any);

		expect(symbols).to.have.length(1);
		expect(symbols[0].name).to.equal('Initialize');
		expect(symbols[0].kind).to.equal(MockSymbolKind.Function);
	});

	it('should provide symbols for classes', () => {
		const document = createDocument(`:CLASS MyClass;
:PROCEDURE DoSomething;
	/* implementation */
:ENDPROC;
:ENDCLASS;`);

		const symbols = provider.provideDocumentSymbols(document as any, null as any);

		expect(symbols).to.have.length(1);
		expect(symbols[0].name).to.equal('MyClass');
		expect(symbols[0].kind).to.equal(MockSymbolKind.Class);
		expect(symbols[0].children).to.have.length(1);
		expect(symbols[0].children[0].name).to.equal('DoSomething');
	});

	it('should provide symbols for multiple procedures', () => {
		const document = createDocument(`:PROCEDURE First;
:ENDPROC;

:PROCEDURE Second;
:ENDPROC;

:PROCEDURE Third;
:ENDPROC;`);

		const symbols = provider.provideDocumentSymbols(document as any, null as any);

		expect(symbols).to.have.length(3);
		expect(symbols[0].name).to.equal('First');
		expect(symbols[1].name).to.equal('Second');
		expect(symbols[2].name).to.equal('Third');
	});

	it('should handle nested class procedures', () => {
		const document = createDocument(`:CLASS Calculator;

:PROCEDURE Init;
	Me:nValue := 0;
:ENDPROC;

:PROCEDURE Add;
:PARAMETERS nAmount;
	Me:nValue := Me:nValue + nAmount;
:ENDPROC;

:ENDCLASS;`);

		const symbols = provider.provideDocumentSymbols(document as any, null as any);

		expect(symbols).to.have.length(1);
		expect(symbols[0].name).to.equal('Calculator');
		expect(symbols[0].kind).to.equal(MockSymbolKind.Class);
		expect(symbols[0].children).to.have.length(2);
		expect(symbols[0].children[0].name).to.equal('Init');
		expect(symbols[0].children[1].name).to.equal('Add');
	});

	it('should provide symbols for regions', () => {
		const document = createDocument(`:REGION Initialization;

:PROCEDURE Setup;
:ENDPROC;

:ENDREGION;`);

		const symbols = provider.provideDocumentSymbols(document as any, null as any);

		expect(symbols).to.have.length(1);
		expect(symbols[0].name).to.equal('Initialization');
		expect(symbols[0].kind).to.equal(MockSymbolKind.Namespace);
		expect(symbols[0].children).to.have.length(1);
		expect(symbols[0].children[0].name).to.equal('Setup');
	});

	it('should handle comment-style regions', () => {
		const document = createDocument(`/* region Helper Functions;

:PROCEDURE HelperOne;
:ENDPROC;

:PROCEDURE HelperTwo;
:ENDPROC;

/* endregion;`);

		const symbols = provider.provideDocumentSymbols(document as any, null as any);

		expect(symbols).to.have.length(1);
		expect(symbols[0].name).to.equal('Helper Functions');
		expect(symbols[0].kind).to.equal(MockSymbolKind.Namespace);
		expect(symbols[0].children).to.have.length(2);
	});

	it('should return empty array for file with no symbols', () => {
		const document = createDocument(`/* Just a comment file;
Some notes here
More notes
;`);

		const symbols = provider.provideDocumentSymbols(document as any, null as any);

		expect(symbols).to.be.an('array');
		expect(symbols).to.have.length(0);
	});

	it('should handle complex document structure', () => {
		const document = createDocument(`:DECLARE sGlobalVar;

:REGION Main Logic;

:PROCEDURE MainEntry;
:PARAMETERS sInput;
	:DECLARE sResult;
	sResult := ProcessData(sInput);
	:RETURN sResult;
:ENDPROC;

:ENDREGION;

:CLASS DataProcessor;

:PROCEDURE ProcessData;
:PARAMETERS sData;
	:RETURN AllTrim(sData);
:ENDPROC;

:ENDCLASS;`);

		const symbols = provider.provideDocumentSymbols(document as any, null as any);

		// Should have: 1 region + 1 class (DECLARE is not tracked as symbol)
		expect(symbols.length).to.be.greaterThan(0);

		const regionSymbol = symbols.find(s => s.name === 'Main Logic');
		expect(regionSymbol).to.exist;
		expect(regionSymbol?.children).to.have.length(1);

		const classSymbol = symbols.find(s => s.name === 'DataProcessor');
		expect(classSymbol).to.exist;
		expect(classSymbol?.children).to.have.length(1);
	});
});
