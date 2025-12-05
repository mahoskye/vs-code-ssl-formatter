import { describe, it } from 'mocha';
import { expect } from 'chai';
import { SSL_DOCUMENT_SELECTORS } from '../src/utils/documentSelectors';

type SelectorFilter = { language?: string; scheme?: string };

const normalizeSelectors = (): SelectorFilter[] => {
	const selectors = Array.isArray(SSL_DOCUMENT_SELECTORS)
		? SSL_DOCUMENT_SELECTORS
		: [SSL_DOCUMENT_SELECTORS];
	return selectors.filter((selector): selector is SelectorFilter =>
		typeof selector === 'object' && selector !== null
	);
};

describe('SSL Document Selectors', () => {
	it('should enable language features for both files and untitled editors', () => {
		const sslFilters = normalizeSelectors().filter(sel => sel.language === 'ssl');
		const schemes = new Set(sslFilters.map(sel => sel.scheme));
		expect(schemes.has('file')).to.equal(true, 'file scheme missing');
		expect(schemes.has('untitled')).to.equal(true, 'untitled scheme missing');
	});
});
