import { describe, it } from 'mocha';
import { expect } from 'chai';
import { toggleCommentLines } from '../src/utils/commentToggle';

describe('SSL Comment Toggle', () => {
	it('comments and uncomments a single line without duplicating semicolons', () => {
		const original = ':IF nValue > 0;';
		const commented = toggleCommentLines([original]);
		expect(commented.lines).to.deep.equal(['/* :IF nValue > 0;']);
		const uncommented = toggleCommentLines(commented.lines);
		expect(uncommented.lines).to.deep.equal([original]);
	});

	it('wraps multi-line selections with SSL comment markers', () => {
		const lines = ['\t:IF nValue > 0;', '\t\tnValue := 1;', '\t:ENDIF;'];
		const commented = toggleCommentLines(lines);
		expect(commented.lines).to.deep.equal(['\t/*', ...lines, '\t;']);
		const uncommented = toggleCommentLines(commented.lines);
		expect(uncommented.lines).to.deep.equal(lines);
	});

	it('preserves inline comments inside selections', () => {
		const lines = ['\tnValue := 1; /* keep me;', '\t:RETURN nValue;'];
		const commented = toggleCommentLines(lines);
		const uncommented = toggleCommentLines(commented.lines);
		expect(uncommented.lines).to.deep.equal(lines);
	});

	it('adds terminator when commenting whitespace-only lines', () => {
		const commented = toggleCommentLines(['\t   ']);
		expect(commented.lines).to.deep.equal(['\t   /* ;']);
	});

	it('uncomments multiple inline-commented lines when selected together', () => {
		const commented = ['\t/* :IF nValue > 0;', '\t/* nValue := 1;', '\t/* :ENDIF;'];
		const uncommented = toggleCommentLines(commented);
		expect(uncommented.lines).to.deep.equal(['\t:IF nValue > 0;', '\tnValue := 1;', '\t:ENDIF;']);
	});
});
