import * as assert from 'assert';
import * as vscode from 'vscode';
import { SSLFoldingProvider } from '../src/sslFoldingProvider';
import { createDocument } from './helpers/mockVSCode';

describe('SSL Folding Provider', () => {
    let provider: SSLFoldingProvider;

    beforeEach(() => {
        provider = new SSLFoldingProvider();
    });

    it('should fold standard blocks (IF/ENDIF)', () => {
        const content = [
            ':PROCEDURE Test;',       // 0
            '    :IF cond;',          // 1
            '        lines;',         // 2
            '    :ENDIF;',            // 3
            ':ENDPROC;'               // 4
        ].join('\n');

        const document = createDocument(content);
        const ranges = provider.provideFoldingRanges(document as any, {} as any, {} as any);

        assert.strictEqual(ranges.length, 2);

        // IF block
        const ifRange = ranges.find(r => r.start === 1);
        assert.ok(ifRange, 'IF block should be folded');
        assert.strictEqual(ifRange.end, 3);
    });

    it('should fold CLASS to end of file', () => {
        const content = [
            ':CLASS MyClass;',        // 0
            '    :PROCEDURE Method;', // 1
            '    :ENDPROC;',          // 2
            '',                       // 3
            '    :PROCEDURE Other;',  // 4
            '    :ENDPROC;',          // 5
            '// End of file'          // 6
        ].join('\n');

        const document = createDocument(content);
        const ranges = provider.provideFoldingRanges(document as any, {} as any, {} as any);

        // Should have CLASS block and 2 PROCEDURE blocks
        const classRange = ranges.find(r => r.start === 0);
        assert.ok(classRange, 'CLASS block should be folded');
        assert.strictEqual(classRange.end, 6, 'CLASS should fold to last line');

        const procRanges = ranges.filter(r => r.kind !== vscode.FoldingRangeKind.Region && r.start !== 0);
        assert.strictEqual(procRanges.length, 2, 'Should fold internal procedures');
    });

    it('should handle nested blocks inside CLASS', () => {
        const content = [
            ':CLASS Container;',      // 0
            '    :PROCEDURE Test;',   // 1
            '        :IF x;',         // 2
            '        :ENDIF;',        // 3
            '    :ENDPROC;',          // 4
            '// EOF'                  // 5
        ].join('\n');

        const document = createDocument(content);
        const ranges = provider.provideFoldingRanges(document as any, {} as any, {} as any);

        const classRange = ranges.find(r => r.start === 0);
        assert.ok(classRange, 'CLASS should fold');
        assert.strictEqual(classRange.end, 5);

        const ifRange = ranges.find(r => r.start === 2);
        assert.ok(ifRange, 'IF should fold');
        assert.strictEqual(ifRange.end, 3);
    });
});
