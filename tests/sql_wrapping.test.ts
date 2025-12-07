
import { describe, it, before, after } from 'mocha';
import { expect } from 'chai';
import * as vscode from 'vscode';
import { SSLFormattingProvider } from '../src/sslFormattingProvider';
import {
    createDocument,
    applyEdits,
    createFormattingOptions
} from './helpers/mockVSCode';

describe('SSL Formatter - SQL Line Wrapping Refinements', () => {
    const formatter = new SSLFormattingProvider();
    const options = createFormattingOptions();
    const config = vscode.workspace.getConfiguration('ssl');

    before(() => {
        config.update('ssl.format.sql.enabled', true);
        config.update('ssl.format.sql.keywordCase', 'upper');
        config.update('ssl.format.sql.indentSpaces', 4);
    });

    after(() => {
        config.update('ssl.format.sql.enabled', false);
    });

    it('should keep Table.Column identifiers together when wrapping', () => {
        // Construct a long line where the break might naturally fall at "Table.Column"
        // "SELECT " (7) + "t1.long_column_name_one, " (25) + "t1.long_column_name_two, " (25) ... use roughly 120 chars
        const input = 'SQLExecute("SELECT t1.col1, t1.col2, t1.col3, t1.col4, t1.col5, t1.col6, table_name.very_long_column_name_that_might_break FROM table_name");';

        const doc = createDocument(input);
        const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
        const formatted = applyEdits(input, edits as any[]);

        // Check that we don't have a line ending in "table_name." or starting with ".very_long..."
        // The split should happen at the comma before the long item if needed
        expect(formatted).to.not.match(/table_name\.\s*\n/);
        expect(formatted).to.not.match(/\n\s*\.very_long_column_name/);

        // Should contain the full identifier on one line (or at least connected)
        expect(formatted).to.include('table_name.very_long_column_name_that_might_break');
    });

    it('should wrap long column lists nicely (balanced lines)', () => {
        const input = 'SQLExecute("SELECT ordtask.ordno, ordtask.testcode, ordtask.sp_code, ordtask.cupno, runs.runno, runs.eqtype, ordtask.servgrp, ordtask.dept, ordtask.method, ordtask.qctype, ordtask.sptestsorter FROM ordtask, preptasks, runs");';
        const doc = createDocument(input);
        const edits = formatter.provideDocumentFormattingEdits(doc as any, options, null as any);
        const formatted = applyEdits(input, edits as any[]);

        const lines = formatted.split('\n');
        // Ensure that we don't just have one huge line
        expect(lines.length).to.be.greaterThan(2);

        // Ensure reasonable indentation for wrapped lines
        const selectLineIndex = lines.findIndex(l => l.includes('SELECT'));
        const nextLine = lines[selectLineIndex + 1];

        // This expects the continuation indent to be working
        expect(nextLine.length).to.be.greaterThan(10);
    });
});
