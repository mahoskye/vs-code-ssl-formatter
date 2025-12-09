
import { describe, it } from 'mocha';
import { expect } from 'chai';
import { SSLDiagnosticProvider } from '../src/sslDiagnosticProvider';
import { createDocument } from './helpers/mockVSCode';
import * as vscode from 'vscode';

const collectDiagnostics = (source: string) => {
    const provider = new SSLDiagnosticProvider();
    const document = createDocument(source);
    provider.updateDiagnostics(document as any);
    return (provider as any).diagnosticCollection.get(document.uri);
};

describe('SSL Diagnostic Provider - Legacy Comments', () => {
    it('flags * at start of line as Error', () => {
        const diagnostics = collectDiagnostics(`:PROCEDURE Test;
* This is not a comment anymore;
:ENDPROC;`);
        const invalidCommentDiagnostics = diagnostics.filter((diag: any) => diag.message.includes("not a valid comment"));
        expect(invalidCommentDiagnostics).to.have.length(1);
        expect(invalidCommentDiagnostics[0].severity).to.equal(vscode.DiagnosticSeverity.Error);
    });

    it('flags * sequences as Error', () => {
        const diagnostics = collectDiagnostics(`:PROCEDURE Test;
*******************************;
:ENDPROC;`);
        const invalidCommentDiagnostics = diagnostics.filter((diag: any) => diag.message.includes("not a valid comment"));
        expect(invalidCommentDiagnostics).to.have.length(1);
    });

    it('flags // comments as Error', () => {
        const diagnostics = collectDiagnostics(`:PROCEDURE Test;
// This is not a valid comment
:ENDPROC;`);
        const invalidCommentDiagnostics = diagnostics.filter((diag: any) => diag.message.includes("not a valid comment"));
        expect(invalidCommentDiagnostics).to.have.length(1);
        expect(invalidCommentDiagnostics[0].severity).to.equal(vscode.DiagnosticSeverity.Error);
    });
});
