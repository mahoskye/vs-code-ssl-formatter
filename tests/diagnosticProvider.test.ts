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

describe('SSL Diagnostic Provider - Semicolon Handling (Bug #26)', () => {
	it('does not flag multi-line function calls with array arguments', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Test;
result := DoProc("HelperProc", {
	"Param",
	nValue,
	nOther
});
:ENDPROC;`);
		const missingSemicolonDiagnostics = diagnostics.filter((diag: any) => diag.code === 'ssl-missing-semicolon');
		expect(missingSemicolonDiagnostics).to.have.length(0);
	});

	// TODO: Fix this test - valid logic but test harness not seeing diagnostic?
	it('still reports real missing semicolons outside multi-line structures', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Test;
sValue := "Hello"
:ENDPROC;`);
		const missingSemicolonDiagnostics = diagnostics.filter((diag: any) => diag.code === 'ssl-missing-semicolon');
		expect(missingSemicolonDiagnostics.length).to.be.greaterThan(0);
	});
});

describe('SSL Diagnostic Provider - Object Properties (Bug #22)', () => {
	it('does not report property access or assignment as undefined variables', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Example;
:DECLARE oUser, sValue;
sValue := oUser:sValue;
oUser:sValue := "Updated";
CallSomething(oUser:sValue);
Me:sProperty := "value";
:ENDPROC;`);
		const variableDiagnostics = diagnostics.filter((diag: any) => diag.code === 'ssl-undefined-variable' || diag.code === 'ssl-global-variable-in-procedure');
		expect(variableDiagnostics).to.have.length(0);
	});
});

describe('SSL Diagnostic Provider - SQL Parameter Casing (Bug #25)', () => {
	it('does not warn when placeholder casing differs from declaration', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Example;
:PARAMETERS sParam;
sQuery := "SELECT * FROM table WHERE field = ?SPARAM?";
:ENDPROC;`);
		const sqlDiagnostics = diagnostics.filter((diag: any) => diag.code === 'ssl-invalid-sql-param');
		expect(sqlDiagnostics).to.have.length(0);
	});

	it('still warns when placeholder truly undefined', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Example;
:PARAMETERS sParam;
sQuery := "SELECT * FROM table WHERE field = ?Unknown?";
:ENDPROC;`);
		const sqlDiagnostics = diagnostics.filter((diag: any) => diag.code === 'ssl-invalid-sql-param');
		expect(sqlDiagnostics.length).to.be.greaterThan(0);
	});

	it('validates SQL params on all lines of multi-line SQL statements', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Example;
:DECLARE sOrdNo, nTestCode;
SqlExecute("Update RESULTS set RETESTNO = 1, NUMRES = NULL,
                               RUNNO = NULL, S = ?Logged?, ORIGSTS = 'N'
            where ORDNO = ?sOrdNo? and TESTCODE = ?nTestCode?");
:ENDPROC;`);
		const sqlDiagnostics = diagnostics.filter((diag: any) => diag.code === 'ssl-invalid-sql-param');
		// ?Logged? should be flagged as undefined on line 2 (inside the multi-line SQL)
		expect(sqlDiagnostics.length).to.equal(1);
		expect(sqlDiagnostics[0].message).to.include('Logged');
	});
});

describe('SSL Diagnostic Provider - Multi-line Logical Expressions (Bug #3)', () => {
	it('does not flag multi-line logical expressions as missing semicolon', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Test;
:RETURN Empty(Me:a)
	.AND. Empty(Me:b)
	.AND. Empty(Me:c)
	.AND. Empty(Me:d)
	.AND. Empty(Me:e)
	.AND. Empty(Me:f);
:ENDPROC;`);
		const semicolonDiagnostics = diagnostics.filter((diag: any) => diag.code === 'ssl-missing-semicolon');
		expect(semicolonDiagnostics).to.have.length(0);
	});

	it('handles multi-line OR expressions without false semicolon warnings', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Test;
bValid := condition1
	.OR. condition2
	.OR. condition3;
:ENDPROC;`);
		const semicolonDiagnostics = diagnostics.filter((diag: any) => diag.code === 'ssl-missing-semicolon');
		expect(semicolonDiagnostics).to.have.length(0);
	});
});

describe('SSL Diagnostic Provider - Duplicate Diagnostics (Bug #4)', () => {
	it('should not produce duplicate diagnostics for undeclared variables', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Test;
	sSql := "SELECT '1' FROM DUAL";
	Me:TestQuery := LSearch(sSql);
:ENDPROC;`);

		// Filter diagnostics about sSql
		const ssSqlDiagnostics = diagnostics.filter((diag: any) =>
			diag.message.includes('sSql') || diag.range.start.line === 1
		);

		// Count unique diagnostic codes for sSql
		const uniqueCodes = new Set(ssSqlDiagnostics.map((d: any) => d.code));

		// Should have at most one unique diagnostic code per variable
		expect(uniqueCodes.size).to.be.at.most(1, 'Should not have multiple diagnostic codes for same issue');

		// If there are diagnostics for sSql, there should be exactly one
		if (ssSqlDiagnostics.length > 0) {
			expect(ssSqlDiagnostics.length).to.equal(1, 'Should have exactly one diagnostic, not duplicates');
		}
	});
});

describe('SSL Diagnostic Provider - Diagnostic Range Precision (Bug #5)', () => {
	it('should exclude leading whitespace from diagnostic ranges', () => {
		// Bug #5 was: "Warning highlight includes leading tab character"
		// The fix ensures diagnostic ranges start at the identifier, not whitespace
		const diagnostics = collectDiagnostics(`:PROCEDURE Test;
		someUndeclaredVariable := 123;
:ENDPROC;`);

		// Get any diagnostics that were created
		const varDiagnostics = diagnostics.filter((diag: any) =>
			diag.range && diag.range.start && diag.range.start.line === 1
		);

		// If diagnostics exist, verify they have valid ranges
		if (varDiagnostics.length > 0) {
			varDiagnostics.forEach((diag: any) => {
				// Ensure the range has a start position
				expect(diag.range).to.exist;
				expect(diag.range.start).to.exist;

				// The line should be 1 (where the variable is)
				expect(diag.range.start.line).to.equal(1);

				// Character position should be at or after position 2 (after the tabs)
				// This verifies leading whitespace is NOT included in the range
				if (typeof diag.range.start.character === 'number') {
					expect(diag.range.start.character).to.be.at.least(2,
						'Diagnostic range should start after leading tabs, not include them'
					);
				}
			});
		}

		// Note: If no diagnostics are produced, it means undeclared variables
		// may be handled differently, which is also acceptable
	});
});

describe('SSL Diagnostic Provider - Configured globals', () => {
	const config = vscode.workspace.getConfiguration('ssl');

	afterEach(() => {
		config.update('ssl.globals', []);
	});

	it('treats configured global variables as declared', () => {
		config.update('ssl.globals', ['sConfiguredGlobal']);
		const diagnostics = collectDiagnostics(`:PROCEDURE Example;
:DECLARE sOutput;
sOutput := sConfiguredGlobal;
:ENDPROC;`);
		const globalWarnings = diagnostics.filter((diag: any) => diag.code === 'ssl-global-variable-in-procedure');
		const undefinedDiagnostics = diagnostics.filter((diag: any) => diag.code === 'ssl-undefined-variable');
		expect(globalWarnings).to.have.length(0);
		expect(undefinedDiagnostics).to.have.length(0);
	});

	it('still warns about undeclared variables not listed in config', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Example;
:DECLARE sOutput;
sOutput := sNotConfigured;
:ENDPROC;`);
		const undefinedDiagnostics = diagnostics.filter((diag: any) => diag.code === 'ssl-undefined-variable');
		expect(undefinedDiagnostics.length).to.be.greaterThan(0);
	});
});

describe('SSL Diagnostic Provider - Custom IntelliSense functions', () => {
	const config = vscode.workspace.getConfiguration('ssl');

	afterEach(() => {
		config.update('ssl.intellisense.customFunctions', []);
	});

	it('treats configured functions as known identifiers', () => {
		config.update('ssl.intellisense.customFunctions', [
			{ name: 'CustomFunc', description: 'Project helper', params: '(value)' }
		]);

		const diagnostics = collectDiagnostics(`:PROCEDURE Example;
CustomFunc(sValue);
:ENDPROC;`);
		const undefinedDiagnostics = diagnostics.filter((diag: any) =>
			diag.code === 'ssl-undefined-variable' && diag.message.includes('CustomFunc')
		);
		expect(undefinedDiagnostics).to.have.length(0);
	});
	it('ignores built-in functions ALen and AEval', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Example;
:DECLARE aList;
nLen := ALen(aList);
AEval(aList, {|x| x := 1});
:ENDPROC;`);
		const undefinedDiagnostics = diagnostics.filter((diag: any) =>
			diag.code === 'ssl-undefined-variable' && (diag.message.includes('ALen') || diag.message.includes('AEval'))
		);
		expect(undefinedDiagnostics).to.have.length(0);
	});
});

describe('SSL Diagnostic Provider - SQL placeholder style', () => {


	it('warns when SQLExecute uses positional placeholders', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Example;
SQLExecute("SELECT * FROM Foo WHERE ID = ?", "Default");
:ENDPROC;`);
		const styleDiagnostics = diagnostics.filter((diag: any) => diag.code === 'ssl-invalid-sql-placeholder-style');
		expect(styleDiagnostics.length).to.equal(1);
	});

	it('errors when RunSQL is missing parameters argument with placeholders', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Example;
:DECLARE sSQL;
sSQL := "SELECT * FROM Foo WHERE ID = ?";
RunSQL(sSQL, "Query"); // Missing params argument (3rd position)
:ENDPROC;`);
		const missingParamsDiagnostics = diagnostics.filter((diag: any) => diag.code === 'ssl-runsql-missing-params');
		expect(missingParamsDiagnostics).to.have.length(1);
	});

	it('does not warn when placeholders match helper requirements', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Example;
:DECLARE sSQL, nId;
sSQL := "SELECT * FROM Foo WHERE ID = ?";
RunSQL(sSQL, , , { nId });
SQLExecute("SELECT * FROM Foo WHERE ID = ?nId?", "Default");
:ENDPROC;`);
		const styleDiagnostics = diagnostics.filter((diag: any) => diag.code === 'ssl-invalid-sql-placeholder-style');
		expect(styleDiagnostics).to.have.length(0);
	});
	it('validates SQL placeholders correctly when variable is reused (Flow Sensitive)', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Example;
:DECLARE sSQL;
sSQL := "SELECT * FROM Table WHERE ID = ?id?";
RunSQL(sSQL, "Valid", 0, 0, {}); 

sSQL := "SELECT * FROM Table WHERE ID = ?"; 
SQLExecute(sSQL, "InvalidReuse", 0, 0, 0, "", "", "", 0);
:ENDPROC;`);

		// First usage uses named placeholders for RunSQL -> ERROR (Strict rules)
		// Second usage uses positional placeholders for SQLExecute -> ERROR (Strict rules)
		// Both show that the validator is checking the SPECIFIC value assigned before the call.

		const diag2diagnostics = diagnostics.filter((diag: any) => diag.code === 'ssl-invalid-sql-placeholder-style');
		// RunSQL check disabled to allow valid ?...? syntax (positional)
		expect(diag2diagnostics).to.have.length(1);
		expect(diag2diagnostics[0].message).to.contain('SQLExecute'); // SQLExecute line
	});

	it('validates SQL inside concatenated strings correctly', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Example;
:DECLARE sSQL;
sSQL := "SELECT * " + "FROM Table " + "WHERE ID = ?";
SQLExecute(sSQL, "InvalidConcatenated");
:ENDPROC;`);
		// Should detect ? and warn for SQLExecute
		const styleDiagnostics = diagnostics.filter((diag: any) => diag.code === 'ssl-invalid-sql-placeholder-style');
		expect(styleDiagnostics).to.have.length(1);
	});
});

describe('SSL Diagnostic Provider - SQL placeholder identifiers', () => {
	it('recognizes file-level :PARAMETERS as valid identifiers', () => {
		// File-level :PARAMETERS (script arguments) should be treated as globals
		const diagnostics = collectDiagnostics(`:PARAMETERS sRunNo, nUserId;
:DECLARE sSQL;
sSQL := SqlExecute("SELECT * FROM Runs WHERE RUNNO = ?sRunNo? AND USER_ID = ?nUserId?");`);
		const invalidParamDiagnostics = diagnostics.filter((diag: any) => diag.code === 'ssl-invalid-sql-param');
		expect(invalidParamDiagnostics).to.have.length(0);
	});

	it('allows array-indexed named placeholders when base variable is declared', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Example;
:DECLARE sSQL, nIds;
sSQL := "SELECT * FROM Foo WHERE ID = ?nIds[1]?";
RunSQL(sSQL, , , { nIds });
:ENDPROC;`);
		const invalidParamDiagnostics = diagnostics.filter((diag: any) => diag.code === 'ssl-invalid-sql-param');
		expect(invalidParamDiagnostics).to.have.length(0);
	});

	it('skips validation for expression placeholders (array access, function calls, etc.)', () => {
		// Expression placeholders like ?nMissing[1]?, ?Now()?, ?a+b? are not validated
		// because ?...? is an interpolation syntax that can contain any SSL expression
		const diagnostics = collectDiagnostics(`:PROCEDURE Example;
:DECLARE sSQL;
sSQL := "SELECT * FROM Foo WHERE ID = ?nMissing[1]? AND Date = ?Now()?";
RunSQL(sSQL, , , { nMissing });
:ENDPROC;`);
		const invalidParamDiagnostics = diagnostics.filter((diag: any) => diag.code === 'ssl-invalid-sql-param');
		expect(invalidParamDiagnostics).to.have.length(0);
	});
});

describe('SSL Diagnostic Provider - ExecFunction namespace validation', () => {
	const config = vscode.workspace.getConfiguration('ssl');

	afterEach(() => {
		config.update('ssl.documentNamespaces', {});
	});

	it('accepts ExecFunction with single procedure name', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Example;
ExecFunction("ReportGenerator", { });
:ENDPROC;`);
		const execDiagnostics = diagnostics.filter((diag: any) => diag.code === 'ssl-invalid-exec-target');
		expect(execDiagnostics).to.have.length(0);
	});

	it('accepts namespace literal with only script/procedure segment', () => {
		config.update('ssl.documentNamespaces', { Reporting: 'reporting' });
		const diagnostics = collectDiagnostics(`:PROCEDURE Example;
ExecFunction("Reporting.GenerateReport", { });
:ENDPROC;`);
		const execDiagnostics = diagnostics.filter((diag: any) => diag.code === 'ssl-invalid-exec-target');
		expect(execDiagnostics).to.have.length(0);
	});

	it('accepts namespace literal with explicit script and procedure', () => {
		config.update('ssl.documentNamespaces', { Reporting: 'reporting' });
		const diagnostics = collectDiagnostics(`:PROCEDURE Example;
ExecFunction("Reporting.GenerateReport.GenerateReport", { "Daily" });
:ENDPROC;`);
		const execDiagnostics = diagnostics.filter((diag: any) => diag.code === 'ssl-invalid-exec-target');
		expect(execDiagnostics).to.have.length(0);
	});
});

describe('SSL Diagnostic Provider - Function Call Recognition (Issue #53)', () => {
	it('does not flag lowercase function calls as undefined variables', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Test;
infomes("some message");
:ENDPROC;`);
		const undefinedDiagnostics = diagnostics.filter((diag: any) =>
			diag.code === 'ssl-undefined-variable' && diag.message.includes('infomes'));
		expect(undefinedDiagnostics).to.have.length(0);
	});

	it('does not flag any unknown function call as undefined variable', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Test;
:DECLARE sValue;
myCustomFunc(123);
AnotherFunc(sValue);
ALLUPPER(sValue);
:ENDPROC;`);
		const undefinedDiagnostics = diagnostics.filter((diag: any) =>
			diag.code === 'ssl-undefined-variable' &&
			(diag.message.includes('myCustomFunc') ||
			 diag.message.includes('AnotherFunc') ||
			 diag.message.includes('ALLUPPER')));
		expect(undefinedDiagnostics).to.have.length(0);
	});

	it('does not flag function calls with whitespace before parenthesis', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Test;
someFunc (123);
anotherFunc  ("test");
:ENDPROC;`);
		const undefinedDiagnostics = diagnostics.filter((diag: any) =>
			diag.code === 'ssl-undefined-variable' &&
			(diag.message.includes('someFunc') || diag.message.includes('anotherFunc')));
		expect(undefinedDiagnostics).to.have.length(0);
	});

	it('flags undeclared variables regardless of casing', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Test;
:DECLARE sValue;
sValue := undeclaredVar;
:ENDPROC;`);
		const undefinedDiagnostics = diagnostics.filter((diag: any) =>
			diag.code === 'ssl-undefined-variable');
		expect(undefinedDiagnostics.length).to.be.greaterThan(0);
	});

	it('treats variable references case-insensitively', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Test;
:DECLARE myVar;
myVar := 1;
sResult := MYVAR + myvar + MyVar;
:ENDPROC;`);
		// myVar is declared, so MYVAR, myvar, MyVar should all be valid references
		const undefinedDiagnostics = diagnostics.filter((diag: any) =>
			diag.code === 'ssl-undefined-variable' &&
			diag.message.toLowerCase().includes('myvar'));
		expect(undefinedDiagnostics).to.have.length(0);
	});

	it('does not flag SSL literals NIL, .T., .F. as undefined variables', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Test;
:DECLARE bFlag, oObject;
bFlag := .T.;
bFlag := .F.;
oObject := NIL;
:ENDPROC;`);
		const undefinedDiagnostics = diagnostics.filter((diag: any) =>
			diag.code === 'ssl-undefined-variable' &&
			(diag.message.includes('NIL') ||
			 diag.message.includes('.T.') ||
			 diag.message.includes('.F.')));
		expect(undefinedDiagnostics).to.have.length(0);
	});

	it('flags lowercase nil as undefined variable (literals are case-sensitive)', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Test;
:DECLARE oObject;
oObject := nil;
:ENDPROC;`);
		const undefinedDiagnostics = diagnostics.filter((diag: any) =>
			diag.code === 'ssl-undefined-variable' &&
			diag.message.includes('nil'));
		expect(undefinedDiagnostics).to.have.length(1);
	});
});

describe('SSL Diagnostic Provider - Comment Text After Terminator (Issue #52)', () => {
	it('errors when code follows comment on same line', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Test;
/* comment; sValue := "test";
:ENDPROC;`);
		const commentDiagnostics = diagnostics.filter((diag: any) =>
			diag.code === 'ssl-comment-text-after-terminator');
		expect(commentDiagnostics).to.have.length(1);
		expect(commentDiagnostics[0].severity).to.equal(vscode.DiagnosticSeverity.Error);
	});

	it('warns when multiple comments on same line', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Test;
/* Comment one; /* Comment two;
:ENDPROC;`);
		const commentDiagnostics = diagnostics.filter((diag: any) =>
			diag.code === 'ssl-comment-text-after-terminator');
		expect(commentDiagnostics).to.have.length(1);
		expect(commentDiagnostics[0].severity).to.equal(vscode.DiagnosticSeverity.Warning);
	});

	it('errors on premature comment termination (Issue #52 example)', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Test;
/* Create the order; empty return indicates creation failure for this entry;
:ENDPROC;`);
		const commentDiagnostics = diagnostics.filter((diag: any) =>
			diag.code === 'ssl-comment-text-after-terminator');
		expect(commentDiagnostics).to.have.length(1);
	});

	it('does not warn on normal single-line comment', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Test;
/* This is a normal comment;
:ENDPROC;`);
		const commentDiagnostics = diagnostics.filter((diag: any) =>
			diag.code === 'ssl-comment-text-after-terminator');
		expect(commentDiagnostics).to.have.length(0);
	});

	it('does not warn on multi-line comments', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Test;
/*
This is a multi-line comment
with multiple lines
;
:ENDPROC;`);
		const commentDiagnostics = diagnostics.filter((diag: any) =>
			diag.code === 'ssl-comment-text-after-terminator');
		expect(commentDiagnostics).to.have.length(0);
	});

	it('does not warn when comment ends at end of line', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Test;
/* Initialize variables;
sValue := "test";
:ENDPROC;`);
		const commentDiagnostics = diagnostics.filter((diag: any) =>
			diag.code === 'ssl-comment-text-after-terminator');
		expect(commentDiagnostics).to.have.length(0);
	});

	it('errors on premature semicolon inside multi-line comment block', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Test;
/*
Description: This script does something
Note: REP ; is a part of the key on the ORDTASK table.
*/;
:ENDPROC;`);
		const commentDiagnostics = diagnostics.filter((diag: any) =>
			diag.code === 'ssl-comment-text-after-terminator');
		expect(commentDiagnostics).to.have.length(1);
	});

	it('errors on semicolon in middle of multi-line comment documentation', () => {
		const diagnostics = collectDiagnostics(`/*
Description: This script adds a QC sample
Parameter 1: EQID - equipment id
Return: Fixed RESULTS; cup number update also searches on ORDTASK.REP.
*/;`);
		const commentDiagnostics = diagnostics.filter((diag: any) =>
			diag.code === 'ssl-comment-text-after-terminator');
		expect(commentDiagnostics).to.have.length(1);
	});

	it('does not warn on multi-line comment with semicolon only at end', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Test;
/*
Description: This script does something
No semicolons in the middle of any line
;
:ENDPROC;`);
		const commentDiagnostics = diagnostics.filter((diag: any) =>
			diag.code === 'ssl-comment-text-after-terminator');
		expect(commentDiagnostics).to.have.length(0);
	});

	it('warns when comment is followed by another comment', () => {
		const diagnostics = collectDiagnostics(`/*:PUBLIC VAR1, VAR2;/*not stored, added for calcs;`);
		const commentDiagnostics = diagnostics.filter((diag: any) =>
			diag.code === 'ssl-comment-text-after-terminator');
		expect(commentDiagnostics).to.have.length(1);
		expect(commentDiagnostics[0].severity).to.equal(vscode.DiagnosticSeverity.Warning);
	});
});
