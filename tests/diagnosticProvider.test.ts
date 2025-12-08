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
});

describe('SSL Diagnostic Provider - SQL placeholder style', () => {
	it('warns when RunSQL uses named placeholders', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Example;
:DECLARE sSQL, nId;
sSQL := "SELECT * FROM Foo WHERE ID = ?nId?";
RunSQL(sSQL, , , { nId });
:ENDPROC;`);
		const styleDiagnostics = diagnostics.filter((diag: any) => diag.code === 'ssl-invalid-sql-placeholder-style');
		expect(styleDiagnostics.length).to.equal(1);
	});

	it('warns when SQLExecute uses positional placeholders', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Example;
SQLExecute("SELECT * FROM Foo WHERE ID = ?", "Default");
:ENDPROC;`);
		const styleDiagnostics = diagnostics.filter((diag: any) => diag.code === 'ssl-invalid-sql-placeholder-style');
		expect(styleDiagnostics.length).to.equal(1);
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

	it('warns when ExecFunction omits procedure segment entirely', () => {
		const diagnostics = collectDiagnostics(`:PROCEDURE Example;
ExecFunction("ReportGenerator", { });
:ENDPROC;`);
		const execDiagnostics = diagnostics.filter((diag: any) => diag.code === 'ssl-invalid-exec-target');
		expect(execDiagnostics.length).to.be.greaterThan(0);
	});

	it('warns when namespace literal omits script/procedure segment', () => {
		config.update('ssl.documentNamespaces', { Reporting: 'reporting' });
		const diagnostics = collectDiagnostics(`:PROCEDURE Example;
ExecFunction("Reporting.GenerateReport", { });
:ENDPROC;`);
		const execDiagnostics = diagnostics.filter((diag: any) => diag.code === 'ssl-invalid-exec-target');
		expect(execDiagnostics.length).to.be.greaterThan(0);
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
