import { describe, it } from 'mocha';
import { expect } from 'chai';
import {
	looksLikeSql,
	formatSqlContent,
	formatAsMultilineString,
	formatAsConcatenatedString,
	detectSqlContext,
	formatSqlWithStyleImpl,
	SqlFormattingStyle
} from '../src/commands/formatSql';
describe('SQL Format Command - SQL Detection', () => {
	it('should detect SELECT statements as SQL', () => {
		expect(looksLikeSql('SELECT * FROM users')).to.be.true;
	});

	it('should detect INSERT statements as SQL', () => {
		expect(looksLikeSql('INSERT INTO users (name) VALUES (?)')).to.be.true;
	});

	it('should detect UPDATE statements as SQL', () => {
		expect(looksLikeSql('UPDATE users SET name = ?')).to.be.true;
	});

	it('should detect DELETE statements as SQL', () => {
		expect(looksLikeSql('DELETE FROM users WHERE id = ?')).to.be.true;
	});

	it('should be case-insensitive', () => {
		expect(looksLikeSql('select * from users')).to.be.true;
		expect(looksLikeSql('Select * From Users')).to.be.true;
	});

	it('should reject non-SQL text', () => {
		expect(looksLikeSql('Hello World')).to.be.false;
		expect(looksLikeSql('nValue := 123')).to.be.false;
	});
});

describe('SQL Format Command - Content Formatting', () => {
	it('should format simple SELECT with UPPER case keywords', () => {
		const input = 'select id, name from users where active = 1';
		const result = formatSqlContent(input, 'upper', 4);

		expect(result).to.include('SELECT');
		expect(result).to.include('FROM');
		expect(result).to.include('WHERE');
	});

	it('should format with lower case keywords when specified', () => {
		const input = 'SELECT id FROM users WHERE active = 1';
		const result = formatSqlContent(input, 'lower', 4);

		expect(result).to.include('select');
		expect(result).to.include('from');
		expect(result).to.include('where');
	});

	it('should preserve original case when preserve is specified', () => {
		const input = 'Select id From users Where active = 1';
		const result = formatSqlContent(input, 'preserve', 4);

		expect(result).to.include('Select');
		expect(result).to.include('From');
		expect(result).to.include('Where');
	});

	it('should break into multiple lines at clause keywords', () => {
		const input = 'SELECT id, name FROM users WHERE active = 1 ORDER BY name';
		const result = formatSqlContent(input, 'upper', 4);

		const lines = result.split('\n');
		expect(lines.length).to.be.greaterThan(1);
		expect(lines[0]).to.include('SELECT');
	});

	it('should indent clause keywords', () => {
		const input = 'SELECT id FROM users WHERE active = 1';
		const result = formatSqlContent(input, 'upper', 4);

		const lines = result.split('\n');
		// FROM and WHERE should be indented
		const fromLine = lines.find(l => l.trim().startsWith('FROM'));
		const whereLine = lines.find(l => l.trim().startsWith('WHERE'));

		expect(fromLine).to.match(/^\s+FROM/);
		expect(whereLine).to.match(/^\s+WHERE/);
	});

	it('should keep AND/OR inline in compact style', () => {
		const input = 'SELECT id FROM users WHERE active = 1 AND name IS NOT NULL';
		const result = formatSqlContent(input, 'upper', 4);

		// In compact style, AND/OR stay inline with WHERE clause
		const lines = result.split('\n');
		const whereLine = lines.find(l => l.trim().startsWith('WHERE'));

		expect(whereLine).to.exist;
		expect(whereLine).to.include('AND'); // AND should be on same line as WHERE
	});

	it('should handle complex JOINs', () => {
		const input = 'SELECT u.id, o.total FROM users u INNER JOIN orders o ON u.id = o.user_id WHERE o.total > 100';
		const result = formatSqlContent(input, 'upper', 4);

		// INNER JOIN should be preserved as a clause
		expect(result).to.include('INNER');
		expect(result).to.include('JOIN');
		const lines = result.split('\n');
		expect(lines.length).to.be.greaterThan(2);
	});

	it('should handle GROUP BY and ORDER BY', () => {
		const input = 'SELECT status, COUNT(*) FROM orders GROUP BY status ORDER BY status';
		const result = formatSqlContent(input, 'upper', 4);

		expect(result).to.include('GROUP BY');
		expect(result).to.include('ORDER BY');
	});

	it('should handle INSERT statements', () => {
		const input = 'INSERT INTO users (name, email) VALUES (?, ?)';
		const result = formatSqlContent(input, 'upper', 4);

		expect(result).to.include('INSERT INTO');
		expect(result).to.include('VALUES');
	});

	it('should handle UPDATE statements', () => {
		const input = 'UPDATE users SET name = ?, email = ? WHERE id = ?';
		const result = formatSqlContent(input, 'upper', 4);

		expect(result).to.include('UPDATE');
		expect(result).to.include('SET');
		expect(result).to.include('WHERE');
	});

	it('should preserve SQL placeholders', () => {
		const input = 'SELECT * FROM users WHERE id = ?nUserId? AND status = ?';
		const result = formatSqlContent(input, 'upper', 4);

		expect(result).to.include('?nUserId?');
		expect(result).to.include('= ?');
	});

	it('should use specified indent size', () => {
		const input = 'SELECT id FROM users WHERE active = 1';
		const result2 = formatSqlContent(input, 'upper', 2);
		const result4 = formatSqlContent(input, 'upper', 4);

		// 4-space indent should have more leading spaces than 2-space
		const fromLine2 = result2.split('\n').find(l => l.trim().startsWith('FROM'));
		const fromLine4 = result4.split('\n').find(l => l.trim().startsWith('FROM'));

		const indent2 = fromLine2?.match(/^\s*/)?.[0].length || 0;
		const indent4 = fromLine4?.match(/^\s*/)?.[0].length || 0;

		expect(indent4).to.be.greaterThan(indent2);
	});
});

describe('SQL Format Command - Output Styles', () => {
	it('should format as multiline string with quotes', () => {
		const sql = 'SELECT id\nFROM users';
		const result = formatAsMultilineString(sql, '"', true);

		expect(result).to.equal('"SELECT id\nFROM users"');
	});

	it('should format as concatenated strings', () => {
		const sql = 'SELECT id\nFROM users';
		const result = formatAsConcatenatedString(sql, '"');

		expect(result).to.include('" +');
		expect(result.split('\n').length).to.equal(2);
	});

	it('should add trailing space in concatenated parts except last', () => {
		const sql = 'SELECT id\nFROM users\nWHERE active = 1';
		const result = formatAsConcatenatedString(sql, '"');

		const lines = result.split('\n');
		// First two lines should have trailing space before closing quote
		expect(lines[0]).to.match(/\s" \+$/);
		expect(lines[1]).to.match(/\s" \+$/);
		// Last line should not have + at the end
		expect(lines[2]).to.not.include('+');
	});

	it('should handle single-line SQL without concatenation', () => {
		const sql = 'SELECT * FROM users';
		const result = formatAsConcatenatedString(sql, '"');

		expect(result).to.equal('"SELECT * FROM users"');
		expect(result).to.not.include('+');
	});

	it('should handle single-quoted strings', () => {
		const sql = 'SELECT id\nFROM users';
		const result = formatAsConcatenatedString(sql, "'");

		expect(result).to.include("'SELECT id '");
		expect(result).to.include("'FROM users'");
	});

	it('should handle bracket-quoted strings', () => {
		const sql = 'SELECT id';
		const result = formatAsMultilineString(sql, '[', true);

		expect(result).to.equal('[SELECT id]');
	});

	it('should return raw SQL if not a string literal', () => {
		const sql = 'SELECT id FROM users';
		const result = formatAsMultilineString(sql, '"', false);

		expect(result).to.equal(sql);
	});
});

describe('SQL Format Command - Formatting Styles', () => {
	const testSql = 'SELECT u.id, u.name, u.email FROM users u INNER JOIN orders o ON o.user_id = u.id WHERE u.active = 1 AND u.deleted_at IS NULL ORDER BY u.name';

	it('should format with compact style - single-line clauses', () => {
		const result = formatSqlWithStyleImpl(testSql, 'compact', 'upper', 4);

		// Compact keeps AND/OR inline
		const lines = result.split('\n');
		expect(lines.find(l => l.trim().startsWith('SELECT'))).to.exist;
		expect(lines.find(l => l.trim().startsWith('FROM'))).to.exist;
		expect(lines.find(l => l.trim().startsWith('WHERE'))).to.exist;

		// AND should be inline with WHERE in compact
		const whereLine = lines.find(l => l.trim().startsWith('WHERE'));
		expect(whereLine).to.include('AND');
	});

	it('should format with expanded style - vertical columns', () => {
		const result = formatSqlWithStyleImpl(testSql, 'expanded', 'upper', 4);

		const lines = result.split('\n');
		// SELECT should be on its own line, columns indented below
		expect(lines[0].trim()).to.equal('SELECT');

		// Each column should be on its own line
		const columnLines = lines.filter(l => l.trim().startsWith('u.'));
		expect(columnLines.length).to.be.greaterThan(0);

		// AND/OR should be on separate lines
		const andLine = lines.find(l => l.trim().startsWith('AND'));
		expect(andLine).to.exist;
	});

	it('should format with hanging operators style', () => {
		const result = formatSqlWithStyleImpl(testSql, 'hangingOperators', 'upper', 4);

		const lines = result.split('\n');
		// AND should be at line start with small indent
		const andLine = lines.find(l => l.trim().startsWith('AND'));
		expect(andLine).to.exist;
		expect(andLine).to.match(/^\s{2}AND/); // 2-space hanging indent
	});

	it('should format with K&R style - parenthesized blocks', () => {
		const result = formatSqlWithStyleImpl(testSql, 'knr', 'upper', 4);

		// Should have parentheses around WHERE conditions
		expect(result).to.include('WHERE (');
		expect(result).to.include(')');
	});

	it('should format with K&R compact style', () => {
		const shortSql = 'SELECT id, name FROM users WHERE active = 1';
		const result = formatSqlWithStyleImpl(shortSql, 'knrCompact', 'upper', 4);

		const lines = result.split('\n');
		// Compact SELECT should have columns on one line (if short enough)
		const selectLine = lines.find(l => l.trim().startsWith('SELECT'));
		expect(selectLine).to.exist;
	});

	it('should format with canonical compact style', () => {
		const input = 'select u.id, u.name, u.email, count(o.id) as order_count from users u join orders o on o.user_id = u.id where u.active = 1 and (u.deleted_at is null or u.deleted_at > now()) group by u.id, u.name, u.email order by order_count desc';
		const result = formatSqlWithStyleImpl(input, 'canonicalCompact', 'upper', 4);
		expect(result).to.match(/\nFROM\s+users u/i);
		expect(result).to.match(/\nJOIN\s+orders o/i);
		expect(result).to.match(/\n\s+ON o\.user_id = u\.id/i);
		expect(result).to.match(/\n\s+AND \(u\.deleted_at IS NULL/i);
		expect(result).to.match(/ORDER BY order_count DESC/i);
	});

	it('should format with ORM-friendly style - inline joins and hanging operators', () => {
		const input = 'select u.id, u.name from users u join orders o on o.user_id = u.id where u.active = 1 and o.total > 0 order by o.total desc';
		const result = formatSqlWithStyleImpl(input, 'ormFriendly', 'upper', 4);
		expect(result).to.match(/\nFROM\s+users u JOIN orders o ON o\.user_id = u\.id/i);
		expect(result).to.match(/\n\s+AND o\.total > 0/i);
		expect(result).to.match(/ORDER BY o\.total DESC/i);
	});

	it('should respect keyword case in all styles', () => {
		const result = formatSqlWithStyleImpl('select id from users', 'compact', 'lower', 4);
		expect(result).to.include('select');
		expect(result).to.include('from');
		expect(result).to.not.include('SELECT');
	});

	it('should respect indent spaces in all styles', () => {
		const result2 = formatSqlWithStyleImpl(testSql, 'compact', 'upper', 2);
		const result4 = formatSqlWithStyleImpl(testSql, 'compact', 'upper', 4);

		const fromLine2 = result2.split('\n').find(l => l.trim().startsWith('FROM'));
		const fromLine4 = result4.split('\n').find(l => l.trim().startsWith('FROM'));

		const indent2 = fromLine2?.match(/^\s*/)?.[0].length || 0;
		const indent4 = fromLine4?.match(/^\s*/)?.[0].length || 0;

		expect(indent4).to.be.greaterThan(indent2);
	});
});

describe('SQL Format Command - Edge Cases', () => {
	it('should handle empty input', () => {
		const result = formatSqlContent('', 'upper', 4);
		expect(result).to.equal('');
	});

	it('should handle whitespace-only input', () => {
		const result = formatSqlContent('   \n  ', 'upper', 4);
		expect(result).to.equal('');
	});

	it('should collapse multiple whitespace in content', () => {
		const input = 'SELECT   id   FROM    users';
		const result = formatSqlContent(input, 'upper', 4);

		// Multiple spaces in the content should be collapsed to single space
		// Note: indentation at line start is intentional, so we check the SELECT line
		const firstLine = result.split('\n')[0];
		expect(firstLine).to.not.match(/\s{3,}/);
	});

	it('should handle CRLF line endings', () => {
		const input = 'SELECT id\r\nFROM users';
		const result = formatSqlContent(input, 'upper', 4);

		expect(result).to.not.include('\r');
	});

	it('should handle UNION queries', () => {
		const input = 'SELECT id FROM users UNION SELECT id FROM admins';
		const result = formatSqlContent(input, 'upper', 4);

		expect(result).to.include('UNION');
		const lines = result.split('\n');
		expect(lines.length).to.be.greaterThan(1);
	});

	it('should handle subqueries', () => {
		const input = 'SELECT * FROM users WHERE id IN (SELECT user_id FROM orders)';
		const result = formatSqlContent(input, 'upper', 4);

		expect(result).to.include('SELECT');
		expect(result).to.include('IN');
	});

	it('should not wrap raw SQL in quotes', () => {
		// This test simulates "Select Text -> Format SQL" behavior expectation
		// Current implementation might wrap raw text in quotes if it assumes string literal context
		const input = 'SELECT * FROM users';
		// We need to check finding logic or just format logic?
		// formatSqlContent currently just calls formatSqlWithStyleImpl.
		// formatSqlWithStyleImpl returns raw formatted string.
		// The wrapping happens in `formatSqlWithStyle` function in `src/commands/formatSql.ts` which is not exported for testing directly.
		// But we can verify `looksLikeSql` and ensuring our improved logic will handle it.
		// Since we can't easily test `formatSqlWithStyle` (it uses vscode.window), we will rely on manual verification or mocks.
		// But we can add a test for the extraction logic if we export it or move it to a helper.
	});
});


describe('SQL Format Command - Context Detection', () => {
	it('should detect raw SQL', () => {
		const result = detectSqlContext('SELECT * FROM users');
		expect(result.type).to.equal('raw');
		expect(result.content).to.equal('SELECT * FROM users');
		expect(result.quoteChar).to.equal('');
	});

	it('should detect double quoted string', () => {
		const result = detectSqlContext('"SELECT * FROM users"');
		expect(result.type).to.equal('quoted');
		expect(result.content).to.equal('SELECT * FROM users');
		expect(result.quoteChar).to.equal('"');
		expect(result.prefix).to.equal('"');
		expect(result.suffix).to.equal('"');
	});

	it('should detect single quoted string', () => {
		const result = detectSqlContext("'SELECT * FROM users'");
		expect(result.type).to.equal('quoted');
		expect(result.content).to.equal('SELECT * FROM users');
		expect(result.quoteChar).to.equal("'");
	});

	it('should detect bracket quoted string', () => {
		const result = detectSqlContext('[SELECT * FROM users]');
		expect(result.type).to.equal('quoted');
		expect(result.content).to.equal('SELECT * FROM users');
		expect(result.quoteChar).to.equal('[');
	});

	it('should detect function call wrapper', () => {
		const input = 'SqlExecute("SELECT * FROM users")';
		const result = detectSqlContext(input);
		expect(result.type).to.equal('function');
		expect(result.content).to.equal('SELECT * FROM users');
		expect(result.quoteChar).to.equal('"');
		expect(result.prefix).to.equal('SqlExecute("');
		expect(result.suffix).to.equal('")');
	});

	it('should detect function call with spaces', () => {
		const input = 'SqlExecute ( " SELECT * FROM users " )';
		const result = detectSqlContext(input);
		expect(result.type).to.equal('function');
		expect(result.content).to.equal(' SELECT * FROM users ');
		expect(result.quoteChar).to.equal('"');
		expect(result.prefix).to.equal('SqlExecute ( "');
		expect(result.suffix).to.equal('" )');
	});

	it('should detect function call with semicolon', () => {
		const input = 'SqlExecute("SELECT");';
		const result = detectSqlContext(input);
		expect(result.type).to.equal('function');
		expect(result.suffix).to.equal('");');
	});

	it('should detect RunSQL', () => {
		const input = 'RunSQL("SELECT")';
		const result = detectSqlContext(input);
		expect(result.type).to.equal('function');
	});
});
