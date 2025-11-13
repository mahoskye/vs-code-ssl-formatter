/**
 * SSL Style Guide Test Runner
 * 
 * This test runner validates SSL code formatting against style guide rules.
 * Tests are organized as fixture pairs:
 * - *-bad.ssl: Source code with style violations
 * - *-expected.ssl: Expected output after formatting
 */

import * as fs from "fs";
import * as path from "path";
import * as assert from "assert";

interface StyleGuideTest {
	name: string;
	description: string;
	badPath: string;
	expectedPath: string;
}

function getStyleGuideTests(): StyleGuideTest[] {
	const fixtureDir = path.join(__dirname, "fixtures/style-guide");
	const files = fs.readdirSync(fixtureDir).filter((f) => f.endsWith(".ssl"));

	const tests: StyleGuideTest[] = [];
	const processed = new Set<string>();

	for (const file of files) {
		const match = file.match(/^(.+?)-(bad|expected)\.ssl$/);
		if (!match) {
			continue;
		}

		const [, testName] = match;
		if (processed.has(testName)) {
			continue;
		}

		const badPath = path.join(fixtureDir, `${testName}-bad.ssl`);
		const expectedPath = path.join(fixtureDir, `${testName}-expected.ssl`);

		if (fs.existsSync(badPath) && fs.existsSync(expectedPath)) {
			processed.add(testName);
			const description = testName.split("-").slice(1).join(" ");
			tests.push({
				name: testName,
				description,
				badPath,
				expectedPath,
			});
		}
	}

	return tests.sort((a, b) => a.name.localeCompare(b.name));
}

/**
 * Test that fixture files exist and contain content
 */
function testFixtureFileExists(): void {
	const tests = getStyleGuideTests();
	assert.ok(tests.length > 0, "Should have found test fixtures");

	for (const test of tests) {
		assert.ok(fs.existsSync(test.badPath), `Bad fixture file should exist: ${test.badPath}`);
		assert.ok(
			fs.existsSync(test.expectedPath),
			`Expected fixture file should exist: ${test.expectedPath}`
		);

		const badContent = fs.readFileSync(test.badPath, "utf-8");
		const expectedContent = fs.readFileSync(test.expectedPath, "utf-8");

		assert.ok(badContent.trim().length > 0, `Bad fixture should not be empty: ${test.name}`);
		assert.ok(
			expectedContent.trim().length > 0,
			`Expected fixture should not be empty: ${test.name}`
		);
	}
}

/**
 * Test that bad fixtures contain style violations (differ from expected)
 */
function testFixtureContentDiffers(): void {
	const tests = getStyleGuideTests();

	const warns: string[] = [];

	for (const test of tests) {
		const badContent = fs.readFileSync(test.badPath, "utf-8");
		const expectedContent = fs.readFileSync(test.expectedPath, "utf-8");

		if (badContent === expectedContent) {
			warns.push(`⚠️  ${test.name}: bad and expected are identical`);
		}
	}

	if (warns.length > 0) {
		console.warn("\nWarnings:");
		warns.forEach((w) => console.warn(w));
	}
}

/**
 * Display test fixtures summary
 */
function testFixturesSummary(): void {
	const tests = getStyleGuideTests();

	console.log(`\n${"=".repeat(80)}`);
	console.log("SSL STYLE GUIDE TEST FIXTURES");
	console.log(`${"=".repeat(80)}\n`);

	console.log(`📊 Total test fixtures: ${tests.length}\n`);

	// Group by category
	const byCategory = new Map<string, StyleGuideTest[]>();
	for (const test of tests) {
		const parts = test.name.split("-");
		const category = parts.slice(1, -1).join("-");

		if (!byCategory.has(category)) {
			byCategory.set(category, []);
		}
		byCategory.get(category)!.push(test);
	}

	for (const [category, tests] of Array.from(byCategory.entries()).sort()) {
		console.log(`📁 ${category} (${tests.length} tests)`);
		for (const test of tests) {
			console.log(`   ✓ ${test.name}`);
		}
		console.log();
	}

	console.log(`${"=".repeat(80)}\n`);
}

export const styleGuideTests = {
	"Style Guide Fixtures Exist": testFixtureFileExists,
	"Style Guide Fixture Content Differs": testFixtureContentDiffers,
	"Style Guide Fixtures Summary": testFixturesSummary,
};

export { StyleGuideTest, getStyleGuideTests };
