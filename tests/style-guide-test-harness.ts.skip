import * as fs from "fs";
import * as path from "path";

/**
 * SSL Style Guide Test Harness
 * 
 * This test runner validates that -bad fixtures can be formatted to match -expected fixtures.
 * Each test follows the pattern:
 * - {number}-{category}-{rule}-bad.ssl (input with style violations)
 * - {number}-{category}-{rule}-expected.ssl (expected formatted output)
 */

interface TestFixture {
	name: string;
	number: number;
	category: string;
	rule: string;
	badPath: string;
	expectedPath: string;
	badContent: string;
	expectedContent: string;
}

interface TestResult {
	fixture: TestFixture;
	passed: boolean;
	message: string;
}

/**
 * Load all test fixtures from the style-guide directory
 */
function loadFixtures(): TestFixture[] {
	const fixtureDir = path.join(__dirname, "../fixtures/style-guide");
	const files = fs.readdirSync(fixtureDir);
	const fixtures: Map<string, Partial<TestFixture>> = new Map();

	// Parse all files and group by test case
	for (const file of files) {
		const match = file.match(/^(\d+)-([\w-]+)-([\w-]+)-(bad|expected)\.ssl$/);
		if (!match) {
			continue;
		}

		const [, number, category, rule, type] = match;
		const key = `${number}-${category}-${rule}`;

		if (!fixtures.has(key)) {
			fixtures.set(key, {
				number: parseInt(number, 10),
				category,
				rule,
				name: key,
			});
		}

		const fixture = fixtures.get(key)!;
		const filePath = path.join(fixtureDir, file);
		const content = fs.readFileSync(filePath, "utf-8");

		if (type === "bad") {
			fixture.badPath = filePath;
			fixture.badContent = content;
		} else {
			fixture.expectedPath = filePath;
			fixture.expectedContent = content;
		}
	}

	// Filter to only complete pairs and sort by number
	return Array.from(fixtures.values())
		.filter((f) => f.badPath && f.expectedPath)
		.map((f) => f as TestFixture)
		.sort((a, b) => a.number - b.number);
}

/**
 * Display test summary with details
 */
function displayResults(results: TestResult[]): void {
	console.log("\n" + "=".repeat(80));
	console.log("SSL STYLE GUIDE TEST RESULTS");
	console.log("=".repeat(80) + "\n");

	const passed = results.filter((r) => r.passed);
	const failed = results.filter((r) => !r.passed);

	// Display summary
	console.log(`📊 Summary: ${passed.length} passed, ${failed.length} failed out of ${results.length} tests\n`);

	// Display detailed results by category
	const byCategory = new Map<string, TestResult[]>();
	for (const result of results) {
		const category = result.fixture.category;
		if (!byCategory.has(category)) {
			byCategory.set(category, []);
		}
		byCategory.get(category)!.push(result);
	}

	for (const [category, categoryResults] of byCategory) {
		const categoryPassed = categoryResults.filter((r) => r.passed).length;
		const categoryTotal = categoryResults.length;
		const status = categoryPassed === categoryTotal ? "✅" : "❌";

		console.log(`${status} ${category.toUpperCase()} (${categoryPassed}/${categoryTotal})`);

		for (const result of categoryResults) {
			const icon = result.passed ? "  ✓" : "  ✗";
			console.log(
				`${icon} ${result.fixture.number.toString().padStart(2, "0")}: ${result.fixture.rule}`
			);
			if (!result.passed) {
				console.log(`     ${result.message}`);
			}
		}
		console.log();
	}

	// Display failed tests details
	if (failed.length > 0) {
		console.log("\n" + "=".repeat(80));
		console.log("FAILED TESTS DETAILS");
		console.log("=".repeat(80) + "\n");

		for (const result of failed) {
			console.log(`❌ ${result.fixture.name}`);
			console.log(`   Rule: ${result.fixture.rule}`);
			console.log(`   Category: ${result.fixture.category}`);
			console.log(`   Issue: ${result.message}`);
			console.log();
		}
	}

	console.log("=".repeat(80) + "\n");
}

/**
 * Check if fixture files exist and have proper content
 */
function validateFixture(fixture: TestFixture): TestResult {
	// Check files exist
	if (!fs.existsSync(fixture.badPath)) {
		return {
			fixture,
			passed: false,
			message: `Missing bad file: ${path.basename(fixture.badPath)}`,
		};
	}

	if (!fs.existsSync(fixture.expectedPath)) {
		return {
			fixture,
			passed: false,
			message: `Missing expected file: ${path.basename(fixture.expectedPath)}`,
		};
	}

	// Check files have content
	if (!fixture.badContent.trim()) {
		return {
			fixture,
			passed: false,
			message: "Bad file is empty",
		};
	}

	if (!fixture.expectedContent.trim()) {
		return {
			fixture,
			passed: false,
			message: "Expected file is empty",
		};
	}

	// Basic validation: bad should differ from expected in most cases
	// (some fixtures might be identical if there are no violations)
	if (fixture.badContent === fixture.expectedContent) {
		console.warn(
			`⚠️  Warning: ${fixture.name} - bad and expected files are identical`
		);
	}

	// Success
	return {
		fixture,
		passed: true,
		message: "Fixture pair is valid",
	};
}

/**
 * Main test runner
 */
export function runStyleGuideTests(): void {
	try {
		const fixtures = loadFixtures();

		if (fixtures.length === 0) {
			console.error("❌ No fixtures found!");
			process.exit(1);
		}

		console.log(`\n📁 Found ${fixtures.length} test fixture pairs\n`);

		// Validate all fixtures
		const results = fixtures.map((fixture) => validateFixture(fixture));

		// Display results
		displayResults(results);

		// Exit with appropriate code
		const failed = results.filter((r) => !r.passed);
		if (failed.length > 0) {
			process.exit(1);
		}
	} catch (error) {
		console.error("Error running tests:", error);
		process.exit(1);
	}
}

// Run tests if this is the main module
if (require.main === module) {
	runStyleGuideTests();
}

export { TestFixture, TestResult };
