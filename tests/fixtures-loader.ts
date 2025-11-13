import * as fs from "fs";
import * as path from "path";

/**
 * Comprehensive fixture list and validation for SSL Style Guide
 *
 * Each fixture pair tests a specific style guide rule:
 * - {number}-{category}-{rule}-bad.ssl: Code with style violations
 * - {number}-{category}-{rule}-expected.ssl: Expected formatted output
 */

interface StyleGuideFixture {
	number: number;
	category: string;
	rule: string;
	badFile: string;
	expectedFile: string;
	badContent?: string;
	expectedContent?: string;
}

/**
 * Load all available fixtures
 */
function loadAllFixtures(): StyleGuideFixture[] {
	// Calculate fixture path relative to workspace root, not compiled output
	// When compiled: __dirname = /workspace/out/tests
	// We want: /workspace/tests/fixtures/style-guide
	const workspaceRoot = path.resolve(__dirname, "../../");
	const fixtureDir = path.resolve(workspaceRoot, "tests/fixtures/style-guide");

	if (!fs.existsSync(fixtureDir)) {
		// Fallback: try relative to current working directory
		const fallbackDir = path.resolve(process.cwd(), "tests/fixtures/style-guide");
		if (fs.existsSync(fallbackDir)) {
			return loadFixturesFrom(fallbackDir);
		}
		console.error(`Fixture directory not found: ${fixtureDir}`);
		console.error(`Fallback also not found: ${fallbackDir}`);
		return [];
	}

	return loadFixturesFrom(fixtureDir);
}

/**
 * Load fixtures from a specific directory
 */
function loadFixturesFrom(fixtureDir: string): StyleGuideFixture[] {
	const files = fs.readdirSync(fixtureDir).filter((f) => f.endsWith(".ssl"));
	const fixtures: Map<string, StyleGuideFixture> = new Map();

	for (const file of files) {
		const match = file.match(/^(\d+)-([\w-]+)-([\w-]+)-(bad|expected)\.ssl$/);
		if (!match) {
			continue;
		}

		const [, num, category, rule, type] = match;
		const key = `${num}-${category}-${rule}`;

		if (!fixtures.has(key)) {
			fixtures.set(key, {
				number: parseInt(num, 10),
				category,
				rule,
				badFile: `${key}-bad.ssl`,
				expectedFile: `${key}-expected.ssl`,
			});
		}

		const fixture = fixtures.get(key)!;
		const filePath = path.join(fixtureDir, file);

		if (type === "bad") {
			fixture.badFile = file;
			fixture.badContent = fs.readFileSync(filePath, "utf-8");
		} else {
			fixture.expectedFile = file;
			fixture.expectedContent = fs.readFileSync(filePath, "utf-8");
		}
	}

	// Return only complete pairs
	return Array.from(fixtures.values())
		.filter((f) => f.badContent && f.expectedContent)
		.sort((a, b) => a.number - b.number);
}

/**
 * Print formatted fixture report
 */
function printFixturesReport(): void {
	const fixtures = loadAllFixtures();

	console.log("\n" + "=".repeat(100));
	console.log(
		"SSL STYLE GUIDE TEST FIXTURES REPORT".padEnd(
			50 + ("SSL STYLE GUIDE TEST FIXTURES REPORT".length - 30) / 2
		)
	);
	console.log("=".repeat(100) + "\n");

	console.log(`📊 Total Fixtures: ${fixtures.length}\n`);

	// Group by category
	const byCategory = new Map<string, StyleGuideFixture[]>();

	for (const fixture of fixtures) {
		if (!byCategory.has(fixture.category)) {
			byCategory.set(fixture.category, []);
		}
		byCategory.get(fixture.category)!.push(fixture);
	}

	let totalRules = 0;

	for (const [category, categoryFixtures] of Array.from(byCategory.entries()).sort()) {
		totalRules += categoryFixtures.length;

		const categoryTitle = `${category.toUpperCase()} (${categoryFixtures.length} rules)`;
		console.log(`\n📁 ${categoryTitle}`);
		console.log("-".repeat(categoryTitle.length + 4));

		for (const fixture of categoryFixtures) {
			const ruleDisplay = fixture.rule.replace(/-/g, " ");
			const num = fixture.number.toString().padStart(2, "0");

			// Show file sizes
			const badSize = fixture.badContent?.length || 0;
			const expectedSize = fixture.expectedContent?.length || 0;
			const sizeDiff = expectedSize - badSize;
			const sizeIcon = sizeDiff > 0 ? "↑" : sizeDiff < 0 ? "↓" : "→";

			console.log(`  ${num}. ${ruleDisplay}`);
			console.log(`      📄 bad: ${badSize} bytes, expected: ${expectedSize} bytes (${sizeIcon})`);

			// Show if content differs
			if (fixture.badContent === fixture.expectedContent) {
				console.log(`      ⚠️  Note: bad and expected are identical`);
			}
		}
	}

	console.log("\n" + "=".repeat(100));
	console.log(`\n✅ Total: ${totalRules} test rules across ${byCategory.size} categories\n`);

	// Print test run command
	console.log("To run tests:");
	console.log("  npm test");
	console.log();
}

/**
 * Generate markdown fixture documentation
 */
function generateMarkdown(): string {
	const fixtures = loadAllFixtures();

	let md = "# SSL Style Guide Test Fixtures\n\n";
	md += `**Total Fixtures:** ${fixtures.length}\n\n`;

	// Group by category
	const byCategory = new Map<string, StyleGuideFixture[]>();

	for (const fixture of fixtures) {
		if (!byCategory.has(fixture.category)) {
			byCategory.set(fixture.category, []);
		}
		byCategory.get(fixture.category)!.push(fixture);
	}

	for (const [category, categoryFixtures] of Array.from(byCategory.entries()).sort()) {
		md += `## ${category.toUpperCase()}\n\n`;
		md += `| # | Rule | Bad File | Expected File |\n`;
		md += `|---|------|----------|---------------|\n`;

		for (const fixture of categoryFixtures) {
			const ruleDisplay = fixture.rule.replace(/-/g, " ");
			md += `| ${fixture.number} | ${ruleDisplay} | `;
			md += `\`${fixture.badFile}\` | \`${fixture.expectedFile}\` |\n`;
		}

		md += "\n";
	}

	return md;
}

// Export functions
export { loadAllFixtures, printFixturesReport, generateMarkdown, StyleGuideFixture };

// Run if executed directly
if (require.main === module) {
	const args = process.argv.slice(2);

	if (args.includes("--markdown")) {
		console.log(generateMarkdown());
	} else {
		printFixturesReport();
	}
}
