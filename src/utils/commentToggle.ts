import { PATTERNS } from "../constants/patterns";

interface ToggleResult {
	lines: string[];
	action: "commented" | "uncommented";
}

const INLINE_PREFIX = "/*";

function getIndent(line: string): string {
	const match = line.match(/^(\s*)/);
	return match ? match[0] : "";
}

function isBlockComment(lines: string[]): boolean {
	if (lines.length < 2) {
		return false;
	}
	const first = lines[0].trim();
	const last = lines[lines.length - 1].trim();

	// Check if first line starts with /* and last line ends with ;
	if (!PATTERNS.COMMENT.MULTILINE_START.test(first) || !PATTERNS.COMMENT.MULTILINE_END.test(last)) {
		return false;
	}

	// For a valid multi-line block comment, no intermediate line should end with ;
	// (because ; terminates the comment in SSL)
	for (let i = 0; i < lines.length - 1; i++) {
		if (PATTERNS.COMMENT.MULTILINE_END.test(lines[i].trim())) {
			return false;
		}
	}

	return true;
}

function isInlineComment(line: string): boolean {
	return PATTERNS.COMMENT.MULTILINE_START.test(line.trimStart());
}

function removeInlineComment(line: string): string {
	const indent = getIndent(line);
	const trimmed = line.trimStart();

	if (!isInlineComment(line)) {
		return line;
	}

	let remainder = trimmed.replace(PATTERNS.COMMENT.MULTILINE_START, "");

	// Remove optional space after /*
	if (remainder.startsWith(" ")) {
		remainder = remainder.slice(1);
	}

	// Remove trailing semicolon if present (inline comments often end with ;)
	// But be careful not to remove needed semicolons for code.
	// However, the toggle logic usually adds ' ;' at the end.
	// Let's strip the specific ' ;' or just rely on user intent?
	// The previous implementation stripped checks for end semicolon.
	// "commented += " ;" in addInlineComment.
	// So distinct from " /* text ;" -> "text"

	// Reverse of addInlineComment:
	// It adds /* at start and ; at end.

	// Only remove trailing " ;" (space + semicolon) which is what addInlineComment adds
	// This preserves original semicolons that didn't have the space
	if (remainder.endsWith(" ;")) {
		remainder = remainder.slice(0, -2);
	}

	return indent + remainder;
}

function addInlineComment(line: string): string {
	const indent = getIndent(line);
	const content = line.slice(indent.length);
	const trimmed = content.trimEnd();

	if (!trimmed) {
		// Empty line case
		return `${indent}/* ;`;
	}

	let commented = `${indent}/* ${trimmed}`;
	if (!trimmed.endsWith(";")) {
		commented += " ;";
	}
	return commented;
}

function commentBlock(lines: string[]): ToggleResult {
	const indent = getIndent(lines[0] ?? "");
	const result = [indent + "/*", ...lines, indent + ";"];
	return { lines: result, action: "commented" };
}

export function toggleCommentLines(lines: string[]): ToggleResult {
	if (lines.length === 0) {
		return { lines, action: "commented" };
	}

	if (isBlockComment(lines)) {
		return { lines: lines.slice(1, -1), action: "uncommented" };
	}

	// Check if all non-empty lines are inline comments
	const nonEmptyLines = lines.filter(l => l.trim().length > 0);
	const allInline = nonEmptyLines.length > 0 && nonEmptyLines.every(isInlineComment);

	if (allInline) {
		return { lines: lines.map(line => isInlineComment(line) ? removeInlineComment(line) : line), action: "uncommented" };
	}

	// Check for semicolons to avoid broken block comments
	if (lines.some(l => l.includes(';'))) {
		const action = "commented"; // Assume we are commenting unless we detect line comments to remove?
		// Wait, if we are here, we passed "allInline" check (which handles uncommenting)
		// So we are likely commenting.
		// But what if mixed?
		// Simplify: just apply line comments to all lines.
		return {
			lines: lines.map(line => addInlineComment(line)),
			action: action
		};
	}

	if (lines.length === 1) {
		const line = lines[0];
		if (isInlineComment(line)) {
			return { lines: [removeInlineComment(line)], action: "uncommented" };
		}
		return { lines: [addInlineComment(line)], action: "commented" };
	}

	return commentBlock(lines);
}
