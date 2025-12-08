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
	return PATTERNS.COMMENT.MULTILINE_START.test(first) && PATTERNS.COMMENT.MULTILINE_END.test(last);
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

	// Try to remove trailing ;
	if (remainder.endsWith(";")) {
		remainder = remainder.slice(0, -1);
	}
	// Try to remove trailing space before ;
	if (remainder.endsWith(" ")) {
		remainder = remainder.slice(0, -1);
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

	if (lines.length === 1) {
		const line = lines[0];
		if (isInlineComment(line)) {
			return { lines: [removeInlineComment(line)], action: "uncommented" };
		}
		return { lines: [addInlineComment(line)], action: "commented" };
	}

	return commentBlock(lines);
}
