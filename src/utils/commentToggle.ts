interface ToggleResult {
	lines: string[];
	action: "commented" | "uncommented";
}

const INLINE_PREFIX = "/*";

function getIndent(line: string): string {
	const match = line.match(/^\s*/);
	return match ? match[0] : "";
}

function isBlockComment(lines: string[]): boolean {
	if (lines.length < 2) {
		return false;
	}
	const first = lines[0].trim();
	const last = lines[lines.length - 1].trim();
	return first === "/*" && last === ";";
}

function isInlineComment(line: string): boolean {
	return line.trimStart().startsWith(INLINE_PREFIX);
}

function removeInlineComment(line: string): string {
	const indent = getIndent(line);
	const trimmed = line.trimStart();
	if (!trimmed.startsWith(INLINE_PREFIX)) {
		return line;
	}
	let remainder = trimmed.slice(INLINE_PREFIX.length);
	if (remainder.startsWith(" ")) {
		remainder = remainder.slice(1);
	}
	return indent + remainder;
}

function addInlineComment(line: string): string {
	const indent = getIndent(line);
	const content = line.slice(indent.length);
	const trimmed = content.trimEnd();
	const trailingWhitespace = content.slice(trimmed.length);
	if (!trimmed) {
		return `${indent}/* ;${trailingWhitespace}`;
	}
	let commented = `${indent}/* ${trimmed}`;
	if (!trimmed.endsWith(";")) {
		commented += " ;";
	}
	commented += trailingWhitespace;
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

	const allInline = lines.every(isInlineComment);
	if (allInline) {
		return { lines: lines.map(removeInlineComment), action: "uncommented" };
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
