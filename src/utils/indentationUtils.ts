// src/utils/indentationUtils.ts
export function getIndentationString(
    level: number,
    tabSize: number,
    insertSpaces: boolean
): string {
    if (level <= 0) {
        return "";
    }
    if (insertSpaces) {
        return " ".repeat(level * tabSize);
    } else {
        return "\t".repeat(level);
    }
}
