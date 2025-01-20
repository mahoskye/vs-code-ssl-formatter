/**
 * Ensures that there is a newline after semicolons only when multiple statements
 * exist on the same line. Does not add newlines after single statements.
 *
 * Examples:
 * "thisis;aline;" -> "thisis;\naline;"
 * "aline;" -> "aline;"
 */
export function ensureSemicolonNewline(text: string): string {
  return text.replace(/;(?!\s*$)/gm, ";\n");
}
