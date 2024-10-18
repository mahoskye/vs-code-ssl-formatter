export function ensureSemicolonNewline(text: string): string {
    return text.replace(/;(?!$)/gm, ";\n");
}
