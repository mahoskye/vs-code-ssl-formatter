export function ensureSingleFinalNewline(text: string): string {
    return text.trimEnd() + "\n";
}
