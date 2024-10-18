export function ensureSingleFinalNewline(text: string): string {
    return text.replace(/\n+$/, "\n");
}
