export function correctKeywordCasing(text: string, keywords: string[]): string {
    const keywordRegex = new RegExp(`:(${keywords.join("|")})\\b`, "gi");
    return text.replace(keywordRegex, (match) => {
        const keyword = match.slice(1).toUpperCase(); // Remove ':' and uppercase
        return `:${keyword}`;
    });
}
