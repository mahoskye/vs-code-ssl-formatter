import { describe, it } from "mocha";
import { expect } from "chai";
import { readFileSync, readdirSync, statSync } from "fs";
import { join } from "path";
import { CONFIG_KEYS, CONFIG_DEFAULTS } from "../src/constants/config";

/**
 * Keeps the contributed settings, the CONFIG_KEYS constants and the settings
 * the code reads in step (#107): a setting nobody reads, or a read of a
 * setting nobody contributes, fails here.
 */

const ROOT = process.cwd();
const pkg = JSON.parse(readFileSync(join(ROOT, "package.json"), "utf-8"));
const contributed: Record<string, { default?: unknown }> = pkg.contributes.configuration.properties;

function sourceFiles(dir: string): string[] {
    return readdirSync(dir).flatMap(name => {
        const path = join(dir, name);
        return statSync(path).isDirectory() ? sourceFiles(path) : path.endsWith(".ts") ? [path] : [];
    });
}

// The code that reads settings: everything but the constants file itself.
const source = sourceFiles(join(ROOT, "src"))
    .filter(f => !f.endsWith(join("constants", "config.ts")))
    .map(f => readFileSync(f, "utf-8"))
    .join("\n");

/** Keys read as `config.get("x")` / `.get<T>('x', …)` / `.update("x", …)` on the `ssl` section. */
function keysReadInSource(): Set<string> {
    const keys = new Set<string>();
    for (const m of source.matchAll(/\.(?:get|update|inspect)(?:<[^(]*>)?\(\s*["']([A-Za-z][\w.]*)["']/g)) {
        keys.add(m[1].startsWith("ssl.") ? m[1] : `ssl.${m[1]}`);
    }
    // editor.tabSize is read from the `editor` section, not `ssl`.
    keys.delete("ssl.tabSize");
    return keys;
}

describe("Contributed settings", () => {
    it("CONFIG_KEYS lists exactly the contributed settings", () => {
        expect(Object.values(CONFIG_KEYS).sort()).to.deep.equal(Object.keys(contributed).sort());
    });

    it("CONFIG_DEFAULTS match the contributed defaults", () => {
        for (const [key, def] of Object.entries(contributed)) {
            expect((CONFIG_DEFAULTS as Record<string, unknown>)[key], key).to.deep.equal(def.default);
        }
    });

    it("every setting the code reads is contributed", () => {
        const notContributed = [...keysReadInSource()].filter(k => !(k in contributed));
        expect(notContributed).to.deep.equal([]);
    });

    it("every contributed setting is read somewhere", () => {
        const read = keysReadInSource();
        const unread = Object.entries(CONFIG_KEYS)
            .filter(([name, key]) => !read.has(key) && !source.includes(`CONFIG_KEYS.${name}`))
            .map(([, key]) => key);
        expect(unread).to.deep.equal([]);
    });
});
