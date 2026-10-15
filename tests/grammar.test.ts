import { describe, it, before } from "mocha";
import { expect } from "chai";
import { execFileSync } from "child_process";
import { existsSync, readFileSync } from "fs";
import { resolve } from "path";
// Both packages are CommonJS; under the ESM test loader only the default
// import exposes their exports.
import oniguruma from "vscode-oniguruma";
import vsctm, { type IGrammar } from "vscode-textmate";

/**
 * Tokenizes with the grammar the extension ships (synced from ssl-style-guide,
 * see scripts/sync-grammar.mjs), the same way VS Code does. A grammar change
 * belongs upstream; these tests pin what the shipped copy highlights (#108).
 */

// Unit tests run from the project root (see the test:unit script).
const ROOT = process.cwd();

let grammar: IGrammar;

before(async () => {
    const wasm = readFileSync(resolve(ROOT, "node_modules/vscode-oniguruma/release/onig.wasm"));
    await oniguruma.loadWASM(wasm.buffer.slice(wasm.byteOffset, wasm.byteOffset + wasm.byteLength));
    const registry = new vsctm.Registry({
        onigLib: Promise.resolve({
            createOnigScanner: (patterns: string[]) => new oniguruma.OnigScanner(patterns),
            createOnigString: (s: string) => new oniguruma.OnigString(s),
        }),
        loadGrammar: async (scopeName: string) => {
            if (scopeName !== "source.ssl") {
                return null;
            }
            const path = resolve(ROOT, "syntaxes/ssl.tmLanguage.json");
            return vsctm.parseRawGrammar(readFileSync(path, "utf-8"), path);
        },
    });
    const loaded = await registry.loadGrammar("source.ssl");
    if (!loaded) {
        throw new Error("Could not load syntaxes/ssl.tmLanguage.json");
    }
    grammar = loaded;
});

/** Tokenizes `source` and returns each token's text with its scopes. */
function tokenize(source: string): Array<{ text: string; scopes: string[] }> {
    const out: Array<{ text: string; scopes: string[] }> = [];
    let state = vsctm.INITIAL;
    for (const line of source.split("\n")) {
        const result = grammar.tokenizeLine(line, state);
        for (const token of result.tokens) {
            out.push({ text: line.slice(token.startIndex, token.endIndex), scopes: token.scopes });
        }
        state = result.ruleStack;
    }
    return out;
}

/** The scopes of the first token whose text is exactly `text`. */
function scopesOf(source: string, text: string): string[] {
    const token = tokenize(source).find(t => t.text === text);
    expect(token, `no token "${text}" in ${JSON.stringify(source)}`).to.exist;
    return token!.scopes;
}

describe("TextMate grammar", () => {
    describe("literals", () => {
        it("highlights .T. and .F. after whitespace", () => {
            expect(scopesOf("bDone := .T.;", ".T.")).to.include("constant.language.boolean.ssl");
            expect(scopesOf("bDone := .F.;", ".F.")).to.include("constant.language.boolean.ssl");
        });

        it("highlights NIL", () => {
            expect(scopesOf("x := NIL;", "NIL")).to.include("constant.language.nil.ssl");
        });

        it("highlights numbers, including scientific notation", () => {
            expect(scopesOf("n := 42;", "42")).to.include("constant.numeric.ssl");
            expect(scopesOf("n := 1.5e3;", "1.5e3")).to.include("constant.numeric.ssl");
        });
    });

    describe("keywords", () => {
        it("highlights colon keywords, :RESUME included", () => {
            for (const kw of [":IF", ":ELSE", ":ENDIF", ":WHILE", ":FOR", ":NEXT", ":TRY", ":CATCH", ":RESUME", ":RETURN"]) {
                expect(scopesOf(`${kw} x;`, kw), kw).to.include("keyword.control.ssl");
            }
        });

        it("does not highlight a lowercase colon keyword (the v12 lexer is case-sensitive)", () => {
            expect(tokenize(":try;").some(t => t.scopes.includes("keyword.control.ssl"))).to.equal(false);
        });

        it("names the procedure and class being declared", () => {
            expect(scopesOf(":PROCEDURE LoadSamples;", "LoadSamples")).to.include("entity.name.function.ssl");
            expect(scopesOf(":CLASS SampleBatch;", "SampleBatch")).to.include("entity.name.type.class.ssl");
        });

        it("names labels, joined or spaced", () => {
            expect(scopesOf(":LABEL Retry;", "Retry")).to.include("entity.name.label.ssl");
            expect(scopesOf(":LABELRetry;", "Retry")).to.include("entity.name.label.ssl");
        });
    });

    describe("operators", () => {
        it("highlights ** and the dotted logical operators", () => {
            expect(scopesOf("n := 2 ** 3;", "**")).to.include("keyword.operator.ssl");
            expect(scopesOf("b := x .AND. y;", ".AND.")).to.include("keyword.operator.ssl");
            expect(scopesOf("b := .NOT. y;", ".NOT.")).to.include("keyword.operator.ssl");
        });
    });

    describe("comments", () => {
        it("ends a comment at the first semicolon", () => {
            const tokens = tokenize("/* note; x := 1;");
            const x = tokens.find(t => t.text.trim() === "x");
            expect(x, "token after the comment").to.exist;
            expect(x!.scopes).to.not.include("comment.block.ssl");
        });

        it("runs a comment across lines until its semicolon", () => {
            const second = tokenize("/* first line\nsecond line;\nx := 1;").find(t => t.text.startsWith("second"));
            expect(second!.scopes).to.include("comment.block.ssl");
        });
    });

    describe("strings", () => {
        it("highlights the three string forms", () => {
            expect(scopesOf('s := "abc";', '"')).to.include("string.quoted.double.ssl");
            expect(scopesOf("s := 'abc';", "'")).to.include("string.quoted.single.ssl");
            expect(scopesOf("s := [abc];", "[abc]")).to.include("string.quoted.other.bracket.ssl");
        });

        it("highlights SQL placeholders and ODBC escapes inside strings", () => {
            const source = 'SQLExecute("SELECT * FROM T WHERE ID = ?nId? AND D > {fn NOW()}");';
            expect(scopesOf(source, "?nId?")).to.include("keyword.other.database-parameter.ssl");
            expect(scopesOf(source, "{fn NOW()}")).to.include("support.function.odbc-escape.ssl");
        });
    });

    describe("built-ins and code blocks", () => {
        it("highlights built-in functions, the bitwise _AND family included", () => {
            expect(scopesOf("n := Len(s);", "Len")).to.include("support.function.ssl");
            expect(scopesOf("n := _AND(a, b);", "_AND")).to.include("support.function.ssl");
        });

        it("marks a code block with no bound variable as invalid", () => {
            expect(tokenize("b := {|| 1};").some(t => t.scopes.includes("invalid.illegal.ssl"))).to.equal(true);
        });

        it("ends a code block's parameter list at its second bar", () => {
            const after = tokenize("b := {|x| x + 1};\ny := 2;").find(t => t.text.trim() === "y");
            expect(after!.scopes).to.not.include("variable.parameter.ssl");
        });
    });

    describe("against the SSL reference (bundled server's --export-signatures)", () => {
        interface Inventory {
            keywords: string[];
            functions: Array<{ name: string }>;
            classes: Array<{ name: string }>;
            operators: Array<{ symbol: string }>;
        }

        // The binary for this host, as lspClient.ts picks it.
        const platform = { win32: "windows", darwin: "darwin", linux: "linux" }[process.platform as string];
        const arch = process.platform === "win32" ? "amd64" : { x64: "amd64", arm64: "arm64" }[process.arch as string];
        const binary = resolve(ROOT, "server", `starlims-lsp-${platform}-${arch}${process.platform === "win32" ? ".exe" : ""}`);

        let inventory: Inventory;

        before(function () {
            if (!platform || !arch || !existsSync(binary)) {
                this.skip();
            }
            inventory = JSON.parse(execFileSync(binary, ["--export-signatures"], { encoding: "utf-8", maxBuffer: 64 * 1024 * 1024 }));
        });

        it("highlights every reference keyword as a colon keyword", () => {
            const missed = inventory.keywords.filter(kw =>
                !tokenize(`:${kw} x;`).some(t => t.text === `:${kw}` && t.scopes.includes("keyword.control.ssl")));
            expect(missed, "keywords the grammar does not highlight").to.deep.equal([]);
        });

        it("highlights every reference function", () => {
            const missed = inventory.functions.map(f => f.name).filter(name =>
                !scopesOf(`x := ${name}(a);`, name).includes("support.function.ssl"));
            expect(missed, "functions the grammar does not highlight").to.deep.equal([]);
        });

        it("highlights every reference class when instantiated", () => {
            const missed = inventory.classes.map(c => c.name).filter(name =>
                !tokenize(`o := ${name}{};`).some(t => t.text.includes(name) && t.scopes.includes("support.class.ssl")));
            expect(missed, "classes the grammar does not highlight").to.deep.equal([]);
        });

        it("highlights every reference operator", () => {
            const missed = inventory.operators.map(o => o.symbol).filter(op =>
                !tokenize(`x ${op} y;`).some(t => t.text.trim() === op && t.scopes.includes("keyword.operator.ssl")));
            expect(missed, "operators the grammar does not highlight").to.deep.equal([]);
        });
    });
});
