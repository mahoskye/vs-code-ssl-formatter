import * as cp from "child_process";
import * as fs from "fs";
import * as os from "os";
import * as vscode from "vscode";
import {
    SSLClass,
    SSLFunction,
    SSL_BUILTIN_CLASSES,
    SSL_BUILTIN_FUNCTIONS,
    SSL_KEYWORD_DESCRIPTIONS
} from "../constants/language";

interface ExportedParameter {
    name: string;
    type?: string;
    required?: boolean;
    description?: string;
}

interface ExportedFunction {
    name: string;
    description?: string;
    return_type?: string;
    parameters?: ExportedParameter[];
}

interface ExportedConstructor {
    signature: string;
    description?: string;
    parameters?: ExportedParameter[];
}

interface ExportedClassMember {
    name: string;
    returns?: string;
    description?: string;
}

interface ExportedClass {
    name: string;
    summary?: string;
    constructors?: ExportedConstructor[];
    methods?: ExportedClassMember[];
    properties?: ExportedClassMember[];
}

interface ExportedInventory {
    version: string;
    functions: ExportedFunction[];
    classes: ExportedClass[];
    operators?: unknown[];
    keywords?: string[];
}

interface LoadedInventory {
    functions: SSLFunction[];
    classes: SSLClass[];
    keywords: Record<string, string>;
    sourceVersion: string;
}

let loaded: LoadedInventory | null = null;

function buildParamsString(params: ExportedParameter[] = [], untyped: boolean): string {
    if (params.length === 0) {
        return "()";
    }
    const parts = params.map(p => {
        const name = p.required === false ? `[${p.name}]` : p.name;
        if (untyped || !p.type) {
            return name;
        }
        return `${p.type} ${name}`;
    });
    return `(${parts.join(", ")})`;
}

function mapFunction(fn: ExportedFunction): SSLFunction {
    const typedParams = buildParamsString(fn.parameters, false);
    const untypedParams = buildParamsString(fn.parameters, true);
    return {
        name: fn.name,
        description: fn.description ?? "",
        params: typedParams,
        returns: fn.return_type,
        returnType: fn.return_type,
        signature: `${fn.name}${typedParams}`,
        untypedSignature: `${fn.name}${untypedParams}`
    };
}

function mapClass(cls: ExportedClass): SSLClass {
    const ctor = cls.constructors?.[0];
    const instantiation = ctor?.signature ?? `${cls.name}{}`;
    const methods = (cls.methods ?? []).map(m => m.name);
    const properties = (cls.properties ?? []).map(p => p.name);
    return {
        name: cls.name,
        description: cls.summary ?? "",
        instantiation,
        usage: `o${cls.name} := ${instantiation};`,
        methods,
        properties
    };
}

function mapKeywords(keywords: string[]): Record<string, string> {
    const result: Record<string, string> = {};
    for (const kw of keywords) {
        const upper = kw.toUpperCase();
        result[upper] = SSL_KEYWORD_DESCRIPTIONS[upper] ?? "";
    }
    // Preserve any documentation we have for keywords the LSP didn't list.
    for (const [upper, desc] of Object.entries(SSL_KEYWORD_DESCRIPTIONS)) {
        if (!(upper in result)) {
            result[upper] = desc;
        }
    }
    return result;
}

function getServerBinaryPath(context: vscode.ExtensionContext): string | null {
    const platform = os.platform();
    const arch = os.arch();
    const platformName = platform === "win32" ? "windows" : platform === "darwin" ? "darwin" : platform === "linux" ? "linux" : null;
    const archName = arch === "arm64" ? "arm64" : arch === "x64" ? "amd64" : null;
    if (!platformName || !archName) {
        return null;
    }
    const ext = platform === "win32" ? ".exe" : "";
    const binaryName = `starlims-lsp-${platformName}-${archName}${ext}`;
    const fullPath = context.asAbsolutePath(`server/${binaryName}`);
    return fs.existsSync(fullPath) ? fullPath : null;
}

/**
 * Runs the bundled LSP binary with `--export-signatures` and parses the result.
 * Falls back to null on any failure; callers should use the hardcoded subset.
 */
async function fetchInventory(binaryPath: string): Promise<ExportedInventory | null> {
    return new Promise(resolve => {
        const child = cp.spawn(binaryPath, ["--export-signatures"], { stdio: ["ignore", "pipe", "pipe"] });
        const chunks: Buffer[] = [];
        let timedOut = false;
        const timeout = setTimeout(() => {
            timedOut = true;
            child.kill();
        }, 10_000);

        child.stdout.on("data", (chunk: Buffer) => chunks.push(chunk));
        child.on("error", () => {
            clearTimeout(timeout);
            resolve(null);
        });
        child.on("close", code => {
            clearTimeout(timeout);
            if (timedOut || code !== 0) {
                resolve(null);
                return;
            }
            try {
                const text = Buffer.concat(chunks).toString("utf-8");
                resolve(JSON.parse(text) as ExportedInventory);
            } catch {
                resolve(null);
            }
        });
    });
}

/**
 * Loads the full SSL element inventory from the bundled LSP binary.
 *
 * Called once during extension activation. On success, native fallback
 * providers (completion, hover, signature help, inlay hints, diagnostics)
 * see the full ~330-function / ~29-class inventory instead of the small
 * hardcoded subset in `constants/language.ts`. Failures (binary missing,
 * spawn error, malformed output) leave the loader inactive — getters
 * transparently return the hardcoded subset.
 */
export async function loadInventory(context: vscode.ExtensionContext): Promise<void> {
    const binaryPath = getServerBinaryPath(context);
    if (!binaryPath) {
        return;
    }
    const exported = await fetchInventory(binaryPath);
    if (!exported) {
        return;
    }
    loaded = {
        functions: exported.functions.map(mapFunction),
        classes: exported.classes.map(mapClass),
        keywords: mapKeywords(exported.keywords ?? []),
        sourceVersion: exported.version
    };
}

export function getBuiltinFunctions(): SSLFunction[] {
    return loaded?.functions ?? SSL_BUILTIN_FUNCTIONS;
}

export function getBuiltinClasses(): SSLClass[] {
    return loaded?.classes ?? SSL_BUILTIN_CLASSES;
}

export function getKeywordDescriptions(): Record<string, string> {
    return loaded?.keywords ?? SSL_KEYWORD_DESCRIPTIONS;
}

export function getInventorySourceVersion(): string | null {
    return loaded?.sourceVersion ?? null;
}

export function isInventoryLoaded(): boolean {
    return loaded !== null;
}
