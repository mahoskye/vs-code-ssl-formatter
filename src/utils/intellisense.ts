import * as vscode from "vscode";
import { CONFIG_DEFAULTS, CONFIG_KEYS } from "../constants/config";
import {
    SSL_BUILTIN_CLASSES,
    SSL_BUILTIN_FUNCTIONS,
    SSLClass,
    SSLFunction
} from "../constants/language";

type CustomFunctionConfig = Partial<SSLFunction> & { name: string };
type CustomClassConfig = Partial<SSLClass> & { name: string };

/**
 * Generic helper to merge built-in definitions with user configuration.
 */
function mergeWithConfig<T extends { name: string }, C extends { name: string }>(
    builtIns: T[],
    customItems: C[],
    createDefault: (name: string) => T,
    normalize: (base: T, custom: C) => T
): T[] {
    const merged = new Map<string, T>();

    // Index built-ins
    builtIns.forEach(item => {
        merged.set(item.name.toUpperCase(), item);
    });

    // Merge custom items
    customItems.forEach(custom => {
        if (!custom || !custom.name) {
            return;
        }
        const key = custom.name.toUpperCase();
        const base = merged.get(key) || createDefault(custom.name);

        merged.set(key, normalize(base, custom));
    });

    return Array.from(merged.values());
}

/**
 * Merge built-in functions with user-provided overrides/additions from config.
 */
export function getConfiguredFunctions(config: vscode.WorkspaceConfiguration): SSLFunction[] {
    const customFunctions = config.get<CustomFunctionConfig[]>(
        CONFIG_KEYS.INTELLISENSE_CUSTOM_FUNCTIONS,
        CONFIG_DEFAULTS[CONFIG_KEYS.INTELLISENSE_CUSTOM_FUNCTIONS] as unknown as CustomFunctionConfig[]
    ) || [];

    return mergeWithConfig(
        SSL_BUILTIN_FUNCTIONS,
        customFunctions,
        (name) => ({
            name,
            description: "",
            params: "()"
        } as SSLFunction),
        (base, func) => ({
            ...base,
            ...func,
            name: func.name, // Ensure case from config is preferred if new? or keep base? Logic was: func.name
            description: func.description || base.description || "Custom function",
            params: func.params || base.params || "()",
            signature: func.signature || (func.params ? `${func.name}${func.params || ""}` : base.signature),
            returnType: func.returnType || base.returnType,
            category: func.category || base.category,
            frequency: func.frequency || base.frequency,
            untypedSignature: func.untypedSignature || base.untypedSignature
        })
    );
}

/**
 * Merge built-in classes with user-provided overrides/additions from config.
 */
export function getConfiguredClasses(config: vscode.WorkspaceConfiguration): SSLClass[] {
    const customClasses = config.get<CustomClassConfig[]>(
        CONFIG_KEYS.INTELLISENSE_CUSTOM_CLASSES,
        CONFIG_DEFAULTS[CONFIG_KEYS.INTELLISENSE_CUSTOM_CLASSES] as unknown as CustomClassConfig[]
    ) || [];

    return mergeWithConfig(
        SSL_BUILTIN_CLASSES,
        customClasses,
        (name) => ({
            name,
            description: "",
            instantiation: `${name}{}`,
            usage: "",
            methods: [],
            properties: []
        } as SSLClass),
        (base, cls) => ({
            ...base,
            ...cls,
            name: cls.name,
            description: cls.description || base.description || "Custom class",
            instantiation: cls.instantiation || base.instantiation || `${cls.name}{}`,
            usage: cls.usage || base.usage || `${cls.name.toLowerCase()} := ${cls.name}{};`,
            methods: cls.methods || base.methods || [],
            properties: cls.properties || base.properties || []
        })
    );
}
