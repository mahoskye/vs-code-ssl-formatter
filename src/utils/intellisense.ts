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
 * Merge built-in functions with user-provided overrides/additions from config.
 * Custom entries with the same name replace the built-in metadata.
 */
export function getConfiguredFunctions(config: vscode.WorkspaceConfiguration): SSLFunction[] {
    const customFunctions = config.get<CustomFunctionConfig[]>(
        CONFIG_KEYS.INTELLISENSE_CUSTOM_FUNCTIONS,
        CONFIG_DEFAULTS[CONFIG_KEYS.INTELLISENSE_CUSTOM_FUNCTIONS] as unknown as CustomFunctionConfig[]
    ) || [];

    const merged = new Map<string, SSLFunction>();

    SSL_BUILTIN_FUNCTIONS.forEach(func => {
        merged.set(func.name.toUpperCase(), func);
    });

    customFunctions.forEach(func => {
        if (!func || !func.name) {
            return;
        }
        const key = func.name.toUpperCase();
        const base = merged.get(key) || {
            name: func.name,
            description: "",
            params: "()"
        };

        const normalized: SSLFunction = {
            ...base,
            ...func,
            name: func.name,
            description: func.description || base.description || "Custom function",
            params: func.params || base.params || "()",
            signature: func.signature || func.params ? `${func.name}${func.params || ""}` : base.signature,
            returnType: func.returnType || base.returnType,
            category: func.category || base.category,
            frequency: func.frequency || base.frequency,
            untypedSignature: func.untypedSignature || base.untypedSignature
        };

        merged.set(key, normalized);
    });

    return Array.from(merged.values());
}

/**
 * Merge built-in classes with user-provided overrides/additions from config.
 * Custom entries with the same name replace the built-in metadata.
 */
export function getConfiguredClasses(config: vscode.WorkspaceConfiguration): SSLClass[] {
    const customClasses = config.get<CustomClassConfig[]>(
        CONFIG_KEYS.INTELLISENSE_CUSTOM_CLASSES,
        CONFIG_DEFAULTS[CONFIG_KEYS.INTELLISENSE_CUSTOM_CLASSES] as unknown as CustomClassConfig[]
    ) || [];

    const merged = new Map<string, SSLClass>();

    SSL_BUILTIN_CLASSES.forEach(cls => {
        merged.set(cls.name.toUpperCase(), cls);
    });

    customClasses.forEach(cls => {
        if (!cls || !cls.name) {
            return;
        }
        const key = cls.name.toUpperCase();
        const base = merged.get(key) || {
            name: cls.name,
            description: "",
            instantiation: `${cls.name}{}`,
            usage: "",
            methods: [],
            properties: []
        };

        const normalized: SSLClass = {
            ...base,
            ...cls,
            name: cls.name,
            description: cls.description || base.description || "Custom class",
            instantiation: cls.instantiation || base.instantiation || `${cls.name}{}`,
            usage: cls.usage || base.usage || `${cls.name.toLowerCase()} := ${cls.name}{};`,
            methods: cls.methods || base.methods || [],
            properties: cls.properties || base.properties || []
        };

        merged.set(key, normalized);
    });

    return Array.from(merged.values());
}
