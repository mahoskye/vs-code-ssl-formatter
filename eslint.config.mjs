// ESLint flat config. Carries over the rules of the former .eslintrc.json;
// `semi` moved from @typescript-eslint to @stylistic in typescript-eslint v8.
import stylistic from "@stylistic/eslint-plugin";
import tseslint from "typescript-eslint";

export default tseslint.config(
    {
        ignores: ["out/**", "dist/**", "**/*.d.ts"],
    },
    {
        files: ["src/**/*.ts"],
        languageOptions: {
            parser: tseslint.parser,
            ecmaVersion: 2020,
            sourceType: "module",
        },
        plugins: {
            "@typescript-eslint": tseslint.plugin,
            "@stylistic": stylistic,
        },
        rules: {
            "@typescript-eslint/naming-convention": [
                "warn",
                {
                    selector: "variable",
                    format: ["camelCase", "UPPER_CASE"],
                    leadingUnderscore: "allow",
                },
                {
                    selector: "parameter",
                    format: ["camelCase"],
                    leadingUnderscore: "allow",
                },
                {
                    selector: "typeLike",
                    format: ["PascalCase"],
                },
                {
                    selector: "memberLike",
                    modifiers: ["private"],
                    format: ["camelCase"],
                    leadingUnderscore: "allow",
                },
                {
                    selector: "method",
                    format: ["camelCase"],
                    leadingUnderscore: "allow",
                },
                {
                    selector: "objectLiteralMethod",
                    format: ["camelCase", "UPPER_CASE"],
                },
            ],
            // Type-only imports must say so: the ESM test loader keeps every
            // import it is not told is a type.
            "@typescript-eslint/consistent-type-imports": ["error", { fixStyle: "inline-type-imports" }],
            "@stylistic/semi": "warn",
            curly: "warn",
            eqeqeq: "warn",
            "no-throw-literal": "warn",
        },
    }
);
