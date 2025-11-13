#!/usr/bin/env node

// Adds `untypedSignature` to each function entry in top50_by_count.json.
// Rules:
// - If `untypedSignature` already exists, leave it as-is.
// - If `signature` is null, do nothing (no `untypedSignature`).
// - If `signature` starts with `return X(...);`, set to `X(...args...)`.
// - Otherwise parse a C#-style declaration:
//     [modifiers] ReturnType Name(Type1 p1, Type2 p2, ...)
//   and emit: `Name(p1, p2, ...)`.
// - Strip generics, array markers, `params`, `ref`, `out`, etc. from parameters.
// - Whitespace is normalized; output JSON is pretty-printed with 2 spaces.

const fs = require('fs');
const path = require('path');

const ROOT = path.resolve(__dirname, '..');
const INPUT = path.join(ROOT, 'top50_by_count.json');

/** Extract untypedSignature from a `return X(...);` wrapper. */
function fromReturnWrapper(signature) {
  const m = signature.match(/^\s*return\s+([A-Za-z_][A-Za-z0-9_]*)\s*\((.*)\)\s*;\s*$/);
  if (!m) return null;
  const name = m[1];
  const args = m[2].trim();
  return args ? `${name}(${args})` : `${name}()`;
}

/** Parse a C#-style declaration and return untypedSignature. */
function fromDeclaration(signature) {
  // Remove trailing semicolon if present
  let s = signature.trim().replace(/;\s*$/, '');

  // Match: [modifiers] ReturnType Name(params)
  const decl = s.match(/^(?:public|private|protected|internal|static|virtual|sealed|override|abstract|async|extern|new|unsafe|partial|readonly|volatile|params|ref|out|in|this|implicit|explicit)\s+.*$/)
    ? s
    : s;

  const fnMatch = decl.match(/^(?:[A-Za-z0-9_<>\[\]\s]+\s+)*([A-Za-z_][A-Za-z0-9_]*)\s*\((.*)\)$/);
  if (!fnMatch) return null;

  const name = fnMatch[1];
  const paramsRaw = fnMatch[2].trim();
  if (!paramsRaw) return `${name}()`;

  const params = splitParams(paramsRaw)
    .map(normalizeParam)
    .filter(Boolean);

  return `${name}(${params.join(', ')})`;
}

/** Split parameter list respecting nested generics and parens. */
function splitParams(s) {
  const parts = [];
  let buf = '';
  let depth = 0;
  let inString = false;
  let stringChar = '';

  for (let i = 0; i < s.length; i++) {
    const ch = s[i];

    if (inString) {
      buf += ch;
      if (ch === stringChar && s[i - 1] !== '\\') {
        inString = false;
      }
      continue;
    }

    if (ch === '"' || ch === "'") {
      inString = true;
      stringChar = ch;
      buf += ch;
      continue;
    }

    if (ch === '<' || ch === '(' || ch === '[') {
      depth++;
      buf += ch;
      continue;
    }
    if (ch === '>' || ch === ')' || ch === ']') {
      depth = Math.max(0, depth - 1);
      buf += ch;
      continue;
    }

    if (ch === ',' && depth === 0) {
      parts.push(buf.trim());
      buf = '';
      continue;
    }

    buf += ch;
  }

  if (buf.trim()) parts.push(buf.trim());
  return parts;
}

/** Normalize a single parameter: drop type/modifiers, keep identifier. */
function normalizeParam(param) {
  if (!param) return null;

  // Remove default values: `= something` (greedy to end)
  let p = param.replace(/=.*/, '').trim();

  // Tokens split by whitespace
  const tokens = p.split(/\s+/).filter(Boolean);
  if (tokens.length === 0) return null;

  // Handle `params`, `ref`, `out`, `in` etc. as modifiers
  const modifierSet = new Set(['params', 'ref', 'out', 'in']);

  // If last token looks like `[]`, merge it; otherwise last token is name
  // Strategy: walk from the end backwards to find identifier-like token.
  for (let i = tokens.length - 1; i >= 0; i--) {
    const t = tokens[i];
    // Identifier may contain [] when array arg is written as `name[]`
    if (/^[A-Za-z_][A-Za-z0-9_]*\s*(\[\s*\])*$/.test(t)) {
      return t.replace(/\s+/g, '');
    }
    if (modifierSet.has(t)) {
      // keep scanning left
      continue;
    }
  }

  // Fallback: if single token and identifier-like, return it
  if (tokens.length === 1 && /^[A-Za-z_][A-Za-z0-9_]*$/.test(tokens[0])) {
    return tokens[0];
  }

  return null;
}

function buildUntypedSignature(signature) {
  if (!signature || typeof signature !== 'string') return null;

  // Wrapper-style `return Foo(...);`
  if (/^\s*return\s+/i.test(signature)) {
    return fromReturnWrapper(signature);
  }

  // Otherwise, treat as declaration
  return fromDeclaration(signature);
}

function main() {
  const raw = fs.readFileSync(INPUT, 'utf8');
  const data = JSON.parse(raw);

  if (!data || !Array.isArray(data.functions)) {
    console.error('Invalid top50_by_count.json: missing functions array');
    process.exit(1);
  }

  let updated = 0;

  for (const fn of data.functions) {
    if (fn.untypedSignature) continue; // respect existing

    const ut = buildUntypedSignature(fn.signature);
    if (ut) {
      fn.untypedSignature = ut;
      updated++;
    }
  }

  fs.writeFileSync(INPUT, JSON.stringify(data, null, 2) + '\n', 'utf8');
  console.log(`Added untypedSignature to ${updated} functions.`);
}

if (require.main === module) {
  main();
}
