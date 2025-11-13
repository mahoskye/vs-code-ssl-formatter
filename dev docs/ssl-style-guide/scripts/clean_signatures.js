#!/usr/bin/env node

// Updates `signature` in top50_by_count.json to remove return type and modifiers,
// keeping parameter types.
// Rules:
// - If `signature` is null, do nothing.
// - If `signature` starts with `return X(...);`, set to `X(...args...)`.
// - Otherwise parse a C#-style declaration:
//     [modifiers] ReturnType Name(Type1 p1, Type2 p2, ...)
//   and emit: `Name(Type1 p1, Type2 p2, ...)`.
// - Whitespace is normalized; output JSON is pretty-printed with 2 spaces.

const fs = require('fs');
const path = require('path');

const ROOT = path.resolve(__dirname, '..');
const INPUT = path.join(ROOT, 'top50_by_count.json');

/** Extract cleaned signature from a `return X(...);` wrapper. */
function fromReturnWrapper(signature) {
  const m = signature.match(/^\s*return\s+([A-Za-z_][A-Za-z0-9_]*)\s*\((.*)\)\s*;\s*$/);
  if (!m) return null;
  const name = m[1];
  const args = m[2].trim();
  return args ? `${name}(${args})` : `${name}()`;
}

/** Parse a C#-style declaration and return cleaned signature. */
function fromDeclaration(signature) {
  // Remove trailing semicolon if present
  let s = signature.trim().replace(/;\s*$/, '');

  // Match: [modifiers] ReturnType Name(params)
  const fnMatch = s.match(/^(?:[A-Za-z0-9_<>\[\]\s]+\s+)*([A-Za-z_][A-Za-z0-9_]*)\s*\((.*)\)$/);
  if (!fnMatch) return null;

  const name = fnMatch[1];
  const paramsRaw = fnMatch[2].trim();
  if (!paramsRaw) return `${name}()`;

  const params = splitParams(paramsRaw)
    .map(p => p.trim())
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

function cleanSignature(signature) {
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
    const cleaned = cleanSignature(fn.signature);
    if (cleaned && cleaned !== fn.signature) {
      fn.signature = cleaned;
      updated++;
    }
  }

  fs.writeFileSync(INPUT, JSON.stringify(data, null, 2) + '\n', 'utf8');
  console.log(`Updated signature for ${updated} functions.`);
}

if (require.main === module) {
  main();
}