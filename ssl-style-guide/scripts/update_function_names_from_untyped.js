#!/usr/bin/env node

// Syncs `functions[i].name` casing with the identifier from `untypedSignature`.
//
// Rules:
// - If `untypedSignature` is missing or malformed, leave `name` as-is.
// - Extract the function identifier from the start of `untypedSignature`:
//     Foo(bar, baz) -> Foo
// - If found and different by case from `name`, update `name` to match.
// - Never change other fields.
// - Output JSON is pretty-printed with 2 spaces.

const fs = require('fs');
const path = require('path');

const ROOT = path.resolve(__dirname, '..');
const INPUT = path.join(ROOT, 'top50_by_count.json');

function extractNameFromUntyped(untyped) {
  if (typeof untyped !== 'string') return null;
  const m = untyped.trim().match(/^([A-Za-z_][A-Za-z0-9_]*)\s*\(/);
  return m ? m[1] : null;
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
    if (!fn.untypedSignature) continue;

    const fromUntyped = extractNameFromUntyped(fn.untypedSignature);
    if (!fromUntyped) continue;

    if (typeof fn.name === 'string' && fn.name !== fromUntyped) {
      fn.name = fromUntyped;
      updated++;
    }
  }

  fs.writeFileSync(INPUT, JSON.stringify(data, null, 2) + '\n', 'utf8');
  console.log(`Updated name for ${updated} functions from untypedSignature.`);
}

if (require.main === module) {
  main();
}
