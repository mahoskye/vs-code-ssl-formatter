#!/usr/bin/env node

// Removes `count`, `sourceFile`, and `lineNumber` from each function entry in top50_by_count.json.

const fs = require('fs');
const path = require('path');

const ROOT = path.resolve(__dirname, '..');
const INPUT = path.join(ROOT, 'top50_by_count.json');

function main() {
  const raw = fs.readFileSync(INPUT, 'utf8');
  const data = JSON.parse(raw);

  if (!data || !Array.isArray(data.functions)) {
    console.error('Invalid top50_by_count.json: missing functions array');
    process.exit(1);
  }

  let updated = 0;

  for (const fn of data.functions) {
    if ('count' in fn) {
      delete fn.count;
      updated++;
    }
    if ('sourceFile' in fn) {
      delete fn.sourceFile;
      updated++;
    }
    if ('lineNumber' in fn) {
      delete fn.lineNumber;
      updated++;
    }
  }

  fs.writeFileSync(INPUT, JSON.stringify(data, null, 2) + '\n', 'utf8');
  console.log(`Removed fields from ${updated} entries.`);
}

if (require.main === module) {
  main();
}