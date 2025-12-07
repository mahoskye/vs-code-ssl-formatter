
import * as fs from 'fs';
import * as path from 'path';
import { SSLFormatter } from '../src/formatting/formatter';

async function run() {
    const args = process.argv.slice(2);
    if (args.length < 1) {
        console.error('Usage: ts-node scripts/verify_formatter_standalone.ts <input_file>');
        process.exit(1);
    }

    const inputPath = path.resolve(process.cwd(), args[0]);
    const outputPath = inputPath.replace(/\.srvscr$|\.ssl$|\.ds$/, '.expected$&');
    if (outputPath === inputPath) {
        // Fallback
        path.join(path.dirname(inputPath), path.basename(inputPath) + '.expected');
    }

    console.log(`Reading from ${inputPath}`);
    const content = fs.readFileSync(inputPath, 'utf8');

    const options = {
        tabSize: 4,
        insertSpaces: false,
        cursor: false // Dummy
    };

    console.log('Formatting...');
    const formatter = new SSLFormatter(options);
    const formatted = formatter.format(content);

    console.log(`Writing formatted content to ${outputPath}`);
    fs.writeFileSync(outputPath, formatted);
    console.log('Done.');

    // Also print first 20 lines to console for quick verify
    const lines = formatted.split('\n').slice(0, 20);
    console.log('--- PREVIEW ---');
    console.log(lines.join('\n'));
    console.log('--- END PREVIEW ---');
}

run().catch(err => console.error(err));
