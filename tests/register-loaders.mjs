// Registers ts-node ESM and the vscode mock loader. Used by `npm run test:unit`
// via `node --import ./tests/register-loaders.mjs`. Pulled out into its own
// file because passing the equivalent data: URI on the command line broke
// when the script was made cross-platform-friendly via cross-env.
import { register } from "node:module";
import { pathToFileURL } from "node:url";

register("ts-node/esm", pathToFileURL("./"));
register("./tests/vscode-loader.mjs", pathToFileURL("./"));
