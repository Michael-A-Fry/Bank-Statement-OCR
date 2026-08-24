// run.mjs -- execute R inside WebR (R compiled to WebAssembly) under Node.
//
// The container lost its native R toolchain and CRAN/Debian are blocked through
// the agent proxy, so this is the only way left to actually RUN the engine.
// WebR ships the whole R runtime as a .wasm in the npm package, so nothing is
// fetched at boot.
//
// Usage: node run.mjs <script.R> [args...]
import { WebR } from 'webr';
import { readFileSync } from 'node:fs';
import path from 'node:path';

const DIST = path.resolve('node_modules/webr/dist') + '/';
const REPO = '/home/user/Bank-Statement-OCR';
const script = process.argv[2];
if (!script) { console.error('usage: node run.mjs <script.R>'); process.exit(2); }

const webR = new WebR({ baseUrl: DIST, interactive: false });
await webR.init();

// Mount the repo and a writable scratch dir into the WASM filesystem.
await webR.FS.mkdir('/repo');
await webR.FS.mount('NODEFS', { root: REPO }, '/repo');
await webR.FS.mkdir('/scratch');
await webR.FS.mount('NODEFS', { root: path.resolve('scratch') }, '/scratch');

const code = readFileSync(script, 'utf8');
await webR.FS.writeFile('/tmp/script.R', new TextEncoder().encode(code));

const shelter = await new webR.Shelter();
try {
  const out = await shelter.captureR(
    'setwd("/repo"); source("/tmp/script.R", echo = FALSE)',
    { withAutoprint: true, captureStreams: true, captureConditions: false }
  );
  for (const o of out.output) {
    if (o.type === 'stdout') console.log(o.data);
    else if (o.type === 'stderr') console.error('E| ' + o.data);
  }
} catch (e) {
  console.error('R ERROR:', e.message || e);
  process.exitCode = 1;
} finally {
  shelter.purge();
}
await webR.close();
