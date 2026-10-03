// check.mjs -- press the buttons. Drives the real app in a real browser and FAILS
// (exit code 1) when the Convert screen does not do what it says it does.
//
//     cd tools/ui && npm install           # once, on a machine with internet
//     node check.mjs                       # starts the app itself, checks, stops it
//
// WHY THIS EXISTS. The R suite reads app.R as text: it can prove a line is there,
// never that the screen works. Every change to the Convert table was proven by a
// browser drive like this one -- and those drives lived and died in one session.
// This keeps them. DEV-TIME ONLY: tools/ is not in the offline bundle
// (scripts/bundle-offline.R copies an explicit list), and nothing here runs on the
// server, which has no Node.
//
// Options (environment):
//   PORT=7911            the port to start the app on (default 7911)
//   APP_URL=http://...   check an app that is ALREADY running; nothing is started
//   CHROMIUM_PATH=...    a Chromium to use instead of Playwright's own
//   OUT=dir              where screenshots go (default tools/ui/out)
import { chromium } from 'playwright';
import { spawn, execFileSync } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const HERE = path.dirname(fileURLToPath(import.meta.url));
const ROOT = path.resolve(HERE, '..', '..');
const PORT = Number(process.env.PORT || 7911);
const URL_ = process.env.APP_URL || `http://127.0.0.1:${PORT}/`;
const OUT = path.resolve(process.env.OUT || path.join(HERE, 'out'));
fs.mkdirSync(OUT, { recursive: true });
const sleep = ms => new Promise(r => setTimeout(r, ms));

// ---- the verdicts --------------------------------------------------------------
const results = [];
function check(name, ok, detail = '') {
  results.push({ name, ok: !!ok, detail });
  console.log(`${ok ? 'PASS' : 'FAIL'}  ${name}${!ok && detail ? '\n        ' + detail : ''}`);
}
const eq = (name, got, want) => check(name, JSON.stringify(got) === JSON.stringify(want),
  `got ${JSON.stringify(got)}, want ${JSON.stringify(want)}`);

// ---- the files it uploads: built here, from the suite's own fixtures -------------
function makeFiles() {
  const d = fs.mkdtempSync(path.join(os.tmpdir(), 'bso-ui-'));
  const fx = path.join(ROOT, 'tests', 'testthat', 'fixtures');
  const cp = (src, dst) => fs.copyFileSync(path.join(fx, src), path.join(d, dst));
  cp('anz_everyday_pdf_sample.pdf', 'anz_march.pdf');
  cp('asb_everyday_pdf_sample.pdf', 'asb_march.pdf');
  cp('westpac_everyday_pdf_sample.pdf', 'westpac_march.pdf');
  cp('anz_creditcard_fx.csv', 'anz_card.csv');
  fs.writeFileSync(path.join(d, 'mystery_export.csv'), 'colA;colB;colC\n1;2;3\n4;5;6\n');
  // a picture of a page with no text layer -- what a scanner produces
  execFileSync('Rscript', ['-e', `grDevices::pdf(${JSON.stringify(path.join(d, 'scanned_letter.pdf'))}); ` +
    'graphics::plot.new(); graphics::rasterImage(matrix(stats::runif(400), 20), 0, 0, 1, 1); invisible(grDevices::dev.off())']);
  return d;
}

// ---- the app: started here unless APP_URL says one is running -------------------
async function startApp() {
  if (process.env.APP_URL) return null;
  const app = spawn('Rscript', ['-e', `shiny::runApp(${JSON.stringify(ROOT)}, port = ${PORT}, launch.browser = FALSE)`],
                    { cwd: ROOT, detached: true, stdio: ['ignore', 'ignore', 'pipe'] });
  let log = ''; app.stderr.on('data', b => { log += b; });
  for (let t = 0; t < 120; t++) {
    try { if ((await fetch(URL_)).ok) return app; } catch { /* not up yet */ }
    if (app.exitCode !== null) throw new Error('the app exited while starting:\n' + log.slice(-2000));
    await sleep(1000);
  }
  throw new Error('the app did not answer within two minutes:\n' + log.slice(-2000));
}
const stopApp = app => { if (app) try { process.kill(-app.pid); } catch { /* gone */ } };

// ---- page helpers ------------------------------------------------------------------
async function waitFor(page, fn, ms = 60000, arg) {
  for (let t = 0; t < ms; t += 250) { if (await page.evaluate(fn, arg)) return true; await sleep(250); }
  return false;
}
async function waitIdle(page) {
  await sleep(1200);
  await waitFor(page, () => !document.querySelector('.progress-message') &&
    !document.body.classList.contains('ss-run') && !document.querySelector('.plan-running'), 300000);
  await sleep(1500);
}
const rows = page => page.evaluate(() => [...document.querySelectorAll('tr.plan-row')].map(tr => {
  const q = c => ((tr.querySelector(c) || {}).innerText || '').replace(/\s+/g, ' ').trim();
  const sel = tr.querySelector('select');
  return { file: q('.plan-file'), kind: q('.plan-kind'), value: sel ? sel.value : null,
           chip: q('.plan-state'), result: q('.plan-res'), what: q('.plan-what'),
           open: tr.classList.contains('plan-open') };
}));
const button = page => page.$eval('#cv_go', e => e.innerText.trim());
const byFile = (rs, f) => rs.find(r => r.file === f) || {};
async function pick(page, file, value) {
  await page.evaluate(([f, v]) => {
    const tr = [...document.querySelectorAll('tr.plan-row')].find(t => t.querySelector('.plan-file').innerText === f);
    const s = tr.querySelector('select'); s.value = v; s.dispatchEvent(new Event('change', { bubbles: true }));
  }, [file, value]);
  await sleep(900);
}
async function clickIn(page, file, cell) {
  const h = await page.evaluateHandle(([f, c]) => [...document.querySelectorAll('tr.plan-row')]
    .find(t => t.querySelector('.plan-file').innerText === f).querySelector(c), [file, cell]);
  await h.asElement().click(); await sleep(2500);
}
const shot = (page, name) => page.screenshot({ path: path.join(OUT, name + '.png'), fullPage: true });

// ---- the checks ----------------------------------------------------------------------
async function run(browser, D) {
  const ctx = await browser.newContext({ viewport: { width: 1440, height: 900 }, acceptDownloads: true });
  const page = await ctx.newPage();
  const errs = [];
  page.on('pageerror', e => errs.push(String(e).slice(0, 200)));
  await page.goto(URL_, { waitUntil: 'networkidle' }); await sleep(2000);
  await page.click('a[data-value="Convert"]'); await sleep(800);
  check('the landing page explains itself before any file is chosen',
        (await page.$eval('#cv_empty', e => e.innerText)).includes('Convert a bank statement'));
  const qid = await page.$('#cv_qid'); if (qid) { await qid.fill('UI0001'); await sleep(800); }

  // 1. six files from four banks: a suggestion each
  const six = ['anz_march.pdf', 'asb_march.pdf', 'westpac_march.pdf', 'anz_card.csv',
               'mystery_export.csv', 'scanned_letter.pdf'];
  await page.setInputFiles('#cv_file', six.map(f => path.join(D, f)));
  check('the table appears once the files are checked',
        await waitFor(page, () => document.querySelectorAll('tr.plan-row').length === 6, 60000));
  await sleep(600);
  let r = await rows(page);
  eq('each file gets the template detection will use',
     six.map(f => byFile(r, f).value),
     ['anz_everyday_pdf', 'asb_everyday_pdf', 'westpac_everyday_pdf', 'anz_creditcard_csv', '', '']);
  eq('the chips say how each suggestion was made', six.map(f => byFile(r, f).chip),
     ['Suggested', 'Suggested', 'Suggested', 'Suggested', 'No suggestion - please choose', 'Scanned']);
  check('a scan is said to be one', byFile(r, 'scanned_letter.pdf').kind.startsWith('Scanned PDF'));
  check('the landing text gives way to the table',
        !((await page.$eval('#cv_empty', e => e.innerText)) || '').includes('Convert a bank statement'));
  eq('the button counts the files', await button(page), 'Convert 6 files');

  // 2. two rows changed by hand
  await pick(page, 'westpac_march.pdf', 'asb_everyday_pdf');
  await pick(page, 'mystery_export.csv', 'anz_everyday_csv');
  r = await rows(page);
  eq('a changed row says so', [byFile(r, 'westpac_march.pdf').chip, byFile(r, 'mystery_export.csv').chip],
     ['Your choice', 'Your choice']);
  await shot(page, '1-suggested');

  // 3. convert: the case's progress is IN the table -- no overlay hiding the page --
  //    and the results arrive in the same rows, worst first
  await page.click('#cv_go');
  const seen = { overlay: false, header: new Set(), cells: new Set(), button: new Set(), locked: false };
  for (let t = 0; t < 600; t++) {
    const s = await page.evaluate(() => ({
      overlay: document.body.classList.contains('ss-run'),
      running: !!document.querySelector('.plan-running'),
      header: ((document.querySelector('.plan-running .plan-head') || {}).innerText || '').replace(/ - .*$/, ''),
      cells: [...document.querySelectorAll('td.plan-res')].map(td => td.innerText.split('\n')[0].trim()),
      button: (document.querySelector('#cv_go') || {}).innerText || '',
      locked: [...document.querySelectorAll('select.plan-pick')].some(x => x.disabled) }));
    if (s.overlay) seen.overlay = true;
    if (s.header) seen.header.add(s.header);
    s.cells.forEach(c => seen.cells.add(c)); seen.button.add(s.button);
    if (s.locked) seen.locked = true;
    if (!s.running && t > 4) break;
    await sleep(200);
  }
  await sleep(1500);
  check('a case converts with the page still readable (no overlay)', !seen.overlay);
  check('the table says which file it is on', [...seen.header].some(h => /^(Converting \d+ of 6|Starting)/.test(h)),
        JSON.stringify([...seen.header]));
  check('rows show their own state while the case runs',
        [...seen.cells].some(c => /^(Waiting|Converting)/.test(c)), JSON.stringify([...seen.cells]));
  check('Convert is locked while it runs', seen.button.has('Converting\u2026'), JSON.stringify([...seen.button]));
  check('the dropdowns are locked while it runs', seen.locked);
  check('every row carries its result',
        await waitFor(page, () => document.querySelectorAll('tr.plan-openable').length === 6, 300000));
  r = await rows(page);
  eq('worst first', r.map(x => x.file).slice(0, 3), ['scanned_letter.pdf', 'mystery_export.csv', 'westpac_march.pdf']);
  check('a row read with the template chosen for it says it was',
        byFile(r, 'westpac_march.pdf').result.includes('5 rows'), byFile(r, 'westpac_march.pdf').result);
  check('no second results table', (await page.$$('#cv_batch, #cv_plan .dataTables_wrapper')).length === 0);
  check('Download everything is above the table', !!(await page.$('#cv_batch_dl')));
  eq('the button offers to convert them all again', await button(page), 'Convert all 6 again');
  await shot(page, '2-results');

  // 4. click through -- and a click on a dropdown is not a click on the row
  await clickIn(page, 'anz_march.pdf', '.plan-file');
  r = await rows(page);
  eq('a clicked row opens', r.filter(x => x.open).map(x => x.file), ['anz_march.pdf']);
  check('its result is shown below',
        (await page.$eval('#cv_headline', e => e.innerText)).includes('6 transactions read'));
  await clickIn(page, 'asb_march.pdf', 'select'); await page.keyboard.press('Escape'); await sleep(600);
  eq('a click on a dropdown does not open its row', (await rows(page)).filter(x => x.open).map(x => x.file),
     ['anz_march.pdf']);
  await shot(page, '3-click-through');

  // 5. convert again: only what changed
  const before = Object.fromEntries((await rows(page)).map(x => [x.file, x.result]));
  await pick(page, 'westpac_march.pdf', 'westpac_everyday_pdf');
  r = await rows(page);
  check('a row changed after the run is marked Changed', byFile(r, 'westpac_march.pdf').result.includes('Changed'));
  eq('the button says only the changed file will run', await button(page), 'Convert 1 changed file');
  await page.click('#cv_go'); await waitIdle(page);
  await waitFor(page, () => document.querySelectorAll('tr.plan-openable').length === 6, 300000);
  r = await rows(page);
  check('the other five keep their results',
        r.filter(x => x.file !== 'westpac_march.pdf').every(x => before[x.file] === x.result));
  check('the changed one has its new result', !byFile(r, 'westpac_march.pdf').result.includes('Changed'));

  // 6. download everything
  const [dl] = await Promise.all([page.waitForEvent('download', { timeout: 30000 }), page.click('#cv_batch_dl')]);
  const zp = path.join(OUT, 'case.zip'); await dl.saveAs(zp);
  let entries = -1;
  try { entries = execFileSync('unzip', ['-Z1', zp]).toString().trim().split('\n').length; } catch { /* no unzip */ }
  check('Download everything holds every converted file\'s outputs',
        entries === 15 || (entries === -1 && fs.statSync(zp).size > 10000), `entries ${entries}`);

  // 7. one file
  await page.setInputFiles('#cv_file', [path.join(D, 'asb_march.pdf')]);
  await waitFor(page, () => document.querySelectorAll('tr.plan-row').length === 1, 30000); await sleep(600);
  check('one file is a one-row table',
        (await page.$eval('.plan-head', e => e.innerText)).startsWith("We've suggested a template."));
  check('choosing new files cleared the last case', (await page.$$('tr.plan-openable')).length === 0);
  await page.click('#cv_go'); await waitIdle(page);
  check('its result is below the table',
        (await page.$eval('#cv_headline', e => e.innerText)).includes('5 transactions read'));
  eq('...and the table offers to try another template', await page.$eval('.plan-head', e => e.innerText),
     'Not the template you expected? Choose another and press Convert again.');
  eq('the button says again', await button(page), 'Convert again');
  await pick(page, 'asb_march.pdf', 'anz_everyday_pdf');
  await page.click('#cv_go'); await waitIdle(page);
  check('a wrong template is never a clean result',
        (await page.$eval('#cv_status', e => e.innerText)).includes('read nothing'));
  await shot(page, '4-single');

  // 8. a phone
  await page.setViewportSize({ width: 390, height: 900 }); await sleep(800);
  eq('nothing pushes the page sideways on a phone',
     await page.evaluate(() => document.documentElement.scrollWidth - window.innerWidth), 0);
  eq('no script errors on the page', errs, []);
  await ctx.close();
}

// ---- main -------------------------------------------------------------------------
let app = null, code = 0;
try {
  const D = makeFiles();
  app = await startApp();
  const opts = process.env.CHROMIUM_PATH ? { executablePath: process.env.CHROMIUM_PATH } : {};
  const browser = await chromium.launch(opts);
  try { await run(browser, D); } finally { await browser.close(); }
} catch (e) {
  check('the run finished', false, e.stack || String(e));
} finally {
  stopApp(app);
}
const bad = results.filter(r => !r.ok);
console.log(`\n${results.length - bad.length} of ${results.length} checks passed. Screenshots: ${OUT}`);
if (bad.length) code = 1;
process.exit(code);
