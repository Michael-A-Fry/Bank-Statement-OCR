// check.mjs -- press the buttons. Drives the real app in a real browser and FAILS
// (exit code 1) when a screen does not do what it says it does.
//
//     cd tools/ui && npm install           # once, on a machine with internet
//     node check.mjs                       # starts the app itself, checks, stops it
//
// WHY THIS EXISTS. The R suite reads app.R as text: it can prove a line is there,
// never that the screen works. This keeps the browser drives that prove it.
// DEV-TIME ONLY: tools/ is not in the offline bundle (scripts/bundle-offline.R
// copies an explicit list), and nothing here runs on the server, which has no Node.
//
// The tour: the Convert table (banks pre-filled, changed, a new bank named), a case
// converted with its progress in the table, every outcome, click-through, the table
// at desktop, laptop and tablet width, Please check on a spreadsheet (Re-read wrong,
// Undo, Re-read right, Set aside, It's right - accept it) and on a PDF (the page, its ticks, the column
// editor with a box drawn and saved), Download everything, a single file with a bank
// the statement disagrees with, scans, Stop, and every Admin tab -- Needs attention
// (a held fix, two drafts merged, a draft accepted), Recipes (a recipe turned off
// and on, its card: Test, a recognise word added and removed, Save, Undo; a new
// recipe from a statement, twice), Words, and Health (automatic reading: the
// spot-check rate, a spot check answered, the carry-off summary; training with
// another bank's statement in the pile) -- each at desktop and phone width. Last, the app's own
// console: an R error or warning there fails the run.
//
// Options (environment):
//   PORT=7911            the port to start the app on (default 7911)
//   APP_URL=http://...   check an app that is ALREADY running; nothing is started,
//                        and nothing that writes to an install's data is pressed
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
const LIVE = !!process.env.APP_URL;
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
  // A BNZ export whose preamble names the holder's account, so the bank is filled
  // in from the statement; the account number is made by the check-digit rule from
  // a register branch (tests/testthat/helper-statements.R), never anyone's real one.
  const acct = execFileSync('Rscript', ['-e',
    'suppressMessages({for (f in list.files("R", "[.]R$", full.names = TRUE)) source(f)}); ' +
    'source("tests/testthat/helper-statements.R"); cat(nz_test_account())'], { cwd: ROOT }).toString().trim();
  const w = (name, lines) => fs.writeFileSync(path.join(d, name), lines.join('\n') + '\n');
  w('bnz_export.csv', [`BNZ - Transactions - ${acct}`, 'Period 13/04/2025 to 30/04/2025',
    'Date,Description,Debit,Credit,Balance', '13/04/2025,Opening balance,,,1000.00',
    '14/04/2025,Salary,,2500.00,3500.00', '15/04/2025,Rent,1200.00,,2300.00',
    '16/04/2025,Account fee,0.00,,2300.00', '17/04/2025,Coffee,4.50,,2295.50',
    '18/04/2025,Groceries,85.20,,2210.30']);
  // Adds up two ways round, and nothing on it says which: a person decides.
  w('ambiguous.csv', ['Date,Narrative,Col A,Col B,Col C', '13/04/2025,Opening balance,,,1000.00',
    '14/04/2025,Item one,,2500.00,3500.00', '15/04/2025,Item two,1200.00,,2300.00',
    '17/04/2025,Item three,4.50,,2295.50', '18/04/2025,Item four,85.20,,2210.30']);
  // No balance and no totals: nothing on it can prove which column is which.
  w('unproven.csv', ['Date,Details,Amount', '14/04/2025,Salary,2500.00', '15/04/2025,Rent,-1200.00',
    '17/04/2025,Coffee,-4.50']);
  fs.writeFileSync(path.join(d, 'mystery_export.csv'), 'colA;colB;colC\n1;2;3\n4;5;6\n');
  // a picture of a page with no text layer -- what a scanner produces
  execFileSync('Rscript', ['-e', `grDevices::pdf(${JSON.stringify(path.join(d, 'scanned_letter.pdf'))}); ` +
    'graphics::plot.new(); graphics::rasterImage(matrix(stats::runif(400), 20), 0, 0, 1, 1); invisible(grDevices::dev.off())']);
  // ...and a real statement as a scanner would hand it over: the ANZ sample, rendered
  // to a picture of each page with no text layer left
  execFileSync('Rscript', ['-e', `im <- magick::image_read_pdf(${JSON.stringify(path.join(fx, 'anz_everyday_pdf_sample.pdf'))}, density = 200); ` +
    `magick::image_write(magick::image_convert(im, colorspace = 'gray'), ${JSON.stringify(path.join(d, 'anz_scan.pdf'))}, format = 'pdf')`]);
  return d;
}

// ---- the app: started here unless APP_URL says one is running -------------------
// Started with a THROWAWAY config (BSO_CONFIG): its logs, uploads, feed, learned
// layouts and tracking all go to a temporary folder, so a check never writes into
// a real install's data and every run starts from nothing learned.
const ADMIN_PW = 'ui-check-' + process.pid;
const ASK = process.env.UNKNOWN_DESIGN === 'ask';
let appLog = '';           // everything the app wrote to its console, checked at the end
let WORDS_DIR = '';        // the throwaway copies of the two words files
function makeConfig() {
  const d = fs.mkdtempSync(path.join(os.tmpdir(), 'bso-ui-cfg-'));
  const p = s => JSON.stringify(path.join(d, s));
  // the words files too: a word taught by the check lands in a copy, never in the repo
  WORDS_DIR = d;
  for (const f of ['labels.yaml', 'lexicon.yaml'])
    fs.copyFileSync(path.join(ROOT, 'dictionaries', f), path.join(d, f));
  fs.writeFileSync(path.join(d, 'config.yaml'), [
    'paths:', `  logs: ${p('logs')}`, `  uploads: ${p('uploads')}`, `  requests: ${p('requests')}`,
    `  layouts: ${p('layouts')}`, `  tracking: ${p('tracking')}`,
    `  dictionary: ${p('labels.yaml')}`, `  lexicon: ${p('lexicon.yaml')}`,
    'feed:', `  feed_dir: ${p('feed')}`,
    // the tour below checks the READER's outcomes, so a new design converts on its
    // arithmetic; UNKNOWN_DESIGN=ask tours the product's "always ask once" instead
    'auto_reading:', `  unknown_design: ${process.env.UNKNOWN_DESIGN || 'auto'}`, ''].join('\n'));
  return path.join(d, 'config.yaml');
}
async function startApp() {
  if (LIVE) return null;
  const env = { ...process.env, BSO_CONFIG: makeConfig(), BSO_ADMIN_PASSWORD: ADMIN_PW };
  const app = spawn('Rscript', ['-e', `shiny::runApp(${JSON.stringify(ROOT)}, port = ${PORT}, launch.browser = FALSE)`],
                    { cwd: ROOT, detached: true, stdio: ['ignore', 'ignore', 'pipe'], env });
  app.stderr.on('data', b => { appLog += b; });
  for (let t = 0; t < 120; t++) {
    try { if ((await fetch(URL_)).ok) return app; } catch { /* not up yet */ }
    if (app.exitCode !== null) throw new Error('the app exited while starting:\n' + appLog.slice(-2000));
    await sleep(1000);
  }
  throw new Error('the app did not answer within two minutes:\n' + appLog.slice(-2000));
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
           chip: q('td.plan-tpl .plan-chip') || q('td.plan-tpl .plan-note'), layout: q('.plan-layout'),
           result: q('.plan-res'), open: tr.classList.contains('plan-open') };
}));
const button = page => page.$eval('#cv_go', e => e.innerText.trim());
const byFile = (rs, f) => rs.find(r => r.file === f) || {};
const text = (page, sel) => page.$eval(sel, e => e.innerText).catch(() => '');
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
// a selectInput is a selectize control: set it the way a person picking does
async function selectize(page, id, value) {
  await page.evaluate(([i, v]) => { const el = document.getElementById(i); el.selectize.setValue(v); }, [id, value]);
  await sleep(700);
}
// press Convert once it can be pressed; if it never can, say what the page says why
async function go(page) {
  const ok = await waitFor(page, () => { const b = document.querySelector('#cv_go'); return b && !b.classList.contains('disabled'); }, 60000);
  if (!ok) throw new Error('Convert never became pressable: ' + await text(page, '#cv_go_btn'));
  await page.click('#cv_go');
}
const shot = (page, name) => page.screenshot({ path: path.join(OUT, name + '.png'), fullPage: true });
// the QID is asked once a session, unless the host has a sign-in: wait for the box
// (or for the line saying who is recorded), fill it, and wait for it to be taken
async function setQid(page) {
  await waitFor(page, () => !!document.querySelector('#cv_qid') || /Recording as/.test(document.body.innerText), 15000);
  const q = await page.$('#cv_qid');
  if (q) { await q.fill('UI0001'); await waitFor(page, () => /Recording as/.test(document.body.innerText), 10000); }
}
async function freshConvert(ctx) {
  const p = await ctx.newPage();
  await p.goto(URL_, { waitUntil: 'networkidle' }); await sleep(1500);
  await p.click('a[data-value="Convert"]'); await sleep(600);
  await setQid(p);
  return p;
}
// wait for the result of a re-read on Please check: a new message under its buttons
async function rereadDone(page, before) {
  await waitIdle(page);
  await waitFor(page, b => { const m = document.querySelector('#cv_ck_msg'); return m && m.innerText.trim() && m.innerText !== b; },
                120000, before);
  await sleep(800);
  return text(page, '#cv_ck_msg');
}

// ---- the checks ----------------------------------------------------------------------
async function run(browser, D) {
  const ctx = await browser.newContext({ viewport: { width: 1440, height: 900 }, acceptDownloads: true });
  const page = await ctx.newPage();
  const errs = [];
  page.on('pageerror', e => errs.push(String(e).slice(0, 200)));
  await page.goto(URL_, { waitUntil: 'networkidle' }); await sleep(2000);
  await page.click('a[data-value="Convert"]'); await sleep(800);
  check('the landing page explains itself before any file is chosen',
        (await text(page, '#cv_empty')).includes('Convert a bank statement'));
  check('no template is mentioned anywhere on Convert',
        !/template/i.test(await page.evaluate(() => document.querySelector('.tab-pane.active').innerText)));
  await setQid(page);

  // 1. six files: a bank each, filled in from the statement where it says
  const six = ['anz_march.pdf', 'bnz_export.csv', 'ambiguous.csv', 'unproven.csv', 'mystery_export.csv', 'scanned_letter.pdf'];
  await page.setInputFiles('#cv_file', six.map(f => path.join(D, f)));
  check('the table appears once the files are checked',
        await waitFor(page, () => document.querySelectorAll('tr.plan-row').length === 6, 60000));
  await waitFor(page, () => !document.querySelector('td.plan-tpl .plan-chip') ||
    ![...document.querySelectorAll('td.plan-tpl .plan-chip')].some(c => c.innerText.includes('Reading the scan')), 120000);
  await sleep(600);
  let r = await rows(page);
  eq('the bank is filled in from the statement where it says', byFile(r, 'bnz_export.csv').value, 'bnz');
  eq('...and says where it came from', byFile(r, 'bnz_export.csv').chip, 'From the statement');
  eq('a statement that names no bank asks for one', [byFile(r, 'anz_march.pdf').value, byFile(r, 'anz_march.pdf').chip],
     ['', 'Please choose the bank']);
  check('a scan is said to be one', byFile(r, 'scanned_letter.pdf').kind.startsWith('Scanned PDF'));
  check('every bank dropdown offers the NZ banks and a new one',
        await page.evaluate(() => [...document.querySelectorAll('select.plan-pick')].every(s =>
          [...s.options].some(o => o.value === 'anz') && [...s.options].some(o => o.value === '__new__'))));
  check('...and only banks: not the register\'s "a business that banks through ANZ"',
        await page.evaluate(() => [...document.querySelectorAll('select.plan-pick option')].every(o => !/banks through/.test(o.text))));
  check('the landing text gives way to the table', !(await text(page, '#cv_empty')).includes('Convert a bank statement'));
  eq('the button counts the files', await button(page), 'Convert 6 files');

  // 2. banks changed by hand, and a bank the list does not have
  await pick(page, 'anz_march.pdf', 'anz');
  await pick(page, 'unproven.csv', 'kiwibank');
  await pick(page, 'mystery_export.csv', '__new__');
  check('"Another bank" asks for its name', await waitFor(page, () => !!document.querySelector('#cv_new_bank'), 10000));
  await page.fill('#cv_new_bank', '01-0102-0123456-00'); await page.click('#cv_new_bank_ok'); await sleep(900);
  check('...refuses an account number for a name', (await text(page, '#cv_new_bank_msg')).includes('account number'));
  await page.fill('#cv_new_bank', 'Smith Credit Union'); await page.click('#cv_new_bank_ok'); await sleep(1200);
  r = await rows(page);
  eq('a changed row says so', [byFile(r, 'anz_march.pdf').chip, byFile(r, 'unproven.csv').chip], ['your choice', 'your choice']);
  eq('...and the named bank is the row\'s bank', byFile(r, 'mystery_export.csv').value, 'Smith Credit Union');
  await shot(page, '01-convert-banks');

  // 3. convert: the case's progress is IN the table -- no overlay hiding the page --
  //    and the results arrive in the same rows, worst first
  await go(page);
  const seen = { overlay: false, header: new Set(), cells: new Set(), button: new Set(), locked: false };
  for (let t = 0; t < 900; t++) {
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
        [...seen.cells].some(c => /^(Waiting|Converting|Opening|Reading|Checking|Writing)/.test(c)), JSON.stringify([...seen.cells]));
  check('Convert is locked while it runs', seen.button.has('Converting\u2026'), JSON.stringify([...seen.button]));
  check('the bank dropdowns are locked while it runs', seen.locked);
  check('every row carries its result',
        await waitFor(page, () => document.querySelectorAll('tr.plan-openable').length === 6, 300000));
  r = await rows(page);
  const word = f => byFile(r, f).result.split(':')[0].replace(/ \d+ rows?$/, '').trim();
  // D16: one word each. With "always ask once", a new design that adds up waits for a person.
  eq('each file has its outcome in plain words', six.map(word),
     [ 'Done', ASK ? 'Needs you' : 'Done', 'Needs you', 'Needs you', "Couldn't read", "Couldn't read"]);
  check('...in one word and a few words of reason', r.every(x => { const w = x.result.split('\n')[0].split(':');
          return w.length < 2 || w.slice(1).join(':').replace(/(\d[\d,]* rows?)?\s*(Please check.*)?$/, '').trim().split(/\s+/).length <= 8; }),
        JSON.stringify(r.map(x => x.result)));
  check('a reason is given where a person has something to do',
        byFile(r, 'ambiguous.csv').result.includes('reading of the columns') && byFile(r, 'unproven.csv').result.length > 20,
        JSON.stringify([byFile(r, 'ambiguous.csv').result, byFile(r, 'unproven.csv').result]));
  check('the design a recipe read is named', /ANZ .*Account/.test(byFile(r, 'anz_march.pdf').layout),
        byFile(r, 'anz_march.pdf').layout);
  check('worst first', r.slice(0, 2).every(x => word(x.file) === "Couldn't read"), JSON.stringify(r.map(x => x.file)));
  check('no second results table', (await page.$$('#cv_batch, #cv_plan .dataTables_wrapper')).length === 0);
  check('Download everything is above the table', !!(await page.$('#cv_batch_dl')));
  eq('the button offers to convert them all again', await button(page), 'Convert all 6 again');
  check('a row that needs a person offers Please check',
        await page.evaluate(() => ['ambiguous.csv', 'unproven.csv'].every(f => [...document.querySelectorAll('tr.plan-row')]
          .some(t => t.querySelector('.plan-file').innerText === f && t.querySelector('a.plan-check')))));
  check('...and never one that proved itself', await page.evaluate(() => [...document.querySelectorAll('tr.plan-row')]
          .filter(t => /^Done/.test(t.querySelector('.plan-res').innerText)).every(t => !t.querySelector('a.plan-check'))));
  await shot(page, '02-convert-results');
  //    a converted case fits its panel at every width: a table at desktop, a card
  //    per file below that -- never an outcome cut off behind a sideways scroll
  for (const w of [1440, 1280, 1024, 800]) {
    await page.setViewportSize({ width: w, height: 900 }); await sleep(700);
    eq(`the converted case needs no sideways scroll at ${w}px`,
       await page.evaluate(() => { const s = document.querySelector('.plan-scroll'); return s.scrollWidth - s.clientWidth; }), 0);
  }
  await shot(page, '02b-convert-results-tablet');
  await page.setViewportSize({ width: 1440, height: 900 }); await sleep(700);

  // 4. click through -- and a click on a dropdown is not a click on the row
  await clickIn(page, 'anz_march.pdf', '.plan-file');
  r = await rows(page);
  eq('a clicked row opens', r.filter(x => x.open).map(x => x.file), ['anz_march.pdf']);
  check('its result is shown below',
        (await text(page, '#cv_headline')).includes('Done \u2014 6 transactions, the balance adds up.'), await text(page, '#cv_headline'));
  await clickIn(page, 'bnz_export.csv', 'select'); await page.keyboard.press('Escape'); await sleep(600);
  eq('a click on a dropdown does not open its row', (await rows(page)).filter(x => x.open).map(x => x.file),
     ['anz_march.pdf']);

  //    ask mode: a new design that adds up says so, and asks with two buttons only
  if (ASK) {
    await clickIn(page, 'bnz_export.csv', '.plan-file'); await sleep(2500);
    check('a new design says so', /New design/.test(await text(page, '#cv_status')), await text(page, '#cv_status'));
    eq('...and asks with two buttons', await page.evaluate(() =>
       [...document.querySelectorAll('#cv_check button')].map(b => b.innerText.trim())),
       ['It\u2019s right \u2014 accept it', 'Set aside']);
    check('...and offers no download yet', !(await page.$('#dl_xlsx')));
    await clickIn(page, 'anz_march.pdf', '.plan-file'); await sleep(2500);
  }
  //    the table is the QVF's, with its Check column: a tick where the balance follows
  check('the transactions table has a Check column', await waitFor(page, () =>
        [...document.querySelectorAll('#cv_txns th')].some(th => th.textContent.trim() === 'Check'), 20000));
  check('...ticked where the balance follows', await page.evaluate(() =>
        [...document.querySelectorAll('#cv_txns tbody td')].some(td => td.innerText.trim() === '\u2713')));
  check('a finished file offers its downloads', !!(await page.$('#dl_xlsx')));

  // 5. PLEASE CHECK, on a PDF: a row of the table clicked opens its page, line marked
  await page.click('#cv_txns tbody tr:nth-child(2) td:nth-child(2)'); await sleep(3000);
  check('a clicked transaction opens the page it is on', await waitFor(page, () => !!document.querySelector('#cv_ck_plot img'), 20000));
  check('...at that page', (await page.evaluate(() => (document.querySelector('input[name="cv_ck_page"]:checked') || {}).value)) === '1');
  check('each page carries its balance tick', (await text(page, '#cv_ck_pages')).includes('Page 1 \u2713'),
        await text(page, '#cv_ck_pages'));
  check('...and says it in words', (await text(page, '#cv_ck_tick_line')).includes('the balance adds up'));
  check('one question per column of figures', (await page.$$('.ck-ask')).length === 3);
  check('...each shown by its own lines', (await text(page, '.ck-ask')).includes('Lines from this column'), await text(page, '.ck-ask'));
  check('someone not signed in as admin is offered no word teaching', !(await page.$('#cv_ck_teach')));
  await shot(page, '03-please-check-pdf');
  //    the last resort: drawing the columns by hand
  await page.click('#cv_ck_side details > summary'); await sleep(500);
  check('the last resort is behind More detail', (await text(page, '#cv_ck_side details > summary')) === 'More detail');
  await page.click('#cv_ck_editor');
  check('the column editor opens on the page', await waitFor(page, () => !!document.querySelector('#ed_plot img'), 30000));
  await sleep(1000);
  await shot(page, '04-column-editor');
  const box = await (await page.$('#ed_plot img')).boundingBox();
  await selectize(page, 'ed_field', 'debit');
  const X = pt => box.x + box.width * pt / 595;
  const Y = Math.min(box.y + 250, 850);    // the mouse works in the window, and the page is taller
  await page.mouse.move(X(287), Y);
  await page.mouse.down();
  for (let k = 1; k <= 12; k++) { await page.mouse.move(X(287 + k * 9.6), Y + k); await sleep(40); }
  await page.mouse.up(); await sleep(2200);
  await page.click('#ed_set'); await sleep(1200);
  check('a drawn box is set as the column', (await text(page, '#ed_msg')).includes('Money going out set on page 1'),
        await text(page, '#ed_msg'));
  await page.click('#ed_save');
  const boxed = await rereadDone(page, '');
  check('the drawn columns are read again and still have to prove themselves',
        boxed.startsWith('Done - with the columns you drew'), boxed);
  check('...and say they apply to this file only', boxed.includes('this file only'));

  // 6. PLEASE CHECK, on a spreadsheet: Re-read wrong, Re-read right
  await page.click('tr.plan-row:has(td.plan-file:text-is("ambiguous.csv")) a.plan-check'); await sleep(3000);
  check('Please check opens from the row', (await text(page, '#cv_check')).startsWith('Please check'));
  check('...with the reason', /readings? of the columns/.test((await text(page, '#cv_status')) + (await text(page, '#cv_check'))),
        (await text(page, '#cv_status')).slice(0, 300));
  check('a spreadsheet shows its columns by heading', (await text(page, '#cv_ck_table')).includes('Col A'));
  await sleep(1500);
  check('nothing on the result page, its checks and its field coverage included, says "template"',
        !/template/i.test(await page.evaluate(() => document.querySelector('.tab-pane.active').innerText)));
  check('...and no flag or message shows an engine code', !/amount_from_balance|sum\(amount\)|_[a-z]+_/.test(
        await page.evaluate(() => document.querySelector('#cv_status').innerText + document.querySelector('#cv_check').innerText)));
  check('an untouched reading offers no Undo', !(await page.$('#cv_ck_undo')));
  await shot(page, '05-please-check-csv');
  await page.check('input[name="cv_ck_role_debit"][value="credit"]'); await page.check('input[name="cv_ck_role_credit"][value="debit"]'); await sleep(700);
  await page.click('#cv_ck_reread');
  const wrong = await rereadDone(page, '');
  check('a wrong reading is said not to prove, at once', wrong.startsWith('Still not proven'), wrong);
  //    the way back: Undo reads it again as it was first found -- still there after
  //    another file was opened and this one opened again
  await clickIn(page, 'bnz_export.csv', '.plan-file');
  await clickIn(page, 'ambiguous.csv', '.plan-file');
  check('a reading made with a person\'s change offers Undo, even after leaving it', !!(await page.$('#cv_ck_undo')));
  await page.click('#cv_ck_undo');
  const undone = await rereadDone(page, '');
  check('Undo reads it as the tool first found it', undone.startsWith('Your changes are undone'), undone);
  check('...and then offers no Undo', !(await page.$('#cv_ck_undo')));
  r = await rows(page);
  eq('...and the row is back to Needs you', word('ambiguous.csv'), 'Needs you');
  await page.check('input[name="cv_ck_role_debit"][value="debit"]'); await page.check('input[name="cv_ck_role_credit"][value="credit"]'); await sleep(700);
  await page.click('#cv_ck_reread');
  const right = await rereadDone(page, undone);
  check('the right roles prove it, and it says so', right.startsWith('Done'), right);
  r = await rows(page);
  eq('...and the row is updated in place', word('ambiguous.csv'), 'Done');
  await shot(page, '06-reread-proven');
  //    It's right - accept it, on a statement nothing on it can prove; Set aside first
  await page.click('tr.plan-row:has(td.plan-file:text-is("unproven.csv")) a.plan-check'); await sleep(3000);
  check('no downloads before it is done', !(await page.$('#dl_xlsx')) && !(await page.$('#dl_csv')));
  eq('at most three buttons, in plain words', await page.evaluate(() =>
     [...document.querySelectorAll('#cv_check button')].map(b => b.innerText.trim())),
     ['Read it again', 'It\u2019s right \u2014 accept it', 'Set aside']);
  await page.click('#cv_ck_aside');
  const aside = await rereadDone(page, '');
  check('Set aside says it is not converted and goes to an admin', aside.startsWith('Set aside') && aside.includes('admin'), aside);
  await page.click('#cv_ck_confirm');
  const conf = await rereadDone(page, aside);
  check('"It\u2019s right \u2014 accept it" converts it as read and holds it for an admin', conf.startsWith('Confirmed') && conf.includes('admin'), conf);
  r = await rows(page);
  eq('...and the row says who decided', word('unproven.csv'), 'Done - you checked it');
  check('...and its downloads appear once accepted', await waitFor(page, () => !!document.querySelector('#dl_xlsx'), 20000));
  check('a reading a person vouched for does not wear the proven green',
        await page.evaluate(() => { const v = document.querySelector('#cv_headline .verdict');
          return !!v && v.classList.contains('verdict-medium') && /you checked it/.test(v.innerText); }),
        await text(page, '#cv_headline'));

  // 7. download everything
  const [dl] = await Promise.all([page.waitForEvent('download', { timeout: 30000 }), page.click('#cv_batch_dl')]);
  const zp = path.join(OUT, 'case.zip'); await dl.saveAs(zp);
  let entries = -1;
  try { entries = execFileSync('unzip', ['-Z1', zp]).toString().trim().split('\n').length; } catch { /* no unzip */ }
  check('Download everything holds every converted file\'s outputs',
        entries === 12 || (entries === -1 && fs.statSync(zp).size > 10000), `entries ${entries}`);

  // 8. one file, read as a bank the statement disagrees with
  await page.setInputFiles('#cv_file', [path.join(D, 'bnz_export.csv')]);
  await waitFor(page, () => document.querySelectorAll('tr.plan-row').length === 1, 30000); await sleep(600);
  check('one file is a one-row table', (await text(page, '.plan-head')).startsWith('Check the bank'));
  check('choosing new files cleared the last case', (await page.$$('tr.plan-openable')).length === 0);
  await pick(page, 'bnz_export.csv', 'anz');
  await go(page); await waitIdle(page);
  check('its result is below the table', /5 transactions|New design/.test((await text(page, '#cv_headline')) + (await text(page, '#cv_status'))),
        (await text(page, '#cv_headline')) + (await text(page, '#cv_status')));
  check('a statement that names another bank asks which is right', (await text(page, '#cv_bank_note')).includes('Which bank?'),
        await text(page, '#cv_bank_note'));
  check('...in a plain sentence, with no grade and the bank named once',
        /looks like BNZ: /.test(await text(page, '#cv_bank_note')) && !/confidence\)|BNZ: BNZ/.test(await text(page, '#cv_bank_note')),
        await text(page, '#cv_bank_note'));
  check('...and the row says it too', (await rows(page))[0].chip === 'Which bank? The statement looks like BNZ',
        (await rows(page))[0].chip);
  eq('the one row carries its outcome too', word('bnz_export.csv'), ASK ? 'Needs you' : 'Done');
  eq('the button says again', await button(page), 'Convert again');
  await shot(page, '07-bank-question');
  await page.click('#cv_bank_use'); await waitIdle(page); await sleep(1500);
  check('...and answering it reads it again as that bank', !(await text(page, '#cv_bank_note')).includes('Which bank?'));
  eq('...with the table showing the same bank', (await rows(page))[0].value, 'bnz');

  // 9. SCANS: the first pages are read in the background to find the bank
  {
    const ps = await freshConvert(ctx);
    await ps.setInputFiles('#cv_file', [path.join(D, 'anz_scan.pdf'), path.join(D, 'scanned_letter.pdf')]);
    await waitFor(ps, () => document.querySelectorAll('tr.plan-row').length === 2, 60000);
    check('a scan says it is being read', await waitFor(ps, () => [...document.querySelectorAll('td.plan-tpl .plan-chip')]
      .some(c => c.innerText.includes('Reading the scan')), 8000));
    await waitFor(ps, () => ![...document.querySelectorAll('td.plan-tpl .plan-chip')].some(c => c.innerText.includes('Reading the scan')), 180000);
    const sr = await rows(ps);
    check('a scan whose pages were read asks for its bank like any other file',
          byFile(sr, 'anz_scan.pdf').chip === 'Please choose the bank', byFile(sr, 'anz_scan.pdf').chip);
    await pick(ps, 'anz_scan.pdf', 'anz');
    await go(ps); await waitIdle(ps);
    await waitFor(ps, () => document.querySelectorAll('tr.plan-openable').length === 2, 300000);
    const s2 = await rows(ps);
    check('...and is converted', byFile(s2, 'anz_scan.pdf').result.length > 0, byFile(s2, 'anz_scan.pdf').result);
    console.log(`        (the scan: ${byFile(s2, 'anz_scan.pdf').result})`);
    await shot(ps, '08-scans');
    await ps.close();
  }

  // 10. STOP. A long case can be stopped; a first run stopped leaves the table as it
  //     was before Convert, with nothing kept.
  {
    for (const n of [1, 2, 3]) fs.copyFileSync(path.join(D, 'anz_scan.pdf'), path.join(D, `scan_${n}.pdf`));
    const pz = await freshConvert(ctx);
    await pz.setInputFiles('#cv_file', [1, 2, 3].map(n => path.join(D, `scan_${n}.pdf`)));
    await waitFor(pz, () => document.querySelectorAll('tr.plan-row').length === 3, 60000); await sleep(500);
    await go(pz);
    check('a running case offers Stop', await waitFor(pz, () => !!document.querySelector('#cv_stop'), 30000));
    await sleep(3000);
    await pz.click('#cv_stop'); await sleep(2500);
    check('Stop ends the run', !(await pz.$('.plan-running')) && !(await pz.$('#cv_stop')));
    check('...and leaves the table as it was before Convert', (await pz.$$('td.plan-res')).length === 0);
    eq('...ready to start again', await button(pz), 'Convert 3 files');
    await pz.close();
  }

  // 11. EVERY OTHER SCREEN: About, and each Admin tab -- at desktop and phone width.
  //     Nothing may draw an error where its content should be, push the page
  //     sideways, or put an error in the console.
  const tp = await ctx.newPage();
  const terr = [];
  tp.on('pageerror', e => terr.push('PAGEERROR ' + String(e).slice(0, 160)));
  tp.on('console', m => { if (m.type() === 'error') terr.push('CONSOLE ' + m.text().slice(0, 160)); });
  await tp.goto(URL_ + '?admin', { waitUntil: 'networkidle' }); await sleep(1500);
  const screen = async (pg, name) => {
    await sleep(1500);
    const drawnErr = await pg.evaluate(() => [...document.querySelectorAll('.shiny-output-error')]
      .filter(e => e.offsetParent !== null && !e.classList.contains('shiny-output-error-validation')).map(e => e.innerText.slice(0, 120)));
    check(`${name}: nothing draws an error`, drawnErr.length === 0, JSON.stringify(drawnErr));
    await pg.setViewportSize({ width: 390, height: 900 }); await sleep(700);
    const o = await pg.evaluate(() => document.documentElement.scrollWidth - window.innerWidth);
    check(`${name}: fits a phone`, o === 0, `overflow ${o}px`);
    await shot(pg, 'tour-' + name.toLowerCase().replace(/[^a-z]+/g, '-') + '-phone');
    await pg.setViewportSize({ width: 1440, height: 900 }); await sleep(500);
    await shot(pg, 'tour-' + name.toLowerCase().replace(/[^a-z]+/g, '-'));
  };
  await tp.click('a[data-value="About"]');
  check('About describes reading by bank, not templates',
        !/template/i.test(await tp.evaluate(() => document.querySelector('.tab-pane.active').innerText)));
  await screen(tp, 'About');
  await tp.click('a[data-value="Admin"]'); await sleep(800);
  await tp.fill('#adm_pw', ADMIN_PW); await tp.click('#adm_login'); await sleep(2500);
  const admTabs = await tp.$$eval('#adm_tabs > li > a', as => as.map(a => a.innerText.trim()));
  eq('Admin has exactly four tabs', admTabs, ['Needs attention', 'Recipes', 'Words', 'Health']);
  check('Admin opens on Needs attention, with its cards and counts',
        await waitFor(tp, () => /Statements waiting for a look/.test((document.querySelector('#adm_na_cards') || {}).innerText || ''), 20000) &&
        (await tp.$$('#adm_na_cards .na-count')).length === 4, await text(tp, '#adm_na_cards'));
  if (!LIVE) {
    // the fix a person confirmed, held for an admin
    check('a confirmed reading waits for an admin', (await text(tp, '#adm_fixes')).includes('Kiwibank'), await text(tp, '#adm_fixes'));
    await tp.click('#adm_fixes tbody tr'); await sleep(500);
    await tp.click('#adm_fix_accept'); await sleep(2000);
    check('...and Accept makes it a proven layout', (await text(tp, '#adm_fix_msg')).startsWith('Accepted'), await text(tp, '#adm_fix_msg'));
  }
  await screen(tp, 'Admin Needs attention');
  // RECIPES: one row per recipe, a toggle, a card
  await tp.click('a[data-value="Recipes"]'); await sleep(1500);
  const R = 'anz_everyday_pdf';
  const row = `#adm_rc_list tr[data-recipe="${R}"]`;
  check('Recipes lists one row per recipe, with its bank', await waitFor(tp, s => !!document.querySelector(s), 20000, row) &&
        (await text(tp, row)).includes('ANZ'), await text(tp, '#adm_rc_list'));
  check('...and never shows YAML', !/recipe:|status:|yaml/i.test(await text(tp, '#adm_rc_list')));
  if (!LIVE) {
    await tp.click(`${row} .rc-toggle`);
    check('a recipe is turned off with one click',
          await waitFor(tp, s => /OFF/.test((document.querySelector(s + ' .rc-toggle') || {}).innerText || ''), 15000, row), await text(tp, row));
    await tp.click(`${row} .rc-toggle`);
    check('...and on again', await waitFor(tp, s => /ON/.test((document.querySelector(s + ' .rc-toggle') || {}).innerText || ''), 15000, row),
          await text(tp, row));
  }
  await tp.click(`${row} a.rc-open`);
  check('a click opens the recipe card', await waitFor(tp, () => !!document.getElementById('adm_rc_title'), 15000));
  check('...asking the plain column questions with this recipe\'s answers',
        await tp.$eval('#adm_rc_role_3', e => e.value).catch(() => '') === 'money out' &&
        /Recognised by these words/.test(await text(tp, '#adm_rc_card')), await text(tp, '#adm_rc_card'));
  check('...and no YAML anywhere on it', !/recipe:|status:|under:|yaml/i.test(await text(tp, '#adm_rc_card')));
  await tp.setInputFiles('#adm_rc_test_file', [path.join(D, 'anz_march.pdf')]); await sleep(2500);
  await tp.click('#adm_rc_test');
  check('Test reads a statement with the recipe and says so in one sentence',
        await waitFor(tp, () => /^It adds up/.test((document.querySelector('#adm_rc_test_msg') || {}).innerText || ''), 60000),
        await text(tp, '#adm_rc_test_msg'));
  check('...and draws its page with the columns numbered', await waitFor(tp, () => !!document.querySelector('#adm_rc_plot img'), 20000));
  if (!LIVE) {
    // a recognise-by word: added, removed, added again, saved, undone
    await tp.fill('#adm_rc_word_new', 'Statement period'); await tp.click('#adm_rc_word_add'); await sleep(1200);
    const chips = async () => tp.$$eval('#adm_rc_card .rc-chip', cs => cs.map(c => c.firstChild.textContent.trim()));
    check('a recognise word is added as a chip', (await chips()).includes('Statement period'), JSON.stringify(await chips()));
    await tp.click('#adm_rc_card .rc-chip:has-text("Statement period") .rc-chip-x'); await sleep(1200);
    check('...and removed with its cross', !(await chips()).includes('Statement period'), JSON.stringify(await chips()));
    await tp.fill('#adm_rc_word_new', 'Statement period'); await tp.click('#adm_rc_word_add'); await sleep(1200);
    await tp.click('#adm_rc_test');
    check('Test with the change, before saving: it still adds up',
          await waitFor(tp, () => /^It adds up/.test((document.querySelector('#adm_rc_test_msg') || {}).innerText || ''), 60000),
          await text(tp, '#adm_rc_test_msg'));
    await tp.click('#adm_rc_save');
    check('Save makes a new version, the old one kept',
          await waitFor(tp, () => /^Saved as a new version/.test((document.querySelector('#adm_rc_msg') || {}).innerText || ''), 15000) &&
          /Version 4\s+changed/.test(await text(tp, '#adm_rc_card')), await text(tp, '#adm_rc_card'));
    await tp.click('#adm_rc_undo');
    check('Undo brings the last version back',
          await waitFor(tp, () => /is back to how it was in version 3/.test((document.querySelector('#adm_rc_msg') || {}).innerText || ''), 15000) &&
          !(await chips()).includes('Statement period'), await text(tp, '#adm_rc_msg'));
    // a new recipe from a statement, twice: two drafts of one design
    for (const nm of ['Everyday', 'Everyday (again)']) {
      if (!(await tp.$('#adm_rc_new_file'))) { await tp.click('#adm_rc_new_open'); await sleep(1500); }
      await selectize(tp, 'adm_rc_new_bank', 'anz');
      await tp.fill('#adm_rc_new_name', nm);
      await tp.setInputFiles('#adm_rc_new_file', [path.join(D, 'anz_march.pdf')]); await sleep(2500);
      await tp.click('#adm_rc_new_read');
      check(`New recipe from a statement (${nm}): the reader fills in the answers`,
            await waitFor(tp, () => /^It adds up/.test((document.querySelector('#adm_rc_new_body') || {}).innerText || ''), 90000) &&
            !!(await tp.$('#adm_rc_new_body select')), await text(tp, '#adm_rc_new_body'));
      await tp.click('#adm_rc_new_test'); await sleep(1500);
      await waitFor(tp, () => !document.documentElement.classList.contains('shiny-busy'), 90000); await sleep(800);
      check('...Test says it still adds up', /^It adds up/.test(await text(tp, '#adm_rc_new_body')), await text(tp, '#adm_rc_new_body'));
      await tp.click('#adm_rc_new_save');
      check('...and Save makes it a draft recipe, opened on its card',
            await waitFor(tp, n => /Saved as draft recipe/.test((document.querySelector('#adm_rc_msg') || {}).innerText || '') &&
              ((document.querySelector('#adm_rc_card h4') || {}).innerText || '').endsWith('ANZ ' + n), 90000, nm), await text(tp, '#adm_rc_card') + ' | ' + await text(tp, '#adm_rc_new'));
    }
  }
  await screen(tp, 'Admin Recipes');
  if (!LIVE) {
    // back on Needs attention: the two drafts are offered as one, and merged
    await tp.click('a[data-value="Needs attention"]'); await sleep(2000);
    await waitFor(tp, () => [...document.querySelectorAll('#adm_na_cards button[data-act^="merge|"]')]
      .some(b => (b.dataset.act.match(/_draft_/g) || []).length === 2), 30000);
    const mg = await tp.$$eval('#adm_na_cards button[data-act^="merge|"]', bs => bs.map(b => b.dataset.act)
      .filter(a => (a.match(/_draft_/g) || []).length === 2));
    check('two drafts of one design are offered as a merge', mg.length >= 1, await text(tp, '#adm_na_cards'));
    if (mg.length) await tp.click(`#adm_na_cards button[data-act="${mg[mg.length - 1]}"]`);
    check('...and Merge makes them one', await waitFor(tp, () => /are one now/.test((document.querySelector('#adm_na_msg') || {}).innerText || ''), 15000),
          await text(tp, '#adm_na_msg'));
    const ac = await tp.$$eval('#adm_na_cards button[data-act^="accept|"]', bs => bs.map(b => b.dataset.act));
    check('a draft waits to be accepted', ac.length >= 1, await text(tp, '#adm_na_cards'));
    if (ac.length) await tp.click(`#adm_na_cards button[data-act="${ac[0]}"]`);
    check('...and Accept makes it proven', await waitFor(tp, () => /is accepted/.test((document.querySelector('#adm_na_msg') || {}).innerText || ''), 15000),
          await text(tp, '#adm_na_msg'));
    await screen(tp, 'Admin Needs attention after');
  }
  // HEALTH holds what the old tabs did: how automatic reading is doing, and training
  await tp.click('a[data-value="Health"]'); await sleep(1500);
  check('Automatic reading counts what was read', /Statements read\s*\d+/i.test(await text(tp, '#adm_ar_head')),
        await text(tp, '#adm_ar_head'));
  check('...the automatic rate per kind of file', (await text(tp, '#adm_ar_kinds')).includes('PDF'));
  check('...and never counts a reading as "proven by" a check it failed',
        (await text(tp, '#adm_ar_proof')).includes('Checked against') && !(await text(tp, '#adm_ar_proof')).includes('Proven by'));
  if (!LIVE) {
    // spot checks: off by default; switched on, a conversion asks for one
    eq('spot checks are off by default', await tp.$eval('#adm_spot_rate', e => e.value), '0');
    await tp.fill('#adm_spot_rate', '100'); await tp.click('#adm_spot_save'); await sleep(1500);
    check('the admin sets the spot-check rate', (await text(tp, '#adm_spot_msg')).startsWith('Saved'), await text(tp, '#adm_spot_msg'));
    const sp = await freshConvert(ctx);
    await sp.click('#cv_try_sample'); await waitIdle(sp);
    if (ASK) {
      // a new design is asked about, never spot-checked: a person is already looking
      check('ask mode: a new design is asked about, not spot-checked', !(await sp.$('#cv_spot_right')) &&
            /New design/.test(await text(sp, '#cv_status')), await text(sp, '#cv_status'));
      await sp.close();
      await tp.fill('#adm_spot_rate', '0'); await tp.click('#adm_spot_save'); await sleep(1200);
    } else {
    check('a conversion picked for a spot check asks for one', (await text(sp, '#cv_spot')).includes('Spot check'),
          await text(sp, '#cv_spot'));
    await shot(sp, '09-spot-check');
    await sp.click('#cv_spot_right'); await sleep(1500);
    check('...and records the answer', (await text(sp, '#cv_spot')).includes('recorded'));
    await sp.close();
    await tp.fill('#adm_spot_rate', '0'); await tp.click('#adm_spot_save'); await sleep(1200);
    await tp.click('#adm_ar_refresh'); await sleep(2000);
    check('the spot check is counted', (await text(tp, '#adm_ar_spot')).includes('1 spot check answered: 1 right'),
          await text(tp, '#adm_ar_spot'));
    }
  }
  const [sdl] = await Promise.all([tp.waitForEvent('download', { timeout: 30000 }), tp.click('#adm_ar_export')]);
  const sjp = path.join(OUT, 'automatic-reading-summary.json'); await sdl.saveAs(sjp);
  let sj = {}; try { sj = JSON.parse(fs.readFileSync(sjp, 'utf8')); } catch { /* not JSON */ }
  check('the carry-off summary is counts only', String(sj.what || '').includes('counts only') && Number.isInteger(sj.statements),
        String(sj.what));
  if (!LIVE) {
    // training a bank: many statements, read in the background, then the report
    await selectize(tp, 'adm_train_bank', 'westpac');
    // ...one of them a BNZ export that says so: it proves itself, and teaches
    // Westpac nothing, and the report says which and why
    await tp.setInputFiles('#adm_train_files', [path.join(D, 'westpac_march.pdf'), path.join(D, 'asb_march.pdf'),
                                                path.join(D, 'bnz_export.csv')]);
    await sleep(2500);
    await tp.click('#adm_train_go');
    check('training reports what it found',
          await waitFor(tp, () => /layouts? from 3 statements/.test((document.querySelector('#adm_train_status') || {}).innerText || ''), 300000),
          await text(tp, '#adm_train_status'));
    if (ASK) check('ask mode: training asks about each new design rather than learning it',
          /3 need a look/.test(await text(tp, '#adm_train_status')) &&
          /has not seen this statement design before/.test(await text(tp, '#adm_train_status')) &&
          /bnz_export\.csv\s+-\s+It looks like a BNZ statement, so nothing was learned/.test(await text(tp, '#adm_train_status')),
          await text(tp, '#adm_train_status'));
    else check('...and lists another bank\'s statement as needing a look, with the reason',
          /1 needs a look/.test(await text(tp, '#adm_train_status')) &&
          /bnz_export\.csv\s+-\s+It looks like a BNZ statement, so nothing was learned/.test(await text(tp, '#adm_train_status')),
          await text(tp, '#adm_train_status'));
    console.log(`        (training: ${(await text(tp, '#adm_train_status')).split('\n')[0]})`);
  }
  await screen(tp, 'Admin Health automatic reading');
  await tp.click('a[data-value="Words"]'); await sleep(1500);
  //    one list of meanings, each with an example, and only what the reader reads
  const kinds = await tp.evaluate(() => Object.values(document.getElementById('adm_word_kind').selectize.options)
    .filter(o => o.value !== '').map(o => o.label));
  eq('nothing is picked for the person', await tp.$eval('#adm_word_kind', e => e.value), '');
  check('every meaning on Words carries an example', kinds.length >= 10 && kinds.every(k => /\(e\.g\. "/.test(k)),
        JSON.stringify(kinds));
  check('...and none the reader never reads', !kinds.some(k => /total credits|total debits|account name/i.test(k)),
        JSON.stringify(kinds));
  //    a wording that would clash is refused, with the reason; a new one is taught
  await tp.fill('#adm_word_text', 'balance'); await selectize(tp, 'adm_word_kind', 'opening_balance');
  await tp.click('#adm_word_add'); await sleep(2000);
  const clash = await text(tp, '#adm_word_msg');
  check('a wording that would clash is refused, and says why', /part of "closing balance"/.test(clash), clash);
  await tp.fill('#adm_word_text', 'Kickoff kitty'); await selectize(tp, 'adm_word_kind', 'opening_balance');
  await tp.click('#adm_word_add'); await sleep(2000);
  const taught = await text(tp, '#adm_word_msg');
  check('a new wording is taught, in plain words', taught.startsWith('From now on'), taught);
  if (!LIVE) check('...into the words file the server reads',
    fs.readFileSync(path.join(WORDS_DIR, 'labels.yaml'), 'utf8').includes('"kickoff kitty"'));
  await screen(tp, 'Admin Words');
  await tp.click('a[data-value="Health"]'); await sleep(1500);
  check('Health names layouts, not templates', !/template/i.test(await tp.evaluate(() =>
    document.querySelector('#adm_tabs + .tab-content .tab-pane.active').innerText)));
  await screen(tp, 'Admin Health');
  //    an admin teaches a wording with the statement beside it, on Please check
  await tp.click('a[data-value="Convert"]'); await sleep(800); await setQid(tp);
  await tp.setInputFiles('#cv_file', [path.join(D, 'anz_march.pdf')]);
  await waitFor(tp, () => document.querySelectorAll('tr.plan-row').length === 1, 60000);
  await pick(tp, 'anz_march.pdf', 'anz'); await go(tp); await waitIdle(tp);
  await clickIn(tp, 'anz_march.pdf', '.plan-file');
  await tp.click('#cv_ck_toggle'); await sleep(2500);
  await tp.click('summary:text("Teach it a wording from this statement")'); await sleep(2500);
  check('an admin can teach a wording from the statement on screen',
        await waitFor(tp, () => !!document.getElementById('cv_ck_teach_word'), 20000));
  check('...with nothing drawn as an error', !(await tp.$('#cv_ck_teach .shiny-output-error')));
  const offered = await tp.evaluate(() => Object.keys(document.getElementById('cv_ck_teach_word').selectize.options)
    .filter(v => v !== ''));
  console.log(`        (wordings offered from the ANZ sample: ${JSON.stringify(offered)})`);
  await shot(tp, 'tour-please-check-teach');
  eq('no console or script errors on any Admin screen', terr, []);
  await tp.close();

  // 12. a phone
  await page.setViewportSize({ width: 390, height: 900 }); await sleep(800);
  eq('nothing pushes the Convert page sideways on a phone',
     await page.evaluate(() => document.documentElement.scrollWidth - window.innerWidth), 0);
  await shot(page, '10-convert-phone');
  eq('no script errors on the Convert page', errs, []);
  await ctx.close();
  // THE SERVER'S SIDE OF IT: an error an observer swallowed, or a warning, shows
  // only in the R console -- the screen can look fine over it.
  if (!LIVE) {
    const bad = appLog.split('\n').filter(l => /^(Error|Warning)|^\s*New names:/.test(l));
    eq('the app\'s console has no errors or warnings', bad, []);
  }
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
