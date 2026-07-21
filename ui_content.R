# ui_content.R -- large static UI content (the template-building tutorial modal +
# the About landing page) for app.R. Kept out of the main app file for
# readability; sourced by app.R only (NOT part of the R/ engine). shiny::HTML is
# referenced at call time, so this file sources fine without shiny loaded.

# tutorial_html() -- the step-by-step "how to build a template" walkthrough shown
# in a modal from either wizard. Mirrors docs/wizard-tutorial.md (the canonical,
# fuller version). Teaches the WAYS statements differ so nothing is a surprise.
tutorial_html <- function() HTML('
<style>
 .tut h4{margin:16px 0 6px;color:#137333} .tut table{border-collapse:collapse;width:100%;margin:6px 0}
 .tut th,.tut td{border:1px solid #ddd;padding:5px 8px;text-align:left;vertical-align:top;font-size:13px}
 .tut th{background:#f2f6f2} .tut code{background:#eef;padding:0 3px;border-radius:3px}
 .tut ol,.tut ul{margin:4px 0 4px 18px} .tut .lead{color:#555}
</style>
<div class="tut">
<p class="lead">The engine has <b>zero</b> banks built in. A template just says: the date is in
<i>this</i> column, the amount in <i>that</i> one, amounts are shown <i>this</i> way, and here is a
phrase that proves it is this bank. Add a bank = add one template. Follow along with the worked
example <code>samples/raw/tutorial/sample_everyday_statement.pdf</code>.</p>

<h4>Step 0 &mdash; read the statement&#39;s shape (9 questions)</h4>
<table><tr><th>#</th><th>Question</th><th>What it sets</th></tr>
<tr><td>1</td><td>File type: CSV/TSV, Excel, PDF-with-text, or scanned PDF?</td><td>Which wizard. Scanned PDFs are OCR&#39;d automatically &mdash; check the numbers.</td></tr>
<tr><td>2</td><td>How are amounts shown?</td><td>The amount style &mdash; see below. The #1 setting.</td></tr>
<tr><td>3</td><td>How are dates written?</td><td>The date format &mdash; see below.</td></tr>
<tr><td>4</td><td>Which columns exist?</td><td>Map what&#39;s there; leave the rest blank (e.g. no balance column).</td></tr>
<tr><td>5</td><td>A preamble before the table?</td><td>Header/junk lines above the real header row.</td></tr>
<tr><td>6</td><td>Multi-line rows?</td><td>Nothing to do &mdash; the 2nd line has no date, so it&#39;s ignored.</td></tr>
<tr><td>7</td><td>Redactions (black boxes)?</td><td>Nothing to do &mdash; a redacted cell is left <code>[REDACTED]</code> and the row is kept; a fully-hidden row just doesn&#39;t appear. Nothing under a redaction is ever read or guessed.</td></tr>
<tr><td>8</td><td>One account or several?</td><td>Combined statements parse but flag &mdash; balances aren&#39;t continuous across accounts.</td></tr>
<tr><td>9</td><td>How many statements in the file?</td><td>Merged bundles are flagged up front &mdash; split into one statement per file.</td></tr></table>

<h4>The ways AMOUNTS differ (pick one)</h4>
<ul>
<li><b>One signed column</b> (<code>signed</code>): <code>-45.00</code> out, <code>45.00</code> in.</li>
<li><b>Two columns: Withdrawals &amp; Deposits</b> (<code>debit_credit_cols</code>): map <b>both</b>. <i>(worked example)</i></li>
<li><b>DR/CR suffix</b> (<code>dr_cr_suffix</code>): <code>123.45 DR</code> / <code>123.45 CR</code> &mdash; common on cards.</li>
<li><b>A Type column of D/C</b> (<code>type_dc</code>): a column says D or C; map it too.</li>
</ul>
<p class="lead">Balance going the wrong way? Wrong amount style. Change it, Preview again.</p>

<h4>The ways DATES differ</h4>
<table><tr><th>On the statement</th><th>Setting</th></tr>
<tr><td>21/04/2026</td><td>day/month/year (NZ/UK)</td></tr>
<tr><td>04/21/2026</td><td>month/day/year (US)</td></tr>
<tr><td>2026-04-21</td><td>year/month/day (ISO)</td></tr>
<tr><td>1 April 2025</td><td>day month-name year</td></tr>
<tr><td><b>21 Apr</b> (no year)</td><td><b>day month-name, no year</b> &mdash; year taken from the statement period automatically <i>(worked example)</i></td></tr>
<tr><td>21/04/26</td><td>day/month/2-digit-year</td></tr></table>

<h4>PDF wizard &mdash; draw the boxes</h4>
<ol>
<li>Upload the PDF, pick the page with the table (often page 2).</li>
<li>For each column: choose the <b>field</b>, drag a box across that column, click <b>Assign</b>. Do date, description, the amount column(s), balance. For two-column amounts, draw <b>debit</b> and <b>credit</b> boxes.</li>
<li>Set the <b>date format</b> and <b>amount style</b>.</li>
<li>Type a <b>fingerprint phrase</b> that&#39;s on this statement and few others (e.g. <code>Transaction details</code>).</li>
<li><b>Preview</b> &mdash; only rows whose date box reads as a real date are kept, so headings/notes/gaps drop out by themselves.</li>
<li><b>Save</b>. Boxes are x-position only (full height) &mdash; you&#39;re defining columns; rows are found by the date.</li>
</ol>

<h4>Delimited (CSV/Excel) wizard</h4>
<ol><li>Upload the sample &mdash; delimiter, dates and amount style auto-detect.</li>
<li>Check each field&#39;s dropdown points at the right column (set <code>(none)</code> if absent).</li>
<li>Tick fingerprint columns, <b>Preview</b>, <b>Save</b>.</li></ol>

<h4>Trust it when it reconciles</h4>
<p class="lead">In <b>Checks</b>: <code>running_balance_continuity</code> pass = columns mapped right;
<code>balance_reconciliation</code> pass = opening + all transactions = closing (provably correct).
If a check fails, <b>Diagnostics</b> says where/why/how to fix &mdash; usually wrong amount style or date format.</p>

<h4>Troubleshooting</h4>
<table><tr><th>Symptom</th><th>Fix</th></tr>
<tr><td>No template matched</td><td>Loosen/repick the fingerprint; check you&#39;re on the table page</td></tr>
<tr><td>Deposits look like withdrawals</td><td>Wrong amount style &mdash; switch it</td></tr>
<tr><td>Dates blank/wrong</td><td>Wrong date format; for year-less dates confirm the period is detected</td></tr>
<tr><td>Column empty / description cut off</td><td>Redraw / widen the box (stop before the amount column)</td></tr>
<tr><td>Rows missing</td><td>Their date box didn&#39;t read as a date &mdash; widen/move it</td></tr></table>
<p class="lead" style="margin-top:12px">Full version with more detail: <code>docs/wizard-tutorial.md</code>.</p>
</div>')

# about_html() -- the one-stop-shop for a brand-new forensic accountant: what the
# tool is, how it flows end to end (visual), how it proves it's right, and how to
# start. Rendered on the About tab (the landing tab).
about_html <- function() HTML('
<style>
 .ab{max-width:960px} .ab h3{color:#0b7a34;margin:22px 0 6px} .ab p{color:#333}
 .ab .flow{display:flex;flex-wrap:wrap;align-items:stretch;gap:0;margin:8px 0 6px}
 .ab .box{background:#f2f8f4;border:1px solid #bfe0c8;border-radius:8px;padding:10px 12px;min-width:120px;max-width:170px;font-size:12.5px}
 .ab .box b{display:block;color:#0b5} .ab .arrow{display:flex;align-items:center;padding:0 8px;color:#888;font-size:20px}
 .ab .branch{background:#fff8e6;border-color:#f0c36d} .ab .term{background:#eef;border-color:#c9c9ef}
 .ab table{border-collapse:collapse;margin:6px 0} .ab td,.ab th{border:1px solid #ddd;padding:5px 9px;font-size:13px;text-align:left}
 .ab th{background:#f2f6f2} .ab code{background:#eef;padding:0 3px;border-radius:3px} .ab .muted{color:#777}
 .ab ol{margin:4px 0 4px 18px}
</style>
<div class="ab">
<h2 style="margin:0">Bank Statement OCR</h2>
<p class="muted">Turn any bank statement - PDF, Excel or CSV - into clean, audit-grade transaction data you can trust. Built for forensic accountants. Deterministic (no AI guessing): if it can&#39;t be sure, it tells you exactly why.</p>

<h3>How it flows, end to end</h3>
<div class="flow">
  <div class="box"><b>1. Upload</b>your statement (PDF / Excel / CSV) on the Convert tab</div>
  <div class="arrow">&rarr;</div>
  <div class="box"><b>2. Detect</b>it matches your bank to a saved template automatically</div>
  <div class="arrow">&rarr;</div>
  <div class="box"><b>3. Extract</b>date, description, amount, balance &mdash; verbatim</div>
  <div class="arrow">&rarr;</div>
  <div class="box"><b>4. Check</b>reconciles the balance &amp; flags anything off (trust score)</div>
  <div class="arrow">&rarr;</div>
  <div class="box term"><b>5. Download</b>Excel / CSV / JSON, and rate the result</div>
</div>
<div class="flow">
  <div class="box branch" style="max-width:360px"><b>New bank? No template yet &rarr; Guided setup</b>The tool pre-fills a template from your file; you check the preview and click Save. Next upload of that bank just works. No coding, no jargon.</div>
</div>

<h3>How you know it&#39;s right</h3>
<table>
<tr><th>Signal</th><th>What it tells you</th></tr>
<tr><td><b>Trust: high</b></td><td>Opening balance + every transaction = the closing balance the statement prints. Provably complete.</td></tr>
<tr><td><b>Trust: medium</b></td><td>Parsed, but a check couldn&#39;t run (e.g. no balance on the statement). Eyeball it.</td></tr>
<tr><td><b>Completeness: unverified</b></td><td>Nothing to reconcile against &mdash; confirm the row count matches the statement.</td></tr>
<tr><td><b>Field coverage</b></td><td>Which fields are populated, which are empty (maybe a wrong column), which aren&#39;t on this statement.</td></tr>
<tr><td><b>Diagnostics</b></td><td>If anything&#39;s off: where, why, and how to fix it.</td></tr>
</table>
<p class="muted">Statements often arrive already redacted. Nothing under a redaction is ever read or guessed: a redacted cell is left <code>[REDACTED]</code> and its row is kept, a fully-hidden row simply doesn&#39;t appear, and the tool never estimates how many rows a black block hid. Merged multi-statement PDFs are detected and you&#39;re asked to split them.</p>

<h3>Get started in 3 steps</h3>
<ol>
<li><b>Convert tab</b> &rarr; upload a statement &rarr; <b>Convert</b>. Review the checks &amp; coverage, then download.</li>
<li>If it says <b>unsupported</b>, click <b>🪄 Set up this statement (guided)</b> - confirm the pre-filled preview, Save. Convert again.</li>
<li>Rate the result (the thumbs) so the team can see what works. That&#39;s it.</li>
</ol>
<p class="muted">Deeper how-to (drawing PDF columns, every way statements differ): the <b>ⓘ</b> button on the Add-a-template tab, or <code>docs/wizard-tutorial.md</code>. Best results come from CSV/Excel exports where your bank offers them.</p>
</div>')
