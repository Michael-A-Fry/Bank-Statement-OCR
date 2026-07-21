# ui_content.R -- large static UI content (the template-building tutorial modal +
# the About landing page) for app.R. Kept out of the main app file for
# readability; sourced by app.R only (NOT part of the R/ engine). shiny::HTML is
# referenced at call time, so this file sources fine without shiny loaded.

# tutorial_html() -- the step-by-step "how to build a template" walkthrough,
# opened from the Add-a-template tab. Mirrors docs/wizard-tutorial.md (the
# canonical, fuller version). Teaches the WAYS statements differ so nothing in
# the toolkit is a surprise.
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

<h4>Step 0 - read the statement&#39;s shape (9 questions)</h4>
<table><tr><th>#</th><th>Question</th><th>What it sets</th></tr>
<tr><td>1</td><td>File type: CSV/TSV, Excel, PDF-with-text, or scanned PDF?</td><td>How the toolkit reads it. Scanned PDFs are OCR&#39;d automatically - check the numbers.</td></tr>
<tr><td>2</td><td>How are amounts shown?</td><td>The amount style - see below. The #1 setting.</td></tr>
<tr><td>3</td><td>How are dates written?</td><td>The date format - see below.</td></tr>
<tr><td>4</td><td>Which columns exist?</td><td>Map what&#39;s there; leave the rest blank (e.g. no balance column).</td></tr>
<tr><td>5</td><td>A preamble before the table?</td><td>Header/junk lines above the real header row.</td></tr>
<tr><td>6</td><td>Multi-line rows?</td><td>Nothing to do - the 2nd line has no date, so it&#39;s ignored.</td></tr>
<tr><td>7</td><td>Redactions (black boxes)?</td><td>Nothing to do - a redacted cell is left <code>[REDACTED]</code> and the row is kept; a fully-hidden row just doesn&#39;t appear. Nothing under a redaction is ever read or guessed.</td></tr>
<tr><td>8</td><td>One account or several?</td><td>Combined statements parse but flag - balances aren&#39;t continuous across accounts.</td></tr>
<tr><td>9</td><td>How many statements in the file?</td><td>Merged bundles are flagged up front - split into one statement per file.</td></tr></table>

<h4>The ways AMOUNTS differ (pick one - named as in the toolkit dropdown)</h4>
<ul>
<li><b>One amount column, a minus sign means money out</b> (<code>signed</code>): <code>-45.00</code> out, <code>45.00</code> in.</li>
<li><b>Separate money-in and money-out columns</b> (<code>debit_credit_cols</code>): map <b>both</b>. <i>(worked example)</i></li>
<li><b>Amounts ending in DR / CR</b> (<code>dr_cr_suffix</code>): <code>123.45 DR</code> / <code>123.45 CR</code> - common on cards.</li>
<li><b>A D / C (debit / credit) indicator column</b> (<code>type_dc</code>): a column says D or C; map it too.</li>
<li><b>Unsigned amounts (credit card)</b> (<code>unsigned</code>): a plain number is a charge, a <code>CR</code> is a payment.</li>
</ul>
<p class="lead">Balance going the wrong way? Wrong amount style. Change it, watch the preview.</p>

<h4>The ways DATES differ</h4>
<table><tr><th>On the statement</th><th>Setting</th></tr>
<tr><td>21/04/2026</td><td>day/month/year (NZ/UK)</td></tr>
<tr><td>04/21/2026</td><td>month/day/year (US)</td></tr>
<tr><td>2026-04-21</td><td>year/month/day (ISO)</td></tr>
<tr><td>1 April 2025</td><td>day month-name year</td></tr>
<tr><td><b>21 Apr</b> (no year)</td><td><b>day month-name, no year</b> - year taken from the statement period automatically <i>(worked example)</i></td></tr>
<tr><td>21/04/26</td><td>day/month/2-digit-year</td></tr></table>

<h4>PDF statements - draw the boxes in the toolkit</h4>
<ol>
<li>On <b>Add a template</b>, upload the PDF and click <b>Open the toolkit</b>. The tool proposes
column boxes from the page; your statement stays on the left the whole time.</li>
<li>Check each column: pick the <b>field</b> (date, description, amount, balance&hellip;), drag a box
across that column on the page, click <b>Assign box &rarr; column</b>. For two-column amounts, draw
<b>debit</b> and <b>credit</b> boxes. Remove any proposed column that isn&#39;t really there.</li>
<li>Set the <b>date format</b> and <b>amount style</b> on the <b>Simple</b> tab.</li>
<li>The identifying phrases that recognise this bank next time are drafted automatically;
fine-tune them (and anything else) on the <b>Advanced</b> tab if needed.</li>
<li>Watch the <b>preview</b> under the page - only rows whose date box reads as a real date are
kept, so headings/notes/gaps drop out by themselves.</li>
<li><b>Save</b>. Column boxes are x-position only (full height) - you&#39;re defining columns; rows
are found by the date.</li>
</ol>

<h4>CSV / TSV statements - confirm what was detected</h4>
<ol><li>On <b>Add a template</b>, upload the export and click <b>Open the toolkit</b> - the
separator, date format and amount style are detected for you, and sample rows show on the left.</li>
<li>Check the description / reference / balance pickers point at the right columns.</li>
<li>If the preview looks right, <b>Save</b>. That bank converts automatically from then on.</li></ol>
<p class="lead"><b>Excel (.xlsx)?</b> Most Excel exports convert as-is on the Convert tab (a generic
Excel template ships with the tool). A custom Excel layout can&#39;t be drafted in the toolkit yet -
save the sheet as CSV (File &gt; Save As in Excel) and set that up here instead.</p>

<h4>Trust it when it reconciles</h4>
<p class="lead">In <b>Checks &amp; detail</b>: &quot;Each running balance follows from the last&quot; passing means
the columns are mapped right; &quot;Opening + transactions = closing balance&quot; passing means the
statement is provably complete. If a check fails, <b>Diagnostics</b> says where/why/how to fix -
usually a wrong amount style or date format.</p>

<h4>Troubleshooting</h4>
<table><tr><th>Symptom</th><th>Fix</th></tr>
<tr><td>No template matched</td><td>Check the identifying phrases (Advanced tab) really appear on the statement; check you&#39;re on the table page</td></tr>
<tr><td>Deposits look like withdrawals</td><td>Wrong amount style - switch it</td></tr>
<tr><td>Dates blank/wrong</td><td>Wrong date format; for year-less dates confirm the period is detected</td></tr>
<tr><td>Column empty / description cut off</td><td>Redraw / widen the box (stop before the amount column)</td></tr>
<tr><td>Rows missing</td><td>Their date box didn&#39;t read as a date - widen/move it. After converting, the X-ray view lists every skipped row and why.</td></tr></table>
<p class="lead" style="margin-top:12px">Full version with more detail: <code>docs/wizard-tutorial.md</code>.</p>
</div>')

# about_html() -- the one-stop-shop for a brand-new forensic accountant: what the
# tool is, how it flows end to end (visual), how it proves it's right, and how to
# start. Rendered on the About tab (the landing tab). The product name lives in
# the app header above, so this page opens with the promise, not a repeat title.
about_html <- function() HTML('
<style>
 .ab{max-width:1000px} .ab h3{color:#0b7a34;margin:22px 0 6px} .ab p{color:#333}
 .ab .lead{font-size:15px;color:#444;margin:2px 0 4px}
 .ab .flow{display:flex;flex-wrap:wrap;align-items:stretch;gap:0;margin:8px 0 6px}
 .ab .box{background:#f2f8f4;border:1px solid #bfe0c8;border-radius:8px;padding:10px 12px;min-width:110px;max-width:150px;font-size:12.5px}
 .ab .box b{display:block;color:#0b5} .ab .arrow{display:flex;align-items:center;padding:0 8px;color:#888;font-size:20px}
 .ab .branch{background:#fff8e6;border-color:#f0c36d} .ab .term{background:#eef;border-color:#c9c9ef}
 .ab table{border-collapse:collapse;margin:6px 0} .ab td,.ab th{border:1px solid #ddd;padding:5px 9px;font-size:13px;text-align:left}
 .ab th{background:#f2f6f2} .ab code{background:#eef;padding:0 3px;border-radius:3px} .ab .muted{color:#777}
 .ab ol{margin:4px 0 4px 18px}
</style>
<div class="ab">
<p class="lead">Turn any bank statement - PDF, Excel or CSV - into clean, audit-grade transaction
data you can trust. Built for forensic accountants. Deterministic (no AI guessing): if it
can&#39;t be sure, it tells you exactly why.</p>

<h3>How it flows, end to end</h3>
<div class="flow">
  <div class="box"><b>1. Upload</b>your statement (PDF / Excel / CSV) on the Convert tab</div>
  <div class="arrow">&rarr;</div>
  <div class="box"><b>2. Detect</b>it matches your bank to a saved template automatically</div>
  <div class="arrow">&rarr;</div>
  <div class="box"><b>3. Extract</b>date, description, amount, balance - verbatim</div>
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
<tr><td><b>High confidence</b></td><td>Opening balance + every transaction = the closing balance the statement prints. Provably complete.</td></tr>
<tr><td><b>Medium confidence</b></td><td>Parsed, but a check couldn&#39;t run (e.g. no balance on the statement). Eyeball it.</td></tr>
<tr><td><b>Completeness could not be auto-verified</b></td><td>Nothing to reconcile against - confirm the row count matches the statement.</td></tr>
<tr><td><b>Field coverage</b></td><td>Which fields are populated, which are empty (maybe a wrong column), which aren&#39;t on this statement.</td></tr>
<tr><td><b>Diagnostics</b></td><td>If anything&#39;s off: where, why, and how to fix it.</td></tr>
</table>
<p class="muted">Statements often arrive already redacted. Nothing under a redaction is ever read or guessed: a redacted cell is left <code>[REDACTED]</code> and its row is kept, a fully-hidden row simply doesn&#39;t appear, and the tool never estimates how many rows a black block hid. Merged multi-statement PDFs are detected and you&#39;re asked to split them.</p>

<h3>Get started in 3 steps</h3>
<ol>
<li><b>Convert tab</b> &rarr; upload a statement &rarr; <b>Convert</b>. Review the verdict and the analysis, then download. (No statement handy? There&#39;s a <b>Try it on a sample statement</b> button.)</li>
<li>If it says <b>&quot;No template for this statement yet&quot;</b>, click <b>&#128736; Set up a template for this</b> - the toolkit opens with most of it already worked out; check the preview, Save, then Convert again.</li>
<li>Rate the result at the bottom of the page (Correct / Minor issues / Wrong) so the team can see what works. That&#39;s it.</li>
</ol>
<p class="muted">Deeper how-to (drawing PDF columns, every way statements differ): the &#9432; guide on the Add-a-template tab, or <code>docs/wizard-tutorial.md</code>. Best results come from CSV/Excel exports where your bank offers them.</p>
</div>')
