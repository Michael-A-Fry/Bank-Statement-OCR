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

<h4>CSV / Excel statements - confirm what was detected</h4>
<ol><li>On <b>Add a template</b>, upload the export and click <b>Open the toolkit</b> - the
date format, amount style and column mapping are detected for you. For Excel the right sheet is
picked automatically, junk rows above the header are skipped, and dates stored as Excel serial
numbers are read correctly.</li>
<li>Check the field pickers point at the right columns.</li>
<li>If the preview looks right, <b>Save</b>. That bank converts automatically from then on.</li></ol>

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

# about_html() -- the proof story under the About hub cards: how a conversion
# flows and how the tool earns trust. Deliberately short - the hub cards above
# (built in app.R) are the doors; this is the "why you can rely on it".
about_html <- function() HTML('
<style>
 .ab{max-width:1020px} .ab h3{color:#0b7a34;margin:26px 0 10px;font-size:16px}
 .ab .steps{display:flex;flex-wrap:wrap;counter-reset:step}
 .ab .step{flex:1 1 170px;max-width:196px;margin:0 12px 10px 0;font-size:12.5px;color:#555;
   padding-top:8px;border-top:3px solid #bfe0c8}
 .ab .step b{display:block;color:#1f2a33;font-size:13px;margin-bottom:2px}
 .ab .step b::before{counter-increment:step;content:counter(step) ".  ";color:#0b7a34}
 .ab .trust{display:grid;grid-template-columns:190px 1fr;max-width:860px;font-size:13px}
 .ab .trust dt{font-weight:600;color:#1f2a33;padding:7px 10px 7px 0;border-top:1px solid #eceeed}
 .ab .trust dd{margin:0;color:#555;padding:7px 0;border-top:1px solid #eceeed}
 .ab code{background:#eef;padding:0 3px;border-radius:3px} .ab .muted{color:#777;font-size:12.5px}
</style>
<div class="ab">
<h3>How a conversion flows</h3>
<div class="steps">
  <div class="step"><b>Upload</b>Your bank&#39;s export - PDF, CSV or Excel - on Convert.</div>
  <div class="step"><b>Detect</b>The statement is matched to a saved template automatically.</div>
  <div class="step"><b>Extract</b>Date, description, amount, balance - read verbatim, never edited.</div>
  <div class="step"><b>Check</b>Opening + transactions vs closing balance; anything off is flagged with the reason.</div>
  <div class="step"><b>Download</b>Excel, CSV or JSON - and rate the result so the team sees what works.</div>
</div>
<p class="muted">No template for it yet? The toolkit pre-fills one from your file; you confirm
against a live preview and save. Next time that bank just works.</p>

<h3>How you know it&#39;s right</h3>
<dl class="trust">
<dt>High confidence</dt><dd>Opening balance + every transaction = the closing balance the statement prints. Provably complete.</dd>
<dt>Medium confidence</dt><dd>Read cleanly, but a completeness check couldn&#39;t run (e.g. no running balance on the statement). Worth an eyeball.</dd>
<dt>Field coverage</dt><dd>Which fields are populated, which came back empty (maybe a wrong column), which aren&#39;t on this statement at all.</dd>
<dt>Diagnostics</dt><dd>When anything is off: where, why, and how to fix it - in plain words.</dd>
<dt>Redactions</dt><dd>Nothing under a redaction is ever read or guessed. A redacted cell stays <code>[REDACTED]</code>, its row is kept, and the tool never estimates what a black block hid.</dd>
</dl>
<p class="muted" style="margin-top:14px">Deeper how-to - drawing PDF columns, every way statements
differ - lives in the 2-minute guide on the Add-a-template tab, or <code>docs/wizard-tutorial.md</code>.
Best results come from CSV/Excel exports where your bank offers them.</p>
</div>')
