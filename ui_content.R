# ui_content.R -- large static UI content (the About landing page) for app.R.
# Kept out of the main app file for readability; sourced by app.R only (NOT part
# of the R/ engine). shiny::HTML is referenced at call time, so this file sources
# fine without shiny loaded.

# about_html() -- the proof story under the About hub card: how a conversion flows
# and how the tool earns trust. Deliberately short - the card above (built in
# app.R) is the door; this is the "why you can rely on it".
#
# EVERY CLAIM HERE IS ONE THE READER KEEPS. "Proven" means what R/auto_read.R
# means by it: every balance step holds to the cent and no other reading of the
# columns fits. A statement that prints no balance and no totals is never called
# proven; it converts on its own only when it matches a layout already proven.
about_html <- function() HTML('
<style>
 .ab{max-width:1020px} .ab h3{color:#00205b;margin:26px 0 10px;font-size:16px}
 .ab .steps{display:flex;flex-wrap:wrap;counter-reset:step}
 .ab .step{flex:1 1 170px;max-width:196px;margin:0 12px 10px 0;font-size:12.5px;color:#555;
   padding-top:8px;border-top:3px solid #b6c8e0}
 .ab .step b{display:block;color:#1f2a33;font-size:13px;margin-bottom:2px}
 .ab .step b::before{counter-increment:step;content:counter(step) ".  ";color:#00205b}
 .ab .trust{display:grid;grid-template-columns:190px 1fr;max-width:860px;font-size:13px}
 .ab .trust dt{font-weight:600;color:#1f2a33;padding:7px 10px 7px 0;border-top:1px solid #eceeed}
 .ab .trust dd{margin:0;color:#555;padding:7px 0;border-top:1px solid #eceeed}
 .ab .muted{color:#777;font-size:12.5px}
 @media (max-width:767px){.ab .trust{grid-template-columns:1fr}}
</style>
<div class="ab">
<h3>How a conversion flows</h3>
<div class="steps">
  <div class="step"><b>Upload</b>Your bank&#39;s statements - PDF, scan, CSV or Excel - on Convert.</div>
  <div class="step"><b>Bank</b>Each file&#39;s bank is filled in from the statement itself. Change it if it is wrong.</div>
  <div class="step"><b>Read</b>Dates, descriptions and figures are read from the page&#39;s content, not from heading words.</div>
  <div class="step"><b>Prove</b>The statement&#39;s own arithmetic - its running balance, opening and closing balance, printed totals - must add up.</div>
  <div class="step"><b>Download</b>Excel or CSV. Anything that did not prove is shown on Please check with the reason.</div>
</div>
<p class="muted">Each bank&#39;s layouts are learned from the statements it has read and proved, so the
next statement of the same layout is quicker and surer. A layout is a starting point, never the
answer: every statement is checked against its own arithmetic every time.</p>

<h3>How you know it&#39;s right</h3>
<dl class="trust">
<dt>Proven</dt><dd>Every balance step adds up to the cent, and no other reading of the columns
fits. Nothing to do.</dd>
<dt>Matches a learned layout</dt><dd>The statement prints no running balance, but its totals
check (where printed) and it matches a layout this bank has already proven.</dd>
<dt>Please check</dt><dd>Read, but not proven - for example two readings both fit, or a balance
step does not add up. You see the page with the columns drawn on it, set what a column is if it
is wrong, and re-read; or confirm it is right.</dd>
<dt>Couldn&#39;t read</dt><dd>Nothing usable came out. The reason is shown.</dd>
<dt>Derived amounts</dt><dd>An amount that could not be read but that the running balance fixes is
filled in, marked in the Flags column, and the statement always goes to Please check.</dd>
</dl>
<p class="muted" style="margin-top:14px">Best results come from CSV or Excel exports where your bank
offers them.</p>
</div>')
