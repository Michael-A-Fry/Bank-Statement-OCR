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
# proven; it converts on its own only when a recipe a person accepted reads it.
about_html <- function() HTML('
<div class="ab">
<h3 class="ab-h">How it works</h3>
<ol class="ab-steps">
  <li class="ab-step"><span class="ab-num">1</span><b>Check the bank</b><span>Upload PDF, scan, CSV or Excel on Convert. Each file&#39;s bank is filled in for you; change it if it is wrong.</span></li>
  <li class="ab-step"><span class="ab-num">2</span><b>We read it</b><span>Dates, descriptions and figures are read from what is on the page.</span></li>
  <li class="ab-step"><span class="ab-num">3</span><b>We check the maths</b><span>The statement&#39;s own running balance and totals must add up to the cent.</span></li>
  <li class="ab-step"><span class="ab-num">4</span><b>Download</b><span>Excel or CSV. Anything not proven is shown to you with the reason.</span></li>
</ol>

<h3 class="ab-h">How you know it&#39;s right</h3>
<dl class="ab-trust">
<dt><span class="pill pill-ok">Done</span></dt><dd>The design is one the tool knows, and every balance step adds up to the cent.
Nothing to do.</dd>
<dt><span class="pill pill-warn">Needs you</span></dt><dd>Read, but not proven &#8212; for example the balance stops adding up at a row,
or the design is new. You see the page, answer a plain question if asked, and it is read again.</dd>
<dt><span class="pill pill-bad">Couldn&#39;t read</span></dt><dd>Nothing usable came out. The reason, and what to try, is shown.</dd>
</dl>

<h3 class="ab-h">Behind the scenes</h3>
<div class="ab-more">
<p>Best results come from CSV or Excel exports.</p>
</div>
</div>')
