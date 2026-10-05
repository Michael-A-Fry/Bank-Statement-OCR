# NEXT SESSION -- start here (written 5 Oct 2026)

# FINAL PHASE -- the owner's decisions (5 Oct). This phase is meant to be the LAST. Build exactly this.

SCOPE (day one): the QVF's 11 types ONLY -- ANZ, ANZ Loan, ANZ Visa, ASB Visa, BNZ Visa,
Kiwibank CC, Kiwibank PDF, Westpac CC (PDF) and BNZ (.xls), Kiwibank, Westpac (Excel).
Everything else (smaller banks, image-only PDFs, other designs, other exports) comes later.

DONE MEANS: on the owner's REAL statements, every in-scope type reads automatically at least
as often as the QVF does, 0 automatic-but-wrong, and any new or changed design needs at most
ONE round of plain questions. Owner runs as many real statements per type as possible.

UNKNOWN DESIGN: ALWAYS ASK ONCE. A statement no recipe recognises is NEVER converted without
a person: it gets the plain questions on Please check (pre-filled with the automatic
reader's guess), and the proven answers become a draft recipe. The automatic reader is only
the drafter. (Drift = same: questions once, new recipe version, old one untouched.)
Consequence: learned "layouts" are superseded by recipes -- one concept, not two.

ROLLOUT: side by side with the QVF; each type moves over once the new tool matches or beats
the QVF on real statements; the QVF is retired when all 11 have moved.

RUNS ON: the server now. Laptops (no-install folder) are the long-term goal once the owner
is confident -- keep everything laptop-friendly (R only, no external installs).

SPEED: a 150-page bundle of a known type in UNDER 1 MINUTE (text PDFs).

OUTPUT: Excel + CSV download (per statement / per case). No Qlik feed requirement.
Columns: full description as printed + separate type/particulars/code/reference; account
number and account name filled.

PROGRESS (owner's pain point): the queue showed only "1/5, 2/5" -- never progress WITHIN a
PDF. Show real progress per file: stage (reading text / OCR / reading the table / checking
the sums / writing files) and page n of N.

TIMINGS: log the time each step takes, anonymously (no names/numbers/file names in that
log): per file -- pages, kind, recipe id, seconds per step. Owner can share it so speed
problems are found from real use.

ACCEPTING A RECIPE: automatic after 3 proofs from 2+ accounts, or an admin sooner.
TWO RECIPES PROVE: identical figures -> newer recipe; any difference -> ask a person.


Paste to the new session: "Read docs/context/NEXT-SESSION.md on branch
wip/qvf-kit and do the next unchecked step. One step per commit; push after each."

## DECISIONS MADE BY CLAUDE from the owner's direction (owner may overrule any)
D1  ONE CONCEPT: recipes replace learned layouts. Layout learning stops; existing layout
    files stay on disk unused; Admin -> Banks lists recipes. Layout code is removed in the
    final code check.
D2  WHERE RECIPES LIVE: shipped (the 11) in recipes/ (product, overwritten on update);
    drafted/learned ones in the server's state folder templates/recipes/<bank>/<id>@v<n>.yaml
    -- versioned, never edited, never overwritten by an update (same rules layouts had).
D3  RECOGNITION: every recipe of every bank is tried; the chosen bank never hides one. If the
    recognised recipe's bank differs from the pick: "This reads as <bank> <product>, but
    <picked> was chosen. Use <bank>?" The bank box is pre-filled from the recipe.
D4  EXCEL IN RECIPES: the recipe format gains kind: excel/csv (exact column headers,
    preamble labels, sheet) because 3 of the 11 types are spreadsheets. Plus the 3 Excel
    rules: OD in the number format, "." = zero, split by an account-number column.
D5  ACCOUNT FIELDS IN RECIPES: a recipe names the labels for account number and account
    name (e.g. account: {number_label: "Account number", name_label: "Account name"}) so
    both are filled in every output.
D6  DRAFTING FROM ANSWERS: a draft recipe is written from the proven reading -- table
    header words, columns left to right, date format, money style, period label -- and
    recognise.all = the table's full header line + the longest digit-free title/box heading
    on the statement's first page. The admin sees and may edit the words before accepting.
D7  SPOT CHECKS: a recipe proven only by opening + rows = closing (no running balance, e.g.
    cards) gets the adaptive spot checks the owner chose earlier until it has 3 proofs.
D8  SIDE-BY-SIDE TOOL: tools/compare-qvf.R -- given the QVF's output CSV and the new tool's
    CSV for the same statement, prints counts only (rows, totals, rows matched on date +
    amount, rows only in one). The owner runs it locally; no data leaves.
D9  PROGRESS: the conversion job writes its stage and page n/N to its progress file; the
    queue shows "File 2 of 5 -- reading the table, page 37 of 150".
D10 TIMINGS: each conversion adds seconds per stage (text, OCR, recognise, read, prove,
    write) to the existing anonymous tracking line (no names, numbers or file names); the
    Admin "Automatic reading" carry-off summary includes median seconds per page by kind.
D11 SCOPE (corrected after owner's challenge): the 11 types are the FINISH LINE for "done",
    NOT a limit on what we pre-fill. From the owner's vocabulary doc keep every general
    wording that helps read ANY statement -- balance labels, period wording (incl. START),
    totals/subtotal lines ("Total of new transactions for", "Totals at end of page"),
    "Current balance", column headings, non-transaction lines, labels-above-figures -- as
    long as it passes the clash guard. Drop only the extra OUTPUT fields (loan/fee/event/
    notice models, transaction classes, new value types): output is transactions in
    Excel/CSV. The old "Stage 4" list -> dropped except D7, D9, D10.
    Admin: see D15 (owner: admin must be "simple as f***" -- it is a core deliverable).
D14 DRAFT RECIPES FROM EVERYTHING WE KNOW: any design we have partial knowledge of gets a
    status: draft recipe -- the 13 old 1.x templates in tests/testthat/fixtures/templates
    (ANZ/ASB/Westpac everyday PDF with x-bands, and CSV exports), the owner's Copilot
    STATEMENT DESIGN blocks for other types, and the doc's wording. A draft is NEVER trusted
    alone (owner's "always ask once"): a statement it recognises gets the Please check
    questions PRE-FILLED from the draft, so the person confirms in one click instead of
    answering from scratch; each confirmed reading that proves counts as a proof; after 3
    proofs from 2+ accounts (or an admin) it becomes proven and automatic. A recipe needs at
    minimum recognise words, header/columns, date format and money style to READ; anything
    less is still used as pre-fill.
D12 VERSION: the release is 3.0.0 (recipes), with a 2.x -> 3.0 server update guide.
D13 FINAL CODE CHECK at the end, focused on the recipe path, the proof, outputs and the
    removal of superseded code (layout learning, the generic reader's auto-convert path).

D15 ADMIN -- RECIPES MUST BE DEAD SIMPLE TO CREATE, MODIFY, ENABLE/DISABLE AND MERGE.
    Rule: the admin NEVER sees YAML; every change is made while LOOKING at a real page.
    Admin has 4 tabs:
      1. NEEDS ATTENTION -- cards with counts: statements needing a look (page picture +
         Fix -> Please check); draft recipes waiting (Accept / Retire); recipes failing
         recently = probable drift (Fix); suggested merges (Merge).
      2. RECIPES -- one row per recipe: bank | name | ON/OFF toggle | statements read |
         needed help (last 30 days) | draft/proven. Click -> the recipe card.
      3. WORDS (as now). 4. HEALTH (as now). Banks/layouts tab retired (D1).
    THE RECIPE CARD (one screen): sample page with columns drawn + numbered; the same plain
    Please-check questions with this recipe's answers; recognise-by words as chips; version
    history.
    CREATE: never a blank form. (a) Please check answers that prove -> "Save as a recipe for
      <bank> <product>?" (b) Admin -> "New recipe from a statement": upload one -> questions
      pre-filled by the automatic reader -> Read it -> Save.
    MODIFY: on the card change an answer, drag a column edge, or click a word on the page to
      add/remove it as a recognise-by word -> TEST re-reads every statement this recipe read
      that the server still keeps (uploads retention) and says "23 statements: all still add
      up, 0 figures changed" -> Save = NEW VERSION (old kept). A change that breaks earlier
      statements or changes their figures is blocked, with the reason.
    ENABLE/DISABLE: one toggle; Off = not tried (statements get the questions). Never delete;
      Retire = Off + hidden.
    MERGE: offered only when both recipes are checked to read each other's statements
      identically; one click keeps the one with more proofs, unions recognise words and proof
      counts, switches the other Off pointing to the survivor.
    UNDO: version history -> "Use this version".
    Safety stays automatic: the proof gates every reading; a recipe is never live without
    proofs (3 from 2+ accounts, or admin Accept). The existing pieces to reuse: the page
    picture + numbered columns + plain questions (app.R cv_ck_*), the drawn-columns editor
    (ed_*), wip/admin-review (R/review.R) for the needs-a-look list.
    Browser checks (tools/ui/check.mjs) must drive every one of these actions end to end.

D16 THE ACCOUNTANT'S SCREEN -- NO MORE COMPLEX THAN THE QVF (owner: "INFO OVERLOAD").
    The QVF gives a transaction table plus a per-row "Balance Check". That is the ceiling.
    1. ONE sentence at the top: "Done -- 42 transactions, the balance adds up." or
       "Needs you -- the balance stops adding up at 14 Mar (page 3)."
    2. The table, QVF-style: Date | Description | Money out | Money in | Balance | Check
       (tick / cross / blank). Jumps to and highlights the FIRST cross; clicking a row shows
       that page with the line marked.
    3. The plain column questions ONLY when the tool is unsure what a column is.
    4. At most three buttons: "Read it again" | "It's right -- accept it" | "Set aside".
    5. Downloads (Excel, CSV) once done or accepted.
    REMOVED from the accountant's screen (to Admin, or behind ONE closed "More detail"
    link): the checks list, diagnostics, flags, charts, field coverage, "Draw the columns
    yourself", word teaching, the feedback form, layout/recipe chips.
    Convert table "Outcome" column: one word -- Done / Needs you / Couldn't read -- plus at
    most 8 words of reason.
    Browser checks must assert the accountant's screen holds nothing beyond this list.

## Rules (owner's, still in force)
- Work branch: claude/bank-statement-ocr-platform-t6n934. Pushing to main allowed when the full suite is green.
- Commit footer: Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com> / Claude-Session: https://claude.ai/code/session_01Sf7ppx17bCYkfX8ivYbVEb
- R sources ASCII only. Short, plain answers to the owner. Max 6 agents. No per-claim verifier agents.
- Every fix = one general rule in one sentence. AUTO_WRONG must stay 0.
- Owner's private PDFs: never copy, commit or give to agents. Don't worry about Qlik dashboards (owner: no need to match QVF output).
- Tokens are scarce: prefer doing it yourself in small steps over big agent fan-outs.

## Where things are
- main = 3c61147 (5 Oct, end of session): EVERYTHING below merged -- words screen, toddler-clear questions, speed fixes, .xls support AND the recipe reader with ANZ + ANZ Loan recipes. Full suite 6,776/0/0. Earlier main 8b1ae4b had: words screen (one list + clash guard + teach from Please check),
  plain toddler-clear column questions on Please check, speed fixes. Suite 6,645/0/0, browser 125/125.
- claude/bank-statement-ocr-platform-t6n934 = 36d2209 = main + .xls support (related tests green; full suite not yet run).
- wip/recipes: recipe reader (R/recipes.R, recipes/anz_everyday_pdf.yaml + anz_loan_pdf.yaml). MEASURED: own tests 122/0; ANZ lookalikes 14/15 auto_right, 0 wrong (was 8/15). Zoo regression unchanged (0 wrong). Not yet: full suite. See RECIPES-STATUS.md there.
- wip/admin-review: Admin Review screen (R/review.R, app.R). UNTESTED, cut off mid-work ("selection-by-id on Banks' two tables").
- wip/qvf-kit (this branch): docs/context/qvf/ = format cards of the QVF's 11 types, lookalike generators (python, import tools/synth/make_layouts.py), BRIEF.md, recipes-design.md.

## Decision made: direction C = recipes + proof (read docs/context/qvf/recipes-design.md)
Why: on lookalikes of the team's real 11 QVF types the generic reader gets ~36% automatic (0 wrong).
Findings per type are in the cards (sections "special cases"); key general fixes found:
START-date open periods; "No transactions for this period" = proven empty; dated no-money lines (rate change) are events;
section title mistaken for column headings (Westpac card); summary labels-over-figures line (BNZ Visa);
"Current Balance" = closing on cards; dates restart per cardholder section; split on repeated header block;
account number/name empty in feed. ANZ Visa/ASB Visa cards were NOT written (agent stopped) -- read QVF script lines 4120-5471.

## Steps (tick when done) -- ONE step per commit, push after each
Done already: wip/recipes merged to main (3c61147): recipe reader + ANZ and ANZ Loan
recipes, 14/15 on lookalikes, 0 wrong, zoo unchanged, suite green. Copilot intake prompt
on main (docs/context/recipe-intake-prompt.md).
0. [ ] Read: this file top to bottom, HOW-WE-GOT-HERE.md, qvf/FINDINGS.md.
1. [ ] Branch from latest main. Regenerate the QVF lookalikes (qvf/gen) and re-score the
       baseline with score_convert.R --mode cold.
2. [ ] GATE, part 1: ask the owner to run REAL statements of the 11 types through the
       CURRENT tool and report counts per type (automatic / needs you / couldn't read);
       ask for Copilot STATEMENT DESIGN blocks for each type. Continue with 3+ meanwhile.
3. [ ] Recipe engine changes the decisions need: D3 (all banks' recipes tried; wrong-pick
       message), two-recipe rule (same figures -> newer; differ -> person), drift = ask once,
       kind excel/csv (D4), account fields (D5), "always ask once" for unknown designs
       (automatic reader only pre-fills). Tests for each.
4. [ ] Recipes, credit cards first: anz_visa, asb_visa, bnz_visa, kiwibank_cc, westpac_cc
       (one per commit; from Copilot blocks if given, else cards + QVF script lines;
       each measured on its lookalikes, 0 wrong). Use FINDINGS rules 7-14.
5. [ ] Recipes, everyday + Excel: kiwibank_pdf, bnz/kiwibank/westpac excel (FINDINGS 15-21).
6. [ ] GATE, part 2: owner reruns the SAME real files; recipes must raise the automatic
       count with 0 wrong. If not, STOP and rethink with the owner.
7. [ ] D16 the accountant's screen (QVF-simple) + D9 progress + D10 timings.
8. [ ] D15 admin (Needs attention / Recipes / Words / Health; create, modify+Test,
       toggle, merge, undo) + D14 draft recipes from the 13 old templates and Copilot
       blocks + D1 layouts retired.
9. [ ] Speed: 150-page bundle of a known type under 1 minute (make_bench.py); stop failed
       readings re-reading every page.
10. [ ] Output: full description + type/particulars/code/reference columns; account
       number and name; D8 tools/compare-qvf.R for the side-by-side rollout.
11. [ ] Final code check (D13), full suite, browser checks, version 3.0.0 + server update
       guide (D12). Push main.

## If the owner pastes a "STATEMENT DESIGN" block
It came from docs/context/recipe-intake-prompt.md (on main): Copilot describing a real statement with nothing personal. Write recipes/<bank>_<product>_<kind>.yaml from it, following recipes/anz_everyday_pdf.yaml; build a lookalike from its SAMPLE (tools/synth/make_layouts.py helpers) and prove the recipe reads it automatically with 0 wrong before committing.

## Drift, wrong picks and bank selection (owner asked 5 Oct; decisions to build)
Today (main): recipe_recognise() skips recipes of another bank when a bank is chosen
(R/recipes.R:289); a recognised recipe that does not prove falls back to the automatic
reader with the note "The bank may have changed the design" (R/recipes.R:337).
Build:
1. Never edit a recipe. A drift (e.g. ANZ Visa Oct 2026) becomes a NEW recipe or a new
   version beside the old one; old statements keep proving on the old one.
2. When several recipes are recognised (old + new design, or a tie), READ WITH EACH and let
   the proof decide; two that prove with different figures -> person. (Today a tie picks none.)
3. Try every bank's recipes, not just the chosen bank's. If another bank's recipe proves it,
   say plainly: "This reads as an ANZ Visa statement, but ASB was chosen. Use ANZ?"
4. Three cases, three messages, three Admin Review lists:
   - WRONG PICK: another bank's recipe proves it, or bank identity disagrees with the pick.
   - DRIFT: the same recipe's words are found but its reading does not prove; nothing else fits.
   - NEW DESIGN: no recipe's words are found.
5. Drift / new design -> automatic reader; if it proves, convert and flag "recipe may need
   updating"; else Please check questions -> proven answers saved as a DRAFT recipe
   (version n+1 or new id) -> admin accepts in Admin -> Banks. Accept = proven.
6. Bank stays pre-filled from the statement (no type picking, unlike the QVF); a person
   only picks when the statement does not say. Overriding against the statement is flagged.

## Owner's answers (5 Oct, end of session) -- build to these
- PRIORITY: credit cards and everyday-account PDFs first (Excel and loans after).
- REAL SAMPLES: owner will run docs/context/recipe-intake-prompt.md (on main) in Copilot on
  REAL statements of every type and paste the STATEMENT DESIGN blocks. Build recipes from
  those (lookalikes only as a fallback), then build a lookalike from each SAMPLE to test.
- WHO ACCEPTS A NEW/CHANGED RECIPE: automatic after 3 proofs from at least 2 different
  accounts, or an admin accepts sooner (same rule as layouts today). Until then it is a
  draft; the file a person fixed converts at once.
- TWO RECIPES BOTH PROVE: if the figures are EXACTLY the same, use the newer recipe;
  if they differ in any way, ask a person (show both on Please check).
- OUTPUT DETAILS: BOTH -- one full description as printed (type code first, like the QVF's
  "Description as per bank statement") AND separate columns for type, particulars, code,
  reference where printed. (Today ANZ's 5 detail columns land as text1..text4.)
- SCANS: true scans very unlikely, BUT 10-15% are "non-selectable" PDFs, mostly smaller
  banks. Two kinds to handle: (a) no text layer -> OCR (R/ocr.R exists); (b) a text layer
  that is garbage (fonts without a Unicode map: copy-paste gives symbols) -> must be
  DETECTED and OCR'd instead, never read as text. Recipes must accept OCR'd words too
  (today recipes read kind: pdf only -- R/recipes.R:124). Ask owner for one such file via
  the Copilot prompt (item 3 = pdf-scanned) to see which kind it is.
- SIZE: huge bundles (150+ pages) are common -> speed is a priority. Recipes avoid the
  guessing; also stop failed readings re-reading every page up to 7 times
  (R/auto_read.R .ar_read_pdf repair loop) and measure with tools/synth/make_bench.py.
- R VERSION for laptop mode: unknown; laptop mode parked until owner checks R.version.string.
- NON-SELECTABLE PDFs (owner confirmed 5 Oct): dragging the cursor selects nothing -> NO text
  layer (kind (a) above), i.e. page images. Read with OCR (R/ocr.R), same as a scan. These come
  from smaller banks, which the QVF never supported (its 11 types are all big banks), so the
  QVF/Inphinity Mole offers no rule to copy here. Work: let recipes read OCR'd words
  (kind pdf + scan), and get one such statement described via the Copilot prompt (item 3 =
  pdf-scanned). Whether Mole itself OCRs is unknown (web search found nothing definite).

## THE GATE (owner challenged the direction, 5 Oct -- be honest, not certain)
The ANZ 14/15 result is partly CIRCULAR: the recipe and the lookalikes were both written
from the same QVF format card. It proves the machinery is safe, not that recipes beat the
generic reader on REAL statements. Earlier the generic reader was "certain" on synthetic
sets and real statements disagreed. So, before writing many recipes:
1. Owner runs REAL statements of each priority type through the CURRENT tool on their own
   machine and reports counts per type: automatic / Please check / failed (no PII).
2. Add the recipes for those types; owner reruns the SAME files.
3. Recipes stay only if real-statement automatic counts go up with 0 automatic-but-wrong.
   If they do not, stop and rethink with the owner. Real files are the judge, not lookalikes.

## Best for the analyst (owner asked "what's best for the user?", 5 Oct)
Order of what matters to her: (1) right figures she can trust; (2) almost no clicks, nothing
to pick when the statement says it; (3) fast on 150+ page bundles; (4) help = one plain
question with her own lines, once per design; (5) predictable -- same type reads the same
way (recipes); (6) every statement she gets covered (small banks, image-only PDFs).
ROLLOUT RECOMMENDED: run side by side. The QVF keeps its 11 types while the new tool runs
on the same files and is compared; a type moves over once the new tool matches or beats it
on real statements. Everything the QVF cannot read moves to the new tool at once.
Judge by analyst minutes per statement and how often she has to step in, not test scores.
