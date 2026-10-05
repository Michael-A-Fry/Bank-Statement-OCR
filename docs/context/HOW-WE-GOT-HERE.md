# How we got here -- the story and the reasoning (5 Oct 2026)

Read this after NEXT-SESSION.md when you need the WHY.

## What the tool is for
FIU analysts receive NZ bank statements (mostly PDF; some Excel/CSV; some scans)
and need every transaction as correct figures, for evidence and for Qlik. A wrong
figure is far worse than a statement sent to a person. So the one hard rule:
zero "silently wrong" (AUTO_WRONG = 0). Target: ~95% automatic.

The tool must run on locked-down Windows servers/laptops with R only: no cloud,
no installs, no statement may leave the network. That rules out AI/OCR services.

## Three generations
1. **The QVF (old Qlik app, "Statement Converter").** 11 hand-written readers,
   one per statement type (Excel: BNZ, Kiwibank, Westpac; PDF: ANZ, ANZ Loan,
   ANZ Visa, ASB Visa, BNZ Visa, Kiwibank CC, Kiwibank PDF, Westpac CC). A person
   picks the type; the script walks the PDF's words by fixed landmarks
   ("Account at a glance", "Statement period"). ~7,600 lines, ~500 per type.
   Excellent on its 11 types, blind to anything else, and it does not stop a
   wrong result (it shows a running "Balance Check" a person must notice).
   It has real bugs (loan years, keyword sign traps, Westpac commas).
2. **Statement Studio 1.x: templates.** Per-bank YAML templates (x-bands per
   column) + a wizard. Retired because templates were hand-written, the wizard
   was hard to finish, auto-picking the right template hit only 33%, and
   nothing proved a template's reading right.
3. **Statement Studio 2.0: automatic reader + proof.** Reads any statement by
   where numbers line up, then PROVES the reading with the statement's own
   arithmetic (running balance; opening + rows = closing; printed totals). Learns
   each bank's "layouts". Built and hardened over many rounds: two "silent wrong"
   attack rounds, a stress test, noise/decoy sets, a held-back acceptance run.
   On OUR synthetic test sets: ~85-95% automatic, 0 wrong.

## The turning point (5 Oct)
- The owner's real statements that were not ANZ/Westpac mostly failed (went to
  a person), and 120-page PDFs took minutes.
- Honest diagnosis: we had been marking our own homework -- the synthetic sets
  were imagined, the QVF was built on real statements.
- We mined the QVF (load script extracted from the .qvf in the repo root) with 5
  readers: each wrote a format card per type and built faithful lookalike
  statements, then scored the new tool on them. Result: ~36% automatic on the
  real types (credit cards worst, e.g. BNZ Visa 0/9), still 0 wrong.
- Causes are a handful of general gaps (listed in NEXT-SESSION.md), not words.
  The owner's doc docs/ymalchanges (big vocabulary recommendations) was read:
  useful catalogue; only ~8 items matter for correct figures; keep two word files.
- Speed: a 100-page text PDF took ~50s to read; when the first reading fails,
  up to 7 repair readings each re-read every page (why failures are slow).
  Two quick fixes landed (dictionary read once; no JIT in job processes).

## The decision: C = recipes + proof (owner chose it)
Options weighed: A keep tuning the generic reader (ceiling seen); B port the QVF
as-is (rigid, no proof, developer per new design); D cloud/LLM (not allowed).
**C:** each design = a short YAML recipe (recognise words, statement start
marker, table header words, columns hung under their headers, skip lines, date
and year rule, money style). One small reader runs any recipe (fast, no
guessing); the SAME proof gates every result, so a bank changing its design is
caught, not read wrong. The automatic reader stays as the drafter of new
recipes and the last resort. New design: the reader drafts, the person answers
plain questions on Please check (already built: one question per column, shown
with the column's own lines), an admin saves the recipe.
This is "templates again" -- on purpose -- but fixing all four reasons
templates failed (auto-drafted, plain questions, self-recognising, proven).
Day-one target: the QVF's 11 types as 11 recipes; all lookalikes automatic and
right, then the owner's real files.

## Owner's preferences learned the hard way
- Short, plain answers; "too much info" is a real complaint.
- Questions in the UI must be understandable "by a toddler" ("money out" was not).
- Worried about overfitting: fixes must be general rules; judge on unseen/real data.
- Doesn't want swarms of verifier agents; tokens are limited.
- Don't worry about Qlik dashboards matching the QVF's output columns.
- Private statements: never saved anywhere (sandbox only, delete after).

## Map of the code (what matters for C)
- R/auto_read*.R -- the automatic reader + all proof checks (.ar_pdf_checks).
- R/parse_pdf_table.R, parse_statement(), reconcile() -- the 1.x template table
  reader, still alive; recipes read through it (convert.R .override_boxes shows how).
- R/convert.R -- convert_statement (splitting via R/split.R, learning, outputs).
- R/layouts.R -- learned layouts (store roles/conventions, not column positions).
- R/words.R, dictionaries/labels.yaml, lexicon.yaml -- the words (two files, one Admin list).
- tools/synth/ -- generators and scorers (score_auto.R reader-only; score_convert.R end to end, splits bundles).
- tools/ui/check.mjs -- browser check (CHROMIUM_PATH=/opt/pw-browsers/chromium).
