# NEXT SESSION -- start here (written 5 Oct 2026)

Paste to the new session: "Read docs/context/NEXT-SESSION.md on branch
wip/qvf-kit and do the next unchecked step. One step per commit; push after each."

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

## Steps (tick when done)
0. [ ] Read docs/context/qvf/FINDINGS.md (fix list + baselines).
1. [ ] Regenerate lookalikes: for g in docs/context/qvf/gen/make_*.py: python3 $g (check each file's --out / output dir; sets went to scratchpad/qvf/sets/<area>). Baseline-score with tools/synth/score_convert.R --mode cold.
2. [x] DONE: wip/recipes rebased, own tests 122/0.
3. [x] DONE: ANZ+ANZ Loan lookalikes 14/15 automatic (was 8/15), 0 wrong; zoo dev/corpus/offsweep unchanged.
4. [x] DONE: full suite green, merged to work branch and main (3c61147). Next: find why the 1 remaining ANZ lookalike goes to a person (score_convert --out to see which).
5. [ ] Write recipes for: kiwibank_pdf, westpac_cc, bnz_visa, kiwibank_cc, anz_visa, asb_visa (one per commit, measured on its lookalikes).
6. [ ] Excel types: bnz/kiwibank/westpac excel recipes (+ OD in number format, "." zero, split by account column).
7. [ ] Owner's real files (owner uploads; sandbox only, delete after).
8. [ ] Please check answers -> save a draft recipe for admin to accept.
9. [ ] wip/admin-review: finish, test, merge.
10. [ ] Account number + name into feed. Then full code check (task: untested paths).

## If the owner pastes a "STATEMENT DESIGN" block
It came from docs/context/recipe-intake-prompt.md (on main): Copilot describing a real statement with nothing personal. Write recipes/<bank>_<product>_<kind>.yaml from it, following recipes/anz_everyday_pdf.yaml; build a lookalike from its SAMPLE (tools/synth/make_layouts.py helpers) and prove the recipe reads it automatically with 0 wrong before committing.
