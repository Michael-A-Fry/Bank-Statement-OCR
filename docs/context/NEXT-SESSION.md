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
- main = 8b1ae4b: words screen (one list + clash guard + teach from Please check),
  plain toddler-clear column questions on Please check, speed fixes. Suite 6,645/0/0, browser 125/125.
- claude/bank-statement-ocr-platform-t6n934 = 36d2209 = main + .xls support (related tests green; full suite not yet run).
- wip/recipes: recipe reader (R/recipes.R, recipes/anz_everyday_pdf.yaml + anz_loan_pdf.yaml). MEASURED: own tests 122/0; ANZ lookalikes 14/15 auto_right, 0 wrong (was 8/15). Not yet: full suite, zoo regression. See RECIPES-STATUS.md there.
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
2. [ ] Checkout wip/recipes, rebase on the work branch, run tests/testthat/test-recipes.R + test-auto-read.R + test-convert.R. Fix until green.
3. [ ] Score the ANZ + ANZ Loan lookalikes with recipes; zoo dev/corpus/offsweep must not drop (dev pdf 115/128, corpus 30/43, offsweep 22/26, AUTO_WRONG 0).
4. [ ] Full suite; merge recipes into the work branch; push; push main if green.
5. [ ] Write recipes for: kiwibank_pdf, westpac_cc, bnz_visa, kiwibank_cc, anz_visa, asb_visa (one per commit, measured on its lookalikes).
6. [ ] Excel types: bnz/kiwibank/westpac excel recipes (+ OD in number format, "." zero, split by account column).
7. [ ] Owner's real files (owner uploads; sandbox only, delete after).
8. [ ] Please check answers -> save a draft recipe for admin to accept.
9. [ ] wip/admin-review: finish, test, merge.
10. [ ] Account number + name into feed. Then full code check (task: untested paths).
