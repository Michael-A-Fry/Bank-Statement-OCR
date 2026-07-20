# Private staging — drop real statements here (NEVER committed)

This folder is **git-ignored**. Anything you put here stays on this machine and
is never pushed to GitHub. Only this README is tracked, so the folder exists and
explains itself.

Use it for **real / personal bank statements** you want tested against the
engine but must not publish — e.g. your own ANZ debit/everyday statements.

## How to test a file dropped here
```sh
# from the repo root:
Rscript run.R samples/_private_staging/your_statement.pdf ANZ out_private
# outputs land in out_private/ (also git-ignored)
```
Or open the Shiny app and upload it on the **Convert** tab.

## Safety
- `.gitignore` ignores `samples/_private_staging/*` (this README excepted).
- If you're ever unsure, run `git status` — your files must **not** appear.
- Nothing in here is read by the test suite or committed by any workflow.
