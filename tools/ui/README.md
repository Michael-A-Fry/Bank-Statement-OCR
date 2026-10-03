# Pressing the buttons

`check.mjs` drives the real app in a real browser and **fails** (exit code 1) when
a screen does not do what it says. It starts the app itself on a throwaway config
(nothing learned, nothing written into a real install), and tours every screen:
the Convert table with each file's bank filled in, changed and named; a case
converted with its progress in the table and every outcome, fitting its panel from
desktop to tablet width; Please check on a spreadsheet (Re-read wrong, Undo, Re-read
right, This is right) and on a PDF (the page, its ticks, a column drawn in the
editor); Download everything; a single file whose bank the statement disputes;
scans; Stop; Admin -> Banks (confirm, rename, retire, a held fix, training a bank
with another bank's statement in the pile), Automatic reading (the spot-check rate,
a spot check answered, the carry-off summary), Words and Health -- each at phone
width too. Last, it reads the app's own console: an R error or warning there fails
the run even when every screen looked right.

```
cd tools/ui
npm install                     # once, on a machine with internet
npx playwright install chromium # once, if this machine has no Chromium for Playwright
node check.mjs                  # about five minutes; screenshots land in tools/ui/out/
```

| Environment | Meaning |
|---|---|
| `PORT=7911` | the port it starts the app on |
| `APP_URL=http://127.0.0.1:8100/` | check an app that is **already running** instead |
| `CHROMIUM_PATH=/path/to/chrome` | use this Chromium rather than Playwright's |
| `OUT=dir` | where the screenshots go |

## Why it exists

The R suite (`tests/run_tests.R`) reads `app.R` as text. It can prove a line is
there; it cannot prove the screen works. Every change to the Convert table was
proven by a browser drive like this one, and those drives lived only in the session
that wrote them. This keeps them.

Run it after any change to `app.R`, `www/app.css`, `ui_labels.R` or `R/identify.R`,
alongside the suite. A failure prints what it got and what it wanted.

## Never shipped

`tools/` is not in the offline bundle — `scripts/bundle-offline.R` copies an
explicit list of folders — and the server has no Node. This is a developer's tool.
