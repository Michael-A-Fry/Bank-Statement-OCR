# Pressing the buttons

`check.mjs` drives the real app in a real browser and **fails** (exit code 1) when
the Convert screen does not do what it says. It starts the app itself, uploads six
files from four banks, changes two templates, converts, clicks through, converts
again, downloads everything, does a single file, and checks a phone-width screen.

```
cd tools/ui
npm install                     # once, on a machine with internet
npx playwright install chromium # once, if this machine has no Chromium for Playwright
node check.mjs                  # about two minutes; screenshots land in tools/ui/out/
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

Run it after any change to `app.R`, `www/app.css` or `R/identify.R`, alongside the
suite. A failure prints what it got and what it wanted.

## Never shipped

`tools/` is not in the offline bundle — `scripts/bundle-offline.R` copies an
explicit list of folders — and the server has no Node. This is a developer's tool.
