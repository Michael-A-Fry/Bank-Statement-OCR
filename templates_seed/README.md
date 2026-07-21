# templates_seed — starting points for the launch template suite

These are **skeletons, not finished templates.** Each one carries what we already
know for that bank layout from the legacy tool — the format, date format, amount
style, which columns exist, and whether balances sit inside the table or need
pinning — so the only thing left to do is set the exact positions against a real
statement.

**This folder is NOT loaded by the app.** The app only reads `templates/` (shipped)
and `templates_user/` (built on the box). Nothing in here affects detection or
conversion until you finish it and move it across.

## How to finish one

1. Get one real statement of that type. Keep it in `samples/_private_staging/`
   (git-ignored — real statements never get committed).
2. Open it in Convert. When it doesn't match, the template toolkit opens.
3. Copy this skeleton's known settings in (or load it), then **draw the column
   bands** on the page. For a credit card, **pin the opening/closing balance** with
   a drawn box (the toolkit's "pin box → value"). Use **"this IS a transaction"**
   in the X-ray to catch any row the reader skipped.
4. Confirm the balance reconciles, then **Save**. It lands in `templates_user/`.
5. To ship it to the whole team, move the saved file into `templates/` and add a
   golden-file test (see `tests/HOWTO-add-template-test.md`).

Every coordinate below marked `# TODO` is a placeholder — replace it by drawing on
a real statement. The fingerprint phrases are best guesses from the legacy parser;
confirm them against a real statement so detection is reliable.
