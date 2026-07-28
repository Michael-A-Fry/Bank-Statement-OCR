# Admin and maintenance

Admin is where you keep an eye on the tool, teach it new wordings, manage
templates and tidy up. Everything is point-and-click; there is no database.

Day-to-day upkeep is here. Running the test suite, re-applying an engine
parameter and promoting a template to proven are in
[maintaining-the-engine.md](maintaining-the-engine.md).

## Getting in

Add **`?admin`** to the address:

```
http://your-server:8100/?admin
```

Bookmark it. Without it the tab is not offered — nobody converting a statement
has the password, and a tab they cannot open only invites them to try. That is
not the lock; the password is, and every admin action is re-checked on the server
regardless of what the browser was shown.

The password is `app.admin_password` in `config\config.yaml` (or the
`BSO_ADMIN_PASSWORD` environment variable, which wins). **Until it is changed
from the shipped placeholder, Admin refuses to open for anybody** — see
[first-time-setup.md](first-time-setup.md) §4. Wrong passwords back off: free for
the first three tries, then a doubling wait up to five minutes.

## The four sub-tabs

### Insights
A live picture from the run and feedback logs. Click **Refresh from logs** first
— it re-reads the folder, and takes a few seconds on a busy box.

- **Uploads — every statement converted here, newest first**: one row per upload,
  whatever became of it. It is the route in when you know the file or roughly when
  it was converted, which is where
  [investigating-a-wrong-conversion.md](investigating-a-wrong-conversion.md) §1
  sends you. The `needs_pickup` column marks the subset that matters for setting
  up a new bank — a layout nobody has a template for — and the picker beside the
  table offers only those. *Download its safe summary* gives a shapes-only
  markdown description with no client data in it, safe to send on. (Select an
  upload first — with nothing selected the download still fires and produces an
  error file.)
- **Format requests**: raised by users from a *no template yet* result. **Mark
  done** updates the request file on disk; it does so without saying anything on
  screen, so re-check the list rather than waiting for a confirmation.
- **Conversions by status**, **feedback flagged as wrong**, **the gaps to fill**
  (unrecognised layouts, biggest first — build a template for the top one),
  **templates that started failing recently** (drift), **template usage**.
- **Tidy up logs** rolls run and feedback records older than the retention window
  into `logs\archive\<kind>-<year>.jsonl`. Every line is kept — nothing is
  deleted, and `logs\metadata\` is never touched. The message only counts the
  *run* files it archived, so a zero there does not mean nothing happened.
- **Saved statements — retention**: `uploads\` holds real client statements. The
  red button deletes the ones past `retention.uploads_keep_days` **immediately,
  with no confirmation step and no undo.** The same tidy-up also runs at startup.

### Templates
Every layout the tool can read: *tested* = shipped and checked, *user* = built
here. Click a row to load its YAML.

- **Check it's valid** parses and validates without saving.
- **Duplicate** copies it under a new id into the editor. Nothing is written until
  you Save.
- **Hide** parks a user template out of detection without deleting it.
- **Delete** removes user templates only; a shipped template cannot be deleted
  here, and there is no confirmation step, so read the id first.
- **Near-duplicate user templates** lists variants worth consolidating.
- **Label dictionary** — see below.

### Data capture
- **Local metadata capture**: level (Full / Standard / Off) and per-category
  switches. Written to `logs\metadata\`, kept forever, never in the Qlik feed. No
  statement content is stored; an account number is stored only as a one-way
  hash. Detail: [../context/metadata-capture.md](../context/metadata-capture.md).
- **Words the tool knows to look for** — the lexicon, see below.
- **Words your statements used that the tool didn't recognise** — the suggestion
  queue. It is fed by the `novelty` metadata category, so it stays empty if
  metadata capture is off or that category is unticked, and it only fills once
  conversions have actually met an unrecognised marker.
- **Columns in your statements that no template uses** — a column that keeps
  turning up unused is usually a field worth mapping in that bank's template.
  Expect noise from files that were never statements.

### Batch & audit
Point it at a pile of statements and get one picture: what converts, the gap
layouts biggest-first, and ready-to-edit draft templates for them. Safe to share
— only shapes and counts, never contents.

The two report downloads only work **after** a run, and the CSV only if you
ticked **convert & save**. Clicking either before that returns an error page.

## Teaching it new words

Almost everything you would want to change is data, not code, and there are three
tiers with a clear precedence: **template > lexicon > built-in default.**

| Tier | Holds | Where | Edited in |
|---|---|---|---|
| **Template** | facts about one bank — its columns, date format, fingerprint, its debit marker | `templates\*.yaml` | the toolkit ([adding-a-bank-template.md](adding-a-bank-template.md)) |
| **Lexicon** | the generic vocabulary the engine *tries* when detecting, drafting and parsing — debit/credit markers, money and date shapes, summary-line labels, redaction markers | `dictionaries\lexicon.yaml` | **Data capture → Words the tool knows to look for** |
| **Label dictionary** | the wordings for single labelled values — "opening balance" vs "balance brought forward" vs "starting balance" | `dictionaries\labels.yaml` | **Templates → Label dictionary** |
| **Built-in defaults** | what ships in code; the lexicon falls back to these | `R/lexicon.R` | maintainer only |

`config\config.yaml` is **not** a customisation tier — it holds deployment
switches, never vocabulary.

**Which one do I want?** If a bank writes `Paid`/`Recd` where others write
`D`/`C`: pin it on that one template if it is only this bank; add it to the
lexicon if you want the engine to recognise it everywhere, forever. One lexicon
edit plumbs through detection, drafting and parsing at once. Word lists **add**
to the built-ins (you keep `D`, `DR`, …); a regex **replaces** one, and is
rejected if it will not compile.

Both dictionaries are **your** state, not shipped files. An update never
overwrites them ([updating.md](updating.md)); each save first writes the previous
contents beside it as `….yaml.bak`; and they are on the short list to copy off
the box ([backup-and-restore.md](backup-and-restore.md)).

### How a suggestion becomes a rule

Nothing is learned automatically. Every conversion records the markers and
columns it did **not** recognise; those are counted across the whole metadata
corpus and offered in Data capture, most frequent first; a human picks one, says
which way the money goes, and approves it; the next conversion reads the approved
vocabulary. The engine never changes its own behaviour — approval is the boundary
between a suggestion and a rule, and it is what keeps "never silently wrong"
intact while the tool learns.

## Feedback closes the loop

Anyone can rate a result (correct / minor issues / wrong) with a comment. Ratings
show up in Insights, tagged when the run was not clean. Marking a result **wrong**
also withdraws its rows from the dashboards immediately.

## Routine upkeep

- **Watch for drift.** More failures or low-confidence runs for one bank usually
  means its export format changed — open the toolkit and adjust the template.
- **Work the gaps list.** The top row is the layout blocking the most statements.
- **Clear the format requests.** Most are a template tweak you can do yourself.
- **Tidy up logs** every so often.
- **Back up what cannot be rebuilt** — templates, dictionaries, `logs\metadata\`.
- **Run the test suite after any engine change and after every update** —
  [maintaining-the-engine.md](maintaining-the-engine.md).
