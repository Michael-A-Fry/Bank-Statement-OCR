# How it fits together — one statement, upload to dashboard

The map for whoever inherits this. One page: what happens to a file, which module
owns each step, where the words on screen live, and where you change each kind of
thing. Everything here is a pointer — the detail is in the file named.

---

## The journey

```
  a file            app.R (Convert)
     |              R/uploads.R          keep a copy, so a failed format can be picked up
     v
  READ              R/read_input.R  ->  read_pdf / read_delimited / ocr(+ocr_preprocess)
     |                                  one shape whatever the format: pages, words, table
     v
  RECOGNISE         R/detect.R          which template? (fingerprint phrases + columns)
     |                                  no match -> status "unsupported", and the screen
     |                                  offers the toolkit instead of a dead end
     v
  EXTRACT           R/parse.R           delimited / excel
     |              R/parse_pdf_table.R  pdf geometry (the long one; read its phase notes)
     |              R/normalise.R        dates + amounts, format-only, per the template
     |              R/split.R            optional: a bundle -> one statement per segment
     v
  PROVE             R/reconcile.R       the checks + the trust level  <- the heart of it
     |              R/diagnose.R        for anything not clean: where / why / how to fix
     |              R/coverage.R        which fields were populated / empty / absent
     v
  EMIT              R/outputs.R         xlsx + csv + json (byte-reproducible)
     |              R/logging.R         one run record per conversion
     |              R/metadata_capture.R local-only "how did it go" record, never fed
     v
  PUBLISH           R/feed.R            the governed gate -> Qlik, or held back with a reason
```

`R/convert.R` is the orchestrator that runs that column top to bottom and never
throws: every failure becomes a `failed` result with an actionable message.
`convert_document()` is the front door — it tries the statement pipeline, then
falls back to the FORM pipeline (`R/forms.R`, `R/extract_fields.R`) for a labelled
-value document like an IRD form.

---

## Who owns what

| Step | Module | The one thing to know |
|---|---|---|
| Read any format | `R/read_input.R` | Everything downstream sees one shape. Add a format here, not in the parsers. |
| Scanned pages | `R/ocr.R`, `R/ocr_preprocess.R` | OCR pages are never rated "high" confidence, by design. |
| Which bank | `R/detect.R` | Fingerprint phrases are matched **character for character**. That is the #1 reason a new template "doesn't work". |
| PDF tables | `R/parse_pdf_table.R` | Rows are found by their DATE. The keep-rule (`pdf_keep_row`) is shared with *See it on the page* (`R/inspect.R`) and `R/row_coverage.R`, so the screen cannot contradict the reader. |
| The checks | `R/reconcile.R` | One `.kpi_*()` builder per check; `reconcile()` is the list, in report order. |
| Explaining a bad run | `R/diagnose.R` | `.DIAG_FIX_OWNER` says WHO fixes each category (you / the file / a developer). |
| Downloads | `R/outputs.R` | Same table for the workbook and the CSV, built once. |
| Dashboards | `R/feed.R` | `.feed_gate()` is machine-only: no human can wave a conversion through. |
| Settings | `R/config.R` | Every deployment setting; an absent key falls back to the built-in default. |
| Tuning numbers | `R/params.R` | Every threshold, named once. Catalogue: [engine-parameters.md](engine-parameters.md). |

---

## Where the words live

The engine emits **codes**; the screen shows **sentences**. Keeping those apart is
why a reviewer never reads `needs_review` or `balance_reconciliation`.

| What | Where | Notes |
|---|---|---|
| Every label a non-technical user reads | `ui_labels.R` | `STATUS_PLAIN`, `CHECK_PLAIN`, `DIAG_PLAIN`, `COVERAGE_PLAIN`, `FEED_PLAIN`, column headers. Reword freely — it is copy, not logic. |
| The About page + the in-app 2-minute guide | `ui_content.R` | The only how-to a user has; they cannot see `docs/`. |
| Screen layout and every control | `app.R` | Design tokens (colour, radius, shadow) are in the single `:root` block. |
| How-to-fix advice, KPI details, "who fixes this" | `R/diagnose.R`, `R/reconcile.R` | The engine writes these because only it knows the figures. |
| Statement vocabulary the engine looks for | `dictionaries/labels.yaml`, `dictionaries/lexicon.yaml` | Editable in Admin without YAML; the files keep their comments when it does. |

**The seam is tested.** `tests/testthat/test-seams.R` asserts that every check,
every diagnostic category, every status and every feed verdict the engine can emit
has wording on the other side — and that there are no dead entries. Add a KPI
without a `CHECK_PLAIN` line and the suite fails, instead of a forensic reviewer
being shown `amount_direction`.

---

## Where you change each kind of thing

| I want to… | Change | Who |
|---|---|---|
| Add a bank | a YAML template | analyst, in the app — **Add a template**. Never code. |
| Teach a wording ("balance at start" = opening balance) | `dictionaries/labels.yaml` | admin — **Admin → Templates → Label dictionary**, plain English |
| Teach a marker word ("cow" means money out) | `dictionaries/lexicon.yaml` | admin — **Admin → Data capture → Words the tool knows to look for** |
| Reword anything on screen | `ui_labels.R` / `ui_content.R` | analyst with an editor |
| Add a check | `R/reconcile.R` (builder + the list), `CHECK_PLAIN`, `.KPI_DIAGNOSIS` | maintainer — the file head lists all four |
| Move a threshold | `R/params.R` | maintainer → [engine-parameters.md](engine-parameters.md) |
| Change what reaches Qlik | `config/config.yaml` → `feed:` | admin → [connecting-qlik.md](../operational/connecting-qlik.md) |
| Change a deployment setting | `config/config.yaml` | admin — copy from `config.example.yaml` |

---

## Ten people at once

There is no lock, no queue, no database and nothing to tune, because **nothing is
ever appended to a shared file.**

- Each conversion writes its outputs into its own `tempfile()`-unique directory.
- Each writes **one** run record, `logs/runs/<run_id>.json`. A clashing id gets a
  `~2` suffix rather than overwriting — a destroyed audit record is the cardinal
  failure.
- Each feedback submission writes one file under `logs/feedback/`.
- The feed writes one content-hash-keyed file per statement, atomically (temp file
  in the same directory, then rename), so a Qlik reload can never read a
  half-written CSV.

Appending to one shared log from several machines over SMB is the single
operation that genuinely can interleave and corrupt; one-file-per-event sidesteps
it. Shiny sessions are isolated, so nobody sees another person's upload or
result. The one deliberately shared resource is the template library — a team
asset, not user data.

## Three rules the code is built around

1. **Never silently wrong.** A check that could not run reports *could not be
   checked*, never a green tick — and a check that is a **count** rather than a
   verdict reports *for information*, so an absence of proof and a successful
   count are never worded the same. A figure that cannot be read is `NA` plus a
   flag, never a plausible guess. When the engine refuses, it says so on screen — including the
   quiet refusals: rows held back from the dashboards, a redaction scan that could
   not finish, a year that was inferred rather than printed.
2. **A new bank is a template, never new code.** If a change would make adding a
   bank require an edit in `R/`, it is the wrong change.
3. **Deterministic.** Same input + same template + same build = the same bytes
   out. The outputs carry the engine version and a hash of the template that
   produced them, so a figure can be reproduced years later — and, because those
   stamps live inside the file, so that a comparison across two builds is read on
   the figures rather than on the bytes.

Full statements of these: [charter.md](charter.md).

---

## If something is wrong

| Symptom | Look at |
|---|---|
| A conversion is wrong or won't convert | [when-something-goes-wrong.md](../operational/when-something-goes-wrong.md) |
| A bank has no template | [adding-a-bank-template.md](../operational/adding-a-bank-template.md) |
| A dashboard is missing rows | `feed/runs/` (every conversion, accepted or withheld, with its reason), then [connecting-qlik.md](../operational/connecting-qlik.md) |
| The tool won't start / a setting isn't applying | [running-and-keeping-it-up.md](../operational/running-and-keeping-it-up.md) |
| You are changing the engine | [maintaining-the-engine.md](../operational/maintaining-the-engine.md) |
