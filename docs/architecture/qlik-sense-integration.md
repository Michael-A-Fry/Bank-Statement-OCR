# Qlik Sense integration - the high-trust statement feed (design)

**Status: DESIGN (approved shape, not yet built).** Scope for this round:
**extraction only**, **Qlik Sense on-prem** reading a **Windows file share**,
**high-trust / curated-template** data only. Enrichment (tax year, category, transfer
matching) is explicitly out of scope here and captured as a future option in §10.

This document is the concrete plan: the exact folder layout, the feed schema, the
"only trustworthy data reaches Qlik" gate, the Qlik load script, and the small
implementation gap to close. Nothing here changes the engine or the forensic
contract - it adds one deterministic *writer* alongside the outputs we already
produce.

> **Two Qlik modes, one engine.** There are two distinct ways Qlik meets this tool,
> and they are independent:
> - **Mode A - batch feed into dashboards** (§4-§9): converted results stream into
>   a `feed/` folder that a Qlik app loads for org-wide analysis. Analyst-initiated
>   or scheduled; many statements, one dataset.
> - **Mode B - interactive convert from a Qlik front-end** (§13): a Qlik user
>   uploads one statement and gets the one-sheet result back *in Qlik* - the exact
>   legacy `Statement Converter` experience, with our R engine swapped in for the
>   old Mole + per-bank-script extraction. This is the drop-in replacement for the
>   statement-viewer.
>
> Both share the same folder-handoff spine and the same engine; pick either or both.

---

## 1. The one-paragraph version

Every conversion already writes clean, reconciled data and a one-line run log.
We add a **feed**: when (and only when) a conversion passes a trust gate
(reconciled = `high` trust, converted by a **tested** template), the tool writes a
**flat, fully-stamped transactions CSV** into `feed/transactions/`, plus a
**one-row manifest** into `feed/runs/` for *every* run (accepted or not). Qlik
Sense points a **folder data connection** at `feed/` and wildcard-`LOAD`s those two
tables on a schedule. No API, no ports, no connector licence - the same
folder-and-permissions model the rest of the tool already uses, and the same one
your Qlik shares already run on.

---

## 2. Why this makes sense (and why it helps the Qlik renewal)

The legacy `Statement Converter 300925.qvf` did the **extraction inside Qlik**,
using a **paid PDF connector ("Mole") and hand-written per-bank scripts**. That is
the brittle, licensed part. This tool replaces *only that extraction layer* with
pure-R declarative templates - it does **not** replace the Qlik platform.

So the integration is not "tool vs Qlik". It is **"tool feeds Qlik"**:

- Qlik remains the analysis / financial-crime casework layer - dashboards,
  cross-statement views, the work analysts actually renew the licence for.
- What changes is that Qlik is now fed a **clean, licence-free, reconciled,
  auditable** dataset instead of extracting PDFs itself with a paid connector.

That *strengthens* the renewal case: Qlik keeps its value as the destination, and
the fragile/licensed extraction it used to carry moves to a maintainable tool a
single analyst owns.

---

## 3. Architecture at a glance

```
                Windows file share  \\fileserver\BankStatements\   (AD: RES_QLIKSENSE_PROD)
  ┌───────────┐        │
  │ analyst / │  drop  │  inbox/                         serve_inbox.R (Task Scheduler, ~2 min)
  │   Qlik    │───────▶│  ───────────────────────────────────────────────┐
  └───────────┘        │                                                  │  convert (pure R)
                       │  processed/ | failed/   ◀── original moved        ▼
                       │  outbox/<run_id>/       ◀── xlsx + csv + json (full, per run)
                       │  logs/runs/<run_id>.json◀── one-line audit record
                       │                                                  │  NEW: feed writer
                       │  feed/                  ◀───────────────────────┘  (gate + stamp)
                       │    transactions/<sha>.csv   high-trust rows only
                       │    runs/<run_id>.csv        one manifest row per run (all statuses)
                       │    review/<sha>.csv         withheld runs (optional, separate table)
                       │
                       ▼
               Qlik Sense folder connection "StatementFeed"  ──▶ scheduled reload ──▶ app data model
```

Everything left of `feed/` is **already built**. `feed/` and its writer are the gap
(§8).

---

## 4. Folder layout of the feed

All under the existing share, so it inherits the `RES_QLIKSENSE_PROD` permission -
no new security to set up.

| Path | Contents | Keyed by | Who reads it |
|---|---|---|---|
| `feed/transactions/<sha>.csv` | Flat, stamped transaction rows for **accepted** runs | statement **content hash** | Qlik (the dataset) |
| `feed/runs/<run_id>.csv` | **One row per run** (accepted *and* withheld) - coverage/QA | run id | Qlik (a QA table) |
| `feed/review/<sha>.csv` | Transactions for runs that **failed the gate** (optional) | content hash | Qlik (a *separate* table, never the main dataset) |

**Why keyed by content hash (`sha`), not `run_id`:** `run_id` is
`<sha256[1:10]>-<timestamp>`, so the *same* statement converted twice gets two
run_ids. Naming the transactions file by the statement's **content hash** means a
re-drop simply **overwrites** the same file - Qlik can never double-count a
statement. The runs manifest stays keyed by `run_id` (each attempt is an audit
event); take the latest per `source_sha256` in Qlik if you want one row per
statement.

**Why one file per run/statement (not one shared CSV):** appending to a single CSV
from several PCs over SMB is the one operation that genuinely interleaves and
corrupts. One-file-per-statement + Qlik wildcard-load sidesteps it entirely - the
same invariant the run logs already rely on.

---

## 5. Feed schema

### 5a. `feed/transactions/<sha>.csv` - the dataset

Flat and **self-describing**: every transaction row carries its statement/run
context, so Qlik needs no joins to know provenance, bank, or trust. Columns, in
order:

**Statement / run context (repeated on every row):**

| Column | Source | Note |
|---|---|---|
| `run_id` | run | `<sha10>-<ts>` |
| `converted_ts` | run | ISO 8601 |
| `source_file` | header | original filename |
| `source_sha256` | header | full content hash (the dedup key) |
| `bank` | template | e.g. `ANZ` |
| `statement_type` | template | e.g. `everyday` |
| `template_id` | template | e.g. `anz_everyday_pdf` |
| `template_version` | template | |
| `template_origin` | run | `default` (tested) or `user` |
| `trust_level` | reconcile | `high` for accepted rows |
| `requested_by` | run | Windows user / Qlik user |
| `period_start`,`period_end` | header | statement period |
| `account_number`,`account_name` | header | when present |

**Transaction fields (the clean display schema - no verbatim `*_raw` noise):**

`row_id, date, description, amount, debit, credit, direction, balance,
particulars, code, reference, other_party, type, currency, flags`

**Provenance:**

| Column | Source | Note |
|---|---|---|
| `source_ref` | provenance | `pdf:p3` / `csv:line=42` - the row's origin |

Notes:
- `debit`/`credit` are populated only for split money-in/out statements (blank
  otherwise); `amount`+`direction` are always populated.
- `flags` carries the honest per-row markers (`redacted`, `forced`, `no_date`,
  `date_alt_format`, `ocr_low_conf`...). Qlik can surface or filter on them.
- The verbatim `date_raw`/`amount_raw`/`balance_raw` are **not** in the feed (they
  remain in the per-run JSON + Provenance sheet for a full audit); the feed is the
  clean analyst dataset.

### 5b. `feed/runs/<run_id>.csv` - the coverage / QA manifest (one row)

Written for **every** run, so Qlik can show what came in, what was accepted, and
*why* anything was withheld - coverage is never silent.

`run_id, converted_ts, source_file, source_sha256, bank, template_id,
template_origin, status, trust_level, row_count, kpi_fail_count, period_start,
period_end, n_accounts, multiple_statements, requested_by, gate_result, feed_file`

- `gate_result`: `accepted` or `withheld:<reason>` (e.g. `withheld:needs_review`,
  `withheld:low_trust`, `withheld:user_template`).
- `feed_file`: the `transactions/<sha>.csv` written, or blank if withheld.

---

## 6. The gate - only trustworthy, guaranteed-template data reaches Qlik

This is the "high trust / guaranteed templates" idea, made concrete. A single,
readable config drives it - no code change to tune:

```yaml
# config/qlik_feed.yaml  (edit this, no code)
feed_dir: feed
require_status_ok: true            # only clean conversions
min_trust: high                    # high | medium | any
allowed_template_origins: [default]# curated/tested templates only; add 'user' to include analyst-made
template_allowlist: []             # optional: restrict to specific ids; [] = all allowed origins
include_review_feed: true          # also write withheld runs to feed/review/ (separate Qlik table)
```

A run is **accepted** into `feed/transactions/` when **all** hold:
1. `status == ok` (if `require_status_ok`), and
2. `trust_level` meets `min_trust` (reconciled: opening + every txn = printed
   closing balance = `high`), and
3. `template_origin` ∈ `allowed_template_origins`, and
4. `template_allowlist` is empty **or** `template_id` is in it.

Otherwise it is **withheld**: still logged, still in `outbox/` for a human, and its
manifest row records `withheld:<reason>`. Analysts' Qlik dataset therefore contains
**only** provably-complete extractions from **proven** bank layouts - exactly the
"guaranteed" bar. Loosening it later (e.g. accept `medium`, or a specific vetted
user template) is a one-line config edit.

---

## 7. Idempotency, concurrency, retention

- **Idempotent:** transactions file = content hash → re-drop overwrites, no
  duplicate rows in Qlik. If a statement is genuinely re-issued with different
  content, it's a different hash → a new file (correct).
- **Concurrency-safe:** one file per statement/run, never a shared append -
  matches the existing design; ten analysts + Qlik can run at once.
- **Retention:** `feed/` files roll off with the same nightly archive approach as
  `logs/runs/` (a dated archive folder); Qlik keeps loading the live `feed/`. A
  withdrawn statement = delete its `feed/transactions/<sha>.csv`; the next Qlik
  reload drops it from the model.

---

## 8. What's already built vs. the gap to close

**Already built (no work):**
- Pure-function engine; `convert_statement()` writes clean CSV/JSON/XLSX.
- `serve_inbox.R` folder poller: `inbox/` → convert → `outbox/` → `processed/|failed/`.
- Per-run audit log `logs/runs/<run_id>.json` with `status`, `trust_level`,
  `template`, `template_origin`, `row_count`, period, `requested_by`.
- Stable, flat, Qlik-friendly transactions schema (debit/credit surfaced, raw
  noise removed).

**To build (est. ~1 day, additive, fully testable):**
1. `R/feed.R` - `write_feed(parsed, recon, result, header, cfg)`:
   - always writes `feed/runs/<run_id>.csv` (manifest, with `gate_result`);
   - writes `feed/transactions/<sha>.csv` (flat, stamped) **iff** the gate passes;
   - writes `feed/review/<sha>.csv` when `include_review_feed` and withheld.
2. `config/qlik_feed.yaml` + a small loader (defaults baked in if absent).
3. Wire `write_feed()` into `serve_inbox.R` (and an optional `feed = TRUE` flag on
   `convert_document()` for the interactive app).
4. `scripts/qlik/statement_feed.qvs` - the sample load script (§9).
5. Tests: gate accept/withhold matrix, flat-schema stability (a golden feed CSV),
   idempotent overwrite on re-drop, manifest coverage for a withheld run.

No engine, template, or reconciliation code is touched.

---

## 9. The Qlik Sense side (load script)

Create a **folder data connection** named `StatementFeed` pointing at
`\\fileserver\BankStatements\feed\`. Then, in the app load script:

```qvs
// --- Statement feed: high-trust transactions --------------------------------
Transactions:
LOAD
    *,
    Year(Date)                               as [Year],
    If(Amount <= 0, 'Withdrawal', 'Deposit') as [Transaction Type]   // trivially derivable
FROM [lib://StatementFeed/transactions/*.csv]
(txt, codepage is 65001, embedded labels, delimiter is ',', msq);

// --- Coverage / QA: one row per conversion (accepted AND withheld) -----------
Runs:
LOAD *
FROM [lib://StatementFeed/runs/*.csv]
(txt, codepage is 65001, embedded labels, delimiter is ',', msq);

// Optional: latest attempt per statement, if you want one manifest row per file
// LatestRun:
// LOAD source_sha256, Max(converted_ts) as converted_ts Resident Runs
//   GROUP BY source_sha256;
```

- Schedule the reload (e.g. hourly) in the QMC; each reload picks up new
  `feed/*.csv` files.
- `Year` and `Transaction Type` are computed **in the load** - the two legacy
  fields that need no engine work - so this feed already covers the trivial slice
  of the old enrichment without moving that logic into the tool.
- `codepage 65001` = UTF-8, matching what the tool writes (verbatim descriptions
  with special characters stay intact).

---

## 10. Out of scope now: the enrichment layer (future option)

The legacy Qlik app also derived **Tax Year**, **Transaction Code**, **Transaction
Description**, **Transaction Category**, and **same-owner transfer matching** (see
`docs/legacy-qlik-mapping.md`). Per this round's decision, the feed is
**extraction-only**. When/if enrichment is wanted, there are two clean homes and
one hard dependency:

- **Home A - in Qlik's load script.** `Tax Year` and `Transaction Type` are already
  shown above. `MapSubString`-style category/description mapping fits Qlik's
  `Mapping LOAD` naturally, keeping the taxonomy in Qlik.
- **Home B - in the engine**, as a deterministic, dictionary-driven step (same
  spirit as `dictionaries/labels.yaml`), so every consumer (app, Qlik, CLI) gets
  identical categories.
- **Hard dependency either way:** the category/code **taxonomy** lived in an
  externally-maintained "Transaction Codes" spreadsheet, not in code. Enrichment
  can't be faithful without obtaining that list as data. Recommend Home A first
  (no engine change, taxonomy stays with the analysts who own it), and only move to
  Home B if multiple consumers need identical categories.

---

## 11. Forensic guarantees preserved

The feed is a *projection* of the same parsed result, so every guarantee holds:
- **Nothing guessed / enriched** - extraction only; blanks stay blank.
- **Redactions honoured** - `[REDACTED]` cells and `redacted`/`no_date` flags ride
  through into the feed; the gate does not hide them, it just requires reconciliation.
- **No silent drops** - `row_count` and `kpi_fail_count` travel in the manifest, so
  Qlik can show extracted-vs-stated coverage; withheld runs are *visible* in
  `feed/runs/`, never silently discarded.
- **Reproducible & attributable** - same statement ⇒ same `sha` ⇒ same feed file;
  every row carries `template_id`, `template_version`, `trust_level`, `requested_by`.

---

## 12. Recommended rollout

1. Approve this design.
2. Build §8 (feed writer + config + sample QVS + tests) - ~1 day.
3. Point one Qlik Sense app at a **pilot** `feed/` on the share; load the two
   tables; confirm counts reconcile against a handful of known statements.
4. Turn the gate to `min_trust: high, allowed_template_origins: [default]` for
   production; widen only as specific templates earn trust.
5. (Later, if wanted) enrichment via §10, once the taxonomy list is in hand.

---

## 13. Mode B - interactive convert from a Qlik front-end (the statement-viewer replacement)

**Goal:** keep the legacy Qlik experience *exactly* - user uploads a statement in
Qlik, it appears in a table, they pick it and hit **generate**, and an ODAG app
opens with one sheet (graphs, transactions, download) - but replace the extraction
engine (Mole + hand-written per-bank script) with this tool. **Same front-end,
different engine.**

### 13.1 The legacy flow (as it runs today)

1. **Inphinity Forms** control (`type=upload`) uploads the statement; a
   **save-and-reload** writes it and refreshes a **table** of uploaded files below.
2. The user clicks a file row, then a **generate** button (an app-navigation point)
   that fires **ODAG**, opening the *Statement Converter* app.
3. That ODAG app's **load script** did the extraction (Mole word-boxes + per-bank
   script) and drew the sheet.

Only step 3's extraction changes. Steps 1-2 (Inphinity upload, the file table, the
generate/ODAG navigation) stay as they are.

### 13.2 The one thing that changes: where the transactions come from

Instead of the ODAG app extracting the PDF itself, **our engine produces the clean
`statement.csv` and the ODAG app simply `LOAD`s it.** Two wiring options; pick by
what the Qlik server is allowed to do.

**Option B1 - async via the shared folder (recommended; works with today's
constraints).** *No R on the Qlik server, no `EXECUTE`, engine and Qlik can be on
different machines.*
- Point the **Inphinity upload target at the shared `inbox/`** folder (the same
  share, already permissioned to `RES_QLIKSENSE_PROD`).
- `serve_inbox.R` runs on **any box that has R** - reuse the **Shiny app host**,
  which already has R, so nothing new is installed. It converts each uploaded file
  to `outbox/<key>/statement.csv` + a run log.
- The Qlik **file table** is backed by a tiny **status index** (the poller writes
  `feed/index/<key>.csv`, or Qlik loads `logs/runs/*.json`) so each row shows
  `converted / pending / failed`. The user only **generate**s a converted row.
- **ODAG** then loads `outbox/<key>/statement.csv` - a plain `LOAD`, no extraction
  in Qlik. This fits the existing **two-step** UX perfectly: the upload+reload step
  kicks off conversion; by the time the user picks a file and hits generate, the
  CSV is ready.

```qvs
// ODAG Statement Converter app - load script (Option B1)
// $(odagKey) is bound from the selected file row (the statement's key/hash).
Statement:
LOAD *
FROM [lib://StatementInbox/outbox/$(odagKey)/statement.csv]
(txt, codepage is 65001, embedded labels, delimiter is ',', msq);
// ... then the existing sheet: KPIs, in/out graph, balance line, table, download.
```

**Option B2 - synchronous via `EXECUTE` (one click, later optimisation).** *Only if
R is installed on the Qlik node and `Allow Execute` (standard-mode override) is
enabled in the QMC.*
- The ODAG template app's load script runs the converter inline, then loads it, in
  one reload - collapsing upload and convert into a single generate click:

```qvs
// ODAG app load script (Option B2) - requires R on the Qlik node + Allow Execute
EXECUTE Rscript "D:\BankStatements\run.R" "$(uploadedFile)" "D:\BankStatements\outbox\$(odagKey)";
Statement:
LOAD * FROM [lib://.../outbox/$(odagKey)/statement.csv] (txt, ..., msq);
```

**Option B3 - Rserve / SSE analytic connection (Qlik's sanctioned R channel).**
*R on the Qlik node; **no** `Allow Execute` needed.*
- Qlik's documented R integration is a **Server-Side Extension (SSE)**: an
  **Analytic Connection** (configured in the QMC) points at an **Rserve** instance
  through the open-source **SSE-to-Rserve** connector. Load script / chart
  expressions then call `<Conn>.ScriptEval(...)`, `<Conn>.ScriptEvalStr(...)`, etc.
- Because it's a bounded, supported channel, it typically clears enterprise
  security review where `EXECUTE` (arbitrary program execution) does not - so if
  your Qlik admins lock down standard mode, **this is the in-reload path**.
- **Fit caveat:** SSE is built to exchange *data columns*, not to hand R a file and
  get a fresh table back. It's workable - pass the uploaded file's **path** as a
  string, have the R function convert it, **write the audit artifacts to disk**
  (csv/xlsx/json, same as always) and return the transactions table (or just the
  `run_id`, and let ODAG `LOAD` the CSV) - but it is more moving parts (Rserve
  service + SSE connector + connection config) than B1/B2 for a file->file job.

```qvs
// ODAG app load script (Option B3) - Analytic Connection "R" -> Rserve
// R side: a thin wrapper converts the path, writes out/<run_id>/, returns the table.
Statement:
LOAD *
EXTENSION R.ScriptEval('convert_statement_sse(path)', (FileList){[path]});
// (illustrative; exact call shape depends on the SSE-R connector's table mode)
```

### 13.2a Which invocation to pick

All three land in the same place - Qlik shows a clean transactions table and the
artifacts are on disk for audit - so choose by **what the Qlik server permits**, not
by preference:

| If the Qlik admin... | Use | Why |
|---|---|---|
| allows the standard-mode `Allow Execute` override | **B2 `EXECUTE`** | simplest, one click, matches our CLI (`run.R`) directly |
| forbids `EXECUTE` but allows an Analytic Connection | **B3 Rserve/SSE** | the sanctioned in-reload R channel; no execute override |
| wants no R invoked from Qlik at all | **B1 poller** | R as a scheduled task writing to the shared folder; fully decoupled |

**Forensic key point:** we persist the outputs to disk **regardless** (they *are* the
audit record) and Qlik `LOAD`s them, so Rserve's in-memory return buys little here -
its only real advantage over `EXECUTE` is not needing the execute override. Start
with whichever your admin already allows.

### 13.3 Where R runs

R is installed on the **Qlik server** for production (the laptop-local setup today is
just development). That is what enables the synchronous options (B2/B3) - one click,
convert-and-load in a single ODAG reload. The folder handoff still works
**cross-machine** if you ever prefer the converter on a separate box (e.g. the Shiny
host): the engine only needs to see the shared `inbox/`/`outbox/` folder, so R can
live on the Qlik node, the Shiny host, or its own small VM without changing anything
else.

### 13.4 Keying & idempotency

Key each upload by the statement's **content hash** (or reuse Inphinity's own file
id), and name `outbox/<key>/` by it. Re-generating the same file re-uses the same
output (no duplicates), and the file-table row maps 1:1 to its ODAG output.

### 13.5 What you can test now (all local, before any server work)

The **engine half is fully testable today**, no Qlik needed:
```sh
# drop a statement in inbox/, run the poller once, check the output
cp mystatement.pdf inbox/
Rscript scripts/serve_inbox.R
ls outbox/mystatement/            # -> statement.csv (+ xlsx, json)
```
When the server is stood up, the only Qlik-side wiring is: (a) Inphinity upload
writes to `inbox/`, (b) the file table reads the status index, (c) the ODAG app
`LOAD`s `outbox/<key>/statement.csv`. None of it needs R on the Qlik box.

### 13.6 Privacy note (Mode B)

Isolation is preserved: each user only ever sees the ODAG app generated for the
file **they** selected; the shared `inbox/`/`outbox/` folders are reachable only via
the permissioned share and the Admin tab, never by another Qlik user through the
front-end. Retention of the uploaded statement follows the same policy chosen for
the Shiny app.

### 13.7 Open items to confirm with IT

- **Which R-invocation channel is permitted on the Qlik server** (picks B2 vs B3,
  see §13.2a): is the standard-mode **`Allow Execute`** override allowed, or must R
  go through an **Analytic Connection** (Rserve + SSE)? Both work - it's a
  security-policy question for the Qlik admins, and the answer decides the shape.
- **How Inphinity Forms delivers the upload:** can it write to a **file-share
  folder** (what the poller / load script read)? If it only writes to a DB, add a
  one-line export of that blob to the shared `inbox/`.
- **Rserve/SSE only:** confirm the **SSE-to-Rserve** connector is installed and an
  **Analytic Connection** is registered in the QMC (name, host, port 6311 default).

---

## 14. Proven-templates-only + the Shiny escape hatch (BUILT)

**Rule:** the Qlik path converts with **proven (curated) templates only** - it never
touches analyst-made drafts. If a statement matches no proven template, Qlik does
**not** try to build one; it tells the user and points them at the full Shiny app,
and files the statement into the team's pickup queue so it gets templated. **The
Shiny app is unchanged** - it still offers every template and the full build flow;
only the Qlik entrypoint is restricted.

This is implemented, tested, and config-driven:

- **Entrypoints** (all use proven templates only - `templates/`, never
  `templates_user/`):
  - `convert_for_qlik(path, outdir)` (`R/qlik_convert.R`) - converts and writes the
    outputs **plus `outdir/status.json`**.
  - `scripts/convert_for_qlik.R <file> <outdir>` - the CLI for Qlik `EXECUTE` / the
    poller.
  - `convert_statement_sse(path)` - the Rserve/SSE wrapper (returns the table).
- **`status.json`** is what the Qlik sheet branches on:

```json
{ "status": "ok",           "needs_template": false, "csv": ".../bnz.csv",
  "run_id": "…", "template_id": "bnz_everyday_csv", "trust_level": "high",
  "row_count": 32, "message": "…", "shiny_url": "http://…:8100" }
```
```json
{ "status": "unsupported",  "needs_template": true,  "csv": null,
  "message": "No proven template for this bank yet. Set it up in the full app…",
  "shiny_url": "http://…:8100" }
```

- **On a match:** ODAG `LOAD`s the `csv`. **On a miss:** the Qlik sheet shows
  `message` and a button opening `shiny_url`; meanwhile the statement is already in
  the Shiny **Admin -> Uploads** pickup queue (auto "reach out to us"), so the team
  builds the template and, once it lands in `templates/`, Qlik converts that bank
  from then on. No re-upload, no dead end.

### 14.1 How a new bank becomes available to Qlik

Building happens in Shiny (unchanged). A template is "proven" simply by living in
the **`templates/` folder** (`paths.templates`) that Qlik reads - the curated,
golden-file-backed set. Promoting a vetted draft = placing its YAML there (the
existing curation step); nothing in Shiny changes and Qlik picks it up on next use.

---

## 15. Configuration - one file (`config/config.yaml`)

All deployment settings live in **`config/config.yaml`** (copy it from the committed
**`config/config.example.yaml`**; the real file is git-ignored so the admin password
is never committed). Absent keys fall back to built-in defaults, so a partial file is
fine. Loaded once by `load_config()` (`R/config.R`); the Shiny app, the poller and the
Qlik entrypoints all read the same file.

Keys that matter here:
- `app.admin_password` - the Admin-tab gate (a simple shared barrier). `BSO_ADMIN_PASSWORD`
  env var overrides it if a site prefers to keep it out of the file.
- `app.shiny_url` - where the Qlik "no proven template yet" button sends the user.
- `paths.templates` - the **proven** set Qlik reads; `paths.user_templates` - drafts,
  **Shiny only**.
- `qlik.queue_unsupported` - file a Qlik miss into the Shiny pickup queue (default on).
- `feed.*` - the Mode A batch-feed gate (§6).
