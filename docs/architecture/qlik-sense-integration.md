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
