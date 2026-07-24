# Qlik Sense integration - Shiny converts, Qlik analyses

**The architecture:** accountants convert in the **Shiny app** (opened from a tile in
Qlik); each conversion writes to an **analytics feed** that a **Qlik folder
connection + scheduled reload** turns into dashboards / casework. Qlik is the front
door and the analytics layer; the R engine does the conversion. Qlik never parses a
statement (it can't) - it loads clean, reconciled, licence-free data.

Why this shape (vs. converting inside Qlik) is in
[`qlik-options-analysis.md`](qlik-options-analysis.md). Step-by-step wiring is in
[`../../operational/connecting-qlik.md`](../../operational/connecting-qlik.md).

---

## 1. The flow

```
  ┌──────────┐   opens (tile/link, same AD group)   ┌───────────────────────┐
  │  Qlik    │ ───────────────────────────────────▶ │  Statement Studio      │
  │  hub     │                                       │  (Shiny)               │
  │          │                                       │  upload → audited      │
  │  [tile]  │                                       │  template → convert →  │
  │          │                                       │  download              │
  └──────────┘                                       └───────────┬───────────┘
       ▲                                                          │ write_feed() on each
       │  folder connection + scheduled reload                    │ conversion (gated)
       │                                                          ▼
  ┌────┴───────────────────────────────────────────────  feed/  (on the share)
  │  Qlik analytics app                                    transactions/<hash>.csv   accepted rows
  │  (dashboards, cross-statement casework)                runs/<hash>.csv           one row per statement
  └───────────────────────────────────────────────────    review/<hash>.csv         withheld (optional)
```

1. **Qlik opens the app.** A tile/button in a Qlik app opens Statement Studio (§5).
   Same AD-group gate that protects your Qlik shares.
2. **Accountant converts in Shiny.** Upload -> pick an **audited** template (or
   auto-detect) -> convert -> download. Sessions are isolated; nobody sees another's
   statement.
3. **The feed fills itself.** Each conversion calls `write_feed()` as a silent
   side-effect; if it clears the gate (§3) it lands in `feed/transactions/`.
4. **Qlik loads the feed** on a schedule (§4) -> dashboards.

---

## 2. Feed folder layout (on the share)

Under the share, inheriting the same AD-group permission.

| Path | Contents | Keyed by | Qlik reads |
|---|---|---|---|
| `feed/transactions/<hash>.csv` | Flat, stamped transaction rows for **accepted** conversions | statement **content hash** | the dataset |
| `feed/runs/<hash>.csv` | **One manifest row per statement** (accepted *and* withheld) | statement **content hash** | a QA/coverage table |
| `feed/review/<hash>.csv` | Transactions for **withheld** conversions (optional) | statement **content hash** | a *separate* review table |

All three are keyed by the statement's **content hash** (`substr(sha256, 1, 16)`),
so a re-convert **overwrites** that statement's file in each folder (idempotent,
latest attempt wins) and two different statements never collide - the `runs`
manifest therefore counts each statement once, not once per re-run (the `run_id`
stays a column so the latest attempt is still identifiable). One file per statement
- **never a shared append** - so any number of simultaneous conversions produce
independent files (§6).

---

## 3. The governance gate (what reaches the dashboards)

`write_feed()` (`R/feed.R`) writes to `feed/transactions/` **only when all hold**
(config-driven, `feed.*`):

1. `status == ok` (a clean conversion) - `require_status_ok`,
2. trust meets `min_trust` - default `medium` = every clean conversion; set `high` to
   accept only balance-proven ones (opening + every txn = printed closing balance).
   A clean statement with no running balance is `medium`, so `high` would withhold it
   - hence `medium` is the sensible default for a useful dashboard,
3. the template is **proven** - one of the curated `templates/` set
   (`allowed_template_origins: [default]`; add `user` to include analyst drafts),
4. `template_allowlist` empty, or the template is in it.

Otherwise it's **withheld** (still logged in `runs/`, optionally copied to
`review/`). So the dashboards only ever contain clean extractions from vetted
templates - even though the Shiny converter offers proven templates by default and
user-created ones via an opt-in tick-box (`app.user_templates_default` sets the
box's default).

## 3a. Feed schema

**`transactions/<hash>.csv`** - statement/run context stamped on every row, then the
clean transaction columns:

`run_id, converted_ts, source_file, source_sha256, bank, statement_type, template_id,
template_version, template_origin, trust_level, period_start, period_end,
account_number,` then `row_id, date, description, amount, debit, credit, direction,
balance, particulars, code, reference, other_party, type, currency, flags`.

(Verbatim `*_raw` cells stay out of the feed - they're in the per-run JSON; the feed
is the clean analyst dataset. `debit`/`credit` are populated only for split
money-in/out statements.)

**`runs/<hash>.csv`** - one manifest row per statement (keyed by content hash, so a
re-convert overwrites it): `run_id, converted_ts, source_file, source_sha256,
bank, template_id, template_origin, status, trust_level, row_count, period_start,
period_end, gate_result, feed_file`. `gate_result` is `accepted` or `withheld:<reason>`.

---

## 4. The Qlik side (load script)

Create a **folder data connection** `StatementFeed` -> `\\fileserver\...\feed`. Then:

```qvs
// The dashboard dataset - accepted, high-trust, proven conversions
Transactions:
LOAD
    *,
    Year(Date)                               as [Year],
    If(Amount <= 0, 'Withdrawal', 'Deposit') as [Transaction Type]   // trivially derived
FROM [lib://StatementFeed/transactions/*.csv]
(txt, codepage is 65001, embedded labels, delimiter is ',', msq);

// Coverage / QA - one manifest row per statement (accepted AND withheld)
Runs:
LOAD *
FROM [lib://StatementFeed/runs/*.csv]
(txt, codepage is 65001, embedded labels, delimiter is ',', msq);
```

Schedule the reload (e.g. hourly) in the QMC. `codepage 65001` = UTF-8, matching
what the engine writes (special characters stay intact).

---

## 5. Opening the app from Qlik

A tile/button in a Qlik app that opens the Shiny URL (`app.shiny_url` in
`config/config.yaml`) in a new tab - e.g. a text object or button with an
**Open website** action. Both sit behind the **same AD group**, so a user in the
group opens the converter with no second login; anyone else is denied. (Users can
equally just bookmark the app - the tile is convenience + the "start in Qlik" story.)

---

## 6. Concurrency & isolation guarantee

- **Shiny sessions are isolated:** all user state is session-scoped; nobody sees
  another's upload/result/download; each conversion works in a `tempfile()`-unique
  dir. For a hard wall (memory + temp + crash domain) run process/container per
  session (ShinyProxy / Posit Connect / Shiny Server Pro).
- **The feed is per-file:** `write_feed()` writes one content-hash-keyed transactions
  file + one content-hash-keyed manifest row, **never a shared append** - N
  simultaneous conversions = N independent files, safe over SMB.
- **The one shared resource** (by design) is the template library - a shared team
  asset, not user data.

---

## 7. Configuration

All settings live in `config/config.yaml` (created from `config.example.yaml` on
first run, and kept out of any distributed copy). Keys that matter here:
- `app.shiny_url` - the URL the Qlik tile opens.
- `paths.templates` - the **proven** set (also the feed's "proven" definition);
  `paths.user_templates` - analyst drafts.
- `feed.enabled` - write the feed on each conversion (default on).
- `feed.min_trust` / `feed.require_status_ok` / `feed.allowed_template_origins` /
  `feed.template_allowlist` - the gate (§3).
- `feed.feed_dir` - where the feed is written (point it at the share).

---

## 8. Out of scope now: enrichment (future option)

The legacy Qlik app also derived Tax Year, Transaction Code/Description/Category, and
same-owner transfer matching (see `legacy-qlik-mapping.md`). The feed is
**extraction-only**; `Year` and `Transaction Type` are shown derived in the load
script (§4). Fuller enrichment fits either Qlik's load script (`Mapping LOAD`) or a
deterministic engine step, but needs the categorisation taxonomy spreadsheet as data
first. Recommend keeping it in Qlik's load script until multiple consumers need
identical categories.
