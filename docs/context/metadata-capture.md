# Local metadata capture — the on-box "ML goldmine"

> **At 2.0.0** templates were retired, and with them the record's `template_hints`
> and `detection` blocks (nothing has filled them since; a record written before
> then may still carry them). `template_id` in a record now names the reading's
> candidate or the learned layout, and `template_origin` the feed basis. The suggestion queue is on **Admin -> Words**, not Admin -> Templates.

Every conversion can save a rich, structured record of **how it went** — its
layout signature, how cleanly it parsed, how it reconciled, and any OCR signals.
This is the raw material for future on-box analysis (spot format drift, cluster
unseen layouts). It is deliberately conservative about
what it stores.

## Two hard rules

1. **Local only.** Records are written to `logs/metadata/<run_id>.json` — one
   file per run, the same concurrency-safe "one file per event" story as the run
   log. They **never leave this machine** and **never enter the governed Qlik
   feed** (`feed/`). Nothing under `logs/` is read by a feed connection.
2. **No raw content; PII-conscious.** Descriptions, payees, references,
   particulars and raw per-row amounts are **never** stored — only structure,
   counts, ratios and quality signals. An account number is stored **only** as a
   one-way SHA-256 hash (so the same account links across runs without the number
   being readable).

## Where it's controlled

**Admin → Health → Data capture** (a disclosure at the foot of the tab). A single **level** (Off / Standard / Full) plus
per-category switches. The choice is saved to `config/config.yaml` under
`metadata:` and applies to the next conversion. Full is the default.

```yaml
metadata:
  level: full            # off | standard | full
  capture:
    layout: true
    parse_quality: true
    reconciliation: true
    multi_statement: true
    novelty: true
    ocr: true
  retain_forever: true   # metadata is never rolled up / archived / deleted
```

## What each level records (per-level PII notes)

| Level | What is captured | PII posture |
|---|---|---|
| **Off** | Nothing beyond the normal run log. | — |
| **Standard** | Layout signature + format; row count; trust level; KPI pass/fail counts; the statement period and an account **hash**. | No per-row detail. Period + account-hash only. |
| **Full** (default) | Everything in Standard **plus**: flag histogram, per-field fill ratios, the "misses" (unparsed dates/amounts), value **shapes** (amount magnitude buckets, description length stats, direction split), per-KPI outcomes, opening/closing **balance anchors** and net amount, multi-statement counts (# periods / # accounts / boundary reasons), the **novelty** set (source header inventory, unmapped columns, unrecognised indicator tokens), OCR page/confidence detail, and timing. | Adds balance anchors + the net amount — **financial** metadata, not personal identifiers, and local-only. Value shapes are aggregate counts, never values. Column names, short indicator tokens (e.g. an unrecognised "PAID"/"RECD" debit marker) and *masked* value shapes are structural, not content. Still no descriptions/payees/references and no raw account number. |

### Full coverage — what the "goldmine" answers

The record is designed so a downstream model (or a human) can answer, per run and
across the whole `logs/metadata/` corpus, without ever touching statement content:

- **How many statements / periods / accounts?** `multi_statement.{likely_multiple,
  n_periods, n_accounts, page1_markers, n_opening_labels, n_closing_labels,
  boundary_reasons}`.
- **How many transactions, and how did they shape up?** `parse_quality.{row_count,
  direction_dist, amount_buckets, desc_len}`.
- **What did we NOT read?** `parse_quality.{malformed_rows, unparsed_dates,
  unparsed_amounts, flag_histogram}` — every flag counted (date_unresolved,
  date_alt_format, date_year_inferred, ocr_low_conf, row_stitched, forced, …).
- **What was NEW or unrecognised?** `novelty.{source_headers, unmapped_columns,
  unrecognised_type_values}` — the columns a template never used and the indicator
  tokens it didn't know (the "a new bank writes Paid/Recd for debit/credit" signal),
  plus `layout.signature` for clustering never-before-seen layouts.
- **Did it reconcile, and how far off?** `reconciliation.{trust_level, kpis,
  opening_balance, closing_balance, net_amount, stated_count}`.

These "unrecognised" fields feed the suggestion queue in **Admin → Words**
(see [`../operational/admin-and-maintenance.md`](../operational/admin-and-maintenance.md)):
what keeps turning up unrecognised is counted, offered most-frequent-first, a
human approves one, and the deterministic engine picks it up on the next
conversion. Nothing is ever learned without that approval step.

Balances and the statement period are financial metadata, not personal
identifiers, and never leave the machine. Account numbers appear only as a hash.

## Record shape (Full)

```json
{
  "schema": 1,
  "run_id": "…", "ts": "…Z", "level": "full",
  "requested_by": "…", "source_sha256": "…", "source_ext": "csv", "status": "ok",
  "template_id": "…", "template_origin": "default", "template_version": 1,
  "period_start": null, "period_end": null,
  "account_hash": "…16-char hash… or null",
  "layout":         { "signature", "format", "kind", "n_pages", "n_columns", "hint" },
  "parse_quality":  { "row_count", "malformed_rows", "redacted_rows", "amount_sign",
                      "date_format", "source_line_count", "multiline_extra",
                      "flag_histogram", "field_fill" },
  "multi_statement":{ "likely_multiple", "n_periods", "n_accounts", "page1_markers",
                      "pages_stated", "combined_accounts", "n_opening_labels",
                      "n_closing_labels", "boundary_reasons" },
  "novelty":        { "source_header_count", "source_headers", "unmapped_columns",
                      "unrecognised_type_values" },                    // what we did NOT recognise
  "reconciliation": { "trust_level", "trust_score", "kpis",
                      "opening_balance", "closing_balance", "stated_count", "net_amount" },
  "ocr":            { "pages", "min_confidence", "low_conf_cells" },     // only when OCR ran
  "redaction":      { "redacted_rows", "scan_incomplete" },              // only when relevant
  "elapsed_ms": 0
}
```

## Retention

Metadata is **kept forever**. Log rollup (`rollup_logs`) only ever archives the
`runs` and `feedback` subdirectories; it never touches `metadata`. `retain_forever`
documents that intent.

## Reading it

To analyse, list `logs/metadata/` and read the JSON (one object per run). Each
record is self-contained and carries **no statement content**: no transaction rows,
descriptions, amounts or balances, no filename (only the file *extension* and the
content hash), and any account linkage is only ever a hash.

Two fields are worth naming before you copy the folder anywhere, because "content-free"
is not the same as "anonymous": a record does carry **`requested_by`** (who ran the
conversion) and the **statement period dates**. That is intentional — they are what
make drift and coverage analysis meaningful — but it means the folder is *low-risk*,
not *no-risk*. Treat it as internal operational data: fine to copy to an analysis box
inside the organisation, not something to hand outside it without a look first.

The **run** log (`logs/runs/`) is a different matter: it does record the uploaded
**filename**, which is frequently identifying. See
`architecture/build-contract.md` §10.
