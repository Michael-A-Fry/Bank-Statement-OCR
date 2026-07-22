# Local metadata capture — the on-box "ML goldmine"

Every conversion can save a rich, structured record of **how it went** — the
layout it matched, how cleanly it parsed, what the detector saw, how it
reconciled, and any OCR / redaction signals. This is the raw material for future
on-box analysis and a possible **local ML assist** (recommend template edits,
spot format drift, cluster unseen layouts). It is deliberately conservative about
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

Admin tab → **Data capture**. A single **level** (Off / Standard / Full) plus
per-category switches. The choice is saved to `config/config.yaml` under
`metadata:` and applies to the next conversion. Full is the default.

```yaml
metadata:
  level: full            # off | standard | full
  capture:
    layout: true
    parse_quality: true
    detection: true
    reconciliation: true
    ocr: true
    redaction: true
  retain_forever: true   # metadata is never rolled up / archived / deleted
```

## What each level records (per-level PII notes)

| Level | What is captured | PII posture |
|---|---|---|
| **Off** | Nothing beyond the normal run log. | — |
| **Standard** | Layout signature + format; detection match/score/margin; row count; trust level; KPI pass/fail counts; the statement period and an account **hash**. | No per-row detail. Period + account-hash only. |
| **Full** (default) | Everything in Standard **plus**: flag histogram, per-field fill ratios, the "misses" (unparsed dates/amounts), value **shapes** (amount magnitude buckets, description length stats, direction split), detection candidate scores, per-KPI outcomes, opening/closing **balance anchors** and net amount, multi-statement counts (# periods / # accounts / boundary reasons), the **novelty** set (source header inventory, unmapped columns, unrecognised indicator tokens), OCR page/confidence detail, redaction counts + scan completeness, and timing. | Adds balance anchors + the net amount — **financial** metadata, not personal identifiers, and local-only. Value shapes are aggregate counts, never values. Column names and short indicator tokens (e.g. an unrecognised "COW"/"HORSE" debit marker) are structural, not content. Still no descriptions/payees/references and no raw account number. |

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
  tokens it didn't know (the "a new bank writes cow/horse for debit/credit" signal),
  plus `layout.signature` for clustering never-before-seen layouts.
- **Did it reconcile, and how far off?** `reconciliation.{trust_level, kpis,
  opening_balance, closing_balance, net_amount, stated_count}`.

These "unrecognised" fields are the exact feedback loop for supplementing the
engine's vocabularies (see the customisation model): a model clusters what keeps
turning up unrecognised, proposes a new lexicon/template entry, a human approves
it, and the deterministic engine picks it up — the model never changes behaviour
directly.

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
  "detection":      { "matched", "score", "margin", "runner_up", "n_candidates", "candidate_scores" },
  "parse_quality":  { "row_count", "malformed_rows", "redacted_rows", "amount_sign",
                      "date_format", "source_line_count", "multiline_extra",
                      "flag_histogram", "field_fill" },
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

To analyse, list `logs/metadata/` and read the JSON (one object per run). Because
each record is self-contained and content-free, the whole folder is safe to copy
to an analysis box or feed to a local model — no statement content travels with
it, and any account linkage is only ever a hash.
