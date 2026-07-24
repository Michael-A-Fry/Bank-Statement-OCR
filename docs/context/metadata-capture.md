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
    multi_statement: true
    novelty: true
    template_hints: true
    ocr: true
    redaction: true
  retain_forever: true   # metadata is never rolled up / archived / deleted
```

## What each level records (per-level PII notes)

| Level | What is captured | PII posture |
|---|---|---|
| **Off** | Nothing beyond the normal run log. | — |
| **Standard** | Layout signature + format; detection match/score/margin; row count; trust level; KPI pass/fail counts; the statement period and an account **hash**. | No per-row detail. Period + account-hash only. |
| **Full** (default) | Everything in Standard **plus**: flag histogram, per-field fill ratios, the "misses" (unparsed dates/amounts), value **shapes** (amount magnitude buckets, description length stats, direction split), detection candidate scores, per-KPI outcomes, opening/closing **balance anchors** and net amount, multi-statement counts (# periods / # accounts / boundary reasons), the **novelty** set (source header inventory, unmapped columns, unrecognised indicator tokens), the **template hints** (per-column profiles + suggested mapping), OCR page/confidence detail, redaction counts + scan completeness, and timing. | Adds balance anchors + the net amount — **financial** metadata, not personal identifiers, and local-only. Value shapes are aggregate counts, never values. Column names, short indicator tokens (e.g. an unrecognised "PAID"/"RECD" debit marker) and *masked* value shapes are structural, not content. Still no descriptions/payees/references and no raw account number. |

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
- **Everything needed to DRAFT a template** (the richest signal when a statement
  could NOT be matched): `template_hints` — see below.
- **Did it reconcile, and how far off?** `reconciliation.{trust_level, kpis,
  opening_balance, closing_balance, net_amount, stated_count}`.

### `template_hints` — hand an AI (or a human) everything to build a template

When the engine can't match a statement, the most valuable thing to capture is a
**structural description of its source columns** and the engine's own best guess
at the mapping — enough for a person, or an AI assistant, to write a template
*without ever seeing statement content*. Captured for every run at `full`, it is
the exact payload to paste alongside the lexicon when asking a copilot to
"recommend template and lexicon changes to support this new statement".

For a **CSV / Excel** statement:
- `columns[]` — one profile per source column: its `name`, inferred `kind`
  (`date` / `money` / `integer` / `indicator` / `text`), `fill_rate`, `distinct`
  count, and a **masked** `example_shape` (every digit → `9`, every letter → `A`,
  punctuation kept, so `31/12/2025` → `99/99/9999` and `$1,234.56` → `$9,999.99`).
  Kind-specific detail: a `date` carries its detected `date_format` (`%d/%m/%Y`);
  a `money` column carries the style facts a template needs (`decimal_mark`,
  `thousands_sep`, `currency_symbol`, `parens_negative`, `minus_negative`,
  `dr_cr_suffix`); an `indicator` exposes its short distinct `tokens` (e.g.
  `["D","C"]`); a `text` column carries `length` stats.
- `suggested_mapping` — the engine's own first draft, from the same detectors the
  wizard uses: `date` + `date_format`, the `amount_style`, the `fields`
  (field → source header), and, for a D/C statement, the inferred
  `type_debit_value` / `type_credit_value`. Header names + formats only.
- `delimiter` (CSV).

For a **PDF** statement: `pdf_bands` (the auto-suggested column x-ranges, offered
only when no template matched), `shapes` (money/date style facts scanned from the
page text) and `fingerprint_candidates` (distinctive header phrases a template
could match on).

**PII posture:** identical to the rest of capture — no raw values leave. Column
names, kinds, formats, counts and *masked* shapes are structural; the only literal
tokens emitted are a low-cardinality indicator column's short distinct values
(the same posture as `novelty.unrecognised_type_values`). The masked shape reveals
that a description column *can contain* an account-number pattern
(`AA 99-9999-9999999-99`) without ever storing a real digit.

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
  "multi_statement":{ "likely_multiple", "n_periods", "n_accounts", "page1_markers",
                      "pages_stated", "combined_accounts", "n_opening_labels",
                      "n_closing_labels", "boundary_reasons" },
  "novelty":        { "source_header_count", "source_headers", "unmapped_columns",
                      "unrecognised_type_values" },                    // what we did NOT recognise
  "template_hints": { "kind", "delimiter", "row_sample",
                      "columns": [ { "name", "kind", "fill_rate", "distinct", "example_shape",
                                     "date_format" | "money{…}" | "tokens[]" | "length{…}" } ],
                      "suggested_mapping": { "date", "date_format", "amount_style", "fields{…}",
                                             "type_debit_value", "type_credit_value" } },
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
