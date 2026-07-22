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
| **Full** (default) | Everything in Standard **plus**: flag histogram, per-field fill ratios, detection candidate scores, per-KPI outcomes, opening/closing **balance anchors** and net amount, OCR page/confidence detail, redaction counts + scan completeness, and timing. | Adds balance anchors + the net amount — **financial** metadata, not personal identifiers, and local-only. Still no descriptions/payees/references and no raw account number. |

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
