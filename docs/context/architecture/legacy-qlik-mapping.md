# Legacy "Statement Converter" (Qlik) - schema & enrichment reference

This is a build reference distilled from the legacy Qlik app that this platform
replaces (`Statement Converter 300925.qvf`). The Qlik
app's binary hides its logic, but the full load script is recoverable; the notes
below capture **what the old tool produced and how**, so our templates and output
can be measured against it. Internal infrastructure detail (server paths, staff
identifiers) is deliberately omitted - only the logic that matters for building
templates is kept.

## What the legacy tool did

- **Purpose:** convert bank statements (PDF and Excel) into one standardised,
  analyst-ready dataset for financial-crime casework.
- **PDF ingestion:** a paid external connector ("Mole") extracted every word of a
  PDF **with its position**, plus a custom rule to keep money amounts as a single
  token. Each bank was then reconstructed by **hand-written, per-bank script** that
  walked those word positions (anchor words like "Withdrawals" / "No transactions
  for this period", exclusion flags for interest lines, etc.).
- **Excel ingestion:** per-bank column-mapping blocks (BNZ, Kiwibank, Westpac).
- **Why we're replacing it:** the per-bank logic is imperative and hard to
  maintain, and PDF reading depends on a licensed connector. Our platform reads
  PDFs with pure R (pdftools text layer **or** tesseract OCR) and describes each
  bank as a **declarative YAML template** - same result, no licensed dependency,
  maintainable by one analyst.

## Statement types the legacy tool covered

Our template set should aim to cover the same layouts:

| Kind  | Layouts |
|-------|---------|
| PDF   | ANZ, ANZ Loan, ANZ Visa, ASB Visa, BNZ Visa, Kiwibank Credit Card, Kiwibank (PDF), Westpac Credit Card |
| Excel | BNZ, Kiwibank, Westpac |

## Canonical output schema (the target)

The legacy "Final Transform" emitted these ~21 columns. The right-hand column maps
each to what our engine produces today, so the gaps are explicit.

| Legacy column | Derivation | In this platform |
|---|---|---|
| File Name | source file | ✅ `header$source_file` |
| Doc Reference Bank Statement / Voucher | provenance refs | ~ (we have `provenance$source_ref`) |
| Statement ID / Statement Row ID | per-statement + per-row id | ~ (`row_id`) |
| Bank | template | ✅ `header$bank` |
| Account Name / Account Number | parsed header | ~ (header fields exist; population varies) |
| Other Party Account Name / Number | parsed per bank | ~ (`other_party`) |
| Date | parsed | ✅ `date` (ISO) + `date_raw` |
| Transaction Time | (usually blank) | - |
| Year | `Year(Date)` | ➖ trivially derivable |
| **Tax Year** | NZ tax year: Apr–Mar; **Jan/Feb/Mar → Year−1** | ❌ not derived |
| Details | verbatim description | ✅ `description` |
| **Transaction Type** | sign rule: `Amount<=0 → Withdrawal`, `>0 → Deposit`, else `Unidentified` | ➖ derivable from `direction`/`amount` |
| **Transaction Code** | `200`=deposit, `400`=withdrawal by default, else looked up | ❌ |
| **Transaction Description** | substring-map of lowercased Details | ❌ |
| **Transaction Category** | substring-map of Details; `BP…` prefix → "Transfers to/from third parties"; **transfer-matching** (below) | ❌ |
| Amount | parsed (signed) | ✅ `amount` (+ `amount_raw`) |
| Balance | parsed | ✅ `balance` (+ `balance_raw`) |

**Extraction parity is essentially met** (dates, descriptions, amounts, balances,
signs, redactions, reconciliation). **The gap is the enrichment layer**, below.

## Enrichment rules (future build targets - all deterministic, no ML)

1. **Tax Year** - `if month(Date) in {Jan,Feb,Mar}: Year-1 else Year`.
2. **Transaction Type** - from the amount sign (we already carry `direction`).
3. **Transaction Code** - default `200` (deposit) / `400` (withdrawal); otherwise a
   lookup keyed on the derived Transaction Description.
4. **Transaction Description & Category** - `MapSubString`-style: scan the
   lowercased Details for known substrings and emit the matched tag. Deposits and
   withdrawals use **separate** mapping tables.
5. **`BP` prefix rule** - a Details value beginning `BP` with no category →
   "Transfers to third parties" (withdrawal) / "…from third parties" (deposit).
6. **Same-owner transfer matching** - if an Other-Party account number equals one
   of the subject's **own** account numbers seen in the batch, the row is tagged as
   an internal transfer.

> The description/category/code **taxonomy is not in the app** - it lived in an
> externally-maintained "Transaction Codes" spreadsheet. Any port of this
> enrichment needs that reference list (or an equivalent) as data, not code - a
> natural fit for our YAML / label-dictionary approach.

## Implications for this platform

- Our word-box + YAML-template approach is a clean, licence-free equivalent of
  Mole + per-bank script. Building the 8 PDF + 3 Excel templates above is the path
  to launch parity on **extraction**.
- Whether the **enrichment layer** (tax year, type, code, description, category,
  transfer matching) is in scope, or handled downstream, is a product decision.
  All of it is deterministic and dictionary-driven - it suits this engine and needs
  no ML - but it depends on obtaining the categorisation taxonomy as data.
