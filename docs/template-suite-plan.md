# Template suite plan — reaching launch parity

Goal: cover the same statement layouts the legacy tool handled, as declarative YAML
templates. This plan turns the recovered legacy logic (see
`legacy-qlik-mapping.md`) into a concrete build list. Extraction parity is the
launch target; the enrichment layer is a separate, later decision.

## What the legacy tool tells us up front

For each bank layout the old parser fixes the **date format, amount style, which
columns exist, and where the balances sit** — everything except the exact on-page
positions (it read by word-position logic, not coordinates). So each template can
be **pre-seeded now** and only needs its bands drawn against one real statement.

Those pre-seeds live in `templates_seed/` (not loaded by the app — finish one and
move it into `templates/`). Each carries the known settings with `# TODO`
placeholders for the coordinates.

## Coverage

| Layout | Format | Status | Seed |
|---|---|---|---|
| ANZ everyday | PDF | **shipped** (`anz_everyday_pdf`) | — |
| ASB everyday | PDF | **shipped** (`asb_everyday_pdf`) | — |
| Westpac everyday | PDF | **shipped** (`westpac_everyday_pdf`) | — |
| ANZ investment funds | PDF | **shipped** | — |
| ANZ Loan | PDF | to build | `anz_loan_pdf` |
| ANZ Visa | PDF | to build | `anz_visa_pdf` |
| ASB Visa | PDF | to build | `asb_visa_pdf` |
| BNZ Visa | PDF | to build | `bnz_visa_pdf` |
| Kiwibank Credit Card | PDF | to build | `kiwibank_creditcard_pdf` |
| Kiwibank everyday | PDF | to build | `kiwibank_everyday_pdf` |
| Westpac Credit Card | PDF | to build | `westpac_creditcard_pdf` |
| BNZ / Kiwibank / Westpac | Excel | to build | `*_everyday_excel` |
| ANZ / ASB / BNZ / Kiwibank / Westpac | CSV | **shipped** | — |

The **credit-card PDFs share one shape** — `Date | Details | Amount`, no running
balance in the table — which is exactly what the new `metadata_regions` feature is
for: pin opening/closing balance with a drawn box. ASB Visa, ANZ Loan and Kiwibank
everyday carry a running balance column instead.

## Per-template known settings (from the legacy parser)

- **All PDFs:** `date_format: "%d %b"` (day + month; the year is taken from the
  statement period automatically). `row_tol: 3`.
- **Credit cards** (ANZ/BNZ/Kiwibank/Westpac): `amount_sign: signed`; columns
  `date, description, amount`; balances via `metadata_regions`.
- **ASB Visa / ANZ Loan / Kiwibank everyday:** `amount_sign: signed`; columns
  `date, description, amount, balance`. (If a statement splits money in/out into
  two columns, switch to `debit_credit_cols` with `debit`/`credit` bands — same as
  the shipped `anz_everyday_pdf`.)
- **Excel (BNZ/Kiwibank/Westpac):** column maps mirror the matching CSV templates;
  confirm header text + date format against a real `.xlsx`.

## Build process (per template)

1. One real statement of that type → `samples/_private_staging/` (git-ignored;
   real statements are never committed).
2. Convert it → the toolkit opens on a no-match. Load the seed's settings, **draw
   the column bands**, **pin the balances** (credit cards), and use **"this IS a
   transaction"** in the X-ray for any skipped row.
3. Confirm the balance reconciles → **Save** (lands in `templates_user/`).
4. Promote: move the saved file into `templates/` and add a golden-file test
   (`tests/HOWTO-add-template-test.md`).

## Suggested sequence

1. **Excel ×3** — easiest (pure column mapping, no OCR, no banding).
2. **Credit cards ×4** — one shared shape; balances pinned by box.
3. **ASB Visa / ANZ Loan / Kiwibank everyday** — running-balance PDFs.

## Enrichment layer (Phase 2 — a scope decision, not launch-blocking)

The legacy tool added, on top of extraction: **Tax Year** (NZ Apr–Mar; Jan–Mar →
prior year), **Transaction Type** (from amount sign), **Transaction Code**
(200/400 default, else looked up), **Transaction Description / Category** (substring
mapping of the details), and **same-owner transfer matching**. All deterministic
and dictionary-driven — a good fit for this engine — but the category taxonomy
lived in an external `Transaction Codes.xlsx` we don't have. Recommend deciding
whether these output columns are required at launch; if so, obtain that list.

## What's needed to build

- **One real sample per to-build layout** in `samples/_private_staging/`. Without a
  real sample the exact bands can't be set.
- **Scope call:** launch = extraction only (the templates above), or extraction +
  Phase-2 enrichment (needs the `Transaction Codes.xlsx`).
